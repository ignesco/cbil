{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module CbilLib
(
    cbilVersion
    , DBInit(..)
    , ScriptExecuted(..)
    , DBCacheInitialiser(..)
    , _cbilMain
    , touch
    , initCloneProjects
    , initNeeds
    , initDatabaseGroups
    , initIncrementalDatabaseGroups
    , initVisualStudios
    , initNetTiersGroups
    , cbilHelp
)
where

import Development.Shake
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Set as DS

import System.Console.GetOpt
import qualified System.Directory as SD
import Development.Shake.FilePath

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree

cbilVersion = "v1.5c"
    
documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml = readDocument [] xml >>> getChildren >>> hasName "cbil"

-- Cbil data types -------------------
data RunType = DryRun | NormalRun deriving (Eq, Show)
        
data CbilConfiguration = CbilConfiguration {
        cbilProfileList :: [String]
        , cbilSettingsFiles :: [FilePath]
        , cbilRunType :: RunType
        , showHiddenTargets :: Bool
    } deriving (Show)

loadAllSettingsFiles' :: [FilePath] -> (String -> IO [a]) -> IO [a]
loadAllSettingsFiles' files loader = do
    load' <- mapM loader files
    return $ concat load'

loadAllSettingsFiles :: CbilConfiguration -> (String -> IO [a]) -> IO [a]
loadAllSettingsFiles configuration loader = loadAllSettingsFiles' (cbilSettingsFiles configuration) loader

type CbilRulesInfo = (String, [String])

-- ProfileDefines --------------------

type ProfileDefineList = Maybe [(String, String)]
type ProfileDefine = (String, [(String, String)])
type ProfileDefines = [ProfileDefine]

getProfileDefinesTrees :: String -> IOSLA (XIOState s) a XmlTree
getProfileDefinesTrees xml = documentRoot xml >>> getChildren >>> hasName "ProfileDefines" >>> getChildren >>> hasName "profile"

loadProfileDefines :: String -> IO ProfileDefines
loadProfileDefines xmlFile = do
    trees <- runX (getProfileDefinesTrees xmlFile)
    return $ concat $ map (runLA mapToCbilProfileDefines) trees

mapToCbilProfileDefines :: ArrowXml t => t XmlTree ProfileDefine
mapToCbilProfileDefines = let 
        defineNamesSelector = getAttrValue "name"
        defineValuesSelector = getChildren >>> getText
        kvPairTuplesSelector = defineNamesSelector &&& defineValuesSelector >>> arr2 (,)
    in proc tree -> do
        pid             <- getAttrValue "profileid" -< tree
        defines         <- getChildren >>> hasName "defines" -< tree
        kvPairsTuples   <- listA (getChildren >>> hasName "define" >>> kvPairTuplesSelector) -< defines
        returnA -< (pid, map (\(k, v) -> (concat ["%",k, "%"], v)) kvPairsTuples)

mkProfileDefines :: [String] -> ProfileDefines -> Rules ProfileDefineList
mkProfileDefines profileList profiles = let
        profileDefines = maybe Nothing (Just . concat) $  sequence $ filter Data.Maybe.isJust $ map (flip lookup profiles) profileList
   in return $ profileDefines

initProfileDefines :: CbilConfiguration -> Rules ProfileDefineList
initProfileDefines configuration = do
    profileDefines <- liftIO $ loadAllSettingsFiles configuration loadProfileDefines
    xmlDefines' <- mkProfileDefines (cbilProfileList configuration) profileDefines
    case xmlDefines' of
        Just xmlDefines -> do
            cwdDir <- liftIO $ SD.getCurrentDirectory
            return $ Just (("%__CWD__%", cwdDir):xmlDefines)
        Nothing -> return $ Nothing

applyProfileDefines :: ProfileDefineList -> String -> String
applyProfileDefines Nothing str = str
applyProfileDefines (Just defines) str = _applyProfileDefines defines str
    where
        _applyProfileDefines d s = foldr (\(k, v) a -> rep k v a) s d
        rep replaceString withString str = map (chr . fromEnum) $ BSL.unpack $ BSS.replace (BS8.pack replaceString) (BS8.pack withString) (BS8.pack str)

profileInList :: String -> [String] -> Bool
profileInList profile profileList = elem profile profileList

-- DatabaseGroups ----------------------

data Script = DatabaseGroupScript {
        dbname :: String
        , dbdefinename :: String
        , scriptname :: String
    }
    deriving (Show)

data DatabaseGroup = DatabaseGroup {
            databaseGroupProfileId :: String
            , databaseGroupId :: String
            , workingDirectory :: FilePath
            , serverName :: Maybe String
            , scripts :: [Script]
        }
        deriving (Show)

type DatabaseGroups = [DatabaseGroup]
type RawDatabaseGroup = (String, String, FilePath, Maybe String, [(String, String, String)])
        
getDatabaseGroupsTrees :: String -> IOSLA (XIOState s) a XmlTree
getDatabaseGroupsTrees xml = documentRoot xml >>> getChildren >>> hasName "DatabaseGroups" >>> getChildren >>> hasName "databaseGroup"

loadDatabaseGroups :: String -> IO [RawDatabaseGroup]
loadDatabaseGroups xmlFile = do
    trees <- runX (getDatabaseGroupsTrees xmlFile)
    return $ concat $ map (runLA mapToCbilDatabaseGroups) trees

mapToCbilDatabaseGroups :: ArrowXml t => t XmlTree RawDatabaseGroup
mapToCbilDatabaseGroups = let
        dbnamesSelector         = getAttrValue "dname"
        dbdefinenamesSelector   = getAttrValue "dbdefinename"
        scriptNamesSelector     = getChildren >>> getText
        scriptTuplesSelector    = dbnamesSelector &&& dbdefinenamesSelector &&& scriptNamesSelector >>> arr3 (,,)
    in proc tree -> do
        pid                 <- getAttrValue "profileid" -< tree
        databaseGroupId     <- getAttrValue "id" -< tree
        workingDirectory    <- getChildren >>> hasName "workingDirectory" >>> getChildren >>> getText -< tree
        serverName'         <- listA (getChildren >>> hasName "serverName" >>> getChildren >>> getText) -< tree
        scripts             <- getChildren >>> hasName "scripts" >>> listA (getChildren >>> hasName "script" >>> scriptTuplesSelector) -< tree
        let serverName = if length serverName' == 1 then Just (head serverName') else Nothing
        
        returnA -<  (pid, databaseGroupId, workingDirectory, serverName, scripts)

initDatabaseGroups :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initDatabaseGroups configuration profileDefines = do
    rawDatabaseGroupsList <- liftIO $ loadAllSettingsFiles configuration loadDatabaseGroups
    let
        profileList = cbilProfileList configuration
        normalRun = cbilRunType configuration == NormalRun
        rawDbGroupsForProfile = filter (\(p, _, _, _, _) -> profileInList p profileList) rawDatabaseGroupsList
    rules <- mapM ((mkDatabaseGroupRule normalRun) . (mkDatabaseGroups profileList profileDefines)) rawDbGroupsForProfile
    return ("DatabaseGroups", rules)

mkDatabaseGroupRule :: Bool -> DatabaseGroup -> Rules String
mkDatabaseGroupRule normalRun (DatabaseGroup pid databaseGroupId workingDirectory servername scripts) = do
    let
        mk_sqlcmd wd script = _sqlcmd normalRun wd databaseGroupId servername (dbname script) (dbdefinename script) (scriptname script)
        
    phony databaseGroupId $ do
        mapM_ (mk_sqlcmd workingDirectory) scripts
    return databaseGroupId

mkDatabaseGroups :: [String] -> ProfileDefineList -> RawDatabaseGroup -> DatabaseGroup
mkDatabaseGroups profileList profileDefines (pid, databaseGroupId, workingDirectory', servername', scripts') =
    let
        applyProfileDefines' = applyProfileDefines profileDefines
        workingDirectory = applyProfileDefines' workingDirectory'
        servername = fmap applyProfileDefines' servername'
        scripts = map (\(dbname', dbdefinename', scriptname') -> DatabaseGroupScript (applyProfileDefines' dbname') (applyProfileDefines' dbdefinename') (applyProfileDefines' scriptname') ) scripts'
    
    in DatabaseGroup pid databaseGroupId workingDirectory servername scripts


-- IncrementalDatabaseGroups ----------------------
data IncrementalScript = IncrementalDatabaseGroupScript {
        incscriptname :: String
    }
    deriving (Show)

data IncrementalDatabaseGroup = IncrementalDatabaseGroup {
            incdatabaseGroupProfileId :: String
            , incdatabaseGroupId :: String
            , incworkingDirectory :: FilePath
            , incservername :: Maybe String
            , incdbname :: String
            , incscripts :: [IncrementalScript]
        }
        deriving (Show)

type IncrementalDatabaseGroups = [IncrementalDatabaseGroup]
type RawIncrementalDatabaseGroup = (String, String, FilePath, Maybe String, String, [String])
type DBInit = FilePath -> String -> Maybe String ->String -> Action (Maybe [(String, FilePath)])
type ScriptExecuted = Bool -> String -> FilePath -> Maybe String -> String -> Action Bool
data DBCacheInitialiser = UserDBCache {
        dbInit :: DBInit
        , scriptExecuted :: ScriptExecuted
    }
        
initIncrementalDatabaseGroups :: CbilConfiguration -> ProfileDefineList -> DBCacheInitialiser -> Rules CbilRulesInfo
initIncrementalDatabaseGroups configuration profileDefines initialiaseDBCache = do
    rawIncrementalDatabaseGroupsList <- liftIO $ loadAllSettingsFiles configuration loadIncrementalDatabaseGroups
    let
        profileList = cbilProfileList configuration
        normalRun = cbilRunType configuration == NormalRun
        rawIncrementalDatabaseGroupsForProfile = filter (\(p, _, _, _, _, _) -> profileInList p profileList) rawIncrementalDatabaseGroupsList
    rules <- mapM ((mkIncrementalDatabaseGroupRule normalRun initialiaseDBCache) . (mkIncrementalDatabaseGroups profileList profileDefines)) rawIncrementalDatabaseGroupsForProfile
    return ("IncrementalDatabaseGroups", rules)

getIncrementalDatabaseGroupsTrees :: String -> IOSLA (XIOState s) a XmlTree
getIncrementalDatabaseGroupsTrees xml = documentRoot xml >>> getChildren >>> hasName "IncrementalDatabaseGroups" >>> getChildren >>> hasName "databaseGroup"

loadIncrementalDatabaseGroups :: String -> IO [RawIncrementalDatabaseGroup]
loadIncrementalDatabaseGroups xmlFile = do
    trees <- runX (getIncrementalDatabaseGroupsTrees xmlFile)
    return $ concat $ map (runLA mapToCbilIncrementalDatabaseGroups) trees

mapToCbilIncrementalDatabaseGroups :: ArrowXml t => t XmlTree RawIncrementalDatabaseGroup
mapToCbilIncrementalDatabaseGroups = proc tree -> do
    pid                 <- getAttrValue "profileid" -< tree
    databaseGroupId     <- getAttrValue "id" -< tree
    workingDirectory    <- getChildren >>> hasName "workingDirectory" >>> getChildren >>> getText -< tree
    dbname              <- getChildren >>> hasName "databaseInfo" >>> getAttrValue "dbname" -< tree
    servername'         <- getChildren >>> hasName "databaseInfo" >>> getAttrValue "servername" -< tree
    scripts             <- getChildren >>> hasName "scripts" >>> listA (getChildren >>> hasName "script" >>> getChildren >>> getText) -< tree

    let servername = if length servername' == 0 then Nothing else Just servername'
    returnA -<  (pid, databaseGroupId, workingDirectory, servername, dbname, scripts)

getRunFile :: FilePath -> String -> String -> String -> FilePath
getRunFile runFileDirectory fp databaseGroupId dbname = runFileDirectory </> (intercalate "." [fp,databaseGroupId, dbname, "run"] )
        
mkIncrementalDatabaseGroupRule :: Bool -> DBCacheInitialiser -> IncrementalDatabaseGroup -> Rules String
mkIncrementalDatabaseGroupRule normalRun initialiaseDBCache (IncrementalDatabaseGroup pid databaseGroupId workingDirectory servername dbname scripts) = do
    let
        wd = splitDirectories workingDirectory
        containsDotDot = length (filter (=="..") wd) > 0
        
        runFileDirectory = normalise $ "_build_cbil" </> workingDirectory </> databaseGroupId </> dbname
        dbcache = runFileDirectory </> (databaseGroupId ++ "." ++ dbname ++ ".dbcache.info")
        deleteDirectory path = do
            exists <- doesDirectoryExist path
            if exists then liftIO $ SD.removeDirectoryRecursive path else return ()

    phony databaseGroupId $ 
        if containsDotDot
          then putNormal $ "ERROR Working Directory cannot contain '..':" ++ workingDirectory
          else do
            putNormal $ "INCREMENTAL: " ++ databaseGroupId
            let
                needs' = map (\(IncrementalDatabaseGroupScript sql') -> getRunFile runFileDirectory sql' databaseGroupId dbname) scripts

            deleteDirectory runFileDirectory
            need [dbcache]
            mapM_ (need . (:[]))  needs'

    mkDbCacheRule dbcache runFileDirectory databaseGroupId servername dbname initialiaseDBCache
    mkRunRule normalRun runFileDirectory databaseGroupId workingDirectory servername dbname initialiaseDBCache
        
    return databaseGroupId

mkDbCacheRule :: String -> FilePath -> String -> Maybe String -> String -> DBCacheInitialiser -> Rules ()
mkDbCacheRule dbcache runFileDirectory databaseGroupId servername dbname initialiaseDBCache =
    dbcache %> \out -> do
        files' <- (dbInit initialiaseDBCache) runFileDirectory databaseGroupId servername dbname
        case files' of
            Just files -> do
                let fs = map (\(g, f) -> getRunFile runFileDirectory f g dbname) files
                mapM_ touch fs
                produces fs
                touch out
            Nothing -> return ()

mkRunRule :: Bool -> FilePath -> String -> FilePath -> Maybe String -> String -> DBCacheInitialiser -> Rules ()
mkRunRule normalRun runFileDirectory databaseGroupId workingDirectory servername dbname initialiaseDBCache = do
    let
        servername' = maybe "." id servername
        runSuffix = intercalate "." [databaseGroupId, dbname, "run"]
        extractScriptName suffix target = drop (length (runFileDirectory) + 1 ) (take (length target - length suffix - 1) target)
        mk_sqlcmd servername dbname wd script = _sqlcmd normalRun wd databaseGroupId servername dbname dbname script
        
    ("**/*." ++ runSuffix) %> \out -> do
        let scriptName = extractScriptName runSuffix out
        exists <- doesFileExist out
        if exists
          then do
            putNormal $ concat ["SKIPPING: workingDirectory: ", workingDirectory, " ", "servername: ", servername', " ", "dbname: ", dbname, " ", "dbdefinename: ", dbname, " ", "scriptName: ", scriptName]
            touch out
          else do
            mk_sqlcmd servername dbname workingDirectory scriptName
            ok <- (scriptExecuted initialiaseDBCache) normalRun databaseGroupId scriptName servername dbname
            if ok then touch out else return ()
        
mkIncrementalDatabaseGroups :: [String] -> ProfileDefineList -> RawIncrementalDatabaseGroup -> IncrementalDatabaseGroup
mkIncrementalDatabaseGroups profileList profileDefines (pid, databaseGroupId, workingDirectory', servername', dbname', scripts') =
    let
        applyProfileDefines' = applyProfileDefines profileDefines
        workingDirectory = applyProfileDefines' workingDirectory'
        dbname = applyProfileDefines' dbname'
        servername = fmap applyProfileDefines' servername'
        scripts = map (\(scriptname') -> IncrementalDatabaseGroupScript (applyProfileDefines' scriptname') ) scripts'
    
    in IncrementalDatabaseGroup pid databaseGroupId workingDirectory servername dbname scripts

-- CloneProjects ----------------------

type CloneProject = (String, String, String, String, String, String)
    
getCloneProjectTrees :: String -> IOSLA (XIOState s) a XmlTree
getCloneProjectTrees xml = documentRoot xml >>> getChildren >>> hasName "CloneProjects" >>> getChildren >>> hasName "project"
    
loadCloneProjects :: String -> IO [CloneProject]
loadCloneProjects xmlFile = do
    trees <- runX (getCloneProjectTrees xmlFile)
    return $ concat $ map (runLA mapToCbilCloneProjects) trees

mapToCbilCloneProjects :: ArrowXml t => t XmlTree CloneProject
mapToCbilCloneProjects = proc tree -> do
    pid     <- getAttrValue "id" -< tree
    profid  <- getAttrValue "profileid" -< tree
    wd      <- getChildren >>> hasName "workingDirectory" >>> getChildren >>> getText -< tree
    rloc    <- getChildren >>> hasName "repoLocation" >>> getChildren >>> getText -< tree
    br      <- getChildren >>> hasName "branch" >>> getChildren >>> getText -< tree
    lrname  <- getChildren >>> hasName "localRepoName" >>> getChildren >>> getText -< tree

    returnA -< (pid, profid, wd, rloc, br, lrname)

mkCloneRules :: CbilConfiguration -> [CloneProject] -> Rules CbilRulesInfo
mkCloneRules config ps' = do
    let ps = filter (\(_, prof, _, _, _, _) -> profileInList prof (cbilProfileList config)) ps'
    rules <- mapM (mkCloneRule (cbilRunType config == NormalRun) ) ps
    return ("CloneProjects", rules)
        
mkCloneRule :: Bool -> CloneProject -> Rules String
mkCloneRule normalRun (pid, _, wd, rloc, br, lrname) = do
    phony pid $ do
        putNormal $ "CloneProject: " ++ intercalate ", " ["wd: "++wd, "branch: "++br, "remote: "++rloc, "localrepo: "++lrname]
        if normalRun then command_ [Cwd wd] "git" ["clone", "--single-branch", "--branch", br, rloc, lrname] else return ()
    return pid

initCloneProjects :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initCloneProjects configuration _ = do
    cloneProjects <- liftIO $ loadAllSettingsFiles configuration loadCloneProjects
    mkCloneRules configuration cloneProjects

-- Needs --------------------

type NeedsList = (String, String, [String])

getNeedsListTrees :: String -> IOSLA (XIOState s) a XmlTree
getNeedsListTrees xml = documentRoot xml >>> getChildren >>> hasName "Needs" >>> getChildren >>> hasName "needsList"

mapToCbilNeeds :: ArrowXml t => t XmlTree NeedsList
mapToCbilNeeds = proc tree -> do
    pid         <- getAttrValue "id" -< tree
    profile     <- getAttrValue "profileid" -< tree
    nl          <- listA (getChildren >>> hasName "need" >>> getChildren >>> getText) -< tree
    returnA -< (pid, profile, nl)
    
loadNeeds :: String -> IO [NeedsList]
loadNeeds xmlFile = do
    trees <- runX (getNeedsListTrees xmlFile)
    return $ concat $ map (runLA mapToCbilNeeds) trees

mkNeedsRule :: Bool -> NeedsList -> Rules ()
mkNeedsRule normalRun (pid, _, nl') = do
        phony pid $ do
            putNormal $ "NeedsList: " ++ intercalate ", " nl'
            mapM_ (need . (:[])) nl'

mkNeedsRules :: CbilConfiguration -> [NeedsList] -> Rules CbilRulesInfo
mkNeedsRules config nl' = do
    let nl = filter (\(_, prof, _ ) -> profileInList prof (cbilProfileList config) ) nl'
    mapM_ (mkNeedsRule (cbilRunType config == NormalRun)) nl
    return $ ("Needs", map (\(pid, _, _) -> pid) nl)

initNeeds :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initNeeds configuration _ = do
    needsList <- liftIO $ loadAllSettingsFiles configuration loadNeeds
    mkNeedsRules configuration needsList

-- Visual Studio -------------------
        
type RawVisualStudio = (String, String, String, String, String, String)
data VisualStudio = VisualStudio {
        visualStudioId :: String
        , visualStudioProfile :: [String]
        , visualStudioSolutionPath :: FilePath
        , visualStudioSolutionFile :: String
        , visualStudioTarget :: String
        , visualStudioConfiguration :: String
    } deriving (Show)

getVisualStudioListTrees :: String -> IOSLA (XIOState s) a XmlTree
getVisualStudioListTrees xml = documentRoot xml >>> getChildren >>> hasName "VisualStudioSolutionGroups" >>> getChildren >>> hasName "visualStudioSolutionGroup"

mapToCbilVisualStudio :: ArrowXml t => t XmlTree RawVisualStudio
mapToCbilVisualStudio = proc tree -> do
    pid             <- getAttrValue "id" -< tree
    profile         <- getAttrValue "profileid" -< tree
    solutionPath    <- getChildren >>> hasName "solutionPath" >>> getChildren >>> getText -< tree
    solutionFile    <- getChildren >>> hasName "solutionFile" >>> getChildren >>> getText -< tree
    target          <- getChildren >>> hasName "target" >>> getChildren >>> getText -< tree
    configuration   <- getChildren >>> hasName "configuration" >>> getChildren >>> getText -< tree
    returnA -< (pid, profile, solutionPath, solutionFile, target, configuration)

loadVisualStudios :: String -> IO [RawVisualStudio]
loadVisualStudios xmlFile = do
    trees <- runX (getVisualStudioListTrees xmlFile)
    return $ concat $ map (runLA mapToCbilVisualStudio) trees

initVisualStudios :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initVisualStudios configuration profileDefines = do
    rawVisualStudiosList <- liftIO $ loadAllSettingsFiles configuration loadVisualStudios
    let
        profileList = cbilProfileList configuration
        normalRun = cbilRunType configuration == NormalRun
        rawVisualStudiosForProfile = filter (\(_, p, _, _, _, _) -> profileInList p profileList) rawVisualStudiosList
    rules <- mapM ((mkVisualStudioRule normalRun) . (mkVisualStudios profileList profileDefines)) rawVisualStudiosForProfile
    return ("VisualStudioSolutionGroups", rules)

mkVisualStudioRule :: Bool -> VisualStudio -> Rules String
mkVisualStudioRule normalRun (VisualStudio pid profile solutionPath solutionFile target configuration) = do
    let
        _visualstudio pathToSolution solutionFilename target configuration = do
            putNormal $ "Executing MSBuild: " ++ intercalate ", " ["id: "++pid, "solutionPath(Cwd): "++pathToSolution, "solutionFileName: "++solutionFilename, "target: "++target, "configuration: "++configuration]
            if normalRun then cmd [Cwd pathToSolution, AddPath ["c:\\Program Files (x86)\\MSBuild\\12.0\\Bin\\"] [] ] "MSBuild.exe" [solutionFilename, "/t:" ++ target, "/p:Configuration=" ++ configuration] else return ()
        
    phony pid $ do
        _visualstudio solutionPath solutionFile target configuration
    return pid

mkVisualStudios :: [String] -> ProfileDefineList -> RawVisualStudio -> VisualStudio
mkVisualStudios profileList profileDefines (pid, _, solutionPath', solutionFile', target', configuration') = let
        applyProfileDefines' = applyProfileDefines profileDefines
        solutionPath = applyProfileDefines' solutionPath'
        solutionFile = applyProfileDefines' solutionFile'
        target = applyProfileDefines' target'
        configuration = applyProfileDefines' configuration'
    in VisualStudio pid profileList solutionPath solutionFile target configuration
        
-- NetTiers -------------------
type RawNetTiersGroup = (String, String, String, String, String, String, FilePath)
data NetTiersGroup = NetTiersGroup {
        netTiersId :: String
        , netTiersProfile :: String
        , netTiersPath :: FilePath
        , nettiersTemplateLocation :: FilePath
        , templatedb :: String
        , db :: String
        , nettiersdir :: FilePath
    } deriving (Show)

getNetTiersGroupsTrees :: String -> IOSLA (XIOState s) a XmlTree
getNetTiersGroupsTrees xml = documentRoot xml >>> getChildren >>> hasName "NetTiersGroups" >>> getChildren >>> hasName "netTiers"

mapToCbilNetTiersGroup :: ArrowXml t => t XmlTree RawNetTiersGroup
mapToCbilNetTiersGroup = proc tree -> do
    pid                         <- getAttrValue "id" -< tree
    profile                     <- getAttrValue "profileid" -< tree
    netTiersPath                <- getChildren >>> hasName "netTiersPath" >>> getChildren >>> getText -< tree
    nettiersTemplateLocation    <- getChildren >>> hasName "nettiersTemplateLocation" >>> getChildren >>> getText -< tree
    templatedb                  <- getChildren >>> hasName "templatedb" >>> getChildren >>> getText -< tree
    db                          <- getChildren >>> hasName "db" >>> getChildren >>> getText -< tree
    nettiersdir                 <- getChildren >>> hasName "nettiersdir" >>> getChildren >>> getText -< tree

    returnA -< (pid, profile, netTiersPath, nettiersTemplateLocation, templatedb, db, nettiersdir)

loadNetTiersGroups :: String -> IO [RawNetTiersGroup]
loadNetTiersGroups xmlFile = do
    trees <- runX (getNetTiersGroupsTrees xmlFile)
    return $ concat $ map (runLA mapToCbilNetTiersGroup) trees

initNetTiersGroups :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initNetTiersGroups configuration profileDefines = do
    rawNetTiersGroupsList <- liftIO $ loadAllSettingsFiles configuration loadNetTiersGroups
    let
        profileList = cbilProfileList configuration
        normalRun = cbilRunType configuration == NormalRun
        rawNetTiersGroupsForProfile = filter (\(_, p, _, _, _, _, _) -> profileInList p profileList) rawNetTiersGroupsList
    rules <- mapM ((mkNetTiersGroupRule normalRun) . (mkNetTiersGroup profileList profileDefines)) rawNetTiersGroupsForProfile
    return ("NetTiersGroups", rules)

mkNetTiersGroupRule :: Bool -> NetTiersGroup -> Rules String
mkNetTiersGroupRule normalRun (NetTiersGroup netTiersGroupId pid netTiersPath nettiersTemplateLocation templatedb db nettiersdir) = do
    let        
        generateNettiers :: FilePath -> FilePath -> String -> String -> String -> Action ()
        generateNettiers netttiersPath nettiersTemplateLocation templatedb db nettiersdir = do
            let
                
                buildTemplatePath = nettiersdir </> "Build.xml"
                buildGenericPath = nettiersdir </> "BuildGeneric.xml"
            if normalRun
              then do
                liftIO $ sed buildTemplatePath buildGenericPath ("database=" ++ templatedb) ("database=" ++ db)
                cmd [Cwd netttiersPath, AddPath ["c:\\Program Files (x86)\\CodeSmith\\v3.2"] [] ] "cs.exe" ["/template:" ++ nettiersTemplateLocation, "/propertyset:" ++ buildGenericPath, "/property:OutputDirectory=" ++ nettiersdir]
              else
                return ()
        
    phony netTiersGroupId $ do
        putNormal $ "Executing NetTiersGroup: " ++ intercalate ", " ["id: "++netTiersGroupId, "netTiersPath(Cwd): "++netTiersPath, "nettiersTemplateLocation: "++nettiersTemplateLocation, "templatedb: "++templatedb, "db: "++db, "nettiersdir: "++nettiersdir]
        generateNettiers netTiersPath nettiersTemplateLocation templatedb db nettiersdir
    return netTiersGroupId
        
mkNetTiersGroup :: [String] -> ProfileDefineList -> RawNetTiersGroup -> NetTiersGroup
mkNetTiersGroup profileList profileDefines (netTiersGroupId, pid, netTiersPath', nettiersTemplateLocation', templatedb', db', nettiersdir') = let
        
        applyProfileDefines' = applyProfileDefines profileDefines
        netTiersPath = applyProfileDefines' netTiersPath'
        nettiersTemplateLocation = applyProfileDefines' nettiersTemplateLocation'
        templatedb = applyProfileDefines' templatedb'
        db = applyProfileDefines' db'
        nettiersdir = applyProfileDefines' nettiersdir'
    in NetTiersGroup netTiersGroupId pid netTiersPath nettiersTemplateLocation templatedb db nettiersdir
        
-- Extra helper function for general use ------------
sed :: FilePath -> FilePath -> String -> String -> IO ()
sed srcFile destFile replaceString withString = do
    f <- BS.readFile srcFile
    let
        updatedText = BSS.replace (BS8.pack replaceString) (BS8.pack withString) f
    BSL.writeFile destFile updatedText

_sqlcmd :: Bool -> FilePath -> String -> Maybe String -> String -> String -> String -> Action ()
_sqlcmd normalRun wd databaseGroupId servername' dbname dbdefinename scriptname = do
    let
        servername = maybe "." id servername'
        header title = putNormal $ title ++ intercalate ", " ["id: "++databaseGroupId, "wd: "++wd, "servername: "++servername, "dbname: "++dbname, "dbdefinename: "++dbdefinename, "scriptname: "++scriptname]
    if normalRun
      then do
        header "Executing sqlcmd: "
        command_ [Cwd wd, Shell] "sqlcmd" ["-S", servername, "-b", "-d", dbname, "-v", "DatabaseName="++dbdefinename, "-i", scriptname]
      else header "skipping sqlcmd: "

touch' :: FilePath -> IO ()
touch' fp = do
    SD.createDirectoryIfMissing True $ takeDirectory fp
    appendFile fp ""
        
touch :: FilePath -> Action ()
touch fp = do
    liftIO $ touch' fp

-- Helper function END --

-- Cbil Help ---------------
data Flags = ProfileOpt String | AltSettingsOpt String | RunFlag RunType | ShowHiddenRules | ExtraSettingsOpt String deriving (Eq)

flags = let
        profileBuilder :: Maybe String -> Either String Flags
        profileBuilder (Just p) = Right $ ProfileOpt p
        profileBuilder Nothing = Left ""
        
        settingsFileBuilder :: Maybe String -> Either String Flags
        settingsFileBuilder (Just s) = Right $ AltSettingsOpt s
        settingsFileBuilder Nothing = Left ""
        
        extraSettingsFileBuilder :: Maybe String -> Either String Flags
        extraSettingsFileBuilder (Just s) = Right $ ExtraSettingsOpt s
        extraSettingsFileBuilder Nothing = Left ""
    in [
        Option "" ["profile"] (OptArg profileBuilder "PROFILE") "Select a profile. (comma separated)"
        , Option "" ["settings"] (OptArg settingsFileBuilder "FILE") "Select an alternative setting XML file."
        , Option "" ["extra-settings"] (OptArg extraSettingsFileBuilder "EXTRAFILE") "Select extra setting XML files to load. (comma separated)"
        , Option "" ["dry-run"] (NoArg $ Right $ RunFlag DryRun) "Perform a dry run build."
        , Option "" ["show-hidden-rules"] (NoArg $ Right $ ShowHiddenRules) "Show all hidden rules."
    ]

getProfileOption :: [Flags] -> Maybe String
getProfileOption (ProfileOpt p:_) = Just p
getProfileOption (_:opts) = getProfileOption opts
getProfileOption [] = Nothing
        
getAltSettingsOpt :: [Flags] -> Maybe String
getAltSettingsOpt (AltSettingsOpt s:_) = Just s
getAltSettingsOpt (_:opts) = getAltSettingsOpt opts
getAltSettingsOpt [] = Nothing

getDryRunOpt :: [Flags] -> Maybe RunType
getDryRunOpt (RunFlag r:_) = Just r
getDryRunOpt (_:opts) = getDryRunOpt opts
getDryRunOpt [] = Nothing

getHiddenRulesOpt :: [Flags] -> Maybe Bool
getHiddenRulesOpt (ShowHiddenRules:_) = Just True
getHiddenRulesOpt (_:opts) = getHiddenRulesOpt opts
getHiddenRulesOpt [] = Nothing

getExtraSettingsOpt :: [Flags] -> Maybe String
getExtraSettingsOpt (ExtraSettingsOpt s:_) = Just s
getExtraSettingsOpt (_:opts) = getExtraSettingsOpt opts
getExtraSettingsOpt [] = Nothing
        
filterRules :: CbilConfiguration -> [String] -> [String]
filterRules configuration rules =
    if showHiddenTargets configuration then rules else filter (\s -> if length s > 0 then if head s == '_' then False else True else False) rules

cbilHelp :: CbilConfiguration -> ProfileDefineList -> [CbilRulesInfo] -> Rules ()
cbilHelp configuration profileDefines ruleInfos = do
    phony "help" $ do
        putNormal $ "cbil version (" ++ cbilVersion ++ ")"
        putNormal $ "Settings files: " ++ (show . cbilSettingsFiles) configuration
        putNormal $ "Building with profile: " ++ show (cbilProfileList configuration)
        putNormal "Available Targets:"
        putNormal $ "\tcbil version: _version"
        mapM_ (putNormal . (\(ruleGroup, rules) -> "\t" ++ ruleGroup ++ ": " ++ intercalate ", " (filterRules configuration rules) )) ruleInfos

versionRule :: Rules ()
versionRule = do   
    phony "_version" $ do
        putNormal $ "cbil version : " ++ cbilVersion

getMetaSettingsTrees :: String -> IOSLA (XIOState s) a XmlTree
getMetaSettingsTrees xml = documentRoot xml >>> getChildren >>> hasName "MetaSettings" >>> getChildren >>> hasName "settings"

type RawMetaSettings = (String, [String], [FilePath])

mapToCbilMetaSettings :: ArrowXml t => t XmlTree RawMetaSettings
mapToCbilMetaSettings = let
    in proc tree -> do
        pid         <- getAttrValue "profileid" -< tree
        profiles    <- listA (getChildren >>> hasName "profiles" >>> listA (getChildren >>> hasName "profile" >>> getChildren >>> getText)) -< tree
        settings    <- listA (getChildren >>> hasName "extraSettings" >>> listA (getChildren >>> hasName "extraSettingsFile" >>> getChildren >>> getText)) -< tree
        returnA -<  (pid, concat profiles, concat settings)
        
loadMetaSettings :: String -> IO [RawMetaSettings]
loadMetaSettings xmlFile = do
    trees <- runX (getMetaSettingsTrees xmlFile)
    return $ concat $ map (runLA mapToCbilMetaSettings) trees

filterMetaSettings :: [String] -> [RawMetaSettings] -> ([String], [FilePath])
filterMetaSettings profileList fullMetaSettings = let
        filteredRawMetaSettings = filter (\(p, pList, sList) -> profileInList p profileList) fullMetaSettings
        (profiles', settings')  =unzip $  map (\(p, pList, sList) -> (pList, sList)) filteredRawMetaSettings
    in (concat profiles', concat settings')

getProfileSettingsGroups :: [String] -> [FilePath] -> IO ([String], [FilePath])
getProfileSettingsGroups profiles files = return (profiles, files)

-- Cbil main -----------------------
_cbilMain :: (CbilConfiguration -> ProfileDefineList -> Rules ()) -> IO ()
_cbilMain userRules = shakeArgsWith shakeOptions flags $ \flags targets -> return $ Just $ do
        
    let
        profileList' = maybe ["default"] (splitOn ",") (getProfileOption flags)
        profileList = reverse profileList'
        xmlpath = maybe "cbil.xml" id (getAltSettingsOpt flags)
        extraSettingsFiles = maybe [] (splitOn ",") (getExtraSettingsOpt flags)
        runType = maybe NormalRun id (getDryRunOpt flags)
        hiddenRules = maybe False id (getHiddenRulesOpt flags)
        allParameterSettingsFiles' = concat [[xmlpath], extraSettingsFiles]

    metaSettings' <- liftIO $ mapM loadMetaSettings allParameterSettingsFiles'
        
    let
        (mProfiles, mSettings) = filterMetaSettings profileList (concat metaSettings')
        allProfileList = concat [profileList, mProfiles]
        allParameterSettingsFiles = concat [allParameterSettingsFiles', mSettings]
        configuration = CbilConfiguration allProfileList allParameterSettingsFiles runType hiddenRules
        
    settingsExists <- liftIO $ SD.doesFileExist xmlpath
    if settingsExists
      then do
        if null targets then want ["help"] else want targets
        profileDefines <- initProfileDefines configuration
        userRules configuration profileDefines
        versionRule
      else do
        want ["error"]
        phony "error" $  putNormal ("ERROR: Settings file does not exist: " ++ xmlpath)
