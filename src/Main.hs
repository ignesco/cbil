{-# LANGUAGE Arrows,  NoMonomorphismRestriction #-}
module Main where

import Development.Shake
import Data.Char
import Data.List
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import System.Console.GetOpt
import qualified System.Directory as SD
import Development.Shake.FilePath

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree
    
documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml = readDocument [] xml >>> getChildren >>> hasName "cbil"

-- Cbil data types -------------------
data CbilConfiguration = CbilConfiguration {
        cbilProfile :: String
        , cbilSettingsFile :: FilePath
    }

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

mkProfileDefines :: String -> ProfileDefines -> Rules ProfileDefineList
mkProfileDefines profile profiles = return $ lookup profile profiles

initProfileDefines :: CbilConfiguration -> Rules ProfileDefineList
initProfileDefines configuration = do
    profileDefines <- liftIO $ loadProfileDefines (cbilSettingsFile configuration)
    mkProfileDefines (cbilProfile configuration) profileDefines

applyProfileDefines :: ProfileDefineList -> String -> String
applyProfileDefines Nothing str = str
applyProfileDefines (Just defines) str = _applyProfileDefines defines str
    where
        _applyProfileDefines d s = foldr (\(k, v) a -> rep k v a) s d
        rep replaceString withString str = map (chr . fromEnum) $ BSL.unpack $ BSS.replace (BS8.pack replaceString) (BS8.pack withString) (BS8.pack str)    

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
            , scripts :: [Script]
        }
        deriving (Show)

type DatabaseGroups = [DatabaseGroup]
type RawDatabaseGroup = (String, String, FilePath, [(String, String, String)])
        
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
        scripts             <- getChildren >>> hasName "scripts" >>> listA (getChildren >>> hasName "script" >>> scriptTuplesSelector) -< tree
        
        returnA -<  (pid, databaseGroupId, workingDirectory, scripts)

initDatabaseGroups :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initDatabaseGroups configuration profileDefines = do
    rawDatabaseGroupsList <- liftIO $ loadDatabaseGroups (cbilSettingsFile configuration)
    let
        profile = (cbilProfile configuration)
        rawDbGroupsForProfile = filter (\(p, _, _, _) -> p == profile) rawDatabaseGroupsList
    rules <- mapM (mkDatabaseGroupRule . (mkDatabaseGroups profile profileDefines)) rawDbGroupsForProfile
    return ("DatabaseGroups", rules)

mkDatabaseGroupRule :: DatabaseGroup -> Rules String
mkDatabaseGroupRule (DatabaseGroup pid databaseGroupId workingDirectory scripts) = do
    let
        _sqlcmd wd dbname dbdefinename scriptname = do
            putNormal $ concat ["Executing sqlcmd: ", wd, " : ", dbname, " : ", dbdefinename, " : ", scriptname]
            command_ [Cwd wd, Shell] "sqlcmd" ["-S", ".", "-b", "-d", dbname, "-v", "DatabaseName="++dbdefinename, "-i", scriptname]

        mk_sqlcmd wd script = _sqlcmd wd (dbname script) (dbdefinename script) (scriptname script)
        
    phony databaseGroupId $ do
        putNormal $ show (pid, databaseGroupId, workingDirectory, scripts)
        mapM_ (mk_sqlcmd workingDirectory) scripts
    return databaseGroupId

mkDatabaseGroups :: String -> ProfileDefineList -> RawDatabaseGroup -> DatabaseGroup
mkDatabaseGroups profile profileDefines (pid, databaseGroupId, workingDirectory', scripts') = let
        
        applyProfileDefines' = applyProfileDefines profileDefines
        workingDirectory = applyProfileDefines' workingDirectory'
        scripts = map (\(dbname', dbdefinename', scriptname') -> DatabaseGroupScript (applyProfileDefines' dbname') (applyProfileDefines' dbdefinename') (applyProfileDefines' scriptname') ) scripts'
    
    in DatabaseGroup pid databaseGroupId workingDirectory scripts
                
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
    rname   <- getChildren >>> hasName "localRepoName" >>> getChildren >>> getText -< tree

    returnA -< (pid, profid, wd, rloc, br, rname)

mkCloneRules :: String -> [CloneProject] -> Rules CbilRulesInfo
mkCloneRules profile ps' = do
    let ps = filter (\(_, prof, _, _, _, _) -> prof == profile)  ps'
    rules <- mapM mkCloneRule ps
    return ("CloneProjects", rules)
        
mkCloneRule :: CloneProject -> Rules String
mkCloneRule (pid, _, wd, rloc, br, rname) = do
    phony pid $ do
        putNormal $ "CloneProject: " ++ intercalate " : " [br, rloc, rname]
        command_ [Cwd wd] "git" ["clone", "--single-branch", "--branch", br, rloc, rname]
    return pid

initCloneProjects :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initCloneProjects configuration _ = do
    cloneProjects <- liftIO $ loadCloneProjects (cbilSettingsFile configuration)
    mkCloneRules (cbilProfile configuration) cloneProjects

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

mkNeedsRule :: NeedsList -> Rules ()
mkNeedsRule (pid, _, nl) = do
        phony pid $ do
            need $ reverse nl

mkNeedsRules :: String -> [NeedsList] -> Rules CbilRulesInfo
mkNeedsRules profile nl' = do
    let nl = filter (\(_, prof, _ ) -> prof == profile) nl'
    mapM_ mkNeedsRule nl
    return $ ("Needs", map (\(pid, _, _) -> pid) nl)

initNeeds :: CbilConfiguration -> ProfileDefineList -> Rules CbilRulesInfo
initNeeds configuration _ = do
    needsList <- liftIO $ loadNeeds (cbilSettingsFile configuration)
    mkNeedsRules (cbilProfile configuration) needsList

-- Visual Studio -------------------
        
type RawVisualStudio = (String, String, String, String, String, String)
data VisualStudio = VisualStudio {
        visualStudioId :: String
        , visualStudioProfile :: String
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
    rawVisualStudiosList <- liftIO $ loadVisualStudios (cbilSettingsFile configuration)
    let
        profile = (cbilProfile configuration)
        rawVisualStudiosForProfile = filter (\(_, p, _, _, _, _) -> p == profile) rawVisualStudiosList
    rules <- mapM (mkVisualStudioRule . (mkVisualStudios profile profileDefines)) rawVisualStudiosForProfile
    return ("VisualStudioSolutionGroups", rules)

mkVisualStudioRule :: VisualStudio -> Rules String
mkVisualStudioRule (VisualStudio pid profile solutionPath solutionFile target configuration) = do
    let
        _visualstudio pathToSolution solutionFilename target configuration = do
            putNormal $ concat ["Executing MSBuild: ", pathToSolution, " : ", solutionFilename, " : ", target, " : ", configuration]
            cmd [Cwd pathToSolution, AddPath ["c:\\Program Files (x86)\\MSBuild\\12.0\\Bin\\"] [] ] "MSBuild.exe" [solutionFilename, "/t:" ++ target, "/p:Configuration=" ++ configuration]
        
    phony pid $ do
        _visualstudio solutionPath solutionFile target configuration
    return pid

mkVisualStudios :: String -> ProfileDefineList -> RawVisualStudio -> VisualStudio
mkVisualStudios profile profileDefines (pid, _, solutionPath', solutionFile', target', configuration') = let
        applyProfileDefines' = applyProfileDefines profileDefines
        solutionPath = applyProfileDefines' solutionPath'
        solutionFile = applyProfileDefines' solutionFile'
        target = applyProfileDefines' target'
        configuration = applyProfileDefines' configuration'
    in VisualStudio pid profile solutionPath solutionFile target configuration
        
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
    rawNetTiersGroupsList <- liftIO $ loadNetTiersGroups (cbilSettingsFile configuration)
    let
        profile = (cbilProfile configuration)
        rawNetTiersGroupsForProfile = filter (\(_, p, _, _, _, _, _) -> p == profile) rawNetTiersGroupsList
    rules <- mapM (mkNetTiersGroupRule . (mkNetTiersGroup profile profileDefines)) rawNetTiersGroupsForProfile
    return ("NetTiersGroups", rules)

mkNetTiersGroupRule :: NetTiersGroup -> Rules String
mkNetTiersGroupRule (NetTiersGroup netTiersGroupId pid netTiersPath nettiersTemplateLocation templatedb db nettiersdir) = do
    let        
        generateNettiers :: FilePath -> FilePath -> String -> String -> String -> Action ()
        generateNettiers netttiersPath nettiersTemplateLocation templatedb db nettiersdir = do
            buildDir <- liftIO $ SD.getCurrentDirectory
            let
                outputPath = buildDir </> nettiersdir
                buildTemplatePath = outputPath </> "Build.xml"
                buildGenericPath = outputPath </> "BuildGeneric.xml"
            liftIO $ sed buildTemplatePath buildGenericPath ("database=" ++ templatedb) ("database=" ++ db)
            cmd [Cwd netttiersPath, AddPath ["c:\\Program Files (x86)\\CodeSmith\\v3.2"] [] ] "cs.exe" ["/template:" ++ nettiersTemplateLocation, "/propertyset:" ++ buildGenericPath, "/property:OutputDirectory=" ++ outputPath]
        
    phony netTiersGroupId $ do
        putNormal $ show (netTiersGroupId, pid, netTiersPath, nettiersTemplateLocation, templatedb, db, nettiersdir)
        generateNettiers netTiersPath nettiersTemplateLocation templatedb db nettiersdir
    return netTiersGroupId
        
mkNetTiersGroup :: String -> ProfileDefineList -> RawNetTiersGroup -> NetTiersGroup
mkNetTiersGroup profile profileDefines (netTiersGroupId, pid, netTiersPath', nettiersTemplateLocation', templatedb', db', nettiersdir') = let
        
        applyProfileDefines' = applyProfileDefines profileDefines
        netTiersPath = applyProfileDefines' netTiersPath'
        nettiersTemplateLocation = applyProfileDefines' nettiersTemplateLocation'
        templatedb = applyProfileDefines' templatedb'
        db = applyProfileDefines' db'
        nettiersdir = applyProfileDefines' nettiersdir'
    in NetTiersGroup netTiersGroupId pid netTiersPath nettiersTemplateLocation templatedb db nettiersdir
        
-- Extra helper function for NetTiers ------------
sed :: FilePath -> FilePath -> String -> String -> IO ()
sed srcFile destFile replaceString withString = do
    f <- BS.readFile srcFile
    let
        updatedText = BSS.replace (BS8.pack replaceString) (BS8.pack withString) f
    BSL.writeFile destFile updatedText
-- Helper function END --

-- Cbil Help ---------------
data Flags = ProfileOpt String | AltSettingsOpt String deriving Eq

flags = let
        profileBuilder :: Maybe String -> Either String Flags
        profileBuilder (Just p) = Right $ ProfileOpt p
        profileBuilder Nothing = Left ""
        
        settingsFileBuilder :: Maybe String -> Either String Flags
        settingsFileBuilder (Just s) = Right $ AltSettingsOpt s
        settingsFileBuilder Nothing = Left ""
    in [
        Option "" ["profile"] (OptArg profileBuilder "PROFILE") "Select a profile."
        , Option "" ["settings"] (OptArg settingsFileBuilder "FILE") "Select an alternative setting XML file."
    ]

getProfileOption :: [Flags] -> Maybe String
getProfileOption (ProfileOpt p:_) = Just p
getProfileOption (_:opts) = getProfileOption opts
getProfileOption [] = Nothing
        
getAltSettingsOpt :: [Flags] -> Maybe String
getAltSettingsOpt (AltSettingsOpt s:_) = Just s
getAltSettingsOpt (_:opts) = getAltSettingsOpt opts
getAltSettingsOpt [] = Nothing
      
cbilHelp :: CbilConfiguration -> ProfileDefineList -> [CbilRulesInfo] -> Rules ()
cbilHelp configuration profileDefines ruleInfos = do
    phony "help" $ do
        putNormal $ "Settings file: " ++ (cbilSettingsFile configuration)
        putNormal $ "Building with profile: " ++ (cbilProfile configuration)
        putNormal "Available Targets:"
        mapM_ (putNormal . (\(ruleGroup, rules) -> "\t" ++ ruleGroup ++ ": " ++ intercalate ", " rules)) ruleInfos

-- Cbil main -----------------------
_cbilMain :: (CbilConfiguration -> ProfileDefineList -> Rules ()) -> IO ()
_cbilMain userRules = shakeArgsWith shakeOptions flags $ \flags targets -> return $ Just $ do
        
    let
        profile = maybe "default" id (getProfileOption flags)
        xmlpath = maybe "cbil.xml" id (getAltSettingsOpt flags)
        configuration = CbilConfiguration profile xmlpath

    settingsExists <- liftIO $ SD.doesFileExist xmlpath
    if settingsExists
      then do
        if null targets then want ["help"] else want targets
        profileDefines <- initProfileDefines configuration
        userRules configuration profileDefines
      else do
        want ["error"]
        phony "error" $  putNormal ("ERROR: Settings file does not exist: " ++ xmlpath)

-- main ------------------

main :: IO ()
main = _cbilMain $ (\configuration profileDefines -> do

        -- build the module rules
        cloneRulesInfo <- initCloneProjects configuration profileDefines
        needsRulesInfo <- initNeeds configuration profileDefines
        databaseGroupsRulesInfo <- initDatabaseGroups configuration profileDefines
        visualStudioRulesInfo <- initVisualStudios configuration profileDefines
        netTiersGroupsRulesInfo <- initNetTiersGroups configuration profileDefines

        -- build the help rule
        cbilHelp configuration profileDefines [cloneRulesInfo, needsRulesInfo, databaseGroupsRulesInfo, visualStudioRulesInfo, netTiersGroupsRulesInfo]
    )
