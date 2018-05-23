{-# LANGUAGE Arrows,  NoMonomorphismRestriction #-}
module Main where

import Development.Shake
import Data.Char
import Data.List
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import System.Directory
import Development.Shake.FilePath

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree
    
main :: IO ()

documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml = readDocument [] xml >>> getChildren >>> hasName "cbil"

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

initProfileDefines :: String -> FilePath -> Rules ProfileDefineList
initProfileDefines profile cbilxml = do
    profileDefines <- liftIO $ loadProfileDefines cbilxml
    mkProfileDefines profile profileDefines
        
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
        scripts             <-  getChildren >>> hasName "scripts" >>> listA (getChildren >>> hasName "script" >>> scriptTuplesSelector) -< tree
        
        returnA -<  (pid, databaseGroupId, workingDirectory, scripts)

initDatabaseGroups :: String -> ProfileDefineList -> FilePath -> Rules [String]
initDatabaseGroups profile profileDefines cbilxml = do
    rawDatabaseGroupsList <- liftIO $ loadDatabaseGroups cbilxml
    let
        rawDbGroupsForProfile = filter (\(p, _, _, _) -> p == profile) rawDatabaseGroupsList
    mapM (mkDatabaseGroupRule . (mkDatabaseGroups profile profileDefines)) rawDbGroupsForProfile

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

type CloneProject = (String, String, String, String, String)
    
getCloneProjectTrees :: String -> IOSLA (XIOState s) a XmlTree
getCloneProjectTrees xml = documentRoot xml >>> getChildren >>> hasName "CloneProjects" >>> getChildren >>> hasName "project"
    
loadCloneProjects :: String -> IO [CloneProject]
loadCloneProjects xmlFile = do
    trees <- runX (getCloneProjectTrees xmlFile)
    return $ concat $ map (runLA mapToCbilCloneProjects) trees

mapToCbilCloneProjects :: ArrowXml t => t XmlTree CloneProject
mapToCbilCloneProjects = proc tree -> do
                         pid    <- getAttrValue "id" -< tree
                         wd     <- getChildren >>> hasName "workingDirectory" >>> getChildren >>> getText -< tree
                         rloc   <- getChildren >>> hasName "repoLocation" >>> getChildren >>> getText -< tree
                         br     <- getChildren >>> hasName "branch" >>> getChildren >>> getText -< tree
                         rname     <- getChildren >>> hasName "localRepoName" >>> getChildren >>> getText -< tree
    
                         returnA -< (pid, wd, rloc, br, rname)

mkCloneRules :: String -> [CloneProject] -> Rules [String]
mkCloneRules profile ps' = do
    let ps = filter (\(_, prof, _, _, _) -> prof == profile)  ps'
    mapM_ mkCloneRule ps
    return $ map (\(pid, _, _, _, _) -> pid) ps
        
mkCloneRule :: CloneProject -> Rules ()
mkCloneRule (pid, wd, rloc, br, rname) = do
    phony pid $ do
        putNormal $ pid ++ "!!!"
        command_ [Cwd wd] "git" ["clone", "--single-branch", "--branch", br, rloc, rname]

initCloneProjects :: String -> FilePath -> Rules [String]
initCloneProjects profile cbilxml = do
    cloneProjects <- liftIO $ loadCloneProjects cbilxml
    mkCloneRules profile cloneProjects

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

mkNeedsRules :: String -> [NeedsList] -> Rules [String]
mkNeedsRules profile nl' = do
    let nl = filter (\(_, prof, _ ) -> prof == profile) nl'
    mapM_ mkNeedsRule nl
    return $ map (\(pid, _, _) -> pid) nl

initNeeds :: String -> FilePath -> Rules [String]
initNeeds profile cbilxml = do
    needsList <- liftIO $ loadNeeds cbilxml
    mkNeedsRules profile needsList

-- ---------------------------------

-- Visual Studio -------------------
type VisualStudioList = (String, String, String, String, String, String)

getVisualStudioListTrees :: String -> IOSLA (XIOState s) a XmlTree
getVisualStudioListTrees xml = documentRoot xml >>> getChildren >>> hasName "VisualStudioSolutionGroups" >>> getChildren >>> hasName "visualStudioSolutionGroup"

mapToCbilVisualStudio :: ArrowXml t => t XmlTree VisualStudioList
mapToCbilVisualStudio = proc tree -> do
    pid             <- getAttrValue "id" -< tree
    profile         <- getAttrValue "profileid" -< tree
    solutionPath    <- getChildren >>> hasName "solutionPath" >>> getChildren >>> getText -< tree
    solutionFile    <- getChildren >>> hasName "solutionFile" >>> getChildren >>> getText -< tree
    target          <- getChildren >>> hasName "target" >>> getChildren >>> getText -< tree
    configuration   <- getChildren >>> hasName "configuration" >>> getChildren >>> getText -< tree
    returnA -< (pid, profile, solutionPath, solutionFile, target, configuration)

loadVisualStudios :: String -> IO [VisualStudioList]
loadVisualStudios xmlFile = do
    trees <- runX (getVisualStudioListTrees xmlFile)
    return $ concat $ map (runLA mapToCbilVisualStudio) trees

-- WIP START - visual studio and nettiers --
buildVisualStudioSolution :: FilePath -> FilePath -> String -> String -> Action()
buildVisualStudioSolution pathToSolution solutionFilename target configuration = do
    cmd [Cwd pathToSolution, AddPath ["c:\\Program Files (x86)\\MSBuild\\12.0\\Bin\\"] [] ] "MSBuild.exe" [solutionFilename, "/t:" ++ target, "/p:Configuration=" ++ configuration]

netttiersPath = "d:\\Projects\\nettiers-2.3.0"
nettiersTemplateLocation = netttiersPath </> "NetTiers.cst"

generateNettiers :: String -> String -> String -> Action ()
generateNettiers templatedb db nettiersdir = do
        buildDir <- liftIO $ getCurrentDirectory
        let
            outputPath = buildDir </> nettiersdir
            buildTemplatePath = outputPath </> "Build.xml"
            buildGenericPath = outputPath </> "BuildGeneric.xml"
        liftIO $ sed buildTemplatePath buildGenericPath ("database=" ++ templatedb) ("database=" ++ db)
        cmd [Cwd netttiersPath, AddPath ["c:\\Program Files (x86)\\CodeSmith\\v3.2"] [] ] "cs.exe" ["/template:" ++ nettiersTemplateLocation, "/propertyset:" ++ buildGenericPath, "/property:OutputDirectory=" ++ outputPath]

sed :: FilePath -> FilePath -> String -> String -> IO ()
sed srcFile destFile replaceString withString = do
    f <- BS.readFile srcFile
    let
        updatedText = BSS.replace (BS8.pack replaceString) (BS8.pack withString) f
    BSL.writeFile destFile updatedText
-- WIP END --

main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    let
        profile = "prof1"
        xmlpath = "example.xml"
    want ["help"]

    profDefines <- initProfileDefines profile xmlpath

    phony "meme" $ do
        putNormal $ show profDefines
        putNormal $ applyProfileDefines profDefines "ABC%DatabaseName%DEF"
        
    cloneRuleNames <- initCloneProjects profile xmlpath
    needsRuleNames <- initNeeds profile xmlpath
    databaseGroupsRuleNames <- initDatabaseGroups profile profDefines xmlpath

    phony "help" $ do
        putNormal $ "cleantest, clone1 " ++ intercalate ", " cloneRuleNames ++ " : " ++ intercalate ", " needsRuleNames ++ " : " ++ intercalate ", " databaseGroupsRuleNames

    phony "cleantest" $ do
        liftIO $ removeDirectoryRecursive "testRepos/buildArea/bob"
        liftIO $ removeDirectoryRecursive "testRepos/buildArea/bob2"

    phony "clone1" $ do
        command_ [Cwd "testRepos/buildArea"] "git" ["clone", "--single-branch", "--branch", "master", "../teb", "bob"]
        putNormal "clone1"
    
    phony "clone2" $ do
        command_ [Cwd "testRepos/buildArea"] "git" ["clone", "--single-branch", "--branch", "master", "../teb", "bob2"]
        putNormal "clone1"
        
    let
        basewd = "c:\\Users\\DB_UpgradeScripts"
        _dbdefine = "DB_43"
        _sqlcmd wd dbname dbdefinename script = do
            putNormal $ "Executing: " ++ script
            command_ [Cwd wd, Shell] "sqlcmd" ["-S", ".", "-b", "-d", dbname, "-v", "DatabaseName="++dbdefinename, "-i", script]
        
    phony "jr_baselinedb" $ do
        let
            wd = basewd
            sqlcmd = _sqlcmd wd "master" _dbdefine
        sqlcmd "DropDB.sql"
        sqlcmd "RestoreBaseline.sql"

    phony "jr_allcp" $ do
        let
            wd = basewd
            sqlcmd = _sqlcmd wd _dbdefine _dbdefine
        sqlcmd "LocalAppConfig.sql"
        sqlcmd "CP_0001.sql"
        sqlcmd "CP_0002.sql"
        sqlcmd "CP_0002_data_001.sql"
        sqlcmd "CP_0003.sql"
        
    phony "ab_JR_43" $ do
        let
            wd = basewd <//> "LocalWork\\JR-43\\Current"
            sqlcmd = _sqlcmd wd _dbdefine _dbdefine
        sqlcmd "Update-XX-YY-001.publish.sql"
