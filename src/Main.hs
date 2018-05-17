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

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree
    
main :: IO ()

documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml = readDocument [] xml >>> getChildren >>> hasName "cbil"

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

mkCloneRules :: [CloneProject] -> Rules [String]
mkCloneRules ps = do
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
    mkCloneRules cloneProjects

-- Needs --------------------

type NeedsList = (String, [String])

getNeedsListTrees :: String -> IOSLA (XIOState s) a XmlTree
getNeedsListTrees xml = documentRoot xml >>> getChildren >>> hasName "Needs" >>> getChildren >>> hasName "NeedsList"

mapToCbilNeeds :: ArrowXml t => t XmlTree NeedsList
mapToCbilNeeds = proc tree -> do
                         pid    <- getAttrValue "id" -< tree
                         nl     <- listA (getChildren >>> hasName "need" >>> getChildren >>> getText) -< tree
                         returnA -< (pid, nl)
    
loadNeeds :: String -> IO [NeedsList]
loadNeeds xmlFile = do
    trees <- runX (getNeedsListTrees xmlFile)
    return $ concat $ map (runLA mapToCbilNeeds) trees

mkNeedsRule :: NeedsList -> Rules ()
mkNeedsRule (pid, nl) = do
        phony pid $ do
            need $ reverse nl


mkNeedsRules :: [NeedsList] -> Rules [String]
mkNeedsRules nl = do
    mapM_ mkNeedsRule nl
    return $ map fst nl

initNeeds :: String -> FilePath -> Rules [String]
initNeeds profile cbilxml = do
    needsList <- liftIO $ loadNeeds cbilxml
    mkNeedsRules needsList

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
mapToCbilProfileDefines = proc tree -> do
    pid    <- getAttrValue "profileid" -< tree
    defineNames     <- getChildren >>> hasName "defines" >>> listA (getChildren >>> hasName "define" >>> getAttrValue "name") -< tree
    defineValues     <- getChildren >>> hasName "defines" >>> listA (getChildren >>> hasName "define" >>> getChildren >>> getText) -< tree

    returnA -< (pid, zip (map (\s -> "%" ++ s ++ "%") defineNames) defineValues)

mkProfileDefines :: String -> ProfileDefines -> Rules ProfileDefineList
mkProfileDefines profile profiles = return $ lookup profile profiles

initProfileDefines :: String -> FilePath -> Rules ProfileDefineList
initProfileDefines profile cbilxml = do
    profileDefines <- liftIO $ loadProfileDefines cbilxml
    mkProfileDefines profile profileDefines
        
applyProfileDefines :: String -> ProfileDefineList -> String
applyProfileDefines str Nothing = str
applyProfileDefines str (Just defines) = _applyProfileDefines str defines
    where
        _applyProfileDefines s d = foldr (\(k, v) a -> rep k v a) s d
        rep replaceString withString str = map (chr . fromEnum) $ BSL.unpack $ BSS.replace (BS8.pack replaceString) (BS8.pack withString) (BS8.pack str)    
-- ---------------------------------

main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    let
        profile = "prof1"
        xmlpath = "example.xml"
    want ["help"]

    profDefines <- initProfileDefines profile xmlpath

    phony "meme" $ do
        putNormal $ show profDefines
        putNormal $ applyProfileDefines "ABC%DatabaseName%DEF" profDefines
        
    cloneRuleNames <- initCloneProjects profile xmlpath
    needsRuleNames <- initNeeds profile xmlpath

    phony "help" $ do
        putNormal $ "cleantest, clone1 " ++ intercalate ", " cloneRuleNames ++ " : " ++ intercalate ", " needsRuleNames

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
