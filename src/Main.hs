{-# LANGUAGE Arrows,  NoMonomorphismRestriction #-}
module Main where

import Development.Shake
import Data.List

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

{-
mkCloneRule ::  -> Rules ()
mkCloneRule (pid, nsl) = do
    phony pid $ do
        putNormal $ pid ++ "!!!"
        command_ [Cwd wd] "git" ["clone", "--single-branch", "--branch", br, rloc, rname]
-}


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
    
-- ---------------------------------

main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    
    want ["help"]

    cloneRuleNames <- initCloneProjects "prof1" "example.xml"
    needsRuleNames <- initNeeds "prof1" "example.xml"

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
