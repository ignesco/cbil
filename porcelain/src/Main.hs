{-# LANGUAGE Arrows #-}  --, NoMonomorphismRestriction
module Main where

import CbilLib
import CbilLib.Utils
        
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree

import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL        

import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.IO

type RawPorcelainSettings = [[([[(String, String)]], String, String, [[String]] , String, String, [[String]] )]]
data PorcelainSettings = PorcelainSettings {
        defines :: [(String, String)]
        , groupTemplate :: (FilePath, FilePath, [FilePath])
        , subTemplate :: (FilePath, FilePath, [FilePath])
    } deriving (Show)

mapXml = let
        root = getChildren >>> hasName "cbilporcelain" >>> getChildren

        dk = hasName "define" >>> getAttrValue "name"
        dv = hasName "define" >>> getChildren >>> getText
        defines = root >>> hasName "Defines" >>> getChildren >>> listA (dk &&& dv >>> arr2 (,))

        files sel = root >>> hasName "Template"  >>> getChildren >>> hasName sel >>> getChildren
        group = files "Group"
        sub = files "Sub"

        sourceDirectory r =         r >>> hasName "sourceDirectory" >>> getChildren >>> getText
        destinationDirectory r =    r >>> hasName "destinationDirectory" >>> getChildren >>> getText
        fileList r =                r >>> hasName "files" >>> getChildren >>> listA (hasName "file" >>> getChildren >>> getText )

    in proc tree -> do

        defines' <- listA defines -< tree

        gsd <- sourceDirectory group -< tree
        gdd <- destinationDirectory group -< tree
        gfiles <- listA (fileList group) -< tree
        
        ssd <- sourceDirectory sub -< tree
        sdd <- destinationDirectory sub -< tree
        sfiles <- listA (fileList sub) -< tree

        returnA -< (defines' , gsd, gdd, gfiles, ssd, sdd, sfiles)
        
getSettings :: RawPorcelainSettings -> PorcelainSettings
getSettings raw = let
        (d, gsd, gdd, gf, ssd, sdd, sf) = head $ concat raw
    in PorcelainSettings (concat d) (gsd, gdd, concat gf) (ssd, sdd, concat sf)

documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml =  readDocument [withTrace (-1)] xml

loadSettings :: String -> IO PorcelainSettings
loadSettings xmlFile = do
    trees <- runX (documentRoot xmlFile)

    let
        raw = map (runLA mapXml) trees
    return $ getSettings raw

data PorcelainOptions = PorcelainOptions {
        groupName :: Maybe String
        , subName :: Maybe String
        , settingsFile :: String
        , o ::  [PorcelainOption]
    } deriving (Show)

data PorcelainOption = Settings FilePath | GroupID String | SubID String | Help deriving (Show, Eq)

options = [
        Option [] ["settings"] (ReqArg Settings "SETTING_FILE") "Porcelain settings file"
        , Option "g" ["groupid"] (ReqArg GroupID "GROUP_ID") "GROUP_ID"
        , Option "s" ["subid"] (ReqArg SubID "SUB_ID") "SUB_ID"
        , Option "h" ["help"] (NoArg Help) "Show help"
    ]

getOptions :: [String] -> Either [String] PorcelainOptions
getOptions args = let
        sf = "porcelain.xml"

        getGroupID [] = Nothing
        getGroupID (o:os) = case o of
            GroupID s -> Just s
            otherwise -> getGroupID os
        
        getSubID [] = Nothing
        getSubID (o:os) = case o of
            SubID s -> Just s
            otherwise -> getSubID os
        
        (o, n, e) = getOpt Permute options args
        in case e of
            [] -> let
                    gn = getGroupID o
                    sn = getSubID o
                in Right (PorcelainOptions gn sn sf o)
            otherwise -> Left e

--data PorcelainSettings = PorcelainSettings {
--        defines :: [(String, String)]
--        , groupTemplate :: (FilePath, FilePath, [FilePath])
--        , subTemplate :: (FilePath, FilePath, [FilePath])
--    } deriving (Show)


applyGroupDefines :: Maybe String -> Maybe String -> PorcelainSettings -> PorcelainSettings
applyGroupDefines gid sid (PorcelainSettings defs' (gsd', gdd', gf') (ssd, sdd, sf)) = let
        defs'' = maybe defs' (\id -> ("GROUP_ID", id):defs') gid
        defs = map (\(k, v) -> (concat ["%", k, "%"], v) ) defs''
        applyDefines' d s = foldr (\(k, v) a -> rep k v a) s d
        rep replaceString withString str = map (chr . fromEnum) $ BSL.unpack $ BSS.replace (BS8.pack replaceString) (BS8.pack withString) (BS8.pack str)
        
        gsd = applyDefines' defs gsd'
        gdd = applyDefines' defs gdd'
        gf = map (applyDefines' defs) gf'
    in PorcelainSettings defs (gsd, gdd, gf) (ssd, sdd, sf)

data PorcelainManifest = PorcelainManifest [String] deriving (Show)

loadManifest :: PorcelainSettings -> IO PorcelainManifest
loadManifest po = let
        (_, dir, _) = groupTemplate po 
        manifestFile = dir </> "manifest.xml"
    in return $ PorcelainManifest [manifestFile]
        
main :: IO ()
main = do
    args <- getArgs
    let
        header = "Usage: ic [OPTION...] files..."
        usage [] = getProgName >>= (\prg -> hPutStrLn stderr (usageInfo prg options))
        usage errs = ioError (userError (concat errs ++ usageInfo header options))
            
    case getOptions args of
        Right (PorcelainOptions _ _ _ [Help]) -> usage []
        Right po@(PorcelainOptions gid sid settingsFile o) -> do
            settings' <- loadSettings settingsFile
            let
                settings = applyGroupDefines gid sid settings'
            manifest <- loadManifest settings
            
            putStrLn $ show settings
            putStrLn $ show po
            putStrLn $ show manifest
        
        Left errs -> usage errs

settings' = PorcelainSettings {defines = [("%SUB_ID%","S_001"),("%GROUP_ID%","G_XXX"),("%d1%","D1"),("%d2%","D2")], groupTemplate = ("Template/ID/Current","LocalWork/G_XXX/Current",["cbil.local.xml"]), subTemplate = ("Template/SUB/Current","LocalWork/S_001/Current",["cbil.local.xml"])}

options' = PorcelainOptions {groupName = Just "G_XXX", subName = Just "S_001", settingsFile = "porcelain.xml", o = [GroupID "G_XXX",SubID "S_001",Settings "porcelain.xml"]}
