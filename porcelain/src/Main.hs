{-# LANGUAGE Arrows #-}
module Main where

import CbilLib
import qualified CbilLib.Utils as U
        
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree

import qualified Data.ByteString as BS
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL        

import Data.Char
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

type Defines = [(String, String)]
data CbSettings = CbSettings {
        cbSettings :: Maybe FilePath
        , cbProfile :: Maybe String
        , cbExtraSettings :: Maybe String
    } deriving (Show)

type RawPorcelainSettings = [[ ( [Defines], [String], [String], [String], String, String, String, String, [[String]] , String, String, [[String]] )  ]]
data PorcelainSettings = PorcelainSettings {
        defines :: Defines
        , cSettings :: CbSettings
        , groupTemplate :: (FilePath, FilePath, FilePath, String, [FilePath])
        , subTemplate :: (FilePath, FilePath, [FilePath])
    } deriving (Show)

mapSettingsXml = let
        root = getChildren >>> hasName "cbilporcelain" >>> getChildren

        dk = hasName "define" >>> getAttrValue "name"
        dv = hasName "define" >>> getChildren >>> getText
        defines = root >>> hasName "Defines" >>> getChildren >>> listA (dk &&& dv >>> arr2 (,))

        cSettings = root >>> hasName "CbilDefaults" >>> getChildren >>> hasName "settings" >>> getChildren >>> getText
        cProfile = root >>> hasName "CbilDefaults" >>> getChildren >>> hasName "profile" >>> getChildren >>> getText
        cExtraSettings = root >>> hasName "CbilDefaults" >>> getChildren >>> hasName "extraSettings" >>> getChildren >>> getText

        files sel = root >>> hasName "Template"  >>> getChildren >>> hasName sel >>> getChildren
        group = files "Group"
        sub = files "Sub"

        sourceDirectory r =         r >>> hasName "sourceDirectory" >>> getChildren >>> getText
        destinationDirectory r =    r >>> hasName "destinationDirectory" >>> getChildren >>> getText
        groupCbilTemplate r =       r >>> hasName "cbilSettingsFile" >>> getChildren >>> getText
        groupCbilDbNeedTemplate r = r >>> hasName "cbilDbNeed" >>> getChildren >>> getText
        fileList r =                r >>> hasName "files" >>> getChildren >>> listA (hasName "file" >>> getChildren >>> getText )

    in proc tree -> do
        defines' <- listA defines -< tree
        c1 <- listA cSettings -< tree
        c2 <- listA cProfile -< tree
        c3 <- listA cExtraSettings -< tree

        gsd <- sourceDirectory group -< tree
        gdd <- destinationDirectory group -< tree
        gct <- groupCbilTemplate group -< tree
        gcdn <- groupCbilDbNeedTemplate group -< tree
        gfiles <- listA (fileList group) -< tree
        
        ssd <- sourceDirectory sub -< tree
        sdd <- destinationDirectory sub -< tree
        sfiles <- listA (fileList sub) -< tree

        returnA -< (defines', c1, c2, c3 , gsd, gdd, gct, gcdn, gfiles, ssd, sdd, sfiles)
        
getSettings :: RawPorcelainSettings -> PorcelainSettings
getSettings raw = let
        (d, c1, c2, c3, gsd, gdd, gct, gcdn, gf, ssd, sdd, sf) = head $ concat raw
    in PorcelainSettings (concat d) (CbSettings ((listToMaybe ) c1) ((listToMaybe ) c2) ((listToMaybe ) c3)) (gsd, gdd, gct, gcdn, concat gf) (ssd, sdd, concat sf)

documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml =  readDocument [withTrace (-1)] xml

loadSettings :: String -> IO PorcelainSettings
loadSettings xmlFile = do
    trees <- runX (documentRoot xmlFile)

    let
        raw = map (runLA mapSettingsXml) trees
    return $ getSettings raw

data PorcelainOptions = PorcelainOptions {
        groupName :: Maybe String
        , settingsFile :: String
        , cbilSettings :: Maybe FilePath
        , cbilProfile :: Maybe String
        , cbilExtraSettings :: Maybe String
        , cbilDryRun :: Bool
        , n :: [String]
        , o ::  [PorcelainOption]
    } deriving (Show)

data PorcelainOption = Settings FilePath | GroupID String | CbilSettings String | CbilProfile String | CbilExtraSettings String | CbilDryRun | Help deriving (Show, Eq)

options = [
        Option [] ["settings"] (ReqArg Settings "SETTING_FILE") "Porcelain settings file"
        , Option "g" ["groupid"] (ReqArg GroupID "GROUP_ID") "GROUP_ID"
        , Option "s" ["cbil-settings"] (ReqArg CbilSettings "CBIL_SETTINGS_FILE") "CBIL_SETTINGS_FILE"
        , Option "p" ["cbil-profile"] (ReqArg CbilProfile "CBIL_PROFILE") "CBIL_PROFILE"
        , Option "e" ["cbil-extra-settings"] (ReqArg CbilExtraSettings "CBIL_EXTRA_SETTINGS") "CBIL_EXTRA_SETTINGS"
        , Option "d" ["cbil-dry-run"] (NoArg CbilDryRun) "Cbil Dry run"
        , Option "h" ["help"] (NoArg Help) "Show help"
    ]

getOptions :: [String] -> Either [String] PorcelainOptions
getOptions args = let
        sf = "porcelain.xml"

        getGroupID [] = Nothing
        getGroupID (o:os) = case o of
            GroupID s -> Just s
            otherwise -> getGroupID os

        getCbilSettings [] = Nothing
        getCbilSettings (o:os) = case o of
            CbilSettings s -> Just s
            otherwise -> getCbilSettings os
        
        getCbilProfile [] = Nothing
        getCbilProfile (o:os) = case o of
            CbilProfile s -> Just s
            otherwise -> getCbilProfile os
        
        getCbilExtraSettings [] = Nothing
        getCbilExtraSettings (o:os) = case o of
            CbilExtraSettings s -> Just s
            otherwise -> getCbilExtraSettings os
        
        getCbilDryRun [] = False
        getCbilDryRun (o:os) = case o of
            CbilDryRun -> True
            otherwise -> getCbilDryRun os

        (o, n, e) = getOpt Permute options args
        in case e of
            [] -> Right (PorcelainOptions (getGroupID o) sf (getCbilSettings o) (getCbilProfile o) (getCbilExtraSettings o) (getCbilDryRun o) n o)
            otherwise -> Left e

applyDefines :: Defines -> String -> String
applyDefines d s = foldr (\(k, v) a -> replaceString k v a) s d
    where
        replaceString sString dString str = map (chr . fromEnum) $ BSL.unpack $ BSS.replace (BS8.pack sString) (BS8.pack dString) (BS8.pack str)
        
applyGroupDefines :: Maybe String -> PorcelainSettings -> PorcelainSettings
applyGroupDefines gid (PorcelainSettings defs' cbopts (gsd', gdd', cs, cn, gf') (ssd, sdd, sf)) = let
        defs = maybe defs' (\id -> ("%GROUP_ID%", id):defs') gid
        
        gsd = applyDefines defs gsd'
        gdd = applyDefines defs gdd'
        gf = map (applyDefines defs) gf'
    in PorcelainSettings defs cbopts (gsd, gdd, cs, cn, gf) (ssd, sdd, sf)

applySubDefines :: String -> PorcelainSettings -> PorcelainSettings
applySubDefines sid (PorcelainSettings defs' cbopts (gsd, gdd, cs, cn, gf) (ssd', sdd', sf')) = let
        defs = (("%SUB_ID%", sid):defs')
        
        ssd = applyDefines defs ssd'
        sdd = applyDefines defs sdd'
        sf = map (applyDefines defs) sf'
    in PorcelainSettings defs cbopts (gsd, gdd, cs, cn, gf) (ssd, sdd, sf)

addManifestDefines :: PorcelainManifest -> PorcelainSettings -> PorcelainSettings
addManifestDefines (PorcelainManifest subs) settings = let
        (_, _, cbilFileTemplate, cbilDbNeedsTemplate, _) = groupTemplate settings
        profiles = concat $ map (\s -> concat ["<profile>", s, "</profile>"]) subs
        subfiles = concat $ map (\s -> concat ["<extraSettingsFile>", applyDefines (("%SUB_ID%", s):defines settings) cbilFileTemplate, "</extraSettingsFile>"]) subs
        dbNeeds  = concat $ map (\s -> concat ["<need>", applyDefines (("%SUB_ID%", s):defines settings) cbilDbNeedsTemplate, "</need>"]) subs

    in settings {defines = ( ("%SUB_PROFILES%", profiles):("%SUB_FILES%", subfiles):("%SUB_DBNEED%", dbNeeds):defines settings)  }

data PorcelainManifest = PorcelainManifest [String] deriving (Show)

mapManifestXml = let
        root = getChildren >>> hasName "cbilporcelainmanifest" >>> getChildren
        subs = root >>> hasName "Subs" >>> getChildren >>> listA (hasName "Sub" >>> getChildren >>> getText)

    in proc tree -> do
        subs' <- listA subs -< tree
        returnA -< subs'

type RawPorcelainManifest = [[[[String]]]]
getManifest :: RawPorcelainManifest -> PorcelainManifest
getManifest raw = PorcelainManifest $ concat $ concat $ concat raw
        
loadManifest' :: FilePath -> IO PorcelainManifest
loadManifest' xmlFile = do
    trees <- runX (documentRoot xmlFile)

    let
        raw = map (runLA mapManifestXml) trees
    return $ getManifest raw

getManifestFile :: PorcelainSettings -> FilePath
getManifestFile settings = let (_, dir, _, _, _) = groupTemplate settings in dir </> "manifest.xml"

loadManifest :: PorcelainSettings -> IO PorcelainManifest
loadManifest settings = let
        manifestFile = getManifestFile settings
    in do
        exists <- doesFileExist manifestFile
        if exists then loadManifest' manifestFile
        else return $ PorcelainManifest []

saveManifest :: PorcelainSettings -> PorcelainManifest -> IO ()
saveManifest settings (PorcelainManifest subs) = do
    let
        manifestFile = getManifestFile settings
        contents = concat $ concat [["<cbilporcelainmanifest>", "<Subs>"] , map (\s -> "<Sub>" ++ s ++ "</Sub>" ) subs, ["</Subs>", "</cbilporcelainmanifest>"] ]
    putStrLn $ "save manifest " ++ manifestFile
    writeFile manifestFile contents

type ExecuteResult = IO (Maybe [String])

sed :: FilePath -> FilePath -> String -> String -> IO ()
--FOR DEBUG sed s d k v = putStrLn $ intercalate "!" ["SED", s, d, k, v]
sed = U.sed
        
copyTemplateWithInitial s d [] = do
    f <- BS.readFile s
    BS.writeFile d f
copyTemplateWithInitial s d [(k, v)] = sed s d k v 
copyTemplateWithInitial s d ((k, v):ds) = do
    let
        copyRemainder s d (k, v) = sed s d k v
    copyRemainder s d (k, v)
    mapM_ (copyRemainder d d) ds

copyTemplate :: Defines -> FilePath -> FilePath -> FilePath -> IO ()
copyTemplate defs sd dd f = do
    let
        src = sd </> f
        dest = dd </> f
    putStrLn $ "\t Copy template: " ++ src ++ " -> " ++ dest
    createDirectoryIfMissing True dd
    copyTemplateWithInitial src dest defs

manifestAddSub :: PorcelainManifest -> String -> PorcelainManifest
manifestAddSub m@(PorcelainManifest subs) subid = if subid `elem` subs then m else PorcelainManifest $ concat [subs, [subid]]

buildCbilParams po settings subid subsettings = let
        s = cSettings settings
        _finalSettings = sequence $ filter isJust [cbSettings s, cbilSettings po]
        _finalProfile = sequence $ filter isJust [cbProfile s, cbilProfile po, subid]
        _finalExtraSettings = sequence $ filter isJust [cbExtraSettings s, cbilExtraSettings po, subsettings]

        addFlag s m = if length m == 0 then Nothing else (Just . concat) (s:[intercalate "," m])

    in sequence $ filter isJust $ case (_finalSettings, _finalProfile, _finalExtraSettings) of
            (Just finalSettings, Just finalProfile, Just finalExtraSettings) -> [if cbilDryRun po then (Just "--dry-run") else Nothing, addFlag "--settings=" finalSettings, addFlag "--profile=" finalProfile, addFlag "--extra-settings=" finalExtraSettings]

_executeCbil :: PorcelainOptions -> PorcelainSettings -> Maybe String -> Maybe String -> [String] -> ExecuteResult
_executeCbil po settings subid subsettings targets = do
    let
        _params = buildCbilParams po settings subid subsettings
                    
    case _params of
        Just params -> do
            let allParams = params ++ targets
            putStrLn $ "Running cbil with params: " ++ intercalate " " allParams
            callProcess "cbil" allParams
            return Nothing
        Nothing -> return $ Just ["should not get here [1]"]

executeCbil :: PorcelainOptions -> PorcelainSettings -> [String] -> ExecuteResult
executeCbil po settings targets = _executeCbil po settings Nothing Nothing targets

executeGroupCbil :: PorcelainOptions -> PorcelainSettings -> String -> [String] -> ExecuteResult
executeGroupCbil po settings subid targets = let
        
        defs' = defines settings
        defs = ("%SUB_ID%", subid):defs'
        (_, _, cbilxml', _, _) = groupTemplate settings
        cbilxml = applyDefines defs cbilxml'
        
    in _executeCbil po settings (Just subid) (Just cbilxml) targets

executeSubNew :: PorcelainOptions -> PorcelainSettings -> PorcelainManifest -> String -> ExecuteResult
executeSubNew po settings'' manifest' subid = do
    let
        settings' = applyGroupDefines (groupName po) settings''
        settings = applySubDefines subid settings''
        
        (ssd, sdd, sf) = subTemplate settings
        
    putStrLn $ "Create new sub:" ++ subid
    putStrLn $ "\t Create directory: " ++ sdd
    filesExist' <- mapM (\f -> (doesFileExist (sdd </> f)) >>= (\b -> return (b, sdd </> f))) sf
    let filesThatExist = map snd $ filter fst filesExist'

    if (length filesThatExist == 0)
      then do
        mapM_ (copyTemplate (defines settings) ssd sdd) sf
        let manifest = manifestAddSub manifest' subid
        saveManifest settings manifest
        executeGroupUpdateNew False po settings' manifest (groupName po)
        return Nothing
      else return $ Just $ concat [["ERROR file(s) already exist "], [intercalate ", " filesThatExist], ["\n"]]
        
executeGroupUpdateNew :: Bool -> PorcelainOptions -> PorcelainSettings -> PorcelainManifest -> Maybe String -> ExecuteResult
executeGroupUpdateNew mode po settings manifest Nothing = return $ Just ["Group not set"]
executeGroupUpdateNew mode po settings'' manifest groupid@(Just gid) = do
    let
        settings' = applyGroupDefines groupid settings''
        settings = addManifestDefines manifest settings'
        (gsd, gdd, _, _, gf) = groupTemplate settings
        action = if mode then "Create new" else "Update"
        defines' = defines settings
        
    putStrLn $ action ++ " group: " ++ gid
    putStrLn $ "\t " ++ action ++ " directory: " ++ gdd
    mapM_ (copyTemplate defines' gsd gdd) gf
    
    return Nothing

execute :: PorcelainOptions -> PorcelainSettings -> ExecuteResult
execute po settings'' = let
        d = map (\(k, v) -> (concat ["%", k, "%"], v) ) (defines settings'')
        settings' = settings'' { defines = d }
    in case n po of
        ["group", "new", groupid] -> do
            let settings = applyGroupDefines (Just groupid) settings'
            manifest <- loadManifest settings
            executeGroupUpdateNew True po settings manifest (Just groupid)
        
        ["sub", "new", subid] -> do
            let settings = applyGroupDefines (groupName po) settings'
            manifest <- loadManifest settings
            executeSubNew po settings manifest subid
        
        ("build":targets) -> do
            executeCbil po settings' targets
        
        ("groupbuild":id:targets) -> do
            executeGroupCbil po settings' id targets
        otherwise -> return $ Just ["ERROR"]

main' :: Either [String] PorcelainOptions -> IO ()
main' opts = do
    let
        header = " Usage: XXXXX [OPTION...] group new GID / sub new SID / build TARGETS / groupbuild ID targets"
        usage [] = getProgName >>= (\prg -> hPutStrLn stderr (usageInfo prg options))
        usage errs = ioError (userError (concat errs ++ usageInfo header options))
    case opts of
        Right (PorcelainOptions _ _ _ _ _ _ _ [Help]) -> usage []
        Right po@(PorcelainOptions gid settingsFile _ _ _ _ _ _) -> do
            settings <- loadSettings settingsFile
            res <- execute po settings

            case res  of
                Nothing -> return ()
                Just errs -> usage errs
        Left errs -> usage errs
        
main :: IO ()
main = do
    args <- getArgs
    let opts = getOptions args
    return ()
    main' opts
