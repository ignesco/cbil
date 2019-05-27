{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module CbilLib.Utils
(
    sed
    , _sqlcmd
    , touch
    , touch'
)
where

import Development.Shake
import Data.List
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified System.Directory as SD
import Development.Shake.FilePath

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
