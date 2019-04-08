module Main where

import CbilLib
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Data.List.Split
import Data.Char

tempCacheFilePath = "."
userDbInit :: DBInit
userDbInit baseDirectory databaseGroupId dbname = do
    let cacheFile = (tempCacheFilePath </> ("dbcache." ++ dbname))
    putNormal $ "DBINIT:" ++ cacheFile
    (Exit c, Stdout sout) <- cmd $ "cat " ++ cacheFile

    let
        matchGroup databaseGroupId (i:g:_:[]) = databaseGroupId == g
        matchGroup _ _ = False
        ls' = lines sout
        ls = map ( \(_:group:sql:[]) -> (group, sql) ) $ filter (matchGroup databaseGroupId) $ map (\l -> map (filter (not.isSpace)) (splitOn "!" l)) ls'
        
    return (Just ls)

userScriptExecuted :: ScriptExecuted
userScriptExecuted normalRun group script dbname = do
    putNormal $ "DB insert:" ++ dbname ++ ">" ++ group ++ "!" ++ script
    liftIO $ appendFile (tempCacheFilePath </> ("dbcache." ++ dbname)) ("X!" ++ group ++ "!" ++ script ++ "\r\n")  
    return True
        
userCache = UserDBCache { dbInit = userDbInit, scriptExecuted = userScriptExecuted }

main :: IO ()
main = _cbilMain $ (\configuration profileDefines -> do

        -- build the module rules
        cloneRulesInfo <- initCloneProjects configuration profileDefines
        needsRulesInfo <- initNeeds configuration profileDefines
        databaseGroupsRulesInfo <- initDatabaseGroups configuration profileDefines
        incrementalDatabaseGroupsRulesInfo <- initIncrementalDatabaseGroups configuration profileDefines userCache
        visualStudioRulesInfo <- initVisualStudios configuration profileDefines
        netTiersGroupsRulesInfo <- initNetTiersGroups configuration profileDefines

        -- build the help rule
        cbilHelp configuration profileDefines [cloneRulesInfo, needsRulesInfo, databaseGroupsRulesInfo, incrementalDatabaseGroupsRulesInfo, visualStudioRulesInfo, netTiersGroupsRulesInfo]
    )
