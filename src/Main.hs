module Main (main) where

import Control.Monad      (forM_)
import Data.Foldable      (foldlM)
import Data.List          (intercalate, isPrefixOf, sort)
import Data.Maybe         (fromMaybe, mapMaybe)
import System.Directory   (getDirectoryContents)
import System.Environment (getArgs, lookupEnv)
import System.IO          (hPutStrLn, stderr)

data Program = Cabal | GHC | Alex | Happy

type KnownProgram = (String, (FilePath, FilePath))

getKnownPrograms :: Program -> IO [KnownProgram]
getKnownPrograms prog =
    mapMaybe f `fmap` getDirectoryContents path
  where
    path = "/opt/" ++ progName prog
    f :: FilePath -> Maybe KnownProgram
    f p | p `elem` [".", ".."] = Nothing
        | otherwise            = Just (progName prog ++ "-" ++ p, (path, path ++ "/" ++ p ++ "/bin"))

progName :: Program -> String
progName Cabal  = "cabal"
progName GHC    = "ghc"
progName Alex   = "alex"
progName Happy  = "happy"

parsePath :: String -> [String]
parsePath = go []
  where
    go [] (':':rest) = go [] rest
    go p  (':':rest) = p : go [] rest
    go p  (c : rest) = go (p ++ [c]) rest
    go [] []         = []
    go p  []         = [p]

modifyPath :: [KnownProgram] -> [FilePath] -> String -> IO [FilePath]
modifyPath knownPrograms paths program =
    case lookup program knownPrograms of
        Just (pfx, p) ->
            pure $ p : filter (not . (pfx `isPrefixOf`)) paths
        Nothing   -> do
            hPutStrLn stderr $ "Unknown program: " ++ program
            forM_ knownPrograms $ \(name, _) -> hPutStrLn stderr $ "  - " ++ name
            pure paths

main :: IO ()
main = do
    pathEnv <- fromMaybe "" `fmap` lookupEnv "PATH"
    let paths = parsePath pathEnv
    cabals <- getKnownPrograms Cabal
    ghcs <- getKnownPrograms GHC
    let knownPrograms = sort $ cabals ++ ghcs
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Usage: ghc-select program [program...]"
        _  -> do
            paths' <- foldlM (modifyPath knownPrograms) paths args
            putStrLn $ "export PATH=" ++ intercalate ":" paths'
