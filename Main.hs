module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Data.Char
import Data.Functor
import Data.Monoid
import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import System.Process

data ConfigInfo = ConfigInfo
                { pacnews  :: ![FilePath]
                , pacsaves :: ![FilePath]
                , pacorigs :: ![FilePath]
                }

instance Monoid ConfigInfo where
  mempty                                                 = ConfigInfo [] [] []
  mappend (ConfigInfo ns ss os) (ConfigInfo ns' ss' os') = 
    ConfigInfo (ns `mappend` ns')
               (ss `mappend` ss')
               (os `mappend` os')

printSummary :: ConfigInfo -> IO ()
printSummary (ConfigInfo ns ss os) = do
  prettyPrint "PACNEW "  ns
  prettyPrint "PACSAVE" ss
  prettyPrint "PACORIG" os
  where
    prettyPrint t fs = do putStrLn $ t ++ " (" ++ (show $ length fs) ++ ")"
                          mapM_ (\f -> putStrLn $ "  " ++ f) fs

numConfigFiles :: ConfigInfo -> Int
numConfigFiles (ConfigInfo ns ss os) = length ns + length ss + length os

main = do
  ci <- getConfigInfo "/etc"
  printSummary ci
  when ((>) 0 $ numConfigFiles ci) $ do
    am <- ask "merge .pacnew files"
    when am $ forM_ (pacnews ci) $ \new -> handle printError $ do
      let org = dropExtension new
      permNew <- getPermissions new
      permOrg <- getPermissions org
      if   writable permOrg && readable permNew
      then do (c,o,e) <- readProcessWithExitCode "gvimdiff" [org,new] ""
              case c of
                ExitSuccess     -> do ar <- ask $ "remove file " ++ new
                                      when ar $ removeFile new
                (ExitFailure _) -> putStrLn e
      else putStrLn $ "insufficient permissions: " ++ org ++ "/" ++ new
  where
    ask msg = do
      aw <- putStr (">> " ++ msg ++ "? [y/n] ") >> hFlush stdout 
                                                >> toLower <$> getChar
      putStrLn ""
      case aw of
        'y' -> return True
        'n' -> return False
        _   -> ask msg
    printError :: SomeException -> IO ()
    printError = putStrLn . show

partitionM :: (Functor m, Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p = foldM (\xs x -> select xs x <$> p x) ([],[])
  where
    select (ts,fs) x r | r         = (x:ts,fs)
                       | otherwise = (ts,x:fs)

getConfigInfo :: FilePath -> IO ConfigInfo
getConfigInfo = execWriterT . scanRecursive
  where
    scanRecursive d                   = do
      (ds,fs) <- lift $   handle ignoreErrors
                      $   map (d </>) . filter dots 
                      <$> getDirectoryContents d
                      >>= partitionM doesDirectoryExist
      tell $ foldl select mempty fs
      mapM_ scanRecursive ds                                  
    ignoreErrors :: SomeException -> IO ([FilePath],[FilePath])
    ignoreErrors _                    = return ([],[])
    dots e                            = not $ or [e == ".", e == ".."]
    select ci@(ConfigInfo ns ss os) f = case takeExtensions f of
      ".pacnew"  -> ConfigInfo (f:ns) ss os
      ".pacsave" -> ConfigInfo ns (f:ss) os
      ".pacorig" -> ConfigInfo ns ss (f:os)
      otherwise  -> ci
