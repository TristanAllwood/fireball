module Fireball where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Process
import Text.Regex.Posix
import Trace.Hpc.Mix


main :: IO ()
main = do
  args <- getArgs

  case args of
    (program : modules) -> do
      moduleMixs <- mapM (readMix [".hpc", "."] . Left) modules
      go program (modules `zip` moduleMixs)
    _                   -> do
      putStrLn "Usage: Fileball <program> <modules...>"
      exitWith ExitSuccess

go :: String -> [(String, Mix)] -> IO ()
go program mixs = do

  let progTix = program <.> "tix"
  tixExists <- doesFileExist progTix
  when tixExists $ do
    removeFile progTix
    putStrLn $ "Deleted " ++ progTix


  withProcess "valgrind" ["--vgdb=yes", "--vgdb-error=0", program]
              $ \(val_stdin, val_stdout, val_stderr, val_hdl) -> do
    withProcess "gdb" ["-q", program]
                $ \(gdb_stdin, gdb_stdout, gdb_stderr, gdb_hdl) -> do

      hSkipTo val_stderr (=~ "TO DEBUG THIS PROCESS")

      hPutStrLn gdb_stdin "target remote | vgdb"

      hPutStr gdb_stdin $ unlines [ "break startupHpc"
                                  , "continue"
                                  , "finish"
                                  , "set print elements 0"
                                  , "set print repeats 0"
                                  ]

      forM mixs $ \(modName, Mix _ _ _ _ entries) -> do
        let symbol = "_hpc_tickboxes_" ++ modName ++ "_hpc"
                                              {- TODO: zEncode the modName! -}
        hPutStrLn gdb_stdin $ unlines

          [ "watch ((long long[" ++ show (length entries) ++ "])" ++ symbol ++ ")"
          , "commands"
          , "cont"
          , "end"
          ]

      hPutStrLn gdb_stdin "cont"
      hPutStrLn gdb_stdin "quit"

      tokens <- forM [val_stdout, val_stderr, gdb_stderr]
                     asyncDrainHandle

      {- gdb_stdout -}
      -- Hardware watchpoint 2: ((long long[20])_hpc_tickboxes_Main_hpc)
      --
      -- Old value = {4, 14, 14, 14, 14, 19, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      -- New value = {4, 15, 15, 15, 15, 19, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

      forM_ tokens takeMVar

asyncDrainHandle :: Handle -> IO (MVar ())
asyncDrainHandle handle = do
  mv <- newEmptyMVar
  forkIO (drain `finally` putMVar mv ())
  return mv
  where
    drain = do
      iseof <- hIsEOF handle
      unless iseof $ do
        hGetLine handle >>= putStrLn
        drain


withProcess :: String -> [String] -> ((Handle, Handle, Handle, ProcessHandle) -> IO a)
          -> IO a
withProcess command args cont = do
  let process = (proc command args) { std_in  = CreatePipe
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    }

  (Just stdin, Just stdout, Just stderr, hdl) <- createProcess process

  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  cont (stdin, stdout, stderr, hdl) `finally` do
    hClose stdin
    hClose stdout
    hClose stderr
    terminateProcess hdl
    waitForProcess hdl

hSkipTo :: Handle -> (String -> Bool) -> IO ()
hSkipTo hdl cond = do
  iseof <- hIsEOF hdl
  unless iseof $ do
    line <- hGetLine hdl
    putStrLn line
    unless (cond line) (hSkipTo hdl cond)
