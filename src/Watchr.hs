module Watchr where

import           Control.Monad.Writer
import           System.Directory
import           System.FilePath.Glob
import           System.OSX.FSEvents

type Watchr = Writer [(Event -> IO Bool, FilePath -> IO ())]

runWatchr :: String -> Watchr a -> IO ()
runWatchr paths w = do
  ps <- glob paths
  putStr "Watching: "
  print ps
  let patterns = snd $ runWriter w
  es <- eventStreamCreate ps 5.0 True True True (wherePred patterns)
  getLine
  eventStreamDestroy es

watch :: String -> [EventType] -> (FilePath -> IO ()) -> Watchr ()
watch pat ets callback = tell [(checkMatch ets pat, callback)]

checkMatch ets pat e = return (matchEvents ets e && match (compile pat) (eventPath e))

wherePred :: [(Event -> IO Bool, FilePath -> IO ())] -> Event -> IO ()
wherePred ps e = mapM_ (runPred e) ps

runPred :: Event -> (Event -> IO Bool, FilePath -> IO ()) -> IO ()
runPred e (p, a) = do
  relativized <- makeRelativeToCurrentDirectory $ eventPath e
  result <- p $ e { eventPath = relativized }
  when result (a relativized)

data EventType = Foo

matchEvents ets e = True
