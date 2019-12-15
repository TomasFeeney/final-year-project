module Main where

import Boolean
import qualified Data.ByteString as B
import System.IO
import System.Process.ByteString as B
import Visualise

main :: IO ()
main = do
  sequence (writeTraversalAsGif booleanGraph)
  (code, out, err) <-
    B.readProcessWithExitCode
      "gifsicle"
      ["--loop", "--delay=100", "out/out1.gif", "out/out2.gif", "out/out3.gif"]
      B.empty
  B.putStrLn (out)
