import Control.Monad
import Test.HUnit
import TestVisualise

main :: IO ()
main = do
  counts <- runTestTT visualiseTests
  putStrLn "Tests completed"
