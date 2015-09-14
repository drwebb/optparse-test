module Main where

import Test.Hspec
import System.Process
import System.Exit
import Control.Monad (forM_)
import Data.List (permutations)


argumentPs :: [[String]]
argumentPs = permutations ["build", "--global-foo", "--build-foo"]

main :: IO ()
main =
    hspec $
    before stackBuild $
       describe "Optparse tests" $ forM_ argumentPs
           (\argCase ->  it (unwords argCase) $ do
                exit <- runExe argCase
                exit `shouldBe` ExitSuccess)

stackBuild :: IO ()
stackBuild =
    callProcess
        "stack"
        ["build"]

runExe :: [String] -> IO ExitCode
runExe options = do
    (exitCode,_,_) <-
        readProcessWithExitCode
            "stack"
            (["exec", "--", "optparse-test-exe"] ++
             options) ""
    return exitCode
