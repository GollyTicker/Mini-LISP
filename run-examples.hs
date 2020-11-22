
import System.Command
import System.Exit
import Control.Monad
import Data.List
import Data.String.Utils

main = do
  ExitSuccess <- compile "MiniLISP.cpp" "MiniLISP"
  processLinewise

compile cpp out = command [] "g++" ["-O3",cpp,"-o",out]

runTests = zipWithM ( \inp ex ->
  do
    Stdout out' <- command [Stdin inp] "./MiniLISP" []
    let out = init out' -- ignore \n at end
    if (out == ex)
      then putStrLn (":) | " ++ inp ++ " => " ++ ex)
      else putStrLn ("XX | " ++ inp ++ " => " ++ ex ++ ", but got: " ++ out )
    return (out == ex)
  )

testCases =
  takeWhile (/="-- lisp self-interpreter")
  . filter (\x -> length x > 0 && (take 2 x /= "--"))

splitLines = lines . replace "\\\n" "" -- use backslash for multi-line expressions

processLinewise = do
  inputs <- splitLines <$> readFile "examples.in"
  expected <- splitLines <$> readFile "examples.out"

  let inps = testCases inputs
      expt = testCases expected

  res1 <- runTests inps expt


  let varname = "expr"
      ruleStart = varname ++ " ->"
      replacementRule = head . filter (isPrefixOf ruleStart) . tails . head . filter (isInfixOf ruleStart) $ inputs
      body = drop (length ruleStart) replacementRule
      embedInEval expr = replace varname expr body

  putStrLn $ "*************\nRepeating tests with lisp self-interpreter: " ++ replacementRule

  res2 <- runTests (map embedInEval inps) expt

  if (and res1 && and res2)
    then putStrLn "*+++ All tests passed! +++*"
    else putStrLn "*XXX Some tests failed! XXX*"
