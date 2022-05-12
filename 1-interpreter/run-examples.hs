
import System.Command
import System.Exit
import Control.Monad
import Data.List
import Data.List.Split

skip_rule = "#ignore-embed-eval#"
longTests = True

runTests = zipWithM ( \inp ex ->
  if isInfixOf "(eval" inp && (skip_rule `isInfixOf` inp) then
    putStrLn ("SKIP | " ++ inp ++ " => " ++ ex ++ ", due to " ++ skip_rule)
    >> return True
  else
    do
      Stdout out' <- command [] "bin/MiniLISP" ["-e",inp]
      let out = init out' -- ignore \n at end
      if (out == ex)
        then putStrLn ("✅ | " ++ inp ++ " => " ++ ex)
        else putStrLn ("😵 | " ++ inp ++ " => " ++ ex ++ ", but got: " ++ out )
      return (out == ex)
  )

testCases =
  takeWhile (/="-- lisp self-interpreter")
  . filter (\x -> length x > 0 && (take 2 x /= "--"))

embedEvalCases = takeWhile (\s -> not (isInfixOf "#ignore-embed-eval-following#" s))

splitLines = lines . replace "\\\n" "" -- use backslash for multi-line expressions

replace q v = intercalate v . splitOn q

main = do
  inputs <- splitLines <$> readFile "1-interpreter/examples.in"
  expected <- splitLines <$> readFile "1-interpreter/examples.out"

  let inps = testCases inputs
      expt = testCases expected

  res1 <- runTests inps expt


  let varname = "expr"
      ruleStart = varname ++ " ->"
      replacementRule = head . filter (isPrefixOf ruleStart) . tails . head . filter (isInfixOf ruleStart) $ inputs
      body = drop (length ruleStart) replacementRule
      embedInEval expr = replace varname expr body

  putStrLn $ "*************\nRepeating tests with lisp self-interpreter: " ++ replacementRule

  res2 <- if longTests
      then runTests (map embedInEval (embedEvalCases inps)) expt
      else return []

  if (and res1 && and res2)
    then putStrLn "⭐⭐⭐ All tests passed! ⭐⭐⭐" >> exitSuccess
    else putStrLn "*XXXXXXXXXXXXXXX Some tests failed! XXXXXXXXXXXXXXX*" >> exitFailure
