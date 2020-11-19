
import System.Command
import System.Exit
import Control.Monad

main = do
  ExitSuccess <- compile "MiniLISP.cpp" "MiniLISP"
  processLinewise

compile cpp out = command [] "g++" [cpp,"-o",out]

processLinewise = do
  inputs <- readFile "examples.in"
  expected <- readFile "examples.out"

  res <-
    zipWithM ( \inp ex ->
      if (length inp > 0 && (take 2 inp /= "--")) then
        do
          Stdout out' <- command [Stdin inp] "./MiniLISP" []
          let out = init out' -- ignore \n at end
          if (out == ex)
            then putStrLn (":) | " ++ inp ++ " => " ++ ex)
            else putStrLn ("XX | " ++ inp ++ " => " ++ ex ++ ", but got: " ++ out )
          return (out == ex)
      else return True
     )
     (lines inputs)
     (lines expected)
  if (and res)
    then putStrLn "+++ All tests passed! +++"
    else putStrLn "### Some tests failed! ###"
