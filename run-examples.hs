
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

  zipWithM_ ( \inp ex ->
    when (length inp > 0 && (take 2 inp /= "--")) $
      do
        Stdout out <- command [Stdin inp] "./MiniLISP" []
        when (out /= ex) $
          putStrLn ("Expected " ++ show ex ++ ", but got: " ++ show out)
   )
   (lines inputs)
   (lines expected)
