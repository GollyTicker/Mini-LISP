import System.Environment
import System.IO
import Data.List
import Control.Monad

ext = ".loader"

main :: IO ()
main = do
  [filePath] <- getArgs
  if ext `isSuffixOf` filePath
    then processFile filePath
    else error $ "Please provide a .loader file for preprocessing. Got: " ++ filePath


processFile filePath =
  do
    str <- readFile filePath
    str' <- mapM (processLineWith readFileWithoutFinalNewline) (lines str)
    -- mapM_ putStrLn str'-- DEBUG:
    writeFile (newFilename filePath) (unlines str')

newFilename = reverse . drop (length ext) . reverse

readFileWithoutFinalNewline f =
  do  str <- readFile f
      return $ foldr (\e r -> if (e:r) == "\n" then [] else e:r) [] str

-- processLineWith f str returns a function which replaces the match in str
-- with the IO result of running f to the match
processLineWith ::(String -> IO String) ->  String -> IO String
processLineWith getSource s = do
  let (pre,midrest) = span (/='~') s
  if (not (null midrest))
    then do
      let (sourcePath, _:rest) = span (/='~') (tail midrest)
      str' <- getSource sourcePath
      rest' <- processLineWith getSource rest
      return $ pre ++ str' ++ rest'
    else return s
