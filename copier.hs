import System.Environment
import System.IO.Error
import Control.Exception
import qualified Data.ByteString.Lazy as B

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fileName1:fileName2:_) <- getArgs
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents

handler :: IOError -> IO ()
handler e 
    | isDoesNotExistError e = 
        case ioeGetFileName e of Just path ->
                                     putStrLn $ "The file to copy from doesn't exist at: " ++ path
                                 Nothing ->
                                     putStrLn "The file to copy from doesn't exist at unknown location"
    | otherwise = ioError e
