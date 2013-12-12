import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add)
           ,("view", view)
           ,("remove", modify remove)
           ,("bump", modify bump)]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\ idx line -> show idx ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> Int -> [String]
remove todoTasks idx = delete (todoTasks !! idx) todoTasks 

bump :: [String] -> Int -> [String]
bump todoTasks idx = 
    let (h,t) = splitAt (idx + 1) todoTasks in
    (init h) ++ [(head t)] ++ [(last h)] ++ (tail t)

modify :: ([String] -> Int -> [String]) -> [String] -> IO ()
modify modFun [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = modFun todoTasks number
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
