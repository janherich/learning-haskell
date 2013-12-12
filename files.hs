import System.IO

main = do
  withFile "input.txt" ReadMode (\ handle -> do
                                   -- hSetBuffering handle $ BlockBuffering (Just 2048)
                                   contents <- hGetContents handle
                                   putStr contents)
