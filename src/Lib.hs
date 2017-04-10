{-# LANGUAGE LambdaCase #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc2"

insertAfter :: (Ord a) => a -> a -> [a] -> [a]
insertAfter x y xs = reverse $ foldl (\p -> (\case
  c | c == x -> y:c:p
    | otherwise -> c:p)
  ) [] xs

ff :: (Num a) => [a] -> [a]
ff xs = (\a -> a + 1) . (+6) <$> xs

-- put :: Char -> IO String
-- put c = (\c -> (c:[]) ++ 'b') <*> (putChar c)

foo :: IO ()
foo = do
    filename <- getLine
    contents <- readFile filename
    putStrLn contents

-- greeter :: Reader String String
-- greeter = do
--     name <- ask
--     return ("hello, " ++ name ++ "!")
