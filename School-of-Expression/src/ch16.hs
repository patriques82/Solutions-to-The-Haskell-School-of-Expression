{- # LANGUAGE ScopedTypeVariables #-}

-- to run in ghci (:set -XScopedTypeVariables)

module Ch16 where

import System.IO
import Control.Exception (try, catch, SomeException)

{- 16.1 -}

-- 1.
main :: IO ()
main = do
	interactions
	end <- isEOF
	if end
		then return ()
		else main

interactions :: IO ()
interactions = catch (do
												h <- getFile "Please type the name of your file:"
												putStrLn "Please type something to write:"
												str <- getLine
												mapM_ putStrLn (words str)
												hPutStr h str)													                -- loop
											(\(ex :: SomeException) -> putStrLn (show ex))            -- error handler

getFile :: String -> IO Handle
getFile prompt = do
	putStrLn prompt
	path <- getLine
	openFile path WriteMode

-- alternative way (without end-of-stream handling)
main' :: IO ()
main' = do
	h <- try (getFile "Please type the name of your file:") :: IO (Either SomeException Handle)
	case h of
		Left ex -> do
			putStrLn (show ex)
			main'
		Right handle -> do
			putStrLn "Please type something to write:"
			str <- getLine
			mapM_ putStrLn (words str)
			hPutStr handle str
			main'  													                                -- loop

