module Main (main) where
import Parsers

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Ast


main :: IO ()
main = do 
         putStrLn "A"
