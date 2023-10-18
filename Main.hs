module Main where

import Function
import Control.Monad ( unless )
import System.Directory

main :: IO ()
main = do

    putStr "Input path to file: "
    pathFile <- getLine

    check <- doesFileExist pathFile
    unless (not (null pathFile) || check) $ do 
        putStrLn "This file have invailed name or not exist. Try again!"
    
    unless (null pathFile || not check) $ do

        putStr "Input command (create, read, update, delete): "
        command <- getLine

        unless (command /= "create" && (command == "read" || command == "update" || command == "delete")) $ do
            putStr "Input element name: "
            nameElem <- getLine

            putStr "Input element value (name): "
            valueElemName <- getLine

            putStr "Input element value (value): "
            valueElemVal <- getLine

            putStr "Input element attribute (name): "
            attrElemName <- getLine

            putStr "Input element attribute (value): "
            attrElemVal <- getLine
            
            result <- createElem (NameElem nameElem, ValueElem (makeElem (zip (wordsWithChar (==',') valueElemName) (wordsWithChar (==',') valueElemVal))), Attr ( makeAttr (zip (wordsWithChar (==',') attrElemName) (wordsWithChar (==',') attrElemVal)))) pathFile
            putStrLn result


        unless (command /= "read" && (command == "create" || command == "update" || command == "delete")) $ do
            putStr "Input path to get data: "
            pathData <- getLine

            result <- searche pathData pathFile
            print result


        unless (command /= "delete" && (command == "create" || command == "update" || command == "read")) $ do
            putStr "Name of the element being deleted: "
            nameElem <- getLine

            putStr "Name of value of the element being deleted: "
            valueElemName <- getLine

            putStr "Value of value of the element being deleted: "
            valueElemVal <- getLine

            putStr "Name of attribute of the element being deleted: "
            attrElemName <- getLine

            putStr "Value of attribute of the element being deleted: "
            attrElemVal <- getLine

            result <- deleteElem (NameElem nameElem, ValueElem (makeElem (zip (wordsWithChar (==',') valueElemName) (wordsWithChar (==',') valueElemVal))), Attr ( makeAttr (zip (wordsWithChar (==',') attrElemName) (wordsWithChar (==',') attrElemVal)))) pathFile
            putStrLn result


        unless (command /= "update" && (command == "create" || command == "delete" || command == "read")) $ do
            putStr "Name of the old element: "
            nameElemOld <- getLine

            putStr "Name of value of the old element: "
            valueElemNameOld <- getLine

            putStr "Value of value of the old element: "
            valueElemValOld <- getLine

            putStr "Name of attribute of the old element: "
            attrElemNameOld <- getLine

            putStr "Value of attribute of the old element: "
            attrElemValOld <- getLine


            putStr "Name of the new element: "
            nameElemNew <- getLine

            putStr "Name of value of the new element: "
            valueElemNameNew <- getLine

            putStr "Value of value of the new element: "
            valueElemValNew <- getLine

            putStr "Name of attribute of the new element: "
            attrElemNameNew <- getLine

            putStr "Value of attribute of the new element: "
            attrElemValNew <- getLine

            result <- updateElem (NameElem nameElemOld, ValueElem (makeElem (zip (wordsWithChar (==',') valueElemNameOld) (wordsWithChar (==',') valueElemValOld))), Attr ( makeAttr (zip (wordsWithChar (==',') attrElemNameOld) (wordsWithChar (==',') attrElemValOld)))) (NameElem nameElemNew, ValueElem (makeElem (zip (wordsWithChar (==',') valueElemNameNew) (wordsWithChar (==',') valueElemValNew))), Attr ( makeAttr (zip (wordsWithChar (==',') attrElemNameNew) (wordsWithChar (==',') attrElemValNew)))) pathFile
            putStrLn result