module Main where

import System.Directory
import System.FilePath
import System.Environment
import Control.Monad
import Control.Exception

listFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursive baseDir dir = do
    contents <- getDirectoryContents dir
    let properContents = filter (`notElem` [".", ".."]) contents
    paths <- mapM (\x -> do
        let fullPath = dir </> x
        isDir <- doesDirectoryExist fullPath
        if isDir then
            listFilesRecursive baseDir fullPath
        else
            return [makeRelative baseDir fullPath]) properContents
    return $ join paths

asciidocGenDir :: FilePath -> FilePath -> IO ()
asciidocGenDir sourceDir destDir = do
    files <- listFilesRecursive sourceDir sourceDir
    mapM_ (\path -> do
        let extension = takeExtension path
            path' = take (length path - length extension) path
            sourcePath = sourceDir </> path
            destPath = destDir </> (path' ++ ".adoc")
        case extension of
            ".enum" -> genEnum sourcePath destPath
            ".dto" -> genDTO sourcePath destPath
            _ -> return ()
        ) files

genEnum :: FilePath -> FilePath -> IO ()
genEnum sourcePath destPath = do
    createDirectoryIfMissing True $ takeDirectory destPath
    content <-readFile sourcePath
    case filter (not . null) $ lines content of
        [] -> throwIO $ userError "no content"
        (enumName : cs) -> do
            writeFile destPath $ ":enum:\n=== " ++ enumName ++ "\n|===\n|Name |Description\n\n"
            inner cs False False
    where
        inner :: [String] -> Bool -> Bool -> IO ()
        inner [] _ _ = throwIO $ userError "parse error"
        inner _ False True = throwIO $ userError "unreachable"
        inner (h : []) True first = do
            if h == "-" then
                if first then
                    appendFile destPath "-\n|===\n"
                else
                    appendFile destPath "|===\n"
            else
                throwIO $ userError "parse error"
        inner (h : t) False False = do
            appendFile destPath $ "|" ++ h ++ " |"
            inner t True True
        inner (h : t) True first = do
            if h == "-" then
                if first then do
                    appendFile destPath $ "-\n"
                    inner t False False
                else
                    inner t False False
            else
                do
                appendFile destPath $ h ++ "\n"
                inner t True False

genDTO :: FilePath -> FilePath -> IO ()
genDTO sourcePath destPath = do
    createDirectoryIfMissing True $ takeDirectory destPath
    content <- readFile sourcePath
    case filter (not . null) $ lines content of
        [] -> throwIO $ userError "no content"
        (dtoName : vbts) -> do
            writeFile destPath $ ":dto:\n=== " ++ dtoName ++ "\n|===\n|Name |Type |Description\n\n"
            inner vbts False False
    where
        inner :: [String] -> Bool -> Bool -> IO ()
        inner [] _ _ = throwIO $ userError "parse error"
        inner _ False True = throwIO $ userError "unreachable"
        inner (h : []) True first = do
            if h == "-" then
                if first then
                    appendFile destPath "-\n|===\n"
                else
                    appendFile destPath "|===\n"
            else
                throwIO $ userError "parse error"
        inner (h : t) False False = do
            case words h of
                [] -> throwIO $ userError "parse error"
                [name, typ] ->
                    case typ of
                        ('!' : []) -> throwIO $ userError "parse error"
                        ('!' : typ') -> do
                            appendFile destPath $ "|" ++ name ++ " |" ++ showType typ' ++ " |"
                            inner t True True
                        _ -> do
                            appendFile destPath $ "|" ++ name ++ " |" ++ showType typ ++ " |"
                            inner t True True
                [name, typ, link] ->
                    case typ of
                        ('!' : []) -> throwIO $ userError "parse error"
                        ('!' : typ') -> do
                            appendFile destPath $ "|" ++ name ++ " |link:" ++ link ++ "[" ++ showType typ' ++ ",role=\"popUp\"] |"
                            inner t True True
                        _ -> do
                            appendFile destPath $ "|" ++ name ++ " |link:" ++ link ++ "[" ++ showType typ ++ ",role=\"popUp\"] |"
                            inner t True True
                _ -> throwIO $ userError "parse error"
        inner (h : t) True first = do
            if h == "-" then
                if first then do
                    appendFile destPath $ "-\n"
                    inner t False False
                else
                    inner t False False
            else
                do
                appendFile destPath $ h ++ "\n"
                inner t True False

showType :: String -> String
showType ('!' : x) = x
showType ('~' : x) = x
showType x = x

main :: IO ()
main = do
    args <- getArgs
    case args of
        [sourceDir, destDir] -> asciidocGenDir sourceDir destDir
        _ -> print "Usage: executableName sourceDir destDir"