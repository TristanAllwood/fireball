{-# LANGUAGE ScopedTypeVariables #-}
module FireballRender where

import Control.Applicative
import Data.List
import System.Environment
import System.FilePath
import System.IO
import Trace.Hpc.Mix
import Trace.Hpc.Util
import Control.Monad

main :: IO ()
main = do
  args <- getArgs

  case args of
    [name] -> go name
    _ -> do
      putStrLn "Usage: FireballRender <name>"


go :: FilePath -> IO ()
go name = do
  tixs :: [(String, [(Integer, Integer)])] <- read <$> readFile (name <.> "hfb")

  let moduleNames = map head . group . sort . map fst $ tixs
  mixFiles <- mapM (readMix [".hpc", "."] . Left) moduleNames

  withFile (name <.> "html") WriteMode $ \html -> do
    hPutStrLn html $ unlines
      [ " <!DOCTYPE HTML>"
      , " <html>"
      , "   <head>"
      , "     <meta charset=\"utf8\"/>"
      , "     <title>Fireball " ++ name ++ "</title>"
      , "     <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/></link>"
      , "   </head>"
      , "   <body>"
      , "     <div class=\"header\">"
      , "       <h1>" ++ name ++ "</h1>"
      , "     </div>"
      ]

    addTabBar html moduleNames
    zipWithM_ (addSourceDiv html) moduleNames mixFiles

    hPutStrLn html $ unlines
      [ "     <script src=\"jquery-1.8.3.js\"></script>"
      , "     <script src=\"" ++ (name <.> "js") ++ "\"></script>"
      , "     <script src=\"code.js\"></script>"
      , "   </body>"
      , " </html>"
      ]

  withFile (name <.> "js") WriteMode $ \js -> do
    hPutStr js $ "ticks = ["

    hPutStr js $ concat . intersperse ", " . flip map tixs $ \(name, tvs) ->
      "{ name: " ++ show name ++ ", tixs: " ++
        show (map (fromEnum . uncurry (/=)) tvs) ++ "}"

    hPutStr js "]"
    hPutStrLn js ""


addTabBar :: Handle -> [String] -> IO ()
addTabBar html moduleNames = do
  hPutStr html "<div id=\"source-tabs\">"

  forM_ moduleNames $ \name -> do
    hPutStrLn html $ unlines
      [ "<div id=\"source-tab-" ++ name ++ "\" class=\"source-tab\">"
      , name
      , "</div>"
      ]

  hPutStr html "</div>"

addSourceDiv :: Handle -> String -> Mix -> IO ()
addSourceDiv html name (Mix fp ts hash tabsize entries) = do
  sourceCode <- map (map Left) . lines <$> readFile fp
  let sourceCode' = foldl' addEntry sourceCode (entries `zip` [0..])

  hPutStrLn html $ "<div id=\"source-" ++ name ++ "\" class=\"source\">"

  forM_ sourceCode' $ \(codeLine :: [Either Char String])-> do
    hPutStrLn html $ concatMap (either return id) codeLine
  hPutStrLn html "</div>"

  where
    addEntry :: [[Either Char String]] -> (MixEntry, Integer) -> [[Either Char String]]
    addEntry code ((pos, label), id) =
      let boxType = case label of
                      ExpBox _      -> "source-pos-type-exp"
                      TopLevelBox _ -> "source-pos-type-toplevel"
                      LocalBox _    -> "source-pos-type-local"
                      BinBox _ _    -> "source-pos-type-bin"
          (l1, c1, l2, c2) = fromHpcPos pos
       in insertAt False l1 c1 ("<span class=\"source-pos " ++ boxType ++ "\" id=\"source-pos-" ++ name ++ "-" ++ (show id) ++ "\">")
        . insertAt True l2 c2 "</span>"
        $ code

    insertAt :: Bool -> Int -> Int -> String -> [[Either Char String]] -> [[Either Char String]]
    insertAt b l c txt code =
      let (before, after:afters) = splitAt (l - 1) code
       in before ++ [concat . snd $ mapAccumL insertAtLine 1 after] ++ afters
       where
          insertAtLine :: Int -> Either Char String -> (Int, [Either Char String])
          insertAtLine p x
            | p > c = (p, [x])
          insertAtLine p (Left x)
            | p == c    = (p + 1, (if b then reverse else id) [Right txt, Left x])
            | otherwise = (p + 1, [Left x])
          insertAtLine p x = (p, [x])
