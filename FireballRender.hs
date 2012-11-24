{-# LANGUAGE ScopedTypeVariables #-}
module FireballRender where

import Control.Applicative
import Data.List
import System.Environment
import System.FilePath
import System.IO
import Trace.Hpc.Mix

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
    hPutStr html $ unlines
      [ " <!DOCTYPE HTML>"
      , " <html>"
      , "   <head>"
      , "     <meta charset=\"utf8\"/>"
      , "     <title>Firball " ++ name ++ "</title>"
      , "     <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/></link>"
      , "   </head>"
      , "   <body>"
      , "     <div class=\"header\">"
      , "       <h1>" ++ name ++ "</h1>"
      , "     </div>"
      , " "
      ]

    hPutStr html $ unlines
      [ "     <script src=\"jquery-1.8.3.js\"></script>"
      , "     <script src=\"" ++ (name <.> "js") ++ "\"></script>"
      , "     <script src=\"code.js\"></script>"
      , "   </body>"
      , " </html>"
      ]

  error "TODO"
