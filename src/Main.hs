{-# LANGUAGE TemplateHaskell, LambdaCase, RecordWildCards,
  TypeFamilies #-}
module Main where

import qualified Ohua.DFGraph as G
import qualified Data.Text.Lazy.IO as PG
import qualified Data.GraphViz as PG
import qualified Data.Graph.Inductive.Graph as PG
import qualified Data.Graph.Inductive.PatriciaTree as PG
import Options.Applicative
import Language.Haskell.TH
import System.FilePath
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Maybe
import Text.Printf
import Data.Monoid
import Data.List (intercalate)
import Ohua.Types
import Ohua.Serialize.JSON ()
import qualified Data.ByteString.Lazy as BS


data P = P { graph :: G.OutGraph }

A.deriveFromJSON A.defaultOptions ''P


ohuaGrToDot :: G.OutGraph -> PG.Gr String String
ohuaGrToDot G.OutGraph {..} =
    PG.mkGraph ((envNode, "env") : map opToNode operators) (map arcToEdge arcs)
  where
    envNode = succ $ maximum (map (unwrap . G.operatorId) operators)
    opToNode G.Operator {..} = (unwrap operatorId, show operatorType)
    arcToEdge G.Arc {..} = (sOp, tOp, printf "%d -> %d" sIdx tIdx :: String)
      where
        (sOp, sIdx) =
            case source of
                G.LocalSource t -> targetToInfo t
                G.EnvSource e -> (envNode, unwrap e)
        (tOp, tIdx) = targetToInfo target
        targetToInfo G.Target {..} = (unwrap operator, index)

printDot :: FilePath -> PG.DotGraph PG.Node -> IO ()
printDot path = PG.writeFile path . PG.printDotGraph

readGraph :: FilePath -> IO G.OutGraph
readGraph p = either error graph . A.eitherDecode <$> BS.readFile p

data Opts = Opts { action :: Action, input :: FilePath }

data Action = Preview | Print (Maybe PG.GraphvizOutput) (Maybe FilePath)

formatOptions =
    $(let e = fail "GraphvizOutput had unexpected constructor"
       in reify ''PG.GraphvizOutput >>= \case
              (TyConI (DataD _ _ _ _ cons _)) ->
                  ListE . map (LitE . StringL) <$>
                  mapM
                      (\case
                           NormalC n _ -> pure $ nameBase n
                           _ -> e)
                      cons
              _ -> e)

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> opts)
        (fullDesc <> progDesc "Preview ohua graphs or print them as dot graphs" <>
         header "show")
  where
    opts =
        Opts <$>
        (hsubparser
             (command
                  "print"
                  (info
                       printParser
                       (progDesc "Print the ohua graph as viewable format. To run this command with the --format/-f option specified `graphviz` must be installed on the system. If no format is specified prints a `dot` file, this works *without* having `graphviz` installed.")) <>
              command
                  "preview"
                  (info
                       (pure Preview)
                       (progDesc "Preview an ohua graph with sensible settings"))) <|>
         pure Preview) <*>
        strArgument (metavar "GRAPH_FILE" <> help "Input graph")
    printParser =
        Print <$>
        ((Just <$>
          option
              auto
              (long "format" <> short 'f' <>
               help
                   (printf
                        "Available ouptut formats: %v, if omitted a `dot` file is produced."
                        (intercalate ", " formatOptions)))) <|>
         pure Nothing) <*>
        optional
            (strOption
                 (short 'o' <> long "output" <>
                  help
                      "Specify the output path for the file. Otherwise it will be generated from the input name and the specified format."))

main :: IO ()
main =
    execParser optsParser >>= \opts -> do
        gr <- ohuaGrToDot <$> readGraph (input opts)
        let dottized = PG.graphToDot PG.quickParams gr
        case Main.action opts of
            Preview -> PG.preview gr
            Print format output ->
                case format of
                    Nothing ->
                        printDot
                            (fromMaybe (input opts -<.> "dot") output)
                            dottized
                    Just f -> do
                        path <-
                            case output of
                                Nothing ->
                                    PG.addExtension
                                        (PG.runGraphviz dottized)
                                        f
                                        (dropExtension $ input opts)
                                Just p -> PG.runGraphviz dottized f p
                        printf "Saved output to %v\n" path
