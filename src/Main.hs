{-# LANGUAGE TemplateHaskell, LambdaCase, RecordWildCards,
  TypeFamilies, TupleSections, TypeApplications #-}
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
import qualified Ohua.Types as OT
import qualified Ohua.ALang.PPrint as PP
import qualified Data.Text as T


-- This is a hack. Right now the `GraphFile` data structure is defined only in
-- ohuac, and therefore we do not have access to it here.
data P = P { graph :: G.OutGraph }

A.deriveFromJSON A.defaultOptions ''P


ohuaGrToDot :: G.OutGraph -> PG.Gr String String
ohuaGrToDot G.OutGraph {..} =
    PG.mkGraph
        ((envNode, "From Environment") : (litNode, "Literals") : map opToNode operators)
        (map (arcToEdge targetToInfo) (G.direct arcs) <>
         map (arcToEdge ((, "state") . unwrap)) (G.state arcs))
  where
    mkOpStr = T.unpack . PP.quickRender
    envNode = succ $ maximum (map (unwrap . G.operatorId) operators)
    litNode = succ envNode
    opToNode G.Operator {..} = (unwrap operatorId, mkOpStr operatorType)
    targetToInfo G.Target {..} = (unwrap operator, show index)

    arcToEdge :: (target -> (Int, String)) -> G.Arc target (G.Source OT.Lit) -> (Int, Int, String)
    arcToEdge targetToInfo' G.Arc {..} =
        (sOp, tOp, printf "%v -> %v" sIdx tIdx :: String)
      where
        (sOp, sIdx) =
            case source of
                G.LocalSource t -> targetToInfo t
                G.EnvSource (OT.EnvRefLit l) -> (envNode, show l)
                G.EnvSource e -> (litNode, case e of
                                              OT.NumericLit n -> show n
                                              OT.UnitLit -> "()"
                                              OT.FunRefLit (FunRef f _) -> "<funref" <> mkOpStr f <> " >"
                                              OT.EnvRefLit _ -> error "impossible"
                                )
        (tOp, tIdx) = targetToInfo' target

printDot :: FilePath -> PG.DotGraph PG.Node -> IO ()
printDot path = PG.writeFile path . PG.printDotGraph

readGraph :: FilePath -> IO G.OutGraph
readGraph p = do
  file <- BS.readFile p
  -- This is also kind of a hack. It fist attempts to parse as a `P`/`GraphFile`
  -- structure, if that fails it tries to parse it as a direct graph. In the
  -- future this may be changed.
  either error pure $ (graph <$> A.eitherDecode file) <|> A.eitherDecode file

data Opts = Opts
    { action :: Action
    , input :: FilePath
    }

data Action
    = Preview
    | Print (Maybe PG.GraphvizOutput)
            (Maybe FilePath)

-- This is some Template Haskell to get string representations for the
-- `GraphvizOutput` options which the graphviz library supports. Used for the
-- help text.
formatOptions :: [String]
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


printCommandDescription :: String
printCommandDescription =
    "Print the ohua graph as viewable format. \
    \To run this command with the --format/-f option specified \
    \`graphviz` must be installed on the system. \
    \If no format is specified prints a `dot` file, \
    \this works *without* having `graphviz` installed."

previewCommandDescription :: String
previewCommandDescription =
  "Preview an ohua graph with sensible settings. \
  \Note that this command may currently silently fail \
  \if certain necessary programs are not installed on the system. \
  \See Issue #1. If this happens use the `print` command \
  \in conjunction with the `graphviz` executable as a more stable process."

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
                  (info printParser (progDesc printCommandDescription)) <>
              command
                  "preview"
                  (info (pure Preview) (progDesc previewCommandDescription))) <|>
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
