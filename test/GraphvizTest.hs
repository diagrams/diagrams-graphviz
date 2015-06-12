{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.GraphViz

import           Data.GraphViz
import           Data.GraphViz.Attributes
import           Data.GraphViz.Commands

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()) ]
        )

main = do
  let params :: GraphvizParams n v e () v
      params = defaultParams { globalAttributes = [NodeAttrs [shape Circle]] }
  hex' <- layoutGraph' params Neato hex
  let hexDrawing :: Diagram B
      hexDrawing = drawGraph
                     (const $ place (circle 19))
                     (\v1 p1 v2 p2 e p -> stroke p)
                     hex'
  mainWith $ hexDrawing # frame 1
