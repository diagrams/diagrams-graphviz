{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.GraphViz

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Commands

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )

main = do
  let params :: GraphvizParams n v e () v
      params = defaultParams { globalAttributes = [NodeAttrs [shape Circle], GraphAttrs [Overlap ScaleOverlaps, Splines SplineEdges]] }
  hex' <- layoutGraph' params Dot hex
  let hexDrawing :: Diagram B
      hexDrawing = drawGraph
                     (const $ place (circle 19))
                     (\v1 p1 v2 p2 e p -> stroke p)
                     hex'
  mainWith $ hexDrawing # frame 1
