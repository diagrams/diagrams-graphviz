{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.GraphViz
-- Copyright   :  (c) 2014, 2015 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- XXX
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.GraphViz (
    mkGraph
  , layoutGraph
  , layoutGraph'
  , defaultDiaParams

  , getGraph
  , drawGraph
  ) where

import           Diagrams.Prelude

import qualified Data.Graph.Inductive.Graph        as G (Graph, Node, labEdges,
                                                         labNodes, mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     hiding (Path)
import           Data.GraphViz.Attributes.Complete as G (Attribute (Pos),
                                                         Point (..), Pos (..),
                                                         Spline (..))
import           Data.GraphViz.Commands.IO         (hGetDot)
import           Data.GraphViz.Types.Generalised   (FromGeneralisedDot (..))

import           Data.List                         (group, sort)
import           Data.List.Split                   (chunksOf)
import qualified Data.Map                          as M
import           Data.Maybe                        (catMaybes, fromJust,
                                                    maybeToList)
import           Data.Tuple                        (swap)

-- | Construct a graph from a list of vertex labels (which must be unique) and
--   a list of edges.  The result is suitable as input to 'layoutGraph'.
mkGraph :: Ord v => [v] -> [(v,v,e)] -> Gr v e
mkGraph vs es = G.mkGraph vpairs edges
  where
    vpairs = zip [0..] (map head . group . sort $ vs)
    vmap   = M.fromList $ map swap vpairs
    edges  = catMaybes $ map mkEdge es
    mkEdge (v1,v2,e) = (,,) <$> M.lookup v1 vmap <*> M.lookup v2 vmap <*> pure e

-- | Decompose an annotated, concretely laid-out graph into a map from vertex labels to
--   points and a collection of edges associating vertex and edge
--   labels to 'Path' values.
getGraph
  :: Ord v
  => Gr (AttributeNode v) (AttributeNode e)
  -> (M.Map v (P2 Double), [(v, v, e, Path V2 Double)])
getGraph gr = (vmap, edges)
  where
    nodes = G.labNodes gr
    vmap = M.fromList [ (v, pointToP2 pt) | (_,(attrs,v)) <- nodes, Pos (PointPos pt) <- attrs ]
    ixmap = M.fromList [ (i,v) | (i,(_,v)) <- nodes ]
    edges = [ (fromJust $ M.lookup i ixmap, fromJust $ M.lookup j ixmap, e, getPath attrs)
            | (i, j, (attrs,e)) <- G.labEdges gr
            ]
    getPath attrs = case [ss | Pos (SplinePos ss) <- attrs] of
      [splines] -> mconcat . map getSpline $ splines
      _ -> mempty
    getSpline (Spline { startPoint = s, endPoint = e, splinePoints = pt1:pts}) = thePath
      where
        ptGroups = chunksOf 3 (map pointToP2 pts)
        fixedBeziers = zipWith mkBez (pointToP2 pt1 : map last ptGroups) ptGroups
        mkBez x1 [c1,c2,x2] = FCubic x1 c1 c2 x2
        thePath         = fromLocSegments . fixup . map fromFixedSeg $ fixedBeziers
        fixup []        = [] `at` origin
        fixup (b1:rest) = (unLoc b1 : map unLoc rest) `at` loc b1

-- | Convert a GraphViz point to a diagrams point.
pointToP2 :: G.Point -> P2 Double
pointToP2 (G.Point {xCoord = x, yCoord = y}) = x ^& y

-- | Render an annotated graph as a diagram, given functions
-- controlling the drawing of vertices and of edges.
drawGraph
  :: (Ord v, Semigroup m)
  => (v -> P2 Double -> QDiagram b V2 Double m)
  -> (v -> P2 Double -> v -> P2 Double -> e -> Path V2 Double -> QDiagram b V2 Double m)
  -> Gr (AttributeNode v) (AttributeNode e)
  -> QDiagram b V2 Double m
drawGraph drawV drawE gr
  = mconcat (map drawE' edges)
 <> mconcat (map (uncurry drawV) (M.assocs vmap))
  where
    (vmap, edges) = getGraph gr
    drawE' (v1,v2,e,p)
      = drawE v1 (fromJust $ M.lookup v1 vmap) v2 (fromJust $ M.lookup v2 vmap) e p

------------------------------------------------

-- | Round-trip a graph through the external graphviz command, and
--   read back in a version annotated with explicit positioning
--   information.  The result is suitable for input to 'drawGraph' or,
--   more directly, to 'getGraph'.  XXX 'GraphvizCommand'.  For more
--   control over the functioning of graphviz, see 'layoutGraph''.
layoutGraph
  :: forall gr v e. G.Graph gr
  => GraphvizCommand
  -> gr v e
  -> IO (gr (AttributeNode v) (AttributeEdge e))
layoutGraph = layoutGraph' (defaultDiaParams :: GraphvizParams G.Node v e () v)

-- | Like 'layoutGraph', but with an extra 'GraphvizParams' parameter
--   controlling various aspects of the graphviz layout process.  XXX
--   more info.  Try it.
layoutGraph'
  :: (Ord cl, G.Graph gr)
  => GraphvizParams G.Node v e cl l
  -> GraphvizCommand
  -> gr v e
  -> IO (gr (AttributeNode v) (AttributeEdge e))
layoutGraph' params com gr = dotAttributes' com (isDirected params) gr' dot
  where
    dot = graphToDot params' gr'
    params' = params { fmtEdge = setEdgeIDAttribute $ fmtEdge params }
    gr' = addEdgeIDs gr

-- | Some convenient parameters for GraphViz which work better for
--   diagrams than the default.  In particular, use circular nodes
--   (instead of the default ovals), and allow cubic splines for
--   edges.
defaultDiaParams :: GraphvizParams G.Node v e cl v
defaultDiaParams
  = defaultParams
    { globalAttributes =
      [ NodeAttrs [shape Circle]
      , GraphAttrs [Overlap ScaleOverlaps, Splines SplineEdges]
      ]
    }

-- This should not be exported.  It is more or less copied from the
-- graphviz package source; the problem is that graphviz does not
-- export any way to have this parameterized by the GraphvizCommand.
dotAttributes' :: (G.Graph gr, PPDotRepr dg G.Node, FromGeneralisedDot dg G.Node)
                  => GraphvizCommand -> Bool -> gr v (EdgeID e)
                  -> dg G.Node -> IO (gr (AttributeNode v) (AttributeEdge e))
dotAttributes' command isDir gr dot
  = augmentGraph gr . parseDG <$> graphvizWithHandle command dot DotOutput hGetDot
  where
    parseDG = (`asTypeOf` dot) . fromGeneralised
