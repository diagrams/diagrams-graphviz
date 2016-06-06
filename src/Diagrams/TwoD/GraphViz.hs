{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.GraphViz
-- Copyright   :  (c) 2014, 2015 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- A simple module with some "glue code" necessary for using diagrams
-- and GraphViz (<http://www.graphviz.org/>) in conjunction.  GraphViz
-- is great at laying out graphs but terrible at drawing them, so why
-- not let GraphViz do what it is good at, and use a dedicated drawing
-- library for the actual drawing?
--
-- Here is some example code to lay out and draw a simple directed
-- graph hierarchically:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- >
-- > import           Diagrams.Backend.Rasterific.CmdLine
-- > import           Diagrams.Prelude
-- > import           Diagrams.TwoD.GraphViz
-- >
-- > import           Data.GraphViz
-- > import           Data.GraphViz.Commands
-- >
-- > hex = mkGraph [0..19]
-- >         (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
-- >          ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
-- >          ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
-- >         )
-- >
-- > graphvizExample1 = do
-- >   hex' <- layoutGraph Dot hex
-- >   let hexDrawing :: Diagram B
-- >       hexDrawing = drawGraph
-- >                      (const $ place (circle 19))
-- >                      (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)
-- >                      hex'
-- >       opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)
-- >   return (hexDrawing # frame 1)
--
-- There are a few quirks to note.
--
--   * GraphViz seems to assume the circular nodes have radius 19.
--
--   * Note how we draw an arrow for each edge, and use the path
--     computed by GraphViz (which might be curved) to specify the shaft
--     for the arrow.
--
-- Here is a slightly modified example, which tells GraphViz not to
-- use any arrowheads on the edges:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- >
-- > import           Diagrams.Backend.Rasterific.CmdLine
-- > import           Diagrams.Prelude
-- > import           Diagrams.TwoD.GraphViz
-- >
-- > import           Data.GraphViz
-- > import           Data.GraphViz.Attributes.Complete
-- > import           Data.GraphViz.Commands
-- >
-- > hex = mkGraph [0..19]
-- >         (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
-- >          ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
-- >          ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
-- >         )
-- >
-- > main = do
-- >   let params :: GraphvizParams Int v e () v
-- >       params = defaultDiaParams
-- >                { fmtEdge = const [arrowTo noArrow] }
-- >   hex' <- layoutGraph' params Dot hex
-- >   let hexDrawing :: Diagram B
-- >       hexDrawing = drawGraph
-- >                      (const $ place (circle 19))
-- >                      (\_ _ _ _ _ p -> stroke p)
-- >                      hex'
-- >   mainWith $ hexDrawing # frame 1
--
--   * The type signature on @params@ is unfortunately necessary;
--     otherwise some ambiguity errors arise.
--
--   * Note how in this simple case we can just draw the path
--     for each edge directly.
-----------------------------------------------------------------------------

module Diagrams.TwoD.GraphViz (
    mkGraph
  , layoutGraph
  , layoutGraph'
  , defaultDiaParams

  , drawGraph
  , getGraph
  ) where

import           Diagrams.Prelude

import qualified Data.Graph.Inductive.Graph        as G (Graph, Node, labEdges,
                                                         labNodes, mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     hiding (Path, attrs)
import           Data.GraphViz.Attributes.Complete as G (Attribute (Pos, Overlap, Splines),
                                                         EdgeType (SplineEdges), Overlap (ScaleOverlaps),
                                                         Point (..), Pos (..),
                                                         Spline (..))
import           Data.GraphViz.Commands.IO         (hGetDot)
import           Data.GraphViz.Types.Generalised   (FromGeneralisedDot (..))

import           Data.List                         (group, sort)
import           Data.List.Split                   (chunksOf)
import qualified Data.Map                          as M
import           Data.Maybe                        (catMaybes, fromJust)
import           Data.Tuple                        (swap)

-- | Construct a graph from a list of vertex labels (which must be unique) and
--   a list of (directed) edges.  The result is suitable as input to 'layoutGraph'.
mkGraph :: Ord v => [v] -> [(v,v,e)] -> Gr v e
mkGraph vs es = G.mkGraph vpairs edges
  where
    vpairs = zip [0..] (map head . group . sort $ vs)
    vmap   = M.fromList $ map swap vpairs
    edges  = catMaybes $ map mkEdge es
    mkEdge (v1,v2,e) = (,,) <$> M.lookup v1 vmap <*> M.lookup v2 vmap <*> pure e

-- | Decompose an annotated, concretely laid-out graph into a map from vertex labels to
--   points and a collection of edges associating vertex and edge
--   labels to 'Path' values.  This is used internally by 'drawGraph',
--   but exported since it may also be useful for more fine-grained
--   control over graph drawing.
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
    getSpline (Spline { splinePoints = pt1:pts}) = thePath
      where
        ptGroups = chunksOf 3 (map pointToP2 pts)
        fixedBeziers = zipWith mkBez (pointToP2 pt1 : map last ptGroups) ptGroups
        mkBez x1 [c1,c2,x2] = FCubic x1 c1 c2 x2
        mkBez _ _ = error "Diagrams.TwoD.GraphViz.getGraph.mkBez: impossible!"
        thePath         = fromLocSegments . fixup . map fromFixedSeg $ fixedBeziers
        fixup []        = [] `at` origin
        fixup (b1:rest) = (unLoc b1 : map unLoc rest) `at` loc b1
    getSpline _ = error "Diagrams.TwoD.GraphViz.getGraph: don't know what to do with empty spline!"


-- | Convert a GraphViz point to a diagrams point.
pointToP2 :: G.Point -> P2 Double
pointToP2 (G.Point {xCoord = x, yCoord = y}) = x ^& y

-- | Render an annotated graph as a diagram, given functions
--   controlling the drawing of vertices and of edges.  The first
--   function is given the label and location of each vertex. The
--   second function, for each edge, is given the label and location
--   of the first vertex, the label and location of the second vertex,
--   and the label and path corresponding to the edge.
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

-- | Round-trip a graph through an external graphviz layout algorithm, and
--   read back in a version annotated with explicit positioning
--   information.  The result is suitable for input to 'drawGraph' or,
--   more directly, to 'getGraph'.  The 'GraphvizCommand' should be
--   something like @Dot@ or @Neato@; to access them you should import
--   "Data.GraphViz.Command".  For more control over the functioning
--   of graphviz, see 'layoutGraph''.
layoutGraph
  :: forall gr v e. G.Graph gr
  => GraphvizCommand
  -> gr v e
  -> IO (gr (AttributeNode v) (AttributeEdge e))
layoutGraph = layoutGraph' (defaultDiaParams :: GraphvizParams G.Node v e () v)

-- | Like 'layoutGraph', but with an extra 'GraphvizParams' parameter
--   controlling various aspects of the graphviz layout process.  See
--   'defaultDiaParams', and the "Data.GraphViz.Attributes" and
--   "Data.GraphViz.Attributes.Complete" modules.
layoutGraph'
  :: (Ord cl, G.Graph gr)
  => GraphvizParams G.Node v e cl l
  -> GraphvizCommand
  -> gr v e
  -> IO (gr (AttributeNode v) (AttributeEdge e))
layoutGraph' params com gr = dotAttributes' com (isDirected params) gr' asDot
  where
    asDot = graphToDot params' gr'
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
dotAttributes' command _isDir gr asDot
  = augmentGraph gr . parseDG <$> graphvizWithHandle command asDot DotOutput hGetDot
  where
    parseDG = (`asTypeOf` asDot) . fromGeneralised
