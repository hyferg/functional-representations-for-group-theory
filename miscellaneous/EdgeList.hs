module EdgeList where

import EdgeNode

peaceEdges = [
  Edge Gluon (0,1),
  Edge Gluon (0,3),
  Edge Gluon (0,2),
  Edge Gluon (1,2),
  Edge Gluon (1,3),
  Edge Gluon (2,3)
             ]

sunP1EdgesList = [
  Edge Up (0,2),
  Edge Down (1,2),
  Edge Gluon (2,3),
  Edge Down (3,5),
  Edge Up (3,4)
             ]

sunP1EdgesListAlt = [
  Edge Up (0,2),
  Edge Down (1,2),
  Edge Gluon (2,3),
  Edge Up (5,3),
  Edge Up (3,4)
             ]

chainEdges = [
  Edge Gluon (0,1),
  Edge Gluon (1,2),
  Edge Gluon (2,3)
 ]
