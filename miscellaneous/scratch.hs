import EdgeNode
import Graph
import TensorGraph

instance TensorGraph Graph where
  emptyGraph = Graph.emptyGraph
  addNode = Graph.addNode
  addNodeIfNotExists = Graph.addNodeIfNotExists
  addEdge = Graph.addEdge
  getEdgeType = Graph.getEdgeType
  classifyNode = Graph.classifyNode
  setNodeType = Graph.setNodeType
  addEdgeToNode = Graph.addEdgeToNode
  getAllNodeIDXs = Graph.getAllNodeIDXs
  getAllEdgesOfType = Graph.getAllEdgesOfType

gEmpty = Graph.emptyGraph
(nodeIDX, g0) = Graph.addNode GGG gEmpty

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

--g = buildGraph peaceEdges (Graph.emptyGraph)
g = buildGraph sunP1EdgesList (Graph.emptyGraph)
gg = classifyAndSetNodes (Graph.getAllNodeIDXs g) g

