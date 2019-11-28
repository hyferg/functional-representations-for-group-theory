import EdgeNode
import GraphV2 as Graph
import TensorGraph
import SunP1
import Chain
import EdgeList

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
  removeEdgeFromNode = Graph.removeEdgeFromNode
  deleteEdge = Graph.deleteEdge
  replaceEdgeInNode = Graph.replaceEdgeInNode
  --getEdges = Graph.getEdges
  getOrientedEdges = Graph.getOrientedEdges


g_ = buildGraph chainEdges (Graph.emptyGraph)
g = classifyAndSetNodes (Graph.getAllNodeIDXs g_) g_

e = nextEdgeOfType (-1) Gluon g
es = Graph.getAllEdgesOfType Gluon g
