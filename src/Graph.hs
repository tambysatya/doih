module Graph where


import qualified Data.IntMap as M


data Graph v e = Graph {
                    _successors :: M.IntMap [Int]
                   ,_predecessors :: M.IntMap [Int]
                   ,_vlabels :: M.IntMap v
                   ,_eLabels :: M.IntMap (M.IntMap e) 
                   }



-- throws an error if the vertex (index) already exists.                   
addVertex ::  (Int, v) -> Graph v e -> Graph v e 
addVertex (i, vertex) g = g'
    where _vlabel' = M.insertWith (\ _ _ -> error ("Vertex "++ show i ++ " already exists")) i vertex (_vlabels g)
          _eLabels' = M.insertWith (\ _ _ -> error ("Edge "++ show i ++ " already exists")) i (M.empty ) (_eLabels g)
          --g' = Graph (_neighmap g) _vlabel' (_eLabels g)
          g' = g{_vlabels = _vlabel', _eLabels = _eLabels'}


addDirectedEdge :: ((Int, Int), e) -> Graph v e -> Graph v e
addDirectedEdge ((i,j), e_label) g =  g'
    where _eLabels1 = M.insertWith 
                        (\ prev new -> M.unionWith(\ _ _ -> error ("edge "++ show (i,j) ++ " already exists")) prev new)
                        i 
                        (M.singleton j e_label) 
                        (_eLabels g)
          --_elabels0 = M.insert i _eLabels1 g 
          _successors' = M.insertWith (++) i [j] (_successors g)
          _predecessors' = M.insertWith (++) j [i] (_predecessors g)
          g' = g{_successors = _successors', _predecessors= _predecessors', _eLabels=_eLabels1}

removeVertex :: Int -> Graph v e -> Graph v e
removeVertex i g = g'
    where _vlabels' = M.delete i (_vlabels g)
          g' = g{_vlabels = _vlabels'}

removeEdge :: (Int, Int) -> Graph v e -> Graph v e
removeEdge (i,j) g = g'
    where _eLabels' = M.adjust (M.delete j) i (_eLabels g)
          g' = g{_eLabels = _eLabels'} 

--given index and new label for vertex, updates the label
updateVertex :: Int -> v -> Graph v e -> Graph v e
updateVertex i newlabel g = g{_vlabels = _vlabels'}
    where _vlabels' = M.insert i newlabel (_vlabels g)

updateEdge :: (Int, Int) -> e -> Graph v e -> Graph v e
updateEdge (i,j) newedge g = g{_eLabels = _eLabels'}
    where _eLabels' = M.adjust (M.insert j newedge) i (_eLabels g)




emptyGraph :: Graph v e
emptyGraph = Graph M.empty M.empty M.empty M.empty

mkGraph :: [(Int, v)] -> [((Int,Int), e)] -> Graph v e
mkGraph vertices edges = g'
    where g0 = emptyGraph
          _vlabels0 g= foldr addVertex g  vertices 
          _eLabels0 g = foldr addDirectedEdge g edges
          g' = _eLabels0 $ _vlabels0 g0  

getVertices :: Graph v e -> [(Int, v)] 
getVertices g = M.assocs $ _vlabels g

getEdges :: Graph v e -> [((Int, Int), e)] 
getEdges g = [((i,j), label) | (i,dedge) <- M.assocs $ _eLabels g, (j,label) <- M.assocs dedge]

getVertexLabel :: Int -> Graph v e ->  Maybe v
getVertexLabel i g = M.lookup i (_vlabels g)

getEdgeLabel :: (Int, Int) -> Graph v e -> Maybe e 
getEdgeLabel (i,j) g = undefined
    where ret = case  (M.lookup i (_eLabels g)) of--fails because it can return Nothing?
                    Nothing -> Nothing
                    Just m -> M.lookup j m

getVertexDegree :: Int -> Graph v e -> Int 
getVertexDegree i g= (getInDegree i g) + (getOutDegree i g)

getInDegree :: Int -> Graph v e -> Int 
getInDegree i g=  length (getPredecessors i g)
    

getOutDegree :: Int -> Graph v e -> Int 
getOutDegree i g=  length (getSuccessors i g)
    
getSuccessors :: Int -> Graph v e -> Maybe [Int]
getSuccessors i g = M.lookup i (_successors g)

getPredecessors :: Int -> Graph v e -> Maybe [Int]
getPredecessors i g = M.lookup i (_predecessors g)

filterVertices :: ( Int -> v-> Bool ) -> Graph v e -> M.IntMap v
filterVertices f g = vlist
    where vlist = M.filterWithKey f (_vlabels g) 

{-filterEdges :: (Int -> Int -> e -> Bool) -> Graph v e -> M.IntMap (M.IntMap e)
filterEdges f g = undefined
   -- where elist = M.filterWithKey f (_eLabels g) -}

instance (Show v, Show e) => Show (Graph v e)
    where show g = "Vertices: " ++  show (M.assocs $ _vlabels g) 
                 ++ "\nEdges: " ++ show [((i,j), label ) | (i, dedges)<- M.assocs $ _eLabels g, (j, label)<- M.assocs dedges]


testGraph = mkGraph (zip [1,2,3,4] (repeat Nothing))  (zip [(1,2), (2,3)] (repeat Nothing))  