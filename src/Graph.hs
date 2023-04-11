module Graph where


import qualified Data.IntMap as M


data Graph v e = Graph {
                    neighmap :: M.IntMap [Int]
                   ,vlabels :: M.IntMap v
                   ,vEdges :: M.IntMap (M.IntMap e)
                   }
                    





