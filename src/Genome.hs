module Genome where

import qualified Data.Map.Strict as M
import Text.Printf
import Debug.Trace(trace)
import Gene
import Types(Innovation, Input, Output)

data Genome = Genome {gNodes :: Nodes,
                      gConnections :: Connections,
                      gCounters :: (IO Int, IO Int)}

type Connections = M.Map Int Connection
type Nodes = M.Map Int Node

instance Show Genome where
  show (Genome nMap cMap _) = "Nodes : \n" ++ nodes ++ "\n" ++
                              "Connections : \n" ++ connections
    where
      nodes = unlines $ map show (M.elems nMap)
      connections = unlines $ map show (M.elems cMap)

output :: Input -> Genome -> Output
output input genome = undefined

connectionMutation :: Connection -> Genome -> Genome
connectionMutation = addConnection

nodeMutation :: Connection -> Genome -> Genome
nodeMutation c genome@(Genome ns cs _) = newGenome
  where
  n = Node 8 Hidden []
  i = nInnovation n
  newGenome = (disableOld . addConnections newConnections . addNode n) genome
  disableOld = disableConnection c
  addNewConnections = addConnections newConnections
  newConnections = [toNodeConnection, fromNodeConnection]
  toNodeConnection = connectionFromTo (cIn c) i 5
  fromNodeConnection = connectionFromTo i (cOut c) 6

addNode :: Node -> Genome -> Genome
addNode n genome = genome {gNodes = newNodes}
  where
  newNodes = M.insert (nInnovation n) n (gNodes genome)

addConnections :: [Connection] -> Genome -> Genome
addConnections cs genome = foldl (flip addConnection) genome cs

linkNodeToConnections :: Node -> [Connection] -> Genome -> Genome
linkNodeToConnections n cs genome@(Genome nMap cMap _) = newGenome
  where
  newGenome = foldl (\g c -> linkNodeToConnection n c g) genome cs

addConnection :: Connection -> Genome -> Genome
addConnection c genome@(Genome nMap cMap _) = newGenome
  where
  newGenome = withNewNodes {gConnections = newConnections}
  newConnections = M.insert (cInnovation c) c (gConnections genome)
  withNewNodes   = foldl (\g node -> linkNodeToConnection node c g) genome nodes
  nodes = [nMap M.! cOut c, nMap M.! cIn c]

disableConnection :: Connection -> Genome -> Genome
disableConnection c genome@(Genome nMap cMap _) = 
  genome {gConnections = newConnections}
  where
    newConnections = M.adjust disable (cInnovation c) cMap

linkNodeToConnection :: Node -> Connection -> Genome -> Genome
linkNodeToConnection n c genome@(Genome nMap cMap _) = genome {gNodes = newNodeMap}
  where 
  newNodeMap = M.insert (nInnovation n') n' nMap
  n' = n {nConnections = cInnovation c : nConnections n}
