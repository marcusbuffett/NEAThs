module Genome where

import qualified Data.Map.Strict as M
import Debug.Trace(trace)
import Gene

data Genome = Genome {gNodes :: Nodes,
                      gConnections :: Connections}
                      deriving (Show)

type Connections = M.Map Int Connection
type Nodes = M.Map Int Node

connectionMutation :: Genome -> Connection -> Genome
connectionMutation genome c = genome {gConnections = connections}
  where
  connections = addConnection c (gConnections genome)

nodeMutation :: Node -> Connection -> Genome -> Genome
nodeMutation n c (Genome ns cs) = trace (show $ cOut c) $ Genome nodes connections
  where
  i = nInnovation n
  nodes = addNode n ns
  connections = disableConnection c $ addConnections newConnections cs
  newConnections = [connectionFromTo (cIn c) i 5,
                    connectionFromTo i (cOut c) 6]

addNode :: Node -> Nodes -> Nodes
addNode n = M.insert (nInnovation n) n

addConnections :: [Connection] -> Connections -> Connections
addConnections newCs cs = foldl (flip addConnection) cs newCs

addConnection :: Connection -> Connections -> Connections
addConnection c = M.insert (cInnovation c) c

disableConnection :: Connection -> Connections -> Connections
disableConnection c = M.adjust disable (cInnovation c)

