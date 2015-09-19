module NEAT where

import Genome
import Gene
import qualified Data.Map.Strict as M

main = do
  print exampleGenome
  print $ nodeMutation n2 c1 exampleGenome

addTwo = (+)

exampleGenome :: Genome
exampleGenome = Genome exampleNodes exampleConnections

exampleNodes = M.fromList $ map (\n@(Node i _) -> (i, n)) [n1,n3]

n1 = Node 1 Sensor
n2 = Node 2 Hidden
n3 = Node 3 Output

exampleConnections = M.fromList $ map (\c@(Connection i _ _ _ _) -> (i, c)) [c1]

c1 = Connection {cInnovation = 1,
                 cIn = 1,
                 cOut = 3,
                 cWeight = 1.0,
                 cEnabled = True}
{- c2 = Connection {cInnovation = 2, -}
                 {- cIn = 2, -}
                 {- cOut = 3, -}
                 {- cWeight = 1.0, -}
                 {- cEnabled = True} -}
{- c3 = Connection {cInnovation = 3, -}
                 {- cIn = 1, -}
                 {- cOut = 3, -}
                 {- cWeight = 1.0, -}
                 {- cEnabled = True} -}

