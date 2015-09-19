module Gene where

import Types (Innovation)

data NodeType = Sensor | Hidden | Output
                deriving (Show)

data Node = Node {nInnovation :: Innovation, 
                  nType :: NodeType,
                  nConnections :: [Innovation]}
                  deriving (Show)

data Connection = Connection {cInnovation :: Innovation,
                              cIn :: Innovation,
                              cOut :: Innovation,
                              cWeight :: Double,
                              cEnabled :: Bool}
                              deriving (Show)

instance Eq Node where
  a == b = nInnovation a == nInnovation b


connectionFromTo :: Innovation -> Innovation -> Innovation -> Connection
connectionFromTo f t i = Connection {cInnovation = i,
                                     cIn = f,
                                     cOut = t,
                                     cWeight = 1.0,
                                     cEnabled = True} 
disable :: Connection -> Connection
disable c = c {cEnabled = False}
