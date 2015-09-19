module Gene where

import Types (Innovation)

data NodeType = Sensor | Hidden | Output
                deriving (Show)

data Node = Node {nInnovation :: Innovation, 
                  nType :: NodeType,
                  nConnections :: [Innovation]}

data Connection = Connection {cInnovation :: Innovation,
                              cIn :: Innovation,
                              cOut :: Innovation,
                              cWeight :: Double,
                              cEnabled :: Bool}

instance Eq Node where
  a == b = nInnovation a == nInnovation b

instance Show Node where
  show (Node i t cs) = showInnovation ++ showType ++ showConnections
    where
      showInnovation = "Innovation : " ++ show i ++ " | "
      showType = "Type : " ++ show t ++ " | "
      showConnections = "Connections : " ++ show cs

instance Show Connection where
  show (Connection i inNode outNode weight enabled) = 
    showInnovation ++ showIn ++ showOut ++ showWeight
    where
      showInnovation = "Innovation : " ++ show i ++ " | "
      showIn         = "In : " ++ show inNode ++ " | "
      showOut = "Out : " ++ show outNode ++ " | "
      showWeight     = "Weight : " ++ show weight

connectionFromTo :: Innovation -> Innovation -> Innovation -> Connection
connectionFromTo f t i = Connection {cInnovation = i,
                                     cIn = f,
                                     cOut = t,
                                     cWeight = 1.0,
                                     cEnabled = True} 
disable :: Connection -> Connection
disable c = c {cEnabled = False}


