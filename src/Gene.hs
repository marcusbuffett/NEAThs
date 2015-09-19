module Gene where

data NodeType = Sensor | Hidden | Output
                deriving (Show)

data Node = Node {nInnovation :: Int, 
                  nType :: NodeType}
                  deriving (Show)

data Connection = Connection {cInnovation :: Int,
                              cIn :: Int,
                              cOut :: Int,
                              cWeight :: Double,
                              cEnabled :: Bool}
                              deriving (Show)

instance Eq Node where
  a == b = nInnovation a == nInnovation b


connectionFromTo :: Int -> Int -> Int -> Connection
connectionFromTo f t i = Connection {cInnovation = i,
                                     cIn = f,
                                     cOut = t,
                                     cWeight = 1.0,
                                     cEnabled = True} 
disable :: Connection -> Connection
disable c = c {cEnabled = False}
