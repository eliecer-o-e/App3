module Types where

-- pos: posición del nodo en la matriz (Fila, Columna)
-- weight: peso del nodo
-- g: costo acumulado desde el nodo inicial
-- h: heurística estimada del nodo al nodo objetivo
-- childs: lista de nodos hijos
-- parent: nodo padre
data Node = Node {pos :: (Int, Int), g :: Int, h :: Float, childs :: [Node], parent :: Maybe Node} deriving (Show, Eq, Ord)

creaNodo :: (Int, Int) -> Int -> Node
creaNodo pos g =
    Node
        {   pos = pos,
            g = g,
            h = 0,
            childs = [],
            parent = Nothing
        }
