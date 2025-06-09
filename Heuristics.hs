module Heuristics where

import Types

-- Esta es la función que calcula la heurística de Manhattan
-- la distancia Manhattan es la suma de las distancias horizontales y verticales absolutas
manhattan :: Node -> Node -> Float
manhattan (Node { pos = (x1, y1) }) (Node { pos = (x2, y2) }) = fromIntegral (abs (x1  - x2) + abs (y1 - y2))

-- Esta es la función que calcula la heurística de Euclídea
-- Esta es la distancia "real" en línea recta
euclidean :: Node -> Node -> Float
euclidean (Node { pos = (x1, y1) }) (Node { pos = (x2, y2) }) = sqrt ((fromIntegral x1  - fromIntegral x2) ^ 2 + (fromIntegral y1 - fromIntegral y2) ^ 2)

-- Esta es la función que calcula la heurística de Chebyshev
-- Esta es la máxima de las distancias horizontal o vertical. 
chebyshev :: Node -> Node -> Float
chebyshev (Node { pos = (x1, y1) }) (Node { pos = (x2, y2) }) = fromIntegral (max (abs (fromIntegral x1 - fromIntegral x2)) (abs (fromIntegral y1  - fromIntegral y2)))
