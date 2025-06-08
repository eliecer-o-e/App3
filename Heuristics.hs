module Heuristics where

import Types

-- Esta es la función que calcula la heurística de Manhattan
manhattan :: Node -> Node -> Float
manhattan (Node { pos = (x1, y1) }) (Node { pos = (x2, y2) }) = fromIntegral (abs (x1  - x2) + abs (y1 - y2))

-- Esta es la función que calcula la heurística de Euclídea
euclidean :: Node -> Node -> Float
euclidean (Node { pos = (x1, y1) }) (Node { pos = (x2, y2) }) = sqrt ((fromIntegral x1  - fromIntegral x2) ^ 2 + (fromIntegral y1 - fromIntegral y2) ^ 2)

-- Esta es la función que calcula la heurística de Chebyshev
chebyshev :: Node -> Node -> Float
chebyshev (Node { pos = (x1, y1) }) (Node { pos = (x2, y2) }) = fromIntegral (max (abs (fromIntegral x1 - fromIntegral x2)) (abs (fromIntegral y1  - fromIntegral y2)))
