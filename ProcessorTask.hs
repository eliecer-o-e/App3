module ProcessorTask where

import Types (Node(..), creaNodo)
import Heuristics (manhattan, euclidean, chebyshev)
import Debug.Trace (trace)

-- Esta es la función que realiza el algoritmo A*
-- Espera de parámetro la matriz, el punto de entrada start, el punto de salida end y la heurística
-- Devuelve la lista de nodos que forman el camino
astar :: [[Int]] -> (Int, Int) -> (Int, Int)-> (Node -> Node -> Float) -> [Node]
astar matriz start end  heuristica =
    let nodoInicio = creaNodo start (head (head matriz))
        nodoMeta  = creaNodo end (matriz !! fst end !! snd end)
        listaAbierta = [nodoInicio]
        listaCerrada = []
        currentNode = nodoInicio
    in
        -- trace ("Iniciando: " ++ show nodoMeta ++ "\n") $
        cicloDelAlgoritmo matriz listaAbierta listaCerrada nodoMeta  heuristica
