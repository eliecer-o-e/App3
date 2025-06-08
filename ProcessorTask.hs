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
-- Esta es la función que realiza el algoritmo A*
-- Espera de parámetro la matriz, la lista de nodos abiertos, la lista de nodos cerrados, el nodo objetivo y la heurística
-- Devuelve la lista de nodos que forman el camino
cicloDelAlgoritmo :: [[Int]] -> [Node] -> [Node] -> Node ->  (Node -> Node -> Float) -> [Node]
cicloDelAlgoritmo _ [] _ _ _ =  []  -- si listaAbierta está vacío, no hay camino
cicloDelAlgoritmo matriz listaAbierta listaCerrada nodoMeta heuristica =
    let (actual, resto) = mayorDeLaLista listaAbierta -- extrae el nodo con el mayor valor de g+h de la lista de nodos abiertos
    in if pos actual == pos nodoMeta -- si el nodo actual es el nodo objetivo, entonces devolvemos el camino
        then reconstruirCamino actual []
        else
            trace ("Procesando Nodo posición: " ++ show (pos actual) ) $
            let listaVisitables = obtenerListaVisitables matriz actual listaCerrada -- expande el nodo actual
                -- actualiza los vecinos con el nuevo valor de g y h
                listaVisitablesActualizada = map (\n ->
                        let gTentativo = g actual + g n -- matriz !! fst (pos n) !! snd (pos n)
                            hEstimado  = heuristica n nodoMeta
                        in n { g = gTentativo, h = hEstimado, parent = Just actual }
                    ) listaVisitables

                nuevaListaCerrada = actual : listaCerrada -- agrega el nodo actual a la lista de nodos cerrados
                -- Solo los elementos que no estén en la lista cerrada.
                filteredNeighbors = filter (\n -> pos n `notElem` map pos nuevaListaCerrada) listaVisitablesActualizada
                newOpen = foldr actualizaListaAbierta resto filteredNeighbors
            in 
                trace ( show (map pos newOpen ) ++ "\n" ++ show (map pos nuevaListaCerrada) ++ "\n") $
                cicloDelAlgoritmo matriz newOpen nuevaListaCerrada nodoMeta  heuristica -- llama recursivamente a la función con la nueva lista de nodos abiertos y cerrados

-- Esta es la función que actualiza la lista de nodos abiertos
-- Espera de parámetro el nodo actual y la lista de nodos abiertos
-- Devuelve la lista de nodos abiertos actualizada
-- Si el nodo ya está en la lista de nodos abiertos, se actualiza su valor de g y h si es mayor
-- Si el nodo no está en la lista de nodos abiertos, se agrega al final de la lista
actualizaListaAbierta :: Node -> [Node] -> [Node]
actualizaListaAbierta nodo open =
    let mismaPos n = pos n == pos nodo
    in case filter mismaPos open of
        [] -> nodo : open  -- no está, lo agregamos
        (n:_) ->
            if g nodo > g n
            then nodo : filter (not . mismaPos) open  -- filter me devuelve todo open menos el que tiene la misma pos (nodo)
            else open  -- no es mejor, dejamos como está

-- Esta es la función que expande los nodos
-- Espera de parámetro la matriz, el nodo actual
-- Devuelve la lista de nodos vecinos que son visitables
obtenerListaVisitables :: [[Int]] -> Node -> [Node] -> [Node]
obtenerListaVisitables matriz (Node (fila, columna) _ _ _  _) listaCerrada =
    let filas    = length matriz
        columnas = length (head matriz)
        rangoValido (i, j) = i >= 0 && i < filas && j >= 0 && j < columnas

        posibles = [ (fila + 1, columna), (fila, columna + 1), (fila + 1, columna + 1), (fila, columna - 1), (fila - 1, columna)]
        neighbors = filter rangoValido posibles
        
        valorCelda (i, j) =
            let base = matriz !! i !! j
                baseAjustado = if base == 0 then (-3) else base
                esDiagonal = i == fila + 1 && j == columna + 1
            in if esDiagonal then baseAjustado - 2 else baseAjustado

    in map (\p -> creaNodo p (valorCelda p)) neighbors 


-- Esta es la función que extrae el nodo con el menor valor de g+h de la lista de nodos abiertos
-- Espera de parámetro la lista de nodos abiertos
-- Devuelve el nodo con el mayor valor de g+h y la lista de nodos abiertos sin ese nodo
mayorDeLaLista :: [Node] -> (Node, [Node])
mayorDeLaLista [] = error "Lista vacía"
mayorDeLaLista (x:xs) = extract x [] xs
    where
        extract mayor acumulador [] = (mayor, reverse acumulador)
        extract mayor acumulador (y:ys)
            | fromIntegral(g y) + h y > fromIntegral(g mayor) + h mayor = extract y (mayor : acumulador) ys -- si g+h es mayor, entonces y es el nuevo mayor
            | otherwise  = extract mayor (y : acumulador) ys



-- Esta es la función que reconstruye el camino
-- Espera de parámetro el nodo actual y la lista de nodos que forman el camino
-- Devuelve la lista de nodos que forman el camino
reconstruirCamino :: Node -> [Node] -> [Node]
reconstruirCamino current path =
    let newPath = current : path
    in case parent current of
        Nothing   -> newPath
        Just p -> reconstruirCamino p newPath


