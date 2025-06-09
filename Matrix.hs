module Matrix where


import System.IO

-- Esta es la función que lee el archivo que le dan de parámetro y lo convierte en una matriz y retorna la matriz, el número de filas y el número de columnas
readMatrix :: FilePath -> IO ([[Int]], Int, Int)
readMatrix path = do
    -- Primero se lee el contenido completo del archivo
    text <- readFile path
    let matrix :: [[Int]]
        matrix = map (map read . words) (lines text)
    -- Se calcula la cantidad de filas como la cantidad de listas internas
    let numFilas = length matrix
    -- Se calcula la cantidad de columnas tomando la cantidad de elementos de la primera fila
    let numColumnas = length (head matrix)
    {-
    mapM_ (\(i, fila) -> do
        mapM_ (\(j, elem) ->
            putStr $ show elem ++ " "
          ) (zip [0..] fila)
        putStrLn ""
      ) (zip [0..] matrix)
    -}
    -- Finalmente se retorna la matriz junto con sus dimensiones
    return (matrix, numFilas, numColumnas)
