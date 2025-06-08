module Matrix where


import System.IO

-- Esta es la función que lee el archivo que le dan de parámetro y lo convierte en una matriz y retorna la matriz, el número de filas y el número de columnas
readMatrix :: FilePath -> IO ([[Int]], Int, Int)
readMatrix path = do
    text <- readFile path
    let matrix :: [[Int]]
        matrix = map (map read . words) (lines text)
    let numFilas = length matrix
    let numColumnas = length (head matrix)
    {-
    mapM_ (\(i, fila) -> do
        mapM_ (\(j, elem) ->
            putStr $ show elem ++ " "
          ) (zip [0..] fila)
        putStrLn ""
      ) (zip [0..] matrix)
    -}
    return (matrix, numFilas, numColumnas)
