import System.Environment (getArgs)
import ProcessorTask (astar)
import Heuristics (manhattan, euclidean)
import Types (pos, g)

import System.Exit (die)
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [matrixString, energyString] -> do
            let matrix = read matrixString :: [[Int]]

            -- esto estaría mal energy :: Int
            let energy = read energyString :: Int
            putStrLn ("Energía: " ++ show energy)

            mapM_ (\(i, fila) -> do
                mapM_ (\(j, elem) ->
                        putStr $ show elem ++ " "
                    ) (zip [0..] fila)
                putStrLn ""
                ) (zip [0..] matrix)
            
            let filas = length matrix
            -- head matrix obtiene la primera fila
            let columnas =  length (head matrix)

            let entrada = (0, 0)
            let salida = ( filas - 1, columnas - 1)

            let ruta = astar matrix entrada salida  manhattan
            
            putStrLn (intercalate " -> " (map (\n -> show (pos n) ++ ": " ++ show (g n)) ruta))
        _ -> die "Uso: ./App3 [[Matriz]] energy"
