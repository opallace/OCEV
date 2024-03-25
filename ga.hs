import DataTypes
import Chromosome
import Population

parseLine :: String -> [Int]
parseLine = map read . words

readFileLines :: FilePath -> IO [[Int]]
readFileLines path = do
    contents <- readFile path
    return (map parseLine (lines contents))

main :: IO()
main = do
    population <- generate_boolean_population 25 101 (False, True)

    content <- readFileLines "entrada.txt"

    best_chromossome <- ga population 100 content
    print best_chromossome

   