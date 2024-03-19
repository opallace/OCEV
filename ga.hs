module Main where

import System.IO
import System.Random
import Control.Monad
import Data.List

data TypeChromosome = 
        Binary
    |   Integer
    |   IntegerPermuted
    |   Real

data Chromosome = 
        BinaryChromosome Int [Int] 
    |   IntegerChromosome [Int]
    |   IntegerPermutedChromosome [Int]
    deriving (Show)

type Population = [Chromosome]

-- Generate a binary chromosome
generate_binary_chromosome :: Int -> (Int, Int) -> IO Chromosome
generate_binary_chromosome n_genes interval = do
    binaries <- replicateM n_genes (randomRIO interval)
    return $ BinaryChromosome 0 binaries

-- Generate an integer chromosome
generate_integer_chromosome :: Int -> (Int, Int) -> IO Chromosome
generate_integer_chromosome n_genes interval = do
    integers <- replicateM n_genes (randomRIO interval)
    return $ IntegerChromosome integers

-- Generate a population
generate_population :: TypeChromosome -> Int -> Int -> (Int, Int) -> IO Population
generate_population Binary n_population n_genes interval = do
    pop <- replicateM n_population (generate_binary_chromosome n_genes interval)
    return pop

generate_population Integer n_population n_genes interval = do
    pop <- replicateM n_population (generate_integer_chromosome n_genes interval)
    return pop

generate_population IntegerPermuted _ n_genes _ = do
    return $ map IntegerPermutedChromosome (permutations [1..n_genes])

intToBool::Int->Bool
intToBool 0 = False
intToBool 1 = True

-- Evaluate a chromosome
evaluate_chromosome :: [[Int]] -> Chromosome -> Int
evaluate_chromosome [] _ = 0
evaluate_chromosome (clause:clauses) (BinaryChromosome _ alleles) =
    let l0' = clause !! 0
        l1' = clause !! 1
        l2' = clause !! 2

        l0 = if l0' < 0 then not (intToBool (alleles !! (abs l0'))) else intToBool (alleles !! (abs l0'))
        l1 = if l1' < 0 then not (intToBool (alleles !! (abs l1'))) else intToBool (alleles !! (abs l1'))
        l2 = if l2' < 0 then not (intToBool (alleles !! (abs l2'))) else intToBool (alleles !! (abs l2'))

    in if l0 || l1 || l2
        then 1 + evaluate_chromosome clauses (BinaryChromosome 0 alleles)
        else evaluate_chromosome clauses (BinaryChromosome 0 alleles)

-- Evaluate a population
evaluate_population :: [[Int]] -> Population -> Population
evaluate_population _ [] = []
evaluate_population formula (chromosome:population) =
    case chromosome of
        BinaryChromosome fitness alleles ->
            let evaluatedFitness = evaluate_chromosome formula (BinaryChromosome fitness alleles)
            in (BinaryChromosome evaluatedFitness alleles) : evaluate_population formula population
        _ -> evaluate_population formula population

parseLine :: String -> [Int]
parseLine = map read . words

readFileLines :: FilePath -> IO [[Int]]
readFileLines path = do
    contents <- readFile path
    return (map parseLine (lines contents))

main :: IO()
main = do
    population <- generate_population Binary 1 101 (0,1)
    -- print binary_population

    content <- readFileLines "entrada.txt"
    -- print content

    let evaluated_population = evaluate_population content population
    print evaluated_population
