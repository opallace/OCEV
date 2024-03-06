module Main where

import System.Random
import Control.Monad
import Data.List

data TypeCromossome = 
        Binary
    |   Integer
    |   IntegerPermuted
    |   Real

data Cromossome = 
        BinaryCromossome [Int] 
    |   IntegerCromossome [Int]
    |   IntegerPermutedCromossome [Int]
    |   RealCromossome Int [(Float, Float)]
    deriving (Show)

type Population = [Cromossome]

--Generate a binary cromossome
generate_binary_cromossome :: Int -> (Int, Int) -> IO Cromossome
generate_binary_cromossome n_genes interval = do
    binaries <- replicateM n_genes (randomRIO interval)
    return $ BinaryCromossome binaries

--Generate a integer cromossome
generate_integer_cromossome :: Int -> (Int, Int) -> IO Cromossome
generate_integer_cromossome n_genes interval = do
    inteiros <- replicateM n_genes (randomRIO interval)
    return $ IntegerCromossome inteiros

--Generate a poppulation
generate_population :: TypeCromossome -> Int -> Int -> (Int, Int) -> IO Population

generate_population Binary n_population n_genes interval = do
    pop <- replicateM n_population (generate_binary_cromossome n_genes interval)
    return $ pop

generate_population Integer n_population n_genes interval = do
    pop <- replicateM n_population (generate_integer_cromossome n_genes interval)
    return $ pop

generate_population IntegerPermuted _ n_genes _ = do
    return $ map IntegerPermutedCromossome (permutations [1..n_genes])

main :: IO()
main = do
    binary_population <- generate_population Binary 3 5 (0,1)
    print $ binary_population

    integer_population <- generate_population Integer 3 5 (-10,10)
    print $ integer_population

    integer_permuted_population <- generate_population IntegerPermuted 0 2 (0,0)
    print $ integer_permuted_population
