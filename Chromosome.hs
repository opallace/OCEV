module Chromosome where

import System.IO
import System.Random
import Control.Monad
import Data.List
import DataTypes

------------------------------------- GENERATE CHROMOSOME --------------------------------------------

generate_binary_chromosome :: Int -> (Bool, Bool) -> IO Chromosome
generate_binary_chromosome n_genes interval = do
    binaries <- replicateM n_genes (randomRIO interval)
    return $ BinaryChromosome 0 binaries

generate_integer_chromosome :: Int -> (Int, Int) -> IO Chromosome
generate_integer_chromosome n_genes interval = do
    integers <- replicateM n_genes (randomRIO interval)
    return $ IntegerChromosome 0 integers

------------------------------------- EVALUATE CHROMOSOME --------------------------------------------

evaluate_chromosome :: [[Int]] -> Chromosome -> Int
evaluate_chromosome [] _ = 0
evaluate_chromosome (clause:clauses) (BinaryChromosome _ alleles) =
    let l0' = clause !! 0
        l1' = clause !! 1
        l2' = clause !! 2

        l0 = if l0' < 0 then not (alleles !! (abs l0')) else  alleles !! (abs l0')
        l1 = if l1' < 0 then not (alleles !! (abs l1')) else  alleles !! (abs l1')
        l2 = if l2' < 0 then not (alleles !! (abs l2')) else  alleles !! (abs l2')

    in if l0 || l1 || l2
        then 1 + evaluate_chromosome clauses (BinaryChromosome 0 alleles)
        else evaluate_chromosome clauses (BinaryChromosome 0 alleles)