module Population where

import Control.Monad
import Data.List
import DataTypes
import Chromosome

------------------------------------- GENERATE POPULATION --------------------------------------------

generate_boolean_population :: Int -> Int -> (Bool, Bool) -> IO Population
generate_boolean_population n_population n_genes interval = do
    pop <- replicateM n_population (generate_binary_chromosome n_genes interval)
    return pop


generate_integer_population :: Int -> Int -> (Int, Int) -> IO Population
generate_integer_population n_population n_genes interval = do
    pop <- replicateM n_population (generate_integer_chromosome n_genes interval)
    return pop

generate_integer_permuted_population :: Int -> Int -> (Int, Int) -> IO Population
generate_integer_permuted_population _ n_genes _ = do
    return $ map (IntegerPermutedChromosome 0) (permutations [1..n_genes])


------------------------------------- EVALUATE POPULATION --------------------------------------------

evaluate_population :: [[Int]] -> Population -> Population
evaluate_population _ [] = []
evaluate_population formula (chromosome:population) =
    case chromosome of
        BinaryChromosome fitness alleles ->
            let evaluatedFitness = evaluate_chromosome formula (BinaryChromosome fitness alleles)
            in (BinaryChromosome evaluatedFitness alleles) : evaluate_population formula population
        _ -> evaluate_population formula population