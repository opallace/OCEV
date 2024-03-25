module Population where

import System.IO
import System.Random
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

------------------------------------- SELECT POPULATION --------------------------------------------
best_fitness_selection' :: Population -> Chromosome -> Chromosome
best_fitness_selection' [] x = x
best_fitness_selection' ((BinaryChromosome a b):xs) (BinaryChromosome c d)  | a > c = best_fitness_selection' xs (BinaryChromosome a b)
                                                                            | otherwise = best_fitness_selection' xs (BinaryChromosome c d)
best_fitness_selection :: Population -> Chromosome
best_fitness_selection ((BinaryChromosome a b):xs) =  best_fitness_selection' xs (BinaryChromosome a b)

random_element :: Population -> IO Chromosome
random_element population = do
    index <- randomRIO (0, length population - 1)
    return (population !! index)

random_elements :: Population -> Int -> IO Population
random_elements population n = sequence $ replicate n (random_element population)

tournament_selection :: Population -> IO Chromosome
tournament_selection population = do
    random_chromosomes <- random_elements population 5
    return $ best_fitness_selection random_chromosomes

------------------------------------- GENERATION --------------------------------------------
generation :: Population -> Int -> IO Population
generation _ 0 = return []
generation population n = do
    parent_1 <- tournament_selection population
    parent_2 <- tournament_selection population

    children' <- uniform_crossover_chromosomes parent_1 parent_2
    children <- mutation children'

    rest <- generation population (n - 1)

    return $ children:rest

ga :: Population -> Int -> [[Int]] -> IO Chromosome
ga population 0 _ = return (best_fitness_selection population)
ga population max_generations content = do
    let evaluated_population = evaluate_population content population
    new_generation_population <- generation evaluated_population (length evaluated_population)
    let evaluated_new_generation_population = evaluate_population content new_generation_population
    ga evaluated_new_generation_population (max_generations - 1) content
