module DataTypes where

data Chromosome = 
        BinaryChromosome Int [Bool] 
    |   IntegerChromosome Int [Int]
    |   IntegerPermutedChromosome Int [Int]
    deriving (Show)

type Population = [Chromosome]