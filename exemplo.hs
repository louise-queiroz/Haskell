idade :: Int -- Um valor inteiro constante
idade = 17
testeIdade :: Bool -- Usa a definicao de
testeIdade = idade>=18 -- idade

quadrado :: Int -> Int -- funcao que eleva num
quadrado x = x * x -- ao quadrado
mini :: Int -> Int -> Int --funcao que mostra
mini a b --o menor entre
    | a <= b = a -- dois valores
    | otherwise = b

maiorDeIdade :: Int -> Bool
maiorDeIdade x = x >= 18

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z) 