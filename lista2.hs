--Implementar a função que recebe um inteiro 
--e uma lista e multiplica todos os elementos dessa lista pelo inteiro
multLista :: Int -> [Int] -> [Int]
multLista _ [] = []
multLista n (x:xs) = (n*x) : multLista n xs


--Implemente a função que recebe um inteiro e uma lista, e devolve um booleano dizendo 
--se o inteiro se encontra na lista
elemento :: Int -> [Int] -> Bool
elemento _ [] = False
elemento n (x:xs)
    | n == x    = True
    | otherwise = elemento n xs

--Implemente a função que recebe um inteiro e uma lista, e diz quantas vezes o inteiro ocorre dentro da lista
conta :: Int -> [Int] -> Int
conta _ [] = 0
conta n (x:xs)
    | n == x  = 1 + conta n xs
    | otherwise = conta n xs

--Implemente a função que recebe um inteiro e uma lista e conta quantos elementos da 
--lista são maiores que o inteiro passado como argumento
contaMaiores :: Int -> [Int]-> Int
contaMaiores _ [] = 0
contaMaiores n (x:xs)
    | n < x  = 1 + contaMaiores n xs
    | otherwise = conta n xs

-- Implemente a função que recebe um inteiro e uma lista e devolve uma lista contendo somente os 
--valores que estavam na lista inicial e que são maiores do que o inteiro passado como argumento
maiores :: Int -> [Int] -> [Int]
maiores _ [] = []
maiores n (x:xs)
    |n < x = x : maiores n xs
    | otherwise = maiores n xs

--Implementar a função que recebe um inteiro m e um inteiro n e devolve uma lista contendo m vezes n
geraLista :: Int -> Int -> [Int]
geraLista x y = replicate x y 

--Implementar a função que recebe um inteiro, uma lista e adiciona o elemento no fim da lista (sem usar o ++):
addFim :: Int -> [Int] -> [Int]
addFim x [] = [x]
addFim x (y:ys) = y : addFim x ys

-- Implementar a função que recebe duas listas e concatena as mesmas gerando uma nova lista:
join :: [Int] -> [Int] ->[Int]
join [] ys = ys
join (x:xs) ys = x : join xs ys

-- Implementar a função inverte que recebe uma lista e devolve a mesma invertida
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]