-- 1) Escreva a fun¸c˜ao osQuatroSaoIguais que possui tipo
--Int -> Int -> Int -> Int -> Bool
-- que retorna True se seus quatro argumentos s˜ao iguais

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (b == c) && (c == d) 

-- 2) Defina a fun¸c˜ao quantosSaoIguais :: Int -> Int -> Int -> Int que
-- ecebe 3 valores e diz quantos desses valores s˜ao iguais

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c 
    | (a==b) && (b /= c) = 2
    | (b == c) && (a /= c) = 2
    | (a == c) && (c /= b) = 2
    | (a == b) && (b == c) = 3
    | otherwise = 0

-- Defina a fun¸c˜ao
--todosDiferentes :: Int -> Int -> Int -> Bool
--que retorna True se todos os seus argumentos s˜ao diferentes. Obs: m /= n
--retorna True se m e n s˜ao diferentes

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c) && (a /=c)


todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a == b) && (b == c) 


--O que est´a errado com a seguinte defini¸c˜ao de todosDiferentes:
--todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
-- Resposta: Isso permite que n e p sejam iguais pois não possui uma comparação entre eles, nada garante a diferença.

--Escreva uma defini¸c˜ao de quantosSaoIguais que use a fun¸c˜ao todosDiferentes
--e a fun¸c˜ao todosIguais

quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c 
    | todosDiferentes a b c = 0
    | todosIguais a b c = 3
    | otherwise = 2

-- Defina a fun¸c˜ao elevadoDois :: Int -> Int que recebe um argumento n
--e devolve como resposta n2

elevadoDois :: Int -> Int
elevadoDois x = x * x

-- Defina a fun¸c˜ao elevadoQuatro :: Int -> Int que recebe um argumento
--n e devolve como resposta n4. Use elevadoDois para definir elevadoQuatro

elevadoQuatro :: Int -> Int
elevadoQuatro x = x *  x * x * x

-- Supondo que exista uma fun¸c˜ao vendas:
--vendas :: Int -> Int
--que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
-- na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente
--uma fun¸c˜ao chamada vendaTotal, que recebe um argumento n e calcula
--todas as vendas da semana 0 at´e a semana n. Observe que essa fun¸c˜ao deve
--ser recursiva. Exemplo de calculo: As vendas da semana 0 at´e a semana 2,
--podem ser calculados usando a seguinte formula:
--vendas 0 + vendas 1 + vendas 2

vendas :: Int -> Int
vendas x
    | x == 0 = 10
    | x == 1 = 20
    | otherwise = 30
    -- arrumar essa função

vendaTotal :: Int -> Int
vendaTotal x
    | x < 0 = 0
    | otherwise = vendas x + vendaTotal(x - 1)