--Para os próximos exercícios, use a seguinte definição de Arvore:
data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq,Show)

arv1 :: Arvore
arv1 = Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6))(Folha 9)

-- 1) implementar a função que recebe um inteiro e uma árvore, e multiplica todos os valores contidos na árvore pelo inteiro

multArvore:: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (n * x)
multArvore x (Nodo n a1 a2) = Nodo (x*n) (multArvore x a1) (multArvore x a2)

--2) Implemente a função que recebe uma árvore e conta quantas folhas existem nessa árvore
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = contaFolhas a1 + contaFolhas a2

--3)Implemente a função que conta quantos Nodos uma árvore possui
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) = 1 + contaNodos a1 + contaNodos a2

-- 4) Implemente a função que recebe um inteiro e uma árvore, e conta quantas vezes
-- esse inteiro aparece na árvore
quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha n)
    | n == x = 1
    | otherwise = 0
quantasVezes x (Nodo n a1 a2)
    | n == x = 1 + (quantasVezes x a1) + (quantasVezes x a2)
    | otherwise =  (quantasVezes x a1) + (quantasVezes x a2)

-- 5) A função max do Haskell, recebe dois inteiros e devolve o maior entre eles
--Usando a função max implemente a funçãoque encontra o maior inteiro em uma árvore
maxArvore :: Arvore -> Int
maxArvore (Folha n) =  n 
maxArvore (Nodo n a1 a2) = max n (max (maxArvore a1) (maxArvore a2))

-- Uma árvore refletida, é uma árvore com todos os seus ramos esquerdos e direitos trocados.
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

--7)Implementar a função que transforma uma árvore em uma lista de inteiros. Não importa a ordem
--dos inteiros na lista, apenas que todos os inteiros dos Nodos e Folhas estejam na lista resultante
geraLista :: Arvore -> [Int]
geraLista (Folha n) = [n]
geraLista (Nodo n a1 a2) = [n] ++ geraLista a1 ++ geraLista a2
