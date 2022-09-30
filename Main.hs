-- Ingrid Lima dos Santos
{- q1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.  -}
divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1 .. n], n `mod` x == 0]

{- q2. Usando List Comprehension escreva uma função, chamada  contaCaractere, que conte a ocorrência de um caractere específico, em uma string dada. -}
contaCaractere :: Char -> String -> Int
contaCaractere x xs = length [x' | x' <- xs, x == x']

{- q3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.  -}
dobraNaoNegativo :: [Int] -> [Int]
dobraNaoNegativo n = [x * 2 | x <- n, x >= 0]

{- q4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.  -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], (a ^ 2) + (b ^ 2) == (c ^ 2)]

{- q5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.  -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1 .. n -1], n `mod` x == 0]

{- q6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.  -}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [x * y | (x, y) <- zip xs ys]

{- q7. Usando  List Comprehension escreva uma função, chamada  primeirosPrimos, que devolva uma lista contendo os n primeiros números primos a partir do número 2. -}
primeirosPrimos :: Int -> [Int]
primeirosPrimos n = takeWhile (\x -> length [y | y <- [2 .. (x -1)], length (divisoresden y) == 2] < n) [x | x <- [2 ..], length (divisoresden x) == 2]

{- q8. Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva uma lista de par ordenados contendo uma potência de 2 e uma potência de 3  até um determinado número dado. Observe que estes números podem ser bem grandes. -}
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados n = [(2 ^ x, 3 ^ x) | x <- [0 .. n]]

main = do
  putStrLn $ "\nFunc. 1: entrada: 10; resultado: " ++ show (divisoresden 10)
  putStrLn $ "\nFunc. 2: entrada: 'e' 'Teste'; resultado: " ++ show (contaCaractere 'e' "Teste")
  putStrLn $ "\nFunc. 3: entrada: [-2, -4, 2, 3, -6, 10]; resultado: " ++ show (dobraNaoNegativo [-2, -4, 2, 3, -6, 10])
  putStrLn $ "\nFunc. 4: entrada: 5; resultado: " ++ show (pitagoras 5)
  putStrLn $ "\nFunc. 5: entrada: 6; resultado: " ++ show (numerosPerfeitos 6)
  putStrLn $ "\nFunc. 6: entrada: [1, 2, 3] [4, 5, 6]; resultado: " ++ show (produtoEscalar [1, 2, 3] [4, 5, 6])
  putStrLn $ "\nFunc. 7: entrada: 10; resultado: " ++ show (primeirosPrimos 10)
  putStrLn $ "\nFunc. 8: entrada: 5; resultado: " ++ show (paresOrdenados 5)
