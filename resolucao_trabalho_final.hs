--autor do codigo: Pedro Inácio de Carvalho Junior

potencia :: (Int,Float) -> Float
potencia (exp, base) =  
   if exp == 0 
   then 1.0 
   else base * potencia (exp-1,base)

pot :: Int -> Float -> Float
pot exp base =  
   if exp == 0 
   then 1.0 
   else base * pot (exp-1) base

-- a diferença entre as declarações de tipo acima se dá pelo uso da tupla na funcao potencia
--fazendo os parametros serem vistos como um valor
--e de setas na funcao pot sendo os parametros vistos como valores separados por um espaço de forma sequencial
--as consequencias de seu uso nao sao muito relevantes, mas o uso da tupla tem sido visto por melhorar
--a legibilidade e permitir o compilador em achar erros mais facilmente no codigo, alem de uma pequena
--melhora na performance dependendo do compilador

pot5 = pot 5
--pot5 esta fazendo uma funcao parcialmente aplicada onde o segundo parametro de pot 5 pode ser adicionado posteriormente
--potencia nao pode ser parcialmente aplicada pois ambos os parametros sempre sao recebidos como um so na tupla


--exemplo de aplicacao parcial das funcoes abaixo na secao main ao final do codigo
somaLista :: Num a => [a] -> [a]
somaLista ys = [ x+1 | x <-  ys]

aplicFuncLst :: (t1 -> t2) -> t1 -> [t2]
aplicFuncLst f xs = [f xs]


--fibonacci usando pattern matching
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--fibonacci usando if
fib2 :: Integer -> Integer
fib2 n = if n==0 then 0 else if n==1 then 1 else fib2 (n-1) + fib2 (n-2)

--os metodos de fibonacci anteriores apresentam problemas de desempenho, então uma solução é
--usar listas infinitas em que nenhum elemento é calculado até que seja necessario
--segue abaixo duas funções que juntas permitem esse metodo para calcular fibonacci: 
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
fib3 :: Int -> Integer
fib3 n = fibs !! n

--as proximas 3 funções estão com os tipos declarados
ehPrimo :: Integral a => a -> Bool
ehPrimo x = null [x' | x' <- [2..x-1], x `rem` x' == 0]

from :: Num t => t -> [t]
from n = n : from (n+1)

primPrimo :: Integral a => [a] -> a
primPrimo (n : ns) =
    if ehPrimo n then n else primPrimo ns

--funcao abaixo cria uma lista de pares infinitas a partir de valor n
listaPares :: Integral a => a -> [a]
listaPares a = [ x | x <- [a..], x `mod` 2 == 0]

--funcao abaixo encontra o primeiro valor multiplo de 25 em uma lista de pares infinita
multiplo25 :: Integral a => a -> a
multiplo25 n = head [x | x <- listaPares n, x `mod` 25 == 0]

--o conceito em haskell que permite a criacao de listas infinitas sem produzir um ciclo infinito de chamadas
--se da pela avaliacao preguiçosa em que a avaliacao de uma expressao é atrasada até que o resultado seja necessario
--e que quando se tiver o resultado ele é memorizado sem precisar ser recomputado

genericSort :: (t -> t -> Bool) -> [t] -> [t]
genericSort precede =
   let
      sort [] = []
      sort (n : ns) =
         sort [i | i <- ns, i `precede` n]
            ++ [n]
            ++ sort [i | i <- ns, not (i `precede` n)]
   in sort

ordenaInt :: [Int] -> [Int]
ordenaInt = genericSort (<)

--funcao abaixo dividi dois numeros inteiros e retorna um resultado float
divide :: Int -> Int -> Float
divide x y = ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

--funcao compara a divisao de dois inteiros (numerador e denominador) para saber se o racional e menor ou nao
menorRac :: [Int] -> [Int] -> Bool   
menorRac a b = if divide (head a) (head (tail a)) < divide (head b) (head (tail b)) then True else False

ordenaRac :: [ [Int] ] -> [ [Int] ]
ordenaRac = genericSort (menorRac)


--precedeAl
--nao foi necessario criar precedeAl pois o ordenaAl ja ordena com base nos inteiros
ordenaAl :: [ (Int, String, Float) ] -> [ (Int, String, Float) ]
ordenaAl = genericSort (<)


--funcao abaixo para pegar o terceiro elemento da tupla
getThird :: (a, b, c) -> c
getThird (_, _, x) = x

--funcao precedeAl2 pega os floats das tuplas (CRG) e os compara
precedeAl2 ::  (Int, String, Float)  ->  (Int, String, Float)  -> Bool
precedeAl2 c d = if getThird c < getThird d then True else False

ordenaAl2 ::  [(Int, String, Float)]  -> [(Int, String, Float)]
ordenaAl2 = genericSort (precedeAl2)


main :: IO ()
main = do
    print ("primeiro metodo de potencia")
    print (potencia(2,3))
    print ("segundo metodo de potencia")
    print (pot 2 3)
    
    print ("aplicacao de funcao parcial, soma de elementos dae uma lista com 1:")
    let ap = aplicFuncLst somaLista
    print (ap [1,2])

    print ("primeiro metodo de fibonacci:")
    print (fib 32)
    print ("segundo metodo de fibonacci:")
    print (fib2 32)
    print ("terceiro metodo de fibonacci com lista infinita:")
    print (fib3 (32-1))

    --print ("lista pares infinitos")
    --print (listaPares 5)
    print ("primeiro multiplo de 25 em lista de pares infinita a partir de valor n:")
    print (multiplo25 2)
    print ("ordenar inteiros, demonstracao do generic sort:")
    print (ordenaInt [2,1])

    print ("ordenar racionais em ordem ascendente:")
    print (ordenaRac [ [1,2], [1,3], [1,5], [9,10], [1,10] ])
    
    --nos exemplos a seguir adicionei um valor de matricula 7 para Bernardo e coloquei tuplas no lugar de listas
    --pois Bernardo não tinha numero de matricula no arquivo original e listas em haskell devem ser do mesmo tipo
    print ("ordenar por matricula:")
    print (ordenaAl [(3,"Leôncio", 9.3),(9,"Paulina", 9.8),(7,"Bernardo", 8.9)])
    print ("ordenar por crg:")
    print (ordenaAl2 [(3,"Leôncio", 9.3),(9,"Paulina", 9.8),(7,"Bernardo", 8.9)])