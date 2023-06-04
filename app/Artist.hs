module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace
import Test.QuickCheck

-- Problema 1

separa :: Comanda -> [Comanda]
--Casos base
separa (Gira a) = [Gira a]
separa (Avança a) = [Avança a]
separa (Para) = []
--Cas recursius
separa (a :#: b) = separa a ++ separa b


-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [c] = c :#: Para
ajunta (c:cs) = c :#: ajunta cs

--Versió sense Para pel problema 9
ajunta' :: [Comanda] -> Comanda
ajunta' [c] = c
ajunta' (c:cs) = c :#: ajunta' cs


-- Problema 3

-- prova feta amb prop_equivalent (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7) (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7)
--TRUE
prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent a b = (separa a) == (separa b)


--La primera funció prop separa ajunta ha de
--mirar que ajunta (separa c) es equivalent a c, on c es una comanda qualsevol.
--TEST: prop_split_join (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7)
--TRUE
prop_split_join :: Comanda -> Bool
prop_split_join a = prop_equivalent (ajunta(separa a)) a

--La segona funció prop separa ha de mirar que la llista retornada per la funció separa no contingui cap
--Para ni comanda (:#:) composta.
--TEST: prop_split (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7)
--TRUE
prop_split :: Comanda -> Bool
prop_split a = not (any isInvalid (separa a))
  where
    isInvalid Para       = True
    isInvalid (_ :#: _)  = True
    isInvalid _          = False



-- Per poder comprovar una propietat, farem us del modul QuickCheck. Per comprovar les propietats d’un tipus de dades es necesari definir aquest tipus com a part de la categoria Arbitrary.
-- La classe Arbitrary proporciona una manera de generar valors aleatoris d’un tipus determinat,
-- la qual cosa resulta ´util per als tests basats en propietats amb QuickCheck.
-- Per poder comparar les propietats feu us de la funci´o quickCheck, per exemple,
-- Test.QuickCheck> quickCheck (prop_equivalent Para Para)
-- +++ OK, passed 1 test.


-- Problema 4

copia :: Int -> Comanda -> Comanda
copia n c
  | n <= 1 = c
  | otherwise = c :#: copia (n-1) c


-- Problema 5

pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avança d :#: Gira 72)


-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon d n a = copia n (Avança d :#: Gira a)

prop_poligon_pentagon :: Distancia -> Bool
prop_poligon_pentagon d = prop_equivalent (poligon d 5 72) (pentagon d)


-- Problema 7

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral _ 0 _ _ = Para
espiral costat n pas angle = avança :#: gira :#: espiral novaDistancia (n-1) pas angle
  where
    avança = Avança costat
    gira = Gira angle
    novaDistancia = costat + pas


-- Problema 9

optimitza :: Comanda -> Comanda
optimitza comanda = if c == comanda
                    then c
                    else optimitza c
  where
    c = ajunta'(separa(optimitza' comanda))

optimitza' :: Comanda -> Comanda
optimitza' (Avança 0) = Para
optimitza' (Gira 0) = Para
optimitza' (c1 :#: c2) = optimitza'' (optimitza' c1) (optimitza' c2)
optimitza' c = c

optimitza'' :: Comanda -> Comanda -> Comanda
optimitza'' (Avança a1) (Avança a2) = Avança (a1 + a2)
optimitza'' (Gira g1) (Gira g2) = Gira (g1 + g2)
optimitza'' c1 c2 = c1 :#: c2


-- Problema 10

triangle :: Int -> Comanda
triangle n = triangleAux n

triangleAux :: Int -> Comanda
triangleAux 0 = (ColorLlapis blau) :#: Avança 1
triangleAux n = triangleAux (n - 1) :#: Gira 90 :#: triangleAux (n - 1) :#: Gira (270) :#: triangleAux (n - 1) :#: Gira (270) :#: triangleAux (n - 1) :#: Gira 90 :#: triangleAux (n - 1)


-- Problema 11

fulla :: Int -> Comanda
fulla n = fullaf n

fullaf :: Int -> Comanda
fullaf 0 = (ColorLlapis blau) :#: Avança 1 
fullaf n = fullag (n - 1) :#: Branca (Gira 315 :#: fullaf (n - 1)) :#: Branca (Gira 45 :#: fullaf (n - 1)) :#: Branca (fullag (n - 1) :#: fullaf (n - 1)) 

fullag :: Int -> Comanda
fullag 0 = (ColorLlapis vermell) :#: Avança 1
fullag n = fullag (n-1) :#: fullag (n-1)


-- Problema 12

hilbert :: Int -> Comanda
hilbert n = hilbertl n

hilbertl :: Int -> Comanda
hilbertl 0 = (ColorLlapis blau) :#: Avança 1
hilbertl n = Gira 90 :#: hilbertr (n-1) :#: hilbertf :#: Gira 270 :#: hilbertl (n-1) :#: hilbertf :#: hilbertl (n-1) :#: Gira 270 :#: hilbertf :#: hilbertr (n-1) :#:  Gira 90

hilbertr :: Int -> Comanda
hilbertr 0 = (ColorLlapis verd) :#: Avança 1 
hilbertr n = Gira 270 :#: hilbertl (n-1) :#: hilbertf :#: Gira 90 :#: hilbertr (n-1) :#: hilbertf :#: hilbertr (n-1) :#: Gira 90 :#: hilbertf :#: hilbertl (n-1) :#:  Gira 270

hilbertf :: Comanda
hilbertf =  (ColorLlapis vermell) :#: Avança 1 


-- Problema 13

fletxa :: Int -> Comanda
fletxa n = fletxaf n

fletxaf :: Int -> Comanda
fletxaf 0 = (ColorLlapis blau) :#: Avança 1 
fletxaf n = fletxag (n-1) :#: Gira 90 :#: fletxaf (n-1) :#: Gira 90 :#: fletxag (n-1)

fletxag :: Int -> Comanda
fletxag 0 = Avança 1 :#: (ColorLlapis vermell)
fletxag n = fletxaf (n-1) :#: Gira 270 :#: fletxag (n-1) :#: Gira 270 :#: fletxaf (n-1)


-- Problema 14

branca :: Int -> Comanda
branca n = brancag n

brancag :: Int -> Comanda
brancag 0 = (ColorLlapis blau) :#: Avança 1 
brancag n = brancaf (n-1) :#: Branca(Branca(brancag (n-1)) :#: Gira 90 :#: brancag (n-1)) :#: Gira 90 :#: brancaf (n-1) :#: Branca(Gira 90 :#: brancaf (n-1) :#: brancag (n-1)) :#: Gira 270 :#: brancag (n-1)

brancaf :: Int -> Comanda
brancaf 0 = (ColorLlapis vermell) :#: Avança 1 
brancaf n = brancaf (n-1) :#: brancaf (n-1)