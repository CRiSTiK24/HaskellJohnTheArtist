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
copia = undefined

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon = undefined

-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon = undefined

prop_poligon_pentagon = undefined

-- Problema 7

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral = undefined

-- Problema 9

optimitza :: Comanda -> Comanda
optimitza = undefined

-- Problema 10

triangle :: Int -> Comanda
triangle = undefined

-- Problema 11

fulla :: Int -> Comanda
fulla = undefined

-- Problema 12

hilbert :: Int -> Comanda
hilbert = undefined

-- Problema 13

fletxa :: Int -> Comanda
fletxa = undefined

-- Problema 14

branca :: Int -> Comanda
branca = undefined
