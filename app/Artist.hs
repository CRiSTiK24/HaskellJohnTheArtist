module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1

separa :: Comanda -> [Comanda]
--Casos base
separa (Gira a) = [Gira a]
separa (Avança a) = [Avança a]
--separa Para a = []
--Cas recursius
separa (a :#: b) = separa a ++ separa b


-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [c] = c
ajunta (c:cs) = c :#: ajunta cs


-- Problema 3

prop_equivalent = undefined

prop_split_join = undefined

prop_split = undefined

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
