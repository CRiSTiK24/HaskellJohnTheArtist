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
