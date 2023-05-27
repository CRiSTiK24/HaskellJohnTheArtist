module UdGraphic (
    Comanda(..),
    Distancia,
    Angle,
    execute
    --executenot
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Para
               | Comanda :#: Comanda
                deriving (Eq)


-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

separa' :: Comanda -> [Comanda] --copio el separa
--Casos base
separa' (Gira a) = [Gira a]
separa' (Avança a) = [Avança a]
separa' (Para) = []
--Cas recursius
separa' (a :#: b) = separa' a ++ separa' b

-- Problema 9
-- Pas de comandes a lines a pintar per GL graphics
execute :: Comanda -> [Ln]
execute c = execute' (separa' c) 0 (Pnt 0 0)
  where
    execute' :: [Comanda] -> Angle -> Pnt -> [Ln]
    execute' [] _ _ = []
    execute' (Avança d : cs) angle (Pnt x y) = Ln negre origin end : execute' cs angle end
      where
        origin = Pnt x y
        end = calcularDesti (Pnt x y) d angle
    execute' (Gira a : cs) angle (Pnt x y) = execute' cs angleFinal (Pnt x y)
      where
        angleFinal = angle - a -- per algun motiu, als exemples de l'enunciat es fan els girs en sentit horari, en comptes de sentit anhorari com diu al pricipi 
    execute' (Para : cs) angle (Pnt x y) = execute' cs angle (Pnt x y)

    calcularDesti :: Pnt -> Distancia -> Angle -> Pnt
    calcularDesti (Pnt x1 y1) distancia angle =
      let angleRad = angle * pi / 180
          newX = x1 + distancia * cos angleRad
          newY = y1 + distancia * sin angleRad
      in Pnt newX newY
    --Si la comanda es avança, llavors avançem la distancia en el angle indicat
    --Si la comanda es gira, llavors girem el angle indicat, per tal de que si es torna a avançar, es tingui en compte
    --Si la comanda es para, llavors no fem res
    --Si la comanda es :#: llavors executem les dues comandes. Es important de que si la primera cambia l'angle, la segona comanda utilitzi el angle de la primera
    
-- executenot :: [Comanda] -> Angle -> Pnt -> [Ln]
-- executenot [] _ _ = []
-- executenot (Avança d : cs) angle (Pnt x y) =
--   Ln negre origin end : executenot cs angle end
--   where
--     origin = Pnt x y
--     end = calcularDesti (Pnt x y) d angle
-- executenot (Gira a : cs) angle (Pnt x y) = executenot cs (angle + a) (Pnt x y)
-- executenot (Para : cs) angle (Pnt x y) = executenot cs angle (Pnt x y)

-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))


-- Tests

instance Show Comanda where
  show (Avança d) = "Avança " ++ show d
  show (Gira a) = "Gira " ++ show a
  show Para = "Para"
  show (c1 :#: c2) = show c1 ++ " :#: " ++ show c2
