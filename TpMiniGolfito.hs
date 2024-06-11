import Text.Show.Functions
import Data.List(genericLength)
import Data.List

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-----------------------Punto 1--------------------------------


type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (precisionJugador habilidad * 2) 0

madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5

hierros :: Int -> Palo
hierros numero habilidad = UnTiro (fuerzaJugador habilidad * numero) (precisionJugador habilidad  `div` numero) (max (numero-3) 0)

todosLosPalos :: [Palo]
todosLosPalos = [putter, madera] ++ map hierros [1..10]

-----------------------Punto 2---------------------------------------

golpe :: Palo -> Jugador -> Tiro
golpe palo jugador = palo . habilidad $ jugador

-----------------------Punto 3---------------------------------

data Obstaculo = Obstaculo{
  puedeSuperarlo :: Tiro -> Bool,
  efectoTrasSuperarlo :: Tiro -> Tiro
}

tunel :: Obstaculo
tunel = Obstaculo puedeSuperarTunel superoTunel
puedeSuperarTunel :: Tiro -> Bool
puedeSuperarTunel tiro = (&&) ((> 90) . precision $ tiro) ((== 0) . altura $ tiro)
superoTunel :: Tiro -> Tiro
superoTunel tiro = tiro {velocidad = (3*) . velocidad $ tiro, precision = 100}




--------------------------Pruebas-----------------------------------
tiro1 :: Tiro
tiro1 = UnTiro { precision = 95, altura = 0, velocidad = 50 }