import Text.Show.Functions
import Data.List(genericLength)
import Data.List


-------------------------------Punto 1-------------------------------------------

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving(Show, Eq)
 
type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaMenorA10 auto1 auto2

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = all (leGanaYNoLoTieneCerca auto) carrera

leGanaYNoLoTieneCerca :: Auto -> Auto -> Bool
leGanaYNoLoTieneCerca auto1 auto2 = leGana auto1 auto2 && not (estaCerca auto1 auto2)

puesto :: Auto -> Carrera -> Int
puesto auto = (+1) . length . filter (flip leGana auto)

distanciaMenorA10 :: Auto -> Auto -> Bool
distanciaMenorA10 auto1= (10>) . abs . (distanciaRecorrida auto1-) . distanciaRecorrida

leGana :: Auto -> Auto -> Bool
leGana ganador perdedor = distanciaRecorrida ganador > distanciaRecorrida perdedor

---------------------------Punto 2----------------------------------------------

autoCorreDurante :: Int -> Auto -> Auto
autoCorreDurante tiempo auto = auto {distanciaRecorrida = distanciaRecorrida auto + tiempo * velocidad auto}

type Modificador = Int -> Int

alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificador auto = auto{velocidad = max 0 (modificador . velocidad $ auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidadABajar = alterarVelocidad (\v -> v - cantidadABajar)

---------------------------Punto 3----------------------------------------------

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto = alterarVelocidadSegun (estaCerca auto) 50

miguelitos :: Int -> PowerUp
miguelitos velocidadABajar auto= alterarVelocidadSegun (leGana auto) velocidadABajar

jetPack :: Int -> PowerUp
jetPack cantDeTiempo auto = afectarALosQueCumplen (== auto) (usarJetpack cantDeTiempo)

alterarVelocidadSegun :: (Auto -> Bool) -> Int -> Carrera -> Carrera
alterarVelocidadSegun criterio velocidad carrera = afectarALosQueCumplen criterio (bajarVelocidad velocidad) carrera

usarJetpack :: Int -> Auto -> Auto
usarJetpack tiempoJetPack =  alterarVelocidad (`div`2) . autoCorreDurante tiempoJetPack . alterarVelocidad (*2)

---------------------------Punto 4----------------------------------------------

type Evento = Carrera -> Carrera
type TablaDePosiciones = [(Int, String)]

simularCarrera :: Carrera -> [Evento] -> TablaDePosiciones
simularCarrera carrera = armarTabla . llegarAlEstadoFinalDeCarrera carrera

llegarAlEstadoFinalDeCarrera :: Carrera -> [Evento] -> TablaDePosiciones
llegarAlEstadoFinalDeCarrera carrera [eventos] = foldl (flip($))

armarTabla :: Carrera -> TablaDePosiciones
armarTabla carrera = map (puestosDeCarrera carrera) carrera

puestosDeCarrera :: Carrera -> Auto -> (Int, String)
puestosDeCarrera carrera auto = (puesto auto carrera, color auto)

correnTodos :: Int -> Evento 
correnTodos tiempoQueCorre = map (autoCorreDurante tiempoQueCorre)

usarPowerUp :: PowerUp -> String -> Evento
usarPowerUp powerUp color carrera = powerUp (encontrarAutoDeColor color carrera) carrera

encontrarAutoDeColor :: String -> Carrera -> Auto
encontrarAutoDeColor colorx carrera = head . filter ((==colorx) . color) carrera

---------------------------Ejemplos para practica-------------------------------

ejemplo = simularCarrera [(Auto "rojo" 120 0),(Auto "blanco" 120 0), (Auto "azul" 120 0), (Auto "negro" 120 0)]
    [correnTodos 30, usarPowerUp (jetPack 3) "azul", usarPowerUp terremoto "blanco", correnTodos 40, 
    usarPowerUp (miguelitos 20) "blanco", usarPowerUp (jetPack 6) "negro", correnTodos 10]