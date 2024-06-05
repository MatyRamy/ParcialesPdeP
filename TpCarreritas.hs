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

 
---------------------------Ejemplos para practica-------------------------------

suran :: Auto
suran = Auto {
    color = "rojo",
    velocidad = 100,
    distanciaRecorrida = 119
}

etios :: Auto
etios = Auto {
    color = "azul",
    velocidad = 130,
    distanciaRecorrida = 110
}

civic :: Auto
civic = Auto {
    color = "violeta",
    velocidad = 150,
    distanciaRecorrida = 890
}

carrera1 :: Carrera
carrera1 = [etios, suran, civic]