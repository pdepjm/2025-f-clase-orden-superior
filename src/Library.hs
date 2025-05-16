{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jedi = UnJedi{
    midichlorians :: Number,
    maestros :: [Jedi],
    rango :: Rango,
    resistencia :: Number,
    habilidades :: [Habilidad]
    }  deriving Show

data Rango = Aprendiz | Padawan | Knight | Master | GrandMaster deriving (Show, Ord, Eq)


anakin :: Jedi
anakin = UnJedi{rango = Aprendiz, midichlorians = 1000000, maestros = [obiwan, quiGon], resistencia = 100, habilidades = [darPiruetasSkywalker]}

obiwan :: Jedi
obiwan = UnJedi{rango = Padawan, midichlorians = 5000, maestros = [quiGon], resistencia = 100, habilidades = [ataqueBasico]}

maceWindu :: Jedi
maceWindu = UnJedi{rango = Knight, midichlorians = 6000, maestros = [yaddle], resistencia = 100, habilidades =[]}

quiGon :: Jedi
quiGon = UnJedi{rango = Master, midichlorians = 8000, maestros = [yoda, yaddle], resistencia = 100, habilidades =[]}

yoda :: Jedi
yoda = UnJedi{rango = GrandMaster, midichlorians = 100000, maestros = [], resistencia = 100, habilidades = [sanar, trueno, ataqueBasico]}

yaddle :: Jedi
yaddle = UnJedi{rango = Master, midichlorians = 10000, maestros = [], resistencia = 100, habilidades =[]}


potencialDeCombate :: Jedi -> Number
potencialDeCombate jedi = factorDeExperiencia (rango jedi) * (0.05 * midichlorians jedi)

factorDeExperiencia :: Rango -> Number
factorDeExperiencia Aprendiz = 0
factorDeExperiencia Padawan = 0.5
factorDeExperiencia Knight = 1
factorDeExperiencia Master = 2
factorDeExperiencia GrandMaster = 2.5

estaPreparado :: Jedi -> Bool
estaPreparado jedi = potencialDeCombate jedi > 5000 && tieneExperiencia jedi

tieneExperiencia :: Jedi -> Bool
tieneExperiencia jedi = rango jedi > Padawan

meditar :: Jedi -> Jedi
meditar jedi = jedi{midichlorians = midichlorians jedi * 1.10}

type Batallon = [Jedi]

caminanteEstrellas :: Batallon
caminanteEstrellas = [anakin, obiwan, quiGon]

primeroEnFila :: Batallon -> Jedi
primeroEnFila batallon = head batallon

terceroEnFila :: Batallon -> Jedi
terceroEnFila batallon = batallon !! 2

combinar :: Batallon -> Batallon -> Batallon
combinar batallon1 batallon2 = batallon1 ++ batallon2

sumar :: Batallon -> Jedi -> Batallon
sumar batallon jedi = jedi : batallon

sumar' :: Batallon -> Jedi -> Batallon
sumar' batallon jedi = [jedi] ++ batallon

estaListo :: Batallon -> Bool
estaListo batallon = all estaPreparado batallon

seLaBanca :: Batallon -> Bool
seLaBanca batallon = any tieneExperiencia batallon

losExperimentados :: Batallon -> [Jedi]
losExperimentados batallon = filter tieneExperiencia batallon

meditacionGrupal :: Batallon -> Batallon
meditacionGrupal batallon = map meditar batallon

potencialTotal :: Batallon -> Number
potencialTotal batallon = sum(map potencialDeCombate batallon)

-- Segunda clase

-- Parte 1

tieneBuenEntrenamiento :: Jedi -> Bool
tieneBuenEntrenamiento jedi = any tieneExperiencia (maestros jedi)

estaBienAcompaniado :: Jedi -> Bool
estaBienAcompaniado jedi = all estaPreparado (maestros jedi)

buenosMaestrosDe :: Jedi -> [Jedi]
buenosMaestrosDe jedi = filter tieneExperiencia (maestros jedi)

rangoMaximoDeMaestros :: Jedi -> Rango
rangoMaximoDeMaestros jedi = rangoMaximoEntre (maestros jedi)

rangoMaximoEntre :: [Jedi] -> Rango
rangoMaximoEntre jedis = maximum (map rango jedis)

-- Parte 2

type Habilidad = Jedi -> Jedi

modificarPorcentualResistencia :: Number -> Jedi -> Jedi
modificarPorcentualResistencia porcentaje jedi = jedi{resistencia = resistencia jedi * porcentaje}

reducirMidichlorians :: Number -> Jedi -> Jedi
reducirMidichlorians reduccion jedi = jedi{midichlorians = midichlorians jedi - reduccion}

ataqueBasico :: Habilidad
ataqueBasico jedi = modificarPorcentualResistencia 0.9 jedi

trueno :: Habilidad
trueno jedi = reducirMidichlorians 100 (modificarPorcentualResistencia 0.5 jedi)

sanar :: Habilidad
sanar jedi = modificarPorcentualResistencia 1.25 jedi

darPiruetas :: Number -> Habilidad
darPiruetas cantidadDePiruetas = reducirMidichlorians (cantidadDePiruetas * 50)

darPiruetasSkywalker :: Habilidad
darPiruetasSkywalker jedi = darPiruetas 10 jedi

-- Parte 3


robotEntrenamiento :: Jedi
robotEntrenamiento = UnJedi{rango = Aprendiz, resistencia = 1000, maestros = [], habilidades = [], midichlorians = 0}

esPeligrosa :: Habilidad -> Bool
esPeligrosa habilidad = resistenciaQueLeSacaAlRobot habilidad > 100

esCuradora :: Habilidad -> Bool
esCuradora habilidad = resistenciaQueLeCuraAlRobot habilidad > 10

resistenciaQueLeSacaAlRobot :: Habilidad -> Number
resistenciaQueLeSacaAlRobot habilidad = resistencia (habilidad robotEntrenamiento) - resistencia robotEntrenamiento 

resistenciaQueLeCuraAlRobot :: Habilidad -> Number
resistenciaQueLeCuraAlRobot habilidad = - resistenciaQueLeSacaAlRobot habilidad

esAgresivo :: Jedi -> Bool
esAgresivo jedi = all esPeligrosa (habilidades jedi)

esBalanceado :: Jedi -> Bool
esBalanceado jedi = cantidadHabilidadesPeligrosas jedi == cantidadHabilidadesCuradoras jedi

cantidadHabilidadesPeligrosas :: Jedi -> Number
cantidadHabilidadesPeligrosas jedi = cantidadHabilidadesQueCumplen esPeligrosa jedi

cantidadHabilidadesCuradoras :: Jedi -> Number
cantidadHabilidadesCuradoras jedi = cantidadHabilidadesQueCumplen esCuradora jedi

cantidadHabilidadesQueCumplen :: (Habilidad -> Bool) -> Jedi -> Number
cantidadHabilidadesQueCumplen condicion jedi = length (filter condicion (habilidades jedi))

-- Parte 4

type Criterio = Jedi -> Number

elDeMayorMidichlorians :: Jedi -> Jedi -> Jedi
elDeMayorMidichlorians jedi1 jedi2 = elMayorSegun midichlorians jedi1 jedi2

elDeMayorPotencial :: Jedi -> Jedi -> Jedi
elDeMayorPotencial jedi1 jedi2 = elMayorSegun potencialDeCombate jedi1 jedi2

elQueTieneMasMaestros :: Jedi -> Jedi -> Jedi
elQueTieneMasMaestros jedi1 jedi2 = elMayorSegun cantidadMaestros jedi1 jedi2

cantidadMaestros :: Jedi -> Number
cantidadMaestros jedi = length (maestros jedi)

--Esta funcion no está en el enunciado, pero sirve para mostrar una comparacion mediante una funcion que no devuelve Number (rango):
elDeMayorRango :: Jedi -> Jedi -> Jedi
elDeMayorRango jedi1 jedi2 = elMayorSegun rango jedi1 jedi2


elMayorSegun :: Ord a => (Jedi -> a) -> Jedi -> Jedi -> Jedi
elMayorSegun criterio jedi1 jedi2 
    | criterio jedi1 > criterio jedi2 = jedi1
    | otherwise = jedi2


entrenar :: (Number -> Number) -> Jedi -> Jedi
entrenar tipoEntrenamiento jedi = jedi{midichlorians = tipoEntrenamiento (midichlorians jedi)}   

entrenamientoTranqui :: Jedi -> Jedi
entrenamientoTranqui jedi = entrenar sumar100 jedi

sumar100 :: Number -> Number
sumar100 n = n + 100 

entrenamientoIntenso :: Jedi -> Jedi
entrenamientoIntenso jedi = entrenar doble jedi

id' :: a -> a
id' x = x

entrenamientoVago :: Jedi -> Jedi
entrenamientoVago jedi = entrenar id' jedi

--Tambien seria valido hacer esto, la forma de arriba es para practicar orden superior:
entrenamientoVago' :: Jedi -> Jedi
entrenamientoVago' jedi = jedi

malEntrenamiento :: Jedi -> Jedi
malEntrenamiento jedi = entrenar restar50 jedi

restar50 :: Number -> Number
restar50 n = n - 50

