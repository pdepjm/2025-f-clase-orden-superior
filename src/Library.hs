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
    vida :: Number,
    habilidades :: [Habilidad]
    }  deriving Show

data Rango = Aprendiz | Padawan | Knight | Master | GrandMaster deriving (Show, Ord, Eq)


anakin :: Jedi
anakin = UnJedi{rango = Aprendiz, midichlorians = 1000000, maestros = [obiwan, quiGon], vida = 100, habilidades = []}

obiwan :: Jedi
obiwan = UnJedi{rango = Padawan, midichlorians = 5000, maestros = [quiGon], vida = 100, habilidades = [ataqueBasico]}

maceWindu :: Jedi
maceWindu = UnJedi{rango = Knight, midichlorians = 6000, maestros = [yaddle], vida = 100, habilidades = [ataqueBasico]}

quiGon :: Jedi
quiGon = UnJedi{rango = Master, midichlorians = 8000, maestros = [yoda, yaddle], vida = 100, habilidades = [ataqueBasico, sanar]}

yoda :: Jedi
yoda = UnJedi{rango = GrandMaster, midichlorians = 100000, maestros = [], vida = 100, habilidades = [sanar, trueno, ataqueBasico]}

yaddle :: Jedi
yaddle = UnJedi{rango = Master, midichlorians = 10000, maestros = [], vida = 100, habilidades = [sanar]}


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
potencialTotal batallon = sum (map potencialDeCombate batallon)


-- Lo que vimos en clase de la parte 2
type Habilidad = Jedi -> Jedi

modificarPorcentualVida :: Number -> Jedi -> Jedi
modificarPorcentualVida porcentaje jedi = jedi{vida = vida jedi * porcentaje}

reducirMidichlorians :: Number -> Jedi -> Jedi
reducirMidichlorians reduccion jedi = jedi{midichlorians = midichlorians jedi - reduccion}

ataqueBasico :: Habilidad
ataqueBasico jedi = modificarPorcentualVida 0.9 jedi

trueno :: Habilidad
trueno jedi = reducirMidichlorians 100 (modificarPorcentualVida 0.5 jedi)

sanar :: Habilidad
sanar jedi = modificarPorcentualVida 1.25 jedi

darPiruetas :: Number -> Habilidad
darPiruetas cantidadDePiruetas = reducirMidichlorians (cantidadDePiruetas * 50) jedi

darPiruetasSkywalker :: Habilidad
darPiruetasSkywalker jedi = darPiruetas 10 jedi

elDeMayorMidichlorians :: Jedi -> Jedi -> Jedi
elDeMayorMidichlorians jedi1 jedi2 = elMayorSegun midichlorians jedi1 jedi2

elDeMayorPotencial :: Jedi -> Jedi -> Jedi
elDeMayorPotencial jedi1 jedi2 = elMayorSegun potencialDeCombate jedi1 jedi2

elMayorSegun :: (Jedi -> Number) -> Jedi -> Jedi -> Jedi
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


robotEntrenamiento :: Jedi
robotEntrenamiento = UnJedi{rango = Aprendiz, vida = 1000, maestros = [], habilidades = [], midichlorians = 0}

-- Esta funcion la vimos al final para ver como aplicar una funcion que se pasa por parametro
habilidadCumple :: (Habilidad -> Bool) -> Habilidad -> Bool
habilidadCumple condicion habilidad = condicion habilidad
