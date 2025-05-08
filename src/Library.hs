module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jedi = UnJedi{
    midichlorians :: Number,
    maestros :: [Jedi],
    rango :: Rango
    }  deriving Show

data Rango = Aprendiz | Padawan | Knight | Master | GrandMaster deriving (Show, Ord, Eq)


anakin :: Jedi
anakin = UnJedi{rango = Aprendiz, midichlorians = 1000000, maestros = [obiwan, quiGon]}

obiwan :: Jedi
obiwan = UnJedi{rango = Padawan, midichlorians = 5000, maestros = [quiGon]}

maceWindu :: Jedi
maceWindu = UnJedi{rango = Knight, midichlorians = 6000, maestros = [yaddle]}

quiGon :: Jedi
quiGon = UnJedi{rango = Master, midichlorians = 8000, maestros = [yoda, yaddle]}

yoda :: Jedi
yoda = UnJedi{rango = GrandMaster, midichlorians = 100000, maestros = []}

yaddle :: Jedi
yaddle = UnJedi{rango = Master, midichlorians = 10000, maestros = []}


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