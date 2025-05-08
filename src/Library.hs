module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jedi = UnJedi{
    midichlorians :: Number,
    maestros :: [Jedi],
    rango :: Rango
    } deriving Show

data Rango = Aprendiz | Padawan | Knight | Master | GrandMaster deriving Show


anakin :: Jedi
anakin = UnJedi{rango = Aprendiz, midichlorians = 1000000, maestros = [obiwan, quiGon]}

obiwan :: Jedi
obiwan = UnJedi{rango = Padawan, midichlorians = 5000, maestros = [quiGon]}

maceWindu :: Jedi
maceWindu = UnJedi{rango = Knight, midichlorians = 6000, maestros = [yaddle]}

quiGon :: Jedi
quiGon = UnJedi{rango = Master, midichlorians = 8000, maestros = [yoda, yaddle]}

yoda :: Jedi
yoda = UnJedi{rango = GrandMaster, midichlorians = 10000, maestros = []}

yaddle :: Jedi
yaddle = UnJedi{rango = Master, midichlorians = 10000, maestros = []}

