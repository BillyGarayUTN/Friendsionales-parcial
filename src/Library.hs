module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


-- Resolvemos el parcial
billy = Persona{ nombre = "billy", edad = 32, nivelDeAlegria= 400, nivelDeAnsiedad= 300, tareas=[]}

-- Gerenamos Alias
type Alegronios = Number
type Nerviofrinas = Number

data Persona = Persona{
    nombre :: String,
    edad :: Number,
    nivelDeAlegria :: Alegronios,
    nivelDeAnsiedad :: Nerviofrinas,
    tareas :: [Persona->Persona]
} deriving (Show,Eq)

nivelDeEnergia :: Persona -> Number
nivelDeEnergia persona
    | nivelDeAlegria persona > nivelDeAnsiedad persona =(min 340 ).(*2).nivelDeAlegria $ persona 










