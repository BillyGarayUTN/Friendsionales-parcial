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


--Punto 1

nivelDeEnergia :: Persona -> Number
nivelDeEnergia persona
    | nivelDeAlegria persona > nivelDeAnsiedad persona =(min 340 ).(*2).nivelDeAlegria $ persona 
    | (esJoven persona) && (nivelDeAnsiedad persona > nivelDeAlegria persona) = (300-).nivelDeEstres $ persona
    | otherwise = (10+).nivelDeAlegria $ persona 

nivelDeEstres :: Persona -> Number
nivelDeEstres persona
    | (5<).length.tareas $ persona   = nivelDeAnsiedad persona * 1.5
    | otherwise = nivelDeAnsiedad persona

esJoven :: Persona -> Bool
esJoven persona = (<40).edad $ persona

-- Punto 2
viejoSonLosTrapos :: [Persona] -> Bool
viejoSonLosTrapos grupoPersonas = all (esVital) grupoPersonas

esVital :: Persona -> Bool
esVital persona =  ((not.esJoven) persona) && (((>100).nivelDeEnergia) persona )

nivelTotalDeAnsiedad :: [Persona] -> Number
nivelTotalDeAnsiedad grupoPersonas = sumOf nivelDeAnsiedad grupoPersonas


-- ansiedadMayora50 persona = (50<).nivelDeAnsiedad persona 


losMasCriticados :: (Persona->Bool) ->  
losMasCriticados criterio 



