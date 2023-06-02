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

ansiedadMayorA50 :: Persona -> Bool
ansiedadMayorA50 persona = (50<).nivelDeAnsiedad $ persona 

nivelDeEnergiaPar :: Persona -> Bool
nivelDeEnergiaPar persona = even.nivelDeEnergia $ persona

--Se aplicó point free
losMasCriticados :: (Persona->Bool) -> [Persona] ->  [String]   
losMasCriticados criterio  = map nombre.(take 2).filter criterio 

-- punto 3

-- punto en comun realiza luego de hacer la tarea
baja10NerviofrinasMin0 :: Number -> Number
baja10NerviofrinasMin0 unNumero = (max 0).( 10 `subtract` ) $ unNumero

--test de codearUnProyectoNuevo
juan = Persona{nombre="juan", edad=10, nivelDeAlegria=100, nivelDeAnsiedad=100,tareas= [codearUnProyectoNuevo] }

codearUnProyectoNuevo :: Persona->Persona
codearUnProyectoNuevo persona = persona{nivelDeAlegria = nivelDeAlegria persona+110,nivelDeAnsiedad = baja10NerviofrinasMin0.(+50).nivelDeAnsiedad $ persona }


--test de hacerTramitesEnAfip
cecilia = Persona{nombre="cecilia", edad=10, nivelDeAlegria=40, nivelDeAnsiedad=40,tareas= [hacerTramitesEnAfip 2] }
juan' = Persona{nombre="juan'", edad=10, nivelDeAlegria=100, nivelDeAnsiedad=250,tareas= [hacerTramitesEnAfip 2] }

hacerTramitesEnAfip :: Number -> Persona -> Persona
hacerTramitesEnAfip cantidadDeTramites persona = persona{ nivelDeAnsiedad = baja10NerviofrinasMin0.(max 300).(*cantidadDeTramites).nivelDeAnsiedad $ persona }


--test de andarEnBici
cecilia'= Persona{nombre="cecilia'", edad=10, nivelDeAlegria=90, nivelDeAnsiedad=65,tareas= [andarEnBici 1] }

andarEnBici :: Number -> Persona -> Persona
andarEnBici km persona = persona{ nivelDeAnsiedad = 0,nivelDeAlegria =((50*km)+).nivelDeAlegria $ persona}


--test de escucharMusica
santiago = Persona{nombre="santiago", edad=10, nivelDeAlegria=40, nivelDeAnsiedad=30,tareas= [hacerTramitesEnAfip 2] }

escucharMusica :: Persona -> Persona
escucharMusica persona = persona{nivelDeAnsiedad = baja10NerviofrinasMin0.(10 `subtract` ).nivelDeAnsiedad $ persona}

