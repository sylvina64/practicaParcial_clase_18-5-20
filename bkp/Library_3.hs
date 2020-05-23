module Library
    (Auto (..) ,
    anio, esPatenteNueva, esPatenteVieja, calculoPatental, primerasDosLetras, arribaDeDJ, abajoDeNB,
    autoPeligroso, tecnicosQueDejanElAutoEnCondiciones, cantidadDeTecnicosQueDejanElAutoEnCondiciones,
    estaEntreDjNb, costoDeReparacion, necesitaRevision, cambioRuedasDelanteras,
    fiatRojo, fiatAzul, fiatVerde, upRojo,
    autoPunto4_1,autoPunto4_2,autoPunto4_3,autoPunto4_4,autoPunto4_5 ,
    autosInfinitos,
    ordenadosToc, aplicarOrdenDeReparacion,costoReparacionDeTresAutos,costoReparacionListaDeAutos,
    autoEnCondiciones,
    alfa,bravo,charly,tango,zulu,lima
    ) where

import PdePreludat

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
    patente :: Patente,
    desgasteLlantas :: [Desgaste],
    rpm :: Int,
    temperaturaAgua :: Int,
    ultimoArreglo :: Fecha
} deriving Show

-- ==========================================================================================================================================
-- PUNTO 1: Costo de la reparación de un auto
-- ==========================================================================================================================================
-- (común)

-- Saber el costo de reparación de un auto
-- ●	si la patente tiene 7 dígitos, es $ 12.500
-- ●	si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
--      ○	que es $ 3.000 * la longitud para las patentes que terminen en 4
--      ○	o $ 20.000 para el resto de las patentes
-- ●	de lo contrario, se le cobra $ 15000

-- Importante: tenés que usar composición en las funciones auxiliares

esPatenteNueva :: Patente -> Bool
esPatenteNueva = (== 7).length 

esPatenteVieja :: Patente -> Bool
esPatenteVieja = (== 6).length

calculoPatental :: Patente -> Int
calculoPatental patente | ((== '4').last) patente = 18000 --((* 3000).length) patente, length es siempre 6 acá
                        | otherwise = 20000

primerasDosLetras :: Patente -> String
primerasDosLetras = take 2 

arribaDeDJ :: Patente -> Bool
arribaDeDJ = (>= "DJ").primerasDosLetras

abajoDeNB :: Patente -> Bool
abajoDeNB = (<= "NB").primerasDosLetras

estaEntreDjNb :: Patente -> Bool
estaEntreDjNb patente =  arribaDeDJ patente  && abajoDeNB patente 

costoDeReparacion :: Auto -> Int
costoDeReparacion auto | (esPatenteNueva.patente) auto = 12500
                       | ((esPatenteVieja.patente) auto && (estaEntreDjNb.patente) auto) = (calculoPatental.patente) auto
                       | otherwise = 15000

-- ==========================================================================================================================================
-- PUNTO 2: Auto Peligroso / Necesita revision
-- ==========================================================================================================================================
-- ATENCIÓN: Resolver únicamente con Composición y aplicación parcial
-- Parte 1) Auto peligroso (Pablo)
-- Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de la primera llanta es mayor a 0.5

autoPeligroso :: Auto -> Bool
autoPeligroso = (> 0.5).head.desgasteLlantas

-- Parte 2) Necesita revisión (Sylvina)
-- Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último arreglo fue realizado en el año 2015 ó antes.

necesitaRevision :: Auto -> Bool
necesitaRevision = (<= 2015).anio.ultimoArreglo

-- ==========================================================================================================================================
-- PUNTO 3: Personal técnico encargado de las reparaciones
-- ==========================================================================================================================================

type Mecanico = Auto -> Auto

autoPasaPorMecanicos :: [Mecanico] ->Auto -> Auto 
autoPasaPorMecanicos listaMecanicos auto = foldr ($) auto listaMecanicos

-- Parte 1) (Pablo)
-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico:
-- ●	Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas, en cuyo caso lo deja como está
-- ●	Bravo: cambia todas las cubiertas, dejándolas sin desgaste
-- ●	Charly:  realiza las mismas actividades que Alfa y Bravo

alfa :: Mecanico
alfa auto | ((>2000).rpm) auto = auto {rpm = 2000}
          | otherwise = auto

bravo :: Mecanico
bravo auto = auto {desgasteLlantas = map (*0) (desgasteLlantas auto)}

charly :: Mecanico
charly = autoPasaPorMecanicos [alfa, bravo]

-- Parte 2) (Sylvina)
-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico
-- ●	Tango: le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
-- ●	Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
-- ●	Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las posteriores quedan igual

tango :: Mecanico
tango auto = auto

cambiaTempAgua :: Int -> Auto -> Auto  -- no usé el tipo Mecanico para que sea más expresivo lo que hace
cambiaTempAgua temperatura auto = auto { temperaturaAgua = temperatura }

zulu :: Mecanico
zulu =  autoPasaPorMecanicos [cambiaTempAgua 90, lima]


lima :: Mecanico
lima auto = auto {desgasteLlantas = (cambioRuedasDelanteras.desgasteLlantas) auto }

cambioRuedasDelanteras :: [Desgaste] -> [Desgaste]
cambioRuedasDelanteras (x:y:cola) = [0.0,0.0] ++ cola

-- ==========================================================================================================================================
-- PUNTO 4: Ordenamiento TOC de autos
-- ==========================================================================================================================================

-- (Común para ambos integrantes) 
-- Solamente se puede utilizar recursividad en este punto

-- Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
-- ●	los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
-- ●	los autos ubicados en la posición par deben tener una cantidad de desgaste par
-- ●	asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.

-- NOTA: en esta función auxiliar no usamos "sum" porque interpretamos que no se podía, ya que debíamos resolverlo "solo con recursividad"

desgasteTotalCubiertas :: [Desgaste] -> Desgaste
desgasteTotalCubiertas [] = 0
desgasteTotalCubiertas (desgate:desgastes) = desgate + desgasteTotalCubiertas desgastes

desgaste :: [Desgaste] -> Int
desgaste = round.(*10).desgasteTotalCubiertas

desgasteAuto :: Auto -> Int
desgasteAuto auto = desgaste (desgasteLlantas auto)

ordenadosToc :: [Auto] -> Bool
ordenadosToc [] = True
ordenadosToc [autoImpar] = (desgasteImpar.desgasteAuto) autoImpar 
ordenadosToc (autoImpar:autoPar:restoDeLosAutos) = (desgasteImpar.desgasteAuto) autoImpar && (desgastePar.desgasteAuto) autoPar && ordenadosToc restoDeLosAutos

-- funciones agregadas para darle más expresividad al código

desgasteImpar = odd
desgastePar = even

-- ==========================================================================================================================================
-- PUNTO 5: Orden de Reparación
-- ==========================================================================================================================================

-- (Común para ambos integrantes) 
-- Aplicar una orden de reparación, que tiene
-- ●	una fecha
-- ●	una lista de técnicos
-- y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto, al que además hay que actualizarle la última
-- fecha de reparación.

cambiarFecha :: Fecha -> Auto -> Auto
cambiarFecha fecha auto = auto {ultimoArreglo = fecha}

aplicarOrdenDeReparacion :: Fecha -> [Mecanico] -> Auto -> Auto
aplicarOrdenDeReparacion fecha listaMecanicos = (cambiarFecha fecha).(autoPasaPorMecanicos listaMecanicos)

-- ==========================================================================================================================================
-- PUNTO 6: Orden Superior
-- ==========================================================================================================================================
-- Solamente se puede utilizar funciones de orden superior en este punto.
-- Parte 1) (Pablo): Técnicos que dejan el auto en condiciones
-- Dada una lista de técnicos determinar aquellos técnicos que dejarían el auto en condiciones (que no sea peligroso andar, recordar el punto 
-- 2.1 del integrante a).

autoEnCondiciones :: Auto -> Bool
autoEnCondiciones = not.autoPeligroso

tecnicosQueDejanElAutoEnCondiciones :: Auto -> [Mecanico] -> [Mecanico]
tecnicosQueDejanElAutoEnCondiciones auto = filter (autoEnCondiciones.(flip ($)) auto)

cantidadDeTecnicosQueDejanElAutoEnCondiciones :: Auto -> [Mecanico] -> Int
cantidadDeTecnicosQueDejanElAutoEnCondiciones auto = length.tecnicosQueDejanElAutoEnCondiciones auto

-- Parte 2) (Sylvina): Costo de reparación de autos que necesitan revisión
-- Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.

costoReparacionListaDeAutos :: [Auto] -> Int
costoReparacionListaDeAutos = sum.(map costoDeReparacion).(filter necesitaRevision)

-- ==========================================================================================================================================
-- PUNTO 7: 
-- ==========================================================================================================================================
-- Parte 1) (Pablo): Técnicos que dejan el auto en condiciones
-- En base al punto “dada una lista de técnicos determinar qué técnicos dejarían el auto en condiciones” y considerando una lista de técnicos
-- infinita, ¿podríamos obtener el primer técnico que deja el auto en condiciones? Muestre un ejemplo y justifique. 

tecnicosInfinitos = zulu:tecnicosInfinitos
-- Se puede obtener el primer técnico, ya que es la cabeza de la lista de técnicos y se puede obtener el primer (o el de la posición que sea)
-- elemento de cualquier lista infinita en Haskel, ya que este posee lazy "evaluation".
primerTecnicoQueDejaAutoEnCondiciones :: Auto -> [Mecanico] -> Mecanico
primerTecnicoQueDejaAutoEnCondiciones auto= head.tecnicosQueDejanElAutoEnCondiciones auto

-- Parte 2) (Sylvina): Costo de reparación de autos que necesitan revisión
-- En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.”,  ¿podríamos tener 
-- una lista infinita de autos? Muestre un ejemplo y justifique. Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, 
-- ¿cómo debería cambiar la función? Por otra parte, ¿esta versión aceptaría una lista infinita de autos? Modifique la función 6.b con otro nombre
-- y justifique sus respuestas.

autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0

autosInfinitos' :: Int -> [Auto]
autosInfinitos' n = Auto {
 patente = "AAA000",
 desgasteLlantas = [fromIntegral n, 0, 0, 0.3],
 rpm = 1500 + n,
 temperaturaAgua = 90,
 ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)

-- Respuesta:

-- No podríamos utilizar la función del punto 6b si tenemos una lista infinita, pues sum debe recorrer toda la lista.
-- O sea que si ejecutara: costoReparacionListaDeAutos autosInfinitos, no podría calcular su suma y se "colgaría" (produciría un error en tiempo de ejecución)

-- Ahora bien, si tomamos los tres primeros autos que necesitan revisión, se podría realizar el cálculo de Reparación de la Lista de los tres primeros
-- autos que cumplen con la función "necesitaRevisión", debido a la "lazy evaluation".
-- La nueva función sería:

costoReparacionDeTresAutos :: [Auto] -> Int
costoReparacionDeTresAutos = sum.(map costoDeReparacion).(take 3).(filter necesitaRevision)







-- CASOS DE PRUEBA

fiatRojo :: Auto
fiatRojo = Auto{
    patente = "AT001LN", 
    desgasteLlantas=[0.51, 0.1, 0.6, 0.4], 
    rpm=2200 ,
    temperaturaAgua=30 ,
    ultimoArreglo = (3,12,2015)
}

fiatAzul :: Auto
fiatAzul = Auto{
    patente = "DJV214", 
    desgasteLlantas=[0.6,1,3,3], 
    rpm=1950 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2019)
}

fiatVerde :: Auto
fiatVerde = Auto{
    patente = "DJV215", 
    desgasteLlantas=[0.5, 0.1, 0.6, 0.4], 
    rpm=125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}

upRojo :: Auto
upRojo = Auto{
    patente = "DFH029", 
    desgasteLlantas=[1,0,0,1], 
    rpm=2125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2014)
}

-- autos para probar el Punto 4

autoPunto4_1 :: Auto
autoPunto4_1 = Auto{
    patente = "AUT001", 
    desgasteLlantas=[0.1, 0.4, 0.2, 0], 
    rpm=2125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}

autoPunto4_2 :: Auto
autoPunto4_2 = Auto{
    patente = "AUT002", 
    desgasteLlantas=[0.2, 0.5, 0.6, 0.1], 
    rpm=2125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}

autoPunto4_3 :: Auto
autoPunto4_3 = Auto{
    patente = "AUT003", 
    desgasteLlantas= [0.1, 0.1, 0.1, 0], 
    rpm=2125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}

autoPunto4_4 :: Auto
autoPunto4_4 = Auto{
    patente = "AUT004", 
    desgasteLlantas=[0.3, 0.5, 0.6, 0.1], 
    rpm=2125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}

autoPunto4_5 :: Auto
autoPunto4_5 = Auto{
    patente = "AUT005", 
    desgasteLlantas=[0.1, 0.4, 0.2, 0.1], 
    rpm=2125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}