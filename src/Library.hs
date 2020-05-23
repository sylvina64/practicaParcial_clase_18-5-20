module Library
    (Tarjeta (..) , Viaje (..),
    nombreCompleto, nombresCompletosMayoresDe65, 
    mes,mesDeNacimiento,medioDeTransporte,edad,esMayorDeEdad,estaUsado,tarjetaUsada,mesTexto,
    sube1,sube2,sube3, sube4, viaje1,viaje2,viaje3,viaje4,viajesEfectuados,
    precioDelPasaje,promedioGastoEnPasajes,
    cantidadDeTarjetasSube, sinRepetidos,cantidadDePersonasNacidasEn
    
    ) where

import PdePreludat

type Nombre = String
type Fecha = (Int, Int, Int)
anio (_, _, year) = year

data Tarjeta = Tarjeta {
    nombre :: Nombre,
    apellido :: Nombre,
    dni :: Int,
    fechaNac :: Fecha
} deriving Show

data Viaje = Viaje {
    tarjeta :: Tarjeta,
    medioTransporte :: String,
    importeGastado :: Float,
    fecha :: Fecha
} deriving Show

-- CASOS DE PRUEBA
sube1 :: Tarjeta
sube1 = Tarjeta {
    nombre = "Sylvina",
    apellido = "Enriquez",
    dni = 18181984,
    fechaNac = (03,12,1964)
}

sube2 :: Tarjeta
sube2 = Tarjeta {
    nombre = "Facundo",
    apellido = "Leoni",
    dni = 35143344,
    fechaNac = (12,3,1990)
}

sube3 :: Tarjeta
sube3 = Tarjeta {
    nombre = "Paco",
    apellido = "Rabane",
    dni = 11223344,
    fechaNac = (5,10,1957)
}

sube4 :: Tarjeta
sube4 = Tarjeta {
    nombre = "Juan",
    apellido = "Y Pinchame",
    dni = 12131415,
    fechaNac = (15,05,1967)
}

tarjetasEmitidas ::[Tarjeta]
tarjetasEmitidas = [sube1,sube2,sube3,sube4]

viaje1 = Viaje {tarjeta = sube1, medioTransporte = "tren", importeGastado=25.23, fecha = (10,03,2020)}
viaje2 = Viaje {tarjeta = sube2, medioTransporte = "colectivo", importeGastado=15, fecha = (20,11,2019)}
viaje3 = Viaje {tarjeta = sube3, medioTransporte = "subte", importeGastado=23.2, fecha = (5,10,2019)}
viaje4 = Viaje {tarjeta = sube1, medioTransporte = "tren", importeGastado=25.23, fecha = (7,08,2019)}

viajesEfectuados :: [Viaje]
viajesEfectuados =[viaje1,viaje2,viaje3,viaje4]

-- punto 2
--a) nombreCompleto, que recibe una tarjeta, y devuelve el nombre del dueño de la misma. Determinar el dominio e imagen de esta función.
nombreCompleto :: Tarjeta -> Nombre
nombreCompleto tarjeta = ((++ apellido tarjeta).(++ " ").nombre) tarjeta

--b) mesDeNacimiento, que recibe una tarjeta, y devuelve el mes de nacimiento del dueño de la misma.

mesTexto :: Fecha -> String
mesTexto (_,1,_) = "Enero"
mesTexto (_,2,_) = "Febrero"
mesTexto (_,3,_) = "Marzo"
mesTexto (_,4,_) = "Abril"
mesTexto (_,5,_) = "Mayo"
mesTexto (_,6,_) = "Junio"
mesTexto (_,7,_) = "Julio"
mesTexto (_,8,_) = "Agosto"
mesTexto (_,9,_) = "Septiembre"
mesTexto (_,10,_) = "Octubre"
mesTexto (_,11,_) = "Noviembre"
mesTexto (_,12,_) = "Diciembre"
mesTexto (_,_,_) = "Mal Ingresado el mes"

mesDeNacimiento :: Tarjeta -> String
mesDeNacimiento  = mesTexto.fechaNac

--c) edad, que recibe una tarjeta, y devuelve la edad del dueño hasta el día de hoy.

edad :: Tarjeta -> Int
edad = (+ 2020).(* (-1)).anio.fechaNac

--d) esMayorDeEdad, que recibe una tarjeta, y devuelve si el dueño tiene más de 65 años.

esMayorDeEdad :: Tarjeta -> Bool
esMayorDeEdad = (> 65).edad

--e) tarjetaUsada, que recibe un viaje, y devuelve la tupla completa de la tarjeta usada en ese viaje.

estaUsado :: Viaje -> Bool
estaUsado = (> 0).importeGastado

tarjetaUsada :: Viaje -> Tarjeta
tarjetaUsada viaje | estaUsado viaje = tarjeta viaje

--f) medioDeTransporte, que recibe un viaje, y devuelve el medio de transporte en el que se viajó.

medioDeTransporte :: Viaje -> String
medioDeTransporte = medioTransporte

--g) precioDelPasaje, que recibe un viaje, y devuelve el precio del viaje.

precioDelPasaje :: Viaje -> Float
precioDelPasaje = importeGastado

--h) cantidadDeTarjetasSube, que informa la cantidad total de tarjetas entregadas.

cantidadDeTarjetasSube :: [Viaje] -> Int
cantidadDeTarjetasSube = length

-- 3.	Los nombres completos de las personas mayores de edad (mayores de 65 años). (Hacerlo por listas por comprensión)

nombresCompletosMayoresDe65 :: [Tarjeta] -> [Nombre] 
nombresCompletosMayoresDe65 listaTarjetas = [nombreCompleto tarjeta | tarjeta <- listaTarjetas,esMayorDeEdad tarjeta]

-- 4.	Cuántos viajes realizó un pasajero determinado.

cuantosViajesRealizo :: Nombre -> Int
cuantosViajesRealizo nombreYApellido = (length.filter ((== nombreYApellido).nombreCompleto.tarjeta)) viajesEfectuados

-- 5.	Todos las tarjetas emitidas que viajaron al menos una vez.

listaTarjetasUtilizadas :: [Tarjeta]
listaTarjetasUtilizadas = map tarjeta viajesEfectuados

casteoListaTarjetasATuplas :: [Tarjeta] -> [(Nombre,Nombre,Int,Fecha)]
casteoListaTarjetasATuplas =  map (\tarjeta -> (nombre tarjeta, apellido tarjeta, dni tarjeta,fechaNac tarjeta))

sinRepetidos ::(Eq a)=> [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) | elem x xs = sinRepetidos xs
                    | otherwise = x:(sinRepetidos xs)

descasteoListaTarjetas ::  [(Nombre,Nombre,Int,Fecha)] -> [Tarjeta]
descasteoListaTarjetas = map (\(nombreTupla,apellidoTupla,dniTupla,fechaNacTupla) -> Tarjeta {nombre = nombreTupla , apellido=apellidoTupla, dni=dniTupla,fechaNac=fechaNacTupla})

--tarjetasEmitidasUtilizadas :: [Tarjeta]
tarjetasEmitidasUlitizadas = (descasteoListaTarjetas.sinRepetidos.casteoListaTarjetasATuplas) listaTarjetasUtilizadas

-- 6.	a) cantidadDePersonasNacidasEn que recibe el número de un mes y retorna la cantidad de personas personas que nacieron ese mes.

mes (_,mm,_)=mm

cantidadDePersonasNacidasEn :: Int -> Int
cantidadDePersonasNacidasEn mesIndicado =(length.filter ((== mesIndicado).mes.fechaNac)) tarjetasEmitidas

--      b) Lista de tuplas con (mes, cantDePersonasNacidas). Si tuviésemos la lista de tarjetasSUBE completa con miles de usuarios podríamos hacer:
--         Main> cantidadDePersonasNacidasPorMes
--         [(1,7967), (2,9553), (3,29630), (4,5138), (5,6332), (6,10256), (7,3369), (8,7896), (9,5555), (10,4561), (11, 20589), (12, 15379)]

listaCantidadNacidesPorMes :: [(Int,Int)]
listaCantidadNacidesPorMes = [(x,cantidadDePersonasNacidasEn x) | x <- [1..12]]

-- 7.	a) Gasto total de un pasajero determinado.

-- como el dni es único por persona, lo utilizo para filtrar

gastoTotalDeUnaPersona :: Int -> Float
gastoTotalDeUnaPersona =  sum.listaGastosDeUnaPersona

listaGastosDeUnaPersona :: Int -> [Float] 
listaGastosDeUnaPersona dniPersona = (map importeGastado.filter ((== dniPersona).dni.tarjeta)) viajesEfectuados

--      b) Gasto total de cada persona. (sugerencia hacerlo por listas por comprensión). Ejemplo de uso:
--         Main> gastoDeCadaPersona  
--         [("Juan Perez",7.5),("Pablo Garcia",4.5),("Maria Gonzalez",5.6),("Silvio Rodriguez",0.0)]

gastoDeCadaPersona :: [(Nombre,Float)]
gastoDeCadaPersona = [ ( nombreCompleto tarjeta, (gastoTotalDeUnaPersona.dni) tarjeta) | tarjeta <- tarjetasEmitidas ]

-- 8.	Cuál es el importe promedio de gasto en pasajes de un determinado pasajero.

promedioGastoEnPasajes :: Int -> Float
promedioGastoEnPasajes dniPersona = (sum.listaGastosDeUnaPersona) dniPersona / (fromIntegral.length.listaGastosDeUnaPersona) dniPersona

-- 9.	Cuánto recauda un determinado medio de transporte.

cuantoSeRecaudaCon :: String -> Float
cuantoSeRecaudaCon movil = (sum.map importeGastado.filter ((== movil).medioTransporte)) viajesEfectuados

-- 10.	Las personas que viajaron en todos los medios de transporte que estén en una lista de transportes. Resolver utilizando listas por comprensión.
--  	Main> viajaronEn [“subte A”,”subte B”]
--   	“Juan Perez”

viajaronEn :: [String]-> [Nombre]
viajaronEn listaTransportes = [ nombreCompleto tarjeta  | tarjeta <- tarjetasEmitidas, personaViajoEn (nombreCompleto tarjeta) listaTransportes]
    
listaDeTransportesUtilizadosPor :: Nombre -> [String]
listaDeTransportesUtilizadosPor persona = (map medioTransporte.filter ((== persona).nombreCompleto.tarjeta)) viajesEfectuados

personaViajoEn :: Nombre -> [String]-> Bool
personaViajoEn nombre listaTransportes = ((== length listaTransportes).length.interseccion (listaDeTransportesUtilizadosPor nombre)) listaTransportes

interseccion :: (Eq a)=> [a] -> [a] -> [a]
interseccion x [] = []
interseccion [] y = []
interseccion (x:xs) y | elem x y = x : interseccion xs y
                      | otherwise = interseccion xs y

-- 11.	Medio de transporte que recaudó más en total.

medioDeTransporteQueMasRecaudo :: String
medioDeTransporteQueMasRecaudo = (snd.maximum) listaTuplasMovilImporte

listaTuplasMovilImporte :: [(Float,String)]
listaTuplasMovilImporte = [((cuantoSeRecaudaCon.medioTransporte) viaje , medioTransporte viaje) | viaje <- viajesEfectuados]


-- 12.	obtenerNViajesQueCumplenCon, que recibe un número y un criterio, y devuelve los primeros N viajes que cumplen con dicho criterio.
--   	Main> obtenerNViajesQueCumplenCon 3  ((>=3).precioDelPasaje)
--      [(("Pablo Garcia", 25454545,  (19,5,1990)), "29", 3,(1,11,2012)),
--      ("Maria Gonzalez", 20000777,  (27,7, 1970),  "114", 4,(13,2,2013))]

obtenerNViajesQueCumplenCon :: Int -> (Viaje->Bool) -> [Viaje]
obtenerNViajesQueCumplenCon n condicion = (take n.filter condicion ) viajesEfectuados

-- 13.	¿Qué ocurriría con el punto 12 si utilizamos listas infinitas en viajesEfectuados?

--       Funcionaría igual, debido a la "lazy evaluation"
