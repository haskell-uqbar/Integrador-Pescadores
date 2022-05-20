module Library where
import PdePreludat

type CriterioDevolucion = Pescado -> Bool


type Genero = String
-- data Genero = Masculino | Femenino | NoBinario


data Pescador = Pescador{
    apodo :: String,
    peso :: Number,   
    pescados :: [Pescado],
    genero:: Genero
}deriving Eq

data Pescado = Pescado{
    especie :: String,  
    tamanio :: Number
} deriving Eq

instance Show Pescador where
    show (Pescador apodo peso pescados genero) = apodo ++ " " ++ show peso ++ " " ++ show pescados 

instance Show Pescado where
    show (Pescado especie tamanio ) = especie ++ " " ++ show tamanio  


bagreChico = Pescado "Bagre" 0.3
bagreGrande = Pescado "Bagre" 30
pejerrey = Pescado "Pejerrey" 2

tiburon = Pescado "Tiburon" 100

granPescador = Pescador "Juancho" 80 [bagreChico, bagreGrande, pejerrey] "Masculino"
malPescador = Pescador "Tito" 100 [] "Masculino" 
pescadora = Pescador "Mary" 75 [pejerrey] "Femenino"    

cantPiezas :: Pescador -> Number
cantPiezas = length.pescados

ultimoMasPesadoQueElPrimero :: Pescador -> Bool
ultimoMasPesadoQueElPrimero pescador = 
    (tamanio.last.pescados) pescador > (tamanio.head.pescados) pescador

pescar :: Pescado -> Pescador -> Pescador
pescar pescado pescador = 
    pescador {pescados = pescado:pescados pescador}

merienda :: Pescador -> Number -> Pescador
merienda pescador pesoAlimento = 
    pescador {peso = peso pescador + pesoAlimento * asimilacion (genero pescador) }

asimilacion "Masculino" = 0.02
asimilacion "Femenino" = 0.0175
asimilacion "NoBinario" = 0.03
{-
asimilacion Masculino = 0.02
asimilacion Femenino = 0.0175
asimilacion NoBinario = 0.03
-}

desprenderPescados :: Pescador -> Pescador
desprenderPescados pescador =
    pescador {pescados = filter ((>=500).tamanio) (pescados pescador)}

pequenios :: CriterioDevolucion
pequenios pez = tamanio pez < 1

protegidos :: [String] -> CriterioDevolucion
protegidos especiesProtegidas pez = elem (especie pez) especiesProtegidas

especieParticular :: String -> CriterioDevolucion
especieParticular especiep pez = especie pez == especiep

combinada :: [CriterioDevolucion] -> CriterioDevolucion
combinada criterios pez = any ($ pez ) criterios
--combinada criterios pez = any (aplicar pez ) criterios
--aplicar pez criterio = criterio pez

sinDevolucion :: CriterioDevolucion
sinDevolucion pez = False


devolver :: Pescador -> CriterioDevolucion -> Pescador
devolver pescador criterio = 
    pescador {pescados = filter (not.criterio) (pescados pescador)}


pescarConDevolucion :: CriterioDevolucion -> Pescado -> Pescador -> Pescador
pescarConDevolucion  criterio pescado pescador 
    | criterio pescado = pescador
    | otherwise = pescador {pescados = pescado:pescados pescador}

--------------------------------------------
type Circunstancia = Embarcacion-> Embarcacion

data Embarcacion = Embarcacion{
    pescadores ::[Pescador],
    pesoMaximo :: Number,
    criterio :: CriterioDevolucion 
    } deriving Show


barquito = Embarcacion [granPescador, malPescador] 200 pequenios

{- Incorporar pescador: Se agrega al pescador a la salida de pesca, pero si con su peso hace superar el peso máximo de la embarcación, 
no se le permite subir. -}
incorporarPescador::Pescador -> Circunstancia
incorporarPescador pescador embarcacion
  | pesoTotal embarcacion + peso pescador > pesoMaximo embarcacion = embarcacion
  | otherwise = embarcacion{pescadores = pescador:pescadores embarcacion}

pesoTotal:: Embarcacion -> Number
pesoTotal = sum.map pesoConPesca.pescadores

pesoConPesca:: Pescador -> Number
pesoConPesca pescador = peso pescador + sum (map tamanio (pescados pescador))

{- Encontrar un cardumen: cuando se llega a un cardumen, todos los miembros de la salida pescan un ejemplar del pez 
típico del cardúmen, del que se conoce el tamaño estandar. Por ejemplo, podría tratarse de un cardumen de pejerreyes de 1.5 de peso.-}
cardumen :: Pescado -> Circunstancia
cardumen pez embarcacion = embarcacion{pescadores = 
      map (pescarConDevolucion (criterio embarcacion) pez)  (pescadores embarcacion)    }

{- Tormenta: El mal tiempo hace tambalear la embarcación. Pese a haber controlado que el peso de los pescadores no 
supere el máximo permitido al subirse, podria ser que considerando las pescas realizadas sí se supere ese límite. 
En ese caso, lamentablemente, uno de los pescadores cae al agua. -}
tormenta :: Embarcacion -> Embarcacion
tormenta embarcacion
  | pesoTotal embarcacion <= pesoMaximo embarcacion = embarcacion
  | otherwise = embarcacion{pescadores = tail (pescadores embarcacion)} 

{- Aparece un tiburón enorme: El mejor pescador lo pesca. (Definir con qué criterio se determina el mejor pescador) 
Se calcula que su peso es 100. -}
pescaTiburon :: Embarcacion -> Embarcacion
pescaTiburon embarcacion = embarcacion {
    pescadores = pescarConDevolucion (criterio embarcacion) tiburon mejorPescador : quitar mejorPescador (pescadores embarcacion)
    }
    where mejorPescador = head (pescadores embarcacion)
    -- Reemplazar por un mejor criterio


quitar:: Eq a => a -> [a] -> [a] 
quitar elemento lista = filter (elemento /=) lista

{- Demasiada devolución: En cierto momento se dan cuenta que el criterio de devolución vigente es muy estricto y deciden 
cambiarlo por su opuesto. Es decir, todo lo que anteriormente devolvían en las proximas pescas lo conservan y viceversa. -}
demasiadaDevolucion :: Embarcacion -> Embarcacion
demasiadaDevolucion embarcacion = embarcacion{criterio = not.criterio embarcacion} 


{- Prefectura: Cuando aparece la prefectura e intercepta la embarcación, les impone un nuevo criterio de devolución, 
que se agrega combinándose con el que ya tenía la salida de pesca. Lo mismo ocurre si aparece el Guardafauna o autoridades 
del ente regulador de pesca, aunque el nuevo criterio de devolución puede ser diferente en cada caso. -}

{- Cruzarse con otra salida de pesca: ¿Qué le pasa a un grupo de pescadores en medio de una salida de pesca cuando se 
cruza con otro? ¿Serán rivales, amigos, indiferentes? Inventar una función que lo represente adecuadamente.  -}
cruzarEmbarcaciones :: Embarcacion -> Embarcacion -> Embarcacion
cruzarEmbarcaciones embarcacion2 embarcacion1
  | cantidadPescadores embarcacion1 > cantidadPescadores embarcacion2 = embarcacion1
  | otherwise = embarcacion2

cantidadPescadores :: Embarcacion -> Number
cantidadPescadores = length.pescadores







salidaDePesca::[Circunstancia]->Embarcacion -> Embarcacion
salidaDePesca circunstancias embarcacion = foldr ($) embarcacion circunstancias
--salidaDePesca circunstancias embarcacion = foldl (flip $) embarcacion circunstancias
--salidaDePesca circunstancias embarcacion = foldl aplicarCircunstancia embarcacion circunstancias

--aplicarCircunstancia embarcacion circunstancia = circunstancia embarcacion

