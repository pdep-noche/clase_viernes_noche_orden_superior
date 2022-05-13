module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

cantidadDeElementos :: [a] -> Number
cantidadDeElementos lista = foldl (\sem _ -> sem + 1) 0 lista

cantidadDeElementos' :: [a] -> Number
cantidadDeElementos' lista = foldr (\_ sem -> sem + 1)   0  lista

masGastador :: [(String, Number)] -> (String, Number)
masGastador (cab:lista) = foldl tieneMayorGasto   cab lista

tieneMayorGasto :: (String, Number) -> (String, Number) -> (String, Number)
tieneMayorGasto persona otraPersona | snd persona > snd otraPersona = persona
                                    | otherwise = otraPersona

masGastador' :: [(String, Number)] -> (String, Number)
masGastador' (cab:lista) = foldr tieneMayorGasto  cab   lista

monto :: [(String, Number)] -> Number
monto listaEmple = foldl (\sem (_, gasto) -> sem + gasto ) 0 listaEmple

monto' :: [(String, Number)] -> Number
monto' listaEmple = foldr (\(_, gasto) sem -> gasto + sem)  0 listaEmple

{- foldl (\sem fun -> fun sem) 2 [(3+), (*2), (5+)]

foldl (flip ($)) 2 [(3+), (2*), (5+) ]

-}

{- foldr (\fun sem -> fun sem) 2 [(3+), (*2), (5+)]

foldr ($) 2 [(3+), (*2), (5+)]
-}

type Nombre  = String
type InversionInicial = Number
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"],Proy "ventaChurros" 1000 ["cocinero"] ]

redSocialDeArte = Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}


maximoProySegun::( Proyecto -> Number  ) -> [Proyecto] -> Proyecto
maximoProySegun f (unProy:proyectos) = foldl  (maximoSegun f)   unProy proyectos

{- con foldr
maximoProySegun f (unProy:proyectos) = foldr  (maximoSegun f)   unProy proyectos
-}

{- con foldl1
maximoProySegun f proyectos = foldl1  (maximoSegun f) proyectos
-}

maximoSegun :: (Proyecto -> Number) -> Proyecto -> Proyecto -> Proyecto
maximoSegun f unProy otroProy | f unProy >= f otroProy = unProy
                              | otherwise = otroProy

{- a
> maximoProySegun inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
-}

{- b
*Spec Library Spec> maximoProySegun (length.profesionales) proyectos
Proy {nombre = "restaurante", inversionInicial = 5000, profesionales = ["cocinero","adm. de empresas","contador"]}
-}

{- c
*Spec Library Spec> maximoProySegun (length.words.nombre) proyectos
Proy {nombre = "red social de arte", inversionInicial = 20000, profesionales = ["ing. en sistemas","contador"]}
-}


data Flor= Flor{nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Number } deriving Show
 
rosa = Flor "rosa" "decorativo" 120
jazmin =  Flor "jazmin" "aromatizante" 100
violeta=  Flor "violeta" "infusión" 110
orquidea =  Flor "orquidea" "decorativo" 90

flores = [rosa,violeta, jazmin,orquidea]

maximaFlorSegun :: (Flor -> Number) -> [Flor] -> String
maximaFlorSegun f flores = nombreFlor.maximaFlor f $ flores

maximaFlor :: (Flor -> Number) -> [Flor] -> Flor
maximaFlor  _   [flor] = flor
maximaFlor f (flor: flores) | f flor >= (f. maximaFlor f) flores = flor
                            | otherwise = maximaFlor f flores

{- a
*Spec Library Spec> maximaFlorSegun cantidadDeDemanda flores
"rosa"
-}

{- b
*Spec Library Spec> maximaFlorSegun (length.nombreFlor) flores
"orquidea"
-}

{-c 
*Spec Library Spec> maximaFlorSegun  ((`mod` 4). cantidadDeDemanda) flores
"orquidea"
-}

estaOrdenada :: [Flor] -> Bool
estaOrdenada [_] = True
estaOrdenada (flor: otraFlor: flores) = cantidadDeDemanda flor > cantidadDeDemanda otraFlor &&  estaOrdenada (otraFlor:flores)