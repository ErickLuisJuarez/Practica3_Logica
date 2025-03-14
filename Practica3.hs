module Practica03 where

-- Práctica 03: Resolución Binaria

-- Integrantes: Luis Juárez Erick
--              Herrera Avalos Julio Alejandro
--              Peña Villegas Diego Eduardo

-- 3.1 Lógica Proposicional

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


-- 3.2 Formas Normales

--Funcion auxiliar que devuelve la negacion de la formula ingresada
negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons True) = (Cons False)
negar (Cons False) = (Cons True)
negar (Not f) = f
negar (And f1 f2) = (Or (negar f1) (negar f2))
negar (Or f1 f2) = (And (negar f1) (negar f2))
negar (Impl f1 f2) = (And f1 (negar f2))
negar (Syss f1 f2)= negar (And (Impl f1 f2) (Impl f2 f1))

--Funcion auxiliar que aplica la distribucion de la disyuncion parcialmente
distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or q p)) (distribuir (Or r p))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir p = p

-- Funcion que convierte una formula proposicional en su forma normal negativa
fnn :: Prop -> Prop
fnn (Not p) = negar (fnn p)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = fnn (Or (negar p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))
fnn p = p

-- Ejercicio 2
fnc :: Prop -> Prop
fnc = undefined


-- 3.3 Resolución Binaria

-- Sinónimo Literal
type Literal = Prop

-- Sinónimo Cláusula
type Clausula = [Literal]


-- Ejercicios

-- Función auxiliar para procesar disyunciones
processOR :: Prop -> Clausula
processOR (Or p q) = processOR p ++ processOR q
processOR p = [p]  

-- Función para extraer cláusulas de una fórmula en FNC
clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q 
clausulas (Or p q) = [processOR (Or p q)] 
clausulas p = [[p]]  


-- Ejercicio 2clau
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined


-- 3.4 Algoritmo de saturación

-- Ejercicios

-- Función auxiliar para verificar si dos literales son complementarios
esComplementario :: Literal -> Literal -> Bool
esComplementario (Not l1) l2 = l1 == l2
esComplementario l1 (Not l2) = l1 == l2
esComplementario _ _ = False

-- Función que determina si es posible obtener un resolvente a partir de 2 clausulas
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente clausula1 clausula2 =
    any (\l1 -> any (\l2 -> esComplementario l1 l2) clausula2) clausula1

-- Ejercicio 2
saturacion :: Prop -> Bool
saturacion = undefined