import Text.Show.Functions

data Chofer = Chofer{
    nombre :: String,
    kilometraje:: Int,
    viajes :: [Viaje],
    condicionDeViaje :: CondicionDeViaje
}deriving (Show)

data Viaje = Viaje{
    fecha:: Int,
    cliente:: Cliente,
    costo::Int
} deriving(Show,Eq)

data Cliente = Cliente{
    nombreCliente :: String,
    dondeVive :: String
} deriving(Show,Eq)

type CondicionDeViaje = Viaje -> Bool

cualquierViaje :: CondicionDeViaje
cualquierViaje _ = True

masDe200 :: CondicionDeViaje
masDe200  = ((>200) . costo)

nombreDelClienteMayorA :: Int -> CondicionDeViaje
nombreDelClienteMayorA n  = ((>n) . length . nombreCliente . cliente) 

noVivaEn :: String -> CondicionDeViaje
noVivaEn unaZona  = ((== unaZona) . dondeVive . cliente) 


lucas = Cliente "Lucas" "Victoria"
daniel = Chofer "Daniel" 23500 [ Viaje 20042017 lucas 150 ] (noVivaEn "Olivos")
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje

puedeTomarViaje ::  Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer  = (condicionDeViaje chofer) viaje

liquidacion :: Chofer -> Int
liquidacion =  (sum . map costo . viajes)

quienesLoToman :: Viaje -> [Chofer] -> [Chofer]
quienesLoToman viaje unosChoferes = filter (puedeTomarViaje viaje) unosChoferes

menosViajeTenga :: [Chofer] -> Chofer
menosViajeTenga unosChoferes = minimoSegun (length . viajes) unosChoferes

minimoSegun f lista = foldl1 (menorSegun f) lista
menorSegun f a b
  | f a < f b = a
  | otherwise = b

efectuarUnViaje :: Chofer -> Viaje -> Chofer
efectuarUnViaje chofer viaje = chofer{ viajes = viaje :  (viajes chofer)}

nitoInfy :: Chofer
nitoInfy = Chofer "Nito Infy" 70000 (repeat (Viaje 11032017 lucas 50)) (nombreDelClienteMayorA 3)






























































