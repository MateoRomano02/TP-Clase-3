module Library where
import PdePreludat

doble :: Int -> Int
doble numero = numero + numero

type Nombre = String
type Antiguedad = Int
type Grado = Int
type Clan = String


data Hechicero = Hechicero {
    nombre::Nombre,
    antiguedad::Antiguedad,
    grado::Grado,
    clan::Clan
} deriving Show

nobara = Hechicero "Nobara" 1 3 "Kugisaki"
satoru = Hechicero "Satoru" 15 0 "Gojo"
maki = Hechicero "Maki" 3 4 "Zenin"
yuji = Hechicero "Yuji" 0 1 "Itadori"

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechi = antiguedad hechi > 1

estaPreparado :: [Hechicero] -> Bool
estaPreparado lista = length lista > 3

subirDeGrado :: Hechicero -> Int
subirDeGrado hechi | grado hechi == 0 = grado hechi
                   | otherwise = grado hechi - 1
                         
esPrestigioso :: Hechicero -> Bool
esPrestigioso hechi = clan hechi == "Zenin" || clan hechi == "Gojo" || clan hechi == "Kamo"
