-- PilaConTipoDeDatoAlgebraico.hs
-- Implementación de las pilas mediante tipos de datos algebraicos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

module PilaConTipoDeDatoAlgebraico 
    (Pila,
     vacia,    -- Pila a
     apila,    -- a -> Pila a -> Pila a
     cima,     -- Pila a -> a
     desapila, -- Pila a -> Pila a
     esVacia   -- Pila a -> Bool
    ) where

-- Tipo de dato algebraico de las pilas:
data Pila a = Vacia
            | P a (Pila a)
              deriving Eq

-- Procedimiento de escritura de pilas.
instance (Show a) => Show (Pila a) where
    showsPrec p Vacia cad   = showChar '-' cad
    showsPrec p (P x s) cad = shows x (showChar '|' (shows s cad))

-- Ejemplo de pila:
--    ghci> p1
--    1|2|3|-
p1 :: Pila Int
p1 = apila 1 (apila 2 (apila 3 vacia))

-- vacia es la pila vacía. Por ejemplo,
--    ghci> vacia
--    -
vacia :: Pila a
vacia = Vacia

-- (apila x p) es la pila obtenida añadiendo x encima de la pila p. Por
-- ejemplo, 
--    apila 4 p1  =>  4|1|2|3|-
apila :: a -> Pila a -> Pila a
apila x p = P x p

-- (cima p) es la cima de la pila p. Por ejemplo,
--    cima p1  ==  1
cima :: Pila a -> a
cima Vacia   = error "la pila vacia no tiene cima"
cima (P x _) =  x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo,
--    desapila p1  =>  2|3|-
desapila :: Pila a -> Pila a
desapila Vacia   = error "no se puede desapila la pila vacia"
desapila (P _ p) = p

-- (esVacia p) se verifica si p es la pila vacía. Por ejemplo,
--    esVacia p1         ==  False
--    esVacia vacia  ==  True
esVacia :: Pila a -> Bool
esVacia Vacia = True
esVacia _     = False


[("Photo Editor & Candy Camera & Grid & ScrapBook","ART_AND_DESIGN",Just 4.1),("Coloring book moana","ART_AND_DESIGN",Just 3.9),("U Launcher Lite \8211 FREE Live Cool Themes, Hide Apps","ART_AND_DESIGN",Just 4.7),("Sketch - Draw & Paint","ART_AND_DESIGN",Just 4.5),("Pixel Draw - Number Art Coloring Book","ART_AND_DESIGN",Just 4.3),("Paper flowers instructions","ART_AND_DESIGN",Just 4.4),("Smoke Effect Photo Maker - Smoke Editor","ART_AND_DESIGN",Just 3.8),("Infinite Painter","ART_AND_DESIGN",Just 4.1),("Garden Coloring Book","ART_AND_DESIGN",Just 4.4),("Kids Paint Free - Drawing Fun","ART_AND_DESIGN",Just 4.7),("Text on Photo - Fonteee","ART_AND_DESIGN",Just 4.4),("Name Art Photo Editor - Focus n Filters","ART_AND_DESIGN",Just 4.4),("Tattoo Name On My Photo Editor","ART_AND_DESIGN",Just 4.2),("Mandala Coloring Book","ART_AND_DESIGN",Just 4.6),("3D Color Pixel by Number - Sandbox Art Coloring","ART_AND_DESIGN",Just 4.4),("Learn To Draw Kawaii Characters","ART_AND_DESIGN",Just 3.2),("Photo Designer - Write your name with shapes","ART_AND_DESIGN",Just 4.7),("350 Diy Room Decor Ideas","ART_AND_DESIGN",Just 4.5),("FlipaClip - Cartoon animation","ART_AND_DESIGN",Just 4.3),("ibis Paint X","ART_AND_DESIGN",Just 4.6),("Logo Maker - Small Business","ART_AND_DESIGN",Just 4.0),("Boys Photo Editor - Six Pack & Men's Suit","ART_AND_DESIGN",Just 4.1),("Superheroes Wallpapers | 4K Backgrounds","ART_AND_DESIGN",Just 4.7),("Mcqueen Coloring pages","ART_AND_DESIGN",Nothing),("HD Mickey Minnie Wallpapers","ART_AND_DESIGN",Just 4.7),("Harley Quinn wallpapers HD","ART_AND_DESIGN",Just 4.8),("Colorfit - Drawing & Coloring","ART_AND_DESIGN",Just 4.7),("Animated Photo Editor","ART_AND_DESIGN",Just 4.1),("Pencil Sketch Drawing","ART_AND_DESIGN",Just 3.9),("Easy Realistic Drawing Tutorial","ART_AND_DESIGN",Just 4.1),("Pink Silver Bow Keyboard Theme","ART_AND_DESIGN",Just 4.2),("Art Drawing Ideas","ART_AND_DESIGN",Just 4.1),("Anime Manga Coloring Book","ART_AND_DESIGN",Just 4.5),("Easy Origami Ideas","ART_AND_DESIGN",Just 4.2),("I Creative Idea","ART_AND_DESIGN",Just 4.7),("How to draw Ladybug and Cat Noir","ART_AND_DESIGN",Just 3.8),("UNICORN - Color By Number & Pixel Art Coloring","ART_AND_DESIGN",Just 4.7),("Floor Plan Creator","ART_AND_DESIGN",Just 4.1),("PIP Camera - PIP Collage Maker","ART_AND_DESIGN",Just 4.7),("How To Color Disney Princess - Coloring Pages","ART_AND_DESIGN",Just 4.0),("Drawing Clothes Fashion Ideas","ART_AND_DESIGN",Just 4.2),("Sad Poetry Photo Frames 2018","ART_AND_DESIGN",Just 4.5),("Textgram - write on photos","ART_AND_DESIGN",Just 4.4),("Paint Splash!","ART_AND_DESIGN",Just 3.8),("Popsicle Sticks and Similar DIY Craft Ideas","ART_AND_DESIGN",Just 4.2),("Canva: Poster, banner, card maker & graphic design","ART_AND_DESIGN",Just 4.7),("Install images with music to make video without Net - 2018","ART_AND_DESIGN",Just 4.6),("Little Teddy Bear Colouring Book Game","ART_AND_DESIGN",Just 4.2),("How To Draw Food","ART_AND_DESIGN",Just 4.3),("Monster Truck Stunt 3D 2019","AUTO_AND_VEHICLES",Just 4.2),("Real Tractor Farming","AUTO_AND_VEHICLES",Just 4.0),("Ultimate F1 Racing Championship","AUTO_AND_VEHICLES",Just 3.8),("Used Cars and Trucks for Sale","AUTO_AND_VEHICLES",Just 4.6),("American Muscle Car Race","AUTO_AND_VEHICLES",Just 3.9),("Offroad Oil Tanker Driver Transport Truck 2019","AUTO_AND_VEHICLES",Just 4.3),("Tickets SDA 2018 and Exam from the State Traffic Safety Inspectorate with Drom.ru","AUTO_AND_VEHICLES",Just 4.9)]