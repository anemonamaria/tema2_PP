{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.

    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***

    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Game = Game {
	dim_lines :: Int,
	dim_columns :: Int,
	hunter :: Position,
	targets :: [Target],
	obstacles :: [Position],
	gateways :: [(Position, Position)]

} deriving (Eq, Ord)
{-
    *** Optional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.

    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

findHunter :: Position -> Game -> Bool
findHunter (x,y) game
    | x > (dim_lines game) || y > (dim_columns game) = False
	| (hunter game) == (x, y) = True
	| otherwise = False

findTarget ::  Position -> Game -> Bool
findTarget (x, y) game
	| x > (dim_lines game) || y > (dim_columns game) = False
	| otherwise = any ((x, y) == ) [position target | target <- targets game]

findObstacles :: Position -> Game -> Bool
findObstacles (x, y) game
	| x > (dim_lines game) || y > (dim_columns game) = False
	| otherwise = any ((x, y) ==) (obstacles game)


findGateway :: Position -> Game -> Bool
findGateway (x, y) game
	| x > (dim_lines game) || y > (dim_columns game) = False
	| otherwise = any ((x, y) ==) ([pos1 | (pos1, pos2) <- gateways game] ++ [pos2 | (pos1, pos2) <- gateways game])

obstacle :: Char
obstacle = '@'

target :: Char
target = '*'

hunters :: Char
hunters = '!'

gateway :: Char
gateway = '#'

empty :: Char
empty = ' '

printCell :: Game -> Position -> String
printCell game (x, y)
	| y == (dim_columns game) && x /= (dim_lines game) && findObstacles (x, y) game == True = [obstacle] ++ "\n"
	| y == (dim_columns game) && x /= (dim_lines game) && findTarget (x, y) game == True = [target] ++ "\n"
	| y == (dim_columns game) && x /= (dim_lines game) && findHunter (x, y) game == True = [hunters] ++ "\n"
	| y == (dim_columns game) && x /= (dim_lines game) && findGateway (x, y) game == True = [gateway] ++ "\n"
	| y == (dim_columns game) && x /= (dim_lines game) = [empty] ++ "\n"
	| findObstacles (x, y) game == True = [obstacle]
	| findTarget (x, y) game == True = [target]
	| findHunter (x, y) game == True = [hunters]
	| findGateway (x, y) game == True = [gateway]
 	| otherwise = [empty]


gameAsString :: Game -> String
gameAsString game = foldl (\acc x -> acc ++ printCell game x) [] [(x, y) | x <- [0..(dim_lines game)], y <- [0..(dim_columns game)]]

instance Show Game where
    show = gameAsString

{-
    *** TODO ***

    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame my_lines my_columns = Game{
	dim_lines = my_lines - 1,
	dim_columns = my_columns - 1,
	hunter = (1,1),
	targets = [],
	obstacles = [(x, y) | x <- [0, my_lines - 1], y <- [0..my_columns - 1]] ++ [(x, y) | x <- [0..my_lines - 1], y <- [0, my_columns - 1]],
	gateways = []
}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (x, y) oldgame
	| x > (dim_lines oldgame) || y > (dim_columns oldgame) || x < 0 || y < 0 = oldgame
	| findObstacles (x, y) oldgame == True = oldgame
	| findTarget (x, y) oldgame == True = oldgame
	| findHunter (x, y) oldgame == True = oldgame
	| otherwise = oldgame {hunter = (x, y)}

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav (x, y) oldgame
	| x > (dim_lines oldgame) || y > (dim_columns oldgame) = oldgame
	| findObstacles (x, y) oldgame == True = oldgame
	| findTarget (x, y) oldgame == True = oldgame
	| findHunter (x, y) oldgame == True = oldgame
	| otherwise = oldgame {targets = (Target (x, y) behav) : (targets oldgame)}

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway  (pos1, pos2) game = game {gateways = (pos1, pos2) : (gateways game)}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game = game {obstacles = (pos) : (obstacles game)}

{-
    *** TODO ***

    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
	| findObstacles pos game == True = Nothing
 	| findGateway pos game == True = Just (head (foldl (\acc x -> if pos == fst x then (snd x) : acc else if pos == snd x then (fst x) : acc else acc) [] (gateways game)))
	| findHunter pos game == False && findTarget pos game == False = Just pos

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.

    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.

    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

go :: Position -> Game -> Position -> Target
go (x, y) game (nx, ny)
	| x + nx < 0 || x + nx > (dim_lines game) || y + nx < 0 || y + nx > (dim_columns game) = head [target | target <- targets game,
	 position target == (x, y)]
	| findObstacles (x + nx, y + ny) game && findGateway (x, y) game = Target (head (foldl (\acc var -> if (x, y) == fst var then (snd var) :
	 acc else if (x, y) == snd var then (fst var) : acc else acc) [] (gateways game))) (behavior (head [target | target <-
	  targets game, position target == (x, y)]))
	| findObstacles (x + nx, y + ny) game == True = head [target | target <- targets game, position target == (x, y)]
	| findTarget (x + nx, y + ny) game == True = head [target | target <- targets game, position target == (x, y)]
	| findHunter (x + nx, y + ny) game == True = head [target | target <- targets game, position target == (x, y)]
	| findGateway (x + nx, y + ny) game == True =  Target (head (foldl (\acc var -> if (x + nx, y + ny) == fst var then (snd var) :
	 acc else if (x + nx, y + ny) == snd var then (fst var) : acc else acc) [] (gateways game))) (behavior (head [target | target <-
	  targets game, position target == (x, y)]))
	| length [ target | target <- targets game, position target == (x, y)] == 0 = head [target | target <- targets game, position target == (x, y)]
	| length [ target | target <- targets game, position target == (x, y)] == 1 = Target (x + nx, y + ny) (behavior (head [target | target <- targets game, position target == (x, y)]))

goEast :: Behavior
goEast position game = go position game (0, 1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goWest :: Behavior
goWest position game = go position game (0, -1)


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goNorth :: Behavior
goNorth position game = go position game (-1, 0)


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goSouth :: Behavior
goSouth position game = go position game (1, 0)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud.
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce param pos game
	| param == 1 && findGateway pos game && findObstacles (fst (head (foldl (\acc var -> if (fst pos, snd pos) == fst var then (snd var) :
	 acc else if (fst pos, snd pos) == snd var then (fst var) : acc else acc) [] (gateways game))) + 1, snd (head (foldl (\acc var ->
		 if (fst pos, snd pos) == fst var then (snd var) : acc else if (fst pos, snd pos) == snd var then (fst var) : acc else acc)
		 [] (gateways game)))) game = Target (fst pos - 1, snd pos) (bounce (-1))
	| param == (-1) && findGateway pos game && findObstacles (fst (head (foldl (\acc var -> if (fst pos, snd pos) == fst var then (snd var) :
	 acc else if (fst pos, snd pos) == snd var then (fst var) : acc else acc) [] (gateways game))) - 1, snd (head (foldl (\acc var ->
		 if (fst pos, snd pos) == fst var then (snd var) : acc else if (fst pos, snd pos) == snd var then (fst var) : acc else acc)
		 [] (gateways game)))) game = Target (fst pos + 1, snd pos) (bounce 1)
	| param == 1 = if (position (goSouth pos game)) == pos then Target (position (goNorth pos game)) (bounce (-1)) else Target (position (goSouth pos game)) (bounce 1)
	| param == - 1 = if (position (goNorth pos game)) == pos then Target (position (goSouth pos game)) (bounce 1) else Target (position (goNorth pos game)) (bounce (-1))


{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.

-}
moveTargets :: Game -> Game
moveTargets game = game {targets = foldl (\acc x ->  (behavior x) (position x) game : acc) [] (targets game)}

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled pos target
	| fst pos == fst (position target) + 1 && snd pos == snd (position target) = True
	| fst pos == fst (position target) - 1 && snd pos == snd (position target) = True
	| fst pos == fst (position target) && snd pos == snd (position target) + 1 = True
	| fst pos == fst (position target) && snd pos == snd (position target) - 1 = True
	| otherwise = False

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.

    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
goHun :: Position -> Game -> Position -> Position
goHun (x, y) game (nx, ny)
	| x + nx < 0 || x + nx > (dim_lines game) || y + nx < 0 || y + nx > (dim_columns game) = hunter game
	| findObstacles (x + nx, y + ny) game == True = hunter game
	| findTarget (x + nx, y + ny) game == True = hunter game
	| findHunter (x + nx, y + ny) game == True = hunter game
	| findGateway (x + nx, y + ny) game == True =  (head (foldl (\acc var -> if (x + nx, y + ny) == fst var
		then (snd var) : acc else if (x + nx, y + ny) == snd var then (fst var) : acc else acc) [] (gateways game)))
	| otherwise = (x + nx, y + ny)

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bo game -- = game
	| bo == False && dir == North = game {hunter = goHun (hunter game) game (fst (hunter game) - 1, snd (hunter game))}
	| bo == False && dir == South = game {hunter = goHun (hunter game) game (fst (hunter game) + 1, snd (hunter game))}
	| bo == False && dir == West = game {hunter = goHun (hunter game) game (fst (hunter game), snd (hunter game) - 1)}
	| bo == False && dir == East = game {hunter = goHun (hunter game) game (fst (hunter game), snd (hunter game) + 1)}
	-- | bo == True && dir == North = moveTargets (game {hunter = goHun (hunter game) game (fst (hunter game) - 1, snd (hunter game)), targets = (foldl (\acc var -> ))})
	| otherwise = game


{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game
	| length (targets game) == 0 = False
	| otherwise = True

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***

        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors = undefined

    {-
        *** TODO ***

        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal  = undefined

    {-
        *** TODO ***

        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h = undefined

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică.
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
