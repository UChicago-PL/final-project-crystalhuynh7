module Main where

import Data.Char (toLower, isSpace)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

-- PART A: Data types (world + state)

data Direction = North | South | East | West
    deriving (Eq, Ord, Show)

type RoomId = String
type ItemId = String

data Room = Room
    { roomName        :: String
    , roomDescription :: String
    , roomExits       :: Map Direction RoomId
    , roomItems       :: [ItemId]
    } deriving (Show)

data GameState = GameState
    { gsRooms            :: Map RoomId Room
    , gsPlayerLoc        :: RoomId
    , gsInventory        :: [ItemId]
    , gsMoves            :: Int
    , gsFlashlightOn     :: Bool
    , gsDoorUnlocked     :: Bool
    , gsKnobTurned       :: Bool
    , gsTreasureRevealed :: Bool
    , gsIsOver           :: Bool
    } deriving (Show)

-- PART B: Commands + parsing

data Command
    = CmdLook
    | CmdGo Direction
    | CmdTake ItemId
    | CmdDrop ItemId
    | CmdRead ItemId
    | CmdUse ItemId
    | CmdInv
    | CmdMoves
    | CmdTurn String
    | CmdHelp
    | CmdQuit
    | CmdUnknown String
    deriving (Show, Eq)
    
parseCommand :: String -> Command
parseCommand raw =
    case words (normalizeInput raw) of
        [] -> CmdUnknown ""
        ["look"]           -> CmdLook
        ["l"]              -> CmdLook
        ["inventory"]      -> CmdInv
        ["inv"]            -> CmdInv
        ["i"]              -> CmdInv
        ["help"]           -> CmdHelp
        ["quit"]           -> CmdQuit
        ["q"]              -> CmdQuit
        ["moves"]          -> CmdMoves
        ["use", x]         -> CmdUse x
        ["turn", x]        -> CmdTurn x

        ["go", d]          -> maybe (CmdUnknown raw) CmdGo (parseDir d)
        [d]                -> maybe (CmdUnknown raw) CmdGo (parseDir d)  -- allow "north"
        ["take", x]        -> CmdTake x
        ["get", x]         -> CmdTake x      -- synonym
        ["grab", x]        -> CmdTake x      -- synonym
        ["drop", x]        -> CmdDrop x
        ["read", x]        -> CmdRead x
        ["examine", x]     -> CmdRead x     -- synonym
        ["pick","up",x]    -> CmdTake x

        _                  -> CmdUnknown raw

parseDir :: String -> Maybe Direction
parseDir s =
    case s of
        "north" -> Just North
        "n"     -> Just North
        "south" -> Just South
        "s"     -> Just South
        "east"  -> Just East
        "e"     -> Just East
        "west"  -> Just West
        "w"     -> Just West
        _       -> Nothing

normalizeInput :: String -> String
normalizeInput = map toLower . trim

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse


-- PART C: World definition 

initialState :: GameState
initialState =
    let rooms =
            Map.fromList
            [ ("foyer", Room
                { roomName = "Foyer"
                , roomDescription = "A small foyer. The air smells old. There is a door to the north."
                , roomExits = Map.fromList [(North, "hall")]
                , roomItems = ["note"]
                })

            , ("hall", Room
                { roomName = "Hallway"
                , roomDescription = "A narrow hallway with creaky floorboards. Exits lead south, east, and west."
                , roomExits = Map.fromList [(South, "foyer"), (East, "study"), (West, "basement")]
                , roomItems = []
                })

            , ("study", Room
                { roomName = "Study"
                , roomDescription = "A dusty study. A heavy locked door is to the north. West goes back."
                , roomExits = Map.fromList [(West, "hall"), (North, "vault")]
                , roomItems = ["flashlight"]
                })

            , ("vault", Room
                { roomName = "Vault"
                , roomDescription = "A tiny vault room."
                , roomExits = Map.fromList [(South, "study")]
                , roomItems = []
                })
            
            , ("basement", Room
                { roomName = "Basement"
                , roomDescription = "A dark basement with stone walls. Something glints faintly in the darkness."
                , roomExits = Map.fromList [(East, "hall")]
                , roomItems = ["key"]
                })
            ]
            
    in GameState
        { gsRooms = rooms
        , gsPlayerLoc = "foyer"
        , gsInventory = []
        , gsMoves = 0
        , gsFlashlightOn = False
        , gsDoorUnlocked = False
        , gsKnobTurned = False
        , gsTreasureRevealed = False
        , gsIsOver = False
        }

-- PART D: Game logic

step :: GameState -> Command -> (GameState, [String])
step gs cmd
    | gsIsOver gs = (gs, ["Game is over. Type quit to exit."])
    | otherwise   =
        case cmd of
            CmdHelp ->
                (gs, helpText)

            CmdLook ->
                (gs, describeCurrentRoom gs)

            CmdInv ->
                (gs, describeInventory gs)

            CmdQuit ->
                (gs { gsIsOver = True }, ["bye"])

            CmdMoves ->
                (gs, ["moves: " ++ show (gsMoves gs)])

            CmdGo dir ->
                goDirection (addMove gs) dir

            CmdTake item ->
                takeItem (addMove gs) item

            CmdDrop item ->
                dropItem (addMove gs) item

            CmdRead item ->
                readItem (addMove gs) item

            CmdUse item ->
                useItem (addMove gs) item

            CmdTurn thing ->
                turnThing (addMove gs) thing

            CmdUnknown s ->
                (gs, ["i don't understand: " ++ show s, "type help for commands."])

addMove :: GameState -> GameState
addMove gs = gs { gsMoves = gsMoves gs + 1 }

helpText :: [String]
helpText =
    [ "commands:"
    , "  look | l"
    , "  go north/south/east/west (or just n/s/e/w)"
    , "  take X (or get/grab X)"
    , "  drop X"
    , "  read X"
    , "  inventory | inv | i"
    , "  moves"
    , "  use X"
    , "  turn X"
    , "  help"
    , "  quit | q"
    , ""
    ]

describeCurrentRoom :: GameState -> [String]
describeCurrentRoom gs =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> ["(bug) you are in a room that doesn't exist."]
        Just r  ->
            if gsPlayerLoc gs == "basement" && not (gsFlashlightOn gs)
                then
                    [ roomName r
                    , replicate (length (roomName r)) '-'
                    , "It is too dark to see anything clearly."
                    , exitsLine (roomExits r)
                    , "Items: none"
                    , ""
                    ]
            else
                let desc =
                        if gsPlayerLoc gs == "basement"
                        then
                            if "key" `elem` roomItems r
                            then "A dark basement with stone walls. Something glints faintly in the darkness."
                            else "A dark basement with stone walls. The floor is dusty where something once lay."
                        else roomDescription r
                in
                    [ roomName r
                    , replicate (length (roomName r)) '-'
                    , desc
                    , exitsLine (roomExits r)
                    , itemsLine (roomItems r)
                    , ""
                    ]

exitsLine :: Map Direction RoomId -> String
exitsLine exits =
    if Map.null exits
        then "Exits: none"
        else "Exits: " ++ intercalate ", " (map showDir (Map.keys exits))

showDir :: Direction -> String
showDir d =
    case d of
        North -> "north"
        South -> "south"
        East  -> "east"
        West  -> "west"

itemsLine :: [ItemId] -> String
itemsLine xs =
    if null xs
        then "Items: none"
        else "Items: " ++ intercalate ", " xs

describeInventory :: GameState -> [String]
describeInventory gs =
    if null (gsInventory gs)
        then ["inventory: empty"]
        else ["inventory: " ++ intercalate ", " (gsInventory gs)]

goDirection :: GameState -> Direction -> (GameState, [String])
goDirection gs dir =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> (gs, ["(bug) current room missing"])
        Just r  -> case Map.lookup dir (roomExits r) of
            Nothing -> (gs, ["you can't go that way."])
            Just nextId ->
                if nextId == "vault" && not (gsDoorUnlocked gs)
                    then (gs, ["the door to the north is locked. maybe there's a key somewhere."])
                else
                    let gs' = gs { gsPlayerLoc = nextId }
                    in (gs', ["you walk " ++ showDir dir ++ ".",""] ++ describeCurrentRoom gs')

takeItem :: GameState -> ItemId -> (GameState, [String])
takeItem gs item =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> (gs, ["(bug) current room missing"])
        Just r  ->
            if gsPlayerLoc gs == "basement" && item == "key" && not (gsFlashlightOn gs)
                then (gs, ["it is too dark to find the key. maybe you need to use the flashlight."])
            else if item `elem` roomItems r
            then
            let r'   = r { roomItems = removeOnce item (roomItems r) }
                gs'  = gs
                        { gsRooms = Map.insert (gsPlayerLoc gs) r' (gsRooms gs)
                        , gsInventory = item : gsInventory gs
                        }
                msg  = ["took " ++ item ++ ". " ++ describeItem item]
                win =
                    if item == "treasure"
                    then victoryScreen (gsMoves gs')
                    else []
                gs'' = if item == "treasure" then gs' { gsIsOver = True } else gs'
            in (gs'', msg ++ win)
            else
            (gs, ["that item isn't here."])

useItem :: GameState -> ItemId -> (GameState, [String])
useItem gs item
    | item == "flashlight" =
        if item `elem` gsInventory gs
            then
                let gs' = gs { gsFlashlightOn = True }
                in (gs', ["you switch on the flashlight."])
            else
                (gs, ["you don't have a flashlight."])

    | item == "key" =
        if gsPlayerLoc gs /= "study"
            then (gs, ["there is nothing here to unlock with the key."])
        else if item `notElem` gsInventory gs
            then (gs, ["you don't have the key."])
        else if gsDoorUnlocked gs
            then (gs, ["the door is already unlocked."])
        else
            let gs' = gs { gsDoorUnlocked = True }
            in (gs', ["you unlock the heavy door to the north. turn the knob to enter."])

    | otherwise =
        (gs, ["you can't use that here."])

turnThing :: GameState -> String -> (GameState, [String])
turnThing gs thing
    | thing /= "knob" =
        (gs, ["you can't turn that."])
    | gsPlayerLoc gs /= "study" =
        (gs, ["there is no knob here."])
    | not (gsDoorUnlocked gs) =
        (gs, ["the knob does not move. maybe the door needs to be unlocked first."])
    | gsKnobTurned gs =
        (gs, ["the knob has already been turned."])
    | otherwise =
        let gs' = revealTreasure gs
        in (gs', ["you turn the knob. a hidden panel shifts open in the vault to the north. you should go north to inspect it."])

revealTreasure :: GameState -> GameState
revealTreasure gs =
    case Map.lookup "vault" (gsRooms gs) of
        Nothing -> gs
        Just vaultRoom ->
            let vaultRoom' = vaultRoom { roomItems = ["treasure"] }
            in gs
                { gsRooms = Map.insert "vault" vaultRoom' (gsRooms gs)
                , gsKnobTurned = True
                , gsTreasureRevealed = True
                }

dropItem :: GameState -> ItemId -> (GameState, [String])
dropItem gs item =
    if item `elem` gsInventory gs
        then case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> (gs, ["(bug) current room missing"])
        Just r  ->
            let r'  = r { roomItems = item : roomItems r }
                gs' = gs
                    { gsRooms = Map.insert (gsPlayerLoc gs) r' (gsRooms gs)
                    , gsInventory = removeOnce item (gsInventory gs)
                    }
            in (gs', ["dropped " ++ item ++ "."])
        else
        (gs, ["you don't have that."])

readItem :: GameState -> ItemId -> (GameState, [String])
readItem gs item
    | item == "note" && (item `elem` gsInventory gs || noteInRoom gs) =
        (gs, ["The note reads: 'The treasure is sealed behind a locked vault. Find the key, unlock the door, and turn the knob.'"])
    | otherwise =
        (gs, ["there's nothing interesting to read."])

noteInRoom :: GameState -> Bool
noteInRoom gs =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> False
        Just r  -> "note" `elem` roomItems r

describeItem :: ItemId -> String
describeItem "note" = "A worn piece of paper."
describeItem "flashlight" = "A small flashlight. It might help in dark places."
describeItem "key" = "A small brass key."
describeItem "treasure" = "A glittering pile of treasure."
describeItem _ = "Something mysterious."

removeOnce :: Eq a => a -> [a] -> [a]
removeOnce _ [] = []
removeOnce x (y:ys)
  | x == y    = ys
  | otherwise = y : removeOnce x ys

currentRoomName :: GameState -> String
currentRoomName gs =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> "Unknown"
        Just r  -> roomName r

victoryScreen :: Int -> [String]
victoryScreen moves =
    [ ""
    , "================================="
    , "        TREASURE FOUND"
    , "================================="
    , ""
    , "You open the box and discover a glittering"
    , "pile of treasure."
    , ""
    , "Congratulations — you escaped the house!"
    , ""
    , "Total moves: " ++ show moves
    , ""
    ]

prompt :: GameState -> String
prompt gs =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> "> "
        Just r  -> "[" ++ roomName r ++ " | moves: " ++ show (gsMoves gs) ++ "] > "

-- PART E: IO 

main :: IO ()
main = do
    putStrLn ""
    putStrLn "==========================================="
    putStrLn "        THE HOUSE OF THE HIDDEN VAULT"
    putStrLn "        (a tiny zork-inspired game)"
    putStrLn "==========================================="
    putStrLn ""
    putStrLn "You stand inside an old abandoned house."
    putStrLn "Somewhere inside, a treasure has been hidden."
    putStrLn ""
    putStrLn "Explore the rooms, collect items, and solve the puzzle."
    putStrLn ""
    putStrLn "Type commands to play."
    putStrLn "Examples:  look, north, take key, read note"
    putStrLn "Type 'help' to see all commands."
    putStrLn ""
    putStrLn "-------------------------------------------"
    putStrLn ""

    loop initialState

loop :: GameState -> IO ()
loop gs = do
    if gsIsOver gs
        then putStrLn "(game over)"
        else pure ()

    putStr (prompt gs)
    line <- getLine
    let cmd = parseCommand line
        (gs', outs) = step gs cmd

    mapM_ putStrLn outs
    putStrLn ""

    case cmd of
        CmdQuit -> pure ()
        _       -> loop gs'