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
    { gsRooms     :: Map RoomId Room
    , gsPlayerLoc :: RoomId
    , gsInventory :: [ItemId]
    , gsMoves     :: Int
    , gsIsOver    :: Bool
    } deriving (Show)

-- PART B: Commands + parsing

data Command
    = CmdLook
    | CmdGo Direction
    | CmdTake ItemId
    | CmdDrop ItemId
    | CmdInv
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

        ["go", d]          -> maybe (CmdUnknown raw) CmdGo (parseDir d)
        [d]                -> maybe (CmdUnknown raw) CmdGo (parseDir d)  -- allow "north"
        ["take", x]        -> CmdTake x
        ["get", x]         -> CmdTake x      -- synonym
        ["grab", x]        -> CmdTake x      -- synonym
        ["drop", x]        -> CmdDrop x

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


-- PART C: World definition (4–6 rooms for now)

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
                , roomDescription = "A narrow hallway with creaky floorboards. Exits lead south and east."
                , roomExits = Map.fromList [(South, "foyer"), (East, "study")]
                , roomItems = []
                })

            , ("study", Room
                { roomName = "Study"
                , roomDescription = "A dusty study. A locked-looking door is to the north. West goes back."
                , roomExits = Map.fromList [(West, "hall"), (North, "vault")]
                , roomItems = ["key"]
                })

            , ("vault", Room
                { roomName = "Vault"
                , roomDescription = "A tiny vault room. Something feels final about being here."
                , roomExits = Map.fromList [(South, "study")]
                , roomItems = ["treasure"]
                })
            ]
    in GameState
        { gsRooms = rooms
        , gsPlayerLoc = "foyer"
        , gsInventory = []
        , gsMoves = 0
        , gsIsOver = False
        }

-- PART D: Game logic

step :: GameState -> Command -> (GameState, [String])
step gs cmd
    | gsIsOver gs = (gs, ["Game is over. Type quit to exit."])
    | otherwise   =
        let gs' = gs { gsMoves = gsMoves gs + 1 }
        in case cmd of
            CmdHelp ->
                (gs', helpText)

            CmdLook ->
                (gs', describeCurrentRoom gs')

            CmdInv ->
                (gs', describeInventory gs')

            CmdQuit ->
                (gs' { gsIsOver = True }, ["bye"])

            CmdGo dir ->
                goDirection gs' dir

            CmdTake item ->
                takeItem gs' item

            CmdDrop item ->
                dropItem gs' item

            CmdUnknown s ->
                (gs', ["i don't understand: " ++ show s, "type help for commands."])

helpText :: [String]
helpText =
    [ "commands:"
    , "  look | l"
    , "  go north/south/east/west (or just n/s/e/w)"
    , "  take X (or get/grab X)"
    , "  drop X"
    , "  inventory | inv | i"
    , "  help"
    , "  quit | q"
    ]

describeCurrentRoom :: GameState -> [String]
describeCurrentRoom gs =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> ["(bug) you are in a room that doesn't exist."]
        Just r  ->
            [ roomName r
            , roomDescription r
            , exitsLine (roomExits r)
            , itemsLine (roomItems r)
            ]

exitsLine :: Map Direction RoomId -> String
exitsLine exits =
    if Map.null exits
        then "exits: none"
        else "exits: " ++ intercalate ", " (map showDir (Map.keys exits))

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
        then "items: none"
        else "items: " ++ intercalate ", " xs

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
            -- Ex. “medium goal” hook: lock the vault unless you have the key.
            Just nextId -> 
                if nextId == "vault" && not ("key" `elem` gsInventory gs)
                then (gs, ["the door won't budge. maybe there's a key?"])
                else
                let gs' = gs { gsPlayerLoc = nextId }
                in (gs', describeCurrentRoom gs')

takeItem :: GameState -> ItemId -> (GameState, [String])
takeItem gs item =
    case Map.lookup (gsPlayerLoc gs) (gsRooms gs) of
        Nothing -> (gs, ["(bug) current room missing"])
        Just r  -> 
            if item `elem` roomItems r
            then
            let r'   = r { roomItems = removeOnce item (roomItems r) }
                gs'  = gs
                        { gsRooms = Map.insert (gsPlayerLoc gs) r' (gsRooms gs)
                        , gsInventory = item : gsInventory gs
                        }
                msg  = ["took " ++ item ++ "."]
                win  = if item == "treasure" then ["you found the treasure. you win!"] else []
                gs'' = if item == "treasure" then gs' { gsIsOver = True } else gs'
            in (gs'', msg ++ win)
            else
            (gs, ["that item isn't here."])

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

removeOnce :: Eq a => a -> [a] -> [a]
removeOnce _ [] = []
removeOnce x (y:ys)
  | x == y    = ys
  | otherwise = y : removeOnce x ys

-- PART E: IO 

main :: IO ()
main = do
    putStrLn "text adventure (tiny zork-inspired thing)"
    putStrLn "type help for commands.\n"
    loop initialState

loop :: GameState -> IO ()
loop gs = do
    -- If we just ended the game, still allow quit.
    if gsIsOver gs
        then putStrLn "(game over)"
        else pure ()

    putStr "> "
    line <- getLine
    let cmd = parseCommand line
        (gs', outs) = step gs cmd

    mapM_ putStrLn outs

    if cmd == CmdQuit
        then pure ()
        else loop gs'