import Data.Char(digitToInt)
import Data.List(intersperse, intercalate)
import Data.Maybe(fromJust)

data Square = X | O | Empty deriving Eq
instance Show Square where
    show X = "X"
    show O = "O"
    show Empty = " "

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Board = [[Square]]

row :: Board -> Int -> [Square]
row board n = board !! n

rows :: Board -> Board
rows board = board

col :: Board -> Int -> [Square]
col board n = [board !! 0 !! n, board !! 1 !! n, board !! 2 !! n]

cols :: Board -> Board
cols board = [col board 0, col board 1, col board 2]

leftDiagonal :: Board -> [Square]
leftDiagonal board = [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2]

rightDiagonal :: Board -> [Square]
rightDiagonal board = [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]

groupings :: Board -> [[Square]]
groupings board = (rows board) ++ (cols board) ++
                    [leftDiagonal board, rightDiagonal board]

getWinner :: [Square] -> Maybe Player
getWinner squares | (length $ filter (== X) squares) == 3 = Just PlayerX
                  | (length $ filter (== O) squares) == 3 = Just PlayerO
                  | otherwise = Nothing

getGameWinner :: Board -> Maybe Player
getGameWinner board =
    let winners = filter (/= Nothing) $ map getWinner $ groupings board
        in case length winners of
            0 -> Nothing
            _ -> winners !! 0

isDraw :: Board -> Bool
isDraw board | getGameWinner board == Nothing = null $ filter (== Empty) $
                                                    concat board
             | otherwise = False

coordFromStr :: String -> Maybe (Int, Int)
coordFromStr str =
    let strs = map (!! 0) $ words str
        in case length strs of
            2 -> Just (digitToInt (strs !! 0), digitToInt (strs !! 1))
            _ -> Nothing

doMove :: Board -> Player -> (Int, Int) -> Maybe Board
doMove board player (row, col) | row < 0 || row > 2 = Nothing
                               | col < 0 || col > 2 = Nothing
                               | board !! row !! col /= Empty = Nothing
                               | otherwise = Just (newBoard board player
                                                    (row, col))

newBoard :: Board -> Player -> (Int, Int) -> Board
newBoard board player (row, col) =
    let myNewRow = newRow (board !! row) player col
        in take row board ++ [myNewRow] ++ drop (row + 1) board

newRow :: [Square] -> Player -> Int -> [Square]
newRow row player col =
    let newSquare = playerToSquare player
        in take col row ++ [newSquare] ++ drop (col + 1) row

playerToSquare :: Player -> Square
playerToSquare PlayerX = X
playerToSquare PlayerO = O

winnerMessage :: Player -> String
winnerMessage winner = show winner ++ " wins!"

movePromptMessage :: Player -> String
movePromptMessage player = "\n" ++ show player ++ " what is your move?"

nextPlayer :: Player -> Player
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

printBoard :: Board -> IO ()
printBoard board = do
    let boardStr = intersperse "---------" $ map rowToStr board
    putStrLn $ intercalate "\n" boardStr

endWithWin :: Board -> Player -> IO ()
endWithWin board player = do
    putStrLn $ "\n" ++ show player ++ " won!"
    printBoard board

endWithDraw :: Board -> IO ()
endWithDraw board = do
    putStrLn $ "\n" ++ "Game ended in a draw."
    printBoard board

rowToStr :: [Square] -> String
rowToStr row = intercalate " | " $ map show row

takeGameStep :: Board -> Player -> IO ()
takeGameStep board player | winner /= Nothing = endWithWin board player
                          | isDraw board = endWithDraw board
                          | otherwise = do
                            putStrLn $ movePromptMessage player
                            printBoard board
                            coordStr <- getLine
                            let coord = coordFromStr coordStr
                            if (coord == Nothing) then do
                                putStrLn "Invalid move!"
                                takeGameStep board player
                            else do
                                let nextBoard = doMove board player $
                                        fromJust coord
                                if (nextBoard == Nothing) then do
                                    putStrLn "Invalid move!"
                                    takeGameStep board player
                                else do
                                    takeGameStep (fromJust nextBoard) $
                                        nextPlayer player
                          where winner = getGameWinner board

main :: IO ()
main = do
    putStrLn "Welcome to Tic-Tac-Toe!"
    putStrLn "To play, enter your move in the form \"row, column\""
    putStrLn "Squares are numbered starting from 0 at the top left corner"
    let startingBoard = [[Empty, Empty, Empty],
                         [Empty, Empty, Empty],
                         [Empty, Empty, Empty]]
    takeGameStep startingBoard PlayerX
