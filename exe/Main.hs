import Data.Set (fromList, toList)
import Data.Time (getCurrentTime, diffUTCTime)
import Debug.Trace (trace, traceIO)


charToInt :: Char -> Int
charToInt ch = read [ch]

lineToList :: String -> [Int]
lineToList = map charToInt

splitLines :: String -> [[Int]]
splitLines = map lineToList . lines

findRowNums :: [Int] -> [Int]
findRowNums col = filter (\x -> notElem x col) [1..9]

findColNums :: [[Int]] -> Int -> [Int]
findColNums board row =
    filter (\x -> notElem x rowList) [1..9]
    where rowList = map (\x -> (board !! x) !! row) [0..8]

slice :: [Int] -> Int -> Int -> [Int]
slice list idx sliceLength 
    | sliceLength == 0 || idx > length list = []
    | otherwise = (list !! idx) : slice list (idx+1) (sliceLength-1)

constructBoxList :: Int -> Int -> [[Int]] -> Int -> Int -> [Int]
constructBoxList count maxCount board col row =
    if count == maxCount
        then []
    else filter (/= 0) (slice (board !! col) row 3) <> constructBoxList (count+1) maxCount board (col+1) row

constructBoxList' :: [[Int]] -> Int -> Int -> [Int]
constructBoxList' = constructBoxList 0 3

findBoxNums :: [[Int]] -> Int -> Int -> [Int]
findBoxNums board col row =
    let boxCol = (col `div` 3) * 3 in
    let boxRow = (row `div` 3) * 3 in
    let allBoxNums = constructBoxList' board boxCol boxRow in
    filter (\x -> notElem x (allBoxNums)) [1..9]

clearDuplicates :: [Int] -> [Int]
clearDuplicates l = toList (fromList l)

findNums :: [[Int]] -> Int -> Int -> [Int]
findNums board col row =
    let colNums = findColNums board row in
    let rowNums = findRowNums (board !! col) in
    let boxNums = findBoxNums board col row in
    clearDuplicates
        (filter (\x -> elem x colNums && elem x rowNums && elem x boxNums)
        (colNums <> rowNums <> boxNums))

alterBoard :: [[Int]] -> Int -> Int -> Int -> [[Int]]
alterBoard board c r n =
    zipWith (\column i -> if i == c then 
            zipWith (\row j -> if j == r then n else row)
            (board !! i) [0..8] else column) board [0..8]

findSolvedBoard :: Int -> [Int] -> [[Int]] -> Int -> Int -> [[Int]]
findSolvedBoard index numbers board col row
    | index >= length numbers = []
    | otherwise =
        let newBoard = alterBoard board col row (numbers !! index) in
        let result = solve col (row+1) newBoard in
        if result /= []
            then result
        else findSolvedBoard (index+1) numbers board col row

findSolvedBoard' :: [Int] -> [[Int]] -> Int -> Int -> [[Int]]
findSolvedBoard' = findSolvedBoard 0

solve :: Int -> Int -> [[Int]] -> [[Int]]
solve c r board
    | c >= 8 && r >= 9 = board
    | c < 8 && r >= 9 = solve (c+1) 0 board
    | (board !! c) !! r /= 0 = solve c (r+1) board
    | otherwise =
        let permittedNumbers = findNums board c r in
        findSolvedBoard' permittedNumbers board c r

solve' :: [[Int]] -> [[Int]]
solve' = solve 0 0
        
main :: IO ()
main = do
    file <- readFile "games.txt"
    let contents = splitLines file
    print (solve' contents)