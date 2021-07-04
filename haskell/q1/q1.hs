readLine :: String -> Integer
readLine ('-' : xs) = -1 * read xs
readLine ('+' : xs) = read xs

headInTail :: [Integer] -> Bool
headInTail [] = False
headInTail (x : xs) = x `elem` xs

appendIncrement :: [Integer] -> Integer -> [Integer]
appendIncrement [] y = [y]
appendIncrement (x : xs) y = (x + y) : x : xs

findFirstDup = dropWhile (not . headInTail)

genIncResults = scanl appendIncrement []

readToCycledList = cycle . map readLine

main :: IO ()
main = do
    listOfStrings <- lines <$> readFile "input.txt"
    (print . sum . (map readLine)) listOfStrings
    (print . head . head . findFirstDup . genIncResults . readToCycledList) listOfStrings
    -- print (head (head (dropWhile (not . headInTail) (scanl appendIncrement [] (cycle (map readLine listOfStrings))))))
