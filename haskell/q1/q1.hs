readLine :: String -> Integer
readLine ('-' : xs) = -1 * read xs
readLine ('+' : xs) = read xs

main :: IO ()
main = do
    listOfStrings <- lines <$> readFile "input.txt"
    print (sum (map readLine listOfStrings))
