main = interact $ show . solve . map read . words . head . drop 1 . lines


solve :: [Int] -> Int
solve xs =
    let
        left    = take (div (length xs) 2) xs
        right   = drop (div (length xs) 2) xs
        isSortedL = isSorted left
        isSortedR = isSorted right
        continueL = solve left
        continueR = solve right
    in
        case isSorted xs of
            False ->if (isSortedL || isSortedR)
                    then length left
                    else if (continueL > continueR)
                         then continueL
                         else continueR
            True -> length xs

isSorted :: (Ord a) => [a] -> Bool
isSorted []         = True
isSorted [x]        = True
isSorted (x:y:xs)   = x <= y && isSorted(y:xs)
