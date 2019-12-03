import Data.List
main = interact $ show . solve

-- UNSOLVED AT TEST 55

solve xs =
    let
        xs' = filter (\x -> isSubsequenceOf [x] "nineteen") xs
        occurences = (map (\x -> length x) (group $ sort xs'))
        tup = zip (map (take 1) (group $ sort xs')) occurences
        occurs = map (checkEach) tup
    in
        minimum occurs

checkEach p = 
    case fst p of
        "n" -> (div ((snd p) - 1) 2)
        "i" -> snd p
        "e" -> (div (snd p) 3)
        "t" -> snd p
