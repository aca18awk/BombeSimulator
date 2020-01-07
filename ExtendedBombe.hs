{--
    Solution to COM2018 Assignment 3, written by Aleksandra Kulbaka, December 2019
--}

module ExtendedBombe where
    import AssignmentHelp
    import Enigma
    import Data.Char 
    import Data.List

    type SteckerPair = (Char, Char)

    {--If the solution exists, it returns valid steckerboard, offset
       and % of certainity of correctness of this solution --}
    breakEnigma :: Crib -> Maybe (Offsets, Steckerboard, Int)
    breakEnigma (plain, code) = breakEA (plain, code) (first:restMenu) firstPair (0,0,0)
            where 
                firstPair = [(plain !! (fromIntegral first), 'A')]
                (first:restMenu) = longestMenu (plain, code)

    {--It checks all possible steckerboards for all possible offsets.
       Given the initial offsets and initial pair, it recursively tries to find
       steckerboard by calling findStecker, which checks all possible steckers
       for a given offsets. If it returns Nothing, breakEA advances the offsets
       until steckerboard if found or there are no more combinations of offsets
        to check--}                
    breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard, Int)
    breakEA crib menu pair offsets 
        | (nothingP possibleStecker) == False
                              = Just (offsets, stecker, percent) 
        | (nothingP possibleStecker && offsets /= (25, 25, 25))
                               = breakEA crib menu pair (moveOffsets 1 offsets)
        | otherwise = Nothing
            where 
                possibleStecker = (findStecker crib menu pair offsets)
                (stecker,percent) = fromMaybe possibleStecker

    {-- It checks all possible steckerboards for a given offset.
        If steckerboard if found, it additionally validates it by calling isCorrect
        If steckerboard is valid, it returns it without pairs like (a,a) and with 
        corresponding % of correctness of this solution --}
    findStecker :: Crib -> Menu -> Steckerboard ->  Offsets -> Maybe (Steckerboard, Int)
    findStecker crib menu [(p,x)] offsets 
        | (nothingP (thisStecker) == False && nothingP (finalStecker) == False) 
                    = Just (filter (\(a,b) -> a /= b) stecker, percent)   
        | ((nothingP (thisStecker) || nothingP (finalStecker)) && x /='Z') 
                                = findStecker crib menu nextPair offsets
        | otherwise = Nothing 
            where
                thisStecker = followMenu crib menu [(p,x)] offsets
                nextPair = [(p, posToChar (alphaPos x + 1))]
                finalStecker = isCorrect crib offsets (fromMaybe thisStecker)
                (stecker, percent) = fromMaybe finalStecker


    {-- Given the possible solution, it additionally checks for contradictions.
        It checks for all instances of letters from steckerboard in both
        plain text and cipher and checks for contradictions by calling followMenu.
     --}
    isCorrect :: Crib -> Offsets -> Steckerboard -> Maybe (Steckerboard, Int)
    isCorrect (plain, cipher) offsets stecker 
        | plainStecker /= Nothing && cipherStecker /= Nothing
            = Just (fromMaybe cipherStecker, p)
        | otherwise = Nothing
            where
                plainlist = [ toInteger n | (a,n) <- zip plain [0..],
                      (x,y) <-stecker, (a==x || a==y)]
                cipherlist = [ toInteger n | (b,a,n) <- zip3 plain cipher [0..],
                      (x,y) <-stecker, (a==x || a==y)]
                plainStecker = followMenu (plain, cipher) plainlist stecker offsets
                -- we can swap the order because if y is encoded to x, x is always encoded to y
                cipherStecker = followMenu (cipher, plain) cipherlist 
                                      (fromMaybe plainStecker) offsets
                p = percent (length (unique (concat [plainlist, cipherlist]))) (length plain) 

    --It return list without duplicated values
    unique :: [Integer] -> [Integer]
    unique [] = []
    unique (x:xs) | elem x xs = unique xs
                  | otherwise = x: unique xs

    {- For a given offset, menu and initial steckerboard, it recursively tries to add
       new pair to the existing steckerboard which is encoded letter from plain text
       at current index from the menu and letter from cipher at the same index 
    --}
    followMenu :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
    followMenu (plain, cipher) [] steckerboard offsets = Just steckerboard 
    followMenu (plain, cipher) (first:rest) steckerboard offsets 
        | nothingP (newStecker) = Nothing
        | otherwise = followMenu (plain, cipher) rest (fromMaybe newStecker) offsets
            where 
                newStecker = steckerAdd 
                             (encodedLetter, cipher !! (fromIntegral first)) steckerboard
                encodedLetter = enigmaEncode letter 
                                (SimpleEnigma rotor1 rotor2 rotor3 reflectorB newOffsets)
                letter = swap steckerboard (plain !! (fromIntegral first))
                newOffsets = moveOffsets (fromIntegral first) offsets
    
    -- Returns initial offsets moved by given number       
    moveOffsets :: Int -> Offsets -> Offsets
    moveOffsets n (lo, mo, ro) = (newlo, newmo, newro)
            where
                newro = (ro + n) `mod` 26
                newmo = (mo + ((ro + n) `div` 26)) `mod` 26
                newlo = (lo + ((mo + (ro + n) `div` 26) `div` 26)) `mod` 26

    {-- If SteckerPair is unique, it return new SteckerBoard with that pair.
        if the SteckerPair is already in the Stecker, it returns existing stecker
        and if there is a contradiction, it returns Nothing --}   
    steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
    steckerAdd (a,b) stecker 
        | filteredList == [] = Just (concat [stecker, [(a,b)]])
        | filter (\(x,y) -> ((x==a && y==b) || (x==b && y==a))) filteredList /= [] = Just stecker
        | otherwise = Nothing
            where 
                filteredList = filter (\(x,y) -> (x==a || x==b || y==a || y==b)) stecker

{--
    Testing code on the examples from bombeTesting16:
        1)  p1 = "AIDEGHMC"
            x1 = "TTCMAANO"
            Just ((0,0,0),[('D','A'),('Z','C'),('K','O'),('M','T'),('I','G'),('V','H')],62)
            (0.02 secs, 394,616 bytes)

        2)  p2 = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
            x2 = "NAWLGAFTKDBKDLIKEOSZVAOKXKXFMQQUXBJDRCXRZFDKHLWCNBDJSMJBMJQCBBGJV"
            Just ((0,0,5),[('P','M'),('E','C'),('O','L'),('H','R'),('G','T'),('S','D'),('F','K'),('U','A'),('B','V')],84)
            (0.10 secs, 18,336,976 bytes)
 
        3)  ht = "TURINGBOMBEHASKELLSIMULATIONSTOP"
            x3 = "LKFMTWMTVKDEIVXHFHMNFDAZDRLMYQFRCKHHQSIMPIBZSXSCNMXVEKLXYRLEKZ"
            Nothing
            (245.17 secs, 38,382,514,592 bytes)

        
    Testing code on the examples from Blackboard:
        4)  p = "COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE"
            x = "QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW"
            Just ((0,0,0),[('R','M'),('G','T'),('H','S'),('U','E'),('X','A'),('W','I'),('Q','C'),('L','P'),('D','V')],96)
            (0.58 secs, 147,851,472 bytes)

        5)  p4 = "AFJEQTMC"
            x4 = "FJEQTMCF"
            Just ((0,0,0),[('B','F'),('D','E'),('L','Q'),('U','T'),('P','M'),('I','C')],100)
            (0.01 secs, 1,172,840 bytes)

        6)  p5 = "ZGXWAUTS"
            x5 = "XKGZWAUT"
            Just ((0,0,0),[('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')],100)
            (0.02 secs, 2,333,976 bytes)

        7)  p6="TURINGBOMBEHASKELLSIMULATIONSTOP"
            x6="FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQ"
            Just ((1,2,14),[('S','X'),('B','E'),('N','A'),('J','M'),('C','H'),('V','Y')],78)
            (5.21 secs, 768,491,040 bytes)
        
        8)  p7 = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
            x7 = "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW"
            Just ((4,3,7),[('K','C'),('N','E'),('O','M'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')],95)
            (33.92 secs, 5,197,431,360 bytes)
--}