{--
    Solution to COM2018 Assignment 3, written by Aleksandra Kulbaka, December 2019
--}

module Bombe where
    import AssignmentHelp
    import Enigma
    import Data.Char 
    import Data.List

    type SteckerPair = (Char, Char)

    {--If the solution exists, 
       It returns valid steckerboard and offset --}
    breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
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
    breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
    breakEA crib menu pair offsets 
        | (nothingP possibleStecker) == False
                              = Just (offsets, fromMaybe possibleStecker) 
        | (nothingP possibleStecker && offsets /= (25, 25, 25))
                               = breakEA crib menu pair (moveOffsets 1 offsets)
        | otherwise = Nothing
            where 
                possibleStecker = (findStecker crib menu pair offsets)

    {-- It checks all possible steckerboards for a given offset.
        Given the offset, menu and first initial pair [(x,('A'))], it tries to
        find steckerboard. If current initial pair fails, it tries the next
        assumption from set ([(x,'B')], [(x,'C')],..., [(x,'Z')]) until
        steckerboard if found or there are no more assumptions to check.
        If steckerboard is found, it returns it without pairs like (x,x) --}
    findStecker :: Crib -> Menu -> Steckerboard ->  Offsets -> Maybe Steckerboard
    findStecker crib menu [(p,x)] offsets 
        | (nothingP (thisStecker) == False) =
                     Just (filter (\(a,b) -> a /= b) (fromMaybe thisStecker))   
        | (nothingP (thisStecker) && x /='Z') =
                     findStecker crib menu nextPair offsets
        | otherwise = Nothing 
            where
                thisStecker = followMenu crib menu [(p,x)] offsets
                nextPair = [(p, posToChar (alphaPos x + 1))]

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
                -- in order to work, offsets need to be moved by a current index from the menu
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
            Just ((0,0,0),[('K','O'),('Z','C'),('D','A')])
        
        2)  p2 = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
            x2 = "NAWLGAFTKDBKDLIKEOSZVAOKXKXFMQQUXBJDRCXRZFDKHLWCNBDJSMJBMJQCBBGJV"
            Just ((0,0,0),[('Q','K'),('G','D'),('H','T'),('V','R'),('S','L'),('N','C'),('P','A')])
        
        3)  ht = "TURINGBOMBEHASKELLSIMULATIONSTOP"
            x3 = "LKFMTWMTVKDEIVXHFHMNFDAZDRLMYQFRCKHHQSIMPIBZSXSCNMXVEKLXYRLEKZ"
            Just ((0,6,2),[('B','D'),('G','E'),('W','H'),('J','N'),('M','I'),('S','A'),('C','L')])
        
    Testing code on the examples from Blackboard:
        4)  p = "COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE"
            x = "QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW"
            Just ((0,0,0),[('Q','C'),('W','I'),('X','A'),('U','E'),('H','S'),('G','T'),('R','M')])

        5)  p4 = "AFJEQTMC"
            x4 = "FJEQTMCF"
            Just ((0,0,0),[('I','C'),('P','M'),('U','T'),('L','Q'),('D','E'),('B','F')])

        6)  p5 = "ZGXWAUTS"
            x5 = "XKGZWAUT"
            Just ((0,0,0),[('I','X'),('H','Z'),('K','W'),('V','A'),('D','U'),('F','T')])
        
        7)  p6="TURINGBOMBEHASKELLSIMULATIONSTOP"
            x6="FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQ"
            Just ((1,2,14),[('S','X'),('B','E'),('N','A'),('J','M'),('C','H'),('V','Y')])
        
        8)  p7 = "COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP"
            x7 = "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW"
            Just ((4,3,7),[('K','C'),('N','E'),('O','M'),('J','U'),('X','F'),('G','R'),('L','D'),('B','Z')])
            
            To make sure that every function works as it should I tested each of them on the simple examples above and then
            I was using enigmaEncodeMessage to compare correctness.
            --}