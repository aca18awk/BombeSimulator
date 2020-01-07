{--
    Solution to COM2018 Assignment 2, written by Aleksandra Kulbaka, November 2019
    In this assignment I used solution to COM2018 Assignment 1 (reverseEncode and findLoc)
                                                                   written by Emma Norling
--}

module Enigma where
    import AssignmentHelp
    import Data.Char 
    import Data.List

    type Rotor = String
    type Reflector = [(Char,Char)]
    type Offsets = (Int, Int, Int)
    type Steckerboard = [(Char,Char)] 
    type Menu = [Integer]
    type Crib = (String,String)
    type CribTuple = (Char, Char, Integer)

    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets 
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

    enigmaEncode :: Char -> Enigma -> Char
    enigmaEncode letter (SimpleEnigma lr mr rr reflector offsets) = 
                decode rr ro (decode mr mo (decode lr lo 
                (swap reflector 
                  (encode lr lo (encode mr mo (encode rr ro letter))))))
                where lo = advancedOffset 0 offsets  
                      mo = advancedOffset 1 offsets 
                      ro = advancedOffset 2 offsets
                     
    enigmaEncode letter (SteckeredEnigma lr mr rr reflector offsets steckerboard) = 
        swap steckerboard 
               (decode rr ro (decode mr mo (decode lr lo 
                (swap reflector 
                  (encode lr lo (encode mr mo (encode rr ro 
                                                (swap steckerboard letter))))))))
                where lo = advancedOffset 0 offsets  
                      mo = advancedOffset 1 offsets 
                      ro = advancedOffset 2 offsets

    enigmaEncodeMessage :: String -> Enigma -> String
    enigmaEncodeMessage "" enigma = ""
    enigmaEncodeMessage (x:xs) (SimpleEnigma lr mr rr reflector offsets) 
        | xs == "" = [enigmaEncode x (SimpleEnigma lr mr rr reflector offsets)]
        | otherwise = enigmaEncode x (SimpleEnigma lr mr rr reflector offsets) : 
            enigmaEncodeMessage xs (SimpleEnigma lr mr rr reflector
              (advancedOffset 0 offsets, advancedOffset 1 offsets, advancedOffset 2 offsets))

    enigmaEncodeMessage (x:xs) (SteckeredEnigma lr mr rr reflector offsets steckerboard) 
        | xs == "" = [enigmaEncode x (SteckeredEnigma lr mr rr reflector offsets steckerboard)]
        | otherwise = enigmaEncode x (SteckeredEnigma lr mr rr reflector offsets steckerboard) : 
            enigmaEncodeMessage xs (SteckeredEnigma lr mr rr reflector 
              (advancedOffset 0 offsets, advancedOffset 1 offsets, advancedOffset 2 offsets)
                                                                                 steckerboard)     
                                     
    -- Given the non-empty offsets and the number (0, 1 or 2) of the rotor, it returns advanced offset
    advancedOffset :: Int -> Offsets -> Int
    advancedOffset number offsets = advanceRotors offsets !! number
              where advanceRotors :: Offsets -> [Int]
                    advanceRotors (lo,mo,ro) 
                      | ro < 25 = [lo, mo, ro + 1]
                      | ro == 25 && mo < 25 = [lo, mo + 1, 0]
                      | ro == 25 && mo == 25 && lo < 25 = [lo + 1, 0, 0]
                      | otherwise = [0, 0, 0] 

    -- Given the list of pairs of chars and a letter, it returns matched pair for that letter if that pair exists. 
    swap :: [(Char,Char)] -> Char -> Char
    swap [] letter = letter
    swap ((a,b) : xs) letter  
                      | letter == a = b
                      | letter == b = a
                      | otherwise = swap xs letter  

    encode :: Rotor -> Int -> Char -> Char
    encode rotor offset letter 
      = posToChar ((alphaPos (rotor !! ((alphaPos letter + offset) `mod` 26)) - offset) `mod` 26)  

    decode :: Rotor -> Int -> Char -> Char
    decode rotor offset letter 
      = reverseEncode rotor (offset) (posToChar((alphaPos(letter) + offset) `mod` 26))

    -------------------------------------------------- Functions from Assignment1: -------------------------- 
    alphabet = ['A'..'Z']

    posToChar :: Int -> Char
    posToChar n = chr (ord 'A' + n)

    reverseEncode :: Cipher -> Int -> Char -> Char
    reverseEncode cipher offset c
      = alphabet !! (((findLoc c cipher) - offset) `mod` 26)

    findLoc :: Eq a => a -> [a] -> Int
    findLoc item [single] = 0
    findLoc item (x:xs)
          | item == x = 0
          | otherwise = 1 + findLoc item xs    
    ----------------------------------------------------------------------------------------------------------
                                   
    allLongestMenus :: Crib -> [Menu]
    allLongestMenus (plain, crib) = filter (\x-> length x == 17)
                     [findForwardChain (a,b,c) (plain,crib) |(a,b,c) <- zip3 plain crib [0..]]

    longestMenu :: Crib -> Menu
    longestMenu (plain, crib) = maxlength [findForwardChain (a,b,c) (plain,crib) | 
                                                 (a,b,c) <- zip3 plain crib [0..]]

    findForwardChain :: CribTuple -> Crib -> Menu
    findForwardChain (a,b,c) (plain, crib) 
        | findInPlain (a,b,c) cribTuples == [] = [c] 
        | otherwise = maxlength 
           [c: findForwardChain (x,y,z) ((replaceLetter index plain), (replaceLetter index crib)) 
                              | (x,y,z) <- (findInPlain (a,b,c) cribTuples)]
        where index = fromIntegral c
              cribTuples = (zip3 plain crib [0..])

    -- Given the list of menus lists it returns the longest menu
    maxlength :: [Menu] -> Menu
    maxlength [] = []
    maxlength (x:[]) = x
    maxlength (x:(a:b)) | b == [] && length x < length a = a
                        | b == [] && length x >= length a = x
                        | length x < length a = maxlength (a:b)
                        | otherwise = maxlength (x:b)    
    
    -- Given tuple (a,b,c) and the lists of tuples, it returns the list of linked tuples (x,y,z) where b = x                                                                      
    findInPlain :: CribTuple -> [CribTuple] ->[CribTuple]
    findInPlain (a,b,c) [] = []
    findInPlain (a,b,c) ((x,y,z):xs) | b == x = (x,y,z) : findInPlain (a,b,c) xs
                                     | otherwise = findInPlain (a,b,c) xs

    -- Given the string and the index n, it returns the string with nth character replaced by '0'.
    replaceLetter :: Int -> String -> String
    replaceLetter n text = concat [[a | (b, a) <-zip [0..n - 1] text], "0",
                           map (\x -> text !! x) [n + 1..(length text) - 1]]

