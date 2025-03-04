--Question 1. Explain the effect of the function defined here
mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m==n) && (n==p))
{-
    For the first line, "Mystery" is a function that takes three Integer data types and outputs a Bool. Simple enough
    For the second line, "Mystery" appears to be similar to a NAND gate. It checks if m and p are both equal to n.
    The Yes or No answer of the Bool is than reversed due to "not". "Mystery" equals False when both m and p are equal to n and True for any
    other combonation.
-}

--Question 2. Define a function, result of threeDifferent is True only if m, n and p are different.
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (m /= p)
-- /= is not Equal

--Question 3. fourEqual returns the value True only if all four of its arguments are equal. Give a definition which uses threeDifferent
fourEqual :: Integer ->  Integer -> Integer -> Integer -> Bool
fourEqual m n p z = (not (threeDifferent m n p) && not (threeDifferent m n z) && not (threeDifferent m p z))

--Question 4. Define a function to convert small letters to capitals which returns unchanged characters which are not small letters.
converter :: Char -> Char
converter m
    | m >= 'a' && m <= 'z' = toEnum (fromEnum m + fromEnum 'A' - fromEnum 'a')
    | otherwise = m

--Question 5. Define the function, charToNum, converts a digit like '8' to its value, 8. non-digits = 0
charToNum :: Char -> Int
charToNum m
    | ('0' <= m) && ('9' >= m) = fromEnum m - fromEnum '0'
    | otherwise = 0

--Question 6. Define a function, onThreeLines, takes three strings and returns a single string, shows three strings on seperate lines
onThreeLines :: String -> String -> String -> IO ()
onThreeLines m n p = putStrLn (m ++ "\n" ++ n ++ "\n" ++ p)
{-
    ++ joins strings and "\n" is a break, This problem questioned me as needs an IO() output to use putStrLn like in Week 3 notes. 
    but the question says output String. If String is the output \n will appear as text and not a line break. IO() is needed for line break.
-}

--Question 7. Define a function, converts a digit to its representation in Roman numerals
romanDigit :: Char -> String
romanDigit '0' = ""
romanDigit '1' = "I"
romanDigit '2' = "II"
romanDigit '3' = "III"
romanDigit '4' = "IV"
romanDigit '5' = "V"
romanDigit '6' = "VI"
romanDigit '7' = "VII"
romanDigit '8' = "VIII"
romanDigit '9' = "IX"
-- Might not be the best way but it is definetly a solution. digit means 0-9

--Question 8. Give a function to return the average of three integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree m n p = (fromIntegral (m + n + p)) / 3.0
-- This one had me stumped. I don't remember about talking about fromIntegral in class. However, without it, m,n, and p would have an error.

--Question 9. Using averageThree define a function which returns how many of its inputs are larger than their average value.
howManyAboveAverage :: Integer -> Integer -> Integer -> Float
howManyAboveAverage m n p
    | (fromIntegral m > avg) && (fromIntegral n > avg) && (fromIntegral p > avg) = 3
    | (fromIntegral m > avg) && (fromIntegral n > avg) || (fromIntegral m > avg) && (fromIntegral p > avg) || (fromIntegral n > avg) && (fromIntegral p > avg) = 2
    | (fromIntegral m > avg) || (fromIntegral n > avg) || (fromIntegral p > avg) = 1
    | otherwise = 0
  where avg = averageThree m n p

--Question 10. Write a function given the coefficients of the quadratic, a b and c, will return how many roots the equation has. equation is non-degenerate
numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
    | discriminant > 0 = 2
    | discriminant == 0 = 1
    | otherwise = 0
    where discriminant = b * b - 4 * a * c