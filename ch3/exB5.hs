-- File: ch3/exB5.hs
-- A function that determined if a given list is a palindrome.

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome xs  = (head xs == last xs) && (isPalindrome . init . tail $ xs)
