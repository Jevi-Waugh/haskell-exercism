module Bob (responseFor) where
import Data.Char 

-- Old solution

-- isSpace :: Char -> Bool
-- isSpace s = s == ' '

-- responseFor :: String -> String
-- responseFor s
--   | null s                              = "Fine. Be that way!"
--   | isQuestion && allUpper && hasLetter   = "Calm down, I know what I'm doing!"
--   | allUpper && hasLetter                 = "Whoa, chill out!"
--   | isQuestion                            = "Sure."
--   | otherwise                             = "Whatever."
--   where
--     isUpper  c = 'A' <= c && c <= 'Z'
--     isLetter c = 'a' <= c && c <= 'z' ||  (isUpper c)
--     hasLetter = any isLetter s
--     allUpper  = all isUpper (filter isLetter s)
--     isQuestion = 
--       let t = filter (not .isSpace) s 
--       in not (null t) && last t == '?'
--     spaces s = all (`elem` " \t\n\r") s


responseFor :: String -> String
responseFor xs
    | null text = "Fine. Be that way!"
    | isShouting && isAsking = "Calm down, I know what I'm doing!"
    | isShouting = "Whoa, chill out!"
    | isAsking = "Sure."
    | otherwise = "Whatever."
  where
    text = filter (not . isSpace) xs
    isLetter c = 'a' <= c && c <= 'z' ||  (isUpper c)
    isUpper  c = 'A' <= c && c <= 'Z'
    letters = filter isLetter text
    isShouting = all isUpper letters && any isUpper letters
    isAsking = last text == '?'
