

data Parser a = Parser
    { run :: String -> Res a }

type Res a = Maybe (a, String)

instance Functor Parser where
    fmap f p = p >>= pure . f

instance Applicative Parser where
    pure = return
    (<*>) tf ta = tf >>= flip fmap ta

instance Monad Parser where
    return = item
    (>>=) (Parser p) ftp = Parser $ \inp ->
        (p inp) >>= (\(v, rest) -> (run (ftp v)) rest) 


item :: a -> Parser a
item i = Parser $ \s -> Just (i, s)


is :: (Char -> Bool) -> Parser Char
is predicate = Parser $ \input ->
        case input of
          (c:cs) ->
                if predicate c then 
                    Just (c, cs)
                else
                    Nothing
          [] ->
              Nothing


specificString :: String -> Parser String
specificString = parseMany specificChar


parseMany p = sequence . map p

specificChar :: Char -> Parser Char
specificChar = is . (==)

isDigit :: Parser Char
isDigit = is $ flip elem ['0'..'9']



main = undefined
