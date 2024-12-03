
data Parser a = Parser(String->Maybe(a,String))

parse ::Parser a -> String -> Maybe(a,String)
parse (Parser par) str = par str

item :: (Char->Bool)->Parser Char
item f = Parser (\str-> case str of
    (x:xs)-> if f x then Just(x,xs) else Nothing
    []-> Nothing)

char :: Char->Parser Char
char c =  item (\x -> x == c)

string :: String -> Parser String
string "" = Parser(\str -> Just("",str))
string (x:xs) = Parser(\str -> 
    case parse (char x) str of
        Just(y,ys) -> case parse (string xs) ys of
            Just(z,zs) -> Just(x:xs,zs)
            Nothing -> Nothing
        Nothing -> Nothing )

bind :: Parser a -> (a-> Parser b) -> Parser b
pa `bind` f = Parser(\str->
    case parse pa str of
        Just(a,xs)-> f a
        Nothing -> Nothing)