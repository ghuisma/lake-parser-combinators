module V2.Parser.Combinator
( between
, sepby1
, sepby
, chainl1
, chainr1
, chainl
, chainr
, ops
) where

import V2.Parser.Prim

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = do
    open
    x <- p
    close
    return x

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
    x <- p
    xs <- many $ do
        _ <- sep
        p
    return (x:xs)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
    where
          rest x = do { f <- op
                      ; y <- p
                      ; rest (f x y)
                      } <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do
    x <- p
    do { f <- op
       ; y <- p `chainr1` op
       ; return $ f x y
       } <|> return x

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = (p `chainl1` op) <|> return v

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = (p `chainr1` op) <|> return v

ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) [op <$ p | (p, op) <- xs]
