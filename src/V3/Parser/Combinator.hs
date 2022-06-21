module V3.Parser.Combinator
( between
, sepby1
, sepby
, chainl1
, chainr1
, chainl
, chainr
, ops
) where

import V3.Parser.Prim

between :: LakeParser a -> LakeParser b -> LakeParser c -> LakeParser c
between open close p = do
    open
    x <- p
    close
    return x

sepby1 :: LakeParser a -> LakeParser b -> LakeParser [a]
p `sepby1` sep = do
    x <- p
    xs <- many $ do
        _ <- sep
        p
    return (x:xs)

sepby :: LakeParser a -> LakeParser b -> LakeParser [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

chainl1 :: LakeParser a -> LakeParser (a -> a -> a) -> LakeParser a
p `chainl1` op = p >>= rest
    where
          rest x = do { f <- op
                      ; y <- p
                      ; rest (f x y)
                      } <|> return x

chainr1 :: LakeParser a -> LakeParser (a -> a -> a) -> LakeParser a
p `chainr1` op = do
    x <- p
    do { f <- op
       ; y <- p `chainr1` op
       ; return $ f x y
       } <|> return x

chainl :: LakeParser a -> LakeParser (a -> a -> a) -> a -> LakeParser a
chainl p op v = (p `chainl1` op) <|> return v

chainr :: LakeParser a -> LakeParser (a -> a -> a) -> a -> LakeParser a
chainr p op v = (p `chainr1` op) <|> return v

ops :: [(LakeParser a, b)] -> LakeParser b
ops xs = foldr1 (<|>) [op <$ p | (p, op) <- xs]
