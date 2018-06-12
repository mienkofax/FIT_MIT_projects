faktorial :: Integer -> Integer
faktorial 0 = 1
faktorial n = n * faktorial (n-1)

faktoriall :: Integer -> Integer
faktoriall n = product [1..n]

faktorialll :: Integer -> Integer
faktorialll 0 = 1
faktorialll n = if n < 0 then error "Chyba" else n * faktorialll (n-1)

faktoriallll :: Integer -> Maybe Integer
faktoriallll 0 = Just 1
faktoriallll n = if n < 0 then Nothing else Just (n * faktorialll (n-1))

faktorialllll :: Integer -> Either String Integer
faktorialllll 0 = Right 1
faktorialllll n = if n < 0 then Left "Chyba" else Right (n * faktorialll (n-1))

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fib1 :: Integer -> Integer
fib1 n = f 0 1 n 
    where
        f v _ 0 = v
        f a b n = f b (a+b) (n-1)
