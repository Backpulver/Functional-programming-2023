fact :: Int -> Int
fact 0 = 0
fact 1 = 1
fact x = x * fact(x - 1)

fib :: Int -> Int
fib x
    | x < 2 = x
    | otherwise = fib (x - 1) + fib (x - 2)

myAbs :: Int -> Int
myAbs x
      | x < 0 = (-x)
      | otherwise = x

-- composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
-- composeInt f g = undefined