-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Prelude hiding (repeat, iterate, cycle, scanl)

-- Неща за обсъждане:

-- * изброимо безкрайни множества

-- * кои функции могат да работят с безкрайни списъци

-- * декартово произведение и размисли върху задачите

--------------------------------------------------------------------------------
-- ЗАДАЧИ --
------------

-- Не забравяйте че за решението на всяка задача може да използвате предходните.

-- Безкраен списък от всички естествени числа.
-- Имплементирайте чрез рекурсия.
-- Бонус: Без помощна функция(променлива)
--
-- Пример:
-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
nats :: [Integer]
nats = undefined

-- Безкрайно повторение на стойност
--
-- Пример:
-- >>> take 5 $ repeat 'a'
-- "aaaaa"
repeat :: a -> [a]
repeat = undefined

-- За функция f и аргумент x,
-- генерира безкрайният списък [x, f(x), f(f(x)), ...]
--
-- Пример:
-- >>> take 5 $ iterate (^2) 2
-- [2,4,16,256,65536]
iterate :: (a -> a) -> a -> [a]
iterate = undefined

-- За даден списък xs,
-- генерира безкрайният списък от повторение на елементите на xs
--
-- Пример:
-- >>> take 8 $ cycle [1,2,3]
-- [1,2,3,1,2,3,1,2]
cycle :: [a] -> [a]
cycle = undefined

-- Вариант на foldl, в който пазим междинните резултати от всяка стъпка
-- и връщаме списък от тях, вместо само финалният резултат.
-- (понякога доста полезно за дебъгване на foldl)
--
-- Пример:
-- >>> scanl (+) 0 [1..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = undefined

-- Безкраен поток от числата на Фибоначи (hint: zipWith)
--
-- Пример:
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = undefined

-- Безкраен списък, в който n-тия елемент е n! (факториел)
-- Бонус: Опитайте да я имплементирате чрез scanl.
--
-- Пример:
-- >>> take 7 factsScanl
-- [1,1,2,6,24,120,720]
facts :: [Integer]
facts = undefined

-- Безкраен списък от всички прости числа
-- Бонус: Опитайте чрез алгоритъма "Решето на Ератостен"
--
-- Пример:
-- >>> take 10 primes
-- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
primes :: [Integer]
primes = undefined


-- (a,b,c) е Питагорова тройка ако: a*a + b*b = c*c
-- Да се напише функция която генерира безкраен списък
-- от всички Питагорови тройки.
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = undefined

-- Безкраен списък от всички двойки естествени числа
--
-- Пример:
-- >>> take 10 natTuples
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
natTuples :: [(Int, Int)]
natTuples = undefined

-- Генерирайте безкрайния поток от низове, представляващ всички композиции на
-- функции f и g:
-- ["id", "id . f", "id . g", "id . f . f", "id . f . g", "id . g . f", ...]
compositions :: [String]
compositions = undefined

-- По дадени 2 безкрайни списъка и двуместен предикат, да се намери
-- безкрайният списък от двойки елементи от двата списъка изпълняващи предиката
filterTuples :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
filterTuples = undefined
