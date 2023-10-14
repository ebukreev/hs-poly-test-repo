module Part1.Tasks where

import Util(notImplementedYet)

factorial n = product [1..n]

coerce x = x - 2 * pi * (fromIntegral (floor (x / (2 * pi))))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum [((-1) ^ n) * ((coerce x) ^ (2 * n + 1)) / fromIntegral (factorial (2 * n + 1)) | n <- [0..15]]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum [((-1) ^ n) * ((coerce x) ^ (2 * n)) / fromIntegral (factorial (2 * n)) | n <- [0..15]]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = if b == 0 then abs a else gcd b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | month > 12 || month < 1 = False
    | day > monthLength || day < 1 = False
    | month == 2 && isLeapYear year && day > 29 = False
    | month == 2 && (not (isLeapYear year)) && day > 28 = False
    | otherwise = True
    where
        monthLength = getMonthLength month year

isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0 = True
    | otherwise = False

getMonthLength month year
    | month == 2 && isLeapYear year = 29
    | month == 2 = 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n
    | n == 0 = 1
    | n == 1 = x
    | otherwise = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
    | n == 1 = False
    | otherwise = not (any (\x -> n `mod` x == 0) [2..(n - 1)])

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = 0.5 * (abs (sum (zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) points (tail points ++ [head points]))))

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a + b <= c || a + c <= b || b + c <= a = -1
    | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = 2
    | a^2 + b^2 < c^2 || a^2 + c^2 < b^2 || b^2 + c^2 < a^2 = 0
    | otherwise = 1
