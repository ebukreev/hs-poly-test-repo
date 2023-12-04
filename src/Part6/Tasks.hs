{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    eyeImpl :: Int -> mx
    zeroImpl :: Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    eyeImpl _ = 1
    zeroImpl _ _ = 0

instance Matrix [[Int]] where
    eyeImpl w = [[if i == j then 1 else 0 | j <- [1 .. w]] | i <- [1 .. w]]
    zeroImpl w h = [[0 | j <- [1 .. w]] | i <- [1 .. h]]

instance Matrix (SparseMatrix Int) where
    eyeImpl w = SparseMatrix w w (fromList [((i, i), 1) | i <- [0 .. (w - 1)]])
    zeroImpl w h = SparseMatrix w h mempty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = eyeImpl
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zeroImpl
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
