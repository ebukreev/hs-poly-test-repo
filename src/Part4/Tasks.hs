module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist [] = REmpty
listToRlist list = listToRlist (init list) :< (last list)

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show REmpty = "[]"
    show (init :< last) = "[" ++ show' init ++ show last ++ "]"
      where
        show' REmpty = ""
        show' (init :< last) = show' init ++ show last ++ ","

instance Eq a => Eq (ReverseList a) where
    REmpty == REmpty = True
    (init1 :< last1) == (init2 :< last2) = init1 == init2 && last1 == last2
    _ == _ = False

instance Semigroup (ReverseList a) where
    REmpty <> l = l
    l <> REmpty = l
    l <> (init :< last) = (l <> init) :< last

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap f REmpty = REmpty
    fmap f (init :< last) = fmap f init :< f last

instance Applicative ReverseList where
    pure x = REmpty :< x
    REmpty <*> _ = REmpty
    (init :< last) <*> l = (init <*> l) <> (fmap last l)

instance Monad ReverseList where
    return l = REmpty :< l

    REmpty >>= _ = REmpty
    (init :< last) >>= f = (init >>= f) <> (f last)
