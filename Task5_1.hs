module Task5_1 where

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil _ = error("IndexOutOfBoundException")
index (DCons _ x r) i
  | i == 0    = x
  | otherwise = index r (i - 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt = insertAt' DNil
  where
    insertAt' :: DList a -> DList a -> Int -> a -> DList a
    insertAt' left DNil i v
      | i == 0    = DCons left v DNil
      | otherwise = error("IndexOutOfBoundException")
    insertAt' left (DCons l x r) i v
      | i == 0    = let rec = DCons l v (DCons rec x r) in rec
      | otherwise = let rec = DCons l x (insertAt' rec r (i - 1) v) in rec

removeAt :: DList a -> Int -> DList a
removeAt = removeAt' DNil
  where
    removeAt' _ DNil _ = error("IndexOutOfBoundException")
    removeAt' left (DCons l x r) i
      | i == 0 = case r of
        DNil           -> DNil
        DCons l' x' r' -> DCons left x' r'
      | otherwise = let rec = DCons l x (removeAt' rec r (i - 1)) in rec
