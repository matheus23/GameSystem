module CappedList where

-- stack-like List with capped size
data CappedList a = CappedList {
  hlMaxLength :: Int,
  hlContent :: [a]
}

hlPush :: a -> CappedList a -> CappedList a
hlPush element (CappedList maxLen content) = CappedList maxLen $ take maxLen $ element:content

hlHead :: CappedList a -> a
hlHead (CappedList _ []) = error "head of empty CappedList"
hlHead (CappedList _ (x:_)) = x

hlTail :: CappedList a -> CappedList a
hlTail    (CappedList _ [])     = error "tail of empty CappedList"
hlTail    (CappedList _ (_:[])) = error "tail of CappedList with length 1"
hlTail hl@(CappedList _ (_:xs)) = hl { hlContent = xs }

hlIndex :: CappedList a -> Int -> Maybe a
hlIndex (CappedList maxLen content) index
  | maxLen >= 0 && maxLen < (length content) = Just $ content !! index
  | otherwise = Nothing
