import qualified Data.Map as Map

users = Map.fromList [(1, "John"),(2, "Frank"),(3, "James")]

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\ (k,v) acc -> Map.insert k v acc) Map.empty


