{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Lib
    ( simulate, someFunc
    ) where


import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just), Show(..), Read(..), read, getLine, lookup, print, IO, putStr, putStrLn, Int);
import qualified Prelude;
import Data.Bifunctor  
import qualified Data.HashMap.Lazy as L 
import qualified Data.HashSet as HashSet
import Data.Hashable
import Text.Read (readMaybe)


someFunc :: IO ();
someFunc = putStrLn ""

data Num = One | Bit0 Num | Bit1 Num

data Set a = Set (HashSet.HashSet a) | Coset (HashSet.HashSet a) deriving Show

-- data Pfun a b = Pfun_of_alist [(a, b)] | Pfun_of_map (a -> Maybe b);
data Pfun a b = PfunOfAList [(a, b)] | PfunOfMap (a -> Maybe b) | PfunOfHashMap (L.HashMap a b)

data ITree a b = Ret b | Sil (ITree a b) | Vis (Pfun a (ITree a b))


-- membera :: forall a. (Eq a) => [a] -> a -> Bool
-- membera [] y = False
-- membera (x : xs) y = x == y || membera xs y

member :: forall a. (Eq a, Hashable a) => a -> Set a -> Bool
member x (Coset xs) = not (HashSet.member x xs)
member x (Set xs) = HashSet.member x xs 

tl :: forall a. [a] -> [a]
tl [] = []
tl (x21 : x22) = x22

restrict :: forall a b. (Eq a, Hashable a) => Set a -> Pfun a b -> Pfun a b
restrict a (PfunOfHashMap xs) = PfunOfHashMap (L.filterWithKey (\ k v -> member k a) xs)
restrict a (PfunOfAList xs) = PfunOfAList (filter (\ (k, _) -> member k a) xs)
restrict _ _ = Prelude.undefined

pdom :: forall a b. (Eq a, Hashable a) => Pfun a b -> Set a;
pdom (PfunOfMap xs) = Prelude.undefined
pdom (PfunOfHashMap xs) = Set (L.keysSet xs)
pdom (PfunOfAList xs) = 
    let firsts = map fst xs
    in Set (HashSet.fromList firsts);

mapPfun :: forall a b c. (a -> b) -> Pfun c a -> Pfun c b;
mapPfun f (PfunOfMap m) = Prelude.undefined
mapPfun f (PfunOfHashMap m) = PfunOfHashMap (L.map f m)
mapPfun f (PfunOfAList m) = PfunOfAList (map (Data.Bifunctor.second f) m);

bindITree :: forall a b c. ITree a b -> (b -> ITree a c) -> ITree a c;
bindITree (Vis t) k = Vis (mapPfun (`bindITree` k) t);
bindITree (Sil t) k = Sil (bindITree t k);
bindITree (Ret v) k = Sil (k v);

zeroPFun :: forall a b. Pfun a b;
zeroPFun = PfunOfAList [];

plusPFun :: forall a b. (Eq a, Hashable a) => Pfun a b -> Pfun a b -> Pfun a b;
plusPFun (PfunOfAList f) (PfunOfHashMap g) = PfunOfHashMap (L.union (L.fromList f) g);
plusPFun (PfunOfHashMap f) (PfunOfAList g) = PfunOfHashMap (L.union f (L.fromList g));
plusPFun (PfunOfHashMap f) (PfunOfHashMap g) = PfunOfHashMap (L.union f g);
plusPFun (PfunOfAList f) (PfunOfAList g) = PfunOfAList (g ++ f);
plusPFun _ _ = Prelude.undefined

uminusSet :: forall a. Set a -> Set a;
uminusSet (Coset xs) = Set xs;
uminusSet (Set xs) = Coset xs;

pdomRes :: forall a b. (Eq a, Hashable a) => Set a -> Pfun a b -> Pfun a b;
pdomRes = restrict 
--pdomRes a m = PfunOfAList (restrict a m);

pfunGet :: (Eq a, Hashable a) =>  a -> Pfun a b -> Maybe b
pfunGet x (PfunOfHashMap xs) = L.lookup x xs
pfunGet x (PfunOfAList xs) = lookup x xs
pfunGet x (PfunOfMap xs) = xs x

simulateCnt :: (Eq e, Show e, Read e, Show s, Hashable e) => Int -> ITree e s -> IO ();
simulateCnt n (Ret x) = putStrLn ("Terminated: " ++ show x);
simulateCnt n (Sil p) = 
    do  
        putStrLn "Internal Activity"
        if n >= 10 then 
            do  
                putStr "Continue? [Y/N]" 
                q <- getLine
                if q == "Y" then simulateCnt 0 p else putStrLn "Ended early."
        else simulateCnt (n + 1) p

simulateCnt n (Vis (PfunOfHashMap empty)) = putStrLn "Deadlocked.";
simulateCnt n (Vis (PfunOfAList [])) = putStrLn "Deadlocked.";
simulateCnt n t@(Vis m) = 
  do  
       putStrLn ("Events: " ++ show (pdom m));
       e <- getLine
       case readMaybe e of
         Nothing -> 
             do  
                 putStrLn "No parse" 
                 simulateCnt n t 
         Just v -> case pfunGet v m of
                     Nothing -> do { putStrLn "Rejected"; simulateCnt n t }
                     Just k -> simulateCnt 0 k

simulate :: (Eq e, Show e, Read e, Show s, Hashable e) => ITree e s -> IO ()
simulate = simulateCnt 0
