module Wang where
import List
data Tile a b = T a b b a
                deriving (Eq, Show)

type NTile = Tile String Int

data St = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
                deriving (Show)




repeatT t i = [ t | x <- [1..i]]

b = "_"
f i = "f " ++ (show i)
(#) fn i = case fn of
               A -> 10 + i
               B -> 20 + i
               C -> 30 + i
               D -> 40 + i
               E -> 50 + i
               F -> 60 + i
               G -> 70 + i
               H -> 80 + i
               I -> 90 + i
               J -> 100 + i
               K -> 110 + i
               L -> 120 + i
               M -> 130 + i
               N -> 140 + i
               O -> 150 + i
infixl 1 #

getHeadIdx line = case List.elemIndex h line of
                        Just i -> (i, h)
                where h = getHead line

getHead line = lst
        where x (T l u d r) = if d > 9 then True else False
              [lst] = [l | l <- line, x l]

lOrR line t = case getHeadIdx line of
                (idx, T l u d r) -> case [ t | t@(T l1 u1 d1 r1) <- t, u1 == d] of
                                            [T "_" u2 d2 r2] -> (idx, idx+1, f(d))
                                            [T l2 u2 d2 "_"] -> (idx-1,idx,f(d))
                                            [] -> error $ ">> No tile with up " ++ (show d)
                                            t -> error $ ">>" ++ (show t)

trans line t lside rside = case lOrR line t of
     (lidx, ridx, mid) -> ans
          where (T l1 u1 d1 r1) =  line !! lidx
                (T l2 u2 d2 r2) =  line !! ridx
                [tr1] = [t | t@(T l u d r) <- t, l == b && mid == r && u == d1] 
                [tr2] = [t | t@(T l u d r) <- t, r == b && mid == l && u == d2]
                y = fst (splitAt lidx line)
                z = snd (splitAt (ridx +1) line)
                ans = (lside y) ++ [tr1] ++ [tr2] ++ (rside z)


dtrans line t lside rside = show (lOrR line t)


                   


ud t = map u t
    where u (T _ u d _) = [u,d]
lr t = map l t
    where l (T l u d r) = [l,r]

