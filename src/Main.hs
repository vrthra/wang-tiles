module Main where
import Wang
import Text.JSON

x = 3

xz0 :: NTile
xz0    = T b 0 0 b

xa1    = T b        (A#0)  0      (f(A#0))
xa2    = T (f(A#0)) 1      (B#x)  b
xa3    = T b        1      1      b
xa4    = T b        0      0      b

xb1    = T b        (B#x)     x      (f(B#x))
xb2    = T (f(B#x)) 1     (B#1)   b

xc1    = T b        x      x       b
xc2    = T b        (B#1)  1      (f(B#1))
xc3    = T (f(B#1)) 0      (C#0)  b

xd1    = T b        (C#0)  0      (f(C#0))
xd2    = T (f(C#0)) 0      (D#1)  b

xe1    = T b        0      (E#0)  (f(D#1))
xe2    = T (f(D#1)) (D#1)  1      b

xf1    = T b        1      (E#1)  (f(E#0))
xf2    = T (f(E#0)) (E#0)  0      b

xg1    = T b        x      (A#x)  (f(E#1))
xg2    = T (f(E#1)) (E#1)  1      b

side [] = []
side (T l u d r:ls) = T b d d b : (side ls)

tiles = [xz0, xa1,xa2,xa3,xa4,xb1,xb2,xc1,xc2,xc3,xd1,xd2,xe1,xe2,xf1,xf2,xg1,xg2]

initial = [xz0,xa1,xa2,xa3,xa4] ++ repeatT xa4 4

loop initconf 0 = do putStrLn (show initconf)

loop t n = do putStrLn (show t)
              lnext <- return $ myt t
              loop lnext (n-1)
              return ()

myt t = trans t tiles side side

main = loop initial 10

