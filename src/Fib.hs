module Fib where
import Wang
import Text.JSON

-- Wang tiles for Fibonacci numbers
-- Converted from the the Turing machine on http://people.oregonstate.edu/~gopinatr/turing/tm.html
-- States are renamed as follows: 
-- s   -> A
-- s0  -> B
-- s1  -> C
-- s2  -> D
-- s3  -> E
-- s4  -> F
-- s5  -> G
-- s6  -> H
-- s1' -> I
-- s2' -> J
-- s3' -> K
-- s4' -> L
-- s5' -> M
-- g1  -> N
-- g2  -> O

x = 2
a = 3

xz0 :: NTile
xz0    = T b        0      0      b
xz1    = T b        1      1      b
xzx    = T b        x      x      b
xza    = T b        a      a      b
xzs    = T b        0      (A#x)  b

-- Tiles for transition s x x R s
xa1    = T b        (A#x)  x      (f(A#x))
xa2    = T (f(A#x)) 0      (A#0)  b
xa3    = T (f(A#x)) 1      (A#1)  b
xa4    = T (f(A#x)) x      (A#x)  b
xa5    = T (f(A#x)) a      (A#a)  b
-- Tiles for transition s 0 0 S s0
xa6    = T b        (A#0)  (B#0)  b
-- Tiles for transition s 1 1 S s0
xa7    = T b        (A#1)  (B#1)  b

-- Tiles for transition s0 1 A R s1
xb1    = T b        (B#1)  a      (f(B#1))
xb2    = T (f(B#1)) 0      (C#0)  b
xb3    = T (f(B#1)) 1      (C#1)  b
xb4    = T (f(B#1)) x      (C#x)  b
xb5    = T (f(B#1)) a      (C#a)  b
-- Tiles for transition s0 x x R s1'
xb6    = T b        (B#x)  x      (f(B#x))
xb7    = T (f(B#x)) 0      (I#0)  b
xb8    = T (f(B#x)) 1      (I#1)  b
xb9    = T (f(B#x)) x      (I#x)  b
xb10   = T (f(B#x)) a      (I#a)  b

-- Tiles for transition s1 1 1 R s1
xc1    = T b        (C#1)  1      (f(C#1))
xc2    = T (f(C#1)) 0      (C#0)  b
xc3    = T (f(C#1)) 1      (C#1)  b
xc4    = T (f(C#1)) x      (C#x)  b
xc5    = T (f(C#1)) a      (C#a)  b
-- Tiles for transition s1 x x R s2
xc6    = T b        (C#x)  x      (f(C#x))
xc7    = T (f(C#x)) 0      (D#0)  b
xc8    = T (f(C#x)) 1      (D#1)  b
xc9    = T (f(C#x)) x      (D#x)  b
xc10   = T (f(C#x)) a      (D#a)  b

-- Tiles for transition s2 1 1 R s2
xd1    = T b        (D#1)  1      (f(D#1))
xd2    = T (f(D#1)) 0      (D#0)  b
xd3    = T (f(D#1)) 1      (D#1)  b
xd4    = T (f(D#1)) x      (D#x)  b
xd5    = T (f(D#1)) a      (D#a)  b
-- Tiles for transition s2 x x R s3
xd6    = T b        (D#x)  x      (f(D#x))
xd7    = T (f(D#x)) 0      (E#0)  b
xd8    = T (f(D#x)) 1      (E#1)  b
xd9    = T (f(D#x)) x      (E#x)  b
xd10   = T (f(D#x)) a      (E#a)  b

-- Tiles for transition s3 x 1 L s4
xe1    = T (f(E#x)) (E#x)  1      b 
xe2    = T b        0      (F#0)  (f(E#x))
xe3    = T b        1      (F#1)  (f(E#x))
xe4    = T b        x      (F#x)  (f(E#x))
xe5    = T b        a      (F#a)  (f(E#x))
-- Tiles for transition s3 1 1 R s3
xe6    = T b        (E#1)  1      (f(E#1))
xe7    = T (f(E#1)) 0      (E#0)  b
xe8    = T (f(E#1)) 1      (E#1)  b
xe9    = T (f(E#1)) x      (E#x)  b
xe10   = T (f(E#1)) a      (E#a)  b

-- Tiles for transition s4 1 1 L s4
xf1    = T (f(F#1)) (F#1)  1      b 
xf2    = T b        0      (F#0)  (f(F#1))
xf3    = T b        1      (F#1)  (f(F#1))
xf4    = T b        x      (F#x)  (f(F#1))
xf5    = T b        a      (F#a)  (f(F#1))
-- Tiles for transition s4 x x L s5
xf6    = T (f(F#x)) (F#x)  x      b 
xf7    = T b        0      (G#0)  (f(F#x))
xf8    = T b        1      (G#1)  (f(F#x))
xf9    = T b        x      (G#x)  (f(F#x))
xf10   = T b        a      (G#a)  (f(F#x))

-- Tiles for transition s5 1 1 L s5
xg1    = T (f(G#1)) (G#1)  1      b 
xg2    = T b        0      (G#0)  (f(G#1))
xg3    = T b        1      (G#1)  (f(G#1))
xg4    = T b        x      (G#x)  (f(G#1))
xg5    = T b        a      (G#a)  (f(G#1))
-- Tiles for transition s5 x x L s6
xg6    = T (f(G#x)) (G#x)  x      b 
xg7    = T b        0      (H#0)  (f(G#x))
xg8    = T b        1      (H#1)  (f(G#x))
xg9    = T b        x      (H#x)  (f(G#x))
xg10   = T b        a      (H#a)  (f(G#x))

-- Tiles for transition s6 1 1 L s6
xh1    = T (f(H#1)) (H#1)  1      b 
xh2    = T b        0      (H#0)  (f(H#1))
xh3    = T b        1      (H#1)  (f(H#1))
xh4    = T b        x      (H#x)  (f(H#1))
xh5    = T b        a      (H#a)  (f(H#1))
-- Tiles for transition s6 A A R s0
xh6    = T b        (H#a)  a      (f(H#a))
xh7    = T (f(H#a)) 0      (B#0)  b
xh8    = T (f(H#a)) 1      (B#1)  b
xh9    = T (f(H#a)) x      (B#x)  b
xh10   = T (f(H#a)) a      (B#a)  b

-- Tiles for transition s1' A A R s1',
xi1    = T b        (I#a)  a      (f(I#a))
xi2    = T (f(I#a)) 0      (I#0)  b
xi3    = T (f(I#a)) 1      (I#1)  b
xi4    = T (f(I#a)) x      (I#x)  b
xi5    = T (f(I#a)) a      (I#a)  b
-- Tiles for transition s1' 1 A R s2', #read 1, go to end and write
xi6    = T b        (I#1)  a      (f(I#1))
xi7    = T (f(I#1)) 0      (J#0)  b
xi8    = T (f(I#1)) 1      (J#1)  b
xi9    = T (f(I#1)) x      (J#x)  b
xi10   = T (f(I#1)) a      (J#a)  b
-- Tiles for transition s1' x x S g1,
xi11   = T b        (I#x)  (N#x)  b

-- Tiles for transition s2' 1 1 R s2', #skip to answer
xj1    = T b        (J#1)  1      (f(J#1))
xj2    = T (f(J#1)) 0      (J#0)  b
xj3    = T (f(J#1)) 1      (J#1)  b
xj4    = T (f(J#1)) x      (J#x)  b
xj5    = T (f(J#1)) a      (J#a)  b
-- Tiles for transition s2' x x R s3', #answer writing
xj6    = T b        (J#x)  x      (f(J#x))
xj7    = T (f(J#x)) 0      (K#0)  b
xj8    = T (f(J#x)) 1      (K#1)  b
xj9    = T (f(J#x)) x      (K#x)  b
xj10   = T (f(J#x)) a      (K#a)  b

-- Tiles for transition s3' 1 1 R s3', #skip to end
xk1    = T b        (K#1)  1      (f(K#1))
xk2    = T (f(K#1)) 0      (K#0)  b
xk3    = T (f(K#1)) 1      (K#1)  b
xk4    = T (f(K#1)) x      (K#x)  b
xk5    = T (f(K#1)) a      (K#a)  b
-- Tiles for transition s3' x 1 L s4',  #right 1 and reverse
xk6    = T (f(K#x)) (K#x)  1      b 
xk7    = T b        0      (L#0)  (f(K#x))
xk8    = T b        1      (L#1)  (f(K#x))
xk9    = T b        x      (L#x)  (f(K#x))
xk10   = T b        a      (L#a)  (f(K#x))

-- Tiles for transition s4' 1 1 L s4', # skip to beginning
xl1    = T (f(L#1)) (L#1)  1      b 
xl2    = T b        0      (L#0)  (f(L#1))
xl3    = T b        1      (L#1)  (f(L#1))
xl4    = T b        x      (L#x)  (f(L#1))
xl5    = T b        a      (L#a)  (f(L#1))
-- Tiles for transition s4' x x L s5', # cross to other side
xl6    = T (f(L#x)) (L#x)  x      b 
xl7    = T b        0      (M#0)  (f(L#x))
xl8    = T b        1      (M#1)  (f(L#x))
xl9    = T b        x      (M#x)  (f(L#x))
xl10   = T b        a      (M#a)  (f(L#x))

-- Tiles for transition s5' 1 1 L s5', # go to beginning
xm1    = T (f(M#1)) (M#1)  1      b 
xm2    = T b        0      (M#0)  (f(M#1))
xm3    = T b        1      (M#1)  (f(M#1))
xm4    = T b        x      (M#x)  (f(M#1))
xm5    = T b        a      (M#a)  (f(M#1))
-- Tiles for transition s5' x x R s1',
xm6    = T b        (M#x)  x      (f(M#x))
xm7    = T (f(M#x)) 0      (I#0)  b
xm8    = T (f(M#x)) 1      (I#1)  b
xm9    = T (f(M#x)) x      (I#x)  b
xm10   = T (f(M#x)) a      (I#a)  b
-- Tiles for transition s5' A A R s1',
xm11   = T b        (M#a)  a      (f(M#a))
xm12   = T (f(M#a)) 0      (I#0)  b
xm13   = T (f(M#a)) 1      (I#1)  b
xm14   = T (f(M#a)) x      (I#x)  b
xm15   = T (f(M#a)) a      (I#a)  b

-- Tiles for transition g1 x x L g2,
xn1    = T (f(N#x)) (N#x)  x      b 
xn2    = T b        0      (O#0)  (f(N#x))
xn3    = T b        1      (O#1)  (f(N#x))
xn4    = T b        x      (O#x)  (f(N#x))
xn5    = T b        a      (O#a)  (f(N#x))

-- Tiles for transition g2 A 1 L g2,
xo1    = T (f(O#a)) (O#a)  1      b 
xo2    = T b        0      (O#0)  (f(O#a))
xo3    = T b        1      (O#1)  (f(O#a))
xo4    = T b        x      (O#x)  (f(O#a))
xo5    = T b        a      (O#a)  (f(O#a))
-- Tiles for transition g2 x x R s
xo6    = T b        (O#x)  x      (f(O#x))
xo7    = T (f(O#x)) 0      (A#0)  b
xo8    = T (f(O#x)) 1      (A#1)  b
xo9    = T (f(O#x)) x      (A#x)  b
xo10   = T (f(O#x)) a      (A#a)  b

side [] = []
side (T l u d r:ls) = T b d d b : (side ls)

tiles = [xz0, xz1, xzx, xza, xzs, 
         xa1,xa2,xa3,xa4,xa5,xa6,xa7, 
         xb1,xb2,xb3,xb4,xb5,xb6,xb7,xb8,xb9,xb10,
         xc1,xc2,xc3,xc4,xc5,xc6,xc7,xc8,xc9,xc10,
         xd1,xd2,xd3,xd4,xd5,xd6,xd7,xd8,xd9,xd10,
         xe1,xe2,xe3,xe4,xe5,xe6,xe7,xe8,xe9,xe10,
         xf1,xf2,xf3,xf4,xf5,xf6,xf7,xf8,xf9,xf10,
         xg1,xg2,xg3,xg4,xg5,xg6,xg7,xg8,xg9,xg10,
         xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,xh9,xh10,
         xi1,xi2,xi3,xi4,xi5,xi6,xi7,xi8,xi9,xi10,xi11,
         xj1,xj2,xj3,xj4,xj5,xj6,xj7,xj8,xj9,xj10,
         xk1,xk2,xk3,xk4,xk5,xk6,xk7,xk8,xk9,xk10,
         xl1,xl2,xl3,xl4,xl5,xl6,xl7,xl8,xl9,xl10,
         xm1,xm2,xm3,xm4,xm5,xm6,xm7,xm8,xm9,xm10,xm11,xm12,xm13,xm14,xm15,
         xn1,xn2,xn3,xn4,xn5,
         xo1,xo2,xo3,xo4,xo5,xo6,xo7,xo8,xo9,xo10
         ]

initial = [xzs,xz1,xzx,xz1,xzx] ++ repeatT xz0 4

loop initconf 0 = do putStrLn (show initconf)

loop t n = do putStrLn (show t)
              lnext <- return $ myt t
              loop lnext (n-1)
              return ()

myt t = trans t tiles side side
dmyt t = dtrans t tiles side side

next t 0 = return t
next t n = do lnext <- return $ myt t
              nxt <- next lnext (n-1)
              return nxt

main = loop initial 10


