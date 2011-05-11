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

xz1    = T b        1      1      b
xzx    = T b        x      x      b
xza    = T b        a      a      b
xzs    = T b        x      (A#x)  b

-- Tiles for transition s x x R s
xa1    = T b        (A#x)  x      (f(A#x))
xa2    = T (f(A#x)) 1      (A#1)  b
xa3    = T (f(A#x)) x      (A#x)  b
xa4    = T (f(A#x)) a      (A#a)  b
-- Tiles for transition s 1 1 S s0
---- Tiles for transition s  1 1 R ss
xa5    = T b        (A#1)  1      (f(A#1))
xa6    = T (f(A#1)) 1      (AS#1)  b
xa7    = T (f(A#1)) x      (AS#x)  b
xa8    = T (f(A#1)) a      (AS#a)  b
---- Tiles for transition ss 1 1 L s0
xa9    = T (f(AS#1)) (AS#1) 1      b 
xa10   = T b        1      (B#1)  (f(AS#1))
xa11   = T b        x      (B#x)  (f(AS#1))
xa12   = T b        a      (B#a)  (f(AS#1))
---- Tiles for transition ss x x L s0
xa13   = T (f(AS#x)) (AS#x) x      b 
xa14   = T b        1      (B#1)  (f(AS#x))
xa15   = T b        x      (B#x)  (f(AS#x))
xa16   = T b        a      (B#a)  (f(AS#x))
---- Tiles for transition ss A A L s0
xa17   = T (f(AS#a)) (AS#a) a      b 
xa18   = T b        1      (B#1)  (f(AS#a))
xa19   = T b        x      (B#x)  (f(AS#a))
xa20   = T b        a      (B#a)  (f(AS#a))

-- Tiles for transition s0 1 A R s1
xb1    = T b        (B#1)  a      (f(B#1))
xb2    = T (f(B#1)) 1      (C#1)  b
xb3    = T (f(B#1)) x      (C#x)  b
xb4    = T (f(B#1)) a      (C#a)  b
-- Tiles for transition s0 x x R s1'
xb5    = T b        (B#x)  x      (f(B#x))
xb6    = T (f(B#x)) 1      (I#1)  b
xb7    = T (f(B#x)) x      (I#x)  b
xb8    = T (f(B#x)) a      (I#a)  b

-- Tiles for transition s1 1 1 R s1
xc1    = T b        (C#1)  1      (f(C#1))
xc2    = T (f(C#1)) 1      (C#1)  b
xc3    = T (f(C#1)) x      (C#x)  b
xc4    = T (f(C#1)) a      (C#a)  b
-- Tiles for transition s1 x x R s2
xc5    = T b        (C#x)  x      (f(C#x))
xc6    = T (f(C#x)) 1      (D#1)  b
xc7    = T (f(C#x)) x      (D#x)  b
xc8    = T (f(C#x)) a      (D#a)  b

-- Tiles for transition s2 1 1 R s2
xd1    = T b        (D#1)  1      (f(D#1))
xd2    = T (f(D#1)) 1      (D#1)  b
xd3    = T (f(D#1)) x      (D#x)  b
xd4    = T (f(D#1)) a      (D#a)  b
-- Tiles for transition s2 x x R s3
xd5    = T b        (D#x)  x      (f(D#x))
xd6    = T (f(D#x)) 1      (E#1)  b
xd7    = T (f(D#x)) x      (E#x)  b
xd8    = T (f(D#x)) a      (E#a)  b

-- Tiles for transition s3 x 1 L s4
xe1    = T (f(E#x)) (E#x)  1      b 
xe2    = T b        1      (F#1)  (f(E#x))
xe3    = T b        x      (F#x)  (f(E#x))
xe4    = T b        a      (F#a)  (f(E#x))
-- Tiles for transition s3 1 1 R s3
xe5    = T b        (E#1)  1      (f(E#1))
xe6    = T (f(E#1)) 1      (E#1)  b
xe7    = T (f(E#1)) x      (E#x)  b
xe8    = T (f(E#1)) a      (E#a)  b

-- Tiles for transition s4 1 1 L s4
xf1    = T (f(F#1)) (F#1)  1      b 
xf2    = T b        1      (F#1)  (f(F#1))
xf3    = T b        x      (F#x)  (f(F#1))
xf4    = T b        a      (F#a)  (f(F#1))
-- Tiles for transition s4 x x L s5
xf5    = T (f(F#x)) (F#x)  x      b 
xf6    = T b        1      (G#1)  (f(F#x))
xf7    = T b        x      (G#x)  (f(F#x))
xf8    = T b        a      (G#a)  (f(F#x))

-- Tiles for transition s5 1 1 L s5
xg1    = T (f(G#1)) (G#1)  1      b 
xg2    = T b        1      (G#1)  (f(G#1))
xg3    = T b        x      (G#x)  (f(G#1))
xg4    = T b        a      (G#a)  (f(G#1))
-- Tiles for transition s5 x x L s6
xg5    = T (f(G#x)) (G#x)  x      b 
xg6    = T b        1      (H#1)  (f(G#x))
xg7    = T b        x      (H#x)  (f(G#x))
xg8    = T b        a      (H#a)  (f(G#x))

-- Tiles for transition s6 1 1 L s6
xh1    = T (f(H#1)) (H#1)  1      b 
xh2    = T b        1      (H#1)  (f(H#1))
xh3    = T b        x      (H#x)  (f(H#1))
xh4    = T b        a      (H#a)  (f(H#1))
-- Tiles for transition s6 A A R s0
xh5    = T b        (H#a)  a      (f(H#a))
xh6    = T (f(H#a)) 1      (B#1)  b
xh7    = T (f(H#a)) x      (B#x)  b
xh8    = T (f(H#a)) a      (B#a)  b

-- Tiles for transition s1' A A R s1',
xi1    = T b        (I#a)  a      (f(I#a))
xi2    = T (f(I#a)) 1      (I#1)  b
xi3    = T (f(I#a)) x      (I#x)  b
xi4    = T (f(I#a)) a      (I#a)  b
-- Tiles for transition s1' 1 A R s2', #read 1, go to end and write
xi5    = T b        (I#1)  a      (f(I#1))
xi6    = T (f(I#1)) 1      (J#1)  b
xi7    = T (f(I#1)) x      (J#x)  b
xi8    = T (f(I#1)) a      (J#a)  b
-- Tiles for transition s1' x x S g1,
---- Tiles for transition s1' x x R ss1'
xi9    = T b         (I#x)  x      (f(I#x))
xi10   = T (f(I#x)) 1      (IS#1)  b
xi11   = T (f(I#x)) x      (IS#x)  b
xi12   = T (f(I#x)) a      (IS#a)  b
---- Tiles for transition ss1' 1 1 L g1'
xi13   = T (f(IS#1)) (IS#1) 1      b 
xi14   = T b         1      (N#1)  (f(IS#1))
xi15   = T b         x      (N#x)  (f(IS#1))
xi16   = T b         a      (N#a)  (f(IS#1))
---- Tiles for transition ss1' x x L g1'
xi17   = T (f(IS#x)) (IS#x) x      b 
xi18   = T b         1      (N#1)  (f(IS#x))
xi19   = T b         x      (N#x)  (f(IS#x))
xi20   = T b         a      (N#a)  (f(IS#x))
---- Tiles for transition ss1' A A L g1'
xi21   = T (f(IS#a)) (IS#a) a      b 
xi22   = T b         1      (N#1)  (f(IS#a))
xi23   = T b         x      (N#x)  (f(IS#a))
xi24   = T b         a      (N#a)  (f(IS#a))

-- Tiles for transition s2' 1 1 R s2', #skip to answer
xj1    = T b        (J#1)  1      (f(J#1))
xj2    = T (f(J#1)) 1      (J#1)  b
xj3    = T (f(J#1)) x      (J#x)  b
xj4    = T (f(J#1)) a      (J#a)  b
-- Tiles for transition s2' x x R s3', #answer writing
xj5    = T b        (J#x)  x      (f(J#x))
xj6    = T (f(J#x)) 1      (K#1)  b
xj7    = T (f(J#x)) x      (K#x)  b
xj8   = T (f(J#x)) a      (K#a)  b

-- Tiles for transition s3' 1 1 R s3', #skip to end
xk1    = T b        (K#1)  1      (f(K#1))
xk2    = T (f(K#1)) 1      (K#1)  b
xk3    = T (f(K#1)) x      (K#x)  b
xk4    = T (f(K#1)) a      (K#a)  b
--Tiles for transition s3' x 1 L s4',  #right 1 and reverse
xk5    = T (f(K#x)) (K#x)  1      b 
xk6    = T b        1      (L#1)  (f(K#x))
xk7    = T b        x      (L#x)  (f(K#x))
xk8   = T b        a      (L#a)  (f(K#x))

-- Tiles for transition s4' 1 1 L s4', # skip to beginning
xl1    = T (f(L#1)) (L#1)  1      b 
xl2    = T b        1      (L#1)  (f(L#1))
xl3    = T b        x      (L#x)  (f(L#1))
xl4    = T b        a      (L#a)  (f(L#1))
-- Tiles for transition s4' x x L s5', # cross to other side
xl5    = T (f(L#x)) (L#x)  x      b 
xl6    = T b        1      (M#1)  (f(L#x))
xl7    = T b        x      (M#x)  (f(L#x))
xl8   = T b        a      (M#a)  (f(L#x))

-- Tiles for transition s5' 1 1 L s5', # go to beginning
xm1    = T (f(M#1)) (M#1)  1      b 
xm2    = T b        1      (M#1)  (f(M#1))
xm3    = T b        x      (M#x)  (f(M#1))
xm4    = T b        a      (M#a)  (f(M#1))
-- Tiles for transition s5' x x R s1',
xm5    = T b        (M#x)  x      (f(M#x))
xm6    = T (f(M#x)) 1      (I#1)  b
xm7    = T (f(M#x)) x      (I#x)  b
xm8   = T (f(M#x)) a      (I#a)  b
-- Tiles for transition s5' A A R s1',
xm9   = T b        (M#a)  a      (f(M#a))
xm10   = T (f(M#a)) 1      (I#1)  b
xm11   = T (f(M#a)) x      (I#x)  b
xm12   = T (f(M#a)) a      (I#a)  b

-- Tiles for transition g1 x x L g2,
xn1    = T (f(N#x)) (N#x)  x      b 
xn2    = T b        1      (O#1)  (f(N#x))
xn3    = T b        x      (O#x)  (f(N#x))
xn4    = T b        a      (O#a)  (f(N#x))

-- Tiles for transition g2 A 1 L g2,
xo1    = T (f(O#a)) (O#a)  1      b 
xo2    = T b        1      (O#1)  (f(O#a))
xo3    = T b        x      (O#x)  (f(O#a))
xo4    = T b        a      (O#a)  (f(O#a))
-- Tiles for transition g2 x x R s
xo5    = T b        (O#x)  x      (f(O#x))
xo6    = T (f(O#x)) 1      (A#1)  b
xo7    = T (f(O#x)) x      (A#x)  b
xo8   = T (f(O#x)) a      (A#a)  b

side [] = []
side (T l u d r:ls) = T b d d b : (side ls)

tiles = [xz1, xzx, xza, xzs, 
         xa1,xa2,xa3,xa4,xa5,xa6,xa7,xa8,xa9,xa10,xa11,xa12,xa13,xa14,xa15,xa16,xa17,xa18,xa19,xa20,
         xb1,xb2,xb3,xb4,xb5,xb6,xb7,xb8,
         xc1,xc2,xc3,xc4,xc5,xc6,xc7,xc8,
         xd1,xd2,xd3,xd4,xd5,xd6,xd7,xd8,
         xe1,xe2,xe3,xe4,xe5,xe6,xe7,xe8,
         xf1,xf2,xf3,xf4,xf5,xf6,xf7,xf8,
         xg1,xg2,xg3,xg4,xg5,xg6,xg7,xg8,
         xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,
         xi1,xi2,xi3,xi4,xi5,xi6,xi7,xi8,xi9,xi10,xi11,xi12,xi13,xi14,xi15,xi16,xi17,xi18,xi19,xi20,
         xi21,xi22,xi23,xi24,
         xj1,xj2,xj3,xj4,xj5,xj6,xj7,xj8,
         xk1,xk2,xk3,xk4,xk5,xk6,xk7,xk8,
         xl1,xl2,xl3,xl4,xl5,xl6,xl7,xl8,
         xn1,xn2,xn3,xn4,
         xm1,xm2,xm3,xm4,xm5,xm6,xm7,xm8,xm9,xm10,xm11,xm12,
         xo1,xo2,xo3,xo4,xo5,xo6,xo7,xo8
         ]

initial = [xzs,xz1,xzx,xz1,xzx] ++ repeatT xzx 20

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
