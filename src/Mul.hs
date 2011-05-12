module Mul where
import Wang

-- Wang tiles for multiplication
-- Converted from the the Turing machine on http://people.oregonstate.edu/~gopinatr/turing/tm.html

x = 2 
a = 3 

q0 = T b 0 b 0 
q1 = T b 1 b 1 
qs = T b b (Q0#0) b
q00x = T b (Q0#0) 0 (f(Q0#0)) 
q00y = T (f(Q0#0)) 0 (Q3#0) b 
q00z = T (f(Q0#0)) 1 (Q3#1) b 
q31x = T b (Q3#1) 1 (f(Q3#1)) 
q31y = T (f(Q3#1)) 0 (Q3#0) b 
q31z = T (f(Q3#1)) 1 (Q3#1) b 
q30x = T (f(Q3#0)) (Q3#0) 0 b 
q30y = T b 0 (Q4#0) (f(Q3#0)) 
q30z = T b 1 (Q4#1) (f(Q3#0)) 
q41x = T (f(Q4#1)) (Q4#1) 1 b 
q41y = T b 0 (Q5#0) (f(Q4#1)) 
q41z = T b 1 (Q5#1) (f(Q4#1)) 
q51x = T (f(Q5#1)) (Q5#1) 1 b 
q51y = T b 0 (Q6#0) (f(Q5#1)) 
q51z = T b 1 (Q6#1) (f(Q5#1)) 
q50x = T b (Q5#0) 0 (f(Q5#0)) 
q50y = T (f(Q5#0)) 0 (Q50#0) b 
q50z = T (f(Q5#0)) 1 (Q50#1) b 
q61x = T (f(Q6#1)) (Q6#1) 1 b 
q61y = T b 0 (Q6#0) (f(Q6#1)) 
q61z = T b 1 (Q6#1) (f(Q6#1)) 
q60x = T b (Q6#0) 0 (f(Q6#0)) 
q60y = T (f(Q6#0)) 0 (Q7#0) b 
q60z = T (f(Q6#0)) 1 (Q7#1) b 
q71x = T b (Q7#1) 0 (f(Q7#1)) 
q71y = T (f(Q7#1)) 0 (Q8#0) b 
q71z = T (f(Q7#1)) 1 (Q8#1) b 
q81x = T b (Q8#1) 1 (f(Q8#1)) 
q81y = T (f(Q8#1)) 0 (Q8#0) b 
q81z = T (f(Q8#1)) 1 (Q8#1) b 
q80x = T b (Q8#0) 0 (f(Q8#0)) 
q80y = T (f(Q8#0)) 0 (Q21#0) b 
q80z = T (f(Q8#0)) 1 (Q21#1) b 
q351y = T (f(Q35#1)) 0 (Q36#0) b
q351z = T (f(Q35#1)) 1 (Q36#1) b
q361x = T (f(Q36#1)) (Q36#1) 0 b
q361y = T b 0 (Q4#0) (f(Q36#1))
q361z = T b 1 (Q4#1) (f(Q36#1))
q501x = T b (Q50#1) 0 (f(Q50#1))
q501y = T (f(Q50#1)) 0 (Q51#0) b
q501z = T (f(Q50#1)) 1 (Q51#1) b
q510x = T b (Q51#0) 0 (f(Q51#0))
q510y = T (f(Q51#0)) 0 (Q52#0) b
q510z = T (f(Q51#0)) 1 (Q52#1) b
q521x = T b (Q52#1) 0 (f(Q52#1))
q521y = T (f(Q52#1)) 0 (Q52#0) b
q521z = T (f(Q52#1)) 1 (Q52#1) b
q520x = T (f(Q52#0)) (Q52#0) 1 b
q520y = T b 0 (Q100#0) (f(Q52#0))
q520z = T b 1 (Q100#1) (f(Q52#0))

side [] = []
side (T l u d r:ls) = T b d d b : (side ls)

tiles = [
    q0, q1, qs, q00x, q00y, q00z, q31x, q31y, q31z, q30x, q30y, q30z, q41x, q41y, q41z, q51x, q51y, q51z, q50x, q50y,
    q50z, q61x, q61y, q61z, q60x, q60y, q60z, q71x, q71y, q71z, q81x, q81y, q81z, q80x, q80y, q80z, q211x, q211y, q211z, q221x,
    q221y, q221z, q220x, q220y, q220z, q231x, q231y, q231z, q230x, q230y, q230z, q240x, q240y, q240z, q251x, q251y, q251z, q250x, q250y, q250z,
    q261x, q261y, q261z, q271x, q271y, q271z, q270x, q270y, q270z, q260x, q260y, q260z, q310x, q310y, q310z, q321x, q321y, q321z, q320x, q320y,
    q320z, q331x, q331y, q331z, q341x, q341y, q341z, q340x, q340y, q340z, q350x, q350y, q350z, q351x, q351y, q351z, q361x, q361y, q361z, q501x,
    q501y, q501z, q510x, q510y, q510z, q521x, q521y, q521z, q520x, q520y, q520z]


initial = [q0, qs, q1, q1, q1, q0, q1, q1, q1, q1, q0] ++ repeatT q0 20

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

