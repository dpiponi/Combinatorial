import Data.Ratio
import Debug.Trace

sample a = map (flip count a) [0..10]

(*!) _ 0 = 0
(*!) a b = a*b
(!*) 0 _ = 0
(!*) a b = a*b
(^+) a b = zipWith (+) a b
(^-) a b = zipWith (-) a b

~(a:as) `convolve` (b:bs) = (a *! b):
    ((map (a !*) bs) ^+ (as `convolve` (b:bs)))
compose (f:fs) (0:gs) = f:(gs `convolve` (compose fs (0:gs)))
inverse (0:f:fs) = x where x     = map (recip f *) (0:1:g)
                           _:_:g    = map negate (compose (0:0:fs) x)
invert x = r where r = map (/x0)  ((1:repeat 0) ^- (r `convolve` (0:xs)))
                   x0:xs = x 

(^/) (0:a) (0:b) = a ^/ b
(^/) a b = a `convolve` (invert b)

z :: Fractional a => [a]
z = 0:1:repeat 0

d (_:x) = zipWith (*) (map fromInteger [1..]) x

integrate x = 0 : zipWith (/) x (map fromInteger [1..])

square x = x `convolve` x

instance (Eq r, Num r) => Num [r] where
    x+y  = zipWith (+) x y
    x-y  = zipWith (-) x y
    ~x*y = x `convolve` y
    fromInteger x      = fromInteger x:repeat 0
    negate x     = map negate x
    signum (x:_) = signum x:repeat 0
    abs (x:xs)   = error "Can't form abs of a power series"

instance (Eq r, Fractional r) => Fractional [r] where
    x/y = x ^/ y
    fromRational x    = fromRational x:repeat 0

sqrt' x = 1:rs where rs = map (/2) (xs ^- (rs `convolve` (0:rs)))
                     _:xs = x
instance (Eq r, Fractional r) => Floating [r] where
    sqrt (1:x) = sqrt' (1:x)
    sqrt _      = error "Can only find sqrt when leading term is 1"
    exp x      = e where e = 1+integrate (e * d x)
    log x      = integrate (d x/x)
    sin x      = integrate ((cos x)*(d x))
    cos x      = [1] ... negate (integrate ((sin x)*(d x)))
    asin x      = integrate (d x/sqrt(1-x*x))
    atan x      = integrate (d x/(1+x*x))
    acos x      = error "Unable to form power series for acos"
    sinh x      = integrate ((cosh x)*(d x))
    cosh x      = [1] ... integrate ((sinh x)*(d x))
    asinh x      = integrate (d x/sqrt(1+x*x))
    atanh x      = integrate (d x/(1-x*x))
    acosh x      = error "Unable to form power series for acosh"
    pi       = error "There is no formal power series for pi"

t :: (Eq a, Num a) => ([a])
t = 0:1:repeat 0
t' :: [Rational]
t' = t

lead [] x = x
lead (a:as) x = a : (lead as (tail x))
a ... x = lead a x

one = t'
list x     = 1/(1-x)
set x     = exp x
ring x     = -log(1-x)
pair x     = x*x
oneOf a b   = a+b
necklace x  = -log(1-x)/2+x/2+x*x/4
union a b   = a*b

(//) :: Fractional a => [a] -> (Integer -> Bool) -> [a]
(//) a c = zipWith (\a-> \b->(if (c a :: Bool) then b else 0)) [(0::Integer)..] a

nonEmpty a = a // (/= 0)

factorial 0 = 1
factorial n = n*factorial (n-1)

count n a = numerator ((a!!(fromInteger n)) * (factorial (fromInteger n)))

tree x = p where p = [0] ... union (set p) x

graph = [2^((n*(n-1) `div` 2)) / product (map fromInteger [1..n]) | n <- [0..]] :: [Rational]

connectedGraph = 1 + log graph

delta (g : gs) h = let g' = delta gs h
                   in (0 : ((1 : h) * g')) + gs

fsqrt (0 : 1 : fs) =
    let gs = (fs-(0 : gs*((0 : delta gs gs)+((2 : gs)*(gs*g)))))/2
        g = 0 : 1 : gs
    in g

p f t = (t `compose` f) ^- t
flog :: (Eq a, Fractional a) => [a] -> [a]
flog f@(0 : 1 : _) = flog' f 1 (repeat 0) (0 : 1 : repeat 0)
         where flog' f n t z = take (n+1) t ++ (
                 drop (n+1) $
                    let pz = p f z
                    in flog' f (n+1) (t ^- map (((-1)^n / fromIntegral n) *) pz) pz)

cbrt x = exp (map (/ 3) $ log x)

fexp f@(0 : 0 : _) = fexp' f 0 t' 1
fexp' f total term n = take (n-1) total ++ drop (n-1) (fexp' f (total+term) (map (/fromIntegral n) (f*d term)) (n+1))

main = do
    -- http://oeis.org/A052132
    
    --mapM_ print $ take 20 $ (fexp (map (/2) $ flog (t'/(1-t'))))^2
    let u = map (/2) $ flog (t'/(1-t'))
    let v = fexp u
    mapM_ print $ take 20 $ (v `compose` v)
    --mapM_ print $ take 20 $ (fexp (map (/2) $ flog (t'+t'^2)))^2

    --mapM_ print $ take 20 $ fexp (-flog (sin t'))
    --mapM_ print $ take 20 $ asin t'
    --print "--"
    -- lambert w
    --mapM_ print $ take 200 $ fexp (-flog (t'*exp t'))
    --mapM_ print $ take 10 $ fexp (2*t'^2)
{-
    -- Compare http://math.stackexchange.com/questions/208996/half-iterate-of-x2c
    mapM_ print $ take 20 $ flog (t'+t'^2)
    print "---"
    -- http://math.stackexchange.com/questions/1346667/lim-limits-x-rightarrow-0-frac-sin-nx-x-fracn6x3-left-frac
    mapM_ print $ take 20 $ flog $ sin t'
    print "---"
    -- See bottom of page 14 http://www.springer.com/cda/content/document/cda_downloaddocument/9783642004490-c1.pdf?SGWID=0-0-45-767218-p173888310
    -- or top of page 8 https://www.itp.uni-hannover.de/~flohr/papers/w-cft-survivalkit.pdf
    mapM_ print $ take 20 $ flog (t'/(1-t'))
    print "---"
    mapM_ print $ take 20 $ flog $ t'/sqrt(1-2*t'^2)
    print "---"
    mapM_ print $ take 20 $ flog $ t'/cbrt(1-3*t'^3)
    -}
