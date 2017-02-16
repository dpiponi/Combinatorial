\section{Introduction}
A popular question in mathematics is this: given a function $f$, what is its "square root" $g$ in the sense that $g(g(x)) = f(x)$.
There are many questions about this on [mathoverflow](http://mathoverflow.net/questions/tagged/fractional-iteration) but it's also a popular subject in mathematics forums for non-experts.
This question seems to have a certain amount of notoriety because it's easy to ask but hard to answer fully.
I want to look at an approach that works nicely for formal power series, following from the Haskell code I wrote [here](http://blog.sigfpe.com/2007/11/small-combinatorial-library.html).
There are some methods for directly finding "functional square roots" for formal power series that start as $z+a_2z^2+a_3z^3+\ldots$, but I want to approach the problem indirectly.
When working with real numbers we can find square roots, say, by using $\sqrt{x}=\exp(\frac{1}{2}\log{x})$.
I want to use an analogue of this for functions.
So my goal is to make sense of the idea of the logarithm and exponential of a formal power series as composable functions.
Warning: the arguments are all going to be informal.

\section{Notation}
There's potential for a lot of ambiguous notation here, especially as the usual mathematical notation for $n$th powers of trig functions is so misleading.
I'm going to use $\circ$ for composition of functions and power series, and I'm going to use the notation $f^{\circ n}$ to mean the $n$th iterate of $f$.
So $f^{n+1}(x) = f(x)f^n(x)$ and $f^{\circ n+1}(x) = f(f^{\circ n}(x))$.
As I'll be working mostly in the ring of formal power series $R[\![z]\!]$ for some ring $R$, I'll reserve the variable $z$ to refer only to the corresponding element in this ring.
I'll also use formal power series somewhat interchangeably with functions. So $z$ can be thought of as representing the identity function.
To make sure we're on the same page, here are some small theorems in this notation:
\begin{enumerate}
\item{$z^mz^n = z^{m+n}$}
\item{$z^{\circ m}\circ z^{\circ n} = z^{\circ mn}$.}
\item{$(1+z)^n = \sum_{i=0}^n{n\choose i}z^n$}
\item{$(1+z)^{\circ n}=n+z$.}
!
That last one simply says that adding one $n$ times is the same as adding $n$.

As I'm going to have ordinary logarithms and exponentials sitting around, as well as functional logarithms and exponentials, I'm going to introduce the notation $\operatorname{LOG}$ for functional logarithm and $\operatorname{EXP}$ for functional exponentiation.

\section{Preliminaries}
The first goal is to define a non-trivial function $\operatorname{LOG}$ with the fundamental property that $\operatorname{LOG}(f^{\circ n})=n\operatorname{LOG}(f)$

First, let's note some basic algebraic facts.
The formal power series form a commutative ring with operations $+$ and $\cdot$ (ordinary multiplication) and with additive identity $0$ and multiplicative identity $1$.
The formal power series form a ring-like algebraic structure with operation $+$ and partial operation $\circ$ with additive identity $0$ and multiplicative identity $z$.
But it's not actually ring or even a [near-ring](https://en.wikipedia.org/wiki/Near-ring).
Composition isn't defined for all formal power series and even when it's defined, we don't have distributivity.
For example, in general $f\circ(g+h)\ne f\circ g+f\circ h$, after all there's no reason to expect $f(g(x)+h(x))$ to equal $f(g(x))+f(h(x))$.
We do have right-distributivity however, i.e.
\[
$(f+g)\circ h = f\circ g+f\circ h$,
\]
because
\[
$(f+g)(h(x))=f(h(x))+g(h(x))$,
\]
more or less by definition of $+$.

\section{We can't use power series on our power series}
There's an obvious approach, just use power series of power series.
So we might tentatively suggest that
\[
$\operatorname{LOG}(z+f) = f-\frac{1}{2}f^{\circ 2}+\frac{1}{3}f^{\circ 3}+\ldots$.
\]
Note that I consider $\operatorname{LOG}(z+f)$ rather than $\operatorname{LOG}(1+f)$ because $z$ is the multiplicative identity in our ring-like structure.

Unfortunately this doesn't work.
The reason is this: if we try to use standard reasoning to show that the resulting function has the fundamental property we seek we end up using distributivity.
We don't have distributivity.

\section{Sleight of hand}
There's a beautiful trick I spotted on mathoverflow recently that allows us to bring back distributivity.
(I can't find the trick again, but when I do I'll come back and add a link and credit here.)
Consider the function $R(g)$ defined by $R(g)(f) = f\circ g$.
In other words $R(g)$ is right-composition by $g$.
(Ambiguity alert, I'm using $R$ here to mean \it{right}.
It has nothing to do with the ring underlying our formal power series.)
Because we have right-distributivity, $R(g)$ is a \it{bona fide} linear operator on the space of formal power series.
If you think of formal power series as being infinitely long vectors of coefficients then $R(g)$ can be thought of as an infinitely sized matrix.
This means that as long as we have convergence, we can get away with using power series to compute $\log R(g)$ with the property that $\log(R(g)^n) = n\log R(g)$.
Define:
\[
$\operator{LOG}(f) = \log(R(f))z$.
\]
We have:
\[
$\operator{LOG}(f) = \log(R(f))z = \log(1+(R(f)-1))z$
\]
where I'm using $1$ to mean the identity linear operator.
And now have:
\[
$\operator{LOG}(f) = (R(f)-1)z-\frac{1}{2}(R(f)-1)^2z+\frac{1}{3}(R(f)-1)^3z+\ldots$.
\]
But does it converge?
Suppose $f$ is of the form $x+a_2x^2+a_3x^3+\ldots$.
Then $(R(f)-1)g = g\circ f-g$.
The leading term in $g\circ f$ is the same as the leading term in $g$.
So $R(f)-1$ kills the first term of whatever it is applied to, which means that when we sum the terms in $\operatorname{LOG}(f)$, we only need $n$ to get a power series correct to $n$ coefficients.
Reusing my code from [here](http://blog.sigfpe.com/2007/11/small-combinatorial-library.html), I call $\operatorname{LOG}$ by the name -|flog|-.
Here is its implementation:

> import Data.Ratio

> flog :: (Eq a, Fractional a) => [a] -> [a]
> flog f@(0 : 1 : _) =
>   flog' 1 (repeat 0) (0 : 1 : repeat 0)
>      where flog' n total term = take (n+1) total ++ (
>              drop (n+1) $
>                 let pz = p term
>                 in flog' (n+1) (total-map (((-1)^n / fromIntegral n) *) pz) pz)
>            p total = (total ○ f) - total

The -|take|- and -|drop|- are how I tell Haskell when the first $n+1$ coefficients have been exactly computed and so no more terms are necessary.

Does it work?

Here's an example using the twice iterated sin function:

> ex1 = do
>   let lhs = flog (sin (sin z))
>   let rhs = 2*flog (sin z)
>   mapM_ print $ take 20 (lhs-rhs)

Works to 20 coefficients. Dare we try an inverse function?

> ex2 = do
>   let lhs = flog (sin z)
>   let rhs = flog (asin z)
>   mapM_ print $ take 20 (lhs+rhs)

Seems to work!

\section{Exponentials}
It's no good having logarithms if we can't invert them.
One way to think about the exponential function is that
\[
$\exp(x) = \lim_{n\rightarrow \infty}(1+\frac{x}{n})^n$
\]
We get better and better approximations by writing the expression inside the limit as a product of more and more terms.
We can derive the usual power series for $\exp$ from this, but only if right-distributivity holds.
So let's try to use the above expression directly:
\[
$\operatorname{EXP}(f) = \lim_{n\rightarrow \infty}(z+\frac{f}{n})^{\circ n}$
\]
and get
\[
$\operatorname{EXP}(f) = \lim_{n\rightarrow \infty}R(z+\frac{f}{n})^nz$.
\]
Unfortunately, even though $R(g)$ is linear, $R$ itself isn't.
So it's going to take some extra work to raise $R(z+f/n)$ to the power of $n$.

The good news is that we're dealing with the special case $R(z+\epsilon)$ where $\epsilon$ is something small.
We have
\[
$R(z+\epsilon)f=f(z+\epsilon)=f(z)+\epsilon\frac{df}{dz}+O(\epsilon^2)$.
\]
So $R(z+f/n)$ is actually $1+\frac{1}{n}f\frac{d}{dz}$ modulo higher order terms.
This gives us
\[
$\operatorname{EXP}(f) = \lim_{n\rightarrow \infty}(1+\frac{1}{n}f\frac{d}{dz})^nz=\exp(f\frac{d}{dz})z$.
\]
This is something we can implement using the power series for ordinary $\exp$:
\[
$\operatorname{EXP}(f) = z+f+\frac{1}{2!}f\frac{df}{dz}+\frac{1}{3!}f\frac{d}{dz}(f\frac{df}{dz})+\ldots$.
\]
In code that becomes:

> fexp f@(0 : 0 : _) = fexp' f 0 z 1
> fexp' f total term n = take (n-1) total ++ drop (n-1)
>           (fexp' f (total+term) (map (/fromIntegral n) (f*d term)) (n+1))

Note how when we differentiate a power series we shift the coefficients down by one place.
To counter the effect of that so as to ensure convergence we need $f$ to look like $a_2z^2+a_3a^3+\ldots$.
Luckily this is exactly the kind of series $\operatorname{LOG}$ gives us.

But does it successfully invert $\operatorname{LOG}$?
Let's try:

> ex3 = do
>   let lhs = sin z
>   let rhs = fexp (flog (sin z))
>   mapM_ print $ take 20 (lhs-rhs)

Now we can start computing fractional iterates.
Square root first:

> ex4 = do
>   mapM_ print $ take 20 $ fexp (flog (sin z)/2)

That matches the results at [A048602](http://oeis.org/A048602) and [A048603](http://oeis.org/A048603).

Cube root:

> ex5 = do
>   mapM_ print $ take 20 $ fexp (flog (sin z)/3)

Matches [A052132](http://oeis.org/A052132) and [A052135](http://oeis.org/A052135).

And this gives an alternative to Lagrange inversion for computing power series for inverse functions:

> ex6 = do
>   let lhs = fexp (-flog (sin z))
>   let rhs = asin z
>   mapM_ print $ take 20 (lhs-rhs)

\section{What's really going on with $\operatorname{EXP}$?}
Let's approach $\operatorname{EXP}$ in a slightly different way.
In effect, $\operatorname{EXP}$ is the composition of $n$ lots of $z+\frac{f}{n}$ with $z$.
So let's try composing these one at a time, with one composition every $\frac{1}{n}$ seconds.
After one second we should have our final result.
We can write this as:
\[
$g(0) = z$ and $g(t+\frac{1}{n}) = g(t)+\frac{1}{n}f(g(t))$ to first order.
\]
So we're solving the differential equation:
\[
$g(0) = z$ and $\frac{dg}{dt} = f(g(t))$
\]
with $\operatorname{EXP}(g) = g(1)$.

So $\operatorname{EXP}$ is the function that solves one of the most fundamental differential equations.
This also means I can use Mathematica to solve symbolically and check my results.
For example, Mathematica says that the solution to
\[
$\frac{dg}{dt}=sin(g(t))^2$ and $g(0)=x$
\]
at $t=1$ is
\[
$g(1) = \frac{\tan z}{1-\tan z}$
\]
so let's check:

> ex7 = do
>   let lhs = fexp ((sin z)^2)
>   let rhs = atan (tan z/(1-tan z))
>   mapM_ print $ take 20 (lhs-rhs)

I like this example because it leads to the generalized Catalan numbers [A004148](http://oeis.org/A004148):

> ex8 = do
>     mapM_ print $ take 20 $ fexp (z^2/(1-z^2))

That suggests this question: what does $\operatorname{EXP}$ mean combinatorially?
I don't have a straightforward answer but solving this class of differential equation motivated the original introduction, by Cayley, of the abstract notion of a tree.
See [here](https://arxiv.org/abs/1512.00906).

\section{What is going on geometrically?}
For those who know some differential geometry,
The differential equation
\[
$g(0) = z$ and $\frac{dg}{dt} = f(g(t))$
\]
describes a flow on the real line (or complex plane).
You can think of $f$ as being a one-dimensional vector field describing how points move from time $t$ to $t+dt$.
When we solve the differential equation we get [integral curves](https://en.wikipedia.org/wiki/Integral_curve) that these points follow and $\operatorname{EXP}$ tells us where the points end up after one unit of time.
So $\operatorname{EXP}$ is the [exponential map](https://en.wikipedia.org/wiki/Exponential_map_%28Riemannian_geometry%29).
In fact, $\operatorname{EXP}(f)=\exp(f\frac{d}{dz})z$ is essentially the exponential of the vector field $f\frac{d}{dz}$ where we're now using the differential geometer's notion of a vector field as a differential operator.

\section{Final word}
Unfortunately the power series you get from using $\operator{LOG}$ and $\operator{EXP}$ don't always have good convergence properties.
For example, I'm not sure but I think the series for $\sin^{\circ 1/2} z$ has radius of convergence zero.
If you truncate the series you get a half-decent approximaion to a square root in the vicinity of the origin, but the approximation gets worse, not better, if you use more terms.

\section{And the rest of the code}

> (*!) _ 0 = 0
> (*!) a b = a*b
> (!*) 0 _ = 0
> (!*) a b = a*b
> (^+) a b = zipWith (+) a b
> (^-) a b = zipWith (-) a b

> ~(a:as) ⊗ (b:bs) = (a *! b):
>     ((map (a !*) bs) ^+ (as ⊗ (b:bs)))
> (○) (f:fs) (0:gs) = f:(gs ⊗ (fs ○ (0:gs)))
> inverse (0:f:fs) = x where x     = map (recip f *) (0:1:g)
>                            _:_:g    = map negate ((0:0:fs) ○ x)
> invert x = r where r = map (/x0)  ((1:repeat 0) ^- (r ⊗ (0:xs)))
>                    x0:xs = x 

> (^/) (0:a) (0:b) = a ^/ b
> (^/) a b = a ⊗ (invert b)

> z :: [Rational]
> z = 0:1:repeat 0

> d (_:x) = zipWith (*) (map fromInteger [1..]) x

> integrate x = 0 : zipWith (/) x (map fromInteger [1..])

> instance (Eq r, Num r) => Num [r] where
>     x+y  = zipWith (+) x y
>     x-y  = zipWith (-) x y
>     ~x*y = x ⊗ y
>     fromInteger x      = fromInteger x:repeat 0
>     negate x     = map negate x
>     signum (x:_) = signum x : repeat 0
>     abs (x:xs)   = error "Can't form abs of a power series"

> instance (Eq r, Fractional r) => Fractional [r] where
>     x/y = x ^/ y
>     fromRational x    = fromRational x:repeat 0

> sqrt' x = 1 : rs where rs = map (/2) (xs ^- (rs ⊗ (0:rs)))
>                        _ : xs = x
> instance (Eq r, Fractional r) => Floating [r] where
>     sqrt (1 : x) = sqrt' (1 : x)
>     sqrt _  = error "Can only find sqrt when leading term is 1"
>     exp x   = e where e = 1+integrate (e * d x)
>     log x   = integrate (d x/x)
>     sin x   = integrate ((cos x)*(d x))
>     cos x   = [1] ... negate (integrate ((sin x)*(d x)))
>     asin x  = integrate (d x/sqrt(1-x*x))
>     atan x  = integrate (d x/(1+x*x))
>     acos x  = error "Unable to form power series for acos"
>     sinh x  = integrate ((cosh x)*(d x))
>     cosh x  = [1] ... integrate ((sinh x)*(d x))
>     asinh x = integrate (d x/sqrt(1+x*x))
>     atanh x = integrate (d x/(1-x*x))
>     acosh x = error "Unable to form power series for acosh"
>     pi      = error "There is no formal power series for pi"

> lead [] x = x
> lead (a:as) x = a : (lead as (tail x))
> a ... x = lead a x

> (//) :: Fractional a => [a] -> (Integer -> Bool) -> [a]
> (//) a c = zipWith (\a-> \b->(if (c a :: Bool) then b else 0)) [(0::Integer)..] a

A direct functional square root that doesn't use $\operatorname{LOG}$ and $\operatorname{EXP}$:

> fsqrt (0 : 1 : fs) =
>     let gs = (fs-(0 : gs*((0 : delta gs gs)+((2 : gs)*(gs*g)))))/2
>         g = 0 : 1 : gs
>         delta (g : gs) h = let g' = delta gs h
>                    in (0 : ((1 : h) * g')) + gs
>     in g
