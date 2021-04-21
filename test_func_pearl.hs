import Control.Monad

data LOL a = LOL [a]
   deriving Eq

instance Ord a => Ord (LOL a) where
   LOL x <= LOL y = (length x, x) <= (length y, y)

type LOS = LOL Char

cconcat :: LOL a -> LOL a -> LOL a
cconcat (LOL x) (LOL y) = LOL (x ++ y)


infixl 6 \/
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
   LT -> x : xt\/ys
   EQ -> x : xt\/yt
   GT -> y : xs\/yt

xprod :: (Ord a, Ord b, Ord c) => (a->b->c) -> [a] -> [b] -> [c]
xprod _ [] _ = []
xprod _ _ [] = []
xprod f (x:xt) ys@(y:yt) =
   (f x y) : (xprod f [x] yt) \/ (xprod f xt ys)

cat :: [LOS] -> [LOS] -> [LOS]
cat = xprod cconcat

closure :: Ord a => (a->a->a) -> a -> [a] -> [a]
closure f z [] = [z]
closure f z xs@(x:xt) = if x==z
   then closure f z xt
   else z : xprod f xs (closure f z xs)

data Rexp = Nil
   | Eps
   | Sym Char
   | Clo Rexp
   | Cat Rexp Rexp
   | Alt Rexp Rexp

test :: Rexp
test = Cat allstuff maybec
   where
      maybec = Alt c Eps
      allstuff = Clo stuff
      stuff = Alt aorb caorcb
      aorb = Alt a b
      caorcb = Alt ca cb
      ca = Cat c a
      cb = Cat c b
      c = Sym 'c'
      b = Sym 'b'
      a = Sym 'a'

enumR :: Rexp -> [String]
enumR r = [x | (LOL x) <- enumR' r]

enumR' :: Rexp -> [LOS]
enumR' Nil = []
enumR' Eps = [LOL ""]
enumR' (Sym a) = [LOL [a]]
enumR' (Clo x) = clo (enumR' x)
enumR' (Cat x y) = cat (enumR' x) (enumR' y)
enumR' (Alt x y) = alt (enumR' x) (enumR' y)

alt :: [LOS] -> [LOS] -> [LOS]
alt = (\/)

clo :: [LOS] -> [LOS]
clo = closure cconcat (LOL "")

--Automata

type NFA = [State]
data State = State Ident Char NFA
   deriving Show
type Ident = Int

instance Eq State where
    (State n1 _ _) == (State n2 _ _) = n1 == n2

instance Ord State where
    (State n1 c1 _) <= (State n2 c2 _) = (c1,n1) <= (c2,n2)

r2n :: Rexp -> NFA
r2n r = fs \/ (bp b ds)
   where
      ds = [State 0 '~' []];
      (fs, _, b) = r2n' r 1 ds
      
bp :: Bool -> NFA -> NFA
bp True ds = ds
bp False _ = []

r2n' :: Rexp -> Ident -> NFA -> (NFA,Ident,Bool)
r2n' Nil n _ = ([], n, False)
r2n' Eps n _ = ([], n, True)
r2n' (Sym c) n ds = ([State n c ds], n+1, False)
r2n' (Alt x y) n ds = (fs\/fs', n'', b||b')
   where
      (fs, n', b) = r2n' y n ds
      (fs', n'', b') = r2n' x n' ds
r2n' (Cat x y) n ds = (fs' \/ (bp b' fs), n'', b&&b')
   where
      (fs, n', b) = r2n' y n ds
      (fs', n'', b') = r2n' x n' (fs\/(bp b ds))
r2n' (Clo x) n ds = (fs, n', True)
   where
      (fs, n', _) = r2n' x n (fs\/ds)

type MyWord = (String, NFA)

accept :: NFA -> Bool
accept ds = elem 0 [i | (State i _ _) <- ds]

visit :: [MyWord] -> [String]
visit [] = []
visit ((x,ds):ws) =
   let xs = visit (ws ++ [(c:x,ds') | (State _ c ds') <- grp ds]) in
       if accept ds
       then x:xs
       else xs

grp :: NFA -> NFA
grp [] = []
grp [x] = [x]
grp (m@(State _ c ds) : ms@((State _ c' ds') :mt)) =
   if c==c'
   then grp ((State (-1) c (ds\/ds')):mt)
   else m : grp ms

enumA :: NFA -> [String]
enumA starts = map reverse $ visit [("",starts)]

