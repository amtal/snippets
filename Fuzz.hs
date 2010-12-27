{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- water temperature in degrees
data Water = Scalding | Hot | Warm | Cold 
    deriving (Eq,Ord,Enum,Bounded,Show,Read)
instance Fuzz Water where
    membership = [ (Cold, 0)
                 , (Warm, 30)
                 , (Hot, 60)
                 , (Scalding, 100)
                 ]

data HotValve = ShutOff | Decrease | Stay | Increase
    deriving (Eq,Ord,Enum,Bounded,Show)
instance Fuzz HotValve where
    membership = [ (ShutOff, (-90)) -- degrees/second?
                 , (Decrease, (-10))
                 , (Stay, 0)
                 , (Increase, 10)
                 ] 

trivialCS :: Fuzzy Water -> Fuzzy HotValve
trivialCS = fmap rule where
    rule Scalding = ShutOff
    rule Hot = Decrease
    rule Warm = Stay
    rule Cold = Increase

-----------------
-- IMPLEMENTATION:

-- problem with defining memberships via instances, is you can't
-- tweak them at runtime, for GA optimization or whatever

class (Eq f, Enum f) => Fuzz f where
    membership :: [(f,Double)]

data Fuzzy a = Fuzzy [(a,Double)] deriving (Show)

fuzzify :: Fuzz f => Double -> Fuzzy f
fuzzify n = Fuzzy . fst $ members where
    -- check every interval between member function peaks
    members = foldl fun ([], head membership) (tail membership)
    fun (acc,(name',val')) nxt@(name,val)
        -- if the value's inside, produce two memberships
        | val'<=n && n<=val = let
            list = one:two:acc
            one = (name', slope n val val')
            two = (name, slope n val' val)
                              in
            (list,nxt)
        | otherwise = (acc,nxt)
    -- straight-line interpolation between 1 and 0
    slope n top btm = (n-top)/(btm-top)

-- now need a way to do (a->b->c) -> Fuzzy a -> Fuzzy b -> Fuzzy c
-- a Rule class, maybe?
-- Fuzzy a -> Fuzzy b is a functor
instance Functor Fuzzy where
    fmap f (Fuzzy ms) = Fuzzy $ map (\(a,n)->(f a,n)) ms


-- then implement defuzzify
-- Map would simplify this code greatly :\
defuzzify :: Fuzz f => Fuzzy f -> Double
defuzzify (Fuzzy ms) = sum (zipWith (*) coa areas) / sum areas where
    coa :: [Double]
    coa = zipWith f distances' (tail distances') where
        f l r = l + (r-l)/2
    areas :: [Double]
    areas = zipWith (*) ms' widths where
        ms' = map (maybe 0 id) maybeMs
        maybeMs = map (flip lookup ms) (map fst membership)
    -- distances between adjacent memberships
    distances = zipWith (-) m (tail m) where 
        m =  map snd (membership `asTypeOf` ms)
    -- distances including last and first memberships, assuming
    -- they are symmetic
    distances' :: [Double]
    distances' = (head distances):distances++(tail distances)
    -- the widths of membership functions, assuming symmetry for
    -- first and last ones
    widths :: [Double]
    widths = zipWith (+) distances' (tail distances')

