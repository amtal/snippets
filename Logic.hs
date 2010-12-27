import Prelude hiding ((&&), (||), not)

class Logic a where
    (&&)::a->a->a
    (||)::a->a->a
    not::a->a

instance Logic Bool where
    True && True = True
    _    && _    = False
    False || False = False
    _     || _     = True 
    not True  = False
    not False = True

data Ternary = T
             | F
             | Unk deriving (Eq,Ord,Enum,Bounded,Read,Show)

instance Logic Ternary where
    T && T = T
    F && _ = F
    _ && F = F
    -- a single true argument with an unknown is undeterminable
    -- the unknown argument might turn out to be true...
    _ && _ = Unk 

    F || F = F
    T || _ = T
    _ || T = T
    -- unknowns mixed with false are unknown, since there might
    -- be a true
    _ || _ = Unk

    not T = F
    not F = T
    not Unk = Unk


-- Ternary logic usage example:

isFire :: Ternary -- ^ alarm went off
       -> Ternary -- ^ alarm is being tested
       -> Ternary -- ^ smoke spotted
       -> Ternary -- ^ is there a fire?
isFire alarm alarmTest smoke = (alarm && (not alarmTest)) 
                            || smoke


newtype IndepProb = P Double -- ^ independant probability
                        deriving (Eq,Ord,Show)
instance Logic IndepProb where
    (P a) && (P b) = P $ a * b
    (P a) || (P b) = P $ a + b - a*b
    not (P a) = P $ 1 - a
