-- Data types

data Day = Po | Ut | St | Ct | Pa | So | Ne
      deriving(Eq, Show)

-- Po -> Po
-- Po == Po -> True

-------------------------------------------------------

data Day = Po | Ut | St | Ct | Pa | So | Ne
      deriving(Eq, Ord)

-- Po < Ut -> True
-- Po > Ne -> False

-------------------------------------------------------

data Day = Po | Ut | St | Ct | Pa | So | Ne
        deriving Ord

-- Ord alone does not work; instance:

instance Eq Day3 where
        Po == Ut = True
        _  == _  = False

instance Show Day3 where
        show Po = "Pondeli"
        show _  = "Neni pondeli"

-- Example of ord on list 
instance (Ord a) => Ord [a] where
        [] <= _ = True                               -- pattern for empty list
        (_:_) <= [] = False                          -- one element in list is more than empty list
        (x:s) <= (y:t) = x < y || (x == y && s <= t) -- two lists, more elements

-------------------------------------------------------

-- instance Num !!!

-------------------------------------------------------

-- Does not work yet
class Boolable a where getBool :: a -> Bool

instance Boolable Bool where
        getBool x = x

instance Boolable Int where
        getBool 0 = False
        getBool _ = True

myIf :: Boolable a => a -> b -> b -> b
myIf x t f = if getBool x then t else f
