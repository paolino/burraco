newtype Value = Value Double deriving (Eq,Show,Num,NFData)
mkValue = Value . press
	where press x = (pi/2 + atan x)/pi

type Feature = [(Cards, Value)]

