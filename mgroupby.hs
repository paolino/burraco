import Control.Monad.Writer
data C a = C {v::[a], f:: a -> [a] -> Writer [()] (C a) }

c :: Eq a => a -> [C a ] -> [C a]
c x ys = let (ys',r) = runWriter $ mapM (\(C zs f) -> f x zs) ys in 
	if null r then let 	cs = [x] 
				f x' xs = if x == x' then do
					tell [()]
					return (C (x':xs) f)
						else return (C xs f)
			in C cs f: ys'
		else ys'

mgroup :: Eq a => [a] -> [[a]]
mgroup = map v . foldr c []  
