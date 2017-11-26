module TuringMachine

import StdEnv, StdLib, StdGeneric, GenEq

:: Zipper a = Z [a] [a]

derive gEq Zipper

instance == (Zipper a) | == a
where
	(==) (Z prevElemetsA nextElementsA) (Z prevElementsB nextElementsB) 
		= prevElemetsA == prevElementsB && nextElementsA == nextElementsB

fromList :: [a] -> Zipper a
fromList [] = Z [] []
fromList [x:xs] =  Z [] [x:xs]

read :: (Zipper a) -> a
read (Z [] [y:ys]) = y
read (Z [x:xs] [y:ys]) = y

write :: a (Zipper a) -> Zipper a
write newElement (Z [] [y:ys]) = Z [] [newElement:ys]
write newElement (Z [x:xs] [y:ys]) = Z [x:xs] [newElement:ys]

:: Movement = Forward | Backward | Stay

move :: Movement (Zipper a) -> Zipper a
move Stay (a) = a
move Forward (Z [x:xs] [y:ys]) = Z [y:x:xs] (tl [y:ys])
move Backward (Z [x:xs] [y:ys]) = Z (tl [x:xs]) [x:y:ys]

around :: Int (Zipper a) -> [a]
around r (Z a b) = (reverse (take r a)) ++ (take (r + 1) b)

fromListInf :: a [a] -> Zipper a
fromListInf e a = Z (repeat e) (a ++ repeat e)

class Machine t where
	done :: (t a) -> Bool
	tape :: (t a) -> Zipper a
	step :: (t a) -> t a
	
:: State = InState Int | Accepted | Rejected

:: TuringMachine a = TM State (Zipper a) (Int a -> (State, a, Movement))

// ############

Start = [test_fromList, test_read, test_write, test_move, test_around, test_fromListInf]

test_fromList =
	[ fromList empty === Z [] []
	, fromList [1] === Z [] [1]
	, fromList [1..10] === Z [] [1..10]
	]
	where
		empty :: [Int]
		empty = []
		
test_read =
	[ read (Z [] [1]) == 1
	, read (Z [] [2..]) == 2
	, read (Z [1..] [3..]) == 3
	]
	
test_write =
	[ write 9 (Z [] [1]) === Z [] [9]
	, write 9 (Z [] [1..3]) === Z [] [9,2,3]
	, write 9 (Z [4..6] [1..3]) === Z [4..6] [9,2,3]
	]
	
test_move =
	[ move Stay (Z empty []) === Z [] []
	, move Stay (Z [1,2,3] [4,5,6]) === Z [1,2,3] [4,5,6]
	, move Forward (Z [1,2,3] [4,5,6]) === Z [4,1,2,3] [5,6]
	, move Backward (Z [1,2,3] [4,5,6]) === Z [2,3] [1,4,5,6]
	]
	where
		empty :: [Int]
		empty = []
		
test_around =
	[ around 0 (Z [] [1]) == [1]
	, around 3 (Z [1..] [0..]) == [3,2,1,0,1,2,3]
	]
	
test_fromListInf =
	[ let (Z xs ys) = fromListInf 0 [1..5]
	in take 100 xs == repeatn 100 0
	&& take 100 ys == [1..5] ++ repeatn 95 0
	]