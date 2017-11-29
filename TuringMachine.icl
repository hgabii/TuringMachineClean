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
fromList list =  Z [] list

read :: (Zipper a) -> a
read (Z _ list) = hd list

write :: a (Zipper a) -> Zipper a
write newElement (Z [] [y:ys]) = Z [] [newElement:ys]
write newElement (Z [x:xs] [y:ys]) = Z [x:xs] [newElement:ys]

:: Movement = Forward | Backward | Stay

move :: Movement (Zipper a) -> Zipper a
move Stay (zipper) = zipper
move Forward (Z prevElements [y:ys]) = Z [y:prevElements] (tl [y:ys])
move Backward (Z [x:xs] nextElements) = Z (tl [x:xs]) [x:nextElements]

around :: Int (Zipper a) -> [a]
around radius (Z prevElemets nextElements) = 
	(reverse (take radius prevElemets)) ++ (take (radius + 1) nextElements)

fromListInf :: a [a] -> Zipper a
fromListInf repeatingElement list = 
	Z (repeat repeatingElement) (list ++ repeat repeatingElement) 

class Machine t where
	done :: (t a) -> Bool
	tape :: (t a) -> Zipper a
	step :: (t a) -> t a
	
:: State = InState Int | Accepted | Rejected

:: TuringMachine a = TM State (Zipper a) (Int a -> (State, a, Movement))

instance Machine TuringMachine where
	done (TM (InState _) _ _) = False
	done (TM Accepted _ _) = True
	done (TM Rejected _ _) = True
	
	tape (TM _ (Z prevElements nextElements) _) = Z prevElements nextElements
	
	step (TM (InState s) (Z prevElements nextElements) f) = 
		tapeStep (f s (read (Z prevElements nextElements)))
		where
			//tapeStep :: (State, a, Movement) -> TuringMachine a
			tapeStep (state, newSymbol, direction) = 
				TM state ( move direction (write newSymbol (Z prevElements nextElements)) ) f

run :: (t a) -> [t a] | Machine t
run test = [replicate test]
	where
		replicate :: (TuringMachine a) -> TuringMachine a
		replicate asd = asd

// ############

Test :: [Int] -> Int
Test list = 1


Start = [test_fromList, test_read, test_write, test_move, test_around, test_fromListInf,
	 test_done, test_tape, test_step, test_run]

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
	
test_done =
	[ not (done (TM (InState 0) undef undef))
	, done (TM Accepted undef undef)
	, done (TM Rejected undef undef)
	]
	
test_tape =
	[ tape (TM Accepted (fromList [1..5]) undef) === fromList [1..5]
	]
	
test_step =
	[ let m = step (TM (InState 0) (fromList ['a','b']) f)
	in  not (done m)
	&& tape m === Z ['b'] ['b']
	, let m = step (TM (InState 0) (fromList ['b','b']) f)
	in  not (done m)
	&& tape m === Z ['a'] ['b']
	, let m = step (TM (InState 1) (fromList ['a','b']) f)
	in  done m
	&& tape m === fromList ['x','b']
	]
	where
		f 0 'a' = (InState 0, 'b', Forward)
		f 0 'b' = (InState 0, 'a', Forward)
		f 1 _   = (Accepted,  'x', Stay)
	
test_run =
  [ let m = last (run (tm ['a','b','x','x']))
    in done m
       && tape m === Z ['x','a','b'] ['x']
  , let m = last (run (tm ['b','a','x','x']))
    in done m
       && tape m === Z ['x','b','a'] ['x']
  , let m = last (run (tm ['a','b','x','a']))
    in done m
       && tape m === Z ['x','a','b'] ['!']
  ]
  where
    tm xs = TM (InState 0) (fromList xs) f
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 0 'x' = (InState 1, 'x', Forward)
    f 1 'x' = (Accepted,  'x', Stay)
    f _ ch  = (Rejected,  '!', Stay)

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	