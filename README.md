# TuringMachineClean
Homework in Clean programming language for ELTE Functional Programming course

<div style="margin-left:0px; background: lightgray"><h1 id="beadandó-turing-gépek">2. beadandó: Turing-gépek</h1>
<p><em>A feladatok egymásra épülnek, ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.</em></p>
<p><em>Az egyes feladatokhoz a tesztesetek logikai értékekből álló listaként adottak. Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!</em></p>
<p>A feladatban Turing-gépek szimulációját, és ehhez a zipper adatszerkezetet fogjuk megvalósítani.</p>
<p>Az alábbi importált modulokkal érdemes dolgozni: <code>StdEnv, StdLib, StdGeneric, GenEq</code>.</p>
<h2 id="zipper-adattípus">Zipper adattípus</h2>
<p>A Turing-gép szalagjának szimulációjához olyan lista jellegű adatszerkezetre van szükségünk, amelyben a balra és jobbra léptetés egyszerűen megvalósítható. A zipper alkalmazása esetén három részre bontjuk a listát: egy kijelölt, fókusz alatt álló elemre, és az ettől balra és jobbra lévő elemek listájára.</p>
<p>Ezt két listával fogjuk leírni. Az első a fókusz előtti elemek listája, fordított sorrendben. A fordított sorrend miatt ha balra akarunk lépni, csak az első lista fejelmét kell áttennünk a második listába. A második lista a kijelölt elem és az utána lévő elemek listája. Így jobbra lépésnél pedig a második lista fejelemét helyezzük át az első lista elejére.</p>
<p>Például az <code>[1, 2, 3, 4, 5, 6]</code> listát a <code>[3, 2, 1]</code> és <code>[4, 5, 6]</code> listával fogjuk leírni, ha a <code>4</code> a kijelölt elem.</p>
<p>Mivel a két lista végtelen is lehet, a Turing-gépek két irányban végtelen szalagját nem kell külön megvalósítanunk.</p>
<p>Definiáljuk a <code>Zipper</code> adattípust, amely egy <code>a</code> típusváltozóval paraméterezett! Az adatkonstruktora legyen <code>Z</code>, tároljunk vele két <code>[a]</code> típusú listát! Vezessük le a generikus egyenlőségvizsgálatot (<code>gEq</code> generikus függvény) a <code>Zipper</code> típusra!</p>
<p>FONTOS: <em>az adatkonstruktort tényleg <code>Z</code>-nek nevezzük el, különben nem működik az automatikus tesztelés!</em></p>
<h2 id="zipper-létrehozása-listából">Zipper létrehozása listából</h2>
<p>Definiáljuk a <code>fromList</code> függvényt, amely egy listából létrehoz egy olyan zippert, amely a lista első elemére fókuszál!</p>
<p>A függvény típusa:</p>
<pre><code>fromList :: [a] -&gt; Zipper a</code></pre>
<p>Tesztesetek:</p>
<pre><code>test_fromList =
  [ fromList empty   === Z [] []
  , fromList [1]     === Z [] [1]
  , fromList [1..10] === Z [] [1..10]
  ]
  where
    empty :: [Int]
    empty = []</code></pre>
<h2 id="kijelölt-elem-lekérdezése">Kijelölt elem lekérdezése</h2>
<p>Definiáljuk a <code>read</code> függvényt, amely a zipper kijelölt elemét adja vissza! Üres zipper esetén nem kell működnie a függvénynek.</p>
<p>A függvény típusa:</p>
<pre><code>read :: (Zipper a) -&gt; a</code></pre>
<p>Tesztesetek:</p>
<pre><code>test_read =
  [ read (Z [] [1])      == 1
  , read (Z [] [2..])    == 2
  , read (Z [1..] [3..]) == 3
  ]</code></pre>
<h2 id="kijelölt-elem-módosítása">Kijelölt elem módosítása</h2>
<p>Definiáljuk a <code>write</code> függvényt, amellyel a zipper kijelölt elemét lehet felülírni! Üres zipper esetén nem kell működnie a függvénynek.</p>
<p>A függvény típusa:</p>
<pre><code>write :: a (Zipper a) -&gt; Zipper a</code></pre>
<p>Tesztesetek:</p>
<pre><code>test_write =
  [ write 9 (Z [] [1])        === Z [] [9]
  , write 9 (Z [] [1..3])     === Z [] [9,2,3]
  , write 9 (Z [4..6] [1..3]) === Z [4..6] [9,2,3]
  ]</code></pre>
<h2 id="zipper-léptetése">Zipper léptetése</h2>
<p>Definiáljuk a <code>Movement</code> adattípust három adatkonstruktorral: <code>Forward</code>, <code>Backward</code>, <code>Stay</code>!</p>
<p>FONTOS: <em>a konstruktorok nevei pontosan ugyanezek legyenek, és ebben a sorrendben legyenek megadva.</em></p>
<p>Definiáljuk a <code>move</code> függvényt, amely a kapott <code>Movement</code> érték alapján a zippert jobbra vagy balra lépteti, illetve változatlanul hagyja! Ha olyan irányba léptetnénk, ahol már nincs elem, a függvénynek nem kell működnie.</p>
<p>A függvény típusa:</p>
<pre><code>move :: Movement (Zipper a) -&gt; Zipper a</code></pre>
<p>Tesztesetek:</p>
<pre><code>test_move =
  [ move Stay (Z empty [])            === Z [] []
  , move Stay (Z [1,2,3] [4,5,6])     === Z [1,2,3] [4,5,6]
  , move Forward (Z [1,2,3] [4,5,6])  === Z [4,1,2,3] [5,6]
  , move Backward (Z [1,2,3] [4,5,6]) === Z [2,3] [1,4,5,6]
  ]
  where
    empty :: [Int]
    empty = []</code></pre>
<h2 id="kijelölt-elem-környéke">Kijelölt elem környéke</h2>
<p>Definiáljuk az <code>around</code> függvényt, amellyel egy zipper kijelölt elemét, valamint az előtte és utána lévő <code>r</code> számú elemet kaphatjuk meg egy listában tetszőleges <code>r</code> paraméterre!</p>
<p>A függvény típusa:</p>
<pre><code>around :: Int (Zipper a) -&gt; [a]</code></pre>
<p>Tesztesetek:</p>
<pre><code>test_around =
  [ around 0 (Z [] [1])      == [1]
  , around 3 (Z [1..] [0..]) == [3,2,1,0,1,2,3]
  ]</code></pre>
<h2 id="végtelen-zipper-létrehozása-listából">Végtelen zipper létrehozása listából</h2>
<p>Definiáljuk a <code>fromListInf</code> függvényt, amely egy listából létrehoz egy olyan zippert, amely a lista első elemére fókuszál, a lista két vége előtt és után pedig egy paraméterben kapott elem ismétlődik végtelenül!</p>
<p>A függvény típusa:</p>
<pre><code>fromListInf :: a [a] -&gt; Zipper a</code></pre>
<p>Tipp: egy elemből képzett végtelen listát a <code>repeat</code> függvénnyel kaphatunk.</p>
<p>Tesztesetek:</p>
<pre><code>test_fromListInf =
  [ let (Z xs ys) = fromListInf 0 [1..5]
    in  take 100 xs == repeatn 100 0
        &amp;&amp; take 100 ys == [1..5] ++ repeatn 95 0
  ]</code></pre>
<h2 id="turing-gép-típusosztály">Turing-gép típusosztály</h2>
<p>A Turing-gépen definiált műveleteket kiemeljük egy típusosztályba, hogy ezeket egy interfészen keresztül használhassuk. Definiáljuk a <code>Machine</code> típusosztályt, amelynek a következő műveletei vannak a <code>t</code> típusparaméter és <code>Machine t</code> korlátozás mellett:</p>
<pre><code>done :: (t a) -&gt; Bool</code></pre>
<p>Annak lekérdezése, hogy az <code>a</code> típusú szimbólumokkal dolgozó Turing-gép futása véget ért-e.</p>
<pre><code>tape :: (t a) -&gt; Zipper a</code></pre>
<p>A Turing-gép szalagjának lekérdezése.</p>
<pre><code>step :: (t a) -&gt; t a</code></pre>
<p>A Turing gép egy lépésének elvégzése.</p>
<h2 id="turing-gép-adattípus">Turing-gép adattípus</h2>
<p>Definiáljuk a <code>State</code> adattípust, amellyel a Turing-gép aktuális állapotát fogjuk ábrázolni! Adatkonstruktorai: <code>InState Int</code>, ha futás közben egy bizonyos állapotban vagyunk; <code>Accepted</code>, ha sikeresen lefutott a program, <code>Rejected</code>, ha sikertelenül futott le.</p>
<p>FONTOS: <em>a konstruktorok nevei pontosan ugyanezek legyenek, és ebben a sorrendben legyenek megadva.</em></p>
<p>Definiáljuk a <code>TuringMachine</code> adattípust, amelynek van egy <code>a</code> típusparamétere! Adatkonstruktora <code>TM</code>, amely a következő adatokat tárolja:</p>
<ul>
<li><code>State</code> -- az aktuális állapot.</li>
<li><code>Zipper a</code> -- a szalag.</li>
<li><code>Int a -&gt; (State, a, Movement)</code> -- az állapotátmeneteket leíró függvény. Az aktuális állapot sorszámából és a szalagon kijelölt szimbólumból képez egy új állapotba, egy szalagra írandó szimbólumba és egy irányba.</li>
</ul>
<p>FONTOS: <em>a konstruktor neve <code>TM</code> legyen (mindkettő nagybetű), és a paraméterei a fent megadott sorrendben legyenek megadva.</em></p>
<h2 id="a-turing-gép-működése">A Turing-gép működése</h2>
<p>Definiáljuk a <code>TuringMachine</code> típus <code>Machine</code>-példányát!</p>
<p>A <code>done</code> és <code>tape</code> függvények a típusban tárolt adatok alapján adják vissza az eredményt.</p>
<p>A <code>step</code> függvény működése a következő:</p>
<p>A típusban tárolt függvényt meghívja az aktuális állapottal és a szalagon kijelölt szimbólummal. Ez visszaadja az új állapotot, a kiírandó szimbólumot és az irányt. Az új szalagot a következőképpen kapjuk: kiírjuk a szimbólumot a régi szalagra, majd ezután léptetjük az irány alapján. A függvényt változatlanul hagyja a típusban.</p>
<p>Tesztesetek:</p>
<pre><code>test_done =
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
        &amp;&amp; tape m === Z ['b'] ['b']
  , let m = step (TM (InState 0) (fromList ['b','b']) f)
    in  not (done m)
        &amp;&amp; tape m === Z ['a'] ['b']
  , let m = step (TM (InState 1) (fromList ['a','b']) f)
    in  done m
        &amp;&amp; tape m === fromList ['x','b']
  ]
  where
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 1 _   = (Accepted,  'x', Stay)</code></pre>
<h2 id="a-turing-gép-futtatása">A Turing-gép futtatása</h2>
<p>Definiáljuk a <code>run</code> függvényt, amely lefuttat egy Turing-gépet és visszaadja az összes állapotát a futás során! Ha a gép már lefutott, adjuk vissza ezt egy egyelemű listában! Ha nem, végezzünk el egy lépést, futtassuk le rekurzívan az adódó gépet, majd fűzzük a lista elejére a gépet!</p>
<p>A függvény típusa:</p>
<pre><code>run :: (t a) -&gt; [t a] | Machine t</code></pre>
<p>Tesztesetek:</p>
<pre><code>test_run =
  [ let m = last (run (tm ['a','b','x','x']))
    in done m
       &amp;&amp; tape m === Z ['x','a','b'] ['x']
  , let m = last (run (tm ['b','a','x','x']))
    in done m
       &amp;&amp; tape m === Z ['x','b','a'] ['x']
  , let m = last (run (tm ['a','b','x','a']))
    in done m
       &amp;&amp; tape m === Z ['x','a','b'] ['!']
  ]
  where
    tm xs = TM (InState 0) (fromList xs) f
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 0 'x' = (InState 1, 'x', Forward)
    f 1 'x' = (Accepted,  'x', Stay)
    f _ ch  = (Rejected,  '!', Stay)</code></pre>
<h2 id="turing-gép-állapotainak-megjelenítése">Turing-gép állapotainak megjelenítése</h2>
<p>Tekinstünk azokat a Turing-gépeket, amelyek szimbólumai karakterek! Definiáljuk a <code>showStates</code> függvényt, amely lefuttatja a gépet, majd minden állapot szalagján veszi a fókuszált elem 5 sugarú környezetét (<code>around</code>), ezt a listát pedig pedig szövegesen adja vissza!</p>
<p>A függvény típusa:</p>
<pre><code>showStates :: (t Char) -&gt; [String] | Machine t</code></pre>
<p>Tipp: karakterek listáját <code>String</code> értékké tudjuk alakítani a <code>toString</code> függvénnyel.</p>
<p>Tesztesetek:</p>
<pre><code>test_showStates =
  [ showStates (tm ['a','b','x','x'])
    == [ "     abxx  "
       , "    bbxx   "
       , "   baxx    "
       , "  baxx     "
       , "  baxx     "
       ]
  , showStates (tm ['a','b','x','a'])
    == [ "     abxa  "
       , "    bbxa   "
       , "   baxa    "
       , "  baxa     "
       , "  bax!     "
       ]
  ]
    where
      tm xs = TM (InState 0) (fromListInf ' ' xs) f
      f 0 'a' = (InState 0, 'b', Forward)
      f 0 'b' = (InState 0, 'a', Forward)
      f 0 'x' = (InState 1, 'x', Forward)
      f 1 'x' = (Accepted,  'x', Stay)
      f _ ch  = (Rejected,  '!', Stay)</code></pre>
<h2 id="segítség-a-feltöltéshez">Segítség a feltöltéshez</h2>
<p>Az alábbi állományt érdemes módosítani, így, szövegesen kell feltölteni (az alábbi természetesen hibás működésű program),</p>
<p>FONTOS: <em>csak olyan megoldást töltsünk fel, amely <strong>lefordul</strong>!</em></p>
<pre><code>module Turing

import StdEnv, StdLib, StdGeneric, GenEq

:: Zipper a = Something

derive gEq Zipper

fromList :: [a] -&gt; Zipper a
fromList a = abort "not defined"

read :: (Zipper a) -&gt; a
read a = abort "not defined"

write :: a (Zipper a) -&gt; Zipper a
write a b = abort "not defined"

:: Movement = Something1

move :: Movement (Zipper a) -&gt; Zipper a
move a b = abort "not defined"

around :: Int (Zipper a) -&gt; [a]
around a b = abort "not defined"

fromListInf :: a [a] -&gt; Zipper a
fromListInf a b = abort "not defined"

class Machine t where
  done :: (t a) -&gt; Bool
  tape :: (t a) -&gt; Zipper a
  step :: (t a) -&gt; t a

:: State = Something2

:: TuringMachine a = Something3

instance Machine TuringMachine where
  done a = abort "undefined"
  tape a = abort "undefined"
  step a = abort "undefined"

run :: (t a) -&gt; [t a] | Machine t
run a = abort "not defined"

showStates :: (t Char) -&gt; [String] | Machine t
showStates a = abort "not defined"</code></pre>
<p>A tesztelést az alábbi függvényekkel lehet segíteni:</p>
<pre><code>tests :: [[Bool]]
tests =
  [ test_fromList
  , test_read
  , test_write
  , test_move
  , test_around
  , test_fromListInf
  , test_done
  , test_tape
  , test_step
  , test_run
  , test_showStates
  ]

Start = (all and tests, zip2 [1..] (map and tests))</code></pre></div>
