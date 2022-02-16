module Project02

// Function 1

let rec _max currmax L =
    match L with
    |[] -> currmax
    |hd::tl when hd> currmax -> _max hd tl
    |_::tl-> _max currmax tl

let max L =
    match L with
    |[]-> raise (System.ArgumentException("L cannot be empty"))
    |head:: tail -> _max head tail
    

// Function 2


let rec _min currMin L =
    match L with
    |[] -> currMin
    |hd::tl when hd< currMin -> _min hd tl
    |_::tl-> _min currMin tl

let min L =
    match L with
    |[]-> raise (System.ArgumentException("L cannot be empty"))
    |head:: tail -> _min head tail
// Function 3

let rec nth L n =
    match L with
    |[] -> raise (System.IndexOutOfRangeException("the List is too small for nth element"))
    |thisElem::remainder when n =0 -> thisElem
    |_::remainder-> nth remainder (n-1)

// Function 4

let rec _find L item count = 
    match L with 
    |[]-> raise (System.ArgumentException("List is too small"))
    |thisElem::remainder when thisElem = item-> (count)
    |_::remainder-> _find remainder item (count+1)

let find L item =
    match L with
    |[]->raise( System.ArgumentException("This List is empty"))
    |thisList -> _find thisList item 0
    
// Function 5

let rec _count L item count = 
    match L with
    |[]-> count
    |thisElem::remainder when thisElem = item -> _count remainder item (count+1)
    |_::remainder -> _count remainder item count

let count L item =
    match L with
    |list-> _count L item 0
    
// Function 6

let rec _map func oldList newList = 
    match oldList with 
    |[]-> newList
    |someElem::remainder -> _map func remainder (newList@([func someElem]))


let map F L =
   match L with
   |[]-> []
   |oldList-> _map F oldList []
// Function 7

let rec iter F L =
    match L with
    |[]-> ()
    |someElem:: remainder-> (F someElem) 
                            iter F remainder

// Function 8
let rec _reduce func list hd = 
    match list with
    |[]-> hd
    |someElem::remainder -> _reduce func remainder (func hd someElem)

let reduce F L =
    match L with
    |[]->raise (System.ArgumentException("List is empty"))
    |someElem:: []-> someElem
    |hd:: tl -> _reduce F tl hd

// Function 9
let rec _fold F start L =
    match L with
    |[]-> start
    |someElem::remainder -> _fold F (F start someElem) remainder

let rec fold F start L =
    match L with
    |list-> _fold F start L

// Function 10

let rec _flatten L newList = 
    match L with 
    |[]-> newList
    |someElem:: remainingLists-> _flatten remainingLists (newList@someElem)

let flatten L =
    match L with
    |[]-> raise(System.ArgumentException("List is empty"))
    |list -> _flatten list []

// Function 11
let rec _zip L1 L2 newList = 
    match L1,L2 with
    |[],[]-> newList
    |[],remainder-> raise(System.ArgumentException("not equal length"))
    |remainder,[]-> raise(System.ArgumentException("not equal length"))
    |someElem::remaining, someElem2:: remaining2 -> let x = (someElem,someElem2)
                                                    _zip remaining remaining2 (newList@[x])
                                                      

let zip L1 L2 =
    match L1,L2 with
    |remainder, remainder2-> _zip remainder remainder2 []

// Function 12

let rec _unzip L L1 L2 = 
    match L with
    |[]-> (L1,L2)
    |someElem::remainder-> let (firstElem, secondElem) = someElem
                           _unzip remainder (L1@[firstElem]) (L2@[secondElem])
   

let unzip L =
    match L with
    |[]-> ([],[])
    |list->_unzip L [] [] 


// Function 13
let rec GCD A B =
    if A = 0 then B 
    elif B=0 then A
    else 
        let remainder = A%B
        GCD B remainder
// Function 14
let rec _GCDList A L =
    match L with
    |[]-> A
    |B::remaind-> if A = 0 then 
                        _GCDList B remaind
                    elif B = 0 then 
                        _GCDList A remaind
                    else
                        let remainder = A%B
                        _GCDList B (remainder::remaind) 
                           


let GCDList L =
    match L with
    |[]->raise (System.ArgumentException("There is no GCD of empty List"))
    |e::[]-> e
    |e::list-> _GCDList e L  
    

// Function 15

let rec LotsGCD A count = 
    if count < A then
        let x = GCD A count
        if x = 1 then
            LotsGCD A (count+1)
        else
            false
    else
        true
let isPrime i =
    if i <=1 then
        false
    else
        LotsGCD i 1

// Function 16

let rec _Primes t x count = 
    match count with
    |count when count <= t-> let num = isPrime count
                             match num with
                             |num when num = true-> _Primes t (x@[count]) (count+1)
                             |num-> _Primes t x (count+1)
    |count -> x
let rec Primes t =
    let x =[]
    _Primes t x 0 

// Function 17
let stringToCharList (s: string) =
    Seq.toList s

let rec _isMatch list count = 
    if count < 0 then
        false
    else
        match list with
        |[]->if count = 0 then
                true
             else
                false
        |openPar::remainder when openPar = '('-> _isMatch remainder (count+1)
        |closePar::remainder when closePar = ')'-> _isMatch remainder (count-1)
        |_::remainder -> _isMatch remainder count

let isMatch str =
    let x = stringToCharList str
    _isMatch x 0


// Function 18
let charListToString L =
    let sb = System.Text.StringBuilder()
    L |> List.iter (fun c -> ignore (sb.Append (c:char)))
    sb.ToString()  
  
let rec _isBestMatch list newList count = 
    if count = 0 then
        charListToString newList
    else
        match list with
        |[]->if count = 0 then
                charListToString newList
             else
                raise(System.Exception("Your first parantheses was never closed"))

        |openPar::remainder when openPar = '('-> _isBestMatch remainder (newList@[openPar]) (count+1)
        |closePar::remainder when closePar = ')'-> if count = 1 then 
                                                    _isBestMatch remainder newList (count-1)
                                                   else
                                                   _isBestMatch remainder (newList@[closePar]) (count-1)
        |elem::remainder -> _isBestMatch remainder (newList@[elem]) count

let rec _isGoodMatch list = 
        match list with
        |[]-> ""
        |openPar::remainder when openPar = '('-> _isBestMatch remainder [] 1
        |_::remainder -> _isGoodMatch remainder
let firstMatch s =
    let x = stringToCharList s
    _isGoodMatch x


// Function 19

let allMatches s = 
    []             //   TO BE IMPLEMENTED


// Function 20

let rec RegEx s pattern =
    true         //    TO BE IMPLEMENTED




















