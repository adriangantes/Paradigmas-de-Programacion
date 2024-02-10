(*x-y;;*) 
(*Error: Unbound value x, error de ejecución x no esta definido*)

let x=1;; 
(*definimos x como 1*)

(*x-y;;*) 
(*Error: Unbound value y, error de ejecución, y no esta definido*)

let y=2;; 
(*definimos y como 2*)

x-y;; 
(*hace el cálculo, devuelves -1*)

let x=y in x-y;;
(*se define x como y, que es 2, se hace el cálculo y devuelve 0*)

x-y;; 
(*devuelve -1, antes solo se definio x para ese cálculo*)

x-y;; 
(*devuelve -1, antes solo se definio x para ese cálculo*)

(*z;;*) 
(*Error: Unbound value z, error de ejecución, z no esta definido*)

let z=x+y;; 
(*define z como x+y, devuelve 3*)

z;; 
(*devuelve 3*)

let x=5;; 
(*define x con 5*)

x+y;; 
(*hace el cálculo, devuelve 7*)

z;; 
(*devuelve 3, porque z guarda los valores anteriores de x e y*)

x+y;; 
(*devuelve 7*)

let y=5 in x+y;;
(*define y como 5, se hace el cálculo y devuelve 10*)

x+y;; 
(*devuelve 7*)

let x=x+y in let y=x*y in x+y+z;;
(*devuelve 24, primero guarda x=7, luego hace y=14 y por último suma 7+14+3*)

x+y+z;; 
(*devuelve 10*)

function x->2*x;; 
(*- : int -> int = <fun>*)

(function x->2*x) 2+1;; 
(*devuelve 5, hace la función con 2 y le suma 1*)

(function x->2*x) 2*1;; 
(*devuelve 4, hace la función con 2 y multiplica por 1*)

let f=function x->2*x;; 
(*define la función f*)

f;; 
(*- : int -> int = <fun>*)

f (2+1);; 
(*devuelve 6*)

f 2+1;; 
(*devuelve 5*)

f x;; 
(*devuelve 10*)

let x = 100;; 
(*define x como 100*)

f x;; 
(*devuelve 200*)

let m =1000;; 
(*define m como 1000*)

let g = function x->x+m;; 
(*define la función f*)

g;; 
(*- : int -> int = <fun>*)

g 3;; 
(*devuelve 1003*)

(*g 3.0;;*) 
(*Error: This expression has type float but an expression was expected of type int, error de tipos*)

let m=7;; 
(*define m como 7*)

g 3;; 
(*devuelve 1003*)

let istrue = function true -> true;; 
(*define la función istrue*) 

istrue;; 
(*- : bool -> bool = <fun>*)

istrue (1<2);; 
(*devuelve verdadero*)

(*istrue (2<1);;*) 
(*Exception: Match_failure ("//toplevel//", 1, 13). Error de ejecución*)

(*istrue 0;;*) 
(*Error: This expression has type int but an expression was expected of type bool. Error de tipos*)

let iscero_v1 = function 0 -> true;; 
(*Warning 8 [partial-match]: this pattern-matching is not exhaustive. 
Here is an example of a case that is not matched: 1 
val iscero_v1 : int -> bool = <fun>. Define la función iscero_v1*)

iscero_v1 0;; 
(*devuelve true*)

(*iscero_v1 0.;;*) 
(*Error: This expression has type float but an expression was expected of type int. Error de tipos*)

(*iscero_v1 1;;*) 
(*Exception: Match_failure ("//toplevel//", 1, 16). Error de ejecución*)

let iscero_v2 = function 0 -> true | _ ->false;;
(*define la función iscero_v2*)

iscero_v2 0;; 
(*devuelve true*)

iscero_v2 1;; 
(*devuelve false*)

(*iscero_v2 0.;;*) 
(*Error: This expression has type float but an expression was expected of type int. Error de tipos*)

let all_to_true = function true -> true | false -> true;; 
(*define la función all_to_true, de tipo bool->bool*)

all_to_true (1<2);; 
(*devuelve true*)

all_to_true (2<1);;
 (*devuelve true*)
 
(*all_to_true 0;;*) 
(*Error: This expression has type int but an expression was expected of type bool. Error de tipos*)

let first_all_to_true = all_to_true;; 
(*define la función first_all_to_true igual que all_to_true, de tipo bool->bool*)

let all_to_true = function x-> true;; 
(*define la función all_to_true, de tipo 'a->bool*)

all_to_true (1<2);; 
(*devuelve true*)

all_to_true (2<1);; 
(*devuelve true*)

all_to_true 0;; 
(*devuelve true*)

(*first_all_to_true 0;;*) 
(*Error: This expression has type int but an expression was expected of type bool. Error de tipos*)
