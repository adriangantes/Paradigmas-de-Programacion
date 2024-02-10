();; 
(*parentesis de orden de ejecución. Muestra unit = ()*)

2 + 5 * 3;; 
(*muestra el resultado, int: 17*)

1.25 *. 2.0;; 
(*muestra el resultado, float: 2.5*)

(*2 - 2.0;;*) 
(*error de tipos*)

(*3.0 + 2.0;;*) 
(*error de sintaxis por el signo*)

5/3;; 
(*parte entera de la división, 1*)

5 mod 3;; 
(*muestra el resto de la división, 2*)

2.0 *. 3.0 ** 2.0;; 
(*muestra el resultado, 18*)

2.0 ** 3.0 ** 2.0;; 
(*muestra el resultado, 512*)

sqrt;; 
(*función de raiz cuadrada que recibe un float y devuelve un float*)

(*sqrt 4;;*)
(*error de tipos, sqrt espera un float*)

int_of_float;; 
(*función, float a int*)

float_of_int;; 
(*función, int a float*)

3.0 = float_of_int 3;; 
(*muestra el resultado, true*)

(*int_of_float -2.9;;*) 
(*error de tipos*)

int_of_float 2.1 + truncate (-2.9);; 
(*muestra 0, pasa 2.1 a 2 y trunca -2.9 a -2*)

truncate;; 
(*función de truncamiento*)

truncate 2.1 + truncate (-2.9);; 
(*muestra resutaldo, 0*)

floor;; 
(*funcion que recibe y devuelve float, redondea hacia abajo*)

(*floor 2.1 + floor (-2.9);;*) 
(*error de sintaxis*)

ceil;; 
(*funcion que recibe y devuelve float, redondea hacia arriba*)

ceil 2.1 +. ceil (-2.9);; 
(*muestra resultado, 1*)

int_of_char;; 
(*función que devuelve el caracter ascii de un char*)

int_of_char 'A';; 
(*devuelve 65*)

char_of_int;; 
(*función que devuelve un char del caracter ascii*)

char_of_int 66;; 
(*devuelve B*)

Char.code;; 
(*función que devuleve un entero ascii de un char*)

Char.code 'B';; 
(*devuelve 66, su código ascii*)

Char.chr;; 
(*devuelve un char, correspondiente al valor ascii recibido*)

Char.chr 67;; 
(*devuelve C, letra correspondiente al valor ascii*)

'\067';; 
(*devuelve igualmente C*)

Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');; 
(*obtiene los valores ascii, y al sumar la diferencia entre minuscula y mayuscula de la a, transforma la M en m, devolviendo la letra correspondiente al valor calculado dentro de la expresión*)

Char.lowercase_ascii;; 
(*funcion q devuelve la minuscula de la letra inicial*)

Char.lowercase_ascii 'M';; 
(*devuelve m*)

Char.uppercase_ascii;; 
(*funcion q devuelve la mayuscula de la letra inicial*)

Char.uppercase_ascii 'm';; 
(*devuelve M*)

"this is a string";; 
(*devuelve el tipo al que pertenece, string*)

String.length;; 
(*función que devuelve un entero, la longitud del string*)

String.length "longitud";; 
(*devuelve 8*)

(*"1999" + "1";;*) 
(*error de tipos, opera dos strings esperando dos ints*)

"1999" ^ "1";; 
(*concatena los dos strings, devuelve 19991*)

int_of_string;; 
(*función que transforma un string en entero*)

int_of_string "1999" + 1;; 
(*devuelve 2000*)

"\065\066";; 
(*devuelve un string, AB*)

string_of_int;; 
(*función q transforma un entero en string*)

string_of_int 010;; 
(*transforma el entero 10 en un string que contiene el 10*)

not true;; 
(*operación lógica, devuelve un booleano, falso*)

true && false;; 
(*operación lógica, devuelve un booleano, falso*)

true || false;; 
(*operación lógica, devuelve un booleano, verdadero*)

(1 < 2) = false;; 
(*asigna como resultado de la operación lógica falso*)

"1" < "2";; 
(*devuelve verdadero, pero compara los valores ascii del string*)

2 < 12;; 
(*devuelve verdadero, ahora comparando numeros*)

"2" < "12";; 
(*devuelve falso, compara primero el 2 con el 1, y ya no se cumple la condición*)

"uno" < "dos";; 
(*mismo motivo de antes, opera cada letra con su valor ascii y la u no es menor que la d, devolviendo falso*)

if 3 = 4 then 0 else 4;; 
(*devuelve 4, pues 3 no es igual a 4*)

if 3 = 4 then "0" else "4";; 
(*devuelve el string "4", como se le indica en el condicional*)

(*if 3 = 4 then 0 else "4";;*) 
(*error de tipos, esta funcion espera devolver un entero y se le introduce un string*)

(if 3 < 5 then 8 else 10) + 4;; 
(*se cumple la condición, la función devuelve 8 y le suma 4, devolviendo 12*)
