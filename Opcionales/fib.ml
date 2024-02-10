(*
#use "topfind";;
#list;;
#require "zarith.top";;
*)
 
open Big_int_Z

(* Función para multiplicar dos matrices representadas como tuplas *)
let matrix_mult ((a, b), (c, d)) ((e, f), (g, h)) =
  let x = add_big_int (mult_big_int a e) (mult_big_int b g) in
  let y = add_big_int (mult_big_int a f) (mult_big_int b h) in
  let z = add_big_int (mult_big_int c e) (mult_big_int d g) in
  let w = add_big_int (mult_big_int c f) (mult_big_int d h) in
  ((x, y), (z, w));;
  
  
(* Función para elevar una matriz a una potencia n utilizando multiplicación *)
let rec matrix_pow matrix n =
  if eq_big_int n unit_big_int then
    matrix (* Si n es 1, devuelve la matriz original (caso base) *)
  else if eq_big_int (mod_big_int n (big_int_of_int 2)) zero_big_int then
    let half_power = matrix_pow matrix (div_big_int n (big_int_of_int 2)) in
    matrix_mult half_power half_power (* Si n es par, calcula (matrix^(n/2))^2 *)
  else
    matrix_mult matrix (matrix_pow matrix (pred_big_int n));;
    (* Si n es impar, calcula matrix * matrix^(n-1) *)


(* Función para calcular el número da la posición n de la secuencia de fibonacci *)
let fib n =
  if le_big_int n zero_big_int then
    zero_big_int (* Si n es 0 o negativo, el resultado es 0 *)
  else
    let  ((f_n, _), (_, _)) = matrix_pow ((unit_big_int, unit_big_int), (unit_big_int, zero_big_int)) (pred_big_int n) in
      f_n;; (* Calcula el número de Fibonacci en la posición n usando matrix_pow *)

(* Entrada de la línea de comandos *)
if Array.length Sys.argv <> 2 then
  print_endline "fib: Invalid number of arguments"
else
  let n = big_int_of_string Sys.argv.(1) in
  print_endline (Z.to_string (fib n));;

  
    
    
