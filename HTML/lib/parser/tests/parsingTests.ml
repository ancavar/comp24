(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParsingTests = struct
  let parse_test s =
    match Parser.parse_program s with
    | Ok actual -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog actual
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test _ =
    parse_test {| let a = 3 |};
    [%expect {| let a = 3 |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = 3: int  |};
    [%expect {| let (a : int) = (3 : int) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = 2 ++ 2 ** 2 |};
    [%expect {| let (a : int) = (2 ++ (2 ** 2)) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = (2 ++ 2) ** 2 |};
    [%expect {| let a = ((2 ++ 2) ** 2) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = let f x = x in 13 |};
    [%expect {| 
      let (a : int) = let f = (fun x -> x)
      in 13
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let ( += ) = x -$ 1 |};
    [%expect {| let ( += ) = (x -$ 1) |}]
  ;;

  let%expect_test _ =
    parse_test {|
    let rec ( += ) x = x -$ 1 
   and (-$) = fun x y -> ( + ) x y;;
 |};
    [%expect
      {| 
      let rec ( += ) = (fun x -> (x -$ 1))
      and ( -$ ) = (fun x -> (fun y -> (x + y)))
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = (+) x y |};
    [%expect {| let a = (x + y) |}]
  ;;

  let%expect_test _ =
    parse_test {| let ( += ) a b = (~-) 1 |};
    [%expect {| let ( += ) = (fun a -> (fun b -> (( ~- ) 1))) |}]
  ;;

  let%expect_test _ =
    parse_test {| let f (lst: (int -> int)) = g |};
    [%expect {| let f = (fun (lst : int -> int) -> g) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = let f x = x + 1 in let b = 5 in f (b + 1) |};
    [%expect
      {| 
      let a = let f = (fun x -> (x + 1))
      in let b = 5
      in f (b + 1)
    |}]
  ;;

  let%expect_test _ =
    parse_test "let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])]";
    [%expect
      {| let a = ((3 :: 3 :: []), (4 :: 4 :: [])) :: ((5 :: 5 :: []), (6 :: 6 :: [])) :: ((7 :: 7 :: []), (8 :: 8 :: [])) :: [] |}]
  ;;

  let%expect_test _ =
    parse_test
      {|
    let a = 
      match l with 
      | hd, tl -> hd 
      | _ -> 0
    |};
    [%expect {|
      let a = match l with
      | (hd, tl) -> hd
      | _ -> 0|}]
  ;;

  let%expect_test _ =
    parse_test {| let f (lst: ('b list)) = lst |};
    [%expect {| let f = (fun (lst : 'b list) -> lst) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = - 3";
    [%expect {| let a = (- 3) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = 3 -> 3";
    [%expect {| Syntax error |}]
  ;;

  let%expect_test _ =
    parse_test "let a = f a + f b";
    [%expect {| let a = ((f a) + (f b)) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = - f 3 - f 4";
    [%expect {| let a = ((- (f 3)) - (f 4)) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (hd::tl) = 3";
    [%expect {| let f = (fun hd :: tl -> 3) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x,y) = x + y";
    [%expect {| let f = (fun (x, y) -> (x + y)) |}]
  ;;

  let%expect_test _ =
    parse_test "let f ((x: 'a),y) =  x * y";
    [%expect {| let f = (fun ((x : 'a), y) -> (x * y)) |}]
  ;;

  let%expect_test _ =
    parse_test "let f ((g : (int -> int)),y) = g y + g (y + 1)";
    [%expect {| let f = (fun ((g : int -> int), y) -> ((g y) + g (y + 1))) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x :: (x, y)) = 3";
    [%expect {| let f = (fun x :: (x, y) -> 3) |}]
  ;;

  let%expect_test _ =
    parse_test
      {| 
      let blst : int=
        let rec f x = if x > 0 then g (x - 1) else 1
        and g x = if x > 0 then f (x - 1) + g (x - 2) else 1 in
        f 10
      |};
    [%expect
      {|
      let (blst : int) = let rec f = (fun x -> if (x > 0) then g (x - 1) else 1)
      and g = (fun x -> if (x > 0) then (f (x - 1) + g (x - 2)) else 1)
      in (f 10) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x, (y :: ys)) = 3";
    [%expect {| let f = (fun (x, (y :: ys)) -> 3) |}]
  ;;

  let%expect_test _ =
    parse_test
      {| let xor (x : bool) (y : bool) = 
      match (x, y) with 
      | (true, true) -> false 
      | (true, false) -> true 
      | (false, true) -> true
      | (false, false) -> false
      |};
    [%expect
      {|
      let xor = (fun (x : bool) -> (fun (y : bool) -> match (x, y) with
      | (true, true) -> false
      | (true, false) -> true
      | (false, true) -> true
      | (false, false) -> false)) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = let f x = x in f (f (f (5 + f 5+5+5+f 5) + 5) + 5) + 5";
    [%expect
      {|
      let a = let f = (fun x -> x)
      in (f (f (f ((((5 + (f 5)) + 5) + 5) + (f 5)) + 5) + 5) + 5) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = f x : int";
    [%expect {|
      let a = ((f x) : int)
      |}]
  ;;
end

module ManyTests = struct
  open ParsingTests

  let read_from_file filename =
    let full_file =
      let rec get_rparent s = function
        | 0 -> s
        | n when n < 0 -> failwith "Can't get parent of "
        | n -> get_rparent (Filename.dirname s) (n - 1)
      in
      get_rparent (Unix.getcwd ()) 8 ^ "/manytests/" ^ filename ^ ".ml"
    in
    let ch = open_in full_file in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  ;;

  let read_from_file_typed filename = read_from_file ("typed/" ^ filename)

  let%expect_test _ =
    parse_test (read_from_file_typed "001fac");
    [%expect
      {|
      let rec fac = (fun n -> if (n <= 1) then 1 else (n * fac (n - 1)));;
      let main = let () = (print_int (fac 4))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "002fac");
    [%expect
      {|
      let rec fac_cps = (fun n -> (fun k -> if (n = 1) then (k 1) else (fac_cps (n - 1) (fun p -> k (p * n)))));;
      let main = let () = (print_int ((fac_cps 4) (fun print_int -> print_int)))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "002fac");
    [%expect
      {|
      let rec fac_cps = (fun n -> (fun k -> if (n = 1) then (k 1) else (fac_cps (n - 1) (fun p -> k (p * n)))));;
      let main = let () = (print_int ((fac_cps 4) (fun print_int -> print_int)))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "003fib");
    [%expect
      {|
      let rec fib_acc = (fun a -> (fun b -> (fun n -> if (n = 1) then b else let n1 = (n - 1)
      in let ab = (a + b)
      in (((fib_acc b) ab) n1))));;
      let rec fib = (fun n -> if (n < 2) then n else (fib (n - 1) + fib (n - 2)));;
      let main = let () = (print_int (((fib_acc 0) 1) 4))
      in let () = (print_int (fib 4))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "004manyargs");
    [%expect
      {|
      let wrap = (fun f -> if (1 = 1) then f else f);;
      let test3 = (fun a -> (fun b -> (fun c -> let a = (print_int a)
      in let b = (print_int b)
      in let c = (print_int c)
      in 0)));;
      let test10 = (fun a -> (fun b -> (fun c -> (fun d -> (fun e -> (fun f -> (fun g -> (fun h -> (fun i -> (fun j -> (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)))))))))));;
      let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
      in let () = (print_int rez)
      in let temp2 = ((((wrap test3) 1) 10) 100)
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "005fix");
    [%expect
      {|
      let rec fix = (fun f -> (fun x -> ((f (fix f)) x)));;
      let fac = (fun self -> (fun n -> if (n <= 1) then 1 else (n * self (n - 1))));;
      let main = let () = (print_int ((fix fac) 6))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "006partial");
    [%expect
      {|
      let foo = (fun b -> if b then (fun foo -> (foo + 2)) else (fun foo -> (foo * 10)));;
      let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))));;
      let main = let () = (print_int (foo 11))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "006partial2");
    [%expect
      {|
      let foo = (fun a -> (fun b -> (fun c -> let () = (print_int a)
      in let () = (print_int b)
      in let () = (print_int c)
      in (a + (b * c)))));;
      let main = let foo = (foo 1)
      in let foo = (foo 2)
      in let foo = (foo 3)
      in let () = (print_int foo)
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "006partial3");
    [%expect
      {|
      let foo = (fun a -> let () = (print_int a)
      in (fun b -> let () = (print_int b)
      in (fun c -> (print_int c))));;
      let main = let () = (((foo 4) 8) 9)
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "007order");
    [%expect
      {|
      let _start = (fun () -> (fun () -> (fun a -> (fun () -> (fun b -> (fun _c -> (fun () -> (fun d -> (fun __ -> let () = print_int (a + b)
      in let () = (print_int __)
      in (((a * b) / _c) + d))))))))));;
      let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "008ascription");
    [%expect
      {|
      let addi = (fun f -> (fun g -> (fun x -> ((f x) ((g x : bool)) : int)))));;
      let main = let () = (print_int (((addi (fun x -> (fun b -> if b then (x + 1) else (x * 2)))) (fun _start -> ((_start / 2) = 0))) 4))
      in 0
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "009let_poly");
    [%expect {|
      let temp = let f = (fun x -> x)
      in ((f 1), (f true))
    |}]
  ;;

  let%expect_test _ =
    parse_test (read_from_file_typed "015tuples");
    [%expect
      {|
      let rec fix = (fun f -> (fun x -> ((f (fix f)) x)));;
      let map = (fun f -> (fun p -> let (a, b) = p
      in ((f a), (f b))));;
      let fixpoly = (fun l -> ((fix (fun self -> (fun l -> ((map (fun li -> (fun x -> ((li (self l)) x)))) l)))) l));;
      let feven = (fun p -> (fun n -> let (e, o) = p
      in if (n == 0) then 1 else o (n - 1)));;
      let fodd = (fun p -> (fun n -> let (e, o) = p
      in if (n == 0) then 0 else e (n - 1)));;
      let tie = (fixpoly (feven, fodd));;
      let rec meven = (fun n -> if (n = 0) then 1 else modd (n - 1))
      and modd = (fun n -> if (n = 0) then 1 else meven (n - 1));;
      let main = let () = (print_int (modd 1))
      in let () = (print_int (meven 2))
      in let (even, odd) = tie
      in let () = (print_int (odd 3))
      in let () = (print_int (even 4))
      in 0
    |}]
  ;;
  (*
     let%expect_test _ =
     pe_test (read_from_file_typed "016lists");
     [%expect {|hanging???
    |}]
     ;;
  *)
end
