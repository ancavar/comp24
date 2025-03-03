module AlphaConvTests = struct
  let alpha_conv_test s =
    match Parser.parse_program s with
    | Ok actual ->
      let prog_pe = Patelim.Elim.p_elim_decls actual in
      (match prog_pe with
       | Ok actual_pe ->
         let prog_cc = Anf.Cc_ll.closure_convert actual_pe in
         (match prog_cc with
          | Ok actual_cc ->
            let prog_anf = Anf.Anf_conv.run actual_cc in
            (match prog_anf with
             | Ok actual_anf ->
               let prog_alpha_conv = Anf.Alpha_conv.alpha_convert_prog actual_anf in
               (match prog_alpha_conv with
                | Ok actual_alpha_conv ->
                  Format.printf
                    "---ANF---\n\n%a\n\n---Alpha conv.---\n\n%a\n"
                    Anf.Pp_anf_ast.pp_anf_prog
                    actual_anf
                    Anf.Pp_anf_ast.pp_anf_prog
                    actual_alpha_conv
                | Error err -> Format.printf "%s\n" err)
             | Error err -> Format.printf "%s\n" err)
          | Error err -> Format.printf "%s\n" err)
       | Error err -> Format.printf "%s\n" err)
    | Error err -> Format.printf "%s\n" err
  ;;
end

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
  let x = 0
  let x = x
  let x = x
  let x = x
  let x = x
  let x = 3  
  |};
  [%expect
    {|
    ---ANF---

    let x  = 0;;
    let x  = x;;
    let x  = x;;
    let x  = x;;
    let x  = x;;
    let x  = 3

    ---Alpha conv.---

    let x.1  = 0;;
    let x.2  = x.1;;
    let x.3  = x.2;;
    let x.4  = x.3;;
    let x.5  = x.4;;
    let x.6  = 3 |}]
;;

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
  let x = 0
  let x = if x >= 0 then let x = 1 in x+1 else x 
  let x = x
  
  |};
  [%expect
    {|
    ---ANF---

    let x  = 0;;
    let x  = let app_0 = (x >= 0) in
    let if_1 = if app_0 then let x = 1 in
    let app_2 = (x + 1) in
    app_2 else x in
    if_1;;
    let x  = x

    ---Alpha conv.---

    let x.1  = 0;;
    let x.2  = let app_0.l3 = (x.1 >= 0) in
    let if_1.l0 = if app_0.l3 then let x.l13 = 1 in
    let app_2.l5 = (x.l13 + 1) in
    app_2.l5 else x.1 in
    if_1.l0;;
    let x.3  = x.2 |}]
;;

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
    let a = let x = 1 in
    let f y = x + y in 
    let x = 2 in 
    let x = 2 in 
    f x
  |};
  [%expect
    {|
    ---ANF---

    let cc_ll_0 x y = let app_0 = (x + y) in
    app_0;;
    let a  = let x = 1 in
    let app_0 = cc_ll_0 x in
    let f = app_0 in
    let x = 2 in
    let x = 2 in
    let app_1 = f x in
    app_1

    ---Alpha conv.---

    let cc_ll_0.1 x y = let app_0.l0 = (x + y) in
    app_0.l0;;
    let a.1  = let x.l63 = 1 in
    let app_0.l14 = cc_ll_0.1 x.l63 in
    let f.l42 = app_0.l14 in
    let x.l134 = 2 in
    let x.l139 = 2 in
    let app_1.l101 = f.l42 x.l139 in
    app_1.l101 |}]
;;

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
    let a = let x = 1 in
    let x = 2 in 
    let x = 3 in 
    let x = 4 in 
    let x = 5 in 
    f x
  |};
  [%expect
    {|
    ---ANF---

    let a  = let x = 1 in
    let x = 2 in
    let x = 3 in
    let x = 4 in
    let x = 5 in
    let app_0 = f x in
    app_0

    ---Alpha conv.---

    let a.1  = let x.l0 = 1 in
    let x.l1 = 2 in
    let x.l7 = 3 in
    let x.l12 = 4 in
    let x.l16 = 5 in
    let app_0.l11 = f x.l16 in
    app_0.l11 |}]
;;
