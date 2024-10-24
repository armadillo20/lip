open Toylexer.Token
open Toylexer.Main

(*a parita di occorrenze le precedenze sono decise nella definizione dei token ma  in ordine ascendente*)

(*test che funzionano solo se non vengono implementate i nuovi token del secondo esercizio
let%test "test_frequencies_1" =
lexer "x=1; y=x+1 ; x =1   " |> frequency 3 = [(CONST"1",3);(ID "x", 3); (ASSIGN, 3) ]

let%test "test_frequencies_2" =
lexer "x = y ; x = x = 1" |> frequency 1 = [(ID "x", 3)]

let%test "test_frequencies_3" =
lexer "a = 5; b = a + 3; b = 10" |> frequency 2 = [(ASSIGN, 3);(ID "b", 2) ]


let%test "test_frequencies_4" =
lexer "x = x + y; y = 2" |> frequency 3 = [(ID "y", 2) ;(ID "x", 2);(ASSIGN, 2) ]

let%test "test_frequencies_5" =
  lexer "a = 5; b = a + 3; b = 10; a = 0" |> frequency 3 = [(ASSIGN, 4); (ID "a", 3); (SEQ, 3)]*)



  let%test "test_frequencies_6" =
lexer "x=1; y=x+1 ; x =1   " |> frequency 1 = [(CONST"1",3)]


let%test "test_frequencies_7" =
lexer "Lesgo" |> frequency 1 = [(ATOK,1)]