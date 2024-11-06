open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Some 9
let%test "test_eval_2" = parse "1 + 8 + 3 + (1 + 6)" |> eval = Some 19
let%test "test_eval_3" = parse "10 + (1 + 4)" |> eval = Some 15
let%test "test_eval_4" = parse "10 - (1 + 4)" |> eval = Some 5
let%test "test_eval_5" = parse "10 * 1 + 4" |> eval = Some 14
