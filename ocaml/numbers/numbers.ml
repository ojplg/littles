
(*
 opam config env
 ocamlfind ocamlc -linkpkg -thread -package core numbers.ml
*)

open Core


type pattern = {
    top: bool;
    middle: bool;
    bottom: bool;
    top_left: bool;
    bottom_left: bool;
    top_right: bool;
    bottom_right: bool;
}

let zero = { top=true; middle=false; bottom=true; top_left=true; bottom_left=true; top_right=true; bottom_right=true }
let one = { top=false; middle=false; bottom=false; top_left=false; bottom_left=false; top_right=true; bottom_right=true }
let two = { top=true; middle=true; bottom=true; top_left=false; bottom_left=true; top_right=true; bottom_right=false }
let three = { top=true; middle=true; bottom=true; top_left=false; bottom_left=false; top_right=true; bottom_right=true }
let four = { top=false; middle=true; bottom=false; top_left=true; bottom_left=false; top_right=true; bottom_right=true }
let five = { top=true; middle=true; bottom=true; top_left=true; bottom_left=false; top_right=false; bottom_right=true }
let six = { top=true; middle=true; bottom=true; top_left=true; bottom_left=true; top_right=false; bottom_right=true }
let seven = { top=true; middle=false; bottom=false; top_left=false; bottom_left=false; top_right=true; bottom_right=true }
let eight = { top=true; middle=true; bottom=true; top_left=true; bottom_left=true; top_right=true; bottom_right=true }
let nine = { top=true; middle=true; bottom=true; top_left=true; bottom_left=false; top_right=true; bottom_right=true }

let patterns = [ zero; one; two; three; four; five; six; seven; eight; nine; ]

let pattern_to_string p =
    let top_char = if p.top then "_" else " " in
    let middle_char = if p.middle then "_" else " " in
    let bottom_char = if p.bottom then "_" else " " in
    let top_left_char = if p.top_left then "|" else " " in
    let bottom_left_char = if p.bottom_left then "|" else " " in
    let top_right_char = if p.top_right then "|" else " " in
    let bottom_right_char = if p.bottom_right then "|" else " " in
    String.concat [" "; top_char ; " \n"; 
                    top_left_char; middle_char ; top_right_char; "\n";
                    bottom_left_char; bottom_char; bottom_right_char; "\n";]

let print_pattern p = 
    let s = pattern_to_string p in
    printf "%s\n" s

let () = 
    List.iter patterns print_pattern
