
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

let height magnification = 1 + 2 * magnification
    
let is_first_row row_number = row_number = 0
let is_last_row row_number magnification = row_number + 1 = height magnification
let is_middle_row row_number magnification = row_number = magnification
let is_top_half row_number magnification = row_number < magnification
let is_bottom_half row_number magnification = row_number > magnification

let pattern_to_line pattern row_number magnification =
    if is_first_row row_number then
        let c = if pattern.top then '_' else ' ' in
            String.concat [" "; String.make magnification c; " "]
    else if is_last_row row_number magnification then
        let c = if pattern.bottom then '_' else ' ' in
        let s = if pattern.bottom_left then "|" else " " in
        let e = if pattern.bottom_right then "|" else " " in
            String.concat [s; String.make magnification c; e]
    else if is_middle_row row_number magnification then 
        let c = if pattern.middle then '_' else ' ' in
        let s = if pattern.top_left then "|" else " " in
        let e = if pattern.top_right then "|" else " " in
            String.concat [s; String.make magnification c; e]
    else if is_top_half row_number magnification then 
        let c = ' ' in
        let s = if pattern.top_left then "|" else " " in
        let e = if pattern.top_right then "|" else " " in
            String.concat [s; String.make magnification c; e]
    else if is_bottom_half row_number magnification then
        let c = ' ' in
        let s = if pattern.bottom_left then "|" else " " in
        let e = if pattern.bottom_right then "|" else " " in
            String.concat [s; String.make magnification c; e]
    else "ERROR" 

let range limit = 
    let rec range' n ns = 
        if n = 0 then 0 :: ns
        else n :: range' (n-1) ns
    in List.rev (range' limit [])

let pattern_to_lines pattern magnification = 
    let length = height magnification in
    let ns = range (length-1) in
    List.map ns (function n -> pattern_to_line pattern n magnification)

let print_pattern p = 
    let s = pattern_to_string p in
    printf "%s\n" s

let print_magnified_pattern pattern magnification =
    let lines = pattern_to_lines pattern magnification in
    List.iter lines (function l -> printf "%s\n" l)

let () = 
    List.iter patterns (function p -> print_magnified_pattern p 3)

let print_magnified_line pattern magnification line_number = 
    let line = pattern_to_line pattern line_number magnification in
    printf "%s\n" line
