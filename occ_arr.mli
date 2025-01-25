type 'a occ_table = ('a * int) list

val count_occs: 'a list -> 'a occ_table
    (**
    [count_occs in_arr] takes an array of bytes (that represent chars) 
and returns an array if form [(byte, frequency), ] of frequency of each char
     *)

val construct_occs_table: 'a list -> 'a occ_table
(**
    [construct_occs_table in_arr] takes an array of bytes, construct an array of frequencies and sorts it
    returns an array of form [(char, int(frequency)), ]
        *)

val print_occ_list: int occ_table -> unit
    (**
    [print_occ_list l] takes a frequency table an prints it
    *)

val int_to_bits: int -> int list
    (**
    Converts an integer (0-255) to an array of 8 bits 
    [int_to_bits n]
    *)

val pow: int -> int -> int
    (**
       [pow a n] returns a^n
     *)

val is_singleton: 'a list -> bool
(**
   [is_singleton l]
   returns true if the list contains only one element
   and false otherwise
 *)

val is_empty: 'a list -> bool
(**
    [is_empty l] 
    returns true if the list is empty
    and false otherwise
 *)

val find_min_in_sorted: 'a list -> 'a
(**
   [find_min_in_sorted l] returns the smallest element in the sorted list
 *)


val occ_table_to_heap : 'a occ_table -> ('a, int) Heap.t
(**
    [occ_table_to_heap l] takes a frequency table and returns a heap
    *)

val construct_occs_heap: 'a list -> ('a, int) Heap.t
(**
    [construct_occs_heap l] takes an array of bytes, constructs a frequency table and returns a heap
    *)
