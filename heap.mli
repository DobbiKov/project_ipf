type ('a, 'b) t = { size: int; elements: ('a * 'b) list }

(** The type of heaps. Elements are ordered using generic comparison. 
    Stores the number of elements*)

val empty : ('a , 'b) t
(** [empty] is the empty heap. *)

val add : ('a * 'b) -> ('a, 'b) t -> ('a, 'b) t
(** [add e h] add element [e] to [h]. *)

val find_min : ('a, 'b) t -> ('a * 'b)
(** [find_min h] returns the smallest elements of [h] w.r.t to 
    the generic comparison [<] *)

val remove_min : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
(** [remove_min h] returns the pair of the smallest elements of [h] w.r.t to 
    the generic comparison [<] and [h] where that element has been removed. *)

val is_singleton : ('a, 'b) t -> bool
(** [is_singleton h] returns [true] if [h] contains one element *)

val is_empty : ('a, 'b) t -> bool
(** [is_empty h] returns [true] if [h] contains zero element *)

val min_heapify : ('a, 'b) t -> ('a, 'b) t
(** [min_heapify h] ensures that [h] satisfies the min-heap property. *)