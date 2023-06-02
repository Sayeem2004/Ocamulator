open Alias

(** [Util.ml] contains random utilities that could not be sorted elsewhere. *)

val open_rom : string -> in_channel
(** [open_rom name] opens the file [../data/rom/name] and returns the input
    channel associated with it. *)

val set_color : uint8 -> unit
(** [set_color color] sets the color of the pixel to [color]. *)

val addr_interval : uint16 -> uint16 -> uint16 -> bool

val psize : int
(** [psize] is the size of a pixel. *)

val ssize : int
(** [ssize] is the size of the screen. *)

val stall : int
(** [stall] is the number of cycles to stall the CPU for. *)
