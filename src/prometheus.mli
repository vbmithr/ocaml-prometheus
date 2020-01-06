module SMap : Map.S with type key := string
module FSet : Set.S with type elt := float
module FMap : Map.S with type key := float
module KLL : Kll.S with type elt := float

type 'a complex = {
  count: int ;
  sum: float ;
  data: 'a ;
}

type histogram = int FMap.t complex
type summary = (KLL.t * FSet.t) complex

val cumulate : int FMap.t -> int FMap.t
(** [cumulate hist] is the cumulative histogram of [hist], suitable to
    ingestion by Prometheus. *)

val complex_cum_fmap : int -> float -> int FMap.t -> histogram
(** [complex_cum count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a cumulated histogram or CDF. *)

val complex_cum : int -> float -> (float * int) list -> histogram
(** [complex_cum count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a cumulated histogram or
    CDF. *)

val complex : int -> float -> (float * int) list -> histogram
(** [complex count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a non-cumulated
    histogram. *)

type metric =
  | Counter of float
  | Gauge of float
  | Histogram of histogram
  | Summary of summary

type t = private {
  name: string;
  help: string option;
  labels: string SMap.t;
  ts: Ptime.t option;
  metric: metric
}

val add_labels : (string * string) list -> t -> t

val pp : t Fmt.t
val pp_list : t list Fmt.t

val counter   : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> float -> t
val gauge     : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> float -> t
val histogram : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> histogram -> t
val summary   : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> summary -> t
