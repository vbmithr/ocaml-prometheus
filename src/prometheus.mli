module SMap : Map.S with type key := string
module FMap : Map.S with type key := float
module KLL : Kll.S with type elt := float

type complex = {
  count: int;
  sum: float;
  data: float FMap.t;
}

val cumulate : float FMap.t -> float FMap.t
(** [cumulate hist] is the cumulative histogram of [hist], suitable to
    ingestion by Prometheus. *)

val complex_cum_fmap : int -> float -> float FMap.t -> complex
(** [complex_cum count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a cumulated histogram or CDF. *)

val complex_cum : int -> float -> (float * float) list -> complex
(** [complex_cum count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a cumulated histogram or
    CDF. *)

val complex : int -> float -> (float * float) list -> complex
(** [complex count sum data] is a [complex] value constructed from
    [count], [sum] and [data] where [data] is a non-cumulated
    histogram. *)

type metric =
  | Counter of float
  | Gauge of float
  | Histogram of complex
  | Summary of complex

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
val histogram : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> complex -> t
val summary   : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> complex -> t
