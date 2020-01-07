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

module LabelsMap : Map.S with type key := string SMap.t
(** Map of labels: One prometheus timeseries is identified by name AND
    label set. *)

type 'a metric = 'a series LabelsMap.t
and 'a series = { ts: Ptime.t option ; v: 'a }

type t

val add_labels : (string * string) list -> t -> t

val pp : t Fmt.t
val pp_list : t list Fmt.t

val counter   : ?help:string -> string -> float series LabelsMap.t -> t
val gauge     : ?help:string -> string -> float series LabelsMap.t -> t
val histogram : ?help:string -> string -> histogram series LabelsMap.t -> t
val summary   : ?help:string -> string -> summary series LabelsMap.t -> t
