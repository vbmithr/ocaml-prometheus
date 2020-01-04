module SMap : Map.S with type key := string
module FMap : Map.S with type key := float

type complex = {
  count: int;
  sum: float;
  data: float FMap.t;
}

val complex : int -> float -> (float * float) list -> complex

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
