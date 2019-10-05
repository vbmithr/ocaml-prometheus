module SMap : Map.S with type key := string
module FMap : Map.S with type key := float

type complex = {
  count: int;
  sum: float;
  data: float FMap.t;
}

val complex : int -> float -> (float * float) list -> complex

type 'a t = private {
  name: string;
  help: string option;
  labels: string SMap.t;
  ts: Ptime.t option;
  typ: 'a typ;
  v: 'a;
}
and _ typ

val pp : 'a t Fmt.t
val pp_list : 'a t list Fmt.t

val counter   : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> float -> float t
val gauge     : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> float -> float t
val histogram : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> complex -> complex t
val summary   : ?help:string -> ?labels:(string * string) list -> ?ts:Ptime.t -> string -> complex -> complex t
