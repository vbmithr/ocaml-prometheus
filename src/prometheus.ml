module FSet = Set.Make(Float)
module SMap = Map.Make(String)
module FMap = Map.Make(Float)
module KLL  = Kll.Make(Float)

type 'a complex = {
  count: int;
  sum: float;
  data: 'a;
}

type histogram = int FMap.t complex
type summary = (KLL.t * FSet.t) complex

let cumulate data =
  let open FMap in
  let cum, a =
    fold begin fun k v (cum,a) ->
      let v = cum + v in
      v, add k v a
    end data (0,empty) in
  add Float.infinity cum a

let complex_cum_fmap count sum data =
  { count; sum; data }
let complex_cum count sum data =
  { count; sum; data = FMap.of_seq (List.to_seq data) }
let complex count sum data =
  { count; sum; data = cumulate (FMap.of_seq (List.to_seq data)) }

module Descr = struct
  module T = struct
    type t = {
      name: string ;
      help: string option ;
    }
    let compare { name; _ } { name = name'; _ } = String.compare name name'
    let equal { name; _ } { name = name'; _ } = String.equal name name'
    let hash { name; _ } = Hashtbl.hash name
  end
  include T
  module Map = Map.Make(T)
  module Table = Hashtbl.Make(T)

  let create ?help name = { help; name }
end

module LabelsMap = Map.Make(struct
    type t = string SMap.t
    let compare = SMap.compare String.compare
  end)

type 'a metric = 'a series LabelsMap.t
and 'a series = { ts: Ptime.t option ; v: 'a }

type _ typ =
  | Counter : float typ
  | Gauge : float typ
  | Histogram : histogram typ
  | Summary : summary typ

type m = Metric : 'a typ * 'a metric -> m
type t = Descr.t * m

let add_labels labels (descr, (Metric (typ, m))) =
  let m = LabelsMap.fold begin fun k v a ->
      LabelsMap.add (SMap.add_seq (List.to_seq labels) k) v a
    end m LabelsMap.empty in
  (descr, Metric (typ, m))

let pp_ts ppf ts =
  Fmt.pf ppf "%f" (Ptime.to_float_s ts *. 1e3)

let pp_float ppf f =
  match Float.classify_float f with
  | FP_nan ->  Fmt.string ppf "Nan"
  | FP_infinite when Float.sign_bit f -> Fmt.string ppf "-Inf"
  | FP_infinite -> Fmt.string ppf "+Inf"
  | _ -> Fmt.float ppf f

let pp_label ppf (k, v) = Fmt.pf ppf "%s=\"%s\"" k v

let pp_labels ppf labels =
  if not (SMap.is_empty labels) then begin
    Fmt.pf ppf "{" ;
    Fmt.list ~sep:(fun ppf () -> Fmt.string ppf ",")
      pp_label ppf (SMap.bindings labels) ;
    Fmt.pf ppf "}"
  end

let pp_sum_count name labels ppf { count; sum; _ } =
  Fmt.pf ppf "%s_sum%a %a@." name pp_labels labels pp_float sum ;
  Fmt.pf ppf "%s_count%a %d" name pp_labels labels count

let pp_histogram_line name labels ts ppf (le, v) =
  let labels = SMap.add "le" (Fmt.str "%a" pp_float le) labels in
  Fmt.pf ppf "%s_bucket%a %d %a" name pp_labels labels v (Fmt.option pp_ts) ts

let pp_summary_line name labels ts ppf (le, v) =
  let labels = SMap.add "quantile" (Fmt.str "%a" pp_float le) labels in
  Fmt.pf ppf "%s%a %f %a" name pp_labels labels v (Fmt.option pp_ts) ts

let pp_complex_histogram name labels ppf { ts; v = { data; _ } as cplx } =
  Fmt.list ~sep:Format.pp_print_newline
    (pp_histogram_line name labels ts) ppf (FMap.bindings data) ;
  Format.pp_print_newline ppf () ;
  pp_sum_count name labels ppf cplx

let pp_complex_summary name labels ppf { ts; v = { data; _ } as cplx } =
  Fmt.list ~sep:Format.pp_print_newline
    (pp_summary_line name labels ts) ppf (FMap.bindings data) ;
  Format.pp_print_newline ppf () ;
  pp_sum_count name labels ppf cplx

let pp_typ :
  type a. Format.formatter -> a typ -> unit = fun ppf -> function
  | Counter   -> Fmt.pf ppf "counter"
  | Gauge     -> Fmt.pf ppf "gauge"
  | Histogram -> Fmt.pf ppf "histogram"
  | Summary   -> Fmt.pf ppf "summary"

let counter ?help name metrics   = Descr.create ?help name, Metric (Counter, metrics)
let gauge ?help name metrics     = Descr.create ?help name, Metric (Gauge, metrics)
let histogram ?help name metrics = Descr.create ?help name, Metric (Histogram, metrics)
let summary ?help name metrics   = Descr.create ?help name, Metric (Summary, metrics)

let pp_hdr ppf { Descr.name; help } metric =
  Option.iter (fun msg -> Fmt.pf ppf "# HELP %s %s@." name msg) help ;
  Fmt.pf ppf "# TYPE %s %a@." name pp_typ metric

let pp_metric name ppf (labels, { ts; v }) =
  Fmt.pf ppf "%s%a %f %a" name pp_labels labels v (Fmt.option pp_ts) ts

let pp_histogram name ppf (labels, hist) =
  pp_complex_histogram name labels ppf hist

let pp_summary name ppf (labels, t) =
  let aux { sum; count; data = (kll, pct)} =
    let cdf = KLL.cdf kll in
    let find_pct n = List.find_opt (fun (_,p) -> p -. n > 0.) cdf in
    let quantiles =
      FSet.fold (fun l a -> (l, find_pct l) :: a) pct [] in
    let quantiles =
      List.filter_map
        (fun (p, v) -> Option.map (fun (e,_) -> p, e) v) quantiles in
    complex_cum count sum quantiles in
  pp_complex_summary name labels ppf { t with v = (aux t.v) }

let pp ppf (descr, Metric (typ, data)) =
  pp_hdr ppf descr typ ;
  match typ with
  | Counter -> Fmt.list ~sep:Format.pp_print_newline (pp_metric descr.name) ppf (LabelsMap.bindings data)
  | Gauge -> Fmt.list ~sep:Format.pp_print_newline (pp_metric descr.name) ppf (LabelsMap.bindings data)
  | Histogram -> Fmt.list ~sep:Format.pp_print_newline (pp_histogram descr.name) ppf (LabelsMap.bindings data)
  | Summary -> Fmt.list ~sep:Format.pp_print_newline (pp_summary descr.name) ppf (LabelsMap.bindings data)

let pp_list ts =
  Fmt.list ~sep:Format.pp_print_newline pp ts
