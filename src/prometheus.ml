module FSet = Set.Make(Float)
module SMap = struct
  include Map.Make(String)

  let of_bindings bds =
    List.fold_left (fun a (k, v) -> add k v a) empty bds
end

module FMap = struct
  include Map.Make(Float)

  let of_bindings bds =
    List.fold_left (fun a (k, v) -> add k v a) empty bds
end

module KLL = Kll.Make(Float)

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
  { count; sum; data = FMap.of_bindings data }
let complex count sum data =
  { count; sum; data = cumulate (FMap.of_bindings data) }

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

let pp_sum_count name ppf { count; sum; _ } =
  Fmt.pf ppf "%s_sum %a@." name pp_float sum ;
  Fmt.pf ppf "%s_count %d" name count

let pp_histogram_line name labels ppf (le, v) =
  let labels = SMap.add "le" (Fmt.str "%a" pp_float le) labels in
  Fmt.pf ppf "%s_bucket%a %d" name pp_labels labels v

let pp_summary_line name labels ppf (le, v) =
  let labels = SMap.add "quantile" (Fmt.str "%a" pp_float le) labels in
  Fmt.pf ppf "%s%a %f" name pp_labels labels v

let pp_complex_histogram name labels ppf ({ data; _ } as cplx) =
  Fmt.list ~sep:Format.pp_print_newline
    (pp_histogram_line name labels) ppf (FMap.bindings data) ;
  Format.pp_print_newline ppf () ;
  pp_sum_count name ppf cplx

let pp_complex_summary name labels ppf ({ data; _ } as cplx) =
  Fmt.list ~sep:Format.pp_print_newline
    (pp_summary_line name labels) ppf (FMap.bindings data) ;
  Format.pp_print_newline ppf () ;
  pp_sum_count name ppf cplx

type metric =
  | Counter of float
  | Gauge of float
  | Histogram of histogram
  | Summary of summary

let pp_metric ppf = function
  | Counter _ -> Fmt.pf ppf "counter"
  | Gauge _ -> Fmt.pf ppf "gauge"
  | Histogram _ -> Fmt.pf ppf "histogram"
  | Summary _ -> Fmt.pf ppf "summary"

type t = {
  name: string;
  help: string option;
  labels: string SMap.t;
  ts: Ptime.t option;
  metric: metric;
}

let add_labels labels t =
  let labels = SMap.add_seq (List.to_seq labels) t.labels in
  { t with labels }

let counter ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; metric = Counter v }

let gauge ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; metric = Gauge v }

let histogram ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; metric = Histogram v }

let summary ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; metric = Summary v }

let pp_ts ppf ts =
  Fmt.pf ppf "%f" (Ptime.to_float_s ts *. 1e3)

let pp_hdr ppf { name; help; metric; _ } =
  Option.iter (fun msg -> Fmt.pf ppf "# HELP %s %s@." name msg) help ;
  Fmt.pf ppf "# TYPE %s %a@." name pp_metric metric

let pp_simple ppf { name; labels; ts; metric; _ } =
  match metric with
  | Counter a | Gauge a ->
    Fmt.pf ppf "%s%a %f %a" name pp_labels labels a (Fmt.option pp_ts) ts
  | _ -> assert false

let pp_histogram ppf { name; labels; metric; _ } =
  match metric with
  | Histogram a -> pp_complex_histogram name labels ppf a
  | _ -> assert false

let pp_summary ppf { name; labels; metric; _ } =
  let aux { sum; count; data = (kll, pct)} =
    let cdf = KLL.cdf kll in
    let find_pct n = List.find_opt (fun (_,p) -> p -. n > 0.) cdf in
    let quantiles =
      FSet.fold (fun l a -> (l, find_pct l) :: a) pct [] in
    let quantiles =
      List.filter_map
        (fun (p, v) -> Option.map (fun (e,_) -> p, e) v) quantiles in
    complex_cum count sum quantiles in
  match metric with
  | Summary t -> pp_complex_summary name labels ppf (aux t)
  | _ -> assert false

let pp ppf t =
  pp_hdr ppf t ;
  match t.metric with
  | Counter _ -> pp_simple ppf t
  | Gauge _ -> pp_simple ppf t
  | Histogram _ -> pp_histogram ppf t
  | Summary _ -> pp_summary ppf t

let pp_list ts =
  Fmt.list ~sep:Format.pp_print_newline pp ts
