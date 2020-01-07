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

type t = {
  name: string;
  help: string option;
  metric: metricType ;
}

and metricType =
  | Counter of float metric list
  | Gauge of float metric list
  | Histogram of histogram metric list
  | Summary of summary metric list

and 'a metric = {
  labels : string SMap.t ;
  ts: Ptime.t option ;
  v: 'a ;
}

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

let pp_complex_histogram name ppf { labels; ts; v = { data; _ } as cplx } =
  Fmt.list ~sep:Format.pp_print_newline
    (pp_histogram_line name labels ts) ppf (FMap.bindings data) ;
  Format.pp_print_newline ppf () ;
  pp_sum_count name labels ppf cplx

let pp_complex_summary name ppf { labels; ts; v = { data; _ } as cplx } =
  Fmt.list ~sep:Format.pp_print_newline
    (pp_summary_line name labels ts) ppf (FMap.bindings data) ;
  Format.pp_print_newline ppf () ;
  pp_sum_count name labels ppf cplx

let pp_typ ppf = function
  | Counter _   -> Fmt.pf ppf "counter"
  | Gauge _     -> Fmt.pf ppf "gauge"
  | Histogram _ -> Fmt.pf ppf "histogram"
  | Summary _   -> Fmt.pf ppf "summary"

let metric ?(labels=[]) ?ts v =
  { labels = SMap.of_seq (List.to_seq labels); ts; v }

let add_labels labels t =
  let labels = SMap.add_seq (List.to_seq labels) t.labels in
  { t with labels }

let add_labels labels t =
  match t.metric with
  | Counter a -> { t with metric = Counter (List.map (add_labels labels) a) }
  | Gauge a -> { t with metric = Gauge (List.map (add_labels labels) a) }
  | Histogram a -> { t with metric = Histogram (List.map (add_labels labels) a) }
  | Summary a -> { t with metric = Summary (List.map (add_labels labels) a) }

let counter ?help name metrics   = { help; name; metric = (Counter metrics)   }
let gauge ?help name metrics     = { help; name; metric = (Gauge metrics)     }
let histogram ?help name metrics = { help; name; metric = (Histogram metrics) }
let summary ?help name metrics   = { help; name; metric = (Summary metrics)   }

let pp_hdr ppf { name; help; metric; _ } =
  Option.iter (fun msg -> Fmt.pf ppf "# HELP %s %s@." name msg) help ;
  Fmt.pf ppf "# TYPE %s %a@." name pp_typ metric

let pp_metric name ppf { labels; ts; v } =
  Fmt.pf ppf "%s%a %f %a" name pp_labels labels v (Fmt.option pp_ts) ts

let pp_histogram name ppf hist =
  pp_complex_histogram name ppf hist

let pp_summary name ppf t =
  let aux { sum; count; data = (kll, pct)} =
    let cdf = KLL.cdf kll in
    let find_pct n = List.find_opt (fun (_,p) -> p -. n > 0.) cdf in
    let quantiles =
      FSet.fold (fun l a -> (l, find_pct l) :: a) pct [] in
    let quantiles =
      List.filter_map
        (fun (p, v) -> Option.map (fun (e,_) -> p, e) v) quantiles in
    complex_cum count sum quantiles in
  pp_complex_summary name ppf { t with v = (aux t.v) }

let pp ppf t =
  pp_hdr ppf t ;
  match t.metric with
  | Counter a -> Fmt.list ~sep:Format.pp_print_newline (pp_metric t.name) ppf a
  | Gauge a -> Fmt.list ~sep:Format.pp_print_newline (pp_metric t.name) ppf a
  | Histogram a -> Fmt.list ~sep:Format.pp_print_newline (pp_histogram t.name) ppf a
  | Summary a -> Fmt.list ~sep:Format.pp_print_newline (pp_summary t.name) ppf a

let pp_list ts =
  Fmt.list ~sep:Format.pp_print_newline pp ts
