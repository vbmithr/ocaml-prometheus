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

type complex = {
  count: int;
  sum: float;
  data: int FMap.t;
}

let complex count sum data =
  { count; sum; data = FMap.of_bindings data }

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
  Fmt.pf ppf "%s%a %d" name pp_labels labels v

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

type _ typ =
  | Counter : float typ
  | Gauge : float typ
  | Histogram : complex typ
  | Summary : complex typ

let pp_typ : type a. a typ Fmt.t = fun ppf -> function
  | Counter -> Fmt.pf ppf "counter"
  | Gauge -> Fmt.pf ppf "gauge"
  | Histogram -> Fmt.pf ppf "histogram"
  | Summary -> Fmt.pf ppf "summary"

type 'a t = {
  name: string;
  help: string option;
  labels: string SMap.t;
  ts: Ptime.t option;
  typ: 'a typ;
  v: 'a;
}

let pp_ts ppf ts =
  Fmt.pf ppf "%f" (Ptime.to_float_s ts *. 1e3)

let pp_hdr : type a. a t Fmt.t =
  fun ppf { name; help; typ; _ } ->
  Option.iter (fun msg -> Fmt.pf ppf "# HELP %s %s@." name msg) help ;
  Fmt.pf ppf "# TYPE %s %a@." name pp_typ typ

let pp_simple ppf { name; labels; ts; v; _ } =
  Fmt.pf ppf "%s%a %f %a" name pp_labels labels v (Fmt.option pp_ts) ts

let pp_histogram ppf { name; labels; v; _ } =
  pp_complex_histogram name labels ppf v

let pp_summary ppf { name; labels; v; _ } =
  pp_complex_summary name labels ppf v

let pp : type a. a t Fmt.t = fun ppf t ->
  pp_hdr ppf t ;
  match t.typ with
  | Counter -> pp_simple ppf t
  | Gauge -> pp_simple ppf t
  | Histogram -> pp_histogram ppf t
  | Summary -> pp_summary ppf t

let pp_list ts =
  Fmt.list ~sep:Format.pp_print_newline pp ts

let counter ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; typ = Counter ; v }

let gauge ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; typ = Gauge ; v }

let histogram ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; typ = Histogram ; v }

let summary ?help ?(labels=[]) ?ts name v = {
  name; help; labels = SMap.of_bindings labels;
  ts; typ = Summary ; v }
