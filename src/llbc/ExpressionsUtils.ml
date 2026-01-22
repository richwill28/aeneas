include Charon.ExpressionsUtils
open Charon.Expressions
open Charon.Types
open Charon.Utils
open Errors

(** Return [true] if a place accesses a global *)
let rec place_accesses_global (p : place) : bool =
  match p.kind with
  | PlaceLocal _ -> false
  | PlaceGlobal _ -> true
  | PlaceProjection (p, _) -> place_accesses_global p

(** Return [true] if an rvalue accesses a global *)
let rvalue_accesses_global (rv : rvalue) : bool =
  let visitor =
    object
      inherit [_] iter_rvalue

      method! visit_place _ p =
        if place_accesses_global p then raise Found else ()
    end
  in
  try
    visitor#visit_rvalue () rv;
    false
  with Found -> true

(** Convert [borrow_kind] (expression-level) to [ref_kind] (type-level) *)
let borrow_kind_to_ref_kind (span : Meta.span) (bk : borrow_kind) : ref_kind =
  match bk with
  | BShared | BShallow -> RShared
  | BMut | BTwoPhaseMut -> RMut
  | BUniqueImmutable ->
      [%craise] span "Unique immutable closure captures are not supported"

(** Convert [view_field] (expression-level, with [borrow_kind]) to
    [ty_view_field] (type-level, with [ref_kind]) *)
let view_field_to_ty_view_field (span : Meta.span) (vf : view_field) :
    ty_view_field =
  { path = vf.path; mutbl = borrow_kind_to_ref_kind span vf.kind }

(** Convert a list of [view_field] to [ty_view_field] *)
let view_to_ty_view (span : Meta.span) (view : view_field list option) :
    ty_view_field list option =
  Option.map (List.map (view_field_to_ty_view_field span)) view
