open Util
open Migrate_parsetree
open Ast_406
module Exp = Ast_helper.Exp
module Const = Ast_helper.Const
module Pat = Ast_helper.Pat
module Ast = Asttypes

module To_current = Convert(OCaml_406)(OCaml_current)


let lident name =
  Location.mknoloc (Longident.Lident name)


let longident name =
  Location.mknoloc (Longident.parse name)


let mk_type ~name ~reference_base ~reference_root ?required
    (top_schema : Swagger_t.schema) =
  let rec loop (schema : Swagger_t.schema) =
    match schema.ref with
    | Some r ->
      let path = Mod.reference_module_path ~reference_base ~reference_root r in
      let path = path @ ["type_info"] in
      let longid = some (Longident.unflatten path) in
      let type_info = Exp.ident (Location.mknoloc longid) in
      [%expr [%e type_info] ()]
    | None ->
      match some schema.kind with
      | `String  -> [%expr Depyt.string]
      | `Number  -> [%expr Depyt.float]
      | `Integer -> [%expr Depyt.int]
      | `Boolean -> [%expr Depyt.bool]
      | `Object  ->
        let open Swagger_j in
        begin match schema.additional_properties with
        | Some schema ->
          begin match schema.ref, schema.kind with
          | Some r, _ ->
            let path = Mod.reference_module_path ~reference_base ~reference_root r in
            let path = path @ ["Object"; "type_info"] in
            let longid = some (Longident.unflatten path) in
            let type_info = Exp.ident (Location.mknoloc longid) in
            [%expr [%e type_info] ()]
          | None, Some `String  -> [%expr Object.Of_strings.type_info ()]
          | None, Some `Number  -> [%expr Object.Of_floats.type_info ()]
          | None, Some `Integer -> [%expr Object.Of_ints.type_info ()]
          | None, Some `Boolean -> [%expr Object.Of_bools.type_info ()]
          | None, _ ->
            let val_typ = loop schema in
            [%expr Depyt.(list (pair string [%e val_typ]))]
          end
        | None -> [%expr Depyt.unit]
        end
      | `Array ->
        let open Swagger_j in
        match schema.items with
        | Some schema ->
          let item_typ = loop schema in
          [%expr list [%e item_typ]]
        | None ->
          failwith ("Type_info: array type must have an 'items' field")
  in
  let type' = loop top_schema in
  let is_optional =
    match required with
    | Some required -> not (List.mem name required)
    | None -> false in
  let name_exp = Exp.constant (Const.string name) in
  if is_optional then
    [%expr Depyt.named [%e name_exp] (Depyt.option [%e type'])]
  else
    [%expr Depyt.named [%e name_exp] [%e type']]



let mk_field ~reference_base ~reference_root ~required (name, schema) =
  let field_name = Exp.constant (Const.string name) in
  let field_type = mk_type ~name ~reference_base ~reference_root ~required schema in
  let field_getter =
    let get = Exp.field (Exp.ident (lident "t")) (lident name) in
    [%expr fun t -> [%e get]]
  in
  [%expr field [%e field_name] [%e field_type] [%e field_getter]]
;;

let mk_record ~reference_base ~reference_root ~name ~required props =
  let record_exp =
    let field_vars =
      List.map (fun (name, _schema) ->
          let name = lident name in
          (name, Exp.ident name))
        props
    in
    Exp.record field_vars None
  in
  let record_cons =
    List.fold_right (fun (name, _schema) acc->
        let var = Pat.var (Location.mknoloc name) in
        Exp.fun_ Ast.Nolabel None var acc)
      props
      record_exp
  in
  let record_fields =
    List.map (mk_field ~reference_base ~reference_root ~required) props in
  let record_name = Exp.constant (Const.string name) in
  let record =
    List.fold_left (fun acc field ->
        [%expr (|+) [%e acc] [%e field]])
      [%expr record [%e record_name] [%e record_cons]]
      record_fields
  in
  let record = [%expr
    let open Depyt in
    sealr [%e record]
  ] in
  record

let to_string t =
  t
  |> To_current.copy_expression
  |> Format.asprintf "%a@." Pprintast.expression

