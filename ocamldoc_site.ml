open Core_kernel.Std
open Opium.Std
open Cow

type package = 
  { name: string
  ; version: string
  ; url: string
  ; doc_url: string }

let unoption ?(default=None) =
  (** match on `default` first so that partial-application will yield a function
    * custom for the specified default value, which should marginally improve
    * performance in some cases *)
  match default with
  | Some d ->
      begin function
        | Some x -> x
        | None -> d
      end
  | None ->
      begin function
        | Some x -> x
        | None -> raise (Failure "gambled and lost on an unsafe call to `unoption`")
      end

let homepage packages =
  let plist =
    List.map
      packages
      ~f:(fun pkg ->
        <:html<<a><b>$str:pkg.name$</b> <i>$str:pkg.version$</i></a>&>>)
    |> Html.Create.ul
  in
  let style = <:css<
    * {
      font-stack: Verdana, Helvetica, sans-serif;
    }
    nav#topnav {
      height: 48px;
      position: fixed;
      top: 0;
      line-height: 48px; }
    #sidebar {
      position: fixed;
      left: 0;
      width: 240px; }
    #content-wrapper {
      margin-left: 240px;
      width: calc(100% - 240px); } >>
    |> Html.Create.stylesheet
  in
  <:html<
    <html>
      <head>
        <style>$style$</style>
      </head>
      <body>
        <nav id="topnav">
          <a href="https://ocaml.org">
            <img src="/img/colour-icon-170x148.png" width="55" height="48">ocaml logo</img>
            About OCaml
          </a>
        </nav>
        <div id="sidebar">$plist$</div>
        <div id="content-wrapper"></div>
      </body>
    </html>&>>

let docview = get "/:package" (fun req ->
  let db = Sqlite3.db_open "packages.db" in
  let packages =
    let aux = ref [] in
    let unoption = unoption ~default:(Some "") in
    let cb = fun r h ->
      (* this next thing is crazy non-performant, but I like associations *)
      let row =
        List.map2_exn
          (Array.to_list h)
          (Array.to_list r)
          (fun h r -> h, (unoption r)) in
      (* shorthand for grabbing from a particular association *)
      let find = List.Assoc.find_exn row in
      ignore(aux :=
        { name = find "name"
        ; version = find "version"
        ; url = find "url"
        ; doc_url = find "doc_url" } :: !aux)
    in
    let rc = Sqlite3.exec db ~cb in
    !aux
  in
  `Html (packages |> homepage |> Html.to_string)
  |> respond')

let _ =
  App.empty
  |> middleware (Middleware.static ~local_path:"./" ~uri_prefix:"")
  |> docview
  |> App.run_command
