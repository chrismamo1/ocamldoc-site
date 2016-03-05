open Core_kernel.Std
open Opium.Std
open Cow

type package = 
  { name: string
  ; version: string
  ; url: string
  ; doc_url: string }

type recipe =
  { name: string
  ; get_cmd: string
  ; make_cmd: string
  ; docdir: string
  ; deps: string
  ; version: string }

let urlencode = Netencoding.Url.encode
let urldecode = Netencoding.Url.decode

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
        let doc_url = urlencode pkg.doc_url in
        <:html<<a target="doc-container" href=$str:doc_url$><b>$str:pkg.name$</b> <i>$str:pkg.version$</i></a>&>>)
    |> Html.Create.ul
  in
  let style = <:css<
    a {
      color: #420000;
      text-decoration: none; }
    body {
      font-stack: Verdana, Helvetica, sans-serif; }
    nav {
      list-style-type: none;
      background-color: #42ff42;
      box-shadow: 0 1px 3px rgba(0,0,0,0.12),
      0 1px 2px rgba(0,0,0,0.24);
      color: rgba(255,255,255,0.8);
      font-size: 24px;
      height: 56px;
      line-height: 48px;
      padding: 0 0;
      margin-left: 0;
      left: 0;
      position: fixed;
      top: 0;
      width: calc(100% - 2px);
      z-index: 1000; }
    nav * {
      vertical-align: middle; }
    nav #links {
      display: inline;
      position: absolute;
      right: 25px; }
    #sidebar {
      background-color: #fcfcfc;
      border-right: 1px solid #dedede;
      display: inline-block;
      overflow-y: auto;
      position: fixed;
      top: 56px;
      width: 240px;
      height: 100%; }
    #sidebar li:hover {
      background: rgba(0,0,0,0.1); }
    #content-wrapper {
      position: fixed;
      top: 56px;
      height: calc(100% - 56px);
      margin-left: 240px;
      text-align: left;
      vertical-align: top;
      width: calc(100% - 240px); }
     >>
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
            <img src="/img/colour-icon-170x148.png" width="55" height="48"></img>
            About OCaml
          </a>
          <div id="links">
            <a href="null">About</a>
          </div>
        </nav>
        <div id="sidebar">$plist$</div>
        <iframe name="doc-container" id="content-wrapper"></iframe>
      </body>
    </html>&>>

let get_packages () =
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
      let find x = List.Assoc.find_exn row x |> urldecode in
      ignore(aux :=
        { name = find "name"
        ; version = find "version"
        ; url = find "url"
        ; doc_url = find "doc_url" } :: !aux)
    in
    let rc = Sqlite3.exec db ~cb "SELECT * FROM packages ORDER BY name" in
    !aux
  in
  let _ = Sqlite3.db_close db in
  packages

let get_recipe pkg_name pkg_vers =
  let db = Sqlite3.db_open "packages.db" in
  let sql =
    Printf.sprintf
      "SELECT * FROM packages WHERE name='%s' AND version='%s' LIMIT 1"
      (urlencode pkg_name)
      (urlencode pkg_vers) in
  let recipe = ref None in
  let cb = fun r h ->
    (* this next thing is crazy non-performant, but I like associations *)
    let row =
      List.map2_exn
        (Array.to_list h)
        (Array.to_list r)
        (fun h r -> h, (unoption r)) in
    (* shorthand for grabbing from a particular association *)
    let find x = List.Assoc.find_exn row x |> urldecode in
    ignore(recipe := Some
      { name = find "package"
      ; get_cmd = find "get_cmd"
      ; make_cmd = find "make_cmd"
      ; docdir = find "docdir"
      ; deps = find "deps"
      ; version = find "version" })
  in
  let rc = Sqlite3.exec db ~cb sql in
  let _ = Sqlite3.db_close db in
  !recipe

let update_packages = get "/sys/update-packages/" (fun req ->
  let packages = get_packages () in
  let () = Unix.chdir "./docs" in
  let () =
    List.iter packages
      ~f:(fun pkg ->
        let recipe = get_recipe pkg.name pkg.version in
        match recipe with
        | None -> ()
        | Some r ->
            let _ = Unix.mkdir "tmp" in (** TODO: if this fails, very bad things *)
            let _ = Unix.chdir "./tmp" in
            let _ = Unix.system r.get_cmd in
            let _ = Unix.chdir (urlencode r.name) in
            let _ = Unix.system r.make_cmd in
            let _ = Unix.chdir ".." in
            let _ =
              Unix.system
                ( "mv ./" ^ urlencode r.name ^ " ../" ^ urlencode r.name ^ "/"
                  ^ urlencode r.version) in
            let _ = Unix.system "rm -rf ./* && cd ../ && rmdir tmp" in
            ())
  in
  let () = Unix.chdir ".." in
  `String "Success!" |> respond')


let docview = get "/:package" (fun req ->
  let packages = get_packages () in
  `Html (packages |> List.rev |> homepage |> Html.to_string)
  |> respond')

let _ =
  App.empty
  |> middleware (Middleware.static ~local_path:"./" ~uri_prefix:"")
  |> docview
  |> update_packages
  |> App.run_command
