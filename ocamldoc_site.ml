open Core_kernel.Std
open Opium.Std
open Cow

type package = 
  { name: string
  ; version: string
  ; url: string
  ; doc_url: string }

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
        let doc_url = urldecode pkg.doc_url ^ "/index.html" in
        <:html<<a href=$str:doc_url$><b>$str:pkg.name$</b> <i>$str:pkg.version$</i></a>&>>)
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
      display: inline-block;
      position: fixed;
      top: 48px;
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
            <img src="/img/colour-icon-170x148.png" width="55" height="48"></img>
            About OCaml
          </a>
        </nav>
        <div id="sidebar">$plist$</div>
        <div id="content-wrapper"></div>
      </body>
    </html>&>>

let update_packages = get "/sys/update-packages/" (fun req ->
  let url =
    "https://raw.githubusercontent.com/chrismamo1/ocamldoc-site/master/packages-list.csv" in
  let status = Unix.system ("wget " ^ url ^ " -O packages-list.csv") in
  match status with
  | WEXITED 0 ->
      let ic = open_in "packages-list.csv" in
      let csv = Csv.of_channel ~header:["name"; "version"; "url"; "command"] ic in
      let rows = Csv.Rows.input_all csv in
      let db = Sqlite3.db_open "packages.db" in
      let packages =
        List.map
          rows
          ~f:(fun row ->
            let url = Csv.Row.find row "url" in
            let name = Csv.Row.find row  "name" in
            let command = Csv.Row.find row "command" in
            let () = Unix.chdir "./docs" in
            let _ =
              begin match String.is_prefix url ~prefix:"svn://" with
              | true -> (* use svn *)
                  Unix.system ("svn checkout " ^ url)
              | false -> (* otherwise, assume that it uses Git *)
                  Unix.system ("git clone " ^ url)
              end
            in
            let () = Unix.chdir ("./" ^ name) in
            let _ = Unix.system command in
            let doc_url = "/docs/" ^ name ^ "/doc" in
            let () = Unix.chdir "../../" in
            let version = Csv.Row.find row "version" in
            let sql =
              Printf.sprintf
                "INSERT INTO packages ('name', 'version', 'url', 'doc_url') \
                  VALUES ('%s', '%s', '%s', '%s');"
                (urlencode name)
                (urlencode version)
                (urlencode url)
                (urlencode doc_url)
            in
            let rc = Sqlite3.exec db sql in
            begin match rc with
            | Sqlite3.Rc.OK -> { name; version; url; doc_url }
            | _ ->
                let _ = Sqlite3.db_close db in
                raise (Failure ("sql: " ^ sql ^ "\n\nerror: " ^ Sqlite3.Rc.to_string rc))
            end)
      in
      let _ = Sqlite3.db_close db in
      (`String "Success." |> respond')
  | _ -> `String "Failure." |> respond' )

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
    let rc = Sqlite3.exec db ~cb "SELECT * FROM packages" in
    !aux
  in
  let _ = Sqlite3.db_close db in
  `Html (packages |> homepage |> Html.to_string)
  |> respond')

let _ =
  App.empty
  |> middleware (Middleware.static ~local_path:"./" ~uri_prefix:"")
  |> docview
  |> update_packages
  |> App.run_command
