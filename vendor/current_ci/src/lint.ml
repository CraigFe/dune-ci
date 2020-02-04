open Current.Syntax

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

type source =
  | Opam of { version : string }
  | Vendored of { path : string }
[@@deriving yojson,eq]

module Analyse = struct
  let ocamlformat_version_from_string =
      let re =
        Re.(
          seq
            [
              start;
              rep space;
              str "version";
              rep space;
              char '=';
              rep space;
              group (rep1 @@ diff graph (set "#"));
              rep space;
              eol;
            ]
          |> compile)
      in
      fun path ->
        Re.exec_opt re path |> function
        | Some g -> Some (Re.Group.get g 1)
        | None -> None

  let ocamlformat_version_from_file job path =
    let ( let+ ) = Lwt.Infix.( >|= ) in
    if not (Sys.file_exists path) then
      let () = Current.Job.log job "No .ocamlformat file found" in
      Lwt.return (Ok None)
    else
      let+ versions = Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
          Lwt_io.read_lines channel
          |> Lwt_stream.filter_map ocamlformat_version_from_string
          |> Lwt_stream.to_list
        )
      in
      match versions with
      | [ v ] ->
          let () =
            Current.Job.log job "Found OCamlformat version '%s' in dotfile" v
          in
          Ok (Some v)
      | _ -> Error (`Msg "Unable to parse .ocamlformat file")

  let get_ocamlformat_source job ~opam_files ~root =
    let (>|=) = Lwt.Infix.(>|=) in
    let proj_is_ocamlformat p = String.equal (Filename.basename p) "ocamlformat.opam" in
    match List.find_opt proj_is_ocamlformat opam_files with
    | Some opam_file ->
      let path = Filename.dirname opam_file in
      Lwt.return (Some (Vendored { path }))
    | None ->
      Fpath.(to_string (root / ".ocamlformat")) |> ocamlformat_version_from_file job
      >|= function
      | Ok (Some version) -> Some (Opam { version })
      | Ok None -> None
      | Error (`Msg e) -> failwith e
end

let ocamlformat_dockerfile ~base =
  let open Dockerfile in
  let version = "0.12" in
  from (Docker.Image.hash base)
  @@ run "opam install dune" (* Not necessarily the dune version used by the project *)
  @@ workdir "src"
  @@ run "opam depext ocamlformat=%s" version
  @@ run "opam install ocamlformat=%s" version
  @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()

let v_fmt ~base ~src =
  let dockerfile =
    let+ base = base in
    ocamlformat_dockerfile ~base in
  let img =
    Docker.build ~label:"OCamlformat" ~pull:false ~dockerfile (`Git src)
  in
  Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]
