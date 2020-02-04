module Lint = Lint
module Docker = Docker

type ('a, 'b) curr = 'a Current.t -> 'b Current.t

(* Left-to-right Arrow composition for Current.t terms *)
let ( >>> ) f g x = g (f x)

let ( >>= ) x f = Current.bind f x

module type PIPELINE = sig
  val v : (Current_git.Commit.t, unit) curr
end

module type RUNNER = sig
  val of_github_app : (Current_github.App.t, unit) curr

  val of_git_local : (Current_git.Local.t, unit) curr
end

module Make_runner (P : PIPELINE) : RUNNER = struct
  let head_commit r = r >>= Current_git.Local.head_commit

  let of_git_local = head_commit >>> P.v

  let of_github_app app =
    let open Current_github in
    let it = Current.list_iter in
    app >>= App.installations
    |> it ~pp:Installation.pp @@ fun installation ->
       Installation.repositories installation
       |> it ~pp:Api.Repo.pp @@ fun repo ->
          Api.Repo.ci_refs repo
          |> it ~pp:Api.Commit.pp @@ fun head ->
             Current_git.fetch (Current.map Api.Commit.id head) |> P.v
end

module type NAME = sig
  val name : string
end

module type S = sig
  val run : unit -> (unit, [ `Msg of string ]) result Cmdliner.Term.result
end

module Make_cli (R : RUNNER) (C : NAME) : S = struct
  let main () config mode
        (src : [ `App of Current_github.App.t | `Path of string ]) =
    let (let*) = Lwt.Infix.(>>=) in
    let pipeline, webhooks =
      match src with
      | `Path path ->
          let repo = Current.return (Current_git.Local.v (Fpath.v path)) in
          (R.of_git_local repo, [])
      | `App app ->
          ( R.of_github_app (Current.return app),
            [ ("github", Current_github.input_webhook) ] )
    in
    let engine = Current.Engine.create ~config (fun () -> pipeline) in
    Logging.run
      (Lwt.choose
         [
           Current.Engine.thread engine;
           let* () = Logs_lwt.app (fun m -> m "Running the web server") in
           Current_web.run ~mode ~webhooks engine;
         ])

  open Cmdliner

  let setup_log =
    let init style_renderer level =
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (Logs_fmt.reporter ())
    in
    Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())

  let repo_path =
    Arg.value
    @@ Arg.pos 0 Arg.dir (Sys.getcwd ())
    @@ Arg.info ~doc:"The directory containing the .git subdirectory."
         ~docv:"DIR" []

  let cmds =
    Term.
      [
        ( const main $ setup_log $ Current.Config.cmdliner $ Current_web.cmdliner
          $ app (pure (fun p -> `Path p)) repo_path,
          info "local" );
        ( const main $ setup_log $ Current.Config.cmdliner $ Current_web.cmdliner
          $ app (pure (fun a -> `App a)) Current_github.App.cmdliner,
          info "app" );
      ]

  let default =
    let doc = "Run the pipeline on a set of projects" in
    let exits = Term.default_exits in
    let man = [] in
    Term.(ret (const (`Help (`Auto, None))), info C.name ~doc ~exits ~man)

  let run () = Term.eval_choice default cmds
end

module Make (C : sig
  val name : string
end)
(P : PIPELINE) =
  Make_cli (Make_runner (P)) (C)
