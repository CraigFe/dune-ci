module Cli =
  Current_ci.Make
    (struct
      let name = "dune-ci"
    end)
    (Dune_ci.Pipeline)

let () = Cmdliner.Term.exit (Cli.run ())
