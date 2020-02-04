module Docker = Current_ci.Docker

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

module Pipeline = struct
  let v src =
    let lint_base_image =
      Docker.pull ~schedule:weekly "ocurrent/opam:alpine-3.10-ocaml-4.08"
    in
    let ocamlformat_version = Current_ci.Lint.get_ocamlformat_source in
    Current_ci.Lint.v_fmt ~ocamlformat_version ~base:lint_base_image ~src |> Current.ignore_value
end
