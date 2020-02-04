module Lint : module type of Lint
module Docker : module type of Docker

type ('a, 'b) curr = 'a Current.t -> 'b Current.t

module type NAME = sig
  val name : string
end

module type PIPELINE = sig
  val v : (Current_git.Commit.t, unit) curr
end

module Make (C : NAME) (P : PIPELINE) : sig
  val run : unit -> (unit, [ `Msg of string ]) result Cmdliner.Term.result
end
