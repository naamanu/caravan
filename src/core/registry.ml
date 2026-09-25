type t = (string, Job.packed) Hashtbl.t

let create jobs =
  let t = Hashtbl.create 16 in
  List.iter
    (fun p ->
      let name = Job.packed_name p in
      if Hashtbl.mem t name then
        invalid_arg ("Registry.create: duplicate job name " ^ name);
      Hashtbl.replace t name p)
    jobs;
  t

let names t = Hashtbl.to_seq_keys t |> List.of_seq |> List.sort String.compare

type resolved = {
  run : Ctx.t -> Outcome.t;
  timeout : float option;
  backoff : Backoff.t;
}

let resolve t (row : Row.t) =
  match Hashtbl.find_opt t row.worker with
  | None ->
      Error
        (Outcome.Error
           (Printf.sprintf "no job named %S is registered on this node"
              row.worker))
  | Some (Job.Pack job) -> (
      match (Job.codec job).decode row.args with
      | Error e ->
          Error (Outcome.Discard ("could not decode job arguments: " ^ e))
      | exception e ->
          Error
            (Outcome.Discard
               ("could not decode job arguments: " ^ Printexc.to_string e))
      | Ok args ->
          Ok
            {
              run = (fun ctx -> Job.perform job ctx args);
              timeout = Job.timeout job;
              backoff = Job.backoff job;
            })
