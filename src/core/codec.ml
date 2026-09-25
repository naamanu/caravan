type 'a t = {
  encode : 'a -> Yojson.Safe.t;
  decode : Yojson.Safe.t -> ('a, string) result;
}

let make ~encode ~decode = { encode; decode }
let of_yojson encode decode = { encode; decode }

let unit =
  {
    encode = (fun () -> `Null);
    decode =
      (function
      | `Null | `Assoc [] -> Ok ()
      | _ -> Error "expected null for unit args");
  }

let string =
  {
    encode = (fun s -> `String s);
    decode = (function `String s -> Ok s | _ -> Error "expected a string");
  }

let int =
  {
    encode = (fun i -> `Int i);
    decode = (function `Int i -> Ok i | _ -> Error "expected an int");
  }

let json = { encode = Fun.id; decode = Result.ok }
