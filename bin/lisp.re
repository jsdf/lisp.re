open Containers;

/* Convert a string of characters into a list of tokens. */
let tokenize input: list string => {
  let tokens = String.(
    input
      |> (replace sub::"(" by::" ( " )
      |> (replace sub::")" by::" ) ")
      |> (replace sub::"\n" by::"")
      |> split by::" "
  );
  /* filter out empty strings */
  List.filter (fun token => token != "") tokens;
};

let print_tokens_list list =>
  print_endline (String.concat " " list);

type value =
  | NumberVal float
  | SymbolVal string
  | ListVal (list value);

/* Numbers become numbers; every other token is a symbol. */
let atom token: value => {
  try (NumberVal (float_of_string token)) {
    | Failure "float_of_string" => {
      SymbolVal token
    };
  };
};

let rec format_val = fun value: string => {
  switch (value) {
   | ListVal x => {
     let formatted_items = List.map format_val x;
     let joined = (String.concat " " formatted_items);
     "(" ^ joined ^ ")";
   }
   | NumberVal x => Printf.sprintf "%.12g" x
   | SymbolVal x => x
  }
};

/* Read an expression from a sequence of tokens. */
let rec read_from_tokens = fun (remaining_tokens: ref (list string)) => {
  switch !remaining_tokens {
    | [] => failwith "unexpected EOF while reading"
    | ["(", ...rest] => {
      remaining_tokens := rest;
      let values_list: list value = [];
      read_list_from_tokens remaining_tokens values_list;
    }
    | [")", ...rest] => failwith "unexpected )"
    | [token, ...rest] => {
      remaining_tokens := rest;
      atom token
    }
  };
} and read_list_from_tokens = fun remaining_tokens values_list => {
    switch !remaining_tokens {
      | [] => failwith "unexpected EOF while reading list"
      | [")", ...rest] => {
        remaining_tokens := rest;
        /* we need to reverse values_list because we built the list by using cons to
        push values on to the head of the list, meaning that it's 'backwards'
        (the last value is at the head) */
        ListVal (List.rev values_list);
      }
      | _ => {
        let value = read_from_tokens remaining_tokens;
        let next_values_list = (List.cons value values_list);
        read_list_from_tokens remaining_tokens next_values_list;
      }
    };
};

/* Read a Scheme expression from a string. */
let parse program => {
  let tokens = ref (tokenize program);
  let value = read_from_tokens (tokens);
  if (!tokens != []) {
    print_endline @@ "parsing finished with tokens remaining: " ^ (String.concat " " !tokens);
    failwith "parsing finished with tokens remaining";
  };
  value;
};

let unwrap_number_value (value: value) :float => {
  switch value {
    | NumberVal x => x
    | _ => failwith "expected number value"
  };
};

let apply_arithmetic func (args: list value) :value => {
  let numbers = (List.map unwrap_number_value args);
  let result = (List.fold_left func (List.hd numbers) (List.tl numbers));
  NumberVal result;
};

let call_proc (name: string) (args: list value) => {
  switch (name) {
    | "+" => apply_arithmetic (+.) args
    | "-" => apply_arithmetic (-.) args
    | "*" => apply_arithmetic (*.) args
    | "/" => apply_arithmetic (/.) args
    | _ => failwith "unknown proc"
  };
};

let pi = acos (-1.0);

type env_table = Hashtbl.t string value;
let standard_env () => {
  let env: env_table = Hashtbl.create 1000;
  Hashtbl.add env "pi" (NumberVal pi);
  env;
};

let rec eval value env => {
  switch value {
    | SymbolVal name => {
      try (Hashtbl.find env name) {
        | Not_found => failwith @@ "attempted to access undefined variable: " ^ name
      }
    };
    | NumberVal _ => value
    | ListVal [SymbolVal "define", SymbolVal name, value] => {
      Hashtbl.add env name value;
      SymbolVal "#f";
    }
    | ListVal [SymbolVal "define", ...args] => failwith "invalid usage of 'define'"
    | ListVal [SymbolVal "begin", ...args] => {
      let evaluated_args = List.map (fun arg => eval arg env) args;
      switch (List.rev evaluated_args) {
        | [] => SymbolVal "#f"
        | [head, ...rest] => head
      };
    }
    | ListVal [SymbolVal sym_to_call, ...args] => {
      let evaluated_args = List.map (fun arg => eval arg env) args;
      call_proc sym_to_call evaluated_args;
    }
    | _ => failwith "unknown list form"
  };
};

let read_eval_print program env => {
  let result = parse program
    |> (fun program_value => eval program_value env);

  print_string "=> ";
  result
    |> format_val
    |> print_endline;
};

let read_eval_print_loop env => {
  while true {
    print_string "lisp.re> ";
    let input = read_line ();
    try (
      read_eval_print (String.trim input) env
    ) {
      | Failure message => print_endline @@ "Error: " ^ message;
    };
  }
};

let test_env = standard_env ();

print_endline "test stuff";
read_eval_print "(+ 1 2 (* 3 4))" test_env;
read_eval_print "(+ (* 3 4) 2)" test_env;
read_eval_print "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5))" test_env;

print_endline "test begin";
let begin_test = "(begin
    (define r 10)
    (* pi (* r r)))";
read_eval_print begin_test test_env;

print_endline "test invalid expression";
try (read_eval_print "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5)" test_env) {
  | Failure "unexpected EOF while reading list" => print_endline "ok";
  | _ => failwith "expected exception Failure(\"unexpected EOF while reading list\")"
};

print_endline "start repl";
let global_env = standard_env ();
read_eval_print_loop global_env;
