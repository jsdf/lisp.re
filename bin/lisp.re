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
  | ListVal (list value)
  | CallableVal (list string) value;

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
   | CallableVal args_names body_value => {
     let formatted_args = (String.concat " " args_names);
     "(lambda " ^ formatted_args ^ " " ^ (format_val body_value) ^ ")"
   }
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

let unwrap_symbol_value (value: value) :string => {
  switch value {
    | SymbolVal x => x
    | _ => failwith "expected symbol value"
  };
};

let apply_arithmetic func (args: list value) :value => {
  let numbers = (List.map unwrap_number_value args);
  let result = (List.fold_left func (List.hd numbers) (List.tl numbers));
  NumberVal result;
};

let sym_false = SymbolVal "#f";

let is_truthy (value: value) => {
  not (switch value {
    | SymbolVal "#f" => true
    | _  => false
  });
};

let sym_of_bool b =>
  switch b {
    | true => SymbolVal "#t"
    | false  => SymbolVal "#f"
  };

let apply_number_comparator (func: 'a => 'a => bool) (args: list value) :value => {
  switch args {
    | [NumberVal a, NumberVal b] => sym_of_bool (func a b)
    | _ => failwith "expected 2 args of number type"
  };
};

let are_referentially_equal (args: list value) :value => {
  sym_of_bool (switch args {
    | [NumberVal a, NumberVal b] => a == b
    | [SymbolVal a, SymbolVal b] => a == b
    | [CallableVal a_args a_body, CallableVal b_args b_body] => a_args == b_args && a_body == b_body
    | _ => failwith "expected 2 args of same type"
  });
};

let are_structurally_equal (args: list value) :value => {
  sym_of_bool (switch args {
    | [NumberVal a, NumberVal b] => a === b
    | [SymbolVal a, SymbolVal b] => a === b
    | [CallableVal a_args a_body, CallableVal b_args b_body] => a_args === b_args && a_body === b_body
    | _ => failwith "expected 2 args of same type"
  });
};

/* constants */
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
      Hashtbl.add env name (eval value env);
      sym_false;
    }
    | ListVal [SymbolVal "define", ...args] => failwith "invalid usage of 'define'"
    | ListVal [SymbolVal "begin", ...args] => {
      let evaluated_args = List.map (fun arg => eval arg env) args;
      switch (List.rev evaluated_args) {
        | [] => failwith "invalid usage of 'begin'"
        | [head, ...rest] => head
      };
    }
    | ListVal [SymbolVal "lambda", ListVal args_names, body_value] => {
      CallableVal (List.map unwrap_symbol_value args_names) body_value;
    }
    | ListVal [SymbolVal "lambda", ...args] => failwith "invalid usage of 'lambda'"
    | ListVal [SymbolVal sym_to_call, ...args] => {
      let evaluated_args = List.map (fun arg => eval arg env) args;
      call_proc sym_to_call evaluated_args env;
    }
    | _ => failwith "unknown list form"
  };
} and call_proc (name: string) (args: list value) (env: env_table) => {
  switch (name) {
    | "+" => apply_arithmetic (+.) args
    | "-" => apply_arithmetic (-.) args
    | "*" => apply_arithmetic (*.) args
    | "/" => apply_arithmetic (/.) args
    | "<" => apply_number_comparator (<) args
    | ">" => apply_number_comparator (>) args
    | "<=" => apply_number_comparator (<=) args
    | ">=" => apply_number_comparator (>=) args
    | "=" => are_structurally_equal args
    | "eq?" => are_referentially_equal args
    | "equal?" => are_structurally_equal args
    | name => {
        let proc = try (Hashtbl.find env name) {
          | Not_found => failwith @@ "attempted to call undefined function: " ^ name
        };
        let (arg_names, body) = switch proc {
          | CallableVal names body => (names, body)
          | _ => failwith @@ "expected callable, got " ^ format_val proc
        };
        let fn_env = Hashtbl.copy env;
        List.iter2 (fun name arg => {
          Hashtbl.add fn_env name arg;
        }) arg_names args;

        eval body fn_env;
    }
    /* | _ => failwith "unknown proc" */
  };
};

let read_eval_print program env => {
  let program_value = parse program;
  let result = eval program_value env;
  let result_formatted = format_val result;
  print_endline ("=> " ^ result_formatted);
};

let read_eval_print_loop env => {
  while true {
    print_string "lisp.re> ";
    let input = read_line ();
    try (
      read_eval_print (String.trim input) env
    ) {
      | Failure message => print_endline ("error: " ^ message);
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

print_endline "test lambda";
let lambda_test = "(begin
    (define  sq (lambda (x) (* x x)))
    (sq 2))";
read_eval_print lambda_test test_env;

print_endline "test invalid expression";
try (read_eval_print "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5)" test_env) {
  | Failure "unexpected EOF while reading list" => print_endline "ok";
  | _ => failwith "expected exception Failure(\"unexpected EOF while reading list\")"
};

print_endline "start repl";
let global_env = standard_env ();
read_eval_print_loop global_env;
