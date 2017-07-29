let debug = false;
let debugger_stepping = ref false;

/* Convert a string of characters into a list of tokens. */
let tokenize input: list string => {
  let tokens = CCString.(
    input
      |> (replace sub::"(" by::" ( " )
      |> (replace sub::")" by::" ) ")
      |> (replace sub::"'" by::" ' ")
      |> (replace sub::"\n" by::"")
      |> split by::" "
  );
  /* filter out empty strings */
  List.filter (fun token => token != "") tokens;
};

let print_tokens_list list =>
  print_endline (String.concat " " list);

type env = {
  table: Hashtbl.t string value,
  outer: option env,
} and value =
  | NumberVal float
  | SymbolVal string
  | ListVal (list value)
  | QuotedVal value
  | CallableVal (list string) value env
  | BuiltinCallableVal string (list value => value) env;

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
   | QuotedVal x => "'" ^ (format_val x)
   | NumberVal x => Printf.sprintf "%.12g" x
   | SymbolVal x => x
   | CallableVal args_names body_value _env => {
     let formatted_args = (String.concat " " args_names);
     "(lambda (" ^ formatted_args ^ ") " ^ (format_val body_value) ^ ")";
   }
   | BuiltinCallableVal name _func _env => {
     "#<procedure " ^ name ^ ">"
   }
  }
};

/* Read an expression from a sequence of tokens. */
let rec read_from_tokens = fun (remaining_tokens: ref (list string)) => {
  switch !remaining_tokens {
    | [] => failwith "unexpected EOF while reading"
    | ["'", ...rest] => {
      remaining_tokens := rest;
      QuotedVal (read_from_tokens remaining_tokens);
    }
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
      read_list_from_tokens remaining_tokens [value, ...values_list];
    }
  };
};

/* Read a Scheme expression from a string. */
let parse program => {
  let tokens = ref (tokenize program);
  let value = read_from_tokens (tokens);
  if (!tokens != []) {
    failwith @@ "parsing finished with tokens remaining: " ^ (String.concat " " !tokens);
  };
  value;
};

let unwrap_number_value (value: value) :float => {
  switch value {
    | NumberVal x => x
    | _ => failwith @@ "expected number value, got " ^ (format_val value)
  };
};

let unwrap_symbol_value (value: value) :string => {
  switch value {
    | SymbolVal x => x
    | _ => failwith @@ "expected symbol value, got " ^ (format_val value)
  };
};

let unwrap_list_value (value: value) :list value => {
  switch value {
    | ListVal x => x
    | _ => failwith @@ "expected list value, got " ^ (format_val value)
  };
};

let apply_arithmetic func (args: list value) :value => {
  let numbers = (List.map unwrap_number_value args);
  let result = (List.fold_left func (List.hd numbers) (List.tl numbers));
  NumberVal result;
};

/* constants */
let sym_true = SymbolVal "#t";
let sym_false = SymbolVal "#f";
let pi = NumberVal (acos (-1.0));
let empty_list = ListVal [];

let is_truthy (value: value) => {
  not (switch value {
    | SymbolVal "#f" => true
    | ListVal [] => true
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
    | [a, b] => a === b
    | _ => failwith "expected 2 args"
  });
};

let are_structurally_equal (args: list value) :value => {
  let retval = sym_of_bool (switch args {
    | [NumberVal a, NumberVal b] => a == b
    | [SymbolVal a, SymbolVal b] => a == b
    | [a, b] => a == b
    | _ => failwith "expected 2 args"
  });

  if debug { print_endline @@ "= " ^ (format_val (ListVal args)) ^ " ret " ^ (format_val retval) };
  retval;
};

let list_length (args: list value) => {
  NumberVal (float_of_int (switch args {
    | [l] => List.length (unwrap_list_value l)
    | _ => failwith "expected single argument"
  }));
};

let set_in_env env key value => {
  Hashtbl.add env.table key value;
};

let rec find_env_with_key (env: env) key => {
  switch (Hashtbl.mem env.table key) {
    | true => Some env
    | false =>
      switch (env.outer) {
        | None => None
        | Some outer => find_env_with_key outer key
      }
  };
};

let update_existing_in_env env key value => {
  switch (find_env_with_key env key) {
    | Some found_env => {
      set_in_env found_env key value;
      true;
    }
    | None => false;
  }
};

let get_in_env env key => Hashtbl.find env.table key;

let create_env outer :env => {
  {
    table: Hashtbl.create 100,
    outer: outer,
  };
};

let rec dump_env (maybe_env: option env) => {
  switch maybe_env {
    | None => ()
    | Some env => {
      print_endline "Env {";
      Hashtbl.iter (fun k v => print_endline @@ "  " ^ k ^ ": " ^ (format_val v)) env.table;
      print_endline "}";
      dump_env env.outer;
    }
  };
};

let load_file file => {
  if false {
    let in_channel = open_in file;
    let content_length = in_channel_length in_channel;
    let contents = Bytes.create content_length;
    really_input in_channel contents 0 content_length;
    close_in in_channel;
    Bytes.to_string contents;
  } else {
    ""
  }
};

let rec eval value (env: env) => {
  if (!debugger_stepping) {
    print_endline @@ "eval: " ^ format_val value;
    dump_env (Some env);
    print_string "(debugger paused)";
    read_line () |> ignore;
    print_endline "";
  };

  let evaluated = switch value {
    | QuotedVal value_to_quote => value_to_quote
    | SymbolVal name => {
      switch (find_env_with_key env name) {
        | Some env_with_key => get_in_env env_with_key name
        | None => failwith @@ "attempted to access undefined variable: " ^ name
      }
    };
    | NumberVal _ => value
    | ListVal [] => empty_list
    | ListVal [SymbolVal "if", test, conseq, alt] => {
      if debug { print_endline @@ "if " ^ (format_val test) ^ (format_val conseq) ^ (format_val alt) };
      let cond_evaluated = eval test env;
      if debug { print_endline @@ "if condition evals to " ^ (format_val cond_evaluated)};
      is_truthy (cond_evaluated) ? eval conseq env : eval alt env;
    }
    | ListVal [SymbolVal "if", ...args] => failwith @@ "invalid usage of 'if'"
    | ListVal [SymbolVal "define", SymbolVal name, value] => {
      set_in_env env name (eval value env);
      sym_false;
    }
    | ListVal [SymbolVal "define", ...args] => failwith "invalid usage of 'define'"
    | ListVal [SymbolVal "lambda", ListVal args_names, body_value] => {
      CallableVal (List.map unwrap_symbol_value args_names) body_value (create_env (Some env));
    }
    | ListVal [SymbolVal "lambda", ...args] => failwith "invalid usage of 'lambda'"
    | ListVal [SymbolVal "load", SymbolVal name] => {
      let program = load_file (name ^ ".scm");
      let tokens = ref (tokenize program);
      let last_result = ref sym_false;
      while (!tokens != []) {
        let value = read_from_tokens (tokens);
        last_result := eval value env;
      };
      !last_result;
    }
    | ListVal [SymbolVal "load", ...args] => failwith "invalid usage of 'load'"
    | ListVal [SymbolVal "quote", value_to_quote] => value_to_quote
    | ListVal [SymbolVal "quote", ...args] => failwith "invalid usage of 'quote'"
    | ListVal [SymbolVal "set!", SymbolVal name, value] => {
      switch (update_existing_in_env env name (eval value env)) {
        | true => ()
        | false => {
          failwith "attempted to 'set!' undefined variable"
        }
      };
      sym_false;
    }
    | ListVal [SymbolVal name_to_call, ...args] => {
      let evaluated_args = List.map (fun arg => eval arg env) args;
      call_by_name name_to_call evaluated_args env;
    }
    | ListVal [expr_to_call, ...args] => {
      let evaluated_expr_to_call = eval expr_to_call env;
      let evaluated_args = List.map (fun arg => eval arg env) args;
      call_callable evaluated_expr_to_call "[dynamic]" evaluated_args;
    }
    | _ => failwith "unknown list form"
  };

  if (!debugger_stepping) {
    print_endline @@ "evaluated: " ^ (format_val value) ^ " => " ^ (format_val evaluated);
  };
  evaluated;
} and call_by_name (name: string) (args: list value) (env: env) => {
  if debug { print_endline @@ "call_by_name " ^ name ^ (format_val (ListVal args))};

  switch (find_env_with_key env name) {
    | Some found => call_callable (get_in_env found name) name args;
    | None => {
      if debug { print_endline "dumping env"; dump_env (Some env) };
      failwith @@ "attempted to call undefined function: " ^ name;
    }
  };
} and call_callable (maybe_callable: value) (name: string) (args: list value) :value => {
  if debug { print_endline @@ "call_callable " ^ name ^ (format_val (ListVal args)) };

  let retval = switch maybe_callable {
    | BuiltinCallableVal name func env => func args
    | CallableVal arg_names body env => {
        let fn_env = create_env (Some env);
        try (
          List.iter2 (fun name arg => {
            set_in_env fn_env name arg;
          }) arg_names args
        ) {
          | Invalid_argument "List.iter2" =>  {
            let expected_arity = List.length arg_names;
            let actual_arity = List.length args;

            failwith ("called function " ^ name ^ " with " ^ (string_of_int actual_arity)
             ^ " argument(s), expected " ^ (string_of_int expected_arity) ^ " argument(s)");
          }
        };
        eval body fn_env;
    }
    | _ => failwith @@ "expected callable, got " ^ format_val maybe_callable
  };

  if debug { print_string @@ "returning from " ^ name ^ " with " ^ (format_val retval) ^ "\n" };
  retval;
};

let standard_env () => {
  let env = create_env None;

  set_in_env env "pi" pi;
  set_in_env env "#f" sym_false;
  set_in_env env "#t" sym_true;

  let define_builtin name func =>
    set_in_env env name (BuiltinCallableVal name func env);

  define_builtin "+" @@ apply_arithmetic (+.);
  define_builtin "-" @@ apply_arithmetic (-.);
  define_builtin "*" @@ apply_arithmetic (*.);
  define_builtin "/" @@ apply_arithmetic (/.);
  define_builtin "mod" @@ apply_arithmetic mod_float;
  define_builtin "<" @@ apply_number_comparator (<);
  define_builtin ">" @@ apply_number_comparator (>);
  define_builtin "<=" @@ apply_number_comparator (<=);
  define_builtin ">=" @@ apply_number_comparator (>=);
  define_builtin "=" are_structurally_equal;
  define_builtin "abs" @@ fun args =>
    switch args {
      | [NumberVal x] => NumberVal (abs_float x)
      | _ => failwith "invalid usage of 'abs'"
    };
  define_builtin "apply" @@ fun args =>
    switch args {
      | [maybe_callable, ListVal callable_args] => {
        switch maybe_callable {
          | CallableVal _ => call_callable maybe_callable "[lambda]" callable_args
          | BuiltinCallableVal name _ _ => call_callable maybe_callable name callable_args
          | _ => failwith "cannot use 'apply' with non-callable first argument"
        }
      }
      | _ => failwith "invalid usage of 'apply'"
    };
  define_builtin "begin" @@ fun args => {
    switch (List.rev args) {
      | [] => failwith "invalid usage of 'begin'"
      | [head, ...rest] => head
    };
  };
  define_builtin "car" @@ fun args =>
    switch args {
      | [ListVal [head, ...rest]] => head
      | [ListVal []] => failwith "cannot use car on empty list"
      | _ =>  failwith "cannot use car on non-list value"
    };
  define_builtin "cdr" @@ fun args => ListVal (
    switch args {
      | [ListVal [head, ...rest]] => rest
      | [ListVal []] => failwith "cannot use cadr on empty list"
      | _ =>  failwith "cannot use cadr on non-list value"
    });
  define_builtin "cons" @@ fun args => ListVal (
    switch args {
      | [new_val, ListVal existing_list] => [new_val, ...existing_list]
      | _ =>  failwith "invalid use of 'cons'"
    });
  define_builtin "eq?" are_referentially_equal;
  define_builtin "equal?" are_structurally_equal;
  define_builtin "length" list_length;
  define_builtin "list" @@ fun args => ListVal args;
  define_builtin "list?" @@ fun args => {
    sym_of_bool (switch args {
      | [ListVal list_value] => true
      | [head] => false
      | _ => failwith "invalid use of 'list?'"
    })
  };
  define_builtin "map" @@ fun args =>
    switch args {
      | [maybe_callable, ListVal list_to_map] => {
        ListVal (List.map (fun v => {
          call_callable maybe_callable "[lambda]" [v]
        }) list_to_map);
      }
      | _ => failwith "invalid usage of 'map'"
    };
  define_builtin "max" @@ apply_arithmetic max;
  define_builtin "min" @@ apply_arithmetic min;
  define_builtin "not" @@ fun args =>
    switch args {
      | [operand] => sym_of_bool (not (is_truthy operand))
      | _ => failwith "invalid usage of 'not'"
    };
  define_builtin "null?" @@ fun args =>
    sym_of_bool (switch args {
      | [ListVal []] => true
      | [_] => false
      | _ => failwith "invalid usage of 'null?'"
    });
  define_builtin "number?" @@ fun args =>
    sym_of_bool (switch args {
      | [NumberVal _] => true
      | [_] => false
      | _ => failwith "invalid usage of 'number?'"
    });
  define_builtin "pow" @@ fun args =>
    switch args {
      | [NumberVal base, NumberVal exponent] => NumberVal (base ** exponent);
      | _ => failwith "invalid usage of 'pow'"
    };
  define_builtin "procedure?" @@ fun args =>
    sym_of_bool (switch args {
      | [CallableVal _] => true
      | [BuiltinCallableVal _] => true
      | [_] => false
      | _ => failwith "invalid usage of 'procedure?'"
    });
  define_builtin "reverse" @@ fun args =>
    switch args {
      | [ListVal x] => ListVal (List.rev x)
      | _ => failwith "invalid usage of 'reverse'"
    };
  define_builtin "round" @@ fun args =>
    switch args {
      | [NumberVal x] => NumberVal (floor x)
      | _ => failwith "invalid usage of 'round'"
    };
  define_builtin "symbol?" @@ fun args =>
    sym_of_bool (switch args {
      | [SymbolVal _] => true
      | [_] => false
      | _ => failwith "invalid usage of 'symbol?'"
    });

  env;
};

let load_stdlib env => {
  let stdlib_text = "
  (define filter
    (lambda (fn input)
      (begin
        (define res ())
        (define iter
          (lambda (input1 res1)
            (if input1
              (if (fn (car input1))
                  (iter (cdr input1) (cons (car input1) res1))
                  (iter (cdr input1) res1))
              res1)))
        (reverse (iter input ())))))

  (define foldl
    (lambda (fn acc lst)
      (if lst
        (foldl fn (fn acc (car lst)) (cdr lst))
        acc)))

  (define range
    (lambda (a b)
      (if (= a b)
        ()
        (cons a (range (+ a 1) b)))))

  (define even? (lambda (x) (eq? 0 (round (mod x 2)))))

  (define odd? (lambda (x) (not (even? x))))
  ";

  let tokens = ref (tokenize stdlib_text);
  while (!tokens != []) {
    let value = read_from_tokens (tokens);
    eval value env |> ignore;
  };
};

let read_eval_print program (env: env) => {
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
