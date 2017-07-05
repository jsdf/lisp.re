open Containers;

/* Convert a string of characters into a list of tokens. */
let tokenize input: list string => {
  let tokens = String.(
    input
      |> (replace sub::"(" by::" ( " )
      |> (replace sub::")" by::" ) ")
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
  if ((List.length !tokens) > 0) {
    failwith ("parsing finished with tokens remaining: " ^ (String.concat " " !tokens));
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

let rec eval value => {
  switch value {
    | SymbolVal _ => value
    | NumberVal _ => value
    | ListVal [SymbolVal sym_to_call, ...args] => {

      let evaluated_args = List.map eval args;
      call_proc sym_to_call evaluated_args;
    }
    | _ => failwith "unknown list form"
  };
};

let read_eval_print program => {
  print_string "=> ";
  parse program
    |> eval
    |> format_val
    |> print_endline;
};

let read_eval_print_loop () => {
  while true {
    print_string "lisp.re> ";
    let input = read_line ();
    read_eval_print (String.trim input);
  }
};

read_eval_print "(+ 1 2 (* 3 4))";
read_eval_print "(+ (* 3 4) 2)";
read_eval_print "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5))";

try (read_eval_print "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5)") {
  | Failure "unexpected EOF while reading list" => ()
  | _ => failwith "expected exception Failure(\"unexpected EOF while reading list\")"
};

/* read_eval_print_loop (); */
