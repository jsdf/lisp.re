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
      let list_values = ref ([]: list value);
      while (List.hd !remaining_tokens != ")") {
        list_values := (List.cons (read_from_tokens remaining_tokens) !list_values);
      };
      /* we need to reverse list_values because we built the list by using cons to
      push values on to the head of the list, meaning that it's 'backwards'
      (the head is at the end) */
      ListVal (List.rev !list_values);
    }
    | [")", ...rest] => failwith "unexpected )"
    | [token, ...rest] => {
      remaining_tokens := rest;
      atom token
    }
  };
};

/* Read a Scheme expression from a string. */
let parse program => {
  let tokens = tokenize program;
  read_from_tokens (ref tokens);
};

let read_eval_print program => {
  print_string "=> ";
  parse program
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

read_eval_print_loop ();
