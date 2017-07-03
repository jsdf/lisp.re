open Containers;

/* Convert a string of characters into a list of tokens. */
let tokenize input: list string =>
  String.(
    input
      |> (replace sub::"(" by::" ( " )
      |> (replace sub::")" by::" ) ")
      |> split by::" "
      |> List.filter (fun token => token != "")
  );

let rec print_list_string list =>
  switch list {
  | [] => ()
  | [head, ...body] =>
    print_string (head ^ " ");
    print_list_string body
  };

type value =
  | IntVal int
  | FloatVal float
  | SymbolVal string
  | ListVal (list value);

/* Numbers become numbers; every other token is a symbol. */
let atom token: value => {
  try (IntVal (int_of_string token)) {
    | Failure "int_of_string" => {
      try (FloatVal (float_of_string token)) {
        | Failure "float_of_string" => {
          SymbolVal token
        };
      };
    };
  };
};

let rec format_val = fun value: string => {
switch (value) {
 | ListVal x => format_list x
 | IntVal x => string_of_int x
 | FloatVal x => string_of_float x
 | SymbolVal x => x
}
} and format_list = fun list: string => {
let formatted_items = List.map format_val list;
  "(" ^ String.concat " " formatted_items ^ ")"
};

/* Read an expression from a sequence of tokens. */
let rec read_from_tokens = fun (remaining_tokens: ref (list string)) => {
  switch !remaining_tokens {
  | [] => failwith "unexpected EOF while reading"
  | [current_token, ...rest] =>
    remaining_tokens := rest;
    switch current_token {
      | "(" => {
        let list_read = ListVal (read_list_from_tokens remaining_tokens ([]: list value));
        list_read;
      };
      | ")" => failwith "unexpected )"
      | _ => atom current_token
    }
  }
} and read_list_from_tokens = fun remaining_tokens values_list => {
  switch !remaining_tokens {
    | [] => failwith "unexpected EOF while reading list"
    | [")", ...rest] => {
      /* got to the end of the list we are currently reading from the tokens */
      remaining_tokens := rest;
      List.rev values_list /* reverse our values list so the first item is at the head */
    }
    | _ => {
      /* push next value onto head of list (meaning the list head is the last item and goes in reverse
      order) */
      let updated_values_list = (List.cons (read_from_tokens remaining_tokens) values_list);
      read_list_from_tokens remaining_tokens updated_values_list;
    }
  };
};

/* Read a Scheme expression from a string. */
let parse program =>
  program
   |> tokenize
   |> ref
   |> read_from_tokens;

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

/* read_eval_print_loop (); */
