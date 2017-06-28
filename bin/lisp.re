open Containers;

let rec print_list_string list =>
  switch list {
  | [] => print_char '\n'
  | [head, ...body] =>
    print_string ("'" ^ head ^ "' ");
    print_list_string body
  };

/* Convert a string of characters into a list of tokens. */
let tokenize input: list string =>
  String.(
    input
      |> (replace sub::"(" by::" ( " )
      |> (replace sub::")" by::" ) ")
      |> split by::" "
  );

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

/* Read an expression from a sequence of tokens. */
let rec read_from_tokens = fun tokens :value => {
  switch tokens {
  | [] => {
    print_endline "unexpected EOF while reading";
    exit 1;
  }
  | [head, ...body] =>
    print_endline @@ "got token " ^ head;
    switch head {
      | "" => read_from_tokens body
      | "(" => {
        print_endline "entering list";
        ListVal (read_list_from_tokens body ([]: list value));
      };
      | ")" => {
        print_endline "unexpected )";
        exit 1;
      }
      | _ => atom head
    }
  }
} and read_list_from_tokens = fun tokens into :list value => {
  switch tokens {
  | [] => {
    print_endline "unexpected EOF while reading";
    exit 1;
  }
  | [head, ...body] =>
    switch head {
      | ")" => {
        List.rev into
      }
      | _ => {
        read_list_from_tokens body (List.cons (read_from_tokens tokens) into)
      }
    }
  }
};

/* Read a Scheme expression from a string. */
let parse program =>
  read_from_tokens @@ tokenize program;

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

let program = "(+ 1 2)";

parse program
  |> format_val
  |> print_endline;
