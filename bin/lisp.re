open Containers;

/* Convert a string of characters into a list of tokens. */
let tokenize input: list string =>
  String.(
    input
      |> (replace sub::"(" by::" ( " )
      |> (replace sub::")" by::" ) ")
      |> split_on_char ' '
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
              | Failure "int_of_string" => {
                SymbolVal token
              };
          };
    };
  };
};

/*
def read_from_tokens(tokens):
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return atom(token)
*/

/* Read an expression from a sequence of tokens. */
let rec read_from_tokens tokens: value => {
  if ((List.length tokens) == 0) {
    print_endline "unexpected EOF while reading";
    exit 1;
  };
  let token = List.hd tokens;
  switch token {
    | "(" => {
      let list = ref ([]: list value);

      while (List.hd tokens != ")") {
        list := (List.cons (read_from_tokens tokens) !list);
      };
      ListVal (List.rev !list);
    };
    | ")" => {
      print_endline "unexpected )";
      exit 1;
    }
    | _ => atom token
  };
};
/* Read a Scheme expression from a string. */
let parse program =>
  read_from_tokens @@ tokenize program;

let program = "(+ 1 2)";
print_list_string (tokenize program);
