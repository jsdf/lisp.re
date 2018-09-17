open Lisp;

[@bs.module "bin/browserterm"]
external await_input: (string => string) => unit = "await_input";

[@bs.module "bin/browserterm"]
external write_out: string => unit = "write_out";

let js_read_eval_format = (program, env: env) => {
  let program_value = parse(program);
  let result = eval(program_value, env);
  let result_formatted = format_val(result);
  "=> " ++ result_formatted;
};

let js_read_eval_format_catch = (input, env) =>
  try (js_read_eval_format(String.trim(input), env)) {
  | Failure(message) => "error: " ++ message
  };

let js_read_eval_print_loop = env =>
  await_input(input => js_read_eval_format_catch(input, env));

write_out(
  "
  welcome to lisp.re



    here are some things to try:

    add up some numbers:
      (+ 2 6 8 33 43)

    reverse a list of symbols:
      (reverse (quote (id buy that for a dollar)))

    define a function to square a number:
      (define square (lambda (x) (pow x 2)))
      (square 3)

    calculate the squares of a list of numbers:
      (define square (lambda (x) (pow x 2)))
      (map square (list 1 2 3 4 5))

    calculate the arithmetic mean of a list of numbers:
      (define mean (lambda (lst) (/ (apply + lst) (length lst))))
      (mean (list 3.0 3.1 4.10 5.1 6.1 6.2 10))

    type 'help' to list all available functions and values
  ",
);

let global_env = standard_env();

load_stdlib(global_env);

js_read_eval_print_loop(global_env);