open Lisp;

[@bs.module "bin/browserterm"] external await_input : (string => string) => unit = "await_input";

[@bs.module "bin/browserterm"] external write_out : string => unit = "write_out";

let js_read_eval_format = (program, env: env) => {
  let program_value = parse(program);
  let result = eval(program_value, env);
  let result_formatted = format_val(result);
  "=> " ++ result_formatted
};

let js_read_eval_format_catch = (input, env) =>
  try (js_read_eval_format(String.trim(input), env)) {
  | Failure(message) => "error: " ++ message
  };

let js_read_eval_print_loop = (env) =>
  await_input((input) => js_read_eval_format_catch(input, env));

write_out(
  "\nwelcome to lisp.re\n\n\n\n  here are some things to try:\n\n  add up some numbers:\n    (+ 2 6 8 33 43)\n\n  reverse a list of symbols:\n    (reverse (quote (id buy that for a dollar)))\n\n  define a function to square a number:\n    (define square (lambda (x) (pow x 2)))\n    (square 3)\n\n  calculate the squares of a list of numbers:\n    (define square (lambda (x) (pow x 2)))\n    (map square (list 1 2 3 4 5))\n\n  calculate the arithmetic mean of a list of numbers:\n    (define mean (lambda (lst) (/ (apply + lst) (length lst))))\n    (mean (list 3.0 3.1 4.10 5.1 6.1 6.2 10))\n\n"
);

let global_env = standard_env();

load_stdlib(global_env);

js_read_eval_print_loop(global_env);
