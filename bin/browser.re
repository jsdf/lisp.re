open Lisp;

external await_input : (string => string) => unit = "await_input" [@@bs.module "bin/browserterm"];
external write_out : string => unit = "write_out" [@@bs.module "bin/browserterm"];

let js_read_eval_format program (env: env) => {
  let program_value = parse program;
  let result = eval program_value env;
  let result_formatted = format_val result;
  "=> " ^ result_formatted;
};

let js_read_eval_format_catch input env => {
    try (
      js_read_eval_format (String.trim input) env
    ) {
      | Failure message => "error: " ^ message;
    };
};

let js_read_eval_print_loop env => {
  await_input (fun input => {
    js_read_eval_format_catch input env;
  });
};

let run_tests = true;

if run_tests {
  let test_env = standard_env ();

  write_out "test stuff";
  let test_expr expr => {
    let t = Sys.time ();
    write_out @@ "test> " ^ expr;
    write_out @@ js_read_eval_format expr test_env;
    write_out @@ Printf.sprintf "took: %fs\n" (Sys.time () -. t);
  };

  test_expr "(+ 1 2 (* 3 4))";
  test_expr "(+ (* 3 4) 2)";
  test_expr "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5))";

  test_expr "(define circle-area (lambda (r) (* pi (* r r))))";
  test_expr "(circle-area 3)";
  test_expr "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))";
  test_expr "(fact 10)";
  test_expr "(fact 100)";
  test_expr "(circle-area (fact 10))";
  test_expr "(procedure? map)";
  test_expr "(define first car)";
  test_expr "(define rest cdr)";
  test_expr "(define count (lambda (item L) (if L (+ (if (equal? item (first L)) 1 0) (count item (rest L))) 0)))";
  test_expr "(count 0 (list 0 1 2 3 0 0))";
  test_expr "(count (quote the) (quote (the more the merrier the bigger the better)))";
  test_expr "(define twice (lambda (x) (* 2 x)))";
  test_expr "(twice 5)";
  test_expr "(define repeat (lambda (f) (lambda (x) (f (f x)))))";
  test_expr "((repeat twice) 10)";
  test_expr "((repeat (repeat twice)) 10)";
  test_expr "((repeat (repeat (repeat twice))) 10)";
  test_expr "((repeat (repeat (repeat (repeat twice)))) 10)";
  test_expr "(pow 2 16)";
  test_expr "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))";
  test_expr "(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))";
  test_expr "(range 0 10)";
  test_expr "(map fib (range 0 10))";
  test_expr "(map fib (range 0 20))";

  test_expr "(begin
      (define r 10)
      (* pi (* r r)))";

  test_expr "(begin
      (define  sq (lambda (x) (* x x)))
      (sq 2))";

  try (test_expr "(+ 1 2 (* 3 4) (- 5 6) (/ 10 5)") {
    | Failure "unexpected EOF while reading list" => write_out "ok";
    | _ => failwith "expected exception Failure(\"unexpected EOF while reading list\")"
  };
};

write_out "start repl";
let global_env = standard_env ();
load_stdlib global_env;
js_read_eval_print_loop global_env;
