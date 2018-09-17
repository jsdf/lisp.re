open Lisp;

let run_tests = true;

let run_benchmarks = false;

let print_time_taken = (fn) => {
  let t = Sys.time();
  fn();
  Printf.printf("took: %fs\n", Sys.time() -. t)
};

let benchmark = (fn) =>
  print_time_taken @@
  (
    () =>
      for (_ in 0 to 10000) {
        fn()
      }
  );

let tokenize_test_input = "\n  (define filter\n    (lambda (fn input)\n      (begin\n        (define res ())\n        (define iter\n          (lambda (input1 res1)\n            (if input1\n              (if (fn (car input1))\n                  (iter (cdr input1) (cons (car input1) res1))\n                  (iter (cdr input1) res1))\n              res1)))\n        (reverse (iter input ())))))\n\n  (define foldl\n    (lambda (fn acc lst)\n      (if lst\n        (foldl fn (fn acc (car lst)) (cdr lst))\n        acc)))\n\n  (define range\n    (lambda (a b)\n      (if (= a b)\n        ()\n        (cons a (range (+ a 1) b)))))\n\n  (define even? (lambda (x) (eq? 0 (round (mod x 2)))))\n\n  (define odd? (lambda (x) (not (even? x))))\n\n  (+ 1 2 (* 3 4))\n  (+ (* 3 4) 2)\n  (+ 1 2 (* 3 4) (- 5 6) (/ 10 5))\n  (define circle-area (lambda (r) (* pi (* r r))))\n  (circle-area 3)\n  (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))\n  (fact 10)\n  (fact 100)\n  (circle-area (fact 10))\n  (procedure? map)\n  (define first car)\n  (define rest cdr)\n  (define count (lambda (item L) (if L (+ (if (equal? item (first L)) 1 0) (count item (rest L))) 0)))\n  (count 0 (list 0 1 2 3 0 0))\n  (count (quote the) '(the more the merrier the bigger the better))\n  (define twice (lambda (x) (* 2 x)))\n  (twice 5)\n  (define repeat (lambda (f) (lambda (x) (f (f x)))))\n  ((repeat twice) 10)\n  ((repeat (repeat twice)) 10)\n  ((repeat (repeat (repeat twice))) 10)\n  ((repeat (repeat (repeat (repeat twice)))) 10)\n  (pow 2 16)\n  (define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))\n  (define range (lambda (a b) (if (= a b) () (cons a (range (+ a 1) b)))))\n  (range 0 10)\n  (map fib (range 0 10))\n  (map fib (range 0 20))\n  ";

if (run_benchmarks) {
  print_endline("tokenizer benchmarks");
  print_endline @@ "tokenize";
  benchmark @@ (() => tokenize(tokenize_test_input))
};

if (run_tests) {
  let test_env = standard_env();
  print_endline("test stuff");
  let test_expr = (expr) =>
    print_time_taken @@
    (
      () => {
        print_endline @@ "test> " ++ expr;
        read_eval_print(expr, test_env)
      }
    );
  test_expr("(+ 1 2 (* 3 4))");
  test_expr("(+ (* 3 4) 2)");
  test_expr("(+ 1 2 (* 3 4) (- 5 6) (/ 10 5))");
  test_expr("(define circle-area (lambda (r) (* pi (* r r))))");
  test_expr("(circle-area 3)");
  test_expr("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))");
  test_expr("(fact 10)");
  test_expr("(fact 100)");
  test_expr("(circle-area (fact 10))");
  test_expr("(procedure? map)");
  test_expr("(define first car)");
  test_expr("(define rest cdr)");
  test_expr(
    "(define count (lambda (item L) (if L (+ (if (equal? item (first L)) 1 0) (count item (rest L))) 0)))"
  );
  test_expr("(count 0 (list 0 1 2 3 0 0))");
  test_expr("(count (quote the) '(the more the merrier the bigger the better))");
  test_expr("(define twice (lambda (x) (* 2 x)))");
  test_expr("(twice 5)");
  test_expr("(define repeat (lambda (f) (lambda (x) (f (f x)))))");
  test_expr("((repeat twice) 10)");
  test_expr("((repeat (repeat twice)) 10)");
  test_expr("((repeat (repeat (repeat twice))) 10)");
  test_expr("((repeat (repeat (repeat (repeat twice)))) 10)");
  test_expr("(pow 2 16)");
  test_expr("(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))");
  test_expr("(define range (lambda (a b) (if (= a b) () (cons a (range (+ a 1) b)))))");
  test_expr("(range 0 10)");
  test_expr("(map fib (range 0 10))");
  test_expr("(map fib (range 0 20))");
  test_expr("(begin\n      (define r 10)\n      (* pi (* r r)))");
  test_expr("(begin\n      (define  sq (lambda (x) (* x x)))\n      (sq 2))");
  try (test_expr("(+ 1 2 (* 3 4) (- 5 6) (/ 10 5)")) {
  | Failure("unexpected EOF while reading list") => print_endline("ok")
  | _ => failwith("expected exception Failure(\"unexpected EOF while reading list\")")
  }
};

print_endline("start repl");

let global_env = standard_env();

load_stdlib(global_env);

read_eval_print_loop(global_env);
