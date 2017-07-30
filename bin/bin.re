open Lisp;

let run_tests = false;
let run_benchmarks = true;

let print_time_taken fn => {
  let t = Sys.time ();
  fn ();
  Printf.printf "took: %fs\n" (Sys.time () -. t);
};

let benchmark fn => {
  print_time_taken @@ fun () => {
    for _ in 0 to 10000 {
      fn ();
    };
  };
};

let tokenize_test_input = "
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

  (+ 1 2 (* 3 4))
  (+ (* 3 4) 2)
  (+ 1 2 (* 3 4) (- 5 6) (/ 10 5))
  (define circle-area (lambda (r) (* pi (* r r))))
  (circle-area 3)
  (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
  (fact 10)
  (fact 100)
  (circle-area (fact 10))
  (procedure? map)
  (define first car)
  (define rest cdr)
  (define count (lambda (item L) (if L (+ (if (equal? item (first L)) 1 0) (count item (rest L))) 0)))
  (count 0 (list 0 1 2 3 0 0))
  (count (quote the) '(the more the merrier the bigger the better))
  (define twice (lambda (x) (* 2 x)))
  (twice 5)
  (define repeat (lambda (f) (lambda (x) (f (f x)))))
  ((repeat twice) 10)
  ((repeat (repeat twice)) 10)
  ((repeat (repeat (repeat twice))) 10)
  ((repeat (repeat (repeat (repeat twice)))) 10)
  (pow 2 16)
  (define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
  (define range (lambda (a b) (if (= a b) () (cons a (range (+ a 1) b)))))
  (range 0 10)
  (map fib (range 0 10))
  (map fib (range 0 20))
  ";

if run_benchmarks {
  print_endline "tokenizer benchmarks";

  print_endline @@ "tokenize3";
  benchmark @@ fun () => {
    tokenize3 tokenize_test_input;
  };
  print_endline @@ "tokenize2";
  benchmark @@ fun () => {
    tokenize2 tokenize_test_input;
  };
  print_endline @@ "tokenize";
  benchmark @@ fun () => {
    tokenize tokenize_test_input;
  };

  print_endline @@ "tokenize";
  benchmark @@ fun () => {
    tokenize tokenize_test_input;
  };
  print_endline @@ "tokenize2";
  benchmark @@ fun () => {
    tokenize2 tokenize_test_input;
  };
  print_endline @@ "tokenize3";
  benchmark @@ fun () => {
    tokenize3 tokenize_test_input;
  };
};

if run_tests {
  let test_env = standard_env ();

  print_endline "test stuff";
  let test_expr expr => {
    print_time_taken @@ fun () => {
      print_endline @@ "test> " ^ expr;
      read_eval_print expr test_env;
    };
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
  test_expr "(count (quote the) '(the more the merrier the bigger the better))";
  test_expr "(define twice (lambda (x) (* 2 x)))";
  test_expr "(twice 5)";
  test_expr "(define repeat (lambda (f) (lambda (x) (f (f x)))))";
  test_expr "((repeat twice) 10)";
  test_expr "((repeat (repeat twice)) 10)";
  test_expr "((repeat (repeat (repeat twice))) 10)";
  test_expr "((repeat (repeat (repeat (repeat twice)))) 10)";
  test_expr "(pow 2 16)";
  test_expr "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))";
  test_expr "(define range (lambda (a b) (if (= a b) () (cons a (range (+ a 1) b)))))";
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
    | Failure "unexpected EOF while reading list" => print_endline "ok";
    | _ => failwith "expected exception Failure(\"unexpected EOF while reading list\")"
  };
};

print_endline "start repl";
let global_env = standard_env ();
load_stdlib global_env;
read_eval_print_loop global_env;
