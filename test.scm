(define-syntax test-error-message
  (syntax-rules ()
    ((_ message form ...)
     (test-assert
      (guard
       (ex ((parse-error-object? ex)
            (equal? (parse-error-message ex) message))
           ((error-object? ex)
            (equal? (error-object-message ex) message)))
       (begin
         form ...
         #f))))))

(test-begin "sequences")

(test-error (parse "" #f))
(test-error-message "Empty input" (parse "" #f))
(test-error (parse "xxx" #f))
(test-error-message "Expected item" (parse "xxx" #f))
(test-equal '(((note (key . #\c))
               (note (key . #\d))
               (note (key . #\e))))
            (parse "c d e" #f))
(test-error (parse "c d e xxx" #f))
(test-error-message "Expected item" (parse "c d e xxx" #f))
(test-error (parse "c d exxx" #f))
(test-error-message "Trailing garbage" (parse "c d exxx" #f))

(test-equal '(((note (key . #\c)
                     (duration . 4)
                     (shift . 1)
                     (octave-shift . 3)
                     (dotted . 2)
                     (natural . #t))))
            (parse "c4+>>>.._" #f))
(test-equal '(((note (key . #\c)
                     (duration 1 1))))
            (parse "c1~1" #f))
(test-error (parse "c1~1~" #f))
(test-error-message "Expected duration" (parse "c1~1~" #f))

(test-equal '(((chord (note (key . #\c))
                      (note (key . #\e))
                      (note (key . #\g)))))
            (parse "c/e/g" #f))
(test-equal '(((chord (note (key . #\c))
                      (note (key . #\e)
                            (shift . -1))
                      (note (key . #\g)))))
            (parse "c/e-/g" #f))
(test-error (parse "c/e/" #f))
(test-error-message "Expected note" (parse "c/e/" #f))

(test-equal '(((rest . 4) (rest . 2) (rest . 1)))
            (parse "r4 r2 r1" #f))
(test-equal '(((rest 1 1))) (parse "r1~1" #f))
(test-equal '(((rest))) (parse "r" #f))
(test-error (parse "rx" #f))
(test-error-message "Invalid duration" (parse "rx" #f))
(test-error (parse "r3" #f))
(test-error-message "Invalid duration" (parse "r3" #f))

(test-equal '(((octave . 2) (octave . 3) (octave . 4)))
            (parse "o2 o3 o4" #f))
(test-error (parse "ox" #f))
(test-error-message "Invalid octave" (parse "ox" #f))

(test-equal '(((octave-shift . 1))) (parse ">" #f))
(test-equal '(((octave-shift . -1))) (parse "<" #f))
(test-error (parse "><>" #f))
(test-error-message "Trailing garbage" (parse "><>" #f))

(test-equal '(((sexp . (volume 80))
               (sexp . (tempo 120))
               (sexp . (instrument 5))))
            (parse "(volume 80) (tempo 120) (instrument 5)" #f))

(test-equal '(((chord
                (note (key . #\a))
                (note (key . #\c))
                (note (key . #\e)))))
            (parse "a/c/e # ignore this" #f))

(test-equal '(((octave . 3)
               (note (key . #\c)
                     (duration . 4))
               (note (key . #\d))
               (note (key . #\e))
               (note (key . #\f))

               (note (key . #\g)
                     (duration . 2))
               (note (key . #\g))

               (note (key . #\a)
                     (duration . 4))
               (note (key . #\a))
               (note (key . #\a))
               (note (key . #\a))

               (note (key . #\g)
                     (duration . 2))
               (rest)

               (note (key . #\a)
                     (duration . 4))
               (note (key . #\a))
               (note (key . #\a))
               (note (key . #\a))

               (note (key . #\g)
                     (duration . 2))
               (rest)

               (note (key . #\f)
                     (duration . 4))
               (note (key . #\f))
               (note (key . #\f))
               (note (key . #\f))

               (note (key . #\e)
                     (duration . 2))
               (note (key . #\e))

               (note (key . #\d)
                     (duration . 4))
               (note (key . #\d))
               (note (key . #\d))
               (note (key . #\d))

               (note (key . #\c)
                     (duration . 1))))
            (parse "o3 c4 d e f | g2 g |
                             a4 a a a | g2 r |
                             a4 a a a | g2 r |
                             f4 f f f | e2 e |
                             d4 d d d | c1" #f))

(test-end "sequences")
