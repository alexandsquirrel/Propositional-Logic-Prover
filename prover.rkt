#lang racket

; Preliminaries

; Atoms are defined using symbols.

; Basic logical connectors:

(struct Not (x) #:transparent)

(struct And (x y) #:transparent)

(struct Or (x y) #:transparent)

; Derived logical connectors:

(define-syntax Implies
  (syntax-rules ()
    [(Implies x y)
     (Or (negate x) y)]))

(define-syntax Iff
  (syntax-rules ()
    [(Iff x y)
     (And (Implies x y) (Implies y x))]))

; Helper function.

(define (negate formula)
    (if (Not? formula)
        (Not-x formula)
        (Not formula)))

; Actual prover:

(define (disprove formula)
  (define (solve knowledge stack)
    (if (null? stack)
        (begin (printf "Open clause.\n")
               (print-literals knowledge)
               #f)
        (letrec ([clause (car stack)]
                 [pending (cdr stack)])
          (begin (pretty-print clause)
                 (printf "\n")
          (cond
            [(And? clause)
             (let ([sub-x (And-x clause)]
                   [sub-y (And-y clause)])
               (solve (cons sub-x (cons sub-y knowledge)) (cons sub-x (cons sub-y pending))))]
            [(Or? clause)
             (let ([sub-x (Or-x clause)]
                   [sub-y (Or-y clause)])
               (and (solve (cons sub-x knowledge) (cons sub-x pending))
                    (solve (cons sub-y knowledge) (cons sub-y pending))))]
            [(Not? clause)
             (let ([inside (Not-x clause)])
               (cond
                 [(And? inside)
                  (let ([sub-x (negate (And-x inside))]
                        [sub-y (negate (And-y inside))])
                    (and (solve (cons sub-x knowledge) (cons sub-x pending))
                         (solve (cons sub-y knowledge) (cons sub-y pending))))]
                 [(Or? inside)
                  (let ([sub-x (negate (Or-x inside))]
                        [sub-y (negate (Or-y inside))])
                    (solve (cons sub-x (cons sub-y knowledge)) (cons sub-x (cons sub-y pending))))]
                 [#t ; some literal
                  (if (member inside knowledge)
                      #t ; contradiction
                      (solve (cons clause knowledge) pending))]))]
            [#t ; positive literal
             (if (member (negate clause) knowledge)
                 #t
                 (solve (cons clause knowledge) pending))])))))
  (solve null (cons (negate formula) null)))

(define (pretty-print formula)
  (define (paren string)
    (string-append "(" string ")"))
  (define (pp-string formula)
    (cond
      [(Not? formula)
       (string-append "¬" (paren (pp-string (Not-x formula))))]
      [(And? formula)
       (string-append (paren (pp-string (And-x formula))) "∧" (paren (pp-string (And-y formula))))]
      [(Or? formula)
       (string-append (paren (pp-string (Or-x formula))) "∨" (paren (pp-string (Or-y formula))))]
      [#t (~v formula)]))
  (printf (pp-string formula)))

(define (print-literals knowledge)
  (define (print-filtered lits)
    (if (null? lits)
        ""
        (let ([lit (car lits)])
          (string-append
           (if (Not? lit)
               (string-append (~v (Not-x lit)) ": false")
               (string-append (~v lit) ": true"))
           "\n"
           (print-filtered (cdr lits))))))
  (printf
   (print-filtered
    (filter-map
     (lambda (x) (and (not (And? x)) (not (Or? x)) x))
     knowledge))))

