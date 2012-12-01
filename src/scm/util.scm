;;;;
;;;; utility functions
;;;;

(##include "ffi.sch")
(c-include "util.h" 'local)

(define rand-int (c-lambda (int int) int "rand_int"))
(define rand-flonum (c-lambda () double "rand_double"))
(define logmsg
  (c-lambda (nonnull-char-string) void "logmsg(___arg1);"))

;; Takes a list and randomly shuffles the order of the elements.
(define (shuffle lst)
  (define (vector-swap vec idx1 idx2)
    (let ((tmp (vector-ref vec idx1)))
      (vector-set! vec idx1 (vector-ref vec idx2))
      (vector-set! vec idx2 tmp))
    vec)

  (vector->list
   (let loop ((data (list->vector lst))
              (size (length lst))
              (idx  0))
     (if (>= idx size)
         data
         (loop (vector-swap data (rand-int 0 (- size 1)) idx)
               size
               (+ 1 idx))))))

;; Sorts a list of elements (quicksort)
(define (sort lst compare)
  (cond
   ((or (null? lst) (eq? (length lst) 1)) lst)
   ((eq? (length lst) 2)
    (if (compare (car lst) (cadr lst))
        lst
        (list (cadr lst) (car lst))))
   (else
    (let* ((p    (car lst))
           (rest (cdr lst)))
      (call-with-values
          (lambda () (partition (lambda (x) (compare x p)) rest)) 
          (lambda (left right) 
            (append (sort left compare)
                    (list p)
                    (sort right compare))))))))

(define (integer->list int)
  (if (equal? int 0)
      (list 0)
      (let loop ((result '())
                 (num    int))
        (if (eqv? num 0)
            result
            (loop (cons (remainder num 10) result)
                  (quotient num 10))))))

;; Takes a variety of inputs and transforms them into a string.  This expects
;; each argument to be either a primitive type or a list.
;; If a list it should be a formatting option and a value.  The following
;; formatting options are implemented:
;;  =Integers=
;;   (pad-right x width [chr]) -> Pads a number to the right hand.  It will pad
;;                                with zeros unless chr is provided.
(define (str arg . rest)
  (define (pad-right n width chr)
    (let ((int-lst (integer->list n)))
      (list->string
       (append (make-list (- width (length int-lst)) chr)
               (map (lambda (x) (integer->char (+ x 48))) int-lst)))))

  (fold
   (lambda (current result)
     (string-append result
                    (cond
                     ((pair? current)
                      (case (car current) 
                        ((pad-right) (pad-right (cadr current)
                                                (caddr current)
                                                (if (> (length current) 3)
                                                    (cadddr current) #\0)))))
                     ((number? current)  (number->string current))
                     ((char? current)    (string current))
                     ((boolean? current) (if current "true" "false"))
                     (else current))))
   "" (cons arg rest)))

