;;;;
;;;; ffi.sch
;;;;
;;;; Foreign interface helper macros.
;;;;

;; Handy macro fro including header files.
;;   header-filename -> Filename to include
;;   opts            -> Can be empty, 'local or 'system.  If empty it defaults
;;                      to 'system.  Local will use #include "xxx.h" and system
;;                      will use #include <xxx.hh>
(define-macro (c-include header-filename . opts)
  (let* ((scope         (if (null? opts) 'system (car opts)))
         (begin-bracket (if (eq? scope 'system) "<" "\""))
         (end-bracket   (if (eq? scope 'system) ">" "\"")))
    `(c-declare ,(string-append "#include " begin-bracket
                                header-filename
                                end-bracket))))

;;;
;;; C Struct interface
;;;
;;; The only macro needed out of here is the define-c-struct macro at the 
;;; bottom.  The rest are helper macros that augment that macro.
;;;

;; Used by the define-c-struct to create getters.
(define-macro (define-c-struct-getter struct-name field-name field-type)
  `(define ,(string->symbol (string-append (symbol->string struct-name) "-" 
                                           (symbol->string field-name)))
     (c-lambda (,struct-name) ,field-type 
               ,(string-append "___result = ___arg1." 
                               (symbol->string field-name) ";"))))

;; Used by the define-c-struct to create setters
(define-macro (define-c-struct-setter struct-name field-name field-type)
  `(define ,(string->symbol (string-append (symbol->string struct-name) "-" 
                                           (symbol->string field-name) 
                             "-set!"))
     (c-lambda (,struct-name ,field-type) void
               ,(string-append "___arg1." (symbol->string field-name)
                               " = ___arg2;"))))

;; Used by the define-c-struct to create an allocator for the struct.  An
;; allocator doesn't really do construction - it just allocates the memory
;; via c declare.
(define-macro (define-c-struct-allocator struct-name c-struct-name)
  `(define ,(string->symbol (string-append "allocate-" 
                                           (symbol->string struct-name)))
     (c-lambda () ,struct-name
               ,(string-append c-struct-name " new_struct;"
                               "___result = new_struct;"))))

;; Used to create an equivalency function for a struct.  This will generate
;; a (struct-name-equal? x y) function where x and y are the struct.
(define-macro (define-c-struct-equal? struct-name fields)
  (let ((is-a? (string->symbol 
                (string-append (symbol->string struct-name) "?")))
        (accessors (map (lambda (field)
                          (string->symbol (string-append 
                                           (symbol->string struct-name)
                                           "-"
                                           (symbol->string (cadr field)))))
                        fields))
        (eq-fns    (map (lambda (field)
                          (let ((rest (member eq: field)))
                            (if (not rest)
                                'equal?
                                (cadr rest))))
                        fields)))
    `(define (,(string->symbol (string-append (symbol->string struct-name) 
                                              "-equal?"))
              struct1 struct2)
       (if (and (,is-a? struct1)
                (,is-a? struct2))
           ;; Compare the fields
           (and ,@(map (lambda (accessor eq-fn)
                         `(,eq-fn (,accessor struct1) (,accessor struct2)))
                       accessors eq-fns))
           #f))))

;; Creates a macro to test if a scheme object is-a specific c struct.  Utility
;; macro.
(define-macro (define-c-struct-is-a? struct-name)
  `(define (,(string->symbol (string-append (symbol->string struct-name) "?"))
            obj)
     (and (foreign? obj)
          (eq? (car (foreign-tags obj)) (quote ,struct-name)))))

;; Creates a wrapped release function from a given existing function name.
(define-macro (generate-release-fn release-fn)
  `(c-declare
    ,(string-append "___SCMOBJ " release-fn "_release(void* arg) { "
                    "   " release-fn "(arg); "
                    "   return ___FIX(___NO_ERR); "
                    "}")))

;; Used to define a c structure.  Here is an example:
;;
;;  (define-c-struct foo "foo_t" "foo_delete"
;;     (int x)
;;     (float y eq: eqv?))
;;
;; Generated:
;;   Will create a struct named foo with members x and y.  It will fully expand
;;   into a couple of getters and setters.  These will be of the form
;;     (foo-x <foo>) and (foo-x-set! <foo>)
;;
;;   It will generate an equivalency function:
;;     (foo-equal? x y)
;;
;;   It will generate an is-a? function to test for a type:
;;     (foo? x) -> #t if x is a foo
;;
;; Note that fields have options.  Here are the available options:
;;   eq: -> defines the equivalency function to use
(define-macro (define-c-struct struct-name c-struct-name . fields)
  (let ((true-fields (if (string? (car fields))
                         (cdr fields)
                         fields))
        (release-fn (if (string? (car fields))
                        (car fields)
                        #f)))
    `(begin
       ; Generate the release function if needed
       ,(if release-fn
            `(generate-release-fn ,release-fn))

       ; Make sure we tag the new type with it's scheme name for equivalency and
       ; is-a? testing purposes.  If a cleanup function has been added use
       ; it when the object is cleaned up by the garbage collector.
       (c-define-type ,struct-name 
                      ,(append `(type ,c-struct-name ,struct-name) 
                               (list release-fn)))

       ; Define the getters and setters
       ,@(map (lambda (field)
                `(define-c-struct-getter 
                   ,struct-name 
                   ,(cadr field) 
                   ,(car field)))
              true-fields)
       ,@(map (lambda (field)
                `(define-c-struct-setter 
                   ,struct-name 
                   ,(cadr field) 
                   ,(car field)))
              true-fields)

       ; Utility functions
       (define-c-struct-is-a? ,struct-name)
       (define-c-struct-equal? ,struct-name ,true-fields))))

;;;
;;; Enumeration
;;;

;; Defines an enumerated set of constant defines.  Each value may be a single
;; symbol or a pair (symbol num) where symbol will be bound to num instead of
;; the increasing current count.  prefix will be added to all values.  Here
;; is an example:
;;
;;  (define foo
;;    bar
;;    (baz 5)
;;    rahr)
;;
;; Will define: foo-bar as 0, foo-baz as 5 and foo-rahr as 6
(define-macro (define-enum prefix . values)
  `(begin
     ,@(let loop ((results '())
                  (head    (car values))
                  (tail    (cdr values))
                  (num     0))
         (let ((value (if (pair? head) (cadr head) num))
               (name (string->symbol 
                      (string-append (symbol->string prefix)
                                     "-"
                                     (symbol->string 
                                      (if (pair? head) (car head) head))))))
           (if (null? tail)
               (append (list `(define ,name ,value)) results)
               (loop (append (list `(define ,name ,value)) results)
                     (car tail)
                     (cdr tail)
                     (+ 1 value)))))))
