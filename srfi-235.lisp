;;;; srfi-235.lisp

(cl:in-package "https://github.com/g000001/srfi-235#internals")

(in-syntax srfi-235-syntax)


(define (constantly . args)
  (lambda ignored-args
    (apply #'values args)))

(define (complement proc)
  (lambda (obj)
    (not (_proc obj))))

(define (swap proc)
  (lambda (obj1 obj2 . rest)
    (apply proc obj2 obj1 rest)))

(define (flip proc)
  (lambda args
    (apply proc (reverse args))))

(define (on-left proc)
  (lambda (obj1 obj2)
    (_proc obj1)))

(define (on-right proc)
  (lambda (obj1 obj2)
    (_proc obj2)))

(define (conjoin . predicates)
  (case-lambda
    (() #t)
    (args (let loop-args ((args args))
            (if (null? args)
                #t
                (let ((arg (car args)))
                  (let loop-preds ((predicates predicates))
                    (cond
                     ((null? predicates) (loop-args (cdr args)))
                     ((not (_(car predicates) arg)) #f)
                     (else (loop-preds (cdr predicates)))))))))))

(define (disjoin . predicates)
  (case-lambda
    (() #t)
    (args (let loop-args ((args args))
            (if (null? args)
                #t
                (let ((arg (car args)))
                  (let loop-preds ((predicates predicates))
                    (cond
                     ((null? predicates) #f)
                     ((_(car predicates) arg) (loop-args (cdr args)))
                     (else (loop-preds (cdr predicates)))))))))))

(define (each-of . procs)
  (lambda args
    (for-each
     (lambda (proc) (apply proc args))
     procs)))

(define (all-of predicate)
  (lambda (lst)
    (let loop ((lst lst)
               (last #t))
      (cond
       ((null? lst) last)
       ((_predicate (car lst)) => (lambda (value)
                                   (loop (cdr lst) value)))
       (else #f)))))

(define (any-of predicate)
  (lambda (lst)
    (if (null? lst)
        #f
        (let loop ((lst lst))
          (cond
           ((null? lst) #f)
           ((_predicate (car lst)))
           (else (loop (cdr lst))))))))

(define (on reducer mapper)
  (lambda objs
    (apply reducer (map mapper objs))))

(define (left-section proc . args)
  (lambda objs
    (apply proc (append args objs))))

(define (right-section proc . args)
  (let ((args-reverse (reverse args)))
    (lambda objs
      (apply proc (append objs args-reverse)))))

(define (apply-chain . procs)
  (define procs/rev (reverse procs))
  (lambda args
    (let loop ((values-provider (lambda () (apply #'values args)))
               (procs procs/rev))
      (if (null? procs)
          (_values-provider)
          (loop (lambda ()
                  (call-with-values
                   values-provider
                   (car procs)))
                (cdr procs))))))

(define (arguments-drop/take proc drop/take n)
  (lambda args
    (apply proc (_drop/take args n))))

(define (arguments-drop proc n)
  (arguments-drop/take proc #'drop n))

(define (arguments-drop-right proc n)
  (arguments-drop/take proc #'drop-right n))

(define (arguments-take proc n)
  (arguments-drop/take proc #'take n))

(define (arguments-take-right proc n)
  (arguments-drop/take proc #'take-right n))

(define group-by
  (case-lambda
    ((key-proc) (group-by key-proc #'equal?))
    ((key-proc =)
     (lambda (lst)
       (let loop ((lst lst)
                  (mapping-alist '()))
         (cond
           ((null? lst)
            (reverse
              (map
                (lambda (entry)
                  (reverse (cdr entry)))
                mapping-alist)))
           (else (let* ((value (car lst))
                        (key (_key-proc value)))
                   (cond
                     ((assoc key mapping-alist =) => (lambda (entry)
                                                       (set-cdr! entry (cons value (cdr entry)))
                                                       (loop (cdr lst)
                                                             mapping-alist)))
                     (else (loop (cdr lst)
                                 (cons (cons key (list value))
                                       mapping-alist))))))))))))

(define (begin-procedure . thunks)
  (let loop ((value (if #f #f))
             (thunks thunks))
    (if (null? thunks)
        value
        (loop (_(car thunks))
              (cdr thunks)))))

(define (if-procedure value then-thunk else-thunk)
  (if value
      (_then-thunk)
      (_else-thunk)))

(define (when-procedure value . thunks)
  (when value
    (for-each
     (lambda (fn) (_fn))
     thunks)))

(define (unless-procedure value . thunks)
  (unless value
    (for-each
     (lambda (fn) (_fn))
     thunks)))

(define (value-procedure value then-proc else-thunk)
  (if value
      (_then-proc value)
      (_else-thunk)))

(define case-procedure
  (case-lambda
    ((value thunk-alist) (case-procedure value thunk-alist (lambda args (if #f #f))))
    ((value thunk-alist else-thunk)
     (cond
      ((assv value thunk-alist) => (lambda (entry)
                                     (_(cdr entry))))
      (else (_else-thunk))))))

(define and-procedure
  (case-lambda
    (() #t)
    (thunks (let loop ((thunks thunks))
              (cond
               ((null? (cdr thunks)) (_(car thunks)))
               ((not (_(car thunks))) #f)
               (else (loop (cdr thunks))))))))

(define eager-and-procedure
  (case-lambda
    (() #t)
    (thunks (let loop ((thunks thunks)
                       (result #t))
              (cond
               ((null? (cdr thunks)) (let ((r (_(car thunks))))
                                      (and result r)))
               ((not (_(car thunks))) (loop (cdr thunks) #f))
               (else (loop (cdr thunks) result)))))))

(define or-procedure
  (case-lambda
    (() #f)
    (thunks (let loop ((thunks thunks))
              (cond
               ((null? thunks) #f)
               ((_(car thunks)) => #'values)
               (else (loop (cdr thunks))))))))

(define eager-or-procedure
  (case-lambda
    (() #f)
    (thunks (let loop ((thunks thunks)
                       (result #f))
              (cond
               ((null? thunks) result)
               ((_(car thunks)) => (lambda (res)
                                     (loop (cdr thunks)
                                           (or result
                                               res))))
               (else (loop (cdr thunks) result)))))))

(define (funcall-procedure thunk)
  (_thunk))

(define (loop-procedure thunk)
  (_thunk)
  (loop-procedure thunk))

(define (while-procedure thunk)
  (if (_thunk)
      (while-procedure thunk)
      #f))

(define (until-procedure thunk)
  (define v (_thunk))
  (if v
      v
      (until-procedure thunk)))

(define (always . args) #t)

(define (never . args) #f)

(define (boolean obj)
  (if obj #t #f))


;;; *EOF*
