(library (schism compiler)
  (export compile-library)
  (import (rnrs)
          (only (chezscheme) gensym))

  (define (tag-size) 3)
  (define (fixnum-mask) -8) ;; a magic mask that turns ptrs into fixnums
  (define (tag-mask) 7)

  (define (tag-constant value tag)
    (bitwise-ior (bitwise-arithmetic-shift-left value (tag-size)) tag))

  ;; fixnums are 0 so most arithmetic doesn't require shifting
  (define (fixnum-tag) 0)
  ;; constant-tag is used for implementation constants, such as #f, #t and ()
  (define (constant-tag) 1)
  (define (constant-false) (tag-constant 0 (constant-tag)))
  (define (constant-true) (tag-constant 1 (constant-tag)))
  (define (constant-null) (tag-constant 2 (constant-tag)))
  (define (constant-eof) (tag-constant 3 (constant-tag)))

  (define (pair-tag) 2)
  (define (char-tag) 3)
  (define (string-tag) 4)
  (define (symbol-tag) 5)

  (define (allocation-pointer) 0)
  (define (word-size) 4)

  ;; ====================== ;;
  ;; Helpers, etc.          ;;
  ;; ====================== ;;

  (define (trace-value x)
    (display x) (newline)
    x)

  (define (index-of-helper x ls index)
    (if (pair? ls)
        (if (eq? x (car ls))
            index
            (index-of-helper x (cdr ls) (+ 1 index)))
        (begin
          (display x) (newline)
          (error 'index-of "Could not find item"))))
  (define (index-of x ls)
    (index-of-helper x ls 0))

  (define (primitives)
    (expand-macros
     `((%wasm-import "rt" (rt-add1 n))
       (%wasm-import "rt" (read-char))
       (%wasm-import "rt" (peek-char))
       (define (%base-pair) (%set-tag ,(allocation-pointer) ,(pair-tag)))
       (define (%symbol-table) (cdr (%base-pair)))
       (define (%alloc tag num-words)
         (let ((new-pointer (+ (* ,(word-size) num-words) (car (%base-pair)))))
           (begin
             (set-car! (%base-pair) new-pointer)
             ;; We have an unstated assumption that we always allocate at least 8 bytes.
             ;; Add two words to offset for base-pair.
             (%set-tag (+ new-pointer ,(* 2 (word-size))) tag))))
       (define (cons a d)
         (init-pair (%alloc ,(pair-tag) 2) a d))
       (define (init-pair p a d)
         (set-car! p a)
         (set-cdr! p d)
         p)
       (define (car p)
         (read-ptr p 0))
       (define (cdr p)
         (read-ptr p ,(word-size)))
       (define (read-ptr p offset)
         (%read-mem (%as-fixnum p) offset))
       (define (char->integer c)
         (%as-fixnum c))             ;; TODO: check tag
       (define (char-between c c1 c2) ;; inclusive
         (if (char-ci<? c c1)
             #f
             (if (char-ci<? c c2)
                 #t
                 (if (eq? c c2) #t #f))))
       (define (char-numeric? c)
         (char-between c #\0 #\9))
       (define (char-hex? c)
         (or (char-numeric? c) (char-between c #\a #\f)))
       (define (char-ci<? c1 c2)
         (< (char->integer c1) (char->integer c2)))
       (define (list->string ls)
         ;; For now we represent strings as lists of characters. That
         ;; means converting between the two is just a matter of
         ;; changing the tags.
         (%set-tag ls ,(string-tag)))
       (define (string->list s)
         (%set-tag s ,(pair-tag)))
       (define (string-equal? s1 s2)
         (list-all-eq? (string->list s1) (string->list s2)))
       (define (%find-symbol-by-name s table)
         (if (or (zero? table) (null? table))
             #f
             (if (string-equal? s (car table))
                 (%set-tag table ,(symbol-tag))
                 (%find-symbol-by-name s (cdr table)))))
       (define (string->symbol s)
         (or (%find-symbol-by-name s (%symbol-table))
             (let ((x (cons s (%symbol-table))))
               (begin (set-cdr! (%base-pair) x)
                      (%set-tag x ,(symbol-tag))))))
       (define (symbol->string x)
         ;; TODO: typecheck
         (car (%set-tag x ,(pair-tag))))
       (define (list-all-eq? a b)
         (if (null? a)
             (null? b)
             (if (null? b)
                 #f
                 (if (eq? (car a) (car b))
                     (list-all-eq? (cdr a) (cdr b))
                     #f))))
       (define (< a b)
         (if (< a b) #t #f))
       (define (read)
         (start-read (read-char)))
       (define (start-read c)
         (if (char-numeric? c)
             (read-number (- (char->integer c) (char->integer #\0)))
             (if (eq? c #\#)
                 (read-hash (read-char))
                 #f)))
       (define (read-number acc)
         (if (char-numeric? (peek-char))
             (read-number (+ (* acc 10) (- (char->integer (read-char))
                                           (char->integer #\0))))
             acc))
       (define (hex-digit c)
         (if (char-numeric? c)
             (- (char->integer c)
                (char->integer #\0))
             (+ 10 (- (char->integer c)
                      (char->integer #\a)))))
       (define (read-hex acc)
         (if (char-hex? (peek-char))
             (read-hex (+ (* acc 16) (hex-digit (read-char))))
             acc))
       (define (read-hash c)
         (cond
          ((eq? c #\\)
           (read-char))
          ((eq? c #\x)
           (read-hex 0))
          (else #f)))
       (define (eq? a b)
         (if (eq? a b) #t #f))
       (define (zero? n)
         (eq? n 0))
       (define (null? x)
         (eq? x '()))
       (define (pair? p)
         (eq? (%get-tag p) ,(pair-tag))))))

  (define (intrinsic? x)
    (or (eq? x '%read-mem) (eq? x '%store-mem) (eq? x '%get-tag)
        (eq? x '%set-tag) (eq? x '%as-fixnum) (eq? x 'bitwise-and)
        (eq? x 'bitwise-not) (eq? x 'eof-object) (eq? x '+) (eq? x '*)
        (eq? x '-) (eq? x 'set-car!) (eq? x 'set-cdr!)))

  ;; ====================== ;;
  ;; Parsing                ;;
  ;; ====================== ;;

  (define (expand-macros expr)
    (if (pair? expr)
        (let ((tag (car expr)))
          (cond
           ((eq? tag 'or)
            (if (null? (cdr expr))
                #f
                (if (null? (cddr expr))
                    (expand-macros (cadr expr))
                    (let ((t (gensym "t")))
                      `(let ((,t ,(expand-macros (cadr expr))))
                         (if ,t ,t ,(expand-macros (cons 'or (cddr expr)))))))))
           ((eq? tag 'not)
            `(if ,(expand-macros (cadr expr)) #f #t))
           ((eq? tag 'cond)
            (let ((clause (cadr expr))
                  (rest (cddr expr)))
              (if (eq? (car clause) 'else)
                  (expand-macros (cadr clause))
                  `(if ,(expand-macros (car clause))
                       ,(expand-macros (cadr clause))
                       ,(expand-macros (cons 'cond rest))))))
           (else (expand-macros* expr))))
        expr))
  (define (expand-macros* exprs)
    (if (null? exprs)
        '()
        (cons (expand-macros (car exprs)) (expand-macros* (cdr exprs)))))

  (define (parse-exprs exprs)
    (if (null? exprs)
        '()
        (cons (parse-expr (car exprs)) (parse-exprs (cdr exprs)))))
  (define (parse-expr expr)
    (cond
     ((null? expr) '(null))
     ((number? expr)
      (list 'number expr))
     ((boolean? expr) (list 'bool expr))
     ((char? expr)
      (list 'char expr))
     ((string? expr)
      (list 'call 'list->string (parse-expr (list 'quote (string->list expr)))))
     ((symbol? expr)
      (list 'var expr))
     ((pair? expr)
      (let ((op (car expr)))
        (cond
         ((eq? op 'quote)
          (parse-expr (expand-quote (cadr expr) #f)))
         ((eq? op 'quasiquote)
          (parse-expr (expand-quote (cadr expr) #t)))
         ((eq? op 'if)
          (let ((t (cadr expr))
                (c (caddr expr))
                (a (cadddr expr)))
            (list 'if (parse-pred t) (parse-expr c) (parse-expr a))))
         ((eq? op 'let)
          (let ((bindings (parse-bindings (cadr expr)))
                (body (parse-expr (caddr expr))))
            (list 'let bindings body)))
         ((eq? op 'begin)
          (cons 'begin (parse-exprs (cdr expr))))
         ((intrinsic? op)
          (cons op (parse-exprs (cdr expr))))
         (else
          ;; this is a function call
          (cons 'call (cons (car expr) (parse-exprs (cdr expr))))))))
     (else
      (display expr) (newline)
      (error 'parse-expr "Unrecognized expression"))))
  (define (parse-pred expr)
    (cond
     ((boolean? expr) (list 'bool expr))
     ((pair? expr)
      (let ((op (car expr)))
        (cond
         ((eq? op 'eq?)
          (list 'eq? (parse-expr (cadr expr)) (parse-expr (caddr expr))))
         ((eq? op '<)
          (list '< (parse-expr (cadr expr)) (parse-expr (caddr expr))))
         (else
          (list 'neq? (parse-expr expr) (parse-expr #f))))))
     (else
      (list 'neq? (parse-expr expr) (parse-expr #f)))))

  (define (parse-bindings bindings)
    (if (null? bindings)
        '()
        (let ((var (caar bindings))
              (value (parse-expr (cadar bindings))))
          (cons (list var value) (parse-bindings (cdr bindings))))))

  (define (parse-body* body*)
    (if (null? body*)
        '()
        (cons (parse-expr (car body*)) (parse-body* (cdr body*)))))
  (define (parse-body body)
    ;; expr ... -> (begin expr ...)
    (cons 'begin (parse-body* body)))

  (define (parse-function function)
    (let ((type (car function)))
      (cond
       ((eq? 'define type)
        (let ((name (caadr function))
              (args (cdadr function))
              (body (parse-body (cddr function))))
          (list (cons name args) body)))
       ((eq? '%wasm-import type)
        function)
       (else
        (display function) (newline)
        (error 'parse-function "Invalid top-level declaration")))))

  (define (parse-functions functions)
    (if (null? functions)
        '()
        (cons (parse-function (car functions)) (parse-functions (cdr functions)))))

  (define (parse-library lib)
    ;; For now just assume it's correctly formed. We can do error checking later.
    (let ((body (cddr lib)))      ;; skip the library and name
      (let ((exports (cdar body)) ;; names of the functions exported
            (functions (parse-functions (append (primitives) (cddr body)))))
        (cons exports functions))))

  (define (expand-quote expr quasi)
    (cond
     ;; Literals self-evaluate
     ((or (number? expr) (boolean? expr) (char? expr) (string? expr) (null? expr))
      expr)
     ((symbol? expr)
      `(string->symbol ,(symbol->string expr)))
     ((pair? expr)
      (if (and quasi (eq? (car expr) 'unquote))
          (cadr expr)
          (list 'cons (expand-quote (car expr) quasi) (expand-quote (cdr expr) quasi))))
     (else
      (error 'expand-quote "Invalid datum" expr))))

  (define (args->types args)
    (if (null? args)
        '()
        (cons 'i32 (args->types (cdr args)))))


  ;; ====================== ;;
  ;; Apply representation   ;;
  ;; ====================== ;;

  (define (apply-representation fn*)
    (if (null? fn*)
        '()
        (cons (apply-representation-fn (car fn*)) (apply-representation (cdr fn*)))))
  (define (apply-representation-fn fn)
    (if (eq? (car fn) '%wasm-import)
        fn
        (let ((def (car fn))
              (body (cadr fn)))
          (list def (apply-representation-expr body)))))
  (define (apply-representation-expr expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'null)
        (list 'ptr (constant-null)))
       ((eq? tag 'bool)
        (list 'ptr (if (cadr expr)
                       (constant-true)
                       (constant-false))))
       ((eq? tag 'char)
        (list 'ptr (tag-constant (char->integer (cadr expr)) (char-tag))))
       ((eq? tag 'begin)
        (cons 'begin (apply-representation-expr* (cdr expr))))
       ((eq? tag 'var) expr)
       ((eq? tag 'number) (list 'ptr (bitwise-arithmetic-shift-left (cadr expr) (tag-size))))
       ((eq? tag 'eof-object) (list 'ptr (constant-eof)))
       ((eq? tag 'call)
        (cons 'call (cons (cadr expr) (apply-representation-expr* (cddr expr)))))
       ((eq? tag 'if)
        (let ((t (cadr expr))
              (c (caddr expr))
              (a (cadddr expr)))
          (list 'if
                (apply-representation-pred t)
                (apply-representation-expr c)
                (apply-representation-expr a))))
       ((eq? tag 'let)
        (list 'let
              (apply-representation-bindings (cadr expr))
              (apply-representation-expr (caddr expr))))
       ((intrinsic? tag)
        (cons tag (apply-representation-expr* (cdr expr))))
       (else
        (display expr) (newline)
        (error 'apply-representation-expr "Unrecognized expr")))))
  (define (apply-representation-expr* expr*)
    (if (null? expr*)
        '()
        (cons (apply-representation-expr (car expr*)) (apply-representation-expr* (cdr expr*)))))
  (define (apply-representation-pred pred)
    (let ((tag (car pred)))
      (cond
       ((eq? tag 'zero?) (list 'zero? (apply-representation-expr (cadr pred))))
       ((eq? tag 'eq?) (list 'eq?
                             (apply-representation-expr (cadr pred))
                             (apply-representation-expr (caddr pred))))
       ((eq? tag '<) (list '<
                           (apply-representation-expr (cadr pred))
                           (apply-representation-expr (caddr pred))))
       ((eq? tag 'neq?) (list 'neq?
                              (apply-representation-expr (cadr pred))
                              (apply-representation-expr (caddr pred))))
       ((eq? tag 'bool) pred)
       (else
        (display pred) (newline)
        (error 'apply-representation-pred "Unrecognized pred")))))
  (define (apply-representation-bindings bindings)
    (if (null? bindings)
        '()
        (cons (list (caar bindings)
                    (apply-representation-expr (cadar bindings)))
              (apply-representation-bindings (cdr bindings)))))

  ;; ====================== ;;
  ;; Compile (make wasm)    ;;
  ;; ====================== ;;

  (define (compile-exprs exprs env)
    (if (null? exprs)
        '()
        (cons (compile-expr (car exprs) env) (compile-exprs (cdr exprs) env))))
  (define (compile-expr expr env)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'begin) (cons 'begin (compile-exprs (cdr expr) env)))
       ((eq? tag 'number) (cons 'i32.const (cdr expr)))
       ((eq? tag 'ptr) (cons 'i32.const (cdr expr)))
       ((eq? tag 'var) (list 'get-local (cdr (assq (cadr expr) env))))
       ((eq? tag 'call) (cons 'call (cons (cadr expr) (compile-exprs (cddr expr) env))))
       ((eq? tag 'if)
        (let ((t (cadr expr))
              (c (caddr expr))
              (a (cadddr expr)))
          (list 'if (compile-pred t env) (compile-expr c env) (compile-expr a env))))
       ((eq? tag 'let)
        (let ((index (length env)))
          (list 'begin
                (compile-bindings (cadr expr) env index)
                (compile-expr (caddr expr) (bindings->env (cadr expr) env index)))))
       ((eq? tag 'set-car!)
        (let ((p (compile-expr (cadr expr) env))
              (x (compile-expr (caddr expr) env)))
          `(i32.store (offset 0) (i32.and ,p (i32.const ,(fixnum-mask))) ,x)))
       ((eq? tag 'set-cdr!)
        (let ((p (compile-expr (cadr expr) env))
              (x (compile-expr (caddr expr) env)))
          `(i32.store (offset ,(word-size)) (i32.and ,p (i32.const ,(fixnum-mask))) ,x)))
       ((eq? tag '+)
        (let ((a (compile-expr (cadr expr) env))
              (b (compile-expr (caddr expr) env)))
          `(i32.add ,a ,b)))
       ((eq? tag '-)
        (let ((a (compile-expr (cadr expr) env))
              (b (compile-expr (caddr expr) env)))
          ;; Subtraction might borrow into the tag, so mask off the low bits
          `(i32.and (i32.sub ,a ,b) (i32.const -8))))
       ((eq? tag '*)
        (let ((a (compile-expr (cadr expr) env))
              (b (compile-expr (caddr expr) env)))
          ;; Shift only one of them and we don't have to shift back when we're done.
          `(i32.mul (i32.shr_s ,a (i32.const ,(tag-size))) ,b)))
       ((eq? tag '%as-fixnum)
        `(i32.and ,(compile-expr (cadr expr) env) (i32.const ,(fixnum-mask))))
       ((eq? tag '%set-tag)
        `(i32.or (i32.and ,(compile-expr (cadr expr) env) (i32.const ,(fixnum-mask)))
                 (i32.shr_s ,(compile-expr (caddr expr) env) (i32.const ,(tag-size)))))
       ((eq? tag '%get-tag)
        `(i32.shl (i32.and ,(compile-expr (cadr expr) env) (i32.const ,(tag-mask)))
                  (i32.const ,(tag-size))))
       ((eq? tag '%read-mem)
        `(i32.load (offset 0) (i32.add ,(compile-expr (cadr expr) env)
                                       (i32.shr_s ,(compile-expr (caddr expr) env)
                                                  (i32.const ,(tag-size))))))
       ((eq? tag 'bitwise-and)
        (cons 'i32.and (compile-exprs (cdr expr) env)))
       ((eq? tag 'bitwise-not)
        (cons 'i32.not (compile-exprs (cdr expr) env)))
       (else (display expr) (newline) (error 'compile-expr "Unrecognized expression")))))
  (define (compile-pred expr env)
    (let ((op (car expr)))
      (cond
       ((eq? op 'zero?)
        (list 'i32.eqz (compile-expr (cadr expr) env)))
       ((eq? op 'eq?)
        (list 'i32.eq
              (compile-expr (cadr expr) env)
              (compile-expr (caddr expr) env)))
       ((eq? op 'neq?)
        (list 'i32.ne
              (compile-expr (cadr expr) env)
              (compile-expr (caddr expr) env)))
       ((eq? op '<)
        (list 'i32.lt_s
              (compile-expr (cadr expr) env)
              (compile-expr (caddr expr) env)))
       ((eq? op 'bool)
       	;; Since we're in pred context, bools get translated into
        ;; things that go directly into if.
        (if (cadr expr)
            (list 'i32.const 1)
            (list 'i32.const 0)))
       (else
        (display expr) (newline)
        (error 'compile-pred "Unrecognized predicate")))))

  (define (bindings->env bindings env index)
    (if (null? bindings)
        env
        (cons (cons (caar bindings) index)
              (bindings->env (cdr bindings) env (+ 1 index)))))
  (define (compile-binding binding env index)
    (list 'set-local index (compile-expr (cadr binding) env)))
  (define (compile-bindings bindings env index)
    (if (null? (cdr bindings))
        (compile-binding (car bindings) env index)
        (list 'begin
              (compile-binding (car bindings) env index)
              (compile-bindings (cdr bindings) env (+ 1 index)))))
  (define (compile-function fn)
    (if (eq? (car fn) '%wasm-import)
        fn
        (let ((args (number-variables (cdar fn) 0)))
          (let ((body (compile-expr (cadr fn) args)))
            (list
             (- (count-locals body) (length args)) ;; Number of local variables
             body)))))

  ;; Determines how many instructions were used in a body.
  (define (count-locals body)
    (let ((tag (car body)))
      (cond
       ((eq? tag 'begin)
        (count-locals-exprs (cdr body)))
       ((eq? tag 'call)
        (count-locals-exprs (cddr body)))
       ((eq? tag 'get-local) 0)
       ((eq? tag 'set-local) (+ 1 (cadr body)))
       ((wasm-simple-op? tag)
        (count-locals-exprs (cdr body)))
       ((eq? tag 'i32.const) 0)
       ((or (eq? tag 'i32.store) (eq? tag 'i32.load))
        (count-locals-exprs (cddr body)))
       ((eq? tag 'if)
        (count-locals-exprs (cdr body)))
       (else
        (trace-value body)
        (error 'count-locals "Unrecognized expression")))))
  (define (count-locals-exprs exprs)
    (if (null? exprs)
        0
        (max (count-locals (car exprs)) (count-locals-exprs (cdr exprs)))))

  (define (number-variables vars index)
    (if (pair? vars)
        (cons (cons (car vars) index) (number-variables (cdr vars) (+ 1 index)))
        '()))

  (define (function->type fn)
    ;; Functions are assumed to always return an i32 and take some number of i32s as inputs
    (let ((args (if (eq? (car fn) '%wasm-import)
                    (args->types (cdaddr fn))
                    (args->types (cdar fn)))))
      (cons 'fn (list args '(i32)))))
  (define (functions->types fns)
    (if (null? fns)
        '()
        (let ((type (function->type (car fns))))
          (cons type (functions->types (cdr fns))))))

  (define (function->name fn)
    (if (eq? (car fn) '%wasm-import)
        (caaddr fn)
        (caar fn)))
  (define (functions->names fns)
    (if (null? fns)
        '()
        (cons (function->name (car fns)) (functions->names (cdr fns)))))

  (define (compile-functions fn*)
    (if (null? fn*)
        '()
        (cons (compile-function (car fn*)) (compile-functions (cdr fn*)))))

  (define (replace-export exports name index)
    (if (null? exports)
        '()
        (let ((ex (car exports))
              (rest (replace-export (cdr exports) name index)))
          (if (or (pair? ex) (not (eq? ex name)))
              (cons ex rest)
              (cons (list 'fn index (symbol->string name)) rest)))))

  (define (build-exports exports functions index)
    (if (null? functions)
        exports
        (let ((name (if (eq? (caar functions) '%wasm-import)
                        (caddar functions)
                        (caaar functions))))
          (let ((exports (replace-export exports name index)))
            (build-exports exports (cdr functions) (+ 1 index))))))

  (define (number-list ls i)
    (if (null? ls)
        '()
        (cons i (number-list (cdr ls) (+ 1 i)))))

  (define (wasm-simple-op? op)
    (or (eq? op 'i32.and) (eq? op 'i32.add) (eq? op 'i32.sub) (eq? op 'i32.mul)
        (eq? op 'i32.or) (eq? op 'i32.eq) (eq? op 'i32.ne) (eq? op 'i32.not)
        (eq? op 'i32.lt_s) (eq? op 'i32.shr_s) (eq? op 'i32.shl)))

  (define (resolve-calls-exprs exprs env)
    (if (null? exprs)
        '()
        (cons (resolve-calls-expr (car exprs) env) (resolve-calls-exprs (cdr exprs) env))))
  (define (resolve-calls-expr expr env)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'i32.const) expr)
       ((eq? tag 'get-local) expr)
       ((eq? tag 'set-local) (list 'set-local (cadr expr) (resolve-calls-expr (caddr expr) env)))
       ((eq? tag 'begin) (cons 'begin (resolve-calls-exprs (cdr expr) env)))
       ((eq? tag 'call) (cons 'call (cons (index-of (cadr expr) env)
                                          (resolve-calls-exprs (cddr expr) env))))
       ((eq? tag 'if)
        ;; TODO: support calls in the predicate
        (let ((t (cadr expr))
              (c (caddr expr))
              (a (cadddr expr)))
          (list 'if
                (resolve-calls-expr t env)
                (resolve-calls-expr c env)
                (resolve-calls-expr a env))))
       ((eq? tag 'i32.store)
        (let ((offset (cadr expr))
              (index (resolve-calls-expr (caddr expr) env))
              (value (resolve-calls-expr (cadddr expr) env)))
          (list 'i32.store offset index value)))
       ((eq? tag 'i32.load)
        (let ((offset (cadr expr))
              (index (resolve-calls-expr (caddr expr) env)))
          (list 'i32.load offset index)))
       ((wasm-simple-op? tag)
        (let ((args (resolve-calls-exprs (cdr expr) env)))
          (cons tag args)))
       (else
        (display expr) (newline)
        (error 'resolve-calls-expr "Unrecognized expression")))))
  (define (resolve-calls-fn function env)
    (list (car function) (resolve-calls-expr (cadr function) env)))
  (define (resolve-calls functions env)
    (if (null? functions)
        '()
        (if (eq? (caar functions) '%wasm-import)
            (resolve-calls (cdr functions) env)
            (cons (resolve-calls-fn (car functions) env) (resolve-calls (cdr functions) env)))))

  (define (gather-imports compiled-module)
    (if (null? compiled-module)
        '()
        (let ((rest (gather-imports (cdr compiled-module)))
              (entry (car compiled-module)))
          (if (eq? (caar compiled-module) '%wasm-import)
              (let ((module (cadr entry))
                    (name (symbol->string (caaddr entry)))
                    (type (function->type entry)))
                (cons (list module name type) rest))
              rest))))

  ;; ====================== ;;
  ;; Wasm Binary Generation ;;
  ;; ====================== ;;

  (define (number->leb-u8-list n)
    (if (and (< n #x40) (> n (- #x40)))
        (list (bitwise-and n #x7f))
        (cons (bitwise-ior #x80 (bitwise-bit-field n 0 7))
              (number->leb-u8-list (bitwise-arithmetic-shift-right n 7)))))

  (define (encode-chars chars)
    (if (null? chars)
        '()
        (cons (char->integer (car chars)) (encode-chars (cdr chars)))))

  (define (encode-string s)
    (let ((chars (string->list s)))
      (make-vec (length chars) (encode-chars chars))))

  (define (wasm-header)
    (list #x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))

  (define (make-vec length contents)
    (append (number->leb-u8-list length) contents))

  ;; id is the number, contents is a list of bytes
  (define (make-section id contents)
    (cons id (make-vec (length contents) contents)))

  (define (encode-type-vec-contents types)
    (if (null? types)
        '()
        (append (encode-type (car types)) (encode-type-vec-contents (cdr types)))))

  (define (encode-type-vec types)
    (make-vec (length types) (encode-type-vec-contents types)))

  (define (encode-type type)
    (cond
     ((eq? type 'i32) '(#x7f))
     ((eq? type 'i64) '(#x7e))
     ((eq? type 'f32) '(#x7d))
     ((eq? type 'f64) '(#x7c))
     ;; functions are (fn (t1 ...) (t2 ...)), for t1 ... -> t2 ...
     ((and (pair? type) (eq? (car type) 'fn))
      (cons #x60 (append (encode-type-vec (cadr type)) (encode-type-vec (caddr type)))))
     (else (display type) (newline) (error 'encode-type "Unrecognized type"))))

  (define (wasm-type-section types)
    (make-section 1 (encode-type-vec types)))

  (define (wasm-import-section imports)
    (make-section 2 (make-vec (length imports) (encode-imports imports 0))))
  (define (encode-imports imports index)
    (if (null? imports)
        '()
        (append (encode-import (car imports) index) (encode-imports (cdr imports) (+ 1 index)))))
  (define (encode-import import index)
    (let ((module (car import))
          (name (cadr import)))
      (append (encode-string module) (encode-string name) '(#x00) (number->leb-u8-list index))))

  (define (encode-u32-vec-contents nums)
    (if (null? nums)
        '()
        (append (number->leb-u8-list (car nums)) (encode-u32-vec-contents (cdr nums)))))

  (define (encode-u32-vec nums)
    (make-vec (length nums) (encode-u32-vec-contents nums)))

  (define (wasm-function-section function-type-ids)
    (make-section 3 (encode-u32-vec function-type-ids)))

  (define (encode-export export)
    (cond
     ((eq? (car export) 'fn)
      (append (encode-string (caddr export)) (cons #x00 (number->leb-u8-list (cadr export)))))))

  (define (encode-export-contents exports)
    (if (null? exports)
        '()
        (append (encode-export (car exports)) (encode-export-contents (cdr exports)))))

  (define (wasm-export-section exports)
    (make-section 7 (make-vec (length exports) (encode-export-contents exports))))

  (define (encode-exprs exprs)
    (if (null? exprs)
        '()
        (append (encode-expr (car exprs)) (encode-exprs (cdr exprs)))))

  (define (encode-simple-op op expr)
    (append (encode-exprs (cdr expr)) (list op)))

  (define (encode-expr expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'begin)
        (encode-exprs (cdr expr)))
       ((eq? tag 'i32.const)
        (cons #x41 (number->leb-u8-list (cadr expr))))
       ((eq? tag 'i32.eqz)
        (append (encode-expr (cadr expr)) (list #x45)))
       ((eq? tag 'i32.eq)
        (encode-simple-op #x46 expr))
       ((eq? tag 'i32.ne)
        (encode-simple-op #x47 expr))
       ((eq? tag 'i32.lt_s)
        (encode-simple-op #x48 expr))
       ((eq? tag 'get-local)
        (cons #x20 (number->leb-u8-list (cadr expr))))
       ((eq? tag 'set-local)
        (append (encode-expr (caddr expr)) (cons #x21 (number->leb-u8-list (cadr expr)))))
       ((eq? tag 'call)
        (append (encode-exprs (cddr expr))
                (cons #x10 (number->leb-u8-list (cadr expr)))))
       ((eq? tag 'if)
        (let ((t (cadr expr))
              (c (caddr expr))
              (a (cadddr expr)))
          ;; For now, if blocks are assumed to always return i32
          (append (encode-expr t)
                  (list #x04 #x7f) (encode-expr c)
                  (list #x05) (encode-expr a)
                  (list #x0b))))
       ((eq? tag 'i32.store)
        (let ((align 0)
              (offset (cadadr expr))
              (index (encode-expr (caddr expr)))
              (value (encode-expr (cadddr expr))))
          (append index value
                  (list #x36 #x0) ;;always use 0 alignment
                  (number->leb-u8-list offset))))
       ((eq? tag 'i32.load)
        (let ((align 0)
              (offset (cadadr expr))
              (index (encode-expr (caddr expr))))
          (append index
                  (list #x28 #x0) ;;always use 0 alignment
                  (number->leb-u8-list offset))))
       ((eq? tag 'i32.add)
        (encode-simple-op #x6a expr))
       ((eq? tag 'i32.sub)
        (encode-simple-op #x6b expr))
       ((eq? tag 'i32.mul)
        (encode-simple-op #x6c expr))
       ((eq? tag 'i32.and)
        (encode-simple-op #x71 expr))
       ((eq? tag 'i32.not)
        (encode-simple-op #x71 expr))
       ((eq? tag 'i32.or)
        (encode-simple-op #x72 expr))
       ((eq? tag 'i32.shl)
        (encode-simple-op #x74 expr))
       ((eq? tag 'i32.shr_s)
        (encode-simple-op #x75 expr))
       (else
        (display expr) (newline)
        (error 'encode-expr "Unrecognized expr")))))

  (define (encode-code locals body)
    (let ((contents (append
                     (if (zero? locals)
                         (make-vec 0 '())
                         (make-vec 1 (append (number->leb-u8-list locals) (list #x7f))))
                     (encode-expr body)
                     '(#x0b))))
      (make-vec (length contents) contents)))

  (define (encode-codes codes)
    (if (null? codes)
        '()
        (append (encode-code (caar codes) (cadar codes)) (encode-codes (cdr codes)))))

  (define (wasm-code-section codes)
    (make-section 10 (make-vec (length codes) (encode-codes codes))))

  (define (wasm-memory-section)
    ;; For now we hardcode a memory
    (make-section 5 (make-vec 1
                              ;; Memory with 1 page and no maximum
                              (list 0 1))))

  ;; Takes a library and returns a bytevector of the corresponding Wasm module
  ;; bytes
  (define (compile-library library)
    ;; (parsed-lib : (exports . functions)
    (let ((parsed-lib (parse-library (expand-macros library))))
      (let ((exports (car parsed-lib))
            (types (functions->types (cdr parsed-lib)))
            (function-names (functions->names (cdr parsed-lib))))
        (let ((compiled-module (compile-functions
                                (apply-representation
                                 (cdr parsed-lib)))))
          (let ((exports (build-exports exports (cdr parsed-lib) 0))
                (imports (gather-imports compiled-module))
                (functions (resolve-calls compiled-module function-names)))
            (let ((module (append (wasm-header)
                                  (wasm-type-section types)
                                  (wasm-import-section imports)
                                  ;; Function signatures happen after imports
                                  (wasm-function-section (number-list functions (length imports)))
                                  (wasm-memory-section)
                                  (wasm-export-section exports)
                                  (wasm-code-section functions))))
              (u8-list->bytevector module))))))))
