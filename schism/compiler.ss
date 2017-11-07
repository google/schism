(library (schism compiler)
  (export compile-library)
  (import (rnrs))

  (define (tag-size) 3)
  
  (define (tag-constant value tag)
    (bitwise-ior (bitwise-arithmetic-shift-left value (tag-size)) tag))
  
  ;; fixnums are 0 so most arithmetic doesn't require shifting
  (define (fixnum-tag) 0)
  ;; constant-tag is used for implementation constants, such as #f, #t and ()
  (define (constant-tag) 1)
  (define (constant-false) (tag-constant 0 (constant-tag)))
  (define (constant-true) (tag-constant 1 (constant-tag)))

  (define (pair-tag) 2)

  (define (allocation-pointer) 0)
  (define (word-size) 4)
  
  ;; ====================== ;;
  ;; Helpers, etc.          ;;
  ;; ====================== ;;

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
    `((define (cons a d)
	(init-pair (%alloc ,(pair-tag) 2) a d))
      (define (init-pair p a d)
	(set-car! p a)
	(set-cdr! p d)
	p)))

  ;; ====================== ;;
  ;; Parsing                ;;
  ;; ====================== ;;

  (define (parse-exprs exprs)
    (if (null? exprs)
	'()
	(cons (parse-expr (car exprs)) (parse-exprs (cdr exprs)))))
  (define (parse-expr expr)
    (cond
     ((number? expr)
      (list 'number expr))
     ((boolean? expr) (list 'bool expr))
     ((symbol? expr)
      (list 'var expr))
     ((pair? expr)
      (let ((op (car expr)))
	(cond
	 ((eq? op 'if)
	  (let ((t (cadr expr))
		(c (caddr expr))
		(a (cadddr expr)))
	    (list 'if (parse-pred t) (parse-expr c) (parse-expr a))))
	 ((or (eq? op 'set-car!) (eq? op 'set-cdr!))
	  (let ((p (parse-expr (cadr expr)))
		(x (parse-expr (caddr expr))))
	    (list op p x)))
	 ((eq? op '%alloc)
	  (list '%alloc (parse-expr (cadr expr)) (parse-expr (caddr expr))))
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
	   ((eq? op 'zero?)
	    (list 'zero? (parse-expr (cadr expr))))
	   (else
	    (display expr) (newline)
	    (error 'parse-pred "Unrecognized predicate")))))
     (else
      (display expr) (newline)
      (error 'parse-pred "Unrecognized predicate"))))
	  
  (define (parse-body* body*)
    (if (null? body*)
	'()
	(cons (parse-expr (car body*)) (parse-body* (cdr body*)))))
  (define (parse-body body)
    ;; expr ... -> (begin expr ...)
    (cons 'begin (parse-body* body)))
  
  (define (parse-function function)
    (let ((name (caadr function))
	  (args (cdadr function))
	  (body (parse-body (cddr function))))
      (list (cons name args) body)))

  (define (parse-functions functions)
    (if (null? functions)
	'()
	(cons (parse-function (car functions)) (parse-functions (cdr functions)))))
  
  (define (parse-library lib)
    ;; For now just assume it's correctly formed. We can do error checking later.
    (let ((body (cddr lib))) ;; skip the library and name
      (let ((exports (cdar body)) ;; names of the functions exported
	    (functions (parse-functions (append (cddr body) (primitives)))))
	(cons exports functions))))

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
    (let ((def (car fn))
	  (body (cadr fn)))
      (list def (apply-representation-expr body))))
  (define (apply-representation-expr expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'bool)
	(list 'ptr (if (cadr expr)
		       (constant-true)
		       (constant-false))))
       ((eq? tag 'begin)
	(cons 'begin (apply-representation-expr* (cdr expr))))
       ((eq? tag 'var) expr)
       ((eq? tag 'number) expr)
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
       ((or (eq? tag 'set-car!) (eq? tag 'set-cdr!))
	(let ((p (apply-representation-expr (cadr expr)))
	      (x (apply-representation-expr (caddr expr))))
	  (list tag p x)))
       ((eq? tag '%alloc)
	(let ((t (apply-representation-expr (cadr expr)))
	      (l (apply-representation-expr (caddr expr))))
	  (list tag t l)))
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
       ((eq? tag 'bool) pred)
       (else
	(display pred) (newline)
	(error 'apply-representation-pred "Unrecognized pred")))))
	   
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
       ((eq? tag 'var) (list 'get-local (index-of (cadr expr) env)))
       ((eq? tag 'call) (cons 'call (cons (cadr expr) (compile-exprs (cddr expr) env))))
       ((eq? tag 'if)
	(let ((t (cadr expr))
	      (c (caddr expr))
	      (a (cadddr expr)))
	  (list 'if (compile-pred t env) (compile-expr c env) (compile-expr a env))))
       ((eq? tag 'set-car!)
	(let ((p (compile-expr (cadr expr) env))
	      (x (compile-expr (caddr expr) env)))
	  `(i32.store (offset 0) (i32.and ,p (i32.const -8)) ,x)))
       ((eq? tag 'set-cdr!)
	(let ((p (compile-expr (cadr expr) env))
	      (x (compile-expr (caddr expr) env)))
	  `(i32.store (offset ,(word-size)) (i32.and ,p (i32.const -8)) ,x)))
       ((eq? tag '%alloc)
	(let ((tag (compile-expr (cadr expr) env))
	      (len (compile-expr (caddr expr) env))) ;; length is in words (i.e. 32-bit)
	  ;; We have an unstate assumption that we always allocate at least 8 bytes.
	  `(begin
	     (i32.load (offset 0) (i32.const ,(allocation-pointer)))
	     (i32.store (offset 0)
			(i32.const ,(allocation-pointer))
			(i32.add (i32.load (offset 0) (i32.const ,(allocation-pointer)))
				 (i32.mul (i32.const ,(word-size)) ,len)))
	     (i32.or ,tag)
	     ;; Add one word to save room for the allocation pointer
	     (i32.add (i32.const ,(word-size))))))
       (else (display expr) (newline) (error 'compile-expr "Unrecognized expression")))))
  (define (compile-pred expr env)
    (let ((op (car expr)))
      (cond
       ((eq? op 'zero?)
	(list 'i32.eqz (compile-expr (cadr expr) env)))
       ((eq? op 'bool)
       	;; Since we're in pred context, bools get translated into
	;; things that go directly into if.
	(if (cadr expr)
	    (list 'i32.const 1)
	    (list 'i32.const 0)))
       (else
	(display expr) (newline)
	(error 'compile-pred "Unrecognized predicate")))))
  
  (define (compile-function fn)
    (let ((args (cdar fn))) ;; basically just a list of the arguments
      (let ((body (compile-expr (cadr fn) args)))
	(list '() body)))) ;; the empty list holds the types of the locals

  (define (function->type fn)
    ;; Functions are assumed to always return an i32 and take some number of i32s as inputs
    (let ((args (args->types (cdar fn))))
      (cons 'fn (list args '(i32)))))
  (define (functions->types fns)
    (if (null? fns)
	'()
	(let ((type (function->type (car fns))))
	  (cons type (functions->types (cdr fns))))))

  (define (function->name fn)
    (caar fn))
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
	(let ((name (caaar functions)))
	  (let ((exports (replace-export exports name index)))
	    (build-exports exports (cdr functions) (+ 1 index))))))

  (define (number-list ls i)
    (if (null? ls)
	'()
	(cons i (number-list (cdr ls) (+ 1 i)))))

  (define (wasm-binop? op)
    (or (eq? op 'i32.and)
	(eq? op 'i32.add)
	(eq? op 'i32.mul)
	(eq? op 'i32.or)))
  
  (define (resolve-calls-exprs exprs env)
    (if (null? exprs)
	'()
	(cons (resolve-calls-expr (car exprs) env) (resolve-calls-exprs (cdr exprs) env))))
  (define (resolve-calls-expr expr env)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'i32.const) expr)
       ((eq? tag 'get-local) expr)
       ((eq? tag 'begin) (cons 'begin (resolve-calls-exprs (cdr expr) env)))
       ((eq? tag 'call) (cons 'call (cons (index-of (cadr expr) env)
					  (resolve-calls-exprs (cddr expr) env))))
       ((eq? tag 'if)
	;; TODO: support calls in the predicate
	(let ((t (cadr expr))
	      (c (caddr expr))
	      (a (cadddr expr)))
	  (list 'if t (resolve-calls-expr c env) (resolve-calls-expr a env))))
       ((eq? tag 'i32.store)
	(let ((offset (cadr expr))
	      (index (resolve-calls-expr (caddr expr) env))
	      (value (resolve-calls-expr (cadddr expr) env)))
	  (list 'i32.store offset index value)))
       ((eq? tag 'i32.load)
	(let ((offset (cadr expr))
	      (index (resolve-calls-expr (caddr expr) env)))
	  (list 'i32.load offset index)))
       ((wasm-binop? tag)
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
	(cons (resolve-calls-fn (car functions) env) (resolve-calls (cdr functions) env))))
  
  ;; ====================== ;;
  ;; Wasm Binary Generation ;;
  ;; ====================== ;;
  
  (define (number->leb-u8-list n)
    (if (and (< n #x80) (> n (- #x80)))
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

  (define (encode-binop op expr)
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
       ((eq? tag 'get-local)
	(cons #x20 (number->leb-u8-list (cadr expr))))
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
	(encode-binop #x6a expr))
       ((eq? tag 'i32.mul)
	(encode-binop #x6c expr))
       ((eq? tag 'i32.and)
	(encode-binop #x71 expr))
       ((eq? tag 'i32.or)
	(encode-binop #x72 expr))
       (else
	(display expr) (newline)
	(error 'encode-expr "Unrecognized expr")))))
  
  (define (encode-code locals body)
    (let ((contents (append (encode-type-vec locals)
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
    (let ((parsed-lib (parse-library library))) ;; (parsed-lib : (exports . functions)
      (let ((exports (car parsed-lib))
	    (types (functions->types (cdr parsed-lib)))
	    (function-names (functions->names (cdr parsed-lib))))
	(let ((exports (build-exports exports (cdr parsed-lib) 0))
	      (functions (resolve-calls
			  (compile-functions
			   (apply-representation
			    (cdr parsed-lib)))
			  function-names)))
	  (let ((module (append (wasm-header)
				(wasm-type-section types)
				(wasm-function-section (number-list functions 0))
				(wasm-memory-section)
				(wasm-export-section exports)
				(wasm-code-section functions))))
	    (u8-list->bytevector module)))))))
