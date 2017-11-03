(library (schism compiler)
  (export compile-library)
  (import (rnrs))

  (define (index-of-helper x ls index)
    (if (eq? x (car ls))
	index
	(index-of-helper x (cdr ls) (+ 1 index))))
  (define (index-of x ls)
    (index-of-helper x ls 0))
  
  (define (parse-expr expr)
    (cond
     ((number? expr)
      (list 'number expr))
     ((pair? expr)
      (let ((op (car expr)))
	(cond
	 (else
	  ;; this is a function call
	  (cons 'call expr)))))
     (else
      (display expr) (newline)
      (error 'parse-expr "Unrecognized expression"))))
  
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
	    (functions (parse-functions (cddr body))))
	(cons exports functions))))

  (define (args->types args)
    (if (null? args)
	'()
	(cons 'i32 (args->types (cdr args)))))

  (define (compile-exprs exprs env)
    (if (null? exprs)
	'()
	(cons (compile-expr (car exprs) env) (compile-exprs (cdr exprs) env))))
  (define (compile-expr expr env)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'begin) (cons 'begin (compile-exprs (cdr expr) env)))
       ((eq? tag 'number) (cons 'i32.const (cdr expr)))
       ((eq? tag 'call) (cons 'call (cons (cadr expr) (compile-exprs (cddr expr) env))))
       (else (display expr) (newline) (error 'compile-expr "Unrecognized expression")))))
  
  (define (compile-function fn)
    (let ((args (cdadr fn))) ;; basically just a list of the arguments
      (let ((body (compile-expr (cadr fn) args)))
	(list '() body)))) ;; the empty list holds the types of the locals

  (define (function->type fn)
    ;; Functions are assumed to always return an i32 and take some number of i32s as inputs
    (let ((args (args->types (cddadr fn))))
      (cons 'fn (list args '(i32)))))
  (define (functions->types fns)
    (if (null? fns)
	'()
	(cons (function->type (car fns)) (functions->types (cdr fns)))))

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

  (define (resolve-calls-exprs exprs env)
    (if (null? exprs)
	'()
	(cons (resolve-calls-expr (car exprs) env) (resolve-calls-exprs (cdr exprs) env))))
  (define (resolve-calls-expr expr env)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'i32.const) expr)
       ((eq? tag 'begin) (cons 'begin (resolve-calls-exprs (cdr expr) env)))
       ((eq? tag 'call) (cons 'call (cons (index-of (cadr expr) env) (cddr expr))))
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
	(list n)
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
  
  (define (encode-expr expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'begin)
	(encode-exprs (cdr expr)))
       ((eq? tag 'i32.const)
	(cons #x41 (number->leb-u8-list (cadr expr))))
       ((eq? tag 'call)
	(append (encode-exprs (cddr expr))
		(cons #x10 (number->leb-u8-list (cadr expr)))))
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
  
  ;; Takes a library and returns a bytevector of the corresponding Wasm module
  ;; bytes
  (define (compile-library library)
    (let ((parsed-lib (parse-library library))) ;; (parsed-lib : (exports . functions)
      (let ((exports (car parsed-lib))
	    (types (functions->types (cdr parsed-lib)))
	    (function-names (functions->names (cdr parsed-lib))))
	(let ((exports (build-exports exports (cdr parsed-lib) 0))
	      (functions (resolve-calls (compile-functions (cdr parsed-lib)) function-names)))
	  (let ((module (append (wasm-header)
				(wasm-type-section types)
				(wasm-function-section (number-list functions 0))
				(wasm-export-section exports)
				(wasm-code-section functions))))
	    (u8-list->bytevector module)))))))
