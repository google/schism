(library (schism compiler)
  (export compile-library)
  (import (rnrs))

  (define (parse-library lib)
    ;; For now just assume it's correctly formed. We can do error checking later.
    (let ((body (cddr lib))) ;; skip the library and name
      (let ((exports (cdar body)) ;; names of the functions exported
	    (functions (cddr body)))
	(cons exports functions))))

  (define (args->types args)
    (if (null? args)
	'()
	(cons 'i32 (cdr args))))

  (define (compile-expr expr env)
    (cond
     ((number? expr) (list 'i32.const expr))))
  
  (define (compile-function fn)
    (let ((args (cdadr fn))) ;; basically just a list of the arguments
      (let ;; for now we assume bodies have a single expression
	  ((body (compile-expr (caddr fn) args)))
	(list '() body)))) ;; the empty list holds the types of the locals

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
	(let ((name (caadar functions)))
	  (let ((exports (replace-export exports name index)))
	    (build-exports exports (cdr functions) (+ 1 index))))))
  
  ;; ====================== ;;
  ;; Wasm Binary Generation
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
      (cons #x60 (append (encode-type-vec (cadr type)) (encode-type-vec (caddr type)))))))
  
  (define (wasm-type-section types)
    ;; We have no types so far.
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
	'(#x0b)
	(append (encode-expr (car exprs)) (encode-exprs (cdr exprs)))))
  
  (define (encode-expr expr)
    (if (pair? expr)
	(cond
	 ((eq? (car expr) 'begin)
	  (encode-exprs (cdr expr)))
	 ((eq? (car expr) 'i32.const)
	  (cons #x41 (number->leb-u8-list (cadr expr)))))))
  
  (define (encode-code locals body)
    (let ((contents (append (encode-type-vec locals)
			    (encode-exprs body))))
      (make-vec (length contents) contents)))

  (define (encode-codes codes)
    (if (null? codes)
	'()
	(append (encode-code (caar codes) (cdar codes)) (encode-codes (cdr codes)))))
  
  (define (wasm-code-section codes)
    (make-section 10 (make-vec (length codes) (encode-codes codes))))
  
  ;; Takes a library and returns a bytevector of the corresponding Wasm module
  ;; bytes
  (define (compile-library library)
    (let ((parsed-lib (parse-library library))) ;; (parsed-lib : (exports . functions)
      (let ((exports (car parsed-lib))
	    (functions (compile-functions (cdr parsed-lib))))
	(let ((exports (build-exports exports (cdr parsed-lib) 0)))
	  (let ((module (append (wasm-header)
				(wasm-type-section '((fn (i32 i32) (i32))))
				(wasm-function-section '(0))
				(wasm-export-section exports)
				(wasm-code-section functions))))
	    (u8-list->bytevector module)))))))
