(library (schism compiler)
  (export compile-library)
  (import (rnrs))

  (define (number->leb-u8-list n)
    (if (and (< n #x80) (> n (- #x80)))
	(list n)
	(cons (bitwise-ior #x80 (bitwise-bit-field n 0 7))
	      (number->leb-u8-list (bitwise-arithmetic-shift-right n 7)))))
  
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
    ;; For now just return the wasm header, which should be the smallest
    ;; possible module.
    (let ((module (append (wasm-header)
			  (wasm-type-section '((fn (i32 i32) (i32))))
			  (wasm-function-section '(0))
			  (wasm-code-section '((() (i32.const 5)))))))
      (u8-list->bytevector module))))
