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

  ;; id is the number, contents is a list of bytes
  (define (make-section id contents)
    (cons id (append (number->leb-u8-list (length contents)) contents)))

  (define (encode-type-vec-contents types)
    (if (null? types)
	'()
	(append (encode-type (car types)) (encode-type-vec-contents (cdr types)))))

  (define (encode-type-vec types)
    (append (number->leb-u8-list (length types)) (encode-type-vec-contents types)))
  
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
    (make-section 1 (encode-type-vec '((fn (i32 i32) (i32))))))
	  
  ;; Takes a library and returns a bytevector of the corresponding Wasm module
  ;; bytes
  (define (compile-library library)
    ;; For now just return the wasm header, which should be the smallest
    ;; possible module.
    (let ((module (append (wasm-header)
			  (wasm-type-section '()))))
      (u8-list->bytevector module))))
