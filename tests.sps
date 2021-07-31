#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2021).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (except (rnrs (6))
		call-with-current-continuation
		call/cc
		dynamic-wind
		guard)
	(control-operators)
	(control-operators testing))

;;; Helpers

(define call-with-tail-test
  (let ([mark (make-continuation-mark-key 'tail)])
    (lambda (proc)
      (with-continuation-mark mark #t
	(proc (lambda ()
		(call-with-immediate-continuation-mark mark values)))))))

;;; Test Begin

(test-begin "Control Operators")

;;; Evaluation

(test 10 10)

(test (values 1 2) (values 1 2))

;;; Guard

;;; Continuation prompts

(define tag (make-continuation-prompt-tag 'tag))

(test #t (continuation-prompt-tag? tag))

(test 42 (guard (c [(continuation-error? c) 42])
	   (abort-current-continuation tag)))

(test 5 (call-with-continuation-prompt (lambda () 5)
					(make-continuation-prompt-tag)))

(test 1 (+ 1 (abort-current-continuation (default-continuation-prompt-tag)
		(lambda () 1))))

(test 6 (+ 2 (call-with-continuation-prompt
	       (lambda ()
		 (+ 3 (abort-current-continuation (default-continuation-prompt-tag)
			(lambda () 4))))
	       (default-continuation-prompt-tag))))

(test 4 (+ 2 (call-with-continuation-prompt
	       (lambda ()
		 (+ 3 (abort-current-continuation (default-continuation-prompt-tag)
			(lambda () 4))))
	       tag)))

(test 7 (+ 3 (call-with-continuation-prompt
	       (lambda ()
		 (+ 13 (abort-current-continuation tag
			 (lambda () 4))))
	       tag)))

(test 9 (+ 2 (call-with-continuation-prompt
	       (lambda ()
		 (if (call-with-composable-continuation
		      (lambda (proc)
			(abort-current-continuation (default-continuation-prompt-tag)
						    (lambda ()
						      (+ (proc #f) (proc #t))))))
		     3
		     4))
	       (default-continuation-prompt-tag)
	       (lambda (thunk)
		 (thunk)))))

(test 12 (+ 2 (call-with-current-continuation
		(lambda (k)
		  (+ 1 (k 10))))))

(test 15 (+ 2 (call-with-composable-continuation
		(lambda (k)
		  (+ 1 (k 10))))))

(test 13 (+ 3 (call-with-current-continuation
		(lambda (k)
		  (+ 1 (call-in-continuation k (lambda () 10)))))))

(test #t (call-with-current-continuation
	   (lambda (k)
	     (continuation? k))))

(test #t (call-with-composable-continuation
	   (lambda (k)
	     (continuation? k))))

(test #f (continuation-prompt-available? tag))

(test #t (continuation-prompt-available? (default-continuation-prompt-tag)))

(test 103 (call-with-continuation-barrier
	   (lambda ()
	     (call/cc
	      (lambda (k)
		(+ 100 (k 103)))))))

(test 104 (call/cc
	   (lambda (k)
	     (call-with-continuation-barrier
	      (lambda ()
		(+ 100 (k 104)))))))

(test 'mark1 (with-continuation-mark 'key 'mark1
		(call-with-immediate-continuation-mark 'key values)))

(test 'mark2 (with-continuation-mark 'key 'mark1
		(with-continuation-mark 'key 'mark2
		  (call-with-immediate-continuation-mark 'key values))))

(test '(#f) (with-continuation-mark 'key 'mark1
	       (list
		(call-with-immediate-continuation-mark 'key values))))

(test '((mark1) (mark2))
       (with-continuation-mark 'key1 'mark1
	 (with-continuation-mark 'key2 'mark2
	   (list
	    (continuation-mark-set->list #f 'key1)
	    (continuation-mark-set->list #f 'key2)))))

(test '(1)
      (let f ([n 10])
	(if (fxzero? n)
	    (continuation-mark-set->list #f 'key)
	    (with-continuation-mark 'key n
	      (f (fx- n 1))))))

(test '(mark2)
      (with-continuation-mark 'key 'mark1
	(call-with-continuation-prompt
	 (lambda ()
	   (with-continuation-mark 'key 'mark2
	     (continuation-mark-set->list #f 'key))))))

(test '(mark2)
      (with-continuation-mark 'key 'mark1
	(list
	 (with-continuation-mark 'key 'mark2
	   (continuation-mark-set-first #f 'key)))))

(test '(((#(#f mark2) #(mark1 mark2))))
      (with-continuation-mark 'key1 'mark1
	(with-continuation-mark 'key2 'mark2
	  (list
	   (with-continuation-mark 'key3 'mark3
	     (list
	      (with-continuation-mark 'key2 'mark2
		(continuation-mark-set->list* #f '(key1 key2)))))))))

;;; Dynamic-wind

(test '(11 111) (let ([x 0])
		  (list
		   (dynamic-wind
		       (lambda () (set! x (fx+ x 1)))
		       (lambda () (set! x (fx+ x 10)) x)
		       (lambda () (set! x (fx+ x 100))))
		   x)))

(test "in pre out in post out "
      (let-values ([(p get) (open-string-output-port)])
	(let ([v (call/cc
		  (lambda (out)
		    (dynamic-wind
			(lambda () (put-string p "in "))
			(lambda ()
			  (put-string p "pre ")
			  (put-string p (call/cc out))
			  #f)
			(lambda () (put-string p "out ")))))])
	  (when v (v "post ")))
	(get)))

(test "in out"
      (let-values ([(p get) (open-string-output-port)])
	(dynamic-wind
	    (lambda () (put-string p "in "))
	    (lambda () (abort-current-continuation (default-continuation-prompt-tag)
			 (lambda ()
			   (get))))
	    (lambda () (put-string p "out")))))

;;; Parameters

(define param (make-parameter 10 (lambda (x) (* x x))))

(test 100 (param))

(param 12)

(test 144 (param))

(test 169 (parameterize ([param 13]) (param)))

(test 144 (param))

(test 64 (parameterize ([param 13]) (param 8) (param)))

(test 144 (param))

(test #t (call-with-tail-test
	  (lambda (tail?)
	    (parameterize ([param 13])
	      (tail?)))))

;;; Test End

(test-end)

;; Local Variables:
;; mode: scheme
;; End:
