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

(library (control-operators testing)
  (export test)
  (import (except (rnrs (6))
		  call/cc
		  call-with-current-continuation
		  dynamic-wind
		  guard)
	  (control-operators define-who)
	  (control-operators))

  (define-syntax/who test
    (lambda (stx)
      (syntax-case stx ()
	[(_ expected-expr test-expr)
	 #'(test #f expected-expr test-expr)]
	[(_ name expected-expr test-expr)
	 #'(let-values ([expected expected-expr]
			[result (run (lambda () test-expr))])
	     (check expected result))]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define check
    (lambda (expected result)
      (unless (equal? expected result)
	(display "FAIL\n")
	(display "  Expected values:")
	(print-values expected)
	(display "  Actual values:  ")
	(print-values result)
	(exit #f))))

  (define print-values
    (lambda (vals)
      (do ([vals vals (cdr vals)])
	  ((null? vals) (newline))
	(display " ")
	(display (car vals)))))
  )

;; Local Variables:
;; mode: scheme
;; End:
