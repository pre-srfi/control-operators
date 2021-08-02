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

(library (control-operators threading)
  (export %current-dynamic-environment
	  %thread? %thread-start! %thread-yield! %thread-terminate!
	  %thread-join!
	  make-%mutex %mutex-lock! %mutex-unlock!
	  %run)
  (import (rnrs (6))
	  (control-operators primitives)
	  (control-operators schedule))

  (define *exit-continuation*)
  (define *lock* #f)

  (define lock!
    (lambda ()
      (assert (not (locked?)))
      (set! *lock* #t)))

  (define unlock!
    (lambda ()
      (assert (locked?))
      (set! *lock* #f)))

  (define locked?
    (lambda ()
      *lock*))

  (define-record-type %mutex
    (nongenerative) (sealed #t) (opaque #t)
    (fields (mutable locked?))
    (protocol
     (lambda (p)
       (lambda ()
	 (p #f)))))

  (define %mutex-unlock!
    (lambda (mtx)
      (assert (%mutex? mtx))
      (%mutex-locked?-set! mtx #f)))

  (define %mutex-lock!
    (lambda (mtx)
      (assert (%mutex? mtx))
      (let f ()
	(lock!)
	(if (%mutex-locked? mtx)
	    (begin
	      (unlock!)
	      (%thread-yield!)
	      (f))
	    (begin
	      (%mutex-locked?-set! mtx #t)
	      (unlock!))))))

  (define interrupt-handler
    (lambda ()
      (%schedule interrupt-handler)
      (unless (locked?)
	(%thread-yield!))))

  (define-record-type %thread
    (nongenerative) (opaque #t)
    (fields (mutable continuation)
	    (mutable terminated?))
    (protocol
     (lambda (p)
       (lambda (thunk)
	 (assert (procedure? thunk))
	 (p thunk #f)))))

  (define-record-type %primordial-thread
    (parent %thread)
    (nongenerative) (sealed #t) (opaque #t)
    (protocol
     (lambda (n)
       (lambda ()
	 ((n (lambda () (assert #f))))))))

  (define current-threads
    (let ([threads '()])
      (case-lambda
	[() threads]
	[(t) (set! threads t)])))

  (define %current-dynamic-environment
    (let ([environment #f])
      (case-lambda
	[() environment]
	[(env) (set! environment env)])))

  (define %run
    (lambda (thunk)
      (lock!)
      (%schedule interrupt-handler)
      (let-values
	  ([val*
	    (%call-with-current-continuation
	     (lambda (k)
	       (set! *exit-continuation* k)
	       (current-threads
		(list
		 (make-%primordial-thread)))
	       (unlock!)
	       (let-values ([val* (thunk)])
		 (lock!)
		 (apply values val*))))])
	(%schedule #f)
	(current-threads '())
	(unlock!)
	(apply values val*))))

  (define %thread-start!
    (lambda (thunk)
      (assert (procedure? thunk))
      (lock!)
      (letrec ([thread
		(make-%thread
		 (lambda ()
		   (unlock!)
		   (thunk)
		   (%thread-terminate! thread)))])
	(current-threads (append (current-threads) (list thread)))
	(unlock!)
	thread)))

  (define %thread-yield!
    (lambda ()
      (lock!)
      (let* ([t* (current-threads)]
	     [ot (car t*)]
	     [t* (append (cdr t*) (list ot))]
	     [nt (car t*)])
	(if (eq? ot nt)
	    (unlock!)
	    (let ([env (%current-dynamic-environment)])
	      (%call-with-current-continuation
	       (lambda (k)
		 (%thread-continuation-set! ot k)
		 ((%thread-continuation nt))))
	      (%current-dynamic-environment env)
	      (unlock!))))))

  (define %thread-terminate!
    (lambda (thread)
      (lock!)
      (when (%primordial-thread? thread)
	(*exit-continuation*))
      (%thread-terminated?-set! thread #t)
      (let ([t* (remq thread (current-threads))])
       	(current-threads t*)
	((%thread-continuation (car t*))))))

  (define %thread-join!
    (lambda (thread)
      (do ()
	  ((%thread-terminated? thread))
	(%thread-yield!))))
  )

;; Local Variables:
;; mode: scheme
;; End:
