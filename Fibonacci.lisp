;;;; Fun with math!
;;;;
;;;; Mark Peever
;;;; 2019-09-12
;;;;

(require :alexandria)

(defun interval (start end)
  "Print the integers on [start, end]."
  (loop for i from start to end collecting i))

(defun fib (n)
  "Calculate the Nth Fibonacci number."
  (cond ((eql n 0) 1)
	((eql n 1) 1)
	(T (+ (fib (- n 1))
	      (fib (- n 2))))))

(defun fib/tail (n)
  "Calculate the Nth Fibonacci number, with tail recursion."
  (labels ((fseq (m acc)
	     ;; Calculate the Fibonacci sequence to M places.
	     ;; Adding to the front is easier, so the list is REVERSED.
	     (cond ((eql m 0) acc)
		   (T (fseq (- m 1) (cons (+ (first acc)
					     (second acc)) acc))))))
    (cond ((eql n 0) 1)
	  ((eql n 1) 1)
	  ;; Fence-post alert! n + 1 - 2 => n - 1
	  (T (first (fseq (- n 1) '(1 1)))))))
		   

(defmacro with-memo ((memo key) &body body)
  "Wrap a BODY in a memoized code block."
  (alexandria:with-gensyms (memo-name key-name)
    `(let ((,memo-name ,memo)
	   (,key-name ,key))
       (if (gethash ,key-name ,memo-name)
	   (gethash ,key-name ,memo-name)
	   (values (setf (gethash ,key-name ,memo-name)
			 ,@body)
		   nil)))))

(defmacro defun/memo (name (&rest args) &body body)
  "Generate a memoized function."
  (alexandria:with-gensyms (memo-name)
    `(let ((,memo-name (make-hash-table :test 'equalp)))
       (defun ,name (,@args)
	 (with-memo (,memo-name (list ,@args))
	   (progn
	     ,@body))))))

(defun/memo fib/memo (n)
  "Generate the Nth Fibonacci number."
  (cond ((eql 0 n) 1)
	((eql 1 n) 1)
	(T (+ (fib/memo (- n 1))
	      (fib/memo (- n 2))))))
