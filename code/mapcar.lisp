;;; An Inquiry into the Nature and Causes of the MAPCAR Function

(defun normal-mapcar (func list)
  "Normal mapcar function (of a single arg-list)"
  (if (endp list)
      nil
      (cons (funcall func (car list))
	    (normal-mapcar func (cdr list)))))

#+rt
(rt:deftest (normal-mapcar 1)
    (normal-mapcar #'sqrt nil)
  nil)

#+rt
(rt:deftest (normal-mapcar 2)
    (normal-mapcar #'isqrt '(16 9 4 1 0))
  (4 3 2 1 0))


(defun accumulator-mapcar (func args)
  "Accumulator version of mapcar function"
  (labels ((accu-mapcar (a done)
	     (if (endp a)
		 done
		 (accu-mapcar (cdr a)
			      (cons (funcall func (car a)) done)))))
    (nreverse (accu-mapcar args nil))))

#+rt
(rt:deftest (accumulator-mapcar 1)
    (accumulator-mapcar #'sqrt nil)
  nil)

#+rt
(rt:deftest (accumulator-mapcar 2)
    (accumulator-mapcar #'isqrt '(16 9 4 1 0))
  (4 3 2 1 0))


;;; A single step version of accumulator mapcar
;;; The list and accumulator are consed together for convenience
;;; Optionally it could be made to tidy up the input in the very
;;; last step (eg (NIL 0 1 2 3 4) => (4 3 2 1))

(defun mapcar-step (func data)
  "Single step mapcar expecting initial data as a list of a list and will accumulate into the toplevel list"
  ;; functions for readability
  (flet ((data-args (d) (car d))
	 (data-done (d) (cdr d))
	 (make-data (args done) (cons args done)))
    (if (endp (data-args data))
	data
	(make-data (cdr (data-args data))
		   (cons (funcall func (car (data-args data)))
			 (data-done data))))))

#+rt
(rt:deftest (mapcar-step 0)
    (mapcar-step #'isqrt '(nil))
  (nil))

#+rt
(rt:deftest (mapcar-step 1)
    (mapcar-step #'isqrt '((16 9 4 1 0)))
  ((9 4 1 0) 4))

#+rt
(rt:deftest (mapcar-step 2)
    (mapcar-step #'isqrt '((9 4 1 0) 4))
  ((4 1 0) 3 4))

#+rt
(rt:deftest (mapcar-step 3)
    (mapcar-step #'isqrt '((4 1 0) 3 4))
  ((1 0) 2 3 4))

#+rt
(rt:deftest (mapcar-step 4)
    (mapcar-step #'isqrt '((1 0) 2 3 4))
 ((0) 1 2 3 4))

#+rt
(rt:deftest (mapcar-step 5)
    (mapcar-step #'isqrt '((0) 1 2 3 4))
  (nil 0 1 2 3 4))

