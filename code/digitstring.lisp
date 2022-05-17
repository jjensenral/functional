;;;; String of digits puzzle
;;;; jjensenral@github.com April 2022
;;;; Example of mutually recursive functions and functional programming:
;;;; A series of talks on functional programming in Lisp(s) for UKRSE #emacs
;;;;
;;;; Place digits 1 .. N into an array of length 2N s.t. every digit
;;;; appears twice and there are k digits between each occurrence of k

;;; Specifically, for N=7, start with 4 at positions 0, 5, and 1 at 11, 13.
;;; (solve '((4 . 0) (1 . 11)) 7)

;;; Small example:
;;; (solve '((1 . 1)) 3)
;;; #(3 1 2 1 3 2)


(defun place (a k i)
  "Return a new array with k placed at positions i and i+k+1 or nil"
  (let ((i1 (+ i k 1)))
    (and (< i1 (length a))
	 (null (aref a i))
	 (null (aref a i1))
	 (let ((a1 (copy-seq a)))
	   (setf (aref a1 i) k
		 (aref a1 i1) k)
	   a1))))

(defun solve-1 (a)
  (let ((k (find-k a)))
    (if k (solve-pos a k)
	(throw 'found a))))		; later in the course we will gather all

;; Using a loop to build the array would have been much more concise...
;; (loop for (pos . val) in start do (setq a (place a val pos)))

(defun solve (start size)
  "Given an alist of (pos . val) of starting positions, create an array for problem size and find the first solution"
  (catch 'found
    (solve-1 (labels ((build-array (a1 todo)
			  (if (endp todo) a1
			      (let ((a2 (place a1 (caar todo) (cdar todo))))
				(unless a2 (error "Cannot place ~D at ~D in ~A" (caar todo) (cdar todo) a1))
				(build-array a2 (cdr todo))))))
	       (build-array (make-array (ash size 1) :initial-element nil) start)))))


(defun find-k (a)
  "Find an element missing from the array, the next candidate for placement"
  (let ((n (ash (length a) -1)))
    (labels ((find-k-1 (k)
	       (if (not (position k a)) k
		   (and (> k 1)
			(find-k-1 (1- k))))))
      (find-k-1 n))))

#+rt
(rt:deftest (find-k 1)
    (find-k #(1 2 3 nil 1 2 3 nil))
  4)

#+rt
(rt:deftest (find-k 2)
    (find-k #(1 2 nil 4 1 2 nil 4))
  3)
#+rt
(rt:deftest (find-k 3)
    (find-k #(1 nil 3 4 1 nil 3 4))
  2)
#+rt
(rt:deftest (find-k 4)
    (find-k #(nil 2 3 4 nil 2 3 4))
  1)
#+rt
(rt:deftest (find-k 5)
    (find-k #(1 2 3 4 1 2 3 4))
  nil)


(defun solve-pos (a k &optional i)
  "Try all valid positions for k and iterate solving remaining positions"
  (if i
      (if (< i (- (length a) k))	; looping for 1 from 0 below 2N-k
	  (let ((a1 (place a k i)))
	    (and a1 (solve-1 a1))
	    (solve-pos a k (1+ i)))
	  nil)
      (solve-pos a k 0)))
