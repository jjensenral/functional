;;; Code supporting the author's talks on functional programming in
;;; ELisp given to UKRSE Emacs channel spring 2022
;;;
;;; Looking up month numbers using different lookup methods
;;;
;;; 19 Mar 2022

(defmacro exec-time (n expr)
  "Macro for executing sexp expr n times, returning timing info"
  (let ((t1 (gensym))
	(t2 (gensym)))
    `(let ((,t1 (current-time)))
       (loop repeat ,n do ,expr)
       (let* ((,t2 (current-time)))
	 (time-subtract ,t2 ,t1)))))


(defconst +months-string+ '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
			    ("Apr" . 4) ("May" . 5) ("Jun" . 6)
			    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
			    ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  "alist mapping month three letter strings to month number")

(defconst +months-symbol+ '((jan . 1) (feb . 2) (mar . 3)
			    (apr . 4) (may . 5) (jun . 6)
			    (jul . 7) (aug . 8) (sep . 9)
			    (oct . 10) (nov . 11) (dec . 12))
  "alist mapping month three letter symbols to month number")


(defun month-number1 (m)
  (declare (type string m))
  (1+ (floor (search m "JanFebMarAprMayJunJulAugSepOctNovDec") 3)))

(defun month-number2 (m)
  (declare (type string m))
  ;; ELisp uses equal for equality in assoc
  (cdr (assoc m +months-string+)))


;; This does not work with load as Emacs doesn't seem to create the
;; closure correctly
;; (let ((months (make-hash-table :test #'equal :size 12))
;;       (setup nil))
;;   (defun month-number3 (m)
;;     (declare (type string m))
;;     (unless setup
;;       ;; Populate hash table on first run
;;       (loop for (mm . dd) in +months-string+
;; 	    do (puthash mm dd months))
;;       (setq setup t))
;;     (gethash m months)))

(defvar *month-hash* (make-hash-table :test #'equal :size 12))
(defvar *month-hash-setup* nil)

(defun month-number3 (m)
  (declare (type string m))
  (unless *month-hash-setup*
    ;; Populate hash table on first run
    (loop for (mm . dd) in +months-string+
	  do (puthash mm dd *month-hash*))
    (setq *month-hash-setup* t))
  (gethash m *month-hash*))


(defun month-number4 (m)
  (declare (type string m))
  (1+ (position m ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"] :test #'equal)))


;; Symbol version
(defun month-number-sym1 (m)
  (declare (type symbol m))
  (cdr (assoc m +months-symbol+)))


(defun month-number-sym2 (m)
  (declare (type symbol m))
  (1+ (position m [jan feb mar apr may jun jul aug sep oct nov dec])))


(defun test-month-number (func alist)
  (dolist (m alist)
    (unless (eql (funcall func (car m)) (cdr m))
      (error "Failed %s at %s" func m)))
  t)


(defun test-all (k)
  "run all test functions k times, returning timing data"
  ;; ignore result, keep only timing
  (mapcar (lambda (pair)
	    (let ((y (exec-time k (test-month-number (car pair) (cdr pair)))))
	      (+ (ash (first y) 16) (second y) (/ (third y) 1.0e6) (/ (fourth y) 1.0e12))))
	  (list (cons #'month-number1 +months-string+)
		(cons #'month-number2 +months-string+)
		(cons #'month-number3 +months-string+)
		(cons #'month-number4 +months-string+)
		(cons #'month-number-sym1 +months-symbol+)
		(cons #'month-number-sym2 +months-symbol+))))
