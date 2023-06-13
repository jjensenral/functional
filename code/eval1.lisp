;;;; (E)LISP interpreter/evaluation
;;;; This code is formally Common Lisp but is close enough to Emacs Lisp
;;;; that it should work fairly outoftheboxedly


;;; State of Lisp can be held in Lisp itself or in variables in Lisp.
;;; We choose the latter: state is held in a variable


(defstruct state
  (vars nil :type list)
  (stack nil :type list))


(defstruct err
  type
  data)


(defun eval-step (state prog)
  "Evaluate one step of prog according to state, updating state"
  (cond
    ((atom prog) (eval-atom state prog))
    ;; If it is not an atom, it is a list (yes sequences are atoms...)
    ((eq (car prog) 'quote) prog)
    (t (eval-func state (car prog) (cdr prog)))))


(defun eval-atom (state prog)
  "Evaluate an atom according to state"
  (etypecase prog
    ;; Things that evaluate to themselves
    ((or string number keyword) prog)
    ;; If it's a symbol, it's a variable (in our limited evaluator)
    (symbol (let* ((cell (assoc prog (gethash 'vars state)))
		   (val (cdr cell)))
	      (if cell val (make-err :type 'undef-var :data prog))))))


(defun eval-func (state func args)
  "Evaluate a function call"
  ;; This function must take a /single/ step in evaluation, so we
  ;; need to check if we have any non-atomic arguments and evaluate
  ;; those first
  (let ((args1 (eval-args state args)))
    (if args1
	(cons func args1)
	(eval-func-really state func args))))


(defun eval-args (state args)
  "Evaluate one of the arguments, returning updated list"
  (cond
    ((endp args) nil)
    ((
