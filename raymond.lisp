;;; (load (compile-file "raymond.lisp"))

(defparameter *situs*
  '((:a ("Jeff" . :knave) ("Mark" . :knight))))

(defun answer-type-1 (askee question situ)
  (let ((askee-type (mytype askee situ))
	(other-type (other-type askee situ)))
    (case askee-type
      (:knight
       (case (argtype 0 question)
	 (:knight :yes)
	 (:knave :no)
	 ))
      (:knave
       (case (argtype 0 question)
	 (:knight :yes)
	 (:knave :no))))))

(defun answer-type-2 (askee question situ)
  (let ((askee-type (mytype askee situ))
	(other-type (other-type askee situ)))
    (case askee-type
      (:knight
       (case (argtype 0 question)
	 (:truthful :yes)
	 (:lie :no)
	 ))
      (:knave
       (case (argtype 0 question)
	 (:truthful :yes)
	 (:lie :no))))))

(defun answer-type-3 (askee question situ)
  (let ((askee-type (mytype askee situ))
	(other-type (other-type askee situ)))
    (case askee-type
      (:knight
       (case (argtype 0 question)
	 (:knight
	  (case other-type
	    (:knave :no)
	    (:knight :yes)))
	 (:knave
	  (case other-type
	    (:knave :yes)
	    (:knight :no)))
	 ))
      (:knave 
       (case (argtype 0 question)
	 (:knight
	  (case other-type
	    (:knave :yes)
	    (:knight :no)))
	 (:knave
	  (case other-type
	    (:knave :no)
	    (:knight :yes)))
	 )))))

(defun answer-type-4 (askee question situ)
  "Are you both of the same kind?"
  (let ((askee-type (mytype askee situ))
	(other-type (other-type askee situ)))
    (case askee-type
      (:knight (if (eq other-type askee-type) :yes :no))
      (:knave  (if (eq other-type askee-type) :no :yes)))))

(defun answer-type-5 (askee question situ)
  "Are you both (Knights/Knaves)?"
  (let ((askee-type (mytype askee situ))
	(other-type (other-type askee situ)))
    (case (argtype 0 question)
      (:knights (if (and (eq askee-type :knight) (eq other-type :knight)) :yes :no))
      (:knaves  (if (and (eq askee-type :knave) (eq other-type :knave)) :yes :no)))
    ))

(defun answer-type-6 (askee question situ)
  "What type of person will your friend say you are?"
  (let ((askee-type (mytype askee situ))
	(other-type (other-type askee situ)))
    (case askee-type
      (:knight
       (case other-type
	 (:knight :knight)
	 (:knave :knight)
	 ))
      (:knave
       (case other-type
	 (:knight :knave)
	 (:knave :knave)))) ;; This is a double lie! 
    ))

(defun mytype (askee situ)
  (loop for (name . type) in situ
	when (string-equal askee name)
	do (return type)))
(defun other-type (askee situ)
  (loop for (name . type) in situ
	when (not (string-equal askee name))
	do (return type)))

(defvar *query->model* (make-hash-table :test #'equal))

(defun argtype (n question)
  (cdr (nth n (cdr question))))

(defparameter *question-templates* 
  `(("are you a (knight/knave)?" . ,#'answer-type-1)
    ("do you (tell the truth/lie)?" . ,#'answer-type-2)
    ("is your friend a (knight/knave)?" . ,#'answer-type-3)
    ("are you both of the same kind?" . ,#'answer-type-4)
    ("are you both (knights/knaves)?" . ,#'answer-type-5)
    ("what type of person will your friend say you are?" . ,#'answer-type-6)
    ))

(defun ask (askee question situ)
  (let* ((askee-type (mytype askee situ))
	 (key (list askee-type question situ))
	 (answer (gethash key *query->model*)))
    ;;(format t "Asking: ~s ~s~%" askee question)
    ;;(format t " ~a [Key is: ~s]~%" answer key)
    answer))

(defparameter *keyword-simplifiers*
  '((:|TELLING THE TRUTH| . :TRUTHFUL)
    (:|TELL THE TRUTH| . :TRUTHFUL)
    ))

(defun instantiate-questions (situ-key)
  (clrhash *query->model*)
  (let ((situ (cdr (assoc situ-key *situs*))))
    (loop for (question-template . logic-function) in *question-templates*
	  do (loop for question in (loop for (a b) on (expand-question-templates question-template) by #'cddr
					     collect (cons a b))
		       do
		   (loop for (person . type) in situ
			 do (instantiate-question logic-function person type question situ))))))

(defun instantiate-question (logical-function askee askee-type question situ)
  (setf (gethash (list askee-type (string-downcase (car question)) situ) *query->model*)
	(funcall logical-function askee question situ)))

(defun expand-question-templates (qt &optional alts)
  (let* ((alt (find-alt qt))) ;; e.g., ("(Jeff/Mark)" "Jeff" "Mark"), or nil is there are no model models
    (cond ((null alt) (list qt (reverse alts)))
	  (t (loop for option in (cdr alt)
		   with alt = (car alt)
		   append (expand-question-templates
			    (replace-string-once qt alt option)
			    (cons (cons alt (keywordize option)) alts)))))))

(defun keywordize (symbol)
  (let ((k (sub-keywordize symbol)))
    (or (cdr (assoc k *keyword-simplifiers*)) k)))

(defun sub-keywordize (symbol &optional (case :upper))
  "Return a symbol in the KEYWORD package with the same name as SYMBOL (modified appropriately for CASE).  SYMBOL may also be a string."
  (let ((p (find-package :keyword)))
    (flet ((doit 
	    (string)
	    (intern
	     (ecase case
		    (:upper (string-upcase string))
		    (:lower (string-downcase string))
		    ((:preserve nil) string))
	     p)))
	  (cond
	   ((stringp symbol) (doit symbol))
	   ((symbolp symbol) (doit (symbol-name symbol)))
	   (t (error "Invalid argument to KEYWORDIZE: ~A" symbol))
	   ))))

(defun find-alt (qt)
  (let ((p/ (position #\/ qt)))
    (when p/
      (let* ((lparen (position #\( qt))
	     (rparen (position #\) qt)))
	`(,(subseq qt lparen (1+ rparen))
	   ,(subseq qt (1+ lparen) p/)
	   ,(subseq qt (1+ p/) rparen))))))
	   
(defun replace-string-once (string target replacement)
  (let ((p (search target string :test #'char-equal)))
    (concatenate 'string
		 (subseq string 0 p)
		 replacement
		 (subseq string (+ p (length target))))))

(defun string-split (string &key (delimiter #\space) (convert-num-values? nil))
  "Split string into substrings delimited by delimiter"
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring 
	    (i)
	    (push (subseq string last i)
		  substrings)))
	  (dotimes (i length)
	    (when (eq (char string i) delimiter)
	      (add-substring i)
	      (setq last (1+ i))))
	  (add-substring length)
	  (let ((substrings (nreverse substrings)))
	    (if convert-num-values?
		(loop for string in substrings
		      as v = (ignore-errors (read-from-string string))
		      if (numberp v)
		      collect v
		      else 
		      collect string)
	      substrings)))))

(defun talk (&optional (situ-key :a))
  (let* ((situ (cdr (assoc situ-key *situs*)))
	 (p1 (caar situ))
	 (p2 (caadr situ)))
    (format t "Welcome to the island of Knights and Knaves. We're ~a and ~a.
You can ask us anything, but be forewarned,
one of us always lies, and the other always tells the truth.
Before you can depend on us for anything else,
it would be wise to try to figure out which one of us is which!
Can you tell which of us is which?
To ask one of us a question start with @~a:... or @~a:...~%"
	    p1 p2 p1 p2)
    (loop while t
	  do (prin1 '@) (force-output)
	  (let* ((input (read-line t nil nil))
		 (cpos (position #\: input))
		 (askee (string-downcase (when cpos (subseq input 0 cpos))))
		 (question (string-downcase (when cpos (subseq input (1+ cpos))))))
	    (if (null cpos)
		(format t "To ask one of us a question start with @~a:... or @~a:...
  (Don't forget the colon! :-)~%" p1 p2)
		(if (not (assoc askee situ :test #'string-equal))
		    (progn 
		      (format t "~a? ~a's not here!~%" askee askee)
		      (format t "To ask one of us a question start with @~a:... or @~a:...
  (Don't forget the colon! :-)~%" p1 p2))
		    (case (ask askee question situ)
		      (:yes (format t "~a says 'Yes'~%" askee))
		      (:no (format t "~a says 'No'~%" askee))
		      (:knight (format t "~a says 'A Knight''~%" askee))
		      (:knave (format t "~a says 'A Knave''~%" askee))
		      (t (format t "~a says that they can't answer that question.
Here are the questions they know how to answer:~%" askee)
			 (loop for model being the hash-keys of *query->model*
			       as sentence = (second model)
			       with already-shown = nil
			       do
			       (unless (member sentence already-shown :test #'string-equal)
				 (format t "  ~a~%" sentence)
				 (push sentence already-shown)))))))))))
	  
(instantiate-questions :a)
(talk)
