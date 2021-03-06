;; -*- mode: common-lisp; package: system -*-
;; defdecl.cl
;; declarations for portable environments implementation
;;
;; See the file LICENSE for the full license governing this code.

(in-package :system)

#+allegro
(progn
  (provide :defdecl)
  (setq *modules* (delete (string :compdecl) *modules* :test #'equal)))

;; This file houses the define-declarations for the portable
;; environments implementation.  It is separate from environ.cl
;; to ensure modularity; If environ.cl is loaded into a lisp first,
;; followed by this file, followed by an implementation's compiler
;; which might redefine some of the declarations in this file, it
;; still allows environ.cl to be re-loaded for any changes without
;; reverting the declarations back to the pre-compiler versions.

#+clisp
(progn
  (eval-when (:compile-toplevel)
    (defparameter cl-user::.saved-p-lock.
	(remove-if-not #'package-lock *system-package-list*))
    (setf (package-lock cl-user::.saved-p-lock.)  nil))
  (eval-when (:load-toplevel :execute)
    (defparameter cl-user::.saved-p-lock.
	(remove-if-not #'package-lock *system-package-list*))
    (setf (package-lock cl-user::.saved-p-lock.) nil)))

(#+allegro excl::without-package-locks
 #+cmu extensions:without-package-locks
 #-(or allegro cmu) progn

;; The optimize declaration

(define-declaration optimize (&rest qualities)
  .optimize.
  :declare
  (lambda (declaration env)
    (let* ((base (declaration-information 'optimize env))
	   (safety (assoc 'safety base))
	   (space (assoc 'space base))
	   (speed (assoc 'speed base))
	   (compilation-speed (assoc 'compilation-speed base))
	   (debug (assoc 'debug base))
	   new)
      (pop declaration)
      (dolist (decl declaration)
	;; [bug21547]:
	(if (and (consp decl) (cddr decl))
	    (warn "Ignoring unsyntactic optimize declaration ~s." decl)
	  (let ((name (if (consp decl) (car decl) decl))
		(value (if (consp decl) (cadr decl) 3)))
	    (case name
	      (debug
	       (unless (and debug (eql value (cadr debug)))
		 (setq debug (list 'debug value))
		 (push debug new)))
	      (compilation-speed
	       (unless (and compilation-speed (eql value (cadr compilation-speed)))
		 (setq compilation-speed (list 'compilation-speed value))
		 (push compilation-speed new)))
	      (speed
	       (unless (and speed (eql value (cadr speed)))
		 (setq speed (list 'speed value))
		 (push speed new)))
	      (space
	       (unless (and space (eql value (cadr space)))
		 (setq space (list 'space value))
		 (push space new)))
	      (safety
	       (unless (and safety (eql value (cadr safety)))
		 (setq safety (list 'safety value))
		 (push safety new)))))))
      (setq new (nreverse new))
      (if env
	  (setq base (nconc new base))
	(progn
	  ;; We want to recons all changed optimization qualities, since the
	  ;; older values may be part of a stack of environments somewhere
	  (loop for qual in '(safety space speed compilation-speed debug)
	      unless (assoc qual new :test #'eq)
	      do (setq new (nconc new (list (assoc qual base :test #'eq)))))
	  (setq base new)))
          (values :declare `(optimize ,@base)))))

;; The declaration declaration

(defun ignored-declaration (declaration env)
  (declare (ignore declaration env))
  (values :ignored nil))

(define-declaration declaration (&rest declarations)
  .declaration.
  :declare
  (lambda (declaration env)
    (if env
	(warn "~s clause ignored except in ~s and ~s forms."
	      'declaration 'proclaim 'declaim)
      (let* ((base (declaration-information 'declaration nil))
	     kind)
	(pop declaration)
	(dolist (decl declaration)
	  (cond ((symbolp decl)
		 (if (member decl base :test #'eq)
		     (when (and (setq kind (ce-get decl 'declaration-kind))
				(not (eq kind :ignored)))
		       (warn "Declaration ~s already defined as a declaration." decl))
		   (push decl base))
		 (ce-putprop decl :ignored 'declaration-kind)
		 (ce-putprop decl #'ignored-declaration 'declaration-handler))
		(t (warn "Ignoring bad name ~s in ~s declaration"
			 decl 'declaration))))
	(values :declare `(declaration ,@base))))))


;; The type declaration

;; This function might be redefined in the implementation's compiler:

(defun type-declaration-handler (declaration env)
  (declare (ignore env)
	   (:discard-source-file-info))
  (values :variable
	  (let ((spec (list (car declaration) (cadr declaration))))
	    (mapcar #'(lambda (x) (cons x spec))
		    (cddr declaration)))))

(define-declaration type (typespec &rest vars)
  .type.
  :variable
  type-declaration-handler)


(define-declaration discriminated-type (typespec &rest vars)
  nil ;; No global declarations!
  :variable
  type-declaration-handler)

;; (Built-in type declarations removed)

;; The ftype declaration

(define-declaration ftype (typespec &rest fnames)
  .ftype.
  :function
  (lambda (declaration env)
    (declare (ignore env))
    (values :function
	    (mapcar #'(lambda (x) (list x 'ftype (cadr declaration)))
		    (cddr declaration)))))

(define-declaration (inline inline notinline) (&rest fnames)
  .inline.
  :function)

(define-declaration (ignore ignore ignorable #+allegro excl::ignore-if-unused) (&rest vars)
  .ignore.
  :both)

(define-declaration special (&rest vars)
  .globally-special.
  :variable
  (lambda (declaration env)
    (declare (ignore env))
    (let* ((spec '(special t))
	   (res (mapcar #'(lambda (x) (cons x spec))
			(cdr declaration))))
      (values :variable res))))

(define-declaration dynamic-extent (&rest names)
  nil ;; No global declarations!
  :both)

;; CLOS walker support:

;; Note that the ordering of the var and the alternate is
;; reversed from the version in the PCL walker.

(define-declaration variable-rebinding (alternate &rest vars)
  nil ;; No global declarations!
  :variable
  (lambda (declaration env)
    (declare (ignore env))
    (values :variable `((,(caddr declaration) variable-rebinding ,(cadr declaration))))))

(define-declaration excl::struct-by-value (ftype int-offset float-offset stack-offset &rest vars)
  nil ;; No global declarations!
  :variable
  (lambda (declaration env)
    (declare (ignore env))
    (values :variable
	    ;; In 10.1 and earlier we track a foreign-type name and two index indicators: count and altcount
	    #-(version>= 10 2)
	    `((,(fifth declaration) excl::struct-by-value (,(second declaration) ;; ftype
							   ,(third declaration) ;; count
							   ,(fourth declaration)))) ;; altcount
	    ;; In 10.2 we split the altcount into fcount (for a float index) and scount (for a stack index)
	    #+(version>= 10 2)
	    `((,(sixth declaration) excl::struct-by-value (,(second declaration) ;; ftype
							   ,(third declaration) ;; icount
							   ,(fourth declaration) ;; fcount
							   ,(fifth declaration))))))) ;; scount

) ;; without-package-locks


;; Create an initial set of optimizaton settings:

(sys::augment-environment nil :declare '((optimize (safety 1) (space 1) (speed 1) (compilation-speed 1) (debug 2))))

#+clisp
(progn
  (eval-when (:compile-toplevel)
    (setf (package-lock cl-user::.saved-p-lock.) t))
  (eval-when (:load-toplevel :execute)
    (setf (package-lock cl-user::.saved-p-lock.) t)))
