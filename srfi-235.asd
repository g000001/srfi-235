;;;; srfi-235.asd

(cl:in-package :asdf)


(defsystem :srfi-235
  :version "20230302"
  :description "SRFI 235 for CL: Combinators"
  :long-description "SRFI 235 for CL: Combinators
https://srfi.schemers.org/srfi-235"
  :author "CHIBA Masaomi"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (rnrs-compat r6rs-reader srfi-172 srfi-16 srfi-1)
  :components ((:file "package")
               (:file "readtable")
               (:file "srfi-235")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-235))))
  (let ((name "https://github.com/g000001/srfi-235")
        (nickname :srfi-235))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))

;;; *EOF*
