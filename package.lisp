;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-235"
  (:use)
  (:export
   constantly
   complement
   swap
   flip
   on-left
   on-right
   conjoin
   disjoin
   each-of
   all-of
   any-of
   on
   left-section
   right-section
   apply-chain
   arguments-drop
   arguments-drop-right
   arguments-take
   arguments-take-right
   group-by

   begin-procedure
   if-procedure
   when-procedure
   unless-procedure
   value-procedure
   case-procedure
   and-procedure
   eager-and-procedure
   or-procedure
   eager-or-procedure
   funcall-procedure
   loop-procedure
   while-procedure
   until-procedure

   always
   never
   boolean))


(defpackage "https://github.com/g000001/srfi-235#internals" 
  (:use
   "https://github.com/g000001/srfi-235"
   "https://github.com/g000001/srfi-172"
   "https://github.com/g000001/srfi-46"
   "https://github.com/g000001/srfi-16"
   "https://github.com/g000001/srfi-1"
   )
  (:shadowing-import-from rnrs define else)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-61"
   =>)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-1"
   every any assoc
   make-list member list-copy take-right drop-right drop take)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-172"
   string-hash))

;;; *EOF*
