(defsystem "concrete-syntax-tree"
  :description "Library for parsing Common Lisp code into a concrete syntax tree."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD" ; See file LICENSE.text.
  :depends-on ("concrete-syntax-tree-base"
               "concrete-syntax-tree-lambda-list")
  :in-order-to ((test-op (test-op "concrete-syntax-tree/test"))))

(defsystem "concrete-syntax-tree/test"
  :depends-on ("concrete-syntax-tree" "concrete-syntax-tree-destructuring")
  :pathname "Test"
  :components ((:file "packages")
               (:file "random-expression")
               (:file "cst-from-expression")
               (:file "random-sources")
               (:file "reconstruct")
               (:file "parse-macro"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:concrete-syntax-tree-test '#:test-cst-from-expression)
             (uiop:symbol-call '#:concrete-syntax-tree-test '#:test-reconstruct)
             (uiop:symbol-call '#:concrete-syntax-tree-test '#:test-parse-macro)))
