(in-package #:concrete-syntax-tree-test)

(defun fake-defmacro (name lambda-list form)
  `(defmacro ,name ,lambda-list ,form))

(defun call-test-macro (name lambda-list body-form test-form expected)
  (let ((mlambda
          (cst:parse-macro
           nil
           (cst:cst-from-expression name)
           (cst:cst-from-expression lambda-list)
           (list body-form)))
        (dmacro (fake-defmacro name lambda-list body-form)))
    (multiple-value-bind (mfunction warnings-p failure-p)
        (compile nil mlambda)
      (assert (not failure-p) nil
              "Compilation failed for macro: ~a" dmacro)
      (assert (not warnings-p) nil
              "Compilation warned for macro: ~a" dmacro)
      (multiple-value-bind (result errorp)
          (ignore-errors
           (funcall mfunction test-form nil))
        (if (eq expected 'error)
            (assert errorp nil
                    "Macro~%~t~a~%did not signal expected error ~
for input form: ~a~%and instead returned ~a"
                    dmacro test-form result)
            (assert (tree-equal result expected) nil
                    "Macro~%~t~a~%results~%~t~a~%did not match expected ~
~%~t~a~% for input form: ~%~t~a~@[~%Error: ~a~]"
                    dmacro result expected test-form errorp)))))
  (values))

(defmacro test-macro (name lambda-list body-form test-form expected)
  `(call-test-macro ',name ',lambda-list ',body-form ',test-form ',expected))

(defun test-parse-macro ()
  ;; Non-destructuring lambda list tests
  (test-macro 1arg (x) x (1arg (a b)) (a b))
  (test-macro 0arg () nil (0arg) nil)
  (test-macro too-few-req (x) x (too-few-req) error)
  (test-macro too-many-req (x) x (too-many-req a b) error)
  (test-macro optional (x y &optional (z 5 z-p) (w 4 w-p))
              (list x y z z-p w w-p)
              (optional a (b) (function print))
              (a (b) (function print) t 4 nil))
  (test-macro empty-optional (x &optional) x (empty-optional 4) 4)
  (test-macro too-few-optional (x &optional y) (list x y)
              (too-few-optional) error)
  (test-macro too-many-optional (x &optional y) (list x y)
              (too-many-optional a b c) error)
  (test-macro key (x &key (y 5 y-p) (z 4 z-p))
              (list x y y-p z z-p)
              (key a :y (function print))
              (a (function print) t 4 nil))
  (test-macro odd-key (x &key y) (list x y) (odd-key a b) error)
  #+(or) ; known failure
  (test-macro reject-key (&key x) x (reject-key :y 4) error)
  (test-macro and-aok (&key x &allow-other-keys) x
              (and-aok :y 4) nil)
  (test-macro colon-aok (&key x) x
              (colon-aok :y 7 :allow-other-keys t) nil)
  (test-macro colon-aok2 (&key x) x
              (colon-aok :allow-other-keys nil) nil)
  (test-macro rest (x &rest y) (list x y) (rest a b c) (a (b c)))
  #+(or) ; known failure
  (test-macro dot (x . y) (list x y) (dot a b c) (a (b c)))
  (test-macro too-few-rest (x &rest y) (list x y) (rest) error)
  (test-macro whole (&whole w x) (list w x) (whole a) ((whole a) a))
  (test-macro env (&environment e) e (env) nil)
  (test-macro combo (&whole w a b &rest r &key k &environment e)
              (list w a b r k e)
              (combo 1 2 :k 3) ((combo 1 2 :k 3) 1 2 (:k 3) 3 nil))
  ;; Destructuring
  (test-macro dreq (((a) &key b)) (list a b)
              (dreq ((x) :b y)) (x y))
  (test-macro dempty (() a) a (dempty () 4) 4)
  (test-macro too-few-dreq1 ((a)) a (dreq1) error)
  (test-macro too-few-dreq2 ((a)) a (dreq1 x) error)
  (test-macro too-many-dreq1 ((a)) a (dreq1 x y) error)
  (test-macro too-many-dreq2 ((a)) a (dreq (x y)) error)
  (test-macro dopt (&optional ((a b) '(nil nil) r1-p) ((c d) '(3 4) r2-p))
              (list a b r1-p c d r2-p)
              (dopt (1 2)) (1 2 t 3 4 nil))
  (test-macro dkey (&key ((:1 (a b)) '(nil nil) r1-p) ((:2 (c d)) '(3 4) r2-p))
              (list a b r1-p c d r2-p)
              (dkey :1 (1 2)) (1 2 t 3 4 nil))
  (test-macro drest (&rest (a b c)) (list a b c) (drest 1 2 3) (1 2 3))
  ;; Misc
  (test-macro blockt (x) (return-from blockt x) (blockt 4) 4))
