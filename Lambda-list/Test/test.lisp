(cl:in-package #:concrete-syntax-tree-lambda-list-test)

(defun find-final-item (parser)
  (let ((initial-state (car (cst::all-states parser)))
        (final-state (car (cl:last (cst::all-states parser)))))
    (find-if (lambda (item)
               (let* ((rule (cst::rule item))
                      (len (length (cst::right-hand-side rule)))
                      (pos (cst::dot-position item)))
                 (and (eq (cst::left-hand-side (cst::rule item))
                          'cst::target)
                      (= pos len)
                      (eq (cst::origin item) initial-state))))
             (cst::items final-state))))

(defun assert-success (parser)
  (let ((item (find-final-item parser)))
    (assert (not (null item)))
    (car (cst::parse-trees item))))

(defun test-ordinary (lambda-list)
  (let* ((p (make-instance 'cst::parser
              :rules cst::*ordinary-lambda-list-grammar*
              :input lambda-list
              :lambda-list (make-instance 'cst::lambda-list-type-ordinary)
              :client nil)))
    (cst::parse p)
    (let ((result (assert-success p)))
      (compare-parse-trees result (parse-ordinary-lambda-list lambda-list)))))

(defun test ()
  (assert (test-ordinary '()))
  (assert (test-ordinary '(a)))
  (assert (test-ordinary '(a b)))
  (assert (test-ordinary '(&optional)))
  (assert (test-ordinary '(&optional a)))
  (assert (test-ordinary '(&optional a b)))
  (assert (test-ordinary '(&optional (a (f x)))))
  (assert (test-ordinary '(&optional (a (f x)) b)))
  (assert (test-ordinary '(&optional (a (f x) supplied-p))))
  (assert (test-ordinary '(a &optional)))
  (assert (test-ordinary '(a &optional b)))
  (assert (test-ordinary '(a &optional (b (f x)))))
  (assert (test-ordinary '(&rest a)))
  (assert (test-ordinary '(a &rest b)))
  (assert (test-ordinary '(a b &rest c)))
  (assert (test-ordinary '(&optional a &rest b)))
  (assert (test-ordinary '(&optional a (b) &rest c)))
  (assert (test-ordinary '(a &optional b &rest c)))
  (assert (test-ordinary '(&key)))
  (assert (test-ordinary '(&key a)))
  (assert (test-ordinary '(&key a b)))
  (assert (test-ordinary '(&key (a (f x)))))
  (assert (test-ordinary '(&key (a (f x) supplied-p))))
  (assert (test-ordinary '(&key ((:a a) (f x) supplied-p))))
  (assert (test-ordinary '(a &key)))
  (assert (test-ordinary '(a &key b)))
  (assert (test-ordinary '(b &key ((:a a) (f x) supplied-p))))
  (assert (test-ordinary '(b &optional c &key ((:a a) (f x) supplied-p))))
  (assert (test-ordinary '(&key &allow-other-keys))))
