(cl:in-package #:concrete-syntax-tree)

(defclass rule ()
  ((%left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (%right-hand-side :initarg :right-hand-side :reader right-hand-side)))

(defclass earley-item ()
  ((%rule :initarg :rule :reader rule)
   (%dot-position :initarg :dot-position :reader dot-position)
   (%origin :initarg :origin :reader origin)
   (%parse-trees :initarg :parse-trees :reader parse-trees)))

(defgeneric item-equal (item1 item2))

(defmethod item-equal ((item1 earley-item) (item2 earley-item))
  (and (eq (rule item1) (rule item2))
       (eq (dot-position item1) (dot-position item2))
       (eq (origin item1) (origin item2))))

(defclass earley-state ()
  ((%items :initform '() :accessor items)))

(defgeneric possibly-add-item (item state))

(defmethod possibly-add-item ((item earley-item) (state earley-state))
  (unless (find item (items state) :test #'item-equal)
    (setf (items state)
          (nconc (items state) (list item)))))

(defgeneric scanner-action
    (client item lambda-list terminal input))

(defclass grammar-symbol ()
  ((%parse-tree :initarg :parse-tree :reader parse-tree)))

(defclass simple-variable (grammar-symbol) ())

(defmethod scanner-action
    (client item lambda-list (terminal simple-variable) input)
  (if (symbolp input)
      (make-instance 'earley-item
        :rule (rule item)
        :parse-trees (cons (parse-trees item)
                           (make-instance 'simple-variable
                             :parse-tree input))
        :dot-position (1+ (dot-position item)))
      nil))
