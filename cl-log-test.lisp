;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-log/master/cl-log-test.lisp#1 $

(defpackage "COM.RAVENBROOK.COMMON-LISP-LOG.TEST"
  (:nicknames "CL-LOG-TEST")
  (:use :common-lisp :eos "COM.RAVENBROOK.COMMON-LISP-LOG")
  (:export "RUN!" "LOG-TESTS"))

(in-package "COM.RAVENBROOK.COMMON-LISP-LOG.TEST")

;;                       CL-LOG-TEST.LISP
;;         Nick Barnes, Ravenbrook Limited, 2012-05-15
;;
;; 1.  INTRODUCTION
;;
;; This is a test suite for Nick Levine's CL-LOG logging utility.
;;
;; See end for copyright and license.

;; To use:
#||

The test quite is (or: will soon be) available via Quicklisp:

    CL-USER > (ql:quickload :cl-log-test)

Alternatively: first load the EOS unit test framework and then compile
pkg.lisp, cl-log.lisp, and this file:

    CL-USER> (ql:quickload :eos)
    ;; [stuff]
    CL-USER> (in-package :cl-log-test)

Run the tests thus:

    CL-LOG-TEST> (run! 'log-tests)
    ..............................................................................................
     Did 94 checks.
	Pass: 94 (100%)
	Skip: 0 ( 0%)
	Fail: 0 ( 0%)
    NIL
    CL-LOG-TEST>
||#

(def-suite log-tests :description "Unit tests for cl-log.")

(in-suite log-tests)

;;; a partial-order of categories.
;;;
;;;       cat1
;;;    cat2 cat2b
;;;    cat3 cat3b
;;;     catjoin

(defvar *test-categories* '((:cat1 nil)
                            (:cat2 (or :cat1))
                            (:cat3 (or :cat2 :cat3))
                            (:cat2b (or :cat1 :cat2b))
                            (:cat3b (or :cat2b :cat3b))
                            (:catjoin (or :cat3 :cat3b))))

(defun random-category ()
  (car (nth (random (length *test-categories*)) *test-categories*)))

;;; a test-messenger simply collects all the messages sent to its log
;;; manager, and can reproduce them as a list.

(defclass test-messenger (base-messenger)
  ((messages :initform nil)))

(defmethod messenger-send-message ((messenger test-messenger) (message base-message))
  (push message (slot-value messenger 'messages)))

(defmethod test-messenger-messages ((self test-messenger))
  (reverse (slot-value self 'messages)))

;;; COLLECTING-MESSAGES returns all the messages sent to MANAGER
;;; (default: the current global log manager) matching FILTER
;;; (default: nil, i.e. all messages) during the execution of BODY.

(defmacro collecting-messages ((&key manager filter) &body body)
  (let ((messenger-var (gensym "MESSENGER-"))
        (messenger-name-var (gensym "MESSENGER-NAME-"))
        (manager-var (gensym "MANAGER-")))
    `(let* ((,manager-var (or ,manager (log-manager)))
            (,messenger-var (start-messenger 'test-messenger :category ,filter :name ',messenger-name-var :manager ,manager-var)))
       (unwind-protect
           (progn
             ,@body
             (test-messenger-messages ,messenger-var))
         (stop-messenger ,messenger-var)))))

;;; Macro to establish a simple partial order of logging categories,
;;; run a test, and then tear down the categories.  This is done in
;;; the category set SET, or in the category set of the current log
;;; manager (if SET is null).

(defmacro with-categories ((set) &body body)
  (let* ((set-sym (gensym "CATEGORY-SET-"))
         (set-part (when set (list set-sym)))
         (defcategories (loop for (cat def) in *test-categories* collect
                              `(defcategory ,cat ,def ,@set-part))))
    `(let ((,set-sym ,set))
       (unwind-protect
           (progn
             ,@defcategories
             ,@body)
         (clear-categories ,@set-part)))))

(test categories
  "Set up and tear down simple log categories."
  (with-categories (nil)
    (pass)))

(test category-set
  "Set up and tear down simple categories in a category-set."
  (let ((my-categories (make-instance 'category-set)))
    (with-categories (my-categories)
      (pass))))

(test simple-log-once
  "The most basic logging test: log something to the current log
manager."
  (log-message :warning :warning)
  (pass))

(test make-messenger
  "Can we make a messenger?"
  (rebinding-log-manager (nil)
    (is (null (collecting-messages () t)))))

(test log-to-messenger
  "Do log messages go to a messenger?"
  (let ((messages (rebinding-log-manager (nil)
                    (collecting-messages ()
                      (log-message :warning :warning)))))
    (is (= (length messages) 1))))

(test log-description
  "Test that log descriptions and arguments get to the messenger."
  (let* ((desc (format nil "~R" (random 1000)))
         (args (loop for i upto (random 100)
                     collect (random-category)
                     collect (random 10)))
         (messages (rebinding-log-manager (nil)
                     (collecting-messages ()
                       (log-message :spong desc args)))))
    (is (= (length messages) 1)
        (equal (message-description (car messages)) desc)
        (equal (message-arguments (car messages)) args))))

(test log-to-messenger-category
  "Do log messages keep their category and order en route to a messenger?"
  (let* ((category-list (loop for i upto 100 collect (random-category)))
         (messages (rebinding-log-manager (nil)
                     (collecting-messages ()
                       (loop for cat in category-list do
                             (log-message cat nil))))))
    (is (equal category-list (mapcar 'message-category messages)))))

(test multiple-messengers
  "Do log messages go to more than one messenger?"
  (let* ((messages-1)
         (messages-2 (rebinding-log-manager (nil)
                       (collecting-messages ()
                         (setf messages-1 (collecting-messages ()
                                            (log-message :warning :warning)))))))
    (is (equal messages-1 messages-2))
    (is (= (length messages-1) 1))
    (is (eq (message-category (car messages-1)) :warning))))

(test many-messages-multi-messengers
  "Sending several log messages to several messengers."
  (let* ((category-list (loop for i upto 100 collect (random-category)))
         (messages-1)
         (messages-2 (rebinding-log-manager (nil)
                       (collecting-messages ()
                         (setf messages-1 (collecting-messages ()
                                            (loop for cat in category-list do
                                                  (log-message cat nil))))))))
    (is (equal category-list (mapcar 'message-category messages-1)))
    (is (equal messages-1 messages-2))))

(defun log-n-of-each (times)
  (dotimes (i times)
    (log-message :cat1 "1" i)
    (log-message :cat2 "2" i)
    (log-message :cat3 "3" i)
    (log-message :catjoin "join" i)
    (log-message :cat2b "2b" i)
    (log-message :cat3b "3b" i)))

(test simple-filter
  "Do messenger filters work?  Also tests that message descriptions and arguments make it through."
  (let* ((messages-3b)
         (messages-2 (rebinding-log-manager (nil)
                       (with-categories (nil)
                         (collecting-messages (:filter :cat2)
                           (setf messages-3b (collecting-messages (:filter :cat3b)
                                               (log-n-of-each 3))))))))
    (is (equal (mapcar 'message-category messages-2) '(:cat1 :cat2 :cat1 :cat2 :cat1 :cat2)))
    (is (equal (mapcar 'message-description messages-2) '("1" "2" "1" "2" "1" "2")))
    (is (equal (mapcar 'message-arguments messages-2) '((0) (0) (1) (1) (2) (2))))
    (is (equal (mapcar 'message-category messages-3b) '(:cat1 :cat2b :cat3b :cat1 :cat2b :cat3b :cat1 :cat2b :cat3b )))
    (is (equal (mapcar 'message-description messages-3b) '("1" "2b" "3b" "1" "2b" "3b" "1" "2b" "3b")))
    (is (equal (mapcar 'message-arguments messages-3b) '((0) (0) (0) (1) (1) (1) (2) (2) (2))))))

(test combi-filter
  "Complex messenger filters."
  (let* ((messages (rebinding-log-manager (nil)
                     (with-categories (nil)
                       (collecting-messages (:filter '(and :catjoin (not :cat1) (not :cat2b)))
                         (log-n-of-each 1))))))
    (is (equal (mapcar 'message-category messages) '(:cat2 :cat3 :catjoin :cat3b)))))

(test combi-filter-2
  "Complex messenger filters again."
  (let* ((messages (rebinding-log-manager (nil)
                     (with-categories (nil)
                       (collecting-messages (:filter '(or (and :cat3 (not :cat2)) :cat3b))
                         (log-n-of-each 1))))))
    (is (equal (mapcar 'message-category messages) '(:cat1 :cat3 :cat2b :cat3b)))))

(test ring
  "Ring messenger test."
  (rebinding-log-manager (nil)
    (let ((messenger (start-messenger 'ring-messenger :length 10)))
      (loop for i upto 1000
            do (log-message :warning i))
      (is (= (length (ring-messenger-messages messenger)) 10))
      ;; 991 + 992 + ... + 1000 = 9900 + 10*11/2 = 9955
      (is (= (apply '+ (mapcar 'message-description (ring-messenger-messages messenger))) 9955)))))

(test manager-exists
  "Is there always a log manager?"
  (is (log-manager))
  (is (typep (log-manager) 'log-manager)))

(test clear-manager
  "Is there even a log manager when we have set it to nil?"
  (setf (log-manager) (make-instance 'log-manager))
  (is (log-manager))
  (is (typep (log-manager) 'log-manager))
  (is (= (length (log-manager-messengers (log-manager))) 0)))

(test rebound-manager-to-nil
  "Is there a log manager when we have rebound it to nil?"
  (rebinding-log-manager (nil)
    (is (log-manager))
    (is (typep (log-manager) 'log-manager))
    (is (= (length (log-manager-messengers (log-manager))) 0))))

;;; weirdly, if I write (is (collecting-messages ... )) in a test,
;;; I get a syntax error.  So I've lifted this out.
(defun log-enabled (&key manager)
  (collecting-messages (:manager manager) (log-message :warning nil)))

(test manager-switch
  "Can we switch between two log-managers?"
  (let ((manager-1 (make-instance 'log-manager))
        (manager-2 (make-instance 'log-manager)))
    (is (not (eq manager-1 manager-2)))
    (setf (log-manager) manager-2)
    (is (eq (log-manager) manager-2))
    (setf (log-manager) manager-1)
    (is (eq (log-manager) manager-1))
    (rebinding-log-manager (manager-1)
      (is (eq (log-manager) manager-1))
      (rebinding-log-manager (manager-2)
        (is (eq (log-manager) manager-2)))
      (is (eq (log-manager) manager-1)))))

(test log-manager-message
  "Does log-manager-message direct messages correctly?"
  (let ((manager-1 (make-instance 'log-manager))
        (manager-2 (make-instance 'log-manager))
        (messages-1)
        (messages-2))
    (setf messages-1 (collecting-messages (:manager manager-1)
                       (setf messages-2 (collecting-messages (:manager manager-2)
                                          (log-manager-message manager-1 :warning :foo 3 4)
                                          (log-manager-message manager-2 :critical :bar 5 6)))))
    (is (eq (length messages-1) 1))
    (is (eq (message-category (car messages-1)) :warning))
    (is (eq (message-description (car messages-1)) :foo))
    (is (equal (message-arguments (car messages-1)) '(3 4)))
    (is (eq (message-category (car messages-2)) :critical))
    (is (eq (message-description (car messages-2)) :bar))
    (is (equal (message-arguments (car messages-2)) '(5 6)))))

(test setf-log-manager
  "Does setting (log-manager) direct messages correctly?"
  (let ((manager-1 (make-instance 'log-manager))
        (manager-2 (make-instance 'log-manager))
        (messages-1)
        (messages-2))
    (setf messages-1 (collecting-messages (:manager manager-1)
                       (setf messages-2 (collecting-messages (:manager manager-2)
                                          (setf (log-manager) manager-1)
                                          (log-message :warning :foo 3 4)
                                          (setf (log-manager) manager-2)
                                          (log-message :critical :bar 5 6)))))
    (is (eq (length messages-1) 1))
    (is (eq (message-category (car messages-1)) :warning))
    (is (eq (message-description (car messages-1)) :foo))
    (is (equal (message-arguments (car messages-1)) '(3 4)))
    (is (eq (message-category (car messages-2)) :critical))
    (is (eq (message-description (car messages-2)) :bar))
    (is (equal (message-arguments (car messages-2)) '(5 6)))))

(test rebinding-manager
  "Does rebinding-log-manager direct messages correctly?"
  (let ((manager-1 (make-instance 'log-manager))
        (manager-2 (make-instance 'log-manager))
        (messages-1)
        (messages-2))
    (setf messages-1 (collecting-messages (:manager manager-1)
                       (setf messages-2 (collecting-messages (:manager manager-2)
                                          (setf (log-manager) manager-1)
                                          (log-message :warning :foo 3 4)
                                          (rebinding-log-manager (manager-2)
                                            (log-message :critical :bar 5 6))))))
    (is (eq (length messages-1) 1))
    (is (eq (message-category (car messages-1)) :warning))
    (is (eq (message-description (car messages-1)) :foo))
    (is (equal (message-arguments (car messages-1)) '(3 4)))
    (is (eq (message-category (car messages-2)) :critical))
    (is (eq (message-description (car messages-2)) :bar))
    (is (equal (message-arguments (car messages-2)) '(5 6)))))

(test disabled-manager
  "Can we disable logging?"
  (setf (log-manager) (make-instance 'log-manager))
  ;; new manager isn't disabled ...
  (is (log-enabled))
  (is (null (logging-disabled (log-manager))))
  ;; but with-logging-disabled makes it so ...
  (with-logging-disabled
    (is (logging-disabled (log-manager)))
    (is (null (log-enabled)))
    ;; ... unless we tweak the knob by hand ...
    (setf (logging-disabled (log-manager)) nil)
    (is (null (logging-disabled (log-manager))))
    (is (log-enabled))
    ;; ... either way.
    (setf (logging-disabled (log-manager)) t)
    (is (logging-disabled (log-manager)))
    (is (null (log-enabled))))
  ;;; When we come back out of with-logging-disabled,
  ;;; logging works again ...
  (is (log-enabled))
  (is (null (logging-disabled (log-manager))))
  ;;; but setting the disabled flag with setf disables it ...
  (setf (logging-disabled (log-manager)) t)
  (is (logging-disabled (log-manager)))
  (is (null (log-enabled)))
  ;;; ... and it stays disabled as we pass through a
  ;;; with-logging-disabled section ...
  (with-logging-disabled
    (is (logging-disabled (log-manager)))
    (is (null (log-enabled))))
  (is (logging-disabled (log-manager)))
  (is (null (log-enabled)))
  ;;; until we reset it by hand ...
  (setf (logging-disabled (log-manager)) nil)
  (is (null (logging-disabled (log-manager))))
  (is (log-enabled)))

(test with-log-manager-disabled
  "Can we disable an individual log manager independently?"
  (let ((manager-1 (make-instance 'log-manager))
        (manager-2 (make-instance 'log-manager))
        (messages-1)
        (messages-2))
    (setf messages-1 (collecting-messages (:manager manager-1)
                       (setf messages-2 (collecting-messages (:manager manager-2)
                                          (log-manager-message manager-1 :foo nil)
                                          (log-manager-message manager-2 :bar nil)
                                          (with-log-manager-disabled (manager-2)
                                            (log-manager-message manager-1 :qux nil)
                                            (log-manager-message manager-2 :quux nil))
                                          (with-log-manager-disabled (manager-1)
                                            (log-manager-message manager-1 :baz nil)
                                            (log-manager-message manager-2 :spong nil))))))
    (is (equal (mapcar 'message-category messages-1) '(:foo :qux)))
    (is (equal (mapcar 'message-category messages-2) '(:bar :spong)))))

(test category-satisfies
  "Test the basic behaviour of category-satisfies.  See also the combi-filters tests."
  (clear-categories)
  (is (category-satisfies :foo :foo))
  (is (not (category-satisfies :foo :bar)))
  ;; with our usual test categories, test some basic satisfaction criteria
  (with-categories (nil)
    (is (category-satisfies :cat1 :cat2))
    (is (not (category-satisfies :cat2 :cat1)))
    (is (category-satisfies :cat1 :cat2b))
    (is (not (category-satisfies :cat2 :cat2b)))
    (is (category-satisfies :cat1 :catjoin))
    (is (category-satisfies :cat2 :catjoin))
    (is (not (category-satisfies :catjoin :cat1)))
    (is (category-satisfies :cat2b :catjoin)))
  ;;; does clear-categories get rid?
  (clear-categories)
  (is (not (category-satisfies :cat1 :cat2)))
  (is (not (category-satisfies :cat2 :cat1)))
  ;;; How about if we have different log managers with different categories?
  (let* ((manager-1 (make-instance 'log-manager))
         (cat-set (make-instance 'category-set))
         (manager-2 (make-instance 'log-manager :categories cat-set)))
    ;; some categories for my new category set.
    (defcategory :meltdown nil                   cat-set)
    (defcategory :alarm    (or :alarm :meltdown) cat-set)
    (defcategory :cat2     (or :cat2 :alarm)     cat-set)
    (defcategory :cat3     (or :cat3 :alarm)     cat-set)
    (defcategory :cat4     (or :cat2 :cat3)      cat-set)
    (with-categories ((log-manager-category-set manager-1))
      (is (category-satisfies :cat2 :cat3))
      (is (not (category-satisfies :cat2 :cat3 :manager manager-2)))
      (is (category-satisfies :cat3 :cat4 :manager manager-2))
      (is (category-satisfies :cat2b :cat3b :manager manager-1)))))

(test text-messenger
  "Test text-stream-messengers"
  (let ((s (make-string-output-stream))
        (m (make-instance 'log-manager :message-class 'formatted-message)))
    (start-messenger 'text-stream-messenger :manager m :stream s)
    (dotimes (i 100)
      (log-manager-message m :warning "The square of ~d is ~d" i (* i i)))
    (is (= 100 (count #\Newline (get-output-stream-string s))))))

;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2012-05-15 NB  Created.
;;
;;
;; C.  COPYRIGHT
;;
;; This file copyright (c) 2012 Nick Barnes (nb@ravenbrook.com)

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
