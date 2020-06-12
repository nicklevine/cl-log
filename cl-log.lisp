;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-log/master/cl-log.lisp#3 $

(in-package "COM.RAVENBROOK.COMMON-LISP-LOG")

;;                          CL-LOG.LISP
;;         Nick Levine, Ravenbrook Limited, 2007-05-15
;;
;; 1.  INTRODUCTION
;;
;; This is a general purpose logging utility, loosely modelled in some
;; respects after Gary King's "Log5" <http://common-lisp.net/project/log5/>.
;;
;; Developed on LispWorks. Also tested on ACL.
;;
;; See end for copyright and license.

;; Example:
#||
(defcategory :critical)
(defcategory :error   (or :error :critical))
(defcategory :warning (or :warning :error))
(defcategory :notice  (or :notice :warning))
(defcategory :info    (or :info :notice))
(defcategory :debug   (or :debug :info))
(setf (log-manager) (make-instance 'log-manager))
(start-messenger 'ring-messenger :name 'demo :length 10 :filter '(and :info (not :error)))
(dolist (z '(:critical :error :warning :notice :info :debug))
  (log-message z z))
;; Show that we logged everything which was at least :info provided it wasn't also at least :error
(loop for message in (ring-messenger-messages (car (log-manager-messengers (log-manager))))
      when message collect (message-description message))
||#


(defparameter *cl-log-release* "1.0.1")

;; 2.  UTILITIES

(defmacro orf (location form &environment env)
  (multiple-value-bind (vars values new setter getter)
      (get-setf-expansion location env)
    (when (cdr new)
      (error "Can't work with setf-expansion for ~s - ~d values from setter ~s"
             location (length new) new))
    (let ((current (car new)))
      `(let* (,@(mapcar 'list vars values)
              (,current ,getter))
         (or ,current
             (progn (setf ,current ,form)
               ,setter))))))

(defmacro when-let (binding &body body)
  (destructuring-bind (var val) binding
    `(let ((,var ,val))
       (when ,var
	 ,@body))))

#+#:not-in-use
(defmacro when-let* (bindings &body body)
  (if bindings
      `(when-let ,(car bindings)
	 (when-let* ,(cdr bindings)
	   ,@body))
    `(progn ,@body)))


;; 3.  LOG-MANAGER

(defclass log-object ()
  ())

(defclass log-manager (log-object)
  ((messengers      :accessor log-manager-messengers    :initform nil)
   (disabled        :accessor logging-disabled-var      :initform (gensym))
   (message-class   :accessor log-manager-message-class :initarg :message-class :initform 'base-message)
   (message-id      :accessor log-manager-id            :initform 0)            ; for debugging - id of latest message
   (category-set    :accessor log-manager-category-set  :initarg :categories)
   (category-cache  :reader   category-cache            :initform nil)
   (cache-version   :accessor cache-version)
   (first-time      :reader   log-manager-first-time    :initform (first-time-for-log-manager))))

(defvar *log-manager* nil)

(defun log-manager ()
  (orf *log-manager* (make-instance 'log-manager)))

(defmethod initialize-instance :after ((self log-manager) &key disabled categories)
  (setf (logging-disabled self) disabled)
  (unless categories
    (setf (log-manager-category-set self)
          (if *log-manager*
              (log-manager-category-set *log-manager*)
            (make-instance 'category-set)))) ;; for the first log manager.
  (invalidate-log-manager self))

(defun first-time-for-log-manager ()
  (- (* (get-universal-time)
        internal-time-units-per-second)
     (get-internal-real-time)))

(defmethod logging-disabled ((self log-manager))
  (symbol-value (logging-disabled-var self)))

(defmethod (setf logging-disabled) (new-value (self log-manager))
  (setf (symbol-value (logging-disabled-var self)) new-value))

(defmacro with-logging-disabled (&body body)
  `(progv `(,(logging-disabled-var  (log-manager))) '(t)
     ,@body))

(defmacro with-log-manager-disabled ((manager) &body body)
  `(progv `(,(logging-disabled-var  ,manager)) '(t)
     ,@body))

(defmethod (setf log-manager-messengers) :after (new-value (self log-manager))
  (declare (ignore new-value))
  (invalidate-log-manager self))

(defmethod (setf log-manager-category-set) :after (new-value (self log-manager))
  (declare (ignore new-value))
  (invalidate-log-manager self))

(defmethod category-cache :before ((self log-manager))
  (when (< (cache-version self) (category-set-version (log-manager-category-set self)))
    (invalidate-log-manager self)))

(defmethod invalidate-log-manager ((self log-manager))
  (let ((cache (orf (slot-value self 'category-cache)
                    (make-hash-table :test 'equal))))
    (clrhash cache)
    (setf (cache-version self) (category-set-version (log-manager-category-set self)))))

;; There is always a log manager, even when there are no log
;; messengers, etc.  This means that there is always a default
;; category-set and so on, so (for example) client code from earlier
;; versions of cl-log can begin with (defcategory ...)

(defun (setf log-manager) (new-manager)
  (unless (typep new-manager 'log-manager)
    (error "New log-manager is not a log-manager: ~s" new-manager))
  (when-let (previous *log-manager*)
    (dolist (messenger (log-manager-messengers previous))
      (stop-messenger messenger)))
  (setf *log-manager* new-manager)
  new-manager)

(defmacro rebinding-log-manager ((log-manager) &body body)
  (let ((log-manager-var (gensym "LOG-MANAGER-")))
    `(let ((,log-manager-var ,log-manager))
       (unless (typep ,log-manager-var '(or log-manager null))
         (error "New log-manager is neither null nor a log-manager: ~s" ,log-manager-var))
       (when ,log-manager-var
         (setf (slot-value ,log-manager-var 'category-cache) nil)
         (invalidate-log-manager ,log-manager-var))
       (let ((*log-manager* (or ,log-manager-var (make-instance 'log-manager))))
         ,@body))))

;; 4.  MESSAGE

;; Warning: the fraction will be self-consistent but not externally consistent: the fraction
;; won't be zero when the univeral-time changes. (If we wanted this we'd have to wait for it,
;; and we still might not get to it spot-on.)

(defstruct (timestamp
            (:constructor construct-timestamp (universal-time fraction)))
  (universal-time nil :read-only t)
  (fraction       nil :read-only t))

(defun make-timestamp (log-manager)
  (let* ((first-time (log-manager-first-time log-manager))
         (this-time (+ first-time (get-internal-real-time))))
    (multiple-value-bind (univeral-time fraction)
        (floor this-time internal-time-units-per-second)
      (construct-timestamp univeral-time fraction))))

(defmethod print-object ((self timestamp) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :type t :identity t)
        (let ((*print-escape* nil))
          (print-object self stream)))
    (format stream #.(format nil "~~d.~~~d,'0d" (ceiling (log internal-time-units-per-second 10)))
            (timestamp-universal-time self)
            (timestamp-fraction self))))

(defclass base-message (log-object)
  ((id          :reader message-id          ) ; see initialize-instance
   (timestamp   :reader message-timestamp   :initarg :timestamp)
   (category    :reader message-category    :initarg :category)
   (description :reader message-description :initarg :description)
   (arguments   :reader message-arguments   :initarg :arguments)))

(defmethod initialize-instance :after ((self base-message) &key manager timestamp)
  (unless timestamp
    (error "Message with no timestamp: ~s" self))
  (let ((manager (or manager (log-manager))))
    (setf (slot-value self 'id) (incf (log-manager-id manager)))))

(defmethod print-object ((self base-message) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d" (message-id self))))

(defclass formatted-message (base-message)
 ((text :accessor formatted-message-text :initform nil)))

(defmethod message-text ((self formatted-message))
  (orf (formatted-message-text self)
       (format-message self)))

(defmethod format-message ((self formatted-message))
  (format nil "~a ~a ~?~&"
          (message-timestamp self)
          (message-category self)
          (message-description self)
          (message-arguments self)))


;; 5.  MESSENGER

(defclass base-messenger (log-object)
  ((manager  :reader messenger-manager  :initarg :manager  :initform (log-manager))
   (name     :reader messenger-name     :initarg :name     :initform nil)
   (filter   :reader messenger-category :initarg :category
             :reader messenger-filter   :initarg :filter :initform nil)))

(defmethod print-object ((self base-messenger) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (when-let (name (messenger-name self))
      (format stream "~a" name))))

(defmethod initialize-instance :after ((self base-messenger) &key name)
  (when (typep name 'base-messenger)
    (error "It really doesn't help using one messenger ~s to name another ~s" name self)))

(defun start-messenger (class &rest initargs &key name manager &allow-other-keys)
  (when-let (previous (find-messenger name :manager manager))
    (stop-messenger previous))
  (let ((messenger (apply 'make-instance class initargs)))
    (push messenger (log-manager-messengers (or manager (log-manager))))
    messenger))

(defmethod stop-messenger ((self base-messenger) &key)
  (let* ((manager (messenger-manager self))
         (messengers (log-manager-messengers manager)))
    (when (find self messengers)
      (setf (log-manager-messengers manager)
            (remove self messengers)))))

(defmethod stop-messenger (name &key manager)
  (let ((messenger (find-messenger name :manager manager)))
    (if messenger
        (stop-messenger messenger)
      (error "Messenger named ~s not found" name))))

(defun find-messenger (name &key manager)
  (find name (log-manager-messengers (or manager (log-manager)))
        :key 'messenger-name
        :test 'equalp))

(defun category-messengers (category &key manager)
  (let* ((manager (or manager
                      (log-manager)
                      ;; shouldn't ever fall through here, but just in case:
                      (return-from category-messengers nil)))
         (cache (category-cache manager)))
    (unless (logging-disabled manager)
      (multiple-value-bind (satisfies presentp)
          (gethash category cache)
        (if presentp
            satisfies
          (setf (gethash category cache)
                (loop for messenger in (log-manager-messengers manager)
                      when (category-satisfies category (messenger-filter messenger)
                                               :manager manager)
                      collect messenger)))))))

;; Does the supplied category match the filter?
;; The filter is either a keyword or a logical combination
;; of keywords held together with AND, OR and NOT.
;; The supplied category is either a keyword or a list of keywords in
;; which case the implicit combination is AND.
;; [I am unconvinced that there's anything other than unnecessary complexity
;;  to be gained from category being more general than this.]
;; [Although the code doesn't enforce keywords, I am suggesting this to allow for future
;; expansion, e.g. supplying funcallables.]
;; (category-satisfies '(:this :that) '(or :this :that)) => T         ; needed either, got both, so satisfied

(defun category-satisfies (supplied filter &key manager)
  (unless (listp supplied)
    (setf supplied (list supplied)))
  (in-category-satisfies (or manager (log-manager)) supplied filter supplied))

(defun in-category-satisfies (manager supplied filter expanded)
  (typecase filter
    (null t)
    (atom (let ((expansion (unless (find filter expanded)
                             (expand-category filter :set (log-manager-category-set manager)))))
            (if expansion
                (in-category-satisfies manager supplied expansion (cons filter expanded))
              (not (null (find filter supplied))))))
    (t (ecase (car filter)
         ((and) (every (lambda (r) (in-category-satisfies manager supplied r expanded)) (cdr filter)))
         ((or)  (some  (lambda (r) (in-category-satisfies manager supplied r expanded)) (cdr filter)))
         ((not) (if (cddr filter)
                    (error "(Sub)category NOT with more than one 'argument': ~s" filter)
                  (not (in-category-satisfies manager supplied (cadr filter) expanded))))))))

(defun send-message (log-manager messengers category description arguments)
  (let* ((message (make-instance (log-manager-message-class log-manager)
                                 :timestamp (make-timestamp log-manager)
                                 :category category
                                 :description description
                                 :arguments arguments)))
    (loop for messenger in messengers do
          (messenger-send-message messenger message))))

(defmethod messenger-send-message ((messenger base-messenger) message)
  (error "Messenger ~s of class ~s has not specialised ~s for message ~s of class ~s"
         messenger
         (class-of messenger)
         'messenger-send-message
         message
         (class-of message)))


;; 5.1. Ring-Messenger
;;
;; A simple example messenger. We define a ring structure and a class
;; ring-messenger which will remember the last N log-messages cheaply.
;; We have specialised messenger-send-message as required.  We have
;; not specialised stop-messenger as doing so is optional and in this
;; case there's nothing to do.

(defstruct (ring
            (:constructor construct-ring (name ring length)))
  name
  ring
  length)

(defmethod print-object ((self ring) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~(~a~) (~d)"
            (ring-name self)
            (ring-length self))))

(defun make-ring (name length)
  (let ((ring (make-list length)))
    (setf (cdr (last ring)) ring)
    (construct-ring name ring length)))

(defun ring-push (thing ring)
  (setf (car (setf (ring-ring ring)
                   (cdr (ring-ring ring))))
        thing))

(defun ring-list (ring)
  (loop repeat (ring-length ring)
        for x in (cdr (ring-ring ring))
        collect x))

(defclass ring-messenger (base-messenger)
  ((ring :reader ring-messenger-ring)))

(defmethod initialize-instance :after ((self ring-messenger) &key name length)
  (setf (slot-value self 'ring) (make-ring name length)))

(defmethod print-object ((self ring-messenger) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~(~a~)" (ring-name (ring-messenger-ring self)))))

(defmethod messenger-send-message ((messenger ring-messenger) (message base-message))
  (ring-push message (ring-messenger-ring messenger)))

(defmethod ring-messenger-messages ((self ring-messenger))
  (remove nil (ring-list (ring-messenger-ring self))))


;; 5.2. Text-Stream-Messenger

(defclass text-stream-messenger (base-messenger)
  ((stream :reader text-stream-messenger-stream :initarg :stream)
   (closed :accessor text-stream-messenger-closed :initform nil)))

(defmethod messenger-send-message ((messenger text-stream-messenger) (message formatted-message))
  (let ((ostream (text-stream-messenger-stream messenger)))
    (handler-bind (;; Trap race condition where thread A starts a logging operation and lists this as one of
                   ;; its messengers, thread B stops the messenger, and then thread A attempts to complete its
                   ;; logging operation by writing to ostream (now closed). The alternatives would be to halt
                   ;; preemption (application-specific and maybe costly) or to handle all logging operations in
                   ;; a dedicated thread (also application-specific and maybe costly).
                   (serious-condition (lambda (condition)
                                        (declare (ignore condition))
                                        (when (text-stream-messenger-closed messenger)
                                          (return-from messenger-send-message)))))
      (write-string (message-text message) ostream))
    (ignore-errors
      (force-output ostream))))

(defmethod stop-messenger :before ((self text-stream-messenger) &key)
  (let ((stream (text-stream-messenger-stream self)))
    (setf (text-stream-messenger-closed self) t)
    (ignore-errors
      (force-output stream))
    (close stream)))


;; 5.3 Text-File-Messenger

(defclass text-file-messenger (text-stream-messenger)
  ((file :reader text-file-messenger-file :initarg :filename)))

(defmethod initialize-instance :after ((self text-file-messenger) &key filename (external-format :default) &allow-other-keys)
  (setf (slot-value self 'stream)
        (open filename
              :direction :output
              :element-type :default
              :if-does-not-exist :create
              :if-exists :append
              :external-format external-format)))


;; 6.  CATEGORY

(defclass category-set (log-object)
  ((categories :accessor category-set-categories :initform (make-hash-table :test 'eq))
   (version    :accessor category-set-version    :initform 0)))

(defun expand-category (category &key set)
  (or (gethash category (category-set-categories (or set (log-manager-category-set (log-manager)))))
      category))

;; (defcategory :debug (or :debug :info)) will work.
;; Note that (defcategory :critical) doesn't have any effect other than to make your code clearer.
(defmacro defcategory (category &optional expands-as set)
  `(defcategory-fn ',category ',expands-as ,set))

(defun defcategory-fn (category expands-as &optional set)
  (let ((set (or set (log-manager-category-set (log-manager)))))
    (setf (gethash category (category-set-categories set)) expands-as)
    (incf (category-set-version set)))
  category)

(defmacro undefcategory (category &optional set)
  `(undefcategory-fn ',category ,set))

(defun undefcategory-fn (category &optional set)
  (let ((set (or set (log-manager-category-set (log-manager)))))
    (remhash category (category-set-categories set))
    (incf (category-set-version set)))
  nil)

(defun clear-categories (&optional set)
  (let ((set (or set (log-manager-category-set (log-manager)))))
    (clrhash (category-set-categories set))
    (incf (category-set-version set)))
  nil)


;; 7.  LOG-MESSAGE

;; By making this a macro we can defer evaluation of description and arguments until we know
;; that the message will be sent somewhere. The idea is to make :wombat logging very cheap when
;; :wombat logging isn't enabled

(defmacro log-manager-message (manager category description &rest arguments)
  (if (member :no-logging *features*)
      `(values)
    (let ((category-var (gensym "CATEGORY-"))
          (manager-var (gensym "MANAGER-"))
          (messengers-var (gensym "MESSENGERS-")))
      `(let ((,category-var ,category)
             (,manager-var ,manager))
         (when-let (,messengers-var (category-messengers ,category-var :manager ,manager-var))        ; null when logging-disabled is set
           (send-message ,manager-var ,messengers-var ,category-var
                         ,description (list ,@arguments)))
         nil))))

(defmacro log-message (category description &rest arguments)
  `(log-manager-message (log-manager) ,category ,description ,@arguments))

;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2007-05-15 Created.
;;
;;
;; C.  COPYRIGHT
;;
;; This file copyright (c) 2007 - 2009 Nick Levine (ndl@ravenbrook.com).
;;
;; Portions copyright (c) 2012 Nick Barnes (nb@ravenbrook.com).
;;
;; Log5 copyright (c) 2007 Gary Warren King (gwking@metabang.com)

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

