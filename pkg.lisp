;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-log/master/pkg.lisp#1 $

(in-package "CL-USER")

;;                           PKG.LISP
;;         Nick Levine, Ravenbrook Limited, 2007-05-23
;;
;; 1.  INTRODUCTION
;;
;; This is the package definition for the cl-log library.
;;
;; See end for copyright and license.


;; 2.  PACKAGE

(defpackage "COM.RAVENBROOK.COMMON-LISP-LOG"
  (:nicknames "CL-LOG")
  (:use "COMMON-LISP")
  (:export
   "*CL-LOG-RELEASE*"
   ;; manager
   "LOG-MANAGER"                        ; [setfable] the current log-manager; also log-manager class eg (setf (log-manager) (make-instance 'log-manager))
   "REBINDING-LOG-MANAGER"              ; macro which gives local binding for value of (log-manager)
   "LOG-MANAGER-MESSENGERS"             ; [setfable] messengers of the given manager
   "LOG-MANAGER-MESSAGE-CLASS"          ; [setfable] in case you change your mind about the class you specified before
   "LOG-MANAGER-CATEGORY-SET"           ; [setfable] the category definitions this log manager knows about
   "LOGGING-DISABLED"                   ; [setfable] (setf (logging-disabled (log-manager)) t) turns logging off
   "WITH-LOGGING-DISABLED"              ; macro, disables logging while in lexical scope -- be careful about using this in tandem with (setf logging-disabled)
   "WITH-LOG-MANAGER-DISABLED"          ; macro, disables this manager while in lexical scope -- be careful about using this in tandem with (setf logging-disabled)
   "INVALIDATE-LOG-MANAGER"             ; this is called when categories or messengers are changed. you might want to write :after methods on this
   ;; timestamp
   "TIMESTAMP"
   "MAKE-TIMESTAMP"                     ; (make-timestamp (log-manager)) if you ever wanted a timestamp of your own to take home and play with
   "TIMESTAMP-UNIVERSAL-TIME"           ; universal-time at which timestamp was created
   "TIMESTAMP-FRACTION"                 ; fraction of a second (using internal-time-units-per-second)
   ;; message
   "BASE-MESSAGE"                       ; base class for messages
   "MESSAGE-TIMESTAMP"                  ; some readers...
   "MESSAGE-CATEGORY"                   ;
   "MESSAGE-DESCRIPTION"                ;
   "MESSAGE-ARGUMENTS"                  ;
   "FORMATTED-MESSAGE"                  ; class for messages which will be formatted to a stream
   "MESSAGE-TEXT"                       ; lazy invocation of format-message
   "FORMAT-MESSAGE"                     ; method on formatted-message goes (format nil "~a ~a ~?~&" timestamp category description arguments). Feel free to specialise.
   ;; messenger
   ;; ** Every messenger class must define a method on messenger-send-message **
   "BASE-MESSENGER"                     ; base class for messengers
   "MESSENGER-FILTER"                   ; reader
   "MESSENGER-MANAGER"                  ; reader
   "MESSENGER-NAME"                     ; reader - note that start-messenger will remove any previous messenger with the same name in this log manager
   "MESSENGER-CATEGORY"                 ; deprecated reader; use messenger-filter.
   "START-MESSENGER"                    ; (start-messenger 'text-file-messenger :filename "...") adds the new messenger to the current (or specified) log-manager
   "STOP-MESSENGER"                     ; remove this messenger (or messenger with this name) from current (or specified) log-manager; this method may be specialised.
   "FIND-MESSENGER"                     ; find messenger with given name.
   "MESSENGER-SEND-MESSAGE"             ; (defmethod messenger-send-message messenger message) actually logs this message; this method needs specialising.
   "RING-MESSENGER"                     ; (make-instance 'ring-messenger :length 50)
   "RING-MESSENGER-MESSAGES"
   "TEXT-STREAM-MESSENGER"              ; class for messengers for which formatted text will be sent to an output stream.
   "TEXT-STREAM-MESSENGER-STREAM"       ; reader - stream to which output is sent.
   "TEXT-FILE-MESSENGER"                ; subclass of text-stream-messenger where output is to file.
   "TEXT-FILE-MESSENGER-FILE"           ; reader - file to which the output goes
   ;; category
   "CATEGORY-SET"                       ; class of sets of categories
   "DEFCATEGORY"                        ; (defcategory :debug (or :debug :info)) defines a new category. The expansion looks recursive but that's handled for you.  Takes optional third argument of a category-set.
   "DEFCATEGORY-FN"                     ; (defcategory-fn (my-category-keyword) '(or :debug :info)) for runtime use.
   "UNDEFCATEGORY"                      ; (undefcategory :debug) removes one category from the current category set (or from the optional second argument, a category-set).
   "UNDEFCATEGORY-FN"                   ; (undefcategory-fn (my-category-keyword)) for runtime use.
   "CLEAR-CATEGORIES"                   ; (clear-categories) wipes the current (or specified) set of categories.
   "CATEGORY-SATISFIES"                 ; Does this category match that filter?  (category-satisfies :error (and :info (not :error))) => nil
   ;; log-message
   "LOG-MESSAGE"                        ; (log-message <category> <desctiption> <arguments...>) e.g. (log-message :debug "Frob ~a happened" (car frobs))
                                        ; category always evaluated, other parameters only evaluated if category matches at least one messenger
   "LOG-MANAGER-MESSAGE"                ; (log-message <manager> <category> <desctiption> <arguments...>) Just like log-message, but uses <manager>
                                        ; to dispatch the message.
   ))



;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2007-05-23 Created.
;;
;;
;; C.  COPYRIGHT
;;
;; This file copyright (c) 2007 - 2009 Nick Levine (ndl@ravenbrook.com)
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

