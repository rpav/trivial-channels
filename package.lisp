(defpackage :trivial-channels
  (:use #:cl)
  (:export

   ;; Queue
   #:make-queue #:queue-head #:queue-tail
   #:queue-add-cons #:queue-add #:queue-push #:queue-pop
   #:queue-pop-cons #:queue-peek #:queue-pop-to
   #:queue-prepend-to #:queue-has-item-p

   ;; Channels
   #:channel #:make-channel #:hasmsg #:sendmsg #:recvmsg #:getmsg))
