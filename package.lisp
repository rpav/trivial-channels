(defpackage :trivial-channels.queue
  (:use #:cl)
  (:export
   #:queue
   #:make-queue #:queue-head #:queue-tail
   #:queue-add-cons #:queue-add #:queue-push #:queue-pop
   #:queue-pop-cons #:queue-peek #:queue-pop-to
   #:queue-prepend-to #:queue-has-item-p))

(defpackage :trivial-channels
  (:use #:cl #:trivial-channels.queue)
  (:export
   #:channel #:make-channel #:hasmsg #:sendmsg #:recvmsg #:getmsg))

