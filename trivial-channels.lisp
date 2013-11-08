(in-package :trivial-channels)

 ;; Stupid simple queue, no locking

(declaim (inline make-queue))
(defstruct queue head tail)

(defun queue-add-cons (q cons)
  (when cons
    (if (queue-tail q)
        (progn
          (rplacd (queue-tail q) cons)
          (setf (queue-tail q) (cdr (queue-tail q))))
        (progn
          (setf (queue-head q) cons)
          (setf (queue-tail q) cons)))
    cons))

(defun queue-add (q item)
  (car (queue-add-cons q (cons item nil))))

(defun queue-push (q item)
  (setf (queue-head q)
        (cons item (queue-head q)))
  (unless (queue-tail q)
    (setf (queue-tail q) (queue-head q)))
  item)

(defun queue-pop-cons (q)
  (let ((cons (queue-head q)))
    (when cons
      (setf (queue-head q) (cdr cons))
      (rplacd cons nil)
      (unless (queue-head q)
        (setf (queue-tail q) nil))
      cons)))

(defun queue-pop (q)
  (car (queue-pop-cons q)))

(defun queue-has-item-p (q)
  (not (null (queue-head q))))

(defun queue-peek (q)
  "Peek at the head of the queue."
  (car (queue-head q)))

(defun queue-pop-to (q1 q2)
  "Pop from `Q1`, adding to `Q2`, without consing."
  (let ((cons (queue-pop-cons q1)))
    (queue-add-cons q2 cons)
    (car cons)))

(defun queue-prepend-to (q1 q2)
  "Prepend all items in `Q1` to `Q2`, removing them from `Q1`"
  (unless (or (null (queue-head q1)))
    (rplacd (queue-tail q1) (queue-head q2))

    (when (null (queue-tail q2))
      (setf (queue-tail q2) (queue-tail q1)))

    (setf (queue-head q2) (queue-head q1))
    (setf (queue-head q1) nil)
    (setf (queue-tail q1) nil))
  (values))

 ;; timing and timeouts

(defun wait-with-timeout (condition mutex seconds)
  "By default we use TRIVIAL-TIMEOUTS; this can be changed for implementations
later should it prove less-than-optimal."
  (when (or (null seconds) (> seconds 0))
    (handler-case
        (trivial-timeout:with-timeout (seconds)
          (bt:condition-wait condition mutex)
          t)
      (trivial-timeout:timeout-error (e)
        (declare (ignore e))))))

 ;; trivial channels

(defstruct channel
  (queue (make-queue) :type queue)
  (q-condition (bt:make-condition-variable))
  (q-mutex (bt:make-lock)))

(defun hasmsg (channel)
  (bt:with-lock-held ((channel-q-mutex channel))
    (queue-has-item-p (channel-queue channel))))

(defun sendmsg (channel msg)
  (bt:with-lock-held ((channel-q-mutex channel))
    (queue-add (channel-queue channel) msg)
    (bt:condition-notify (channel-q-condition channel))))

(defun recvmsg (channel &optional timeout)
  (bt:with-lock-held ((channel-q-mutex channel))
    (unless (queue-has-item-p (channel-queue channel))
      (wait-with-timeout (channel-q-condition channel)
                         (channel-q-mutex channel)
                         timeout))
    (queue-pop (channel-queue channel))))

(defun getmsg (channel)
  (bt:with-lock-held ((channel-q-mutex channel))
    (when (queue-has-item-p (channel-queue channel))
      (queue-pop (channel-queue channel)))))
