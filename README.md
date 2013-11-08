# trivial-channels

This is a very, very trivial implementation of channels (and a
queue).  I find myself using it in a few places where very trivial
message passing is needed and a more complex, robust solution would be
overkill.

```lisp
(let ((channel (make-channel)))
  (sendmsg channel 'anything)
  (recvmsg channel)) ;; => ANYTHING
```

## API

* `(make-channel)`: Make a channel
* `(sendmsg CHANNEL OBJECT)`: Send `OBJECT` on `CHANNEL`
* `(recvmsg CHANNEL &optional TIMEOUT)`: Receive on `CHANNEL`,
  optionally timing out after `TIMEOUT` seconds
* `(getmsg CHANNEL)`: Get a message if available, or `NIL`
* `(hasmsg CHANNEL)`: Whether the channel has a message.  No guarantee
  it will still have one after the call, if there are multiple
  receivers sharing a channel.

Notably, `recvmsg` supports a timeout.  These functions properly lock
and it's safe to share a channel between threads (that being the
entire purpose).

## Queues

The queue used to implement this is also exported, since it's the
majority of the code, and it's sometimes handy to have a trivial
queue, too.

Queues do **not** lock.

Queues are implemented with conses.

* `(make-queue)`: Make a queue
* `(queue-head Q)`: Head of the queue
* `(queue-tail Q)`: Tail of the queue
* `(queue-add-cons Q CONS)`: `CONS` becomes the tail of the queue; its
  CDR may be destructively modified
* `(queue-add Q ITEM)`: Add `ITEM` to the tail of the queue
* `(queue-push Q ITEM)`: Push `ITEM` onto the front of the queue
* `(queue-pop Q)`: Pop and return and item from the queue, or NIL
* `(queue-pop-cons Q)`: Pop and return the cons from the queue, or NIL
* `(queue-has-item-p Q)`: If there are items in the queue, or NIL
* `(queue-peek Q)`: The front item in the queue, or NIL
* `(queue-pop-to Q1 Q2)`: Pop an item from `Q1` and add it to `Q2`
  without consing
* `(queue-prepend-to Q1 Q2)`: Remove all items from `Q1` and prepend
  them to `Q2` without consing; no value returned

All add or remove type operations return the item (or cons) being
handled, unless otherwise noted.
