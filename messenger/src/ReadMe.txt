The Messenger application provides a message bus service.
While the Erlang runtime provides a message feature,
it does not guarantee delivery, provides durable subscriptions, and persist the messages.

The Messenger implements the publish-subscribe design pattern.
Observers publish notices on topics. 
Subscribers subscribe to specific topics.
If a subscriber is off-line, it can replay the notices sent to a topic while it was off-line.

There is no authentication or authorization.
Publishers and subscribers do not need to establish authority to publish or subscribe on a topic.