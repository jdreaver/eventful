Introduction
============

``eventful`` is a Haskell library for building event sourced applications.

What is Event Sourcing?
-----------------------

At its core, event sourcing is the following: instead of simply storing the
current state of your application, you store a sequence of state-changing
events. Events become the atomic unit of storage in your application, and the
current state is computed from this sequence of events.

General Event Sourcing Resources
--------------------------------

We could explain event sourcing in detail here, but if you've got far enough to
google "haskell event sourcing" and you found this library, chances are you
have a decent idea of what event sourcing is. In lieu of a more complete
explanation, here are some great introduction materials:

* `This first article
  <https://engineering.linkedin.com/distributed-systems/log-what-every-software-engineer-should-know-about-real-time-datas-unifying>`_
  isn't specifically about event sourcing per se, but it is a very compelling
  and thorough introduction to storing your data in a log and using logs as a
  communication mechanism between multiple consumers. I highly recommend
  starting here.
* Introductory `talk <https://www.youtube.com/watch?v=8JKjvY4etTY>`_ by Greg
  Young.
* Great, but slightly dated `overview by Martin Fowler
  <https://martinfowler.com/eaaDev/EventSourcing.html>`_.
* `Article by Martin Kleppmann
  <https://www.confluent.io/blog/making-sense-of-stream-processing/>`_ about
  stream processing in general, but also specifically about event sourcing.

Features
--------

The goal of ``eventful`` is not to be a framework that imposes design decisions
on application developers. It is meant to be a toolbox from which you can
choose specific features to construct your application. Some features include:

* Robust event stream storage using our ``EventStore`` type. There are multiple
  backends implemented, including in-memory, SQLite, PostgreSQL, and AWS
  DynamoDB.
* Simple ``EventStore`` API so you can easily construct new event store
  backends.
* Convenience layer with common ES/CQRS abstractions, including ``Projection``,
  ``Aggregate``, and ``ProcessManager``. All of these integrate with
  ``EventStore`` so you get transparent integration with your underlying event
  storage.
* Extremely flexible serialization system. All ``eventful`` components that do
  serialization use a type parameter called ``serialized``. You provide the
  ``serialized`` type and functions to serialize/deserialize your events, and
  we handle storage to one of the available backends.
* The ``EventStore`` type exposes the monad it operates in as a type parameter;
  we don't force the use of any particular monad stack on you. If your event
  store's monad supports transactions, like ``SqlPersistT`` or ``STM`` does,
  then you get transactional semantics for free.
* ``eventful`` aims to use the most vanilla Haskell possible. We prefer value
  types over type classes, avoid any type-level computations, etc. This makes
  the core API extensible and easy to understand. If you want, you can easily
  add more advanced type system features to your application while still
  maintaining the ability to use the core ``eventful`` constructs.
