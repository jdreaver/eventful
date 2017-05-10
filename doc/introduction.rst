Introduction
============

``eventful`` is a Haskell library for building event sourced applications.

What is Event Sourcing?
=======================

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
