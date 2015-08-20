ecsv – A ridiculously fast CSV parser for Erlang
=====

An incremental / streaming csv parser that is 20 times faster than a
pure Erlang-implemented parser.

The heavy lifting is done through a nif. See ecsv_parser:parse/2 on
how to use it in a streaming context.

Build
-----

    $ rebar3 compile


CI Status
-------
![Build Status](https://travis-ci.org/zeitgeist/ecsv.svg)
