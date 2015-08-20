ecsv – A ridiculously fast CSV parser for Erlang
=====

An incremental / streaming csv parser that is 20 times faster than a
pure Erlang-implemented parser.

The heavy lifting is done through a nif. See ecsv_parser:parse/2 on
how to use it in a streaming context.

Build
-----

    $ rebar3 compile

or

    $ rebar compile



Why?
-----
When building an analytical database, data loading speed is important. A highly-optimized 100%
Erlang-based incremental CSV parser was only capable of parsing ~15mb/s of CSV data per actor.

This library solves this parsing bottleneck problem by leveraging a C-based NIF.

Some performance results (MBP from 2013, single actor, incremental CSV
line-parsing on top of a single big binary):

| # rows        | # fields      | fieldsize | throughput  | runtime | rows/s     |
| ------------- |:-------------:| ---------:|------------:|--------:|-----------:|
| 100k          | 8             | 4 bytes   | 110MiB/s    | 34ms    | 2892598    |
| 100k          | 32            | 4 bytes   | 153MiB/s    | 49ms    | 2008920    |
| 100k          | 128           | 4 bytes   | 202MiB/s    | 301ms   | 331476     |
| 100k          | 8             | 16 bytes  | 255MiB/s    | 50ms    | 1969822    |
| 100k          | 32            | 16 bytes  | 359MiB/s    | 144ms   | 692037     |
| 100k          | 128           | 16 bytes  | 360MiB/s    | 576ms   | 173490     |


CI Status
-------
![Build Status](https://travis-ci.org/zeitgeist/ecsv.svg)
