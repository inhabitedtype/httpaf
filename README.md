# http/af

http/af is a high-performance, memory-efficient, and scalable web server for
OCaml. It implements the HTTP 1.1 specification with respect to parsing,
serialization, and connection pipelining as a state machine that is agnostic to
the underlying IO mechanism, and is therefore portable across many platform.
It uses the [Angstrom][angstrom] and [Faraday][faraday] libraries to implement
the parsing and serialization layers of the HTTP standard, hence the name.

[angstrom]: https://github.com/inhabitedtype/angstrom
[faraday]: https://github.com/inhabitedtype/faraday
[![Build Status](https://travis-ci.org/inhabitedtype/httpaf.svg?branch=master)](https://travis-ci.org/inhabitedtype/httpaf)

## Installation

Install the library and its dependencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install httpaf
```
## Performance

The reason for http/af's existence is [mirage/ocaml-cohttp#328][328], which
highlights the poor scalability of cohttp. This is due to a number of factors,
including poor scheduling, excessive allocation, and starvation of the server's
accept loop. Here is a comparison chart of the data from that issue, along with
data from an async-based http/af server. This server was run on a VM with 3
virtual cores, the host being circa 2015 MacBook Pro:

[328]: https://github.com/mirage/ocaml-cohttp/issues/328

![http/af comparsion to cohttp](https://raw.githubusercontent.com/inhabitedtype/httpaf/master/images/httpaf-comparison.png)

The http/af latency histogram, relative to the cohttp histograms, is pretty
much flat along the x-axis. Here are some additional statistics from that run
(with latencies in milliseconds):

```
#[Mean    =       27.719, StdDeviation   =       31.570]
#[Max     =      263.424, Total count    =      1312140]
#[Buckets =           27, SubBuckets     =         2048]
----------------------------------------------------------
  1709909 requests in 1.00m, 3.33GB read
```

## Usage

WIP

## Development

To install development dependencies, pin the package from the root of the
repository:

```bash
opam pin add -n httpaf .
opam install --deps-only httpaf
```

After this, you may install a development version of the library using the
install command as usual.

Tests can be run via jbuilder:

```bash
jbuilder runtest
```

## License

BSD3, see LICENSE files for its text.
