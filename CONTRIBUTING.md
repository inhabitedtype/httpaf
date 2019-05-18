All contibutors should be aware of the goals and non-goals described below
before submitting issues or pull requests, as it's a necessary precondition for
contibutions to align with them before they will be considered for review.

> People of github,
>
> 1. Raise issue
> 2. Discuss, propose solution
> 3. Implement
>
> Sometimes it's ok to skip a step, but pls don't change the order.

&mdash; [Dave Cheney][process]

[process]: https://twitter.com/davecheney/status/676645735647940608

The maintainers of this library take code review seriously. They also value
their time as much as you value your own. Pull request should be ready for code
review when they are submitted. If you have any questions about feature or
enhancement you'd like to submit, open an issue and ask away.

## Goals

The goal of http/af is to provide an efficient, scalable, and faithful
implementaiton of the HTTP 1.1 protocol, as described primarily in [RFC7230][]
and [RFC7231][]. The core library will be agnostic to the I/O or event sources,
and is designed to work as best as possible with all present and future
sources. It will primarily target x86 deployments.

The library will support basic client and server abstractions for use with both
[Async][]- and [lwt][]-based runtime libraries.

[Async]: https://opensource.janestreet.com/async/
[lwt]: https://ocsigen.org/lwt/4.1.0/manual/manual

[RFC7230]: https://tools.ietf.org/html/rfc7230
[RFC7231]: https://tools.ietf.org/html/rfc7231

## Non-goals

http/af does not support compilation to JavaScript or any other build target
that will significantly compromise performance on x86 architectures. See
inhabitedtype/angstrom#95 for an indication of what this means.

http/af is not a web framework, though it can be used to build such a library.

http/af is not a user agent library, such as [mechanize][], though it can be
used to build such a library.

[mechanize]: https://metacpan.org/pod/WWW::Mechanize
