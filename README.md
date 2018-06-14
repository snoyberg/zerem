# zerem

[![Travis build Status](https://travis-ci.org/snoyberg/zerem.svg?branch=master)](https://travis-ci.org/snoyberg/zerem)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/t56wfu6hgkcgw4mw/branch/master?svg=true)](https://ci.appveyor.com/project/snoyberg/zerem/branch/master)

*Haskell streaming library optimized for low CPU and GC overhead*

## Motivation

Zerem is a Haskell library providing a solution to the streaming data problem.
However, unlike many other libraries, it makes a different set of trade-offs.
Instead of optimizing for usability of the library, or providing the most
useful features, it focuses on high performance from the beginning. Let's
compare against two alternative strategies to see what makes Zerem different.

Libraries like Conduit base streaming around the idea of coroutines. This
offers significant flexibility. For example, concepts like "dual consumers"
(`ZipSink`) exist. Building up more complex streaming components using a
monadic interface and the `yield`/`await` primitives is convenient. However, we
pay for this flexibility: GHC is not able to easily optimize this kind of code.
We end up with high garbage collection overhead due to the creation and
destruction of many small objects. For I/O bound tasks, this is typically fine,
as the I/O dwarfs the CPU pressure. But for CPU bound tasks, Conduit is a
non-starter. Conduit attempts to lessen the problem by including a stream
fusion framework. Unfortunately, this framework is highly sensitive to changes
in GHC, and in general cannot be relied upon to fire reliably.

Libraries like Vector are not primarily focused on streaming data. Instead,
Vector's focus is on high performance packed memory representations of data.
However, Vector includes a powerful stream fusion framework which allows the
vector allocations to be fused away in many cases. Unfortunately, like the case
of Conduit, this cannot always be guaranteed (though Vector does better than
Conduit). Additionally, since it is not primarily designed from streaming data,
some idioms, like `mapM foo >=> mapM bar`, do not fuse at all, and therefore
force buffer allocations.

These topics are covered in more detail in my blog post from 2016, [First class
stream
fusion](https://www.yesodweb.com/blog/2016/02/first-class-stream-fusion).

## The name

I've called this library Zerem, which is a Hebrew word (זרם) meaning stream (or
current of electricity). I'd originally called the proof of concept Vegito,
based on the fused character in Dragon Ball Z (Vegeta + Goku = Vegito). I
renamed the actual library to Zerem for two reasons:

1. The focus on fusion is somewhat irrelevant; the goal is high performance
   streaming, and the implementation technique is just a detail.
2. Since this library uses qualified imports, I wanted to choose an initial
   letter which isn't often used already. The `V` in Vegito is commonly used
   for Vector; to my knowledge, `Z` is not commonly used yet.

## Goal

The goal of this library is to provide a low level, high performance streaming
solution. It has yet to be demonstrated that there is a strong demand for this,
versus the existing lower performance but more user friendly libraries that
already exist. Publishing this library is the best way to gauge such demand.

If such demand really does exist, Zerem would ultimately grow to be an
ecosystem of libraries providing support for many common streaming cases, such
as data file parsing, HTTP clients, and so on, just like Conduit.

## Contributing

This project is wide open for additional contributors. The primary author
(Michael Snoyman) is most interested in getting the basic data types and helper
functions in place. If you're interested in this and want to get involved, some
ideas are:

* Add more functions to the `Zerem` module. Ultimately, we would want to get
  close to feature parity with the `Data.Conduit.Combinators` module.
* Write tutorials and examples for using this library.
* Contribute more test cases and benchmarks.
* Write add-on libraries with support for features like HTTP requests.

Feel free to open issues on Github, or [discuss on
Gitter](https://gitter.im/commercialhaskell/commercialhaskell).

## Usage

Feel free to contribute some actual information on how to use this library!
