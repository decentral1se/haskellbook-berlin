---
title: Haskell in Leipzipg
subtitle: Detailed Program
---

Invited talk
------------

### Preserving Privacy with Monads ## {#russo}

*by [Alejandro Russo](http://www.cse.chalmers.se/~russo/)*

In a all-connected society, users consciously (or unconsciously) value their
privacy. Even skeptical people will recognize its importance; if they do not,
ask them to unlock their smartphone and hand it out to someone else---they will
most probably refuse!  Users want to have control on how their data gets
disseminated, specially today when private information gets handled by software
with heterogeneous trustworthiness---consider, for example, the various
smartphones apps with access to users' private photos, messages, and contacts
that exists today.  Unfortunately, current software practices are insufficient
to protect privacy: users who wish to benefit from software functionality are
often forced to grant access to their private data with no guarantees how it
gets handled. The key insight to guarantee privacy is not about granting or
denying access to private data, but ensuring that information only flows into
the appropriated places.

Information-Flow Control (IFC) is a research area dedicated to protect privacy
of data. Based on programming languages techniques, IFC scrutinizes source code
to track how data of different sensitivity levels (e.g., public or private)
flows within a program, where alarms are raised when privacy might be at stake.
IFC tools often provide specially designed compilers to build privacy-preserving
apps. Rather than building a compiler from scratch (a major task on its own),
Haskell plays a unique privileged role in this scenario: it can provide IFC
security via libraries. As long as developers program against the libraries'
API, code is secure by construction. This talk shows how to build such libraries
by specially designing monads capable to restrict the propagation of private
data. The presentation explores the different techniques used in a wide range of
libraries, namely LIO, MAC, and HLIO, where IFC is enforced dynamically (in the
form of an execution monitor), statically (by leveraging Haskell's type-system),
and as a combination of both.


**Short bio**: [Alejandro Russo] is an associate professor at Chalmers
University of Technology working on the intersection of functional languages,
security, and systems. He is the recipient of a Google Research Awards and
several grants from the Swedish research agencies Vetenskapsrådet, STINT, and
Barbro Osher foundation. Internationally, Prof. Russo worked on prestigious
research institutions like Stanford University, where he was appointed visiting
associate professor. His research ranges from foundational aspects of security
to developing tools to secure software written in Haskell, Python, and
JavaScript.

[Alejandro Russo](http://www.cse.chalmers.se/~russo/)

## Contributed Talks

### Automated Performance Measurements {#waldmann}

*by [Johannes Waldmann]*

### Generalized Algebraic Dynamic Programming: Theory and Applications in Bioinformatics and Linguistics {#stadler}
*by Sarah J. Berkemer, [Peter F. Stadler] and [Christian Hoener Zu Siederdissen]:*

### HGamer3D - a toolset for developing games with Haskell {#althainz}
*by Peter Althainz*

### Management at Algorithmic Financial Markets {#winschel}
*by Viktor Winschel*

### Project report: building a web-application with servant, lucid, and digestive-functors {#fischmann}
*by Matthias Fischmann and Andor Penzes:*

### Dependently Typed Heaps {#brunjes}
*by [Lars Brünjes]*

Using the newest type­level features in GHC, it finally becomes feasible to try some more serious
dependently typed programming in Haskell and to even prove mathematical theorems 
and have their proofs checked by Haskell's type checker.

My talk is intended to serve as an introduction to such techniques
and as an example of how to express reasonably complex invariants on the type level.
The talk should be accessible for Haskellers at an intermediate level, who don't need any special
prior knowledge on dependent types.

As an introduction, demonstration and proof of concept, 
I would like to present my little toy project on 
[Dependently Typed Heaps](https://github.com/brunjlar/heap), 
where I implement *leftists heaps* (following Chris Okasaki's 
[Purely Functional Data Structures](http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures)), 
where both the heap invariant and the leftist property are
statically ensured by the type system. (In my talk, I will of course first explain what those terms mean and how
traditional leftist heaps work.)

To encode those invariants in the Haskell type system, 
typelevel natural numbers have to be defined,
and several simple theorems have to be proved for them.

To this end, instead of directly working with a specific encoding of natural numbers, 
the abstract concept of "type­level natural number" will be defined by a type class, 
for which two implementations will be provided:

 * simple, unary "Peano" numbers, which are easy to define and understand,
 but which are also unfortunately inefficient, and

 * binary numbers, which are more efficient, but also a bit more complicated.

We can prove various properties of such natural numbers 
(decidability of the order relation etc.) 
in Haskell in order to provide a heap implementation with strong type­level guarantees at compile
time.

My benchmarks indicate that the resulting algorithm
is reasonably efficient in the sense of complexity, 
apparently still being of time complexity class
*O(n log n)* like the original algorithm, but they also show that safety comes at a steep price: 
The "unchecked" algorithm is at least an order of magnitude faster than my "safe" algorithm.

### Random access lists, nested data types and numeral systems {#komuves}
*by Balazs Komuves*

### Plugin Architectures in Haskell {#graf}
*by Sebastian Graf*

### Store: An Efficient Binary Serialization Library {#kant}
*by Philipp Kant*

### Csound-expression Haskell framework for computer music {#kholomiov-talk}
*by Anton Kholomiov*

(Note the corresponding [tutorial](#kholomiov-tutorial).)

### Simple blog engine with shape functors and generic eliminators for ADTs {#penez}
*by Andor Penzes*

## Tutorials

### Efficient signal processing using Haskell and LLVM {#thielemann}
*by [Henning Thielemann]*

Haskell has so many features that are useful for signal processing:
Lazy evaluation allows for infinite lists,
simultaneous processing of a sequence of signal transformations and feedback,
monads allow for proper handling of coherent and incoherent sources of noise,
the type system allows for handling of sample rates via phantom types
and for hiding internal filter parameters in opaque types.
However, especially when relying on a lot of laziness
your Haskell programs will be pretty slow in any Haskell implementation.
In order to get reasonable processing speed
you need to switch from lazy lists of sampled displacements
to lazy lists of chunks of unboxed arrays.
This compromise is pretty close to how signals are usually stored
in signal processing packages written in machine oriented languages.
However, GHC still fails to generate efficient code
in cases that are hard to predict.
It may be due to missing inlining, too much sharing of functions
or too much laziness.

With the package `synthesizer-llvm`
we address the issue of efficient signal processing.
We use the LLVM Just-In-Time compiler
in order to perform signal processing with maximum speed.
LLVM provides optimization passes and vector instructions
that enable us to achieve this goal.
Our package also provides the programmer
precise control over strict evaluation,
code and data duplication or sharing.

In the tutorial we examine the examples included in the package,
experiment with them and learn about the basic concepts of the package this way.
The examples include:

 * Signal producers like oscillators and noise generator,
 * Frequency filters and how to work with internal filter parameters
   beyond the restricting scheme of audio and control rates
   found in other digital signal processing frameworks,
 * Sample value types: Stereo sounds, binary logic signals, fixed-size arrays
 * Causal signal processes with arrows,
   sharing and feedback with proven absence of deadlocks,
 * Express arrows by plain functions,
 * Composing sequences of sounds, composing music from tones,
 * MIDI control of sounds,
 * Integration with ALSA and JACK,
 * Vectorisation.

I already gave a tutorial on LLVM at [HaL-9](https://iba-cg.de/hal9.html)
and chose simple signal processing as application.
In this tutorial I will ignore the details of LLVM
and just concentrate on the signal processing part.
I do not plan to give an extensive introduction to signal processing,
the focus is on how to get things done with `synthesizer-llvm`.

Since LLVM changes slightly with every release
my LLVM bindings got a bit out of date.
I am currently updating them.
The tricky part to install is the package `llvm-ffi`.
If you want to attend the tutorial and want to code with us,
please send me your e-mail.
I will then notify you when my packages are ready for installation
and I will give you hints on the installation.


References:

 * [synthesizer-llvm](http://hackage.haskell.org/package/synthesizer-llvm)
   package at Hackage
 * Paper "Compiling Signal Processing Code embedded in Haskell via LLVM"
   at [arXiv](http://arxiv.org/abs/1004.4796)
 * [Demo of a song](https://www.youtube.com/watch?v=4zX0OnLGVV4)
   that is played live over a synthesizer-llvm based software synthesizer.


### Workshop: creating computer music with Haskell {#kholomiov-tutorial}
*by [Anton Kholomiov]*

(Note the corresponding [talk](#kholomiov-talk).)

### Ten example uses of monads {#schuster}
*by Philipp Schuster*


[Johannes Waldmann]: http://www.imn.htwk-leipzig.de/~waldmann/
[Peter F. Stadler]: http://www.bioinf.uni-leipzig.de/
[Christian Hoener Zu Siederdissen]: http://www.bioinf.uni-leipzig.de/~choener/index.html
[Lars Brünjes]:  https://github.com/brunjlar
[Anton Kholomiov]: https://github.com/anton-k
[Henning Thielemann]: http://dr.henning-thielemann.de/

