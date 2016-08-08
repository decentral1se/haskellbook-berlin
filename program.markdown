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

Dynamic programming (DP) algorithms are pervasive in bioinformatics and
linguistics (and more exotic domains) but typically implemented as
“one-shot” solutions.

In addition to just a growing number of applications, there seems to be a trend
towards more complex algorithms as well. We recognize at least these degrees of
increasing complexity and certain desiderata to control this complexity:

(i) More refined search spaces which require larger sets of rules to describe.
Less *ad-hoc* ways to design these improve confidence in construction.

(ii) The desire to calculate more than just a single optimal result, be it
ensemble-style posterior probabilities or classified dynamic programming
benefits from automatic code generation.

(iii) Many DP algorithms operate on sets, trees, or other non-string data
structures. For these more complex structures, both theory and support for
implementations are required.

(iv) On a more basic level, developers have to deal with writing code that
should be free of bugs, maintainable, and sufficiently efficient.

In the talk, we will provide an introduction to *generalized Algebraic
Dynamic Programming* which simplifies design and implementation of dynamic
programming algorithms considerably. Users of our system can combine individual
algorithms like building blocks, have outside algorithms be derived fully
automatically, design algorithms on a multitude of input structures (we
currently provide implementations for strings, trees, and sets), and even go
beyond context-free grammars.

We will focus on recent work on dynamic programming for tree-like data
structures. Dynamic programming for tree structures is arguably less well known
compared to “canonical text book examples” but has interesting applications
in bioinformatics and linguistics, as well as providing easy-to-understand but
non-trivial to implement algorithms.

Compared to our earlier work on dynamic programming on strings, or dynamic
programming on sets, we now have to deal with input structures that are more
complex. Strings are inherently one-dimensional, and recursively decomposed by
removing the first or last element, or splitting the string in two. Sets on the
other hand are completely unstructured and recursively decomposed by the
removal of subsets, or individual vertices. Trees and forests on the other hand
are two-dimensional structures, with decompositions into both, subforests of
siblings, and subforests induced by the removal of local root nodes during
recursive decomposition.

The talk will feature an introduction to a small number of dynamic programming
problems on trees and forests with applications in alignment of structuresd
RNA, as well as alignment of well-formed grammatical structures (sentences) in
human languages.

We discuss single, optimal solutions as well as Inside-Outside solutions which
provide answers for the ensemble of all possible solutions for a given problem
instance, for example the probability of individual nodes of two trees to be
aligned.

We will cover the extension of *ADPfusion* [^HOE:2012], which was developed for
string-type algorithms, to *generalized Algebraic Dynamic Programming* [^HOE:2012] [^HOE:HOF:2014] [^HOE:PRO:2015] [^BER:STA:2016] and the underlying implementation as an embedded domain-specific language directly
in Haskell.

The talk will feature an introduction into the topic of Dynamic Programming
based on string distance problems.

[^BER:STA:2016]: Sarah Berkemer, Peter F. Stadler, and Christian Höner zu Siederdissen.
General reforestation: Parsing trees and forests. submitted, 2016.

[^HOE:2012]: Christian Höner zu Siederdissen. Sneaking Around concatMap: Efficient
Combinators for Dynamic Programming. In Proceedings of the 17th ACM
SIGPLAN international conference on Functional programming, ICFP ’12,
pages 215–226, New York, NY, USA, 2012. ACM.

[^HOE:HOF:2014]: Christian Höner zu Siederdissen, Ivo L. Hofacker, and Peter F. Stadler.
Product Grammars for Alignment and Folding. IEEE/ACM Transactions
on Computational Biology and Bioinformatics, 12(3):507–519, 2014.

[^HOE:PRO:2015]: Christian Höner zu Siederdissen, Sonja J. Prohaska, and Peter F. Stadler.
Algebraic dynamic programming over general data structures. BMC Bioin-
formatics, 16, 2015.

### HGamer3D - a toolset for developing games with Haskell {#althainz-talk}
*by Peter Althainz* (also see the corresponding [tutorial](#althainz-tutorial).)

HGamer3D is a toolset for programming 3D games with Haskell. From the early stages and through some experimental times the project has made progress to a point where some stability has been reached and real games can be created with it. To be useful for a developer such a toolset needs to provide solutions for such seemingly trivial topics as:

* bindings to C++ libraries are needed
* threading needs to be encapsulated between Haskell and underlying libraries
* everything should run on multiple platforms, for example Linux, Windows, Mac
* install procedure should be easy and should work flawlessly
* games should be easy to distribute
* API should be easy to use, should provide good semantics and should bridge OO and functional world
* API should be comprehensive enough to be useful, but high level enough to be usable
* performance should be sufficient
* tooling is needed to handle media and 3D scene creation

For all those topics HGamer3D has some answers but in a short talk like the one presented they cannot be addressed in detail. I also think that most of the audience is probably more interested in a quite pragmatic question: what can I do with it, why should I choose HGamer3D for my next game project, are there any running games programmed with it, if not, why not?

The talk will therefore be split into a small introduction, which just gives an overview of HGamer3D including a simple list of solutions to the topics above and afterwards the main body will be about:

* an introduction to the API structure
* a Demo of a real game, programmed with HGamer3D
* API snippets showing how central features are implemented in the Demo
* an overview over the feature coverage, what is implemented for 3D game programming, what is missing

Some API examples, to give you a first impression:

```haskell

-- create a camera
eCam <- newE hg3d [
    ctCamera #: FullViewCamera,
    ctPosition #: Vec3 1 1 (-30.0),
    ctLight #: Light PointLight 1.0 1000.0 1.0
    ]

-- create a cube
eGeo <- newE hg3d [
    ctGeometry #: ShapeGeometry Cube,
    ctMaterial #: matBlue,
    ctScale #: Vec3 10.0 10.0 10.0,
    ctPosition #: Vec3 0.0 0.0 0.0,
    ctOrientation #: unitU
    ]

-- rotate cube
rotateCube eGeo = forever $$ do
    updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
    sleepFor (msecT 12)
```

To not oversell: no there are no big games programmed with HGamer3D, yet and yes, the demo is still a "toy" example. But it is a fully functional example with sound, input, graphics, gui and gameplay.

The talk will close with a summary, suggesting where HGamer3D is a good fit and where not, it will also try to give some reasoning on the state of the Haskell ecosystem with regards to game programming. Finally an outlook will be shown on possible future directions of HGamer3D. 

### Management at Algorithmic Financial Markets {#winschel}
*by Viktor Winschel*

The financial industry, as all sectors of the economies, is about to be swamped with new algorithmic solutions and processes. I will walk through an example of a usual insurance company or bank and explain the involved tasks that can be automated by an integrated algoritmic approach to management towards a more or less fully digital company. Along these lines I will show how our foundational research in the last years between economics, game theory and formal semantics of programming languages can help to tailor a domain specific language for the management at algorithmic financial markets.

Our approach to management at financial markets is three folded: macroeconomics, game theory and smart contracts. We are providing a revolutionary approach to macroeconomic modelling via traditional physical modelling techniques based on analytical and synthetical differential geometry. Besides clarifying some century old modelling problems within macroeconomics this approach also lends itself naturally to an implementation in functional programming languages. The formalization within category theory provides the framework for the modelling of financial market participants like one’s own banks or insurances and those of the compatitors. The strategic decision models within the macroeconomic models are to be based on our new higher-order approach to game theory based on programming language semantics. The third part of our approach to financial markets are smart contracts like those underlying ethereum and bitcoins which model the elements of balance sheets but also the elements of organizational contracts.  I will dwell into some details of these topics and try to connect to the underlying mathematical machinery that may be more familiar to the Haskell community.

Essentially we are developing a category theory based DSL for our kind of game theory with the compiler being implemented in Haskell in line with the mathematical foundation of Haskell that is at the core of our formalizations. The new features of our games are compositionality with a formal semantics for the composed behaviour, a visual representation of games by string diagrams adapted from quantum computing, a behavioural abstraction by higher-order functions generalizing utility maximization of standard economics as its main model of behaviour and coalgebraic operators for the infinite repetition of games. The direction of our work, based on our compositionality principle, is to establish a scientific base for macroeconomics and money theory underlying the operation of financial markets. We are currently setting up a start-up company in order to market this research. Haskell enthusiasts are welcomed to join our efforts.

Our company emerged from and is still cooperating with a research community based at UK universities, Glasgow, Leicester, London and Oxford and some others in the Netherland and Germany and Switzerland collaborating at the borders of economics and computer science. This movement took off around 2010 with a first contact of Viktor Winschel to the Oxford Quantum compunting group around Samson Abramsky and Bob Coecke. The work so far has been very foundational with papers and PhD thesis on string diagrams and categorical higher-order game theory. By now we have clarified what can actually be done with these tools within economics and push now into industry. The involved researchers are very enthusiatic in that the surprising common foundations of economcis and computer science opens up various interesting possibilities and applications of high end mathematics in very practical fields.

The format of my presentation will be a usual slide show. The indented audience are Haskell programmers who are also interested in the common mathematical structures of programming language semantics and economics and game theory. Even so I will not dwell into very detailed and formal category theory or coalgebras it might be beneficial to see the point of basing economics and computer science on this foundation. Also there might be Haskell programmers who are interested in joining the developing FinTech industry in Frankfurt and possibly colaborate with or join our startup company.

For further information, look at these conferences and papers:

* <http://www.dagstuhl.de/en/program/calendar/semhp/?semnr=14182>
* <http://www.dagstuhl.de/en/program/calendar/semhp/?semnr=15042>
* <http://homepage.tudelft.nl/c9d1n/lsb3/lsb3.html>
* <http://www.lorentzcenter.nl/lc/web/2016/840/info.php3?wsid=840&venue=Oort>
* <http://arxiv.org/find/cs/1/au:+Winschel_V/0/1/0/all/0/1>
* <http://arxiv.org/abs/1603.04641>


### Project report: building a web-application with servant, lucid, and digestive-functors {#fischmann}
*by Matthias Fischmann and Andor Pénzes:*

Servant is a library for writing HTTP routing tables on the type level.  It is commonly used for micro-services and for delivering json data to single-page apps, but it is possible to use it for delivering web pages and forms, and build a low- or no-js web application with it.

Aula ([code](https://github.com/liqd/aula/), [blog of the pilot project](http://aula-blog.website/)) is such a web application.  It is based on [servant](https://hackage.haskell.org/package/servant) for request processing, [lucid](https://hackage.haskell.org/package/lucid) (a sibling package of [blaze](https://hackage.haskell.org/package/blaze-html)) for html content rendering, and [digestive-functors](https://hackage.haskell.org/package/digestive-functors) for web form processing.  Sticking these parts together proved both non-trivial and very rewarding.  Benefits are clear separation between application logic, html rendering, form data validation, and tedious details like authentication or CSRF token handling.

Aula is AGPL.  We plan to refactor parts of the code into general-purpose libraries.

In this talk, we look at pieces of the Aula code base and discus strengths and weeknesses of our approach.  We will also give a summary of our experience with Haskell tooling and deployment.

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

Random access lists, as introduced by Okasaki[^Oka95] [^Oka96], are persistent list-like
data structures with faster lookup than linked lists. This talk will be an explanation
of these data structures and their connection to numeral systems, with a small twist.
I claim no originality at all.

The core idea is that you can take a positional numeral system, like the usual binary,
ternary etc. numeral systems (or more esoteric ones, like the skew binary system), and
"categorify it", turning the length of a list (a number) into an actual sequence data type.
In place of the digits you will have tuples of (full) trees: For example in the decimal system,
if the digit at the hundreds place is 3, that will translate to a triple of 10-way trees,
each containing 100 elements at the leaves.

The usual linked list corresponds to the unary system; but with the non-unary systems, you
get O(log(k)) lookup, while modifying the left end of the list still remains on average O(1)
(worst case is typically O(log(n)), though guaranteed O(1) can be achieved too). They can be
also more compact in memory than linked lists (with GHC's in-memory representation).

Okasaki used the standard tree representation, which is one of the first data types one meets
when learning Haskell; for example, a leaf binary tree is represented as

```haskell
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
```

However, full binary trees can be also represented by a nested data type:

```haskell
data Tree' a
  = Single a
  | Double (Tree' (a,a))
```

The word nested[^BM98] refers to the fact that the type parameter changes during the recursion.
Functions operating on such types require polymorphic recursion, an interesting language feature.
The nested binary tree has two advantages over the usual representation: First, the type system
guarantees that the tree is full (that is, every leaf has the same depth); second, since it lacks
the extra indirection at the leaves, it takes less memory (by two machine words per element, with GHC).
While the extra indirection can be also eliminated by modifying the original tree definition,
that would result in less elegant and more complicated code.

This idea can be applied to our list-like data structures, giving for example the following
very simple representation of leaf binary random-access lists:

```haskell
data Seq a
  = Nil                   -- the empty sequence
  | Zero   (Seq (a,a))    -- a sequence of even length
  | One  a (Seq (a,a))    -- a sequence of odd length
```

The implementation of the usual list operations is also very straightforward and simple
(a Haskell implementation can be found at[^KD16]). The same idea can be also applied to
the other variations. The resulting data structures are somewhat reminiscent of finger trees[^HP06],
which are also implemented using nested data types; however, these are much simpler (of course,
finger trees support more types of efficient operations).

A variation of the above idea uses the so-called skew numeral systems; these allow (modulo
implementation details) guaranteed O(1) cons, in exchange for a slightly more complicated
implementation. The resulting data structures use trees with data on both the nodes and the
leaves (which also results in more compact in-memory representation). Okasaki uses the skew
binary number system.


[^BM98]: Richard S. Bird and Lambert G. L. T. Meertens, Nested datatypes, Proceedings of the
         Mathematics of Program Construction, MPC '98, Springer-Verlag, 1998, pp. 52--67.

[^HP06]: Ralf Hinze and Ross Paterson, Finger trees: A simple general-purpose data structure,
        J. Funct. Program. 16 (2006), no. 2, 197--217.

[^KD16]: Balazs Komuves and Peter Divianszky, The nested-sequence Haskell library, 2016,
        http://hackage.haskell.org/package/nested-sequence.

[^Oka95]: Chris Okasaki, Purely functional random-access lists, In Functional Programming
        Languages and Computer Architecture, ACM Press, 1995, pp. 86--95.

[^Oka96]: Chris Okasaki, Purely functional data structures, Ph.D. thesis, Carnegie Mellon
        University, 1996.

### Plugin Architectures in Haskell {#graf}
*by Sebastian Graf*

Creativity and discontent gave rise to a number of different approaches and even more libraries for customizing applications written in Haskell. Most of them rely on the GHC API in one way or another, so over time many of said libraries have gone obsolete or superseded by another contender. As evidenced by posts on reddit and Stack Overflow, that led to huge confusion in the community, to a point where no-one has a clear overview of the current possible alternatives for extending a Haskell application after compilation.

This talk will try to give an overview of available means to loading and executing (mostly Haskell) code (mostly) at runtime, discussing its impliciations on type safety and versioning. Packages will be categorized by the taken approach and the most prominent proxy per category will have to serve for some live coding adventures. The pitch:

- Embedding a scripting language via lua bindings and hint
- XMonad and Yi style configuration via Dyre
- Loading separately compilated Haskell code from a shared library with dynamic-loader

The goal is to collect resources in a common place and present the results in a manner fit for a talk, while an accompanying blog post will go into more detail to also separate similar approaches.

### Store: An Efficient Binary Serialization Library {#kant}
*by Philipp Kant*

Serialization, the process of converting structured data into a
sequence of bytes, is a necessary step in many software programs.
When designing a serialization library, there are many trade-offs that
have to be considered:

- Is interoperability with other languages required?
- What about interoperability between different hardware architectures?
- Backwards compatibility: is it possible to make additions to the
  serialization format without invalidating previously serialized data?
- How important is the size of the resulting serialization
- How important is the speed of serialization and deserialization?
- How easy is the library to use?  How much code has the user to
  provide in order to support serialization of a custom data type?

The Haskell ecosystem features a selection of serialization
libraries, each of which make different choices for the trade-offs
above.

We present _store_, a recent addition to the line-up of serialization
libraries that prioritizes heavily on speed.  It was designed with the
main use-case of distributed high-performance computing in mind.  By
cutting back on features that are not necessary in that context, and
by exploiting assumptions about the typical data payload, it succeeds
to provide routines for serialization and deserialization that are
faster than those of the libraries that cover the general case.

For example, _store_ assumes that

- There is no need to provide compatibility between different
  architectures and/or versions of the serialization scheme.
  In particular, _store_ can use the host byte order, instead of
  converting to a fixed endianness.  This allows for code that is both
  simpler, and more efficient.

- The size of the serialized data can easily be determined prior to
  serialization, and is small enough to fit into memory.
  Thus, _store_ can allocate the whole memory for serialization in one
  step, without needing to grow buffers.

_Store_ has pre-defined instances for most common datatypes.  For
convenience, the library provides methods to implement custom
instances using Generics or template Haskell.  It also features a thin
streaming layer that allows incrementally consuming input from
streaming sources, such as a network connection.

This talk will give an overview of the design principles behind
_store_, and demonstrate that they lead to efficient code.

### Csound-expression Haskell framework for computer music {#kholomiov-talk}
*by Anton Kholomiov* (also see the corresponding [tutorial](#kholomiov-tutorial).)

The paper presents modern Haskell framework for creation of computer music.
It's called [csound-expression](http://github.com/spell-music/csound-expression/).  It's
EDSL for Csound audio programming language. The functional programming paradigm
can greatly enhance the process of text-based music production. We are going to
look at functional model for computer music.  It combines many aspects of
computer music. With the library we can create instruments from scratch apply
instruments to scores, trigger them with streams of events. We can create UIs
to control our music.

The key guiding principle of API design is modularity. Creation of
self-contained musical objects that can provide building blocks for the tower
of abstractions. The library is designed in such a way that every concept is
self contained and the whole program can be built from expressions. This
feature allows us to construct music in the interactive style. We can create
music in the REPL.

### Simple blog engine with shape functors and generic eliminators for ADTs {#penez}
*by Andor Pénzes*

Processing complex information via generic eliminators (GE), type holes,
algebras and catamorphisms for ADTs are tools in the functional programming toolbox.
I would like to present a use case where combined approach of those tools is
pushed to the limit. As an experiment, a simple blog engine is implemented. The result
shows how a developer can concentrate on solving particular problems, how
algebras and catamorphisms support the separation of concerns and readability, how type holes gives extra power to the abstractions.

Besides the current example, we can make comparison between different
approaches, using lenses or not using generic eliminators et al.


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
*by [Anton Kholomiov]* (also see the corresponding [talk](#kholomiov-talk).)

At the Workshop we are going to learn to create computer music with Haskell. 
We are going to produce buzzes and blips right in the REPL. Right in the ghci
you can create a midi-based instrument and complement it with drums and harmony
from your favorite library of samples or create the melody from scratch with scores
or event streams. Also we are going to learn how to create the UI-controllers on the 
fly with applicative style programming.

At the end of the workshop you should become en-armed with set of pro-quality synthesizers,
tools to create sample based music, trigger them the music in real time. We should discuss
some features unique to text-based programming. Have a glimpse at techniques used in musical generative art.

We are going to use the library
[csound-expression](http://github.com/spell-music/csound-expression/). It's a Haskell framework for sound design and music composition. It embeds very
powerful audio programming language Csound into Haskell. It's a Csound code generator.
The [Csound](http://csound.github.io/) is 30 years old audio programming language that embodies the wisdom of many researchers.
It has active community and it continues to evolve. It has the wide set of features (subtractive, granular,
waveguide, sample-based synthesis, physical modeling, spectral processing, emulators for many analog
synthesizer components)

The Haskell library complements the Csound with expressive language. We can use the great low-level
audio units of Csound and scheduler with higher level features of Haskell like higher order functions, rich
data type system, body of libraries for advanced data types.

Prerequisites:

To follow along you need to install several things:

* [csound-expression](http://hackage.haskell.org/package/csound-expression). It's a main library (available on hackage).
* [Csound compiler](http://csound.github.io/). The thing that converts code to audio (available on official site works on all platforms).
* other goodies based on core library: [csound-catalog](http://hackage.haskell.org/package/csound-catalog) and [csound-sampler](http://hackage.haskell.org/package/csound-sampler).
    They contain ready to use high quality patches (available on Hackage) and tiny DSL to compose music with audio-clips.
* the archive with audio files (drum loops and pads) that we are going to use. I'll publish it when closer to the date.

When everything installed check that it works. Open ghci import the module `Csound.Base` and type:

~~~
> dac (testDrone3 220)
~~~

Hit Ctrl+C to stop the playback. The library has minimal Haskell dependencies and should be easy to install. If something will go wrong with installation 
drop me a line on github or on the email that you can find on Hackage page of the project.

The library is really huge though the core is very simple and minimal. So I
recommend to read/skim through the [user guide of the
library](https://github.com/spell-music/csound-expression/blob/master/tutorial/Index.md).
That way you can get more out of the workshop.


### Ten example uses of monads {#schuster}
*by Philipp Schuster*

### HGamer3D - do it yourself {#althainz-tutorial}
*by Peter Althainz* (also see the corresponding [talk](#althainz-talk).)

During the tutorial we will install HGamer3D development tools on your computer and go through a number of exercises, along the following path:

* Install tooling, including compiler, graphic tools, editor and HGamer3D (15 min)
* The 3D coordinate system and API (10 min)
* Item Hierarchies (5 min)
* Getting media into your game (10 min)
* Break (10 min)
* Challenge: "program your own game": given a vision of a game and essential code snippets to program certain features, you are left on your own to build a running game (30 min)
* Distribute your game (10 min)

Prerequisites: Linux, Windows or OS X installed. Internet connection to download the tooling. 


[Johannes Waldmann]: http://www.imn.htwk-leipzig.de/~waldmann/
[Peter F. Stadler]: http://www.bioinf.uni-leipzig.de/
[Christian Hoener Zu Siederdissen]: http://www.bioinf.uni-leipzig.de/~choener/index.html
[Lars Brünjes]:  https://github.com/brunjlar
[Anton Kholomiov]: https://github.com/anton-k
[Henning Thielemann]: http://dr.henning-thielemann.de/

