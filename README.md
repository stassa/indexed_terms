# indexed_terms
Indexed data structures for Swi-Prolog

Predicates to facilitate creating, initialising, accessing and manipulating
indexed terms, to be used as the building blocks of arrays, vectors and
matrices.

In the context of this module, "Indexed terms" (iterms, for short) are compound
Prolog terms meant to be treated as named collections taking advantage of arg/3
to access data by index and nb_setarg/3 to modify these data in-place.

## Using indexed_terms

indexed_terms is designed to be used with Swi-Prolog and will probably not be
compatible with other Prolog systems. A version of Swi-Prolog from 7.0 above is
required.

This module is meant to be used as a library. Add it to your project and include
it in your code in the usual way, with a use_module/1 directive.

    ?- use_module(src(indexed_terms)).
    ...

If you want to inspect the code, or run the tests, start the project using its
load file, (project root)/ load_project.pl. On Windows you can double-click the
project to bring up the Swi-Prolog IDE, along with the documentation browser.
Starting the project this way will also load and run the tests. If you want to
run the tests manually, use run_tests/[0,1]:

    ?- run_tests.
    ...
    ?- run_tests(window).
    ...
    ?- run_tests(window:window_basic_usage_ascending).
    ...

Inspect the test file, src(indexed_terms.plt) for more information on the tests
themselves and on the usage of the predicates in this module.

## Iterm indices 

The arity of an iterm may range from 0 to whatever is the effective limit of
your hardware. Accordingly, the valid range of indices of an iterm, is an
enumeration of the arguments of the iterm, which informs the choice of iterm
starting indices: the first index of an iterm is either 1, for an iterm with at
least one argument, or 0, for an iterm with 0 arguments.

In other words, for an iterm of arity a, with starting index i:


    ```
          / 1    if a > 0
    i = <
          \ 0    if a = 0
    ```

Additionally, predicates in this module allow negative indices to be used to
access iterm arguments in descending order. For example, an iterm of arity 3 may
be accessed with any of the indices in {-3,-2,-1,1,2,3}, where -3 and 1 would
both access the first argument of the iterm. Refer to the documentation of
iterm_index/3 for a more detailed discussion of positive and negative iterm
indices.

The choice of variable starting indices, may appear a little peculiar - in most
languages the indices of random-access data structures either start at 0, or 1
(though a few, like Ada and Fortran allow the user to specify their own range,
which this module doesn't). When in doubt, iterm_index/3 may be used to clarify
any misunderstandings about the valid range of iterms' indices.

## Iterm creation and manipulation. 

Predicates in this module fall into two broad types: on the one hand, predicates
for creating, indexing and accessing iterms, and on the other, predicates for
selecting ranges of arguments of iterms between two indices.

### Creating iterms

Predicates iterm/3, typed_iterm/4, blank_iterm/3, init_iterm/4, sequence/5 and
sequence/6 may be used to create iterms from a functor name and list of
arguments, or a number denoting its arity.

iterm/3 creates an iterm from a functor name and a list of arguments, without
any restrictions except those that apply to compound_name_arguments/3, which
this predicate is basically just a thin wrapper around.

    ?- iterm(v, [a,b,c,d], T).
    T = v(a, b, c, d).

typed_iterm/4 creates iterms from a list of arguments that must belong to one of
a set of argument types, defined in iterm_types/1. Attempting to create a typed
iterm with arguments of the wrong type raises a type_error. Note however that
any type-checking required after the typed iterm is created is up to the user.

    ?- typed_iterm(v, atom, [a,b,c,d], T).
    T = v(a, b, c, d).
    
    ?- typed_iterm(v, number, [a,b,c,d], T).
    ERROR: Type error: `number' expected, found `a' (an atom)

blank_iterm/3 creates iterms of a given arity with all their arguments as
unbound variables. init_iterm/4 does the same, but binds all arguments to a
given value. You can use these two when you know the size of your data but not
yet its final values.

    ?- blank_iterm(v, 8, T).
    T = v(_1168, _1170, _1172, _1174, _1176, _1178, _1180, _1182).
    
    ?- init_iterm(v, 5, 0, T).
    T = v(0, 0, 0, 0, 0).

The common numeric computing task of creating a vector of zeroes, or other
default values, or a sequence of numbers, is encapsulated by init_iterm/4 and
also sequence/5 and sequence/6.

    ?- sequence(s,1,10,1,S).
    S = s(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).
    
    ?- sequence(s,1,5,1,2,S).
    S = s(1, 1, 2, 2, 3, 3, 4, 4, 5, 5).

iterm_index/3 and iterm_value/3 may be used to index through an iterm
nondeterministically, by generating its valid range of indices.

Additionally, iterm_index/3 may be used to map between negative and positive
indices, while iterm_value/3 accepts negative indices to allow accessing iterm
elements "from the back". See more information on indices in the following
section.

    ?- sequence(s,1,5,1,S), iterm_index(S,I,O) .
    S = s(1, 2, 3, 4, 5),
    I = -5,
    O = 1 ;
    S = s(1, 2, 3, 4, 5),
    I = -4,
    O = 2 .
    (etc)

    ?- sequence(s,1,5,1,S), iterm_value(S,2,V) .
    S = s(1, 2, 3, 4, 5),
    V = 2.

    ?- sequence(s,1,5,1,S), iterm_value(S,-2,V) .
    S = s(1, 2, 3, 4, 5),
    V = 4.

iterm_value/3 may be further used to instantiate arguments of an iterm to a
desired value. The two different use cases, accessing and assignment, are
accomplished by leaving the index and value arguments unbound, or providing
ground values, respectively.

    ?- sequence(s,1,5,1,S), iterm_value(S,-2,100) .
    S = s(1, 2, 3, 100, 5)

Finally, two predicates, iterm_append/4 and iterm_extend/3 may be used to append
two iterms or to increase the sizes of an iterm by a given amount. The latter
may be useful for tasks typically accomplished by "dynamic arrays" in imperative
languages.

    ?- sequence(s,1,5,1,S), iterm(v,[a,b,c,d,e],T), iterm_append(v,S,T,ST).
    S = s(1, 2, 3, 4, 5),
    T = v(a, b, c, d, e),
    ST = v(1, 2, 3, 4, 5, a, b, c, d, e).

    ?- sequence(s,1,5,1,S), iterm_extend(S,5,S_) .
    S = s(1, 2, 3, 4, 5),
    S_ = s(1, 2, 3, 4, 5, _4170, _4172, _4174, _4176, _4178).

### Selecting iterm arguments

subterm/3, slice/4, dice/3 and window/6 may be used to select a sub-set of the
arguments of an iterm, as a new iterm with the same functor name.  The subset to
be selected may be given as an index interval (by specifying two endpoints), a
list of indices, or a number of arguments.

Additionally, interval/4, interval/3 and interval/5 may be used to generate
numbers in a desired interval and can therefore serve as a source of
nondeterminism in forall/2, foreach/2, findall/3 and friends, similar to
between/3 but with the ability to omit, or repeat some values, or enumerate in
descending order.

    ?- interval(1,10,I).
    I = 1 ;
    I = 2 ;
    I = 3 .
    
    ?- interval(1,10,2,I).
    I = 1 ;
    I = 3 ;
    I = 5 .
    
    ?- interval(1,10,2,2,I).
    I = 1 ;
    I = 1 ;
    I = 3 ;
    I = 3 ;
    I = 5 ;
    I = 5 .

    ?- interval(10,1,2,2,I).
    I = 10 ;
    I = 10 ;
    I = 8 ;
    I = 8 ;
    I = 6 ;
    I = 6 .

subterm/3 selects the arguments at the indices provided by a sequence of
numbers, such as one generated by sequence/[5,6].

    ?- iterm(v,[a,b,c,d,e,f,g,h,i,j],T), subterm(T,v(3,4,5,6),Ss).
    T = v(a, b, c, d, e, f, g, h, i, j),
    Ss = v(c, d, e, f).

slice/4 selects arguments between two indices. Indices may be positive or
negative, allowing slicing from the start or end of an iterm. 

    ?- sequence(s,1,10,1,S), slice(S,2,8,Ss).
    S = s(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Ss = s(2, 3, 4, 5, 6, 7, 8).
        
    ?- sequence(s,1,10,1,S), slice(S,-2,2,Ss).
    S = s(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Ss = s(9, 8, 7, 6, 5, 4, 3, 2).

Endpoints may also be given in descending order to obtain the selected arguments
in reverse order. The following example showcases the use of descending-order
endpoints to reverse an iterm:

    ?- slice(v(a,b,c,d,e),5,1,S).
    S = v(e, d, c, b, a).

dice/3 selects successive "chunks" (subsets of an iterm's arguments) of a given
width. It always starts at the first argument and ends at the last, performing a
linear scan over the arguments of an iterm.

    ?- iterm(v,[a,b,c,d,e],T), dice(T,2,Ss).
    T = v(a, b, c, d, e),
    Ss = [v(a, b), v(b, c), v(c, d), v(d, e)].

window/6 selects arguments between two endpoints, yielding chunks of a chosen
width and while skipping any number of arguments. It generalises slice/4 and
dice/3 and has a broad range of applications besides, that include such complex
tasks as collecting n-grams or skip-grams from a string and more. Refer to the
documentation on the predicate itself for more details.

    ?- iterm(s,['The',cat,sat,on,the,mat,with,a,hat,like,a,prat],T)
        ,window(T,1,12,2,3,Ws).
    T = s('The', cat, sat, on, the, mat, with, a, hat, like, a, prat),
    Ws = [s('The', cat), s(on, the), s(with, a), s(like, a)].

Because of the complex relation between window/6 parameters and the broad range
of use cases of this predicate, a number of auxiliary predicates are made public
in this module: these are window_span/3, window_size/5 and maximum_stride/4.
These can be used to reason about the effects of any window/6 parameter values
that give unexpected results.

## Motivation

Despite its otherwise rich library, Swi Prolog does not provide any predicates
to create and manage "arrays" or other high-level facilities for positional
access over collections of terms (other Prolog systems do this, for instance Gnu
Prolog has an extensive array library). While low-level predicates, such as
arg/3 and nb_setarg/3 can serve this function, a more principled approach to
positional access requres quite a bit of work from the user, wheras a more
ad-hoc use of arg/3 etc can lead to a lot of "boilerplate" code. This module
hopes to address this lack of positional access primitives by providing a
starting point for an implementation of arrays for Swi Prolog.

In situations where random access to the elements of a collection is required,
the ability to access terms' arguments by index makes writing efficient code
easier than it is when doing it the traditional way, with Prolog lists, where
elements must be accessed in a sequential order.

Examples of manipulations that can benefit from positional access to the
elements of a collection are "slicing" and "dicing" (selecting elements between
two indices and selecting chunks of a constant size, respectively) and in
general any manipulation that needs to access a range of elements between two
indices, especially in a non-linear order.

Although it's of course entirely feasible to achieve the same results with
Prolog lists, one has to walk through the length of a list to do so. In some
cases, time can be saved by instead jumping between indices at will. This
becomes even more important when one has to manipulate large numbers of elements
in such manner.

A particularly interesting use-case is the representation of vectors and
matrices and the operations over them that are crucial to statistical AI
applications. Although the traditional matrix representation in Prolog, as a
list-of-lists, is serviceable, it can be more efficient to represent such
multi-dimensional data structures, or "tensors" (as they are increasingly
commonly known) as one-dimensional arrays along with some logic to calculate the
appropriate index offsets.

Indeed, predicates in this module are meant to form the basis of an efficient
implementation of a tensor manipulation library for Swi Prolog. The current
version is written entirely in Prolog and therefore makes use of the available
positional access primitives and this may not offer the best possible
performance. On the other hand, the creator of this library considers the most
important aspect of her work to be the formal declaration of a useful public
interface for indexed terms, which can them be re-implemented as foreign
language predicates, if world-class performance proves to be a requirement.

The usual caveats concerning deviating from the logical, declarative and
immutable paradigm that typically accompany every use of arg/3, setarg/3 and
similar apply in this case also. However, providing an entire module with
high-level predicates that abstract those low-level operations should go some
way towards protecting against the worse of this declarative-imperative
impedance mismatch. Additionally, the use of non-backtrackable assignment with
nb_setarg/3 should also help reduce some of the inevitable pain when debugging
recalcitrant applications.

This module makes no assumptions about storage of iterms, such as asserting them
into the dynamic database or not etc. It also does not introduce any new
operators or syntax, such as using square braces for indexing and so on. This is
to allow the greater possible degree of flexibility in its use, particularly so
in the planned, subsequent implementation of higher-level array etc. structures.
