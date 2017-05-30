:-module(indexed_terms, [iterm_types/1
			,infinities/1
			,iterm/3
			,typed_iterm/4
			,blank_iterm/3
			,init_iterm/4
			,iterm_index/3
			,iterm_value/3
			,iterm_append/4
			,iterm_extend/3
			,interval/4
			,interval/3
			,interval/5
			,sequence/5
			,sequence/6
			,subterm/3
			,slice/4
			,dice/3
			,window/6
			,window_span/3
			,window_size/5
			,maximum_stride/4
			]).


/** <module> Indexed Prolog terms with destructive, non-backtrackable assignment.

Predicates to facilitate creating, initialising, accessing and
manipulating indexed terms, to be used as the building blocks of arrays,
vectors and matrices.

In the context of this module, "Indexed terms" (iterms, for short) are
compound Prolog terms meant to be treated as named collections taking
advantage of arg/3 to access data by index and nb_setarg/3 to modify
these data in-place.

Iterm indices
-------------

The arity of an iterm may range from 0 to whatever is the effective
limit of your hardware. Accordingly, the valid range of indices of an
iterm, is an enumeration of the arguments of the iterm, which informs
the choice of iterm starting indices: the first index of an iterm is
either 1, for an iterm with at least one argument, or 0, for an iterm
with 0 arguments.

In other words, for an iterm of arity a, with starting index i:

==
     | 1    if a > 0
i = <
     | 0    if a = 0
==

Additionally, predicates in this module allow negative indices to be
used to access iterm arguments in descending order. For example, an
iterm of arity 3 may be accessed with any of the indices in
{-3,-2,-1,1,2,3}, where -3 and 1 would both access the first argument of
the iterm. Refer to the documentation of iterm_index/3 for a more
detailed discussion of positive and negative iterm indices.

The choice of variable starting indices, may appear a little peculiar -
in most languages the indices of random-access data structures either
start at 0, or 1 (though a few, like Ada and Fortran allow the user to
specify their own range, which this module doesn't). When in doubt,
iterm_index/3 may be used to clarify any misunderstandings about the
valid range of iterms' indices.

Usage
----

This module is meant to be used as a library for Swi-Prolog. Add it to
your project and include it in your code in the usual way, with a
use_module/1 directive:

==
use_module(lib(indexed_terms)).
==

Predicates in this module fall into two broad types: on the one hand,
predicates for creating, indexing and accessing iterms, and on the
other, predicates for selecting ranges of arguments of iterms between
two indices.

*Creating iterms*

Predicates iterm/3, typed_iterm/4, blank_iterm/3, init_iterm/4,
sequence/5 and sequence/6 may be used to create iterms from a functor
name and list of arguments, or a number denoting its arity.

iterm/3 creates an iterm from a functor name and a list of arguments,
without any restrictions except those that apply to
compound_name_arguments/3, which this predicate is basically just a thin
wrapper around.

typed_iterm/4 creates iterms from a list of arguments that must belong
to one of a set of argument types, defined in iterm_types/1. Attempting
to create a typed iterm with arguments of the wrong type raises a
type_error. Note however that any type-checking required after the typed
iterm is created is up to the user.

blank_iterm/3 creates iterms of a given arity with all their arguments
as unbound variables. init_iterm/4 does the same, but binds all
arguments to a given value. You can use these two when you know the size
of your data but not yet its final values.

The common numeric computing task of creating a vector of zeroes, or
other default values, or a sequence of numbers, is encapsulated by
init_iterm/4 and also sequence/5 and sequence/6.

iterm_index/3 and iterm_value/3 may be used to index through an iterm
nondeterministically, by generating its valid range of indices.
Additionally, iterm_index/3 may be used to map between negative and
positive indices, while iterm_value/3 accepts negative indices to allow
accessing iterm elements "from the back". See more information on
indices in the following section.

iterm_value/3 may be further used to instantiate arguments of an iterm
to a desired value. The two different use cases, accessing and
assignment, are accomplished by leaving the index and value arguments
unbound, or providing ground values, respectively.

Finally, two predicates, iterm_append/4 and iterm_extend/3 may be used
to append two iterms or to increase the sizes of an iterm by a given
amount. The latter may be useful for tasks typically accomplished by
"dynamic arrays" in imperative languages.

*Selecting iterm argument ranges*

subterm/3, slice/4, dice/3 and window/6 may be used to select a sub-set
of the arguments of an iterm, as a new iterm with the same functor name.
The subset to be selected may be given as an index interval (by
specifying two endpoints), a list of indices, or a number of arguments.

Additionally, interval/4, interval/3 and interval/5 may be used to
generate numbers in a desired interval and can therefore serve as a
source of nondeterminism in forall/2, foreach/2, findall/3 and friends,
similar to between/3 but with the ability to omit, or repeat some
values, or enumerate in descending order.

subterm/3 selects the arguments at the indices provided by a sequence of
numbers, such as one generated by sequence/[5,6].

slice/4 selects arguments between two indices. Indices may be positive
or negative, allowing slicing from the end of an iterm. They may also be
given in descending order to obtain the selected arguments in reverse
order. The following example showcases the use of descending-order
endpoints to reverse an iterm:

==
?- slice(v(a,b,c,d,e),5,1,S).
S = v(e, d, c, b, a).
==

dice/3 selects successive "chunks" (subsets of an iterm's arguments) of
a given width. It always starts at the first argument and ends at the
last, performing a linear scan over the arguments of an iterm.

window/6 selects arguments between two endpoints, yielding chunks of a
chosen width and while skipping any number of arguments. It generalises
slice/4 and dice/3 and has a broad range of applications besides, that
include such complex tasks as collecting n-grams or skip-grams from a
string and more. Refer to the documentation on the predicate itself for
more details.

Because of the complex relation between window/6 parameters and the
broad range of use cases of this predicate, a number of auxiliary
predicates are made public in this module: these are window_span/3,
window_size/5 and maximum_stride/4. These can be used to reason about
the effects of any window/6 parameter values that give unexpected
results.

Motivation
----------

Despite its otherwise rich library, Swi Prolog does not provide any
predicates to create and manage "arrays" or other high-level facilities
for positional access over collections of terms (other Prolog systems do
this, for instance Gnu Prolog has an extensive array library). While
low-level predicates, such as arg/3 and nb_setarg/3 can serve this
function, a more principled approach to positional access requres quite
a bit of work from the user, wheras a more ad-hoc use of arg/3 etc can
lead to a lot of "boilerplate" code. This module hopes to address this
lack of positional access primitives by providing a starting point for
an implementation of arrays for Swi Prolog.

In situations where random access to the elements of a collection is
required, the ability to access terms' arguments by index makes writing
efficient code easier than it is when doing it the traditional way, with
Prolog lists, where elements must be accessed in a sequential order.

Examples of manipulations that can benefit from positional access to
the elements of a collection are "slicing" and "dicing" (selecting
elements between two indices and selecting chunks of a constant size,
respectively) and in general any manipulation that needs to access a
range of elements between two indices, especially in a non-linear order.

Although it's of course entirely feasible to achieve the same results
with Prolog lists, one has to walk through the length of a list to do
so. In some cases, time can be saved by instead jumping between indices
at will. This becomes even more important when one has to manipulate
large numbers of elements in such manner.

A particularly interesting use-case is the representation of vectors and
matrices and the operations over them that are crucial to statistical AI
applications. Although the traditional matrix representation in Prolog,
as a list-of-lists, is serviceable, it can be more efficient to
represent such multi-dimensional data structures, or "tensors" (as they
are increasingly commonly known) as one-dimensional arrays along with
some logic to calculate the appropriate index offsets.

Indeed, predicates in this module are meant to form the basis of an
efficient implementation of a tensor manipulation library for Swi
Prolog. The current version is written entirely in Prolog and therefore
makes use of the available positional access primitives and this may not
offer the best possible performance. On the other hand, the creator of
this library considers the most important aspect of her work to be the
formal declaration of a useful public interface for indexed terms, which
can them be re-implemented as foreign language predicates, if
world-class performance proves to be a requirement.

The usual caveats concerning deviating from the logical, declarative and
immutable paradigm that typically accompany every use of arg/3, setarg/3
and similar apply in this case also. However, providing an entire module
with high-level predicates that abstract those low-level operations
should go some way towards protecting against the worse of this
declarative-imperative impedance mismatch. Additionally, the use of
non-backtrackable assignment with nb_setarg/3 should also help reduce
some of the inevitable pain when debugging recalcitrant applications.

This module makes no assumptions about storage of iterms, such as
asserting them into the dynamic database or not etc. It also does not
introduce any new operators or syntax, such as using square braces for
indexing and so on. This is to allow the greater possible degree of
flexibility in its use, particularly so in the planned, subsequent
implementation of higher-level array etc. structures.

*/

%!	iterm_types(?Types) is semidet.
%
%	Valid types for typed iterms.
%
iterm_types([atom
	    ,atomic
	    ,boolean
	    ,callable
	    ,chars
	    ,codes
	    ,text
	    ,compound
	    ,constant
	    ,float
	    ,integer
	    ,nonneg
	    ,positive_integer
	    ,negative_integer
	    ,number
	    ,oneof(_)
	    ,list
	    ,list_or_partial_list
	    ,list(_)
	    ,symbol
	    ,var
	    ,rational
	    ,encoding
	    ,dict
	    ,string
	    ,between(_,_)
	    ]).


%!	infinities(?Terms) is semidet.
%
%	Atomic terms used to denote infinite values.
%
infinities([inf,-inf]).


%!	iterm(+Name,+Values,?Term) is det.
%
%	Construct a new indexed Term with the given Name and Values.
%
%	Same as: Term =.. [Functor|Values].
%
iterm(F,Vs,T):-
	must_be(list, Vs)
	,compound_name_arguments(T,F,Vs).



%!	typed_iterm(+Name,+Type,+Values,-Term) is det.
%
%	Typed version of iterm/3
%
%	Type may be any of the iterm types defined in iterm_types/1.
%	Otherwise, a type error is raised.
%
%	Values must be a list of values of the given Type. An empty
%	Values list will raise a domain error.
%
typed_iterm(F,Tp,Vs,T):-
	iterm_types(Ts)
	,(   Vs == []
	 ->  domain_error('non_empty_list', [])
	 ;   true
	 )
	,must_be(oneof(Ts), Tp)
	,forall(member(V,Vs)
	      ,must_be(Tp,V))
	,iterm(F,Vs,T).



%!	blank_iterm(+Name,+Arity,-Term) is det.
%
%	Term is an empty iterm with the given Name and Arity.
%
%	Same as compound_name_arity(Term,Name,Arity). Additionally,
%	Arity may not be 0.
%
blank_iterm(F,A,T):-
	must_be(positive_integer,A)
	,compound_name_arity(T,F,A).



%!	init_iterm(+Name,+Arity,+Value,-Term) is det.
%
%	Bind all arguments of Term to Value.
%
%	Fails silently if Term cannot be unified to a compound term with
%	the given Functor/Arity and each argument bound to Value.
%
init_iterm(F,A,V,T):-
	must_be(positive_integer, A)
	,findall(V
	       ,between(1,A,_)
	       ,Vs)
	,iterm(F,Vs,T).



%!	iterm_index(+Term,?In,?Out) is nondet.
%
%	Generate valid indices for the given Term.
%
%	True when In can be bound to an Out value in the valid range of
%	indices for Term.
%
%	For a term with arity 0, the only valid index is 0. Otherwise,
%	the valid range of indices is {1,...,A} where A the arity of the
%	term. iterm_index/3 also allows negative inputs, mapping them
%	to the valid range of indices of Term in order to "wrap around"
%	and access arguments from the end of Term.
%
%	When In is not a variable it should be an integer with an
%	absolute value between 1 and the arity, A, of Term:
%	==
%	In : {-A,...,-1,1,...A}
%	==
%
%	If In is a positive integer between 1 and the arity, A, of Term,
%	Out is bound to In. If In is a negative integer between -A and
%	-1, Out is bound to the difference of A and the absolute value
%	of In (plus correction):
%	==
%	       | In	        if In > 0
%	Out = <
%	       | A - |In| + 1   otherwise
%	==
%
%	If Term has arity 0 then iterm_index/3 fails silently unless
%	both In and Out can be bound to 0.
%
%	If In is a number outside the valid index range for Term,
%	iterm_index/3 fails silently.
%
%	In mode (+,-,-) the entire range of I and O is enumerated,
%	including negative values and excluding 0, unless the arity of
%	Term is 0 (in which case the only value generated is 0).
%
iterm_index(T,I,O):-
	compound_name_arity(T,_,A)
	,iterm_index_range(A,I)
	,iterm_index_projection(A,I,O).


%!	iterm_index_range(+Arity,?Index) is nondet.
%
%	True when Index is a valune in the valid range of indices for
%	terms of the given Arity.
%
%	Successive backtracking generates the full range of index
%	values for such terms.
%
iterm_index_range(0,0):- !.
iterm_index_range(A,I):-
	A_ is A * -1
	,between(A_,A,I)
	,I \= 0.


%!	iterm_index_projection(+Arity,+In,-Out) is det.
%
%	Map an index to the valid range of indices for terms of the
%	given Arity.
%
%	Currently, this will only map between negative and positive
%	values to allow "wrapping up" indices to access elements from
%	the end of an iterm, but there's no reason why this should be
%	the only possible mapping. Eventually it should be possible for
%	a user to specify their own mapping predicate, or at the very
%	least there should be a selection of mapping predicates allowing
%	for ranges between any two numbers (similar to languages like
%	Ada and Fortran).
%
iterm_index_projection(A,I,O):-
	I < 0
	,!
	,O is A + I + 1.
iterm_index_projection(_,I,I).



%!	iterm_value(+Term,?Index,?Value) is nondet.
%
%	Bind or assign Value at the given Index in Term.
%
%	The outcome of accessing an argument at a given index depends on
%	the mode in which iterm_value/3 is called.
%
%	In mode (+,+,+) Value is assigned to the argument at the
%	Index'th position of Term.
%
%	In any of the modes (+,-,+) and (+,+,-) arguments and their
%	positions are enumerated and no new values bound.
%
%	Assignment is destructive and non-backtrackable.
%
%	This is a thin wrapper around arg/3 and nb_setarg/3 and will
%	throw a wobbly if a value bound to Index is not a variable (in
%	mode (+,-,-)) or a number (in mode (+,+,+)), or if Term is not
%	a term that can be indexed by arg/3 and modified by nb_setarg/3.
%
iterm_value(T,I,V):-
	nonvar(V)
	,nonvar(I)
	,iterm_index(T,I,I_)
	,nb_setarg(I_,T,V)
	,!.
iterm_value(T,I,V):-
	iterm_index(T,I,I_)
	,arg(I_,T,V).



%!	iterm_append(+Name,+Term1,+Term2,-Term3) is det.
%
%	Concatenate two iterms.
%
%	Term3 is a new term with the functor Name of Term1 and the
%	argument lists of Term2 appended to those of Term1.
%
%	Note that this uses append/3 internally, to concatenate the
%	argument lists of the two input terms, so it should not be
%	expected to be terribly efficient.
%
iterm_append(F,T1,T2,T3):-
	compound_name_arguments(T1,_,As1)
	,compound_name_arguments(T2,_,As2)
	,append(As1,As2,As3)
	,compound_name_arguments(T3,F,As3).



%!	iterm_extend(+Term,+Arity,-Extended) is det.
%
%	Extend Term with blank arguments.
%
%	Extended is a new term with the functor name and arguments of
%	Term, followed by a number of unbound arguments, equal to Arity.
%
%	Use this to "grow" an iterm, sort of like extending a dynamic
%	array.
%
%	This calls iterm_append/4 internally and the same caveats about
%	using append/3 to concatenate the two terms'
%
iterm_extend(T,A,T_):-
	compound_name_arguments(T,F,_)
	,blank_iterm(F,A,B)
	,iterm_append(F,T,B,T_).



%!	interval(+I,+J,+K,?N) is nondet.
%
%	Generate, or check, values in an interval.
%
%	True when N is a number in the closed interval [I,J], with
%	potentially discontiguous values varying by strides of K.
%
%	Usage
%	-----
%
%	Successive values are generated on backtracking allowing the
%	use of this predicate as a number series generator. To generate
%	a series as a list see sequence/5.
%
%	Values are generated in ascending order when I < J and in
%	descending order when I > J, in other words, you can reverse the
%	normal order of endpoints to "count down" over the values in the
%	interval.
%
%	Note that opposite enumeration orders with the same non-unit
%	stride may result in different intervals:
%	==
%	?- findall(N, interval(1,10,2,N), Ns).
%       Ns = [1, 3, 5, 7, 9].
%
%       ?- findall(N, interval(10,1,2,N), Ns).
%       Ns = [10, 8, 6, 4, 2].
%	==
%
%	I and J may be integers or the special atoms listed in
%	infinities/1 (by default, "inf" and "-inf"), denoting positive
%	and negative infinity. In that case, interval/4 will not
%	terminate unless stopped externally.
%
%	K must be a positive integer, or 0. A stride of 0 only makes
%	sense when I = J = N; any other configuration with a stride of 0
%	will fail.
%
%	N may be bound to a constant to check its membership in the
%	requested interval. This takes the value of K into account and
%	is not the same as checking that N is between I and J. For
%	example:
%	==
%	?- interval(10,1,2,3).
%	false.
%
%	?- interval(10,1,2,4).
%	true;
%	false.
%	==
%
%	Note that checking interval membership is still
%	non-deterministic and since the entire interval may need to be
%	enumerated, execution can take time proportional to the size of
%	the interval. The exception is testing of infinite intervals-
%	those are handled on an ad-hoc basis and do not force
%	enumeration of the entire range. Because that would take
%	forever. Right?
%
%	Special care has been taken to provide some semblance of
%	consistency in the treatment of intervals with a left-hand side
%	endpoint in positive infinity or a right-hand side endpoint in
%	negative infinity (in other words, counting down from positive
%	infinity, or up from negative).
%
%       In such situations, N is bound to a compound term denoting
%	subtraction from positive infinity or addition to negative
%	infinity: inf-M or -inf+M, respectively, where M a number in the
%	interval [0,+inf] (and +inf, here, standing for positive
%	infinity). In all other cases, N is bound to a number.
%
%	A few examples follow:
%	==
%	?- interval(0,inf,100,N).
%	N = 0 ;
%	N = 100 ;
%	N = 200 ;
%	N = 300 ;
%	N = 400 ;
%	N = 500 ;
%
%	?- interval(inf,0,100,N).
%	N = inf-0 ;
%	N = inf-100 ;
%	N = inf-200 ;
%	N = inf-300 ;
%	N = inf-400 ;
%	N = inf-500 .
%
%	?- interval(-inf,0,100,N).
%	N = -inf+0 ;
%	N = -inf+100 ;
%	N = -inf+200 ;
%	N = -inf+300 ;
%	N = -inf+400 ;
%	N = -inf+500 .
%
%	?- interval(0,-inf,100,N).
%	N = 0 ;
%	N = -100 ;
%	N = -200 ;
%	N = -300 ;
%	N = -400 ;
%	N = -500 .
%	==
%
%	In other words, using interval/4 to count towards infinity
%	generates a series of numbers, but going the other way generates
%	a series of compounds.
%
%	It should be noted that, while these compounds are valid Prolog
%	arithmetic functions that can be passed to is/2, doing so is
%	likely to cause arithmetic overflow errors:
%	==
%       ?- interval(-inf, 10, 2, N), A is N.
%       ERROR: is/2: Arithmetic: evaluation error: `float_overflow'
%       Exception: (8) _G5962 is -inf+0 ? creep
%	==
%
%	The infinite-enumeration behaviour described above is provided
%	to allow a somewhat consistent treatment of intervals with
%	infinite endpoints within the constraints imposed by existing
%	hardware and not as a serious treatment of infinity. The
%	expectation is that enumeration towards infinities would be much
%	more common a use case than enumeration in the opposite
%	direction.
%
%	Motivation
%	----------
%
%	interval/4 is meant to help in writing failure-driven loops to
%	index over sequences of numbers and simulate good, old-fashioned
%	imperative for- and while-loops.
%
%	For instance the following will print the numbers from 1 to 10,
%	similar to a for-loop:
%	==
%	forall(interval(1, 10, 1, K), writeln(K)).
%	==
%
%	Whereas the following idiom may be used to repeat a call while a
%	condition holds true, similar to a while-loop:
%	==
%	,findall(N
%	       ,(interval(1,inf,1,N)
%		,(   condition(A1,...,N,...,An)
%		 ->  true
%		 ;   !
%		    ,fail
%		 ))
%	       ,Ns)
%	==
%
%	The main reason interval/4 is included in this library, however,
%	is to facilitate generating ranges of indices for the traversal
%	of indexed terms.
%
%	For example, the following will print out every second
%	argument of v/13 along with its position in that term:
%	==
%	?- iterm(v, [a,b,c,d,e,f,g,h,i,j,k,l,m], T)
%	 ,forall(interval(1,10,2,N),(iterm_value(T,N,V),writeln(V-N))).
%	 a-1
%	 c-3
%	 e-5
%	 g-7
%	 i-9
%	 T = v(a, b, c, d, e, f, g, h, i, j, k, l, m).
%	==
%
%	While the same results can be achieved with a failure-driven
%	loop built around between/3, the different use cases
%	encapsulated by interval/4, particularly when generating series
%	in descending order and with non-unit strides, are meant to
%	help reduce the boilerplate of re-implementing this
%	functionality every time it is needed.
%
%	@see interval/3, interval/5, sequence/5
%
%	@bug Because enumeration from an infinite endpoint starts at
%	that infinity plus or minus 0 it's not possible to count over
%	odd values (and starting at 1 would make it impossible to
%	enumerate even values). Consider the following:
%	==
%	?- interval(-inf, 1, 1, N).
%	N = -inf+0 ;
%	N = -inf+1 ;
%	N = -inf+2 ;
%	N = -inf+3 ;
%	N = -inf+4 .
%
%	?- interval(-inf, 1, 2, N).
%	N = -inf+0 ;
%	N = -inf+2 ;
%	N = -inf+4 ;
%	N = -inf+6 ;
%	N = -inf+8 .
%	==
%
interval(I,I,K,I):-
	!
       ,K == 0.
interval(I,J,K,N):-
	(   infinite(I)
	;   infinite(J)
	)
	,!
	,infinite_interval(I,J,K,N).
interval(I,J,K,N):-
	forall(member(P,[I,J])
	      ,must_be(number, P))
	,must_be(between(0,inf), K)
	,(   J < I
	 ->  K_ is K * -1
	    ,finite_interval(I,J,K_,N)
	 ;   finite_interval(I,J,K,N)
	 ).


%!	infinite(+I) is det.
%
%	True when I is a symbol denoting positive or negative infinity.
%
infinite(I):-
	\+ number(I)
	,infinities(Is)
	,memberchk(I, Is).


%!	infinite_interval(+I,+J,+K,?N) is nondet.
%
%	An interval with possibly infinite endpoints.
%
infinite_interval(I,J,_K,N):-
	number(N)
	,!
	,between_infinity(I,J,N).
infinite_interval(-inf,_,K,N):-
% Counting up from negative infinity
	!
       ,between(0,inf,M)
       ,KM is K * M
       ,plus_or_minus_infinity(-inf,+,KM,N).
infinite_interval(I,inf,K,N):-
% Counting up to positive infinity
	!
	,between(0,inf,M)
	,N is I + K * M.
infinite_interval(inf,_J,K,N):-
% Counting down from positive infinity
	!
	,between(0,inf,M)
	,KM is K * M
	,plus_or_minus_infinity(inf,-,KM,N).
infinite_interval(I,-inf,K,N):-
% Counting down to negative infinity
	between(0,inf,M)
	,N is I - K * M.


%!	between_infinity(?I,?J,?N) is det.
%
%	Compare a value to positive or negative infinity.
%
%	Auxiliary predicate to facilitate interval checking against
%	infinities. Kind of sucks but between/3 does not like -inf.
%
between_infinity(I,J,N):-
	once(between_infinities(I,J,N)).

%!	between_infinities(?I,?J,?N) is nondet.
%
%	Business end of between_infinity/3.
%
%	Note that this compares intervals as defined in this module,
%	including inverse intervals. In other words:
%	==
%	?- indexed_terms:between_infinity(1,-inf,0).
%       true.
%       ?- indexed_terms:between_infinity(-inf,1,0).
%       true.
%       ==
%
%       But:
%       ==
%       ?- indexed_terms:between_infinity(0,-inf,1).
%       false.
%	?- indexed_terms:between_infinity(-inf,0,1).
%       false.
%	==
%
between_infinities(I,I,I).
between_infinities(inf,-inf,_).
between_infinities(I,J,N):-
	I =< N
	,N =< J.
between_infinities(I,J,N):-
	I >= N
	,N >= J.


%!	plus_or_minus_infinity(+A,+Op,+B,-Result) is det.
%
%	Simulate addition and subtraction with infinities.
%
%	Arithmetic using "inf" or "-inf" tends to produce arithmetic
%	overflow errors. Instead, this creates a compound term denoting
%	the operation we would like to perform, but probably can't and
%	binds it to the output.
%
%	For example:
%	==
%	?- indexed_terms:plus_or_minus_infinity(inf,+,1,R).
%       R = inf+1.
%	==
%
%	A must be an infinity symbol and B must be a number. Since
%	plus_or_minus_infinity/4 is only meant for private use in this
%	module, this requirement is not enforced.
%
plus_or_minus_infinity(A,O,B,R):-
	infinite(A)
	,R =.. [O,A,B].


%!	finite_interval(+I,+J,+K,?N) is nondet.
%
%	Business end of interval/4.
%
%	Convenience predicate to allow parent to flip the order
%	of I,J and the sign of K when J is larger than I, this being the
%	idiom used to request an inverted interval, [J,I], rather than
%	[I,J].
%
finite_interval(I,J,K,N):-
	K \= 0
	,interval_size(I,J,K,J_)
	% between/3 counts only upwards and in increments of 1
	% so we need to rejig the interval [I,J] to an interval
	% [0,J'] of the same extent.
	,between(0,J_,M)
	,N is I + K * M.


%!	interval_size(+I,+J,+K,?S) is semidet.
%
%	Calculate the size of the closed interval [I,J] varying by K.
%
interval_size(I,J,K,S):-
	S is floor((J - I) / K).



%!	interval(+I, +J, ?N) is nondet.
%
%	Generate values in the closed interval [I,J].
%
%	Convenience predicate equivalent to the most basic use-case for
%	interval/4, using a unit stride:
%	==
%	interval(I,J,1,N).
%	==
%
interval(I, J, N):-
	interval(I, J, 1, N).



%!	interval(+I,+J,+P,+F,?N) is nondet.
%
%	Similar to interval/4 but repeating N with frequency F.
%
%	Like interval/4 this generates numbers in the closed interval
%	[I,J] varying by P. Additionally, each value is output
%	repeatedly a number of times equal to F.
%
%	Example:
%	==
%       ?- interval(1, 3, 1, 2, N).
%       N = 1 ;
%	N = 1 ;
%       N = 2 ;
%       N = 2 ;
%       N = 3 ;
%       N = 3.
%	==
%
interval(I, J, P, F, N):-
	interval(I, J, P, M)
	,between(1, F, _)
	,N = M.



%!	sequence(+Name,+I,+J,+K,-Seq) is det.
%
%	A Sequence of numbers increasing by a fixed stride.
%
%	Convenience predicate to create terms with arguments bound to
%	the numbers in an interval generated by interval/[3,4,5].
%
sequence(F,I,J,K,T):-
	findall(V
	       ,interval(I,J,K,V)
	       ,Vs)
	,Vs \= [] % else interval was invalid.
	,compound_name_arguments(T,F,Vs).



%!	sequence(+F,+I,+J,+P,+Fr,-Seq) is det.
%
%	As Sequence/5 but repeating each value Fr times.
%
sequence(F,I,J,P,Fr,T):-
	findall(V
	       ,interval(I,J,P,Fr,V)
	       ,Vs)
	,Vs \= []
	,compound_name_arguments(T,F,Vs).



%!	subterm(+Term,+Sequence,-Subterm) is det.
%
%	Select a Sequence of indices from a Term.
%
%	Sequence must be an iterm with numeric arguments, each
%	argument an index of an argument in Term to be selected for
%	Subterm.
%
subterm(T,It,T_):-
	findall(V
	       ,(iterm_value(It,I,P)
		,I < 0
		,iterm_value(T,P,V)
		)
	       ,Vs)
	,compound_name_arguments(T,F,_)
	,compound_name_arguments(T_,F,Vs).



%!	slice(+Term,+From,+To,-Sub) is det.
%
%	Take a slice of Term.
%
%	Sub has the same functor as Term and the arguments From and To
%	the given indices in Term.
%
%	@tbd Needs documentation and examples.
%
slice(T,I,J,S):-
	% This sucks a bit: iterm_value/3
	% calls iterm_index/3 already,
	% but we need to map indices to their valid range
	% before determining their distance for interval/4.
	iterm_index(T, I, I_)
	,iterm_index(T, J, J_)
	,(   I_ == J_
	 ->  iterm_value(T,I,V)
	    ,Vs = [V]
	 ;  findall(V
		   ,(interval(I_,J_,1,K)
		    ,iterm_value(T,K,V)
		    )
		   ,Vs)
	 )
	,compound_name_arity(T,F,_)
	,compound_name_arguments(S,F,Vs).



%!	dice(+Term,+Width,-Diced) is det.
%
%	Split a Term's argument to contiguous, equal-Width, chunks.
%
%	Example:
%	==
%       ?- iterm(v,[a,b,c,d,e,f,g],T),dice(T,2,Ds).
%       T = v(a, b, c, d, e, f, g),
%       Ds = [v(a, b), v(b, c), v(c, d), v(d, e), v(e, f), v(f, g)].
%       ==
%
dice(T,K,Ss):-
	K > 0
	,compound_name_arity(T,_,A)
	% Last valid dice index
	% Plus width correction
	,J is A - K + 1
	,findall(S
	       ,(%interval(1,J,1,N)
		between(1,J,N)
		% Note width correction.
		,I is N + K - 1
		,slice(T,N,I,S)
		)
	       ,Ss).



%!	window(+Term,+I,+J,+K,+N,-Window) is det.
%
%	Select every N'th window of width K between I and J.
%
%	Intuition
%	---------
%
%	Term should be a compound, perhaps one created using iterm/3 or
%	one of the other iterm-creating predicates from this module. I,J
%	must be integers between 1 and the arity of Term.
%
%	K and N must be integers between 0 and the last effective index
%	of the scan, which is to say, the position of the last argument
%	of Term where a window of width K can begin. This value, the
%	window "span", is equal to the difference of J and K. To help
%	reason about the behaviour of window/6, window_span/3 may be
%	used to calculate this span value.
%
%	Provided a valid set of parameters, window/6 scans Term starting
%	at I and ending at J, collecting K arguments in each time
%	step then moving N arguments.
%
%	The indices of arguments of Term where a new window of width K
%	begins in each time step (the "timestep indices") are the same
%	as the elements of the interval [I,J], with a stride of K, as
%	generated by interval/4.
%
%	As in interval/4, I and J may be in descending order, with I >
%	J. In this case, scanning Term begins at J and proceeds towards
%	I, collecting arguments in reverse order.
%
%	Negative endpoints are not allowed. This will be implemented in
%	a future version.
%
%	Window is bound to the list of subterms of Term selected during
%	a scan. Each subterm in Window is of arity K and has the same
%	functor as Term. The number of results in Window depends on the
%	relation between the I,J, K and N parameters. Refer to the
%	formal description below for a detailed explanation of this
%	relation. window_size/5 may be used to calculate the number of
%	results of a scan with a given set of parameters.
%
%	The number of results of a scan will always be at least one
%	- Window will never be the empty list.
%
%	As K and N increase, the number of results in Window decreases.
%	Once K reaches a certain value, Window will only ever contain
%	one result, always the same for a given Term. The value of K
%	where window/6 can be said to "converge" is a function of I, J,
%	Ki and N and may be calculated using maximum_stride/4 and used
%	to avoid setting K to values that will cause unproductive
%	processing of a Term.
%
%	Formal definition
%	-----------------
%
%	Given the complexity of window/6 the difficulty to reason about
%	its behaviour is not insiginficant, therefore it might be useful
%	to provide a slightly more formal definition of its parameter
%	ranges and expected results. Also, I rather like that sort of
%	thing.
%
%	Formally, a window w, over a term t is a finite state automaton
%	with a set of states defined by a quadruple {i,j,k,n}, where:
%	i,j, the endpoints, and n the stride, of the closed, potentially
%	discontiguous, interval [i,j], varying by n, of indices over the
%	arguments of t that w ranges over; and k, the width of w, the
%	arity of each sub-sequence of the arguments of t output by w.
%
%	i and j must be positive integers no more than the arity, a, of
%	t. n and k may also be 0 and may not exceed the effective range,
%	s, or "span", of w. In other words:
%	==
%	i,j in {1,...,a}
%	k,n in {0,...,s}
%	==
%
%	Where the span, s, of w, denotes the last index of t where a
%	window of width k may begin and is a function of i,j, a positive
%	integer that may not exceed a:
%	==
%	1 =< s =< a
%
%	And:
%
%	     / j - i + 1    when i =< j
%	s = <
%	     \ i - j + 1    otherwise
%	==
%
%	i and j may be equal, in which case n must be 0! This is in
%	keeping with the semantics of discontiguous intervals, as
%	implemented in interval/4, according to which a degenerate,
%	potentially discontiguous interval, [i,j], varying by n, is
%	only valid when n = 0.
%
%	If the constraints above do not hold, the behaviour of w is
%	undefined. Otherwise, w performs a linear scan of t, outputting
%	a set of sub-sets of the arguments of t, W = {s1,....,sm}, where
%	m, the number of sub-sets of t in W. When n = 0, the size of W
%	is always 1. Otherwise, it is a function of i,j,k and n. The
%	size of W may never be 0:
%	==
%	       / 1                      when n = 0
%	|W| = <
%	       \ (s - k + 1) * 1 / n    otherwise
%	==
%
%	Above, fractional results are rounded up.
%
%	Scanning t proceeds in timesteps {p1,...,pm}. In each timestep,
%	pt, w moves to a new index, i', of t, where i' an integer in the
%	interval [i,j] by n, and outputs the arguments of t with indices
%	in the closed, contiguous interval [i',i'+k], in other words, in
%	each timestep, pt, the k elements from i' are output.
%
%	According to the above, we may wish to calculate the time
%	complexity of window/6 as m * k.
%
%	Finally, it should be noted that as n increases from 1 to s,
%	the size of W will eventually become 1, at which point the
%	same subterm of t will always be output for any value of n
%	above the value for which |W| = 1. We call this value of n the
%	"maximum stride" of w, n*.
%
%	Finally, it should be noted that as n increases from 1 to s, it
%	eventually reaches a maximum productive value n*, where the size
%	of W is 1. Increasing n above n* will always yield a single
%	result in W, w*, always the same for every value of n after n*.
%	We call n* the "maximum stride" of w and calculate it as
%	follows:
%	==
%	n* = s - k + 1
%	==
%
%	To assist the user in reasoning about the behaviour of window/6,
%	some of the above calculations are provided as separate
%	predicates: window_span/3, to calculate the span of a window,
%	window_size/5, to calculate its number of results and
%	maximum_stride/4, to calculate the value of n*.
%
%	Examples of usage
%	-----------------
%
%	A simple use case of window/6 is to select all the arguments at
%	even- or odd- valued indices in a term:
%	==
%	?- sequence(v,1,10,1,S), window(S,2,10,1,2,Ws).
%       S = v(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
%       Ws = [v(2), v(4), v(6), v(8), v(10)].
%
%       ?- sequence(v,1,10,1,S), window(S,1,10,1,2,Ws).
%       S = v(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
%       Ws = [v(1), v(3), v(5), v(7), v(9)].
%	==
%
%	Specify endpoints in descending order to scan a term in reverse.
%	The following example shows how window/6 can be used to reverse
%	a term:
%	==
%	?- sequence(v,1,5,1,S), window(S,5,1,1,1,Ws).
%       S = v(1, 2, 3, 4, 5),
%       Ws = [v(5), v(4), v(3), v(2), v(1)].
%	==
%
%	This is how to split a term in  "chunks" of width 3:
%	==
%	?- sequence(v,1,10,1,S), window(S,1,10,3,3,Ws).
%	S = v(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
%	Ws = [v(1, 2, 3), v(4, 5, 6), v(7, 8, 9)].
%	==
%
%	Flip the endpoints to do the same, but in reverse:
%	==
%	?- sequence(v,1,10,1,S), window(S,10,1,3,3,Ws).
%       S = v(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
%       Ws = [v(10, 9, 8), v(7, 6, 5), v(4, 3, 2)].
%	==
%
%	Note how a reverse-order scan may not visit the same terms as
%	a forward-order scan with the same parameters.
%
%	window/6 generalises the behaviour of slice/4 and dice/3 and
%	has many more applications besides.
%
%	To simulate the behaviour of slice/4, specify a width value
%	equal to the window span:
%	==
%	?- iterm(v,[a,b,c,d,e,f,g],T), window_span(2,5,S), window(T,2,5,S,1,Ws).
%	T = v(a, b, c, d, e, f, g),
%       S = 4,
%       Ws = [v(b, c, d, e)].
%	==
%
%	To simulate the behaviour of dice/3, use a window with endpoints
%	in [1,A], where A the arity of the term, and a stride of 1:
%	==
%	?- iterm(v,[a,b,c,d,e,f,g],T), window(T,1,7,2,1,Ws).
%       T = v(a, b, c, d, e, f, g),
%       Ws = [v(a, b), v(b, c), v(c, d), v(d, e), v(e, f), v(f, g)].
%	==
%
%	You can also dice a term back-to-front, which you can't do with
%	dice/3. To do this, pass endpoints in descending order:
%	==
%	?- iterm(v,[a,b,c,d,e,f,g],T), window(T,7,1,2,1,Ws).
%       T = v(a, b, c, d, e, f, g),
%       Ws = [v(g, f), v(f, e), v(e, d), v(d, c), v(c, b), v(b, a)].
%	==
%
%	Incidentally, the above examples show how to obtain n-grams (in
%	this case, bigrams) from a string stored as an iterm. A similar
%	use case is to extract skip-grams. In the following example,
%	window/6 is used to collect keep-2-skip-1-grams:
%	==
%	?- iterm(s,['The',cat,sat,on,the,mat,with,a,hat,like,a,prat],T)
%               ,window(T,1,12,2,3,Ws).
%
%       T = s('The', cat, sat, on, the, mat, with, a, hat, like, a, prat),
%	Ws = [s('The', cat), s(on, the), s(with, a), s(like, a)].
%	==
%
%	window/6 comes with a number of tests. Inspect the test file for
%	more examples of using this predicate.
%
%	For even more complex fun, use between/3 to vary the starting
%	index of the scan:
%	==
%       ?- between(1, 12, I),
%	     iterm(s,['The',cat,sat,on,the,mat,with,a,hat,like,a,prat],T),
%	     window(T,I,12,2,3,Ws).
%       I = 1,
%       T = s('The', cat, sat, on, the, mat, with, a, hat, like, a, prat),
%       Ws = [s('The', cat), s(on, the), s(with, a), s(like, a)] ;
%       I = 2,
%       T = s('The', cat, sat, on, the, mat, with, a, hat, like, a, prat),
%       Ws = [s(cat, sat), s(the, mat), s(a, hat), s(a, prat)] ;
%       ...
%	==
%
%	In this case, remember to constraint the window dimensions to
%	their valid ranges, using one of the auxiliary predicates that
%	go with window/6.
%
%	@ tbd Allow negative indices.
%
%
window(T,I,J,K,N,Ws):-
	compound_name_arity(T,F,A)
	,(   I =< J
	 ->  O = asc
	 ;   O = desc
	 )
	,window_parameters(A,I,J,K,N,O)
	,adjusted_parameters(I,J,K,N,[I_,J_,K_,N_],O)
	,window(T,F,I_,J_,K_,N_,Ws,O).


%!	window(+T,+F,+I,+J,+K,+N,-Ws,+O) is det.
%
%	Business end of window/6.
%
%	Not much to see here. O is one of [asc,desc], for ascending and
%	descending. This determines whether K arguments are selected
%	above or below each Nth index between I and J.
%
window(T,F,I,J,K,N,Ws,O):-
	(   O == asc
	->  D = I_ + K_ - 1
	;   O == desc
	->  D = I_ - K_ + 1
	;   throw('Unknown order':O)
	)
	,findall(Wi
		 % Index generation
	       ,(interval(I,J,N,I_)
		,findall(V
			,(between(1,K,K_)
			 ,W is D
			 ,iterm_value(T,W,V)
			 )
			,Vs)
		,compound_name_arguments(Wi,F,Vs)
		)
	       ,Ws).



%!	window_parameters(+A,+I,+J,+K,+N) is det.
%
%	Ensure that window parameters are in their valid ranges for a
%	term with arity A.
%
%	Valid ranges are as follows:
%	==
%	1 =< I =< J =< A
%	0 =< K =< N =< S
%	==
%
%	Where S is the window effective range, or span, as calculated by
%	window_span/3.
%
%	Additionally, in keeping with interval/4 semantics, stride may
%	not be 0 unless I = J.
%
%	If any parameter falls outside its valid range a type error is
%	raised. This is true also for 0-stride windows with different
%	endpoints in which case the error is as follows:
%	==
%       ?- iterm(v,[a,b,c,d,e,f,g,h],T),window(T,3,4,0,0,Ws).
%       ERROR: Type error: `between(3,3)' expected, found `4' (an integer)
%       ==
%
%	@tbd Raise more informative errors, particularly for coinciding
%	endpoints and zero-strides.
%
window_parameters(A,I,J,K,N,asc):-
	!
	,must_be(between(1,J), I)
	,must_be(between(I,A), J)
	,window_span(I,J,S)
	,must_be(between(0,S), K)
	,must_be(between(0,S), N)
	,(   I == J
	 ->  must_be(between(0,0),N)
	 ;   N == 0
	 ->  must_be(between(I,I), J)
	 ;   true
	 ).

window_parameters(A,I,J,K,N,desc):-
	must_be(between(1,A), I)
	,must_be(between(1,I), J)
	,window_span(I,J,S)
	,must_be(between(0,S), K)
	,must_be(between(0,S), N).


%!	adjusted_parameters(+I,+J,+K,+N,-Adjusted,+Order) is det.
%
%	Bring window parameters into useful ranges.
%
%	Primarily used to correct for off-by-one error when adding or
%	subtracting from 1-based indices. For instance, to find the
%	distance between window endpoints (span), 1 must be added to the
%	difference J - I to avoid getting a result of 0 when J = I.
%
%	Order is one of [asc,desc] for ascending and descending order.
%	This should match the order of I and J - if I =< J, O should be
%	"asc", else "desc".
%
adjusted_parameters(I,J,K,N,[I,J_,K,N_],O):-
	last_step_index(J,K,J_,O)
	,stride_adjustment(I,J_,N, N_).


%!	last_step_index(+J,+K,-Last) is det.
%
%	Starting index of the last timestep of a scan.
%
last_step_index(J,K,J_,asc):-
	K > 0
	,!
	,J_ is J - K + 1.
last_step_index(J,K,J_,desc):-
	K > 0
	,!
	,J_ is J + K - 1.
last_step_index(J,_,J,_).


%!	stride_adjustment(+I,+J,+N,-Adjusted) is det.
%
%	Adjust stride according to the window interval.
%
%	Degenerate intervals require a stride of 0. Otherwise, N remains
%	unchanged.
%
stride_adjustment(I,I,_,0):-
	!.
stride_adjustment(_,_,N,N).



%!	window_span(+I,+J,-S) is det.
%
%	The effective range of a window between I and J.
%
%	S is the span of a window over the arguments of a term, t, the
%	maximum number of arguments of that term that the window is
%	allowed to select in one run.
%
window_span(I,J,S):-
	(   I =< J
	->  S is J - I + 1
	;   S is I - J + 1
	).



%!	window_size(+I,+J,+K,+N,?M) is nondet.
%
%	The number of elements in a window with the given dimensions.
%
%	M is the expected number of elements in the window selection
%	set.
%
%	This does not take into account the arity of a term A. If I,J,K
%	or N are not in valid ranges for a given term, this will not
%	make sense when trying to determine the number of selections
%	from that term's arguments with those parameters.
%
window_size(I,J,K,N,_):-
% Width and stride may not be higher than the span
	window_span(I,J,S)
	,(K > S ; N > S)
	,!
	,fail.
window_size(I,J,K,N,M):-
	N \= 0
	,!
	,window_span(I,J,S)
	,(   K > 0
	->   M is ceiling((S - K + 1) * 1 / N)
	 ;   M is ceiling(S / N)
	 ).
window_size(I,I,_,0,1):-
% With a stride of zero, the window remains stationary and a single
% sub-term is produced.
	!.



%!	maximum_stride(+I,+J,+K,-Mn) is det.
%
%	The stride value for which window size becomes 1.
%
%	Once stride reaches this value, any subsequent processing will
%	only ever yield a single selection, the same one. The window can
%	be said to have converged, at that point.
%
maximum_stride(I,I,_,0):-
	!.
maximum_stride(I,J,K,M):-
	window_span(I,J,S)
	,M_ is S - K + 1
	,M is min(M_,S).
