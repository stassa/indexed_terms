:-module(test_helpers, [alphabetic_sequence/2
		       ,random_iterm/11
                       ,partly_random_iterm/11
		       ]).

:-use_module(prolog/src/indexed_terms).


%!	alphabetic_sequence(+Width,-Sequence) is det.
%
%	Create an ordered sequence of letters of the alphabet, starting
%	at a.
%
%	Use this to generate iterms to test with rather than
%	hand-crafting them.
%
%	@tbd This could allow an alphabetic sequence of any length,
%	or even skipping elements, mapping interval/4 values to letters.
%
alphabetic_sequence(K,T):-
	A = v(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,x,y,z)
	,slice(A,1,K,T).



%!	random_iterm(?F,-A,-T,-I,-J,-K,-N,-S,-M,-Mn) is det.
%
%	Generate a random iterm for testing.
%
%	T is bound to a numeric iterm with functor F and arity A, from 1
%	to 100. Arguments of this iterm form a sequence starting at 1
%	and ending at A, proceeding in strides of N.
%
%	I,J,K and N are bound to valid window/6 parameters for that
%	term.
%
%	S, M and Mn are bound to the span, window size and maximum
%	stride for a window with those parameters over T, respectively.
%
%	Use this to avoid creating iterms by hand (and risk getting
%	spurious results with magic values).
%
%	@tbd reduce boilerplate in this and partly_random_iterm/11
%
%	@tbd this needs to backtrack from invalid parameter
%	combinations.
%
random_iterm(F,A,T,I,J,K,N,S,M,Mn,asc):-
	!
	,random_between(1,100,A)
	% Default functor is v. Doesn't really affect anything...
	,(   var(F)
	 ->  F = v
	 ;   true
	)
	% Off-by-one correction - we want the sequence
	% to start at 1 and still have arity A.
	,(   A > 1
	->  sequence(F,1,A,1,T)
	;   sequence(F,1,A,0,T)
	)
	,assertion(compound_name_arity(T,_,A))
	,random_between(1,A,I)
	,random_between(I,A,J)
	,window_span(I,J,S)
	,random_between(0,S,K)
	,(   I == J
	 ->  N = 0
	 ;   random_between(1,S,N)
	 )
	,window_size(I,J,K,N,M)
	,maximum_stride(I,J,K,Mn).

random_iterm(F,A,T,I,J,K,N,S,M,Mn,desc):-
	random_between(1,100,A)
	,(   var(F)
	 ->  F = v
	 ;   true
	)
	,A_ is A + 1
	,sequence(F,1,A_,1,T)
	,random_between(1,A,I)
	% Will allow I == J! So not strictly descending.
	,random_between(1,I,J)
	,window_span(I,J,S)
	,random_between(0,S,K)
	,(   I == J
	 ->  N = 0
	 ;   random_between(1,S,N)
	 )
	,window_size(I,J,K,N,M)
	,maximum_stride(I,J,K,Mn).


%!	partly_random_iterm(?F,?A,+T,?I,?J,?K,?N,-S,-M,-Mn) is det.
%
%	As random_iterm/10 but any number of items may be bound.
%
%	Use this to randomise only some of an iterm's parameters. Window
%	size, span and max stride are still calculated according to the
%	given, or generated, parameters (you can't specify them and get
%	the parameters for a window with that size, span and max
%	stride).
%
%	Note: the only "input" that can't be ground in advance is T.
%	That one is a blank iterm always. But its arity and functor name
%	can be given.
%
partly_random_iterm(F,A,T,I,J,K,N,S,M,Mn,asc):-
	!
	,maybe_random_between(1,100,A)
	,(   var(F)
	 ->  F = v
	 ;   true
	)
	,(   A > 1
	->  sequence(F,1,A,1,T)
	;   sequence(F,1,A,0,T)
	)
	,maybe_random_between(1,A,I)
	,(   N == 0
	 ->  J = I
	 ;   maybe_random_between(I,A,J)
	 )
	,window_span(I,J,S)
	,maybe_random_between(0,S,K)
	,(   I == J
	 ->  N = 0
	 ;   maybe_random_between(1,S,N)
	 )
	,window_size(I,J,K,N,M)
	,maximum_stride(I,J,K,Mn).

partly_random_iterm(F,A,T,I,J,K,N,S,M,Mn,desc):-
	maybe_random_between(1,100,A)
	,(   var(F)
	 ->  F = v
	 ;   true
	)
	,(   A > 1
	->  sequence(F,1,A,1,T)
	;   sequence(F,1,A,0,T)
	)
	,maybe_random_between(1,A,I)
	,(   N == 0
	 ->  J = I
	 ;   maybe_random_between(1,I,J)
	 )
	,window_span(I,J,S)
	,maybe_random_between(0,S,K)
	,(   I == J
	 ->  N = 0
	 ;   maybe_random_between(1,S,N)
	 )
	,window_size(I,J,K,N,M)
	,maximum_stride(I,J,K,Mn).



%!	maybe_random_between(+High,+Low,?Parameter) is semidet.
%
%	Generate, or accept, a Parameter in the given ranges.
%
maybe_random_between(H,L,P):-
	var(P)
	,!
	,random_between(H,L,P).
maybe_random_between(H,L,P):-
	between(H,L,P).


:-begin_tests(test_helpers).

% Test that the test-generating predicate works as expected.
% Next, we'll test that the predicate testing the test-generating
% predicate works as expected.
test(window_random_iterm_ascending, []):-
	forall(between(1,100,_)
	      ,(random_iterm(_,A,T,I,J,K,N,_,M,_,asc)
	       % Just to make it clear endpoints are in ascending order:
	       ,assertion(I =< J)
	       % Naughty-naughty trespassing on module boundary.
	       ,assertion(indexed_terms:window_parameters(A,I,J,K,N,asc))
	       ,assertion(window(T,I,J,K,N,Ws))
	       ,assertion(length(Ws, M))
	       )
	      ).

test(window_random_iterm_descending, []):-
	forall(between(1,100,_)
	      ,(random_iterm(_,A,T,I,J,K,N,_,M,_,desc)
	       % TODO: make it so I == J
	       ,assertion(I >= J)
	       ,assertion(indexed_terms:window_parameters(A,I,J,K,N,desc))
	       ,assertion(window(T,I,J,K,N,Ws))
	       ,assertion(length(Ws, M))
	       )
	      ).

test(window_partly_random_iterm_ascending, []):-
	forall(between(1,100,_)
	       ,(random_between(1,10,A)
		,random_between(1,A,I)
		,random_between(I,A,J)
		,assertion(I =< J)
		,window_span(I,J,S)
		,random_between(0,S,K)
		,(   I == J
		 ->  N = 0
		 ;   random_between(1,S,N)
		 )
		%,format('~n A: ~w I: ~w J: ~w K: ~w N ~w S: ~w',[A,I,J,S,K,N])
		,partly_random_iterm(v,A,T,I,J,K,N,S_,M,_,asc)
		,assertion(S == S_)
		,assertion(window(T,I,J,K,N,Ws))
		,assertion(length(Ws, M))
		)
	       ).

test(window_partly_random_iterm_descending, []):-
	forall(between(1,100,_)
	       ,(random_between(1,10,A)
		,random_between(1,A,I)
		,random_between(1,I,J)
		,assertion(I >= J)
		,window_span(I,J,S)
		,random_between(0,S,K)
		,(   I == J
		 ->  N = 0
		 ;   random_between(1,S,N)
		 )
		%,format('~n A: ~w I: ~w J: ~w K: ~w N ~w S: ~w',[A,I,J,S,K,N])
		,partly_random_iterm(v,A,T,I,J,K,N,S_,M,_,desc)
		,assertion(S == S_)
		,assertion(window(T,I,J,K,N,Ws))
		,assertion(length(Ws, M))
		)
	       ).

:-end_tests(test_helpers).
