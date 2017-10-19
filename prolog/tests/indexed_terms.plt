:-use_module(prolog/src/indexed_terms).
:-use_module(test_helpers).

/*
Some tests in this file are blocked because they test unbound
enumeration of infinite intervals and may take a long time, or because
they test arithmetic operations with "inf" and "-inf" that may cause
arithmetic overflow depending on the platform. Search and replace
"blocked" with "%blocked" to run those tests.
*/


:-begin_tests(iterm).

test(iterm_basic_usage, []):-
	iterm(v, [a,b,c,d,e,f,g,h,i,j], T)
	,T == v(a,b,c,d,e,f,g,h,i,j).

test(iterm_check, []):-
	iterm(v, [a,b,c], v(a,b,c)).

test(iterm_arity_1, []):-
	iterm(v, [a], T)
	,T == v(a).

% An empty argument list produces an empty iterm.
test(iterm_arity_0, []):-
	Vs = []
	,iterm(v, Vs, v()).

test(iterm_invalid_values, [throws(error(type_error(list,123),Vs))]):-
	Vs = 123
	,iterm(v, Vs, _).

test(iterm_unbound_values, [throws(error(instantiation_error,Vs))]):-
	iterm(v, Vs, _).

test(iterm_invalid_functor, [throws(error(type_error(atom,123),F))]):-
	F = 123
	,iterm(F, [a,b,c], _).

:-end_tests(iterm).


:-begin_tests(typed_iterm).

test(typed_iterm_numbers, []):-
	typed_iterm(v,number,[-3,-2,-1,0,1,2,3,4], T)
	,T == v(-3,-2,-1,0,1,2,3,4).

test(typed_iterm_atoms, []):-
	typed_iterm(v,atom,[beltalowda,beratna,well,walla],T)
	,T == v(beltalowda,beratna,well,walla).

test(typed_iterm_oneof, []):-
	typed_iterm(v,oneof([a,b,c]), [a,b,c,b,a,c], T)
	,T == v(a,b,c,b,a,c).

% Can't make a typed term with 0 args.
test(typed_iterm_empty_arglist
    ,[error(domain_error(non_empty_list,[]),Vs)]):-
	Vs = []
	,typed_iterm(v,_,Vs,_).

test(typed_iterm_wrong_type
    ,[error(type_error(number,of),Vs)]):-
	Vs = [of,course,i,still,love,you]
	,typed_iterm(v,number,Vs,_).

test(typed_iterm_wrong_oneof_type
    ,[error(type_error(oneof([a,b,c]),1),Vs)]):-
	Vs = [1,b,c,b,a,c]
	,typed_iterm(v,oneof([a,b,c]), Vs, _).

test(typed_iterm_list_of_type, []):-
	typed_iterm(v,list(integer),[[1,2],[3,4],[5]],_).

test(typed_iterm_list_of_wrong_type
    ,[error(type_error(list(integer),1),Vs)]):-
	Vs = [1,2,3,4,5]
	,typed_iterm(v,list(integer),Vs,_).

% Also works with floats; can't be bovvered.
test(typed_iterm_between_ints, []):-
	typed_iterm(v,between(1,10),[1,2,3,4,5,6,7,8,9,10],T)
	,T == v(1,2,3,4,5,6,7,8,9,10).

test(typed_iterm_between_wrong_ints
    ,[error(type_error(between(1,10),-1),Vs)]):-
	Vs = [-1,0,1]
	,typed_iterm(v,between(1,10),Vs,_).

test(typed_iterm_invalid_type_spec
    ,[error(type_error(oneof(Ts),beltalowda),Tp)]):-
	iterm_types(Ts)
	,Tp = beratna
	,typed_iterm(v, beltalowda, [a,bc,d,12,3], _).

:-end_tests(typed_iterm).


:-begin_tests(blank_iterm).

test(blank_iterm,[]):-
	blank_iterm(v,5,T)
	,T = v(A,B,C,D,E)
	,forall(member(V,[A,B,C,D,E]),var(V)).

% Can't make a blank iterm with	0 arity. Where do you put the variables?
test(blank_iterm_invalid_arity
     % Looks more like a domain error to me.
    ,[error(type_error(positive_integer,0),A)]):-
	A = 0
	,blank_iterm(v,A,_).

:-end_tests(blank_iterm).


:-begin_tests(init_iterm).

test(init_iterm, []):-
	init_iterm(v, 8, 0, T)
	,T == v(0,0,0,0,0,0,0,0).

test(init_iterm_0_arity
    ,[error(type_error(positive_integer,0),A)]):-
	A = 0
	,init_iterm(v, 0, 0, _).

:-end_tests(init_iterm).


:-begin_tests(iterm_index).

% TODO: need to test what happens when iterm_index(T, I, 1) is requested.
% Can we map from positives to negatives?

test(iterm_index_unbound_term
    ,[error(instantiation_error
		  ,context(system:compound_name_arity/3,T))]):-
	iterm_index(T, 1, _).

% iterm_index can be used to enumerate the valid range of indices of a
% term, including the negative values, but excluding 0.
test(iterm_index_enumerate, []):-
	T = v(a,b,c,d,e,f,g,h)
	,findall(I-O
		,iterm_index(T, I, O)
		,IOs)
	,IOs == [-8-1,-7-2,-6-3,-5-4,-4-5,-3-6,-2-7,-1-8
		,1-1,2-2,3-3,4-4,5-5,6-6,7-7,8-8].

% For terms with arity 0 the only allowed index value is 0.
test(iterm_index_empty_iterm, []):-
	iterm_index(v(), 0, 0).

test(iterm_index_empty_iterm_nonzero_in, [fail]):-
	iterm_index(v(), 0, 1).

test(iterm_index_empty_iterm_nonzero_out, [fail]):-
	iterm_index(v(), 1, 0).

test(iterm_index_empty_iterm_nonzero_both, [fail]):-
	iterm_index(v(), 1, 1).

% Other values are mapped to the valid range.
test(iterm_index_positive_one, []):-
	T = v(a,b,c)
	,iterm_index(T,1,1).

% -1 wraps back to the last index of an iterm.
test(iterm_index_negative_one, []):-
	T = v(a,b,c,d,e,f)
	,I = 6
	,iterm_index(T,-1,I)
	,compound_name_arity(T,v,I).

test(iterm_index_positive_arity, []):-
	T = v(a,b)
	,compound_name_arity(T,v,A)
	,iterm_index(T,A,2).

% -N, where N the arity of an iterm, wraps back to the first argument of
% the iterm.
test(iterm_index_negative_arity, []):-
	T = v(a,b,c,d,e)
	,compound_name_arity(T,v,A)
	,A_ is A * -1
	,iterm_index(T,A_,1).

% Invalid mappings fial silently
test(iterm_index_invalid_mapping_pos_pos, [fail]):-
	T = v(a,b,c,d,e,f,g)
	% Different positive values.
	,iterm_index(T,2,1).

test(iterm_index_invalid_mapping_neg_pos, [fail]):-
	T = v(a,b,c,d,e,f,g,h,i)
	% Not equivalent values
	,iterm_index(T,-2,2).

test(iterm_index_invalid_mapping_pos_neg, [fail]):-
	T = v(a,b,c,d,e,f)
	,iterm_index(T,2,-1).

test(iterm_index_invalid_mapping_neg_neg, [fail]):-
	T = v(a,b,c,d,e,f,g,h)
	% Different negative values
	,iterm_index(T,-1,-3).

test(iterm_index_invalid_mapping_zero_pos, [fail]):-
	T = v(a,b,c,d,e)
	% Zero and a non-zero value
	,iterm_index(T,0,3).

test(iterm_index_invalid_mapping_zero_neg, [fail]):-
	T = v(a,b)
	% Zero and a non-zero value
	,iterm_index(T,0,-1).

test(iterm_index_invalid_mapping_pos_zero, [fail]):-
	T = v(a,b,c,d)
	% Zero and a non-zero value
	,iterm_index(T,3,0).

test(iterm_index_invalid_mapping_neg_zero, [fail]):-
	T = v(a,b,c,d,e,f,g,h,i,j)
	% Zero and a non-zero value
	,iterm_index(T,-1,0).

% Mapping invalid inputs fails silently.
% Notably, this includes 0-inputs: valid index ranges start at 1 (for
% terms with arity other than 0).
test(iterm_index_zero, [fail]):-
	T = v(a,b,c,d,e)
	,iterm_index(T,0,_).

test(iterm_index_positive_out_of_bounds, [fail]):-
	T = v(a,b,c,d,e)
	,iterm_index(T,6,_).

test(iterm_index_negative_out_of_bounds, [fail]):-
	T = v(a,b,c,d)
	,iterm_index(T,-5,_).

:-end_tests(iterm_index).


:-begin_tests(iterm_value).

% Basic indexing
test(iterm_value_random_access, []):-
	T = v(a,b,c,d,e,f,g,h,i,j,k)
	,findall(V
		,(member(I, [1,2,3,-4,-7,6,-8,-1,10,-2])
		 ,iterm_value(T, I, V))
		,Vs)
	,Vs == [a,b,c,h,e,f,d,k,j,j].

% iterm_value/3 can enumerate a term's arguments backwards and forwards
% exploiting the non-determinism in iterm_index/3.
test(iterm_enumerate, []):-
	T = v(a,b,c,d,e,f)
	,findall(I-V
		,(iterm_value(T, I, V))
		,IVs)
	,IVs == [-6-a,-5-b,-4-c,-3-d,-2-e,-1-f,1-a,2-b,3-c,4-d,5-e,6-f].

% iterm_value/3 can be used to get the indices of an argument (both
% positive and negative):
test(iterm_value_index_of, []):-
	T = v(a,b,c,d,e,f,g)
	,findall(I
		,iterm_value(T,I,c)
		,Is)
	,Is == [-5,3].

% iterm_value/3 can assign new values.
test(iterm_value_assignment, []):-
	T = v(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
	,iterm_value(T, 8, eight)
	,T = v(a,b,c,d,e,f,g,eight,i,j,k,l,m,n).

% Note that this means we cannot check the position of an argument
% (because mode (+,+,+) binds the given value at the given index):
test(iterm_value_verify_index_of, []):-
	T = v(a,b,c,d,e,f)
	,iterm_value(T,4,c)
	,T = v(a,b,c,c,e,f).

test(iterm_value_assignment_negative_index, []):-
	T = v(a,b,c,d,e,f,g,h,i,j,k,l,m)
	,iterm_value(T, -4, minus_four)
	,T = v(a,b,c,d,e,f,g,h,i,minus_four,k,l,m).

% The range of positive indices of an iterm is the interval [1,J] where
% J the number of arguments in the iterm:
test(iterm_value_first_positive_index, []):-
	T = v(a,b,c,d,e,f,g,h,i)
	,iterm_value(T,1,V)
	,V == a.

test(iterm_value_last_positive_index, []):-
	T = v(a,b,c)
	,iterm_value(T,3,V)
	,V == c.

% Accordingly, the range of negative indices of an iterm is the interval
% [-1,J] where J the number of arguments in the iterm:
test(iterm_value_first_negative_index, []):-
	T = v(a,b,c,d,e)
	,iterm_value(T,-1,V)
	,V == e.

test(iterm_value_last_negative_index, []):-
	T = v(a,b,c,d)
	,iterm_value(T,-4,V)
	,V == a.

% An index of 0 causes iterm_value/3 to fail silently.
test(iterm_value_zero_index, [fail]):-
	T = v(a,b,c,d,e,f,g,h)
	,iterm_value(T,0,_).

% Other out-of-bound indices also fail silently.
test(iterm_value_positive_index_out_of_bounds, [fail]):-
	T = v(a,b,c,d,e)
	,iterm_value(T,6,_).

test(iterm_value_negative_index_out_of_bounds, [fail]):-
	T = v(a,b,c,d,e,f,g)
	,iterm_value(T,-20,_).

% Indices must be numbers.
test(iterm_value_invalid_index
    ,[throws(error(type_error(integer,not_a_number),I))]):-
	T = v(a,b,c,d,e,f)
	,I = not_a_number
	,iterm_value(T,I,_).

:-end_tests(iterm_value).


:-begin_tests(interval).

% Basic usage: generate numbers in the closed interval [I,J] varying in
% strides of K.
test(interval_one_to_ten_by_one, []):-
	findall(K
	       ,interval(1, 10, 1, K)
	       ,Ks)
	,Ks == [1,2,3,4,5,6,7,8,9,10].

test(interval_zero_to_thirteen_by_two, []):-
	findall(K
	       ,interval(0, 13, 2, K)
	       ,Ks)
	,Ks == [0,2,4,6,8,10,12].

% [I,I], sometimes called a degenerate interval is equivalent to the set
% that only contains the scalar I, {I}.
test(interval_degenerate, []):-
	interval(6,6,0,6).

% Degenerate intervals with a stride other than 0 make no sense.
test(interval_degenerate_nonunit_stride, [fail]):-
	interval(100,100,3,_).

% A stride of 0 only makes sense for degenerate intervals.
test(interval_invalid_0_stride, [fail]):-
	interval(-1, 3, 0, _).

test(interval_invalid_0_stride, [fail]):-
	interval(4, -2, 0, _).

% Flip endpoints to generate the interval in descending order.
test(interval_nine_to_zero_by_one, []):-
	findall(K
	       ,interval(9,0,1,K)
	       ,Ks)
	,Ks == [9,8,7,6,5,4,3,2,1,0].

test(interval_twelve_to_one_by_two, []):-
	findall(K
	       ,interval(12,1,2,K)
	       ,Ks)
	,Ks == [12,10,8,6,4,2].

% Mode (+,+,+,+) can be used to check interval membership.
test(interval_check, [nondet]):-
	% Check is actually semi-deterministic:
	% it succeeds once, then fails.
	interval(1, 4, 1, 1).

test(interval_check, [fail]):-
	interval(2, 5, 1, 1).

% Stride values other than 1 create "sparse" intervals that skip some
% numbers when checking membership:
test(interval_check_with_nonunit_stride, [fail]):-
	interval(3,20,2,4).

% Checking descending order intervals is valid
test(interval_check_descending_order, [nondet]):-
	interval(9, 2, 1, 2).

test(interval_check_descending_order_with_nonunit_stride, [nondet]):-
	interval(7, 1, 2, 1).

% Lower bounds can be negative.
test(interval_negative_lower_bound, []):-
	findall(V
	       ,interval(-5,5,1,V)
	       ,Vs)
	,Vs == [-5,-4,-3,-2,-1,0,1,2,3,4,5].

test(interval_negative_lower_bound_nonunit_stride, []):-
	findall(V
	       ,interval(-5,5,2,V)
	       ,Vs)
	,Vs == [-5,-3,-1,1,3,5].

% Upper bounds can also be negative for a descending interval.
test(interval_negative_upper_bound, []):-
	findall(V
	       ,interval(5,-5,1,V)
	       ,Vs)
	,Vs == [5,4,3,2,1,0,-1,-2,-3,-4,-5].

test(interval_negative_upper_bound_nonunit_stride, []):-
	findall(V
	       ,interval(5,-5,2,V)
	       ,Vs)
	,Vs == [5,3,1,-1,-3,-5].

% Negative interval bounds can be checked also
test(interval_negative_lower_bound_check, [nondet]):-
	interval(-2, 5, 1, 1).

test(interval_negative_upper_bound_check, [fail]):-
	interval(2, -5, 2, 1).

% Negative stride values are invalid.
test(interval_negative_stride
    ,[throws(error(type_error(between(0,inf),-1),K))]):-
	K = -1
	,interval(10,1,K,_).

% Floats are not currently allowed as strides; might change.
test(interval_fractional_stride
    ,[throws(error(type_error(between(0,inf),0.5),K))]):-
	K = 0.5
	,interval(10,1,K,_).

% Bounds and stride must be numeric. Duh.
test(interval_non_numeric_lower_bound
    ,[throws(error(type_error(number,a),I))]):-
	I = a
	,J = 10
	,K = 3
	,interval(I, J, K, _).

test(interval_non_numeric_upper_bound
    ,[throws(error(type_error(number,b),J))]):-
	I = 1
	,J = b
	,K = 3
	,interval(I, J, K, _).

test(interval_non_numeric_stride
    ,[throws(error(type_error(between(0,inf),c),K))]):-
	I = 1
	,J = 5
	,K = c
	,interval(I, J, K, _).

% Intervals with infinite endpoints are tricky and the semantics fuzzy
% around the edges, but we'll try to handle them gracefully. An unbound
% enumeration will eventually lead to stack overflow, however. No, the
% event.
% TODO: test with "infinite" - now not allowed.
test(interval_positive_infinity_unbound_ascending
    ,[
     blocked('Unbound enumeration'),
     error(resource_error(stack),global)]):-
	findall(N, interval(1, inf, N), _Ns).

test(interval_positive_infinity_unbound_descending
    ,[
     blocked('Unbound enumeration'),
     error(resource_error(stack),global)]):-
	findall(N, interval(inf, 1, N), _Ns).

% An infinite interval does not necessarily have to be enumerated
% infinitely. The following idiom is also useful for testing.
test(interval_positive_infinity_ascending, []):-
	Enumeration_limit = 10
	,findall(N
	       ,(interval(1,inf,1,N)
		,(   N =< Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [1,2,3,4,5,6,7,8,9,10].

% Strides can be nonunit
test(interval_positive_infinity_ascending_nonunit_stride, []):-
	Enumeration_limit = 6
	,findall(N
	       ,(interval(0,inf,2,N)
		,(   N =< Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [0,2,4,6].

% Positive infinity as a left-hand endpoint can be used to count down.
% This is where things start getting hairy: instead of a number, we
% generate a compound, inf-N where N a number increasing from 0 to
% positive infinity.
test(interval_positive_infinity_descending, []):-
	Enumeration_limit = 5
	,findall(N
	       ,(interval(inf,1_000_000,1,N)
		,(   N =.. [-,inf,N_]
		     ,N_ =< Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [inf-0,inf-1,inf-2,inf-3,inf-4,inf-5].

% Nonunit strides can be used to count down from a left-hand endpoint
% in positive infinity.
test(interval_positive_infinity_descending_nonunit_strides, []):-
	Enumeration_limit = 20
	,findall(N
	       ,(interval(inf,-1_000,5,N)
		,(   N =.. [-,inf,N_]
		     ,N_ =< Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [inf-0,inf-5,inf-10,inf-15,inf-20].

% Attempting to do arithmetic with an infinity compound will
% most probably lead to arithmetic overflow (because of the attempt to
% evaluate "inf").
test(interval_positive_infinity_compound_addition
    ,[
     blocked('May be platform-dependent'),
     error(evaluation_error(float_overflow)
	   ,context(system:(is)/2,N))
     ]):-
	interval(inf, 3, N)
	,_A is N + 1.

test(interval_positive_infinity_compound_comparison
    ,[
     blocked('May be platform-dependent'),
     error(evaluation_error(float_overflow)
	   ,context(system:(>)/2,N))
     ]):-
	interval(inf, 3, N)
	,N > 0.

% A zero stride between positive infinity and itself is still valid
test(interval_positive_infinity_zero_stride, []):-
	interval(inf,inf,0,inf).

% Any other stride in that case is invalid
test(interval_positive_infinity_invalid_nonzero_stride, [fail]):-
	interval(inf,inf,1,_).

% Unbound enumeration from, or to, negative infinity, will lead to stack
% overflow (etc etc).
test(interval_negative_infinity_unbound_ascending
    ,[
     blocked('Unbound enumeration'),
     error(resource_error(stack),global)]):-
	findall(N, interval(-inf, 138, N), _Ns).

test(interval_negative_infinity_unbound_descending
    ,[
     blocked('Unbound enumeration'),
     error(resource_error(stack),global)]):-
	findall(N, interval(-13948, -inf, N), _Ns).

% A left-hand endpoint in negative infinity can be used to count up.
% This time, what is generated is always a compound, -inf+N where N a
% number between 0 and positive infinity.
test(interval_negative_infinity_ascending, []):-
	Enumeration_limit = 7
	,findall(N
	       ,(interval(-inf,14,1,N)
		,(   N =.. [+,-inf,N_]
		     ,N_ =< Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [-inf+0,-inf+1,-inf+2,-inf+3,-inf+4,-inf+5,-inf+6,-inf+7].

test(interval_negative_infinity_ascending_nonunit_stride, []):-
	Enumeration_limit = 19
	,findall(N
	       ,(interval(-inf,-401,3,N)
		,(   N =.. [+,-inf,N_]
		     ,N_ =< Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [-inf+0,-inf+3,-inf+6,-inf+9,-inf+12,-inf+15,-inf+18].

% This compound also may cause arithmetic overflow.
test(interval_negative_infinity_compound_addition
    ,[
     blocked('May be platform-dependent'),
     error(evaluation_error(float_overflow)
	   ,context(system:(is)/2,N))
     ]):-
	interval(-inf, 13, N)
	,_A is N + 4.

test(interval_negative_infinity_compound_comparison
    ,[
     blocked('May be platform-dependent'),
     error(evaluation_error(float_overflow)
	   ,context(system:(=<)/2,N))
     ]):-
	interval(-inf, -2245, N)
	,N =< 4.

% A right-hand endpoint in negative infinity can be used to count down.
test(interval_negative_infinity_descending, []):-
	Enumeration_limit = 1
	,findall(N
	       ,(interval(12,-inf,1,N)
		,(   N >= Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [12,11,10,9,8,7,6,5,4,3,2,1].

% And with a non-unit stride
test(interval_negative_infinity_descending_nonunit_stride, []):-
	Enumeration_limit = 24
	,findall(N
	       ,(interval(50,-inf,4,N)
		,(   N >= Enumeration_limit
		 ->  true
		 ;   !
		 ,fail
		 ))
	       ,Ns)
	,Ns == [50,46,42,38,34,30,26].

% Zero strides still mean the same thing.
test(interval_negative_infinity_zero_stride, []):-
	interval(-inf,-inf,0,-inf).

test(interval_negative_infinity_invalid_nonzero_stride, [fail]):-
	interval(-inf,-inf,1,_).

% Checking interval membership with infinite endpoints is possible.
test(interval_check_positive_infinity_right_endpoint, []):-
	interval(0, inf, 1, 30).

test(interval_check_positive_infinity_left_endpoint, [fail]):-
	interval(inf, 100, 2, 90).

test(interval_check_negative_infinity_left_endpoint, [fail]):-
	interval(-inf, 0, 1, 90).

test(interval_check_negative_infinity_right_endpoint, []):-
	interval(0, -inf, 1, 0).

:-end_tests(interval).


:-begin_tests(between_infinity).
/* between_infinity/3 is used to check membership to intervals
with infinite endpoints. It's not exported by module indexed_terms but
it can get a bit hairy so it needs testing. Note that the relation
tested takes into account "reverse" intervals (where the left endpoint
is lower than the right) and is therefore not the same as between/3.
Additionally, between/3 throws a hissy fit when it encounters "-inf" and
so cannot be used anyway.
*/

test(check_range_inf_inf_inf,[]):-
	indexed_terms:between_infinity(inf,inf,inf).

test(check_range_neg_inf_neg_inf_neg_inf,[]):-
	indexed_terms:between_infinity(-inf,-inf,-inf).

test(check_range_neg_inf_inf_num,[]):-
	indexed_terms:between_infinity(-inf,inf,0).

test(check_range_inf_neg_inf_num,[]):-
	indexed_terms:between_infinity(inf,-inf,0).

test(check_range_zero_inf_one,[]):-
	indexed_terms:between_infinity(0,inf,1).

test(check_range_one_inf_zero,[fail]):-
	indexed_terms:between_infinity(1,inf,0).

test(check_range_inf_zero_one,[]):-
	indexed_terms:between_infinity(inf,0,1).

test(check_range_inf_one_zero,[fail]):-
	indexed_terms:between_infinity(inf,1,0).

test(check_range_neg_inf_one_zero,[]):-
	indexed_terms:between_infinity(-inf,1,0).

test(check_range_neg_inf_zero_one,[fail]):-
	indexed_terms:between_infinity(-inf,0,1).

test(check_range_one_neg_inf_zero,[]):-
	indexed_terms:between_infinity(1,-inf,0).

test(check_range_zero_neg_inf_one,[fail]):-
	indexed_terms:between_infinity(0,-inf,1).

:-end_tests(between_infinity).


:-begin_tests(interval_5).

% A frequency of 1 is equivalent to an interval/4 call (and so not very
% useful):
test(interval_5_unit_freq_ascending, []):-
	findall(N
	       ,interval(1, 300, 1, 1, N)
	       ,Ns)
	,findall(M
		,interval(1,300,1,M)
		,Ns).

test(interval_5_unit_freq_descending, []):-
	findall(N
	       ,interval(245, -20, 1, 1, N)
	       ,Ns)
	,findall(M
		,interval(245,-20,1,M)
		,Ns).

% Basic usage - geneate series of repeating values:
test(interval_5_base_ascending, []):-
	findall(N
	       ,interval(1, 7, 2, 2, N)
	       ,Ns)
	,Ns == [1,1,3,3,5,5,7,7].

test(interval_5_base_descending, []):-
	findall(N
	       ,interval(1, -1, 1, 3, N)
	       ,Ns)
	,Ns == [1,1,1,0,0,0,-1,-1,-1].

% Changing the stride does not affect enumeration frequency.
test(interval_5_ascending_nonunit_stride, []):-
	P1 = 2
	,P2 = 1
	,F = 2
	,findall(N
	       ,interval(1, 5, P1, F, N)
	       ,Ns)
	,findall(M
		,interval(1, 5, P2, F, M)
		,Ms)
	,Ns == [1,1,3,3,5,5]
	,Ms == [1,1,2,2,3,3,4,4,5,5].

test(interval_5_descending_nonunit_stride, []):-
	P1 = 2
	,P2 = 1
	,F = 2
	,findall(N
	       ,interval(4, 1, P1, F, N)
	       ,Ns)
	,findall(M
		,interval(4, 1, P2, F, M)
		,Ms)
	,Ns == [4,4,2,2]
	,Ms == [4,4,3,3,2,2,1,1].

% Overall, an interval/5 call should suceed exactly F * (M + 1) times,
% where M the size of the requested interval and F the value repeat
% frequency. The size of an interval [I,J] by P, where I < J is equal to
% (J - I) / P rounded down. So the size of an interval [I,J] varying by
% P and repeating values with a frequency F is given by:
% M = (floor((J - I) / P) + 1) * F
test(interval_5_nondeterminism_ascending, []):-
	I = -5
	,J = 5
	,P = 2
	,F = 10
	% Size of the interval (plus correction) times the frequency param
	,M is (floor((J - I) / P) + 1) * F
	,findall(N
		,interval(I,J,P,F,N)
		,Ns)
	,length(Ns,M)
	,M = 60.

% When I > J, the size of the interval is (J - I) / P
test(interval_5_nondeterminism_descending, []):-
	_Ct = i(0)
	,I = 8
	,J = -10
	,P = 3
	,F = 40
	,M is (floor((I - J) / P) + 1) * F
	,findall(N
		,interval(I,J,P,F,N)
		,Ns)
	,length(Ns, M)
	,M = 280.

% Range checks succeed a number of times equal to the frequency
% parameter.
test(interval_5_check_ascending, []):-
	Ct = i(0)
	,F = 5
	% The following will count the number of times interval/5 suceeds
	% For membership checks, that count should match the frequency parameter.
	% The interval/5 call below should succeed exactly 5 times.
	,(   interval(-5,6,1,F,3)
	    ,arg(1,Ct,I)
	    ,I_ is I + 1
	    ,nb_setarg(1, Ct, I_)
	    ,fail
	 ;   arg(1,Ct,F)
	 ).

% In membership checks changing the stride does not affect the frequency
test(interval_5_check_ascending_varying_stride, []):-
	Ct = i(0)
	,P1 = 2
	,P2 = 1
	,F = 3
	,(   interval(1,5,P1,F,3)
	    ,arg(1,Ct,I)
	    ,I_ is I + 1
	    ,nb_setarg(1, Ct, I_)
	    ,fail
	 ;   arg(1,Ct,F)
	 )
	% Resetting counter!
	,nb_setarg(1, Ct, 0)
	,(   interval(1,5,P2,F,3)
	    ,arg(1,Ct,I)
	    ,I_ is I + 1
	    ,nb_setarg(1, Ct, I_)
	    ,fail
	 ;   arg(1,Ct,F)
	 ).

test(interval_5_check_descending_varying_stride, []):-
	Ct = i(0)
	,P1 = 2
	,P2 = 1
	,F = 3
	,(   interval(5,-6,P1,F,3)
	    ,arg(1,Ct,I)
	    ,I_ is I + 1
	    ,nb_setarg(1, Ct, I_)
	    ,fail
	 ;   arg(1,Ct,F)
	 )
	,nb_setarg(1, Ct, 0)
	,(   interval(5,-6,P2,F,3)
	    ,arg(1,Ct,I)
	    ,I_ is I + 1
	    ,nb_setarg(1, Ct, I_)
	    ,fail
	 ;   arg(1,Ct,F)
	 ).

% Degenerate intervals generate repeating sequences of the same number
test(interval_5_degenerate_interval_nonunit_freq, []):-
	findall(N
	       ,interval(64,64,0,3,N)
	       ,Ns)
	,Ns == [64,64,64].

:-end_tests(interval_5).


:-begin_tests(sequence).

% The basic use of sequence/5 is to create a term with its arguments
% bound to a sequence of numbers in a given interval, as generated by
% interval/4.
test(sequence_basic_usage_ascending, []):-
	sequence(v,-10,10,1,T)
	,T = v(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10).

test(sequence_basic_usage_descending, []):-
	sequence(v,-10,10,2,T)
	,T = v(-10,-8,-6,-4,-2,0,2,4,6,8,10).

% Infinite sequences will exhaust the global stack
test(sequence_infinite_ascending
    ,[
     blocked('Infinite sequence'),
     error(resource_error(stack),global)]):-
	sequence(v,-100,inf,1,_).

test(sequence_infinite_descending
    ,[
     blocked('Infinite sequence'),
     error(resource_error(stack),global)
     ]):-
	sequence(v,-inf,0,100,_).

% sequence/5 can be used to check a term's argument are bound to a
% sequence of numbers in an interval
test(sequence_check_ascending, []):-
	T = v(1,2,3,4,5)
	,sequence(v,1,5,1,T).

test(sequence_check_descending, []):-
	T = v(-10,-12,-14)
	,sequence(v,-10,-15,2,T).

test(sequence_check_nonunit_stride, [fail]):-
	T = v(1,4,7,11)
	,sequence(v,1,12,3,T).

% TODO: check sequences from degenerate intervals.

% In keeping with interval/4 semantics, sequence with coinciding
% endpoints must have a stride of 0 and produce a term with arity 1.
test(sequence_degenerate_interval, []):-
	random_between(1,100,R)
	,sequence(v,R,R,0,S)
	,compound_name_arity(S,v,1).

test(sequence_degenerate_interval, [fail]):-
	random_between(1,100,R)
	,sequence(v,R,R,1,_).

/*
Some interesting cases that will need testing, documentation and
possibly fixing:

?- sequence(v,0,0,0,S).
S = v(0).

?- sequence(v,3,3,1,S).
S = v().

?- sequence(v,0,0,1,S).
S = v().

% Degeberate seq with stride 0
?- sequence(v,3,3,0,S).
S = v(3).

*/

:-end_tests(sequence).


:-begin_tests(sequence_6).

% sequence/6 allows repetition
test(sequence_6_ascending, []):-
	sequence(v,-5,5,1,2,T)
	,T = v(-5,-5,-4,-4,-3,-3,-2,-2,-1,-1,0,0,1,1,2,2,3,3,4,4,5,5).

test(sequence_6_descending, []):-
	sequence(v,2,-2,2,3,T)
	,T = v(2,2,2,0,0,0,-2,-2,-2).

% sequence/6 can be used to check arguments of a term are bound to
% numbers in an interval
test(sequence_6_check, []):-
	T = v(1,1,1,3,3,3)
	,sequence(v,1,4,2,3,T).

% TODO: check degenerate sequences with frequency 0 or not 0.
% Generally check 0-frequencies.

test(sequence_6_degenerate_interval, []):-
	random_between(1,100,R)
	,random_between(1,10,F)
	,sequence(v,R,R,0,F,S)
	,compound_name_arity(S,v,F).

test(sequence_6_degenerate_interval, [fail]):-
	random_between(1,100,R)
	,random_between(1,10,F)
	,sequence(v,R,R,1,F,_).


:-end_tests(sequence_6).


:-begin_tests(subterm).

% subterm/3 can be used to select a range of indices, given as a numeric
% iterm, from an other iterm. Doesn't get much more complicated than
% that to be honest.

% Use sequence/[5,6] to generate an indexing term
test(subterm_basic_usage_with_sequences, []):-
	alphabetic_sequence(25,A)
	% sequence/5
	,sequence(v,1,8,1,S1)
	,subterm(A,S1,T1)
	,T1 = v(a,b,c,d,e,f,g,h)
	% sequence/6
	,sequence(v,2,10,2,3,S2)
	,subterm(A,S2,T2)
	,T2 == v(b,b,b,d,d,d,f,f,f,h,h,h,j,j,j).

% Or, you can always use interval/[4,3,5] with findall/3 and iterm/3
% ... but that's a little more cumbersome and also a bit redundant,
% because sequence/[5,6] basically do the same thing.
test(subterm_basic_usage_with_intervals, []):-
	alphabetic_sequence(25,A)
	% interval/4
	,findall(I
		,interval(1,12,3,I)
		,Is1)
	,iterm(i,Is1,It1)
	,subterm(A,It1,T1)
	,T1 == v(a,d,g,j)
	% interval/3
	,findall(I
		,interval(3,8,I)
		,Is2)
	,iterm(i,Is2,It2)
	,subterm(A,It2,T2)
	,T2 == v(c,d,e,f,g,h)
	% interval/5
	,findall(I
		,interval(1,20,4,2,I)
		,Is3)
	,iterm(i,Is3,It3)
	,subterm(A,It3,T3)
	,T3 == v(a,a,e,e,i,i,m,m,q,q).

:-end_tests(subterm).


:-begin_tests(slice).

% Basic usage: take the arguments between two positions in an iterm and
% create a new term with them.
test(slice_basic_usage_full_seq, []):-
	iterm(v,[a,b,c,d,e,f,g],T)
	,slice(T,1,7,S)
	,S = v(a,b,c,d,e,f,g).

test(slice_basic_usage_sub_seq, []):-
	iterm(v,[a,b,c,d,e,f,g,h,i],T)
	,slice(T,3,6,S)
	,S = v(c,d,e,f).

% Negative indices can be used to slice a term "from the end"
test(slice_negative_indices_full_seq, []):-
	iterm(v,[a,b,c,d,e,f,g,h,i,j,k,l],T)
	,slice(T,1,-1,S)
	,S == T.

test(slice_negative_indices_sub_seq, []):-
	iterm(v,[a,b,c,d,e,f,g,h,i,j],T)
	,slice(T,1,-3,S)
	,S == v(a,b,c,d,e,f,g,h).

% When both indices map to the same position, a single element sequence
% is created.
test(slice_monadic_seq_pos_pos, []):-
	iterm(v,[a,b,c,d,e,f],T)
	,slice(T,2,2,S)
	,S = v(b).

test(slice_monadic_seq_pos_neg, []):-
	iterm(v,[a,b,c,d,e,f],T)
	,slice(T,2,-5,S)
	,S = v(b).

test(slice_monadic_seq_neg_pos, []):-
	iterm(v,[a,b,c,d,e,f],T)
	,slice(T,-5,2,S)
	,S = v(b).

test(slice_monadic_seq_neg_neg, []):-
	iterm(v,[a,b,c,d,e,f],T)
	,slice(T,-5,-5,S)
	,S = v(b).

% Slice can be used to reverse an iterm by giving the index closest to
% the end of the iterm as the left-hand side endpoint of the slice:
test(slice_reverse_full_seq_neg_pos, []):-
	iterm(v, [a,b,c,d,e,f,g,h], T)
	,slice(T,-1,1,S)
	,S =.. [v|Vs]
	,reverse(Vs, Vs_)
	,T =.. [v|Vs_].

test(slice_reverse_full_seq_pos_pos, []):-
	iterm(v, [a,b,c,d,e,f,g,h], T)
	,slice(T,8,1,S)
	,S =.. [v|Vs]
	,reverse(Vs, Vs_)
	,T =.. [v|Vs_].

test(slice_reverse_full_seq_neg_neg, []):-
	iterm(v, [a,b,c,d,e,f,g,h], T)
	,slice(T,-1,-8,S)
	,S =.. [v|Vs]
	,reverse(Vs, Vs_)
	,T =.. [v|Vs_].

test(slice_reverse_full_seq_pos_neg, []):-
	iterm(v, [a,b,c,d,e,f,g,h], T)
	,slice(T,8,-8,S)
	,S =.. [v|Vs]
	,reverse(Vs, Vs_)
	,T =.. [v|Vs_].

% ... or partially reverse an iterm:
test(slice_reverse_sub_seq_neg_neg, []):-
	iterm(v, [a,b,c,d,e,f,g,h,i,j,k,l,m],T)
	,slice(T,-3,-8,S)
	,S == v(k,j,i,h,g,f).

test(slice_reverse_sub_seq_pos_pos, []):-
	iterm(v, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o],T)
	,slice(T,8,2,S)
	,S == v(h,g,f,e,d,c,b).

test(slice_reverse_sub_seq_neg_pos, []):-
	iterm(v, [a,b,c,d,e,f,g,h,i,j,k,l],T)
	,slice(T,-4,4,S)
	,S == v(i,h,g,f,e,d).

test(slice_reverse_sub_seq_pos_pos, []):-
	iterm(v, [a,b,c,d,e],T)
	,slice(T,4,2,S)
	,S == v(d,c,b).

% Indices outside the bounds of the iterm cause slice/4 to fail
% silently. This is consistent with the behaviour of iterm_value/3.
test(slice_right_endpoint_too_large, [fail]):-
	iterm(v, [a,b,c,d,e], T)
	,slice(T,1,8,_).

test(slice_left_endpoint_too_large, [fail]):-
	iterm(v, [a,b,c], T)
	,slice(T,5,2,_).

test(slice_left_endpoint_too_small, [fail]):-
	iterm(v, [a,b,c,d], T)
	,slice(T,-5,3,_).

test(slice_right_endpoint_too_small, [fail]):-
	iterm(v, [a,b], T)
	,slice(T,1,-3,_).

% For terms with arity greater than 0, slices with a zero-endpoint are
% invalid
test(slice_left_endpoint_zero, [fail]):-
	iterm(v, [a,b,c,d,e,f,g,h], T)
	,slice(T,0,3,_).

test(slice_right_endpoint_zero, [fail]):-
	iterm(v, [a,b,c,d,e], T)
	,slice(T,2,0,_).

test(slice_left_both_endpoints_zero, [fail]):-
	iterm(v, [a,b,c,d,e,f,g], T)
	,slice(T,0,0,_).

% Slicing an empty iterm fails silently.
% This too is consistent with the behaviour of iterm_value/3 (which
% fails silently when attempting to access any argument of an empty
% iterm.
test(slice_empty_iterm_zero_slice, [fail]):-
	iterm(v, [], T)
	,slice(T,0,0,_).

% Non-zero indices still fail to slice an empty iterm.
test(slice_empty_iterm_slice_zero_to_one, [fail]):-
	iterm(v, [], T)
	,slice(T,0,1,_).

test(slice_empty_iterm_slice_one_to_zero, [fail]):-
	iterm(v, [], T)
	,slice(T,1,0,_).

test(slice_empty_iterm_slice_one_to_one, [fail]):-
	iterm(v, [], T)
	,slice(T,1,1,_).

:-end_tests(slice).


:-begin_tests(dice).

test(dice_basic_usage, []):-
	iterm(v,[a,b,c,d,e,f,g],T)
	,dice(T,2,Ds)
	,Ds == [v(a,b),v(b,c),v(c,d),v(d,e),v(e,f),v(f,g)].

test(dice_width_one, []):-
	iterm(v,[a,b,c,d,e,f],T)
	,dice(T,1,Ds)
	,Ds == [v(a),v(b),v(c),v(d),v(e),v(f)].

test(dice_full_width, []):-
	iterm(v, [a,b,c,d,e], T)
	,compound_name_arity(T,_,A)
	,dice(T,A,Ds)
	,Ds = [T].

% Zero-width chunks are invalid.
test(dice_width_zero, [fail]):-
	iterm(v,[a,b,c,d,e,f,g],T)
	,dice(T,0,_).

% Negative-width chunks are invalid.
test(dice_negative_width, [fail]):-
	iterm(v,[a,b,c],T)
	,dice(T,-2,_).

% Zero-arity terms may not be diced.
test(dice_zero_arity_terms, [fail]):-
	iterm(v,[],T)
	,dice(T,0,_).

:-end_tests(dice).

:-begin_tests(window).

% TODO: add tests for ground Ws - window/6 may not be used for checking.

% Tests using random values can be run many times quickly like this:
% forall(between(1,1000,_), run_tests(window)).
%
% ... that takes a while, obviously.

/*
 *   == Basic usage ==
 *
*/

% window/6 scans an iterm t, starting at index i and ending at j,
% collecting k arguments then moving n indices in each timestep.
test(window_basic_usage_ascending, []):-
	A = 10 % iterm arity
	,I = 1 % left scan endpoint
	,J = 10 % right endpoint
	,K = 2 % window width
	,N = 2 % window stride
	% So, hang on. Why is this random, again?
	,partly_random_iterm(_,A,T,I,J,K,N,_,_,_,asc)
	,window(T,I,J,K,N,Ws)
	,Ws == [v(1,2),v(3,4),v(5,6),v(7,8),v(9,10)].

% Reverse endpoints to scan the term back-to-front.
test(window_basic_usage_descending, []):-
	A = 10
	,I = 10 % reversed endpoints
	,J = 1 % I > J
	,K = 2
	,N = 2
	,partly_random_iterm(_,A,T,I,J,K,N,_,_,_,desc)
	,window(T,I,J,K,N,Ws)
	,Ws == [v(10,9),v(8,7),v(6,5),v(4,3),v(2,1)].

% The endpoint parameters, I, J determine the range of the scan - the
% indices between which arguments may be selected (depending on width
% and stride).
test(window_basic_usage_endpoint_semantics, [
	 %blocked('Failing with new version')
     ]):-
	iterm(v,[a,b,c,d,e,f,g,h],T)
	,compound_name_arity(T,v,A)
	% A pair of endpoints covering the entire term
	% allow every argument of the term to be selected.
	,I1 = 1
	,J1 = A
	,window(T,I1,J1,1,1,Ws1)
	,assertion(Ws1 == [v(a),v(b),v(c),v(d),v(e),v(f),v(g),v(h)])
	% Endpoints in reverse order covering the entire term
	% do the same in reverse
	,window(T,J1,I1,1,1,Ws1_desc)
	,assertion(Ws1_desc == [v(h),v(g),v(f),v(e),v(d),v(c),v(b),v(a)])
	% A shorter span means fewer arguments can be selected.
	,I2 = 3
	,J2 = 5
	,window(T,I2,J2,1,1,Ws2)
	,assertion(Ws2 == [v(c),v(d),v(e)])
	% And in reverse:
	,window(T,J2,I2,1,1,Ws2_desc)
	,assertion(Ws2_desc == [v(e),v(d),v(c)])
	% When the two endpoints coincide
	% only a single argument may be selected.
	,I3 = 4
	,J3 = 4
	,window(T,I3,J3,1,0,Ws3)
	,assertion(Ws3 == [v(d)])
	% Same in reverse:
	,window(T,J3,I3,1,0,Ws3)
	,assertion(Ws3 == [v(d)]).

% The width parameter, k, controls the number of terms selected in each
% step of a full scan from I to J.
test(window_basic_usage_width_semantics, [
	 %blocked('Failing with new version')
     ]):-
	iterm(v,[a,b,c,d,e], T)
	,compound_name_arity(T,v,A)
	% A width of 1 makes little unary terms
	% Aaaaw!
	,K1 = 1
	,window(T,1,A,K1,1,Ws1)
	,assertion(Ws1 == [v(a),v(b),v(c),v(d),v(e)])
	% The order of endpoints does not affect window width:
	,window(T,A,1,K1,1,Ws1_desc)
	,assertion(Ws1_desc == [v(e),v(d),v(c),v(b),v(a)])
	% A width of 2 makes binary terms
	,K2 = 2
	,window(T,1,A,K2,1,Ws2)
	,assertion(Ws2 == [v(a,b),v(b,c),v(c,d),v(d,e)])
	% With endpoints in reverse order, width is the same
	% but order is reversed. Well, duh.
	,window(T,A,1,K2,1,Ws2_desc)
	,assertion(Ws2_desc == [v(e,d),v(d,c),v(c,b),v(b,a)])
	% A width of 3 makes ternary terms, etc.
	,K3 = 3
	,window(T,1,A,K3,1,Ws3)
	,assertion(Ws3 == [v(a,b,c),v(b,c,d),v(c,d,e)])
	% Reverse-order endpoints scan in reverse.
	,window(T,A,1,K3,1,Ws3_desc)
	,assertion(Ws3_desc == [v(e,d,c),v(d,c,b),v(c,b,a)])
	% It is even possible to select 0 arguments
	% yielding only empty iterms:
	,K0 = 0
	,window(T,1,A,K0,1,Ws0)
	,assertion(Ws0 == [v(),v(),v(),v(),v()])
	% And in reverse order also:
	,window(T,A,1,K0,1,Ws0_desc)
	,assertion(Ws0_desc == [v(),v(),v(),v(),v()]).

% The stride parameter, n, controls the starting index of each step of
% a scan from I to J. Or, you can see it as controlling the number of
% arguments skipped in each step.
test(window_basic_usage_stride_semantics, [
	 %blocked('Failing with new version')
     ]):-
	iterm(v,[a,b,c,d,e,f],T)
	,compound_name_arity(T,v,A)
	% A stride of 1 stops at each index over the window span.
	,N1 = 1
	,window(T,1,A,1,N1,Ws1)
	,assertion(Ws1 == [v(a),v(b),v(c),v(d),v(e),v(f)])
	% Reverse-order endpoints do the same in reverse
	,window(T,A,1,1,N1,Ws1_desc)
	,assertion(Ws1_desc == [v(f),v(e),v(d),v(c),v(b),v(a)])
	% A stride of 2 stops in every second index after the first,
	% skipping one argument in each step.
	,N2 = 2
	,window(T,1,A,1,N2,Ws2)
	,assertion(Ws2 == [v(a),v(c),v(e)])
	% Scanning in reverse may produce different results
	% because enumeration starts at a different value
	% in the example, we count down twos from 6
	% instead of up from 1: {6,4,2} =\= {1,3,5}, innit.
	,window(T,A,1,1,N2,Ws2_desc)
	,assertion(Ws2_desc == [v(f),v(d),v(b)])
	% A stride of 3 stops in every third index
	% skipping two arguments each step, etc.
	,N3 = 3
	,window(T,1,A,1,N3,Ws3)
	,assertion(Ws3 == [v(a),v(d)])
	% And in reverse:
	,window(T,A,1,1,N3,Ws3_desc)
	,assertion(Ws3_desc == [v(f),v(c)])
	% A stride of 0 means a single scan step is performed
	% Note that with N = 0, I and J _must_ coincide.
	,N0 = 0
	,window(T,2,2,1,N0,Ws0)
	% Can't reverse [2,2], now can we?
	,assertion(Ws0 == [v(b)]).

% Zero width windows yield only empty iterms
test(window_zero_width_ascending, [
	 % blocked('Failing in _old_ version')
     ]):-
	K = 0
	,partly_random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	,window(T,I,J,K,N,Ws)
	%,format('~nI:~w J:~w K:~w N:~w',[I,J,K,N])
	% All results are empty-iterms.
	,forall(member(W,Ws)
	       ,assertion(W == v())).

% Same in descending order.
test(window_zero_width_descending, []):-
	K = 0
	,partly_random_iterm(_,_,T,I,J,K,N,_,_,_,desc)
	,window(T,I,J,K,N,Ws)
	,forall(member(W,Ws)
	       ,assertion(W == v())).

% Note: zero-strides are special an magickal so they get more attention
% further below, under "Effects of varying stride"

/*
 *   == Slicing and dicing ==
 *
*/

% window/6 generalises the behaviour of slice/4 and dice/3
% and can be used in their stead.

% Use a window with a width equal to its span to obtain the same
% results as slice/4 on the same iterm and endpoints:
test(window_for_slicing_ascending, [
	% blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,_,N,S,_,_,asc)
	,assertion(slice(T,I,J,W))
	% The width of a slice from to I,J is equal to
	% J - I + 1 which is the same as the span of a window
	% from I to J (plus one-based index correction).
	% So the following uses span, S, in place of width.
	,assertion(window(T,I,J,S,N,[W])).

% Descending order endpoints can also be used for slicing an iterm
% backwards.
test(window_for_slicing_descending, [
	% blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,_,N,S,_,_,desc)
	,assertion(slice(T,I,J,W))
	,assertion(window(T,I,J,S,N,[W])).

% To get the behaviour of dice/3, use a window covering the entire iterm
% and a stride of 1 (or 0 if I = J).
% Note that dice/3 does not accept 0-widths, so to make sure that the
% dice call is valid this test uses the window's span value as width
% (because span is always at least 1 and width at most equal to span).
test(window_for_dicing_ascending, []):-
	random_between(1,10,A)
	,blank_iterm(v,A,T)
	,I = 1
	,window_span(I,A,S)
	% Degenerate interval handling
	,(   A == 1 % i.e. coincides with I
	 ->  N = 0
	;    N = 1
	 )
	% Span value used instead of width to avoid 0-width failures.
	,dice(T,S,Ds)
	%,format('~nI: ~w A: ~w S: ~w N: ~w',[I,A,S,N])
	,window(T,I,A,S,N,Ws)
	% Unification rather than == because the terms are blank
	% and [v(X)] == [v(Y)] fails, ja?
	,assertion(Ds = Ws).

% You can't dice a term backwards with dice/3 because you can't
% specify endpoints- but you _can_ with window/6 and the result is the
% reverse of dice/3.
test(window_for_dicing_descending, []):-
	random_between(1,10,A)
	,blank_iterm(v,A,T)
	,I = 1
	,window_span(I,A,S)
	,(   A == 1
	 ->  N = 0
	;    N = 1
	 )
	,dice(T,S,Ds)
	,window(T,A,I,S,N,Ws)
	% Window and dice results are exactly back-to-front.
	,assertion(Ds \== Ws)
	,assertion(reverse(Ds,Ws)).

/*
 *   == Parameter constraints ==
 *
*/
% window/6 is a bith particular when it comes to parameter ranges- it
% likes to raise a type error when parameter constraints are defiled.

test(window_endpoint_ranges_zero_i
    ,[error(type_error(between(1,J),I),_)]):-
	random_iterm(_,_,T,_,J,K,N,_,_,_,asc)
	% J = 0 is also invalid!
	,I = 0
	,window(T,I,J,K,N,_).

test(window_endpoint_ranges_zero_j
    ,[error(type_error(between(I,A),J),_)]):-
	random_iterm(_,A,T,I,_,K,N,_,_,_,asc)
	% I > J is invalid too so this makes it
	% hard to see where the error is raised.
	,J = 0
	,window(T,I,J,K,N,_).

% TODO: allow negative endpoints!
test(window_endpoint_ranges_negative_i
    ,[error(type_error(between(1,J),I),_)]):-
	random_iterm(_,_,T,_,J,K,N,_,_,_,asc)
	,I = -1
	,window(T,I,J,K,N,_).

test(window_endpoint_ranges_negative_j
    ,[error(type_error(between(I,A),J),_)]):-
	random_iterm(_,A,T,I,_,K,N,_,_,_,asc)
	,J = -1
	,window(T,I,J,K,N,_).

test(window_endpoint_ranges_i_exceeds_arity
    ,[error(type_error(between(1,J),I_),_)]):-
	random_iterm(_,A,T,_,J,K,N,_,_,_,asc)
	,I_ is A + 1
	%,functor(T,_,A_)
	%,format('~n(i) A: ~w A_: ~w I_: ~w J: ~w K: ~w N: ~w',[A,A_,I_,J,K,N])
	,window(T,I_,J,K,N,_).

test(window_endpoint_ranges_j_exceeds_arity
    ,[error(type_error(between(I,A),J),_)]):-
	random_iterm(_,A,T,I,_,K,N,_,_,_,asc)
	,J is A + 1
	%,format('~n(j) A: ~w I: ~w J: ~w K: ~w N: ~w',[A,I,J,K,N])
	,window(T,I,J,K,N,_).

% As long as endpoint values are in valid ranges, they may be in
% descending order to request scanning the term back-to-front.
test(window_endpoints_in_descending_order ,[]):-
	random_iterm(_,_,T,I,J,K,N,_,_,_,desc)
	,assertion(window(T,I,J,K,N,_)).

% i and j may be equal (but stride must be 0!)
% TODO: Mnyeah. This is a bit meh. You give N = 0 so
% partly_random_iterm/10 will never allow J =\= I.
test(window_endpoint_ranges_i_equals_j, []):-
	N = 0
	,partly_random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	,assertion(J == I)
	,window(T,I,J,K,N,_).

% and they may be equal to arity
test(window_endpoint_ranges_i_equals_j_equals_a, []):-
	random_between(1,100,A)
	,I = A
	,J = I
	,partly_random_iterm(_,A,T,I,J,K,N,_,_,_,asc)
	,window(T,I,J,K,N,_).

% In either case, if I = J and N is not 0 an error is raised.
test(window_endpoint_ranges_i_equals_j_nonzero_stride
    ,[error(type_error(between(0,0),N),_)]):-
	random_between(1,100,A)
	,I = A
	,J = I
	,partly_random_iterm(_,A,T,I,J,K,_,S,_,_,asc)
	%,N = 1
	,random_between(1,S,N)
	,window(T,I,J,K,N,_).

test(window_width_ranges_negative_k
    ,[error(type_error(between(0,S),K),_)]):-
	K = -1
	,partly_random_iterm(_,_,T,I,J,_,N,S,_,_,asc)
	,window(T,I,J,K,N,_).

% Not affected by endpoint order
test(window_width_ranges_negative_k_descedning
    ,[error(type_error(between(0,S),K),_)]):-
	K = -1
	,partly_random_iterm(_,_,T,I,J,_,N,S,_,_,desc)
	,window(T,I,J,K,N,_).

test(window_width_ranges_k_exceeds_span
    ,[error(type_error(between(0,S),K),_)]):-
	partly_random_iterm(_,_,T,I,J,_,N,S,_,_,asc)
	,K is S + 1
	,window(T,I,J,K,N,_).

% Also not affected by endpoint order
test(window_width_ranges_k_exceeds_span_descending
    ,[error(type_error(between(0,S),K),_)]):-
	partly_random_iterm(_,_,T,I,J,_,N,S,_,_,desc)
	,K is S + 1
	,window(T,I,J,K,N,_).

test(window_stride_ranges_negative_n
    ,[error(type_error(between(0,S),N),_)]):-
	partly_random_iterm(_,_,T,I,J,K,_,S,_,_,asc)
	,N = -1
	,window(T,I,J,K,N,_).

% Still not affected by order
test(window_stride_ranges_negative_n_descending
    ,[error(type_error(between(0,S),N),_)]):-
	partly_random_iterm(_,_,T,I,J,K,_,S,_,_,desc)
	,N = -1
	,window(T,I,J,K,N,_).

test(window_stride_ranges_n_exceeds_span
    ,[error(type_error(between(0,S),N),_)]):-
	partly_random_iterm(_,_,T,I,J,K,_,S,_,_,asc)
	,N is S + 1
	,window(T,I,J,K,N,_).

% Yep.
test(window_stride_ranges_n_exceeds_span_descedning
    ,[error(type_error(between(0,S),N),_)]):-
	partly_random_iterm(_,_,T,I,J,K,_,S,_,_,desc)
	,N is S + 1
	,window(T,I,J,K,N,_).

% Width and stride may be equal.
test(window_width_stride_ranges_k_equals_n, []):-
	random_between(1,100,R)
	,blank_iterm(v, R, T)
	,compound_name_arity(T,v,A)
	,random_between(1,A,I)
	,random_between(I,A,J)
	,window_span(I,J,S)
	,(   I == J
	 ->  N = 0
	 ;   random_between(1,S,N)
	 )
	,K = N
	,assertion(window(T,I,J,K,N,_))
	% And in reverse order:
	,assertion(window(T,I,J,K,N,_)).

% Terms with arity 1 or 0 can be processed, but the valid parameter
% range is limited accordingly.
test(window_diminutive_terms, []):-
	T = v(a)
	,window(T,1,1,1,0,Ws)
	,Ws = [v(a)].

test(window_diminutive_terms
    ,[
	 error(type_error(between(1,0),0),_)
	 ,blocked('Error is stupid. Make it make sense')
     ]):-
	T = v()
	,window(T,0,0,0,0,_).

/*
 *   == Expected window size ==
 *
*/

% All results of a single window scan are the same length!
% Determined by the width parameter, bw.
test(window_basic_usage_constant_chunk_size_ascending, []):-
	random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	,window(T,I,J,K,N,Ws)
	,forall(member(W, Ws)
	       ,compound_name_arity(W,_,K)).

% Same goes when endpoints are in descending order.
test(window_basic_usage_constant_chunk_size_descending, []):-
	random_iterm(_,_,T,I,J,K,N,_,_,_,desc)
	,window(T,I,J,K,N,Ws)
	,forall(member(W, Ws)
	       ,compound_name_arity(W,_,K)).

% Scanning a term with window/6 yields the expected number of results,
% and we can calculate this number with window_size/5.
test(window_number_of_results_ascending, [
	 %blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	,window(T,I,J,K,N,Ws)
	%,nl
	%,writeln(T)
	%,format('I: ~w J: ~w K: ~w N: ~w M: ~w~n',[I,J,K,N,M])
	% random_iterm/11 calls window_size/5 and binds an output argument
	% but we'll check the window size here just to make it perfectly clear.
	,window_size(I,J,K,N,M)
	,assertion(length(Ws,M)).

% Endpoints in descending order also yield the expected number of
% results.
test(window_number_of_results_descending, [
	 %blocked('Failing with new version')
     ]):-
	partly_random_iterm(_,_,T,I,J,K,N,_,_,_,desc)
	,window(T,I,J,K,N,Ws)
	,window_size(I,J,K,N,M)
	,assertion(length(Ws,M)).

test(window_impossible_zero_size, []):-
	% Try with a larger number also,
	% but note that takes some time.
	% random_iterm/11 is not meant to be, like, efficient or anything.
	% It's just for testing.
	forall(between(1,100,_)
	      ,(random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	       ,window(T,I,J,K,N,Ws)
	       ,assertion(Ws \= [])
	       )
	      )
	% Same in descending order:
	,forall(between(1,100,_)
	      ,(random_iterm(_,_,T,I,J,K,N,_,_,_,desc)
	       ,window(T,I,J,K,N,Ws)
	       ,assertion(Ws \= [])
	       )
	      ).

/*
 *   == Effects of varying width ==
 *
*/

% As width increases the number of chunks decreases
test(window_size_decreasing_as_width_increases_ascending, [
	% blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,K,N,S,_,_,asc)
	% window parameters with width increasing from 0 to span
	,findall([I,J,K,N,M]
		,(between(0,S,K)
		 ,window_size(I,J,K,N,M)
		 )
		,Cs)
	% List of numbers of chunks is in decreasing order
	% meaning number of chunks goes down as width goes up.
	,findall(M
		,member([_,_,_,_,M],Cs)
		,Ms)
	,sort(0,@>=,Ms,Ms)
	% All window/6 calls succeed
	,forall(member([I,J,K,N,_],Cs)
	       ,(assertion(window(T,I,J,K,N,_))
		)
	       )
	% ... and produce the expected number of chunks.
	,forall(member([I,J,K,N,M],Cs)
	       ,(window(T,I,J,K,N,Ws)
		,length(Ws, M_)
		,assertion([I,J,K,N,M] == [I,J,K,N,M_])
		)
	       ).

% Endpoints in decreasing order work the same way.
test(window_size_decreasing_as_width_increases_descending, [
	% blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,K,N,S,_,_,desc)
	% window parameters with width increasing from 0 to span
	,findall([I,J,K,N,M]
		,(between(0,S,K)
		 ,window_size(I,J,K,N,M)
		 )
		,Cs)
	% List of numbers of chunks is in decreasing order
	% meaning number of chunks goes down as width goes up.
	,findall(M
		,member([_,_,_,_,M],Cs)
		,Ms)
	,sort(0,@>=,Ms,Ms)
	% All window/6 calls succeed
	,forall(member([I,J,K,N,_],Cs)
	       ,(assertion(window(T,I,J,K,N,_))
		)
	       )
	% ... and produce the expected number of chunks.
	,forall(member([I,J,K,N,M],Cs)
	       ,(window(T,I,J,K,N,Ws)
		,length(Ws, M_)
		,assertion([I,J,K,N,M] == [I,J,K,N,M_])
		)
	       ).

/*
 *   == Effects of varying stride ==
 *
*/

% As stride increases, the number of chunks decreases.
% While stride is between 1 and the maximum productive value, as
% calculated by maximum_stride/4, window/6 succeeds and produces a
% number of chunks between 1 and the number calculated by
% window_size/4. Once the maximum stride is reached, a single chunk will
% always be produced.
test(window_size_decreasing_as_stride_increases_ascending, [
	% blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,K,_,S,_,_,asc)
	,maximum_stride(I,J,K,Mn)
	% Max stride is no more than span
	,assertion(Mn =< S)
	% window parameters with stride increasing
	% from 0 to the maximum useful value
	%,format('Max stride:~w~n', [Mn])
	,findall([I,J,K,N,M]
		,(between(0,Mn,N)
		 ,window_size(I,J,K,N,M)
		 %,format('~nI: ~w J: ~w K: ~w N: ~w M: ~w',[I,J,K,N,M])
		 )
		,Cs)
	%,nl
	% With maximum stride a single chunk is produced.
	%,forall(member(C,Cs)
	%       ,writeln(C))
	% TODO: This is not testing that there's only
	% a single one-length result in there. Should elaborate.
	,assertion(last(Cs, [_,_,_,_,1]))
	% All window/6 calls succeed
	,forall(member([I,J,K,N,_],Cs)
	       ,(assertion(window(T,I,J,K,N,_))
		)
	       )
	%... and produce the expected number of chunks
	,forall(member([I,J,K,N,M],Cs)
	       ,(window(T,I,J,K,N,Ws)
		,length(Ws, M_)
		,assertion([I,J,K,N,M] == [I,J,K,N,M_])
		)
	       ).

% Windows with endpoints in descending order result in the same number
% of results as those with endpoints in ascending order.
test(window_size_decreasing_as_stride_increases_descending, [
	% blocked('Failing with new version')
     ]):-
	random_iterm(_,_,T,I,J,K,_,S,_,_,desc)
	% random_iterm exports this
	,maximum_stride(I,J,K,Mn)
	,assertion(Mn =< S)
	,findall([I,J,K,N,M]
		 % This may cause an error - were is it?
		,(between(0,Mn,N)
		 ,window_size(I,J,K,N,M)
		 )
		,Cs)
	,assertion(last(Cs, [_,_,_,_,1]))
	,forall(member([I,J,K,N,_],Cs)
	       ,(assertion(window(T,I,J,K,N,_))
		)
	       )
	,forall(member([I,J,K,N,M],Cs)
	       ,(window(T,I,J,K,N,Ws)
		,length(Ws, M_)
		,assertion([I,J,K,N,M] == [I,J,K,N,M_])
		)
	       ).

% Windows have a maximum stride- a value for which results change no
% further (and are always a single "chunk"). This maximum stride value
% can be calculated with maximum_stride/4 and may be used to avoid
% producing unnecessary results. Yeah, just like a cut.
test(window_converges_to_unit_size_ascending, []):-
	random_iterm(_,_,T,I,J,K,_,S,_,Mn,asc)
	,assertion(S >= Mn)
	% Degenerate interval handling - if N = 0
	% then its maximum is also 0 and we can't
	% increase it to more than that without errors.
	,(   Mn > 0
	 ->  Lim = S
	 ;   Lim = 0
	 )
	 % Now we can safely generate windows with strides
	 % between their maximum productive value (Mn) and
	 % the limit defined above.
	,findall(Ws
		,(between(Mn,Lim,N)
		 ,window(T,I,J,K,N,Ws)
		 )
		,[R|Rs])
	% Each result has a length of 1
	,forall(member(Ur,Rs)
	       ,assertion(length(Ur,1)))
	% ... and they're all the same
	,forall(member(R_,Rs)
		,assertion(R_ == R)
	       ).

% Windows with endpoints in descending order also "converge" in the same
% manner as ones with endpoints in ascending order.
test(window_converges_to_unit_size_descending, []):-
	random_iterm(_,_,T,I,J,K,_,S,_,Mn,desc)
	% Just checking
	,assertion(J =< I)
	,assertion(S >= Mn)
	,(   Mn > 0
	 ->  Lim = S
	 ;   Lim = 0
	 )
	,findall(Ws
		,(between(Mn,Lim,N)
		 ,window(T,I,J,K,N,Ws)
		 )
		,[R|Rs])
	,forall(member(Ur,Rs)
	       ,assertion(length(Ur,1)))
	,forall(member(R_,Rs)
		,assertion(R_ == R)
	       ).

% In keeping with interval/4 semantics, a stride of 0 is only valid with
% a degenerate interval, i.e. when the two endpoints match so I = J.
test(window_endpoints_of_zero_stride_must_coincide, []):-
	N = 0
	,partly_random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	,assertion(window(T,I,J,K,N,_)).

% Zero stride intervals yield a single arity-one iterm, unless the
% width is 1 in which case, they yield a single iterm of arity 0.
% Don't forget: since N = 0, I should be the same as J!
test(window_zero_stride, []):-
	N = 0
	,partly_random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	,assertion(I == J)
	,window(T,I,J,K,N,[W])
	,compound_name_arity(W,v,A)
	%,format('~nI:~w J:~w K:~w N:~w W:~w A:~w',[I,J,K,N,W,A])
	,(   K == 1
	 ->  assertion(A == 1)
	 ;   assertion(A == 0)
	 ).

% Zero strides always yield a a window of size one.
test(window_zero_stride_window_size_one, [
	 %blocked('Failing with new version')
     ]):-
	forall(between(1,100,_)
	      ,(random_iterm(_,_,T,I,J,K,N,_,_,_,asc)
	       ,(   N == 0
		->  assertion(window(T,I,J,K,N,[_]))
		;   true
		)
	       )
	      )
	% Not affected by endpoint order
	,forall(between(1,100,_)
	      ,(random_iterm(_,_,T,I,J,K,N,_,_,_,desc)
	       ,(   N == 0
		->  assertion(window(T,I,J,K,N,[_]))
		;   true
		)
	       )
	      ).

% The maximum stride of a window with coinciding endpoints is 0.
% Which, btw, means that's the only valid stride value for such windows.
test(window_max_stride_of_degenerate_intervals, []):-
	random_between(1,100,I)
	,forall(between(1,100,K)
	      ,assertion(maximum_stride(I,I,K,0))
	      ).

% Ensure that a zero stride makes sense
%test(window_zero_stride_consistent_results, []).

/*
 *   == Relation of span to window size, stride ==
 *
*/

% TODO

:-end_tests(window).
