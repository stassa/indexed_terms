:-module(corpus_statistics, [examples_module/2
                            ,examples_corpus/1
                            ,corpus_ngrams/3
                            ]).

:-use_module(tokeniser(configuration)).
:-use_module(src(indexed_terms)).
:-use_module(tokeniser(tokeniser_utilities)).


%!	ngram_start(?Start) is semidet.
%
%	Start token prepended to sentences for n-gram construction.
%
ngram_start('$Start').


%!	ngram_end(?End) is semidet.
%
%	End token appended to sentences for n-gram construction.
%
ngram_end('$End').


%!	examples_module(+File,-Module) is det.
%
%	Retrieve the name of the examples Module in the given File.
%
%	File may be the atom "current" in which case Module is the name
%	of the module in the currently specified examples_filename/1
%	option.
%
%	If a different examples file is required to be loaded, its base
%	name only can be given (e.g. "my_corpus_file.txt")
%
%	Examples files are expected to be in the /output directory. This
%	is currently hardcoded in the project load file; if changed, it
%	must also be changed everywhere else where it's referenced in
%	the source.
%
%	An examples module is a module that contains only F/1
%	predicates, where F the currently configured
%	examples_term_functor/1 option.%
%	Examples terms are reexported from the examples module,
%	therefore removing their prior definition.
%
examples_module(Fn, M):-
	(   Fn == current
	->  examples_filename(Fn_)
	;   must_be(nonvar,Fn)
	   ,Fn_ = Fn
	)
	,file_name_path_module_name(output(Fn_),Path,M)
	,(   \+ exists_file(Path)
	 ->  throw('No examples module in path':Path)
	 ;   true
	 )
	,examples_term_functor(F)
	,abolish(F/1)
	,reexport(output(Fn_), except([F/1 as F])).



%!	examples_corpus(+Examples) is det.
%
%	All unique examples in the examples corpus.
%
examples_corpus(Examples):-
	configuration:examples_term_functor(Functor)
	,examples_module(current, M)
	,functor(Examples_term,Functor,1)
	,Examples_term =.. [Functor|[Example]]
	% Remember that Example = [H|T]
	,findall(Example
                ,M:Examples_term
                ,Examples).



%!  corpus_ngrams(+Corpus,+N,-Ngrams) is det.
%
%   Collect all Ngrams in Corpus.
%
corpus_ngrams(Cs, N, Ms):-
    findall(Ns
           ,(member(Ws,Cs)
            ,sentence_ngrams(Ws, N, Ns)
            )
           ,Ns_)
    ,flatten(Ns_,Ms).



%!  sentence_ngrams(+Sentence,+N,-Ngrams) is det.
%
%   Collect all Ngrams in a Sentence.
%
%   If Sentence length is less than N, Sentence will be padded with the
%   padding character specified in ngram_padding/1.
%
sentence_ngrams(Ws,N,Ns):-
    length(Ws, M)
    ,D is N - M
    ,padded_sentence(Ws,D,Ws_)
    ,iterm(ngram,Ws_,St)
    ,compound_name_arity(St,_,A)
    ,window(St,1,A,N,1,Ns).


%!	padded_sentence(+Sentence,+D,-Padded) is det.
%
%	Pad a Sentence with D padding characters.
%
%	The ngram start and end symbols are always affixed to the start
%	and end of Sentence, respectively. Additionally, D determines
%	how many times to padd the end of Sentence using the ngram end
%	symbol.
%
%	D is the difference between the model order, N and the length of
%	Sentence. If D is 0 or more, no padding is needed. If D is less
%	than 0, padding by that many characters is needed.
%
padded_sentence(Ss,D,St_):-
    ngram_start(S)
    ,ngram_end(E)
    ,D_ is D + 1 % for the end symbol
    ,(   D_ > 0
     % Right-padding list
    ->   list_of(D_, E, Es)
     ;   Es = [E]
    )
    ,append([S|Ss],Es,St_).


%!	list_of(+K,+Element,-List) is det.
%
%	Bind a List of length K to Element.
%
list_of(K,El,Ls):-
	once(list_of(K,1,[El],Ls)).


%!	list_of(+K,+I,+El,-Binder) is nondet.
%
%	Business end of list_of/3.
%
list_of(K,K,Ls,Ls).
list_of(K,I,[El|Ls],Bind):-
	succ(I,I_)
	,list_of(K,I_,[El,El|Ls],Bind).
