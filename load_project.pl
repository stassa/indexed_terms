:-prolog_load_context(directory, Dir)
  ,asserta(user:file_search_path(project_root, Dir)).

% Top directory required by pac (the Swi-Prolog package manager).
user:file_search_path(prolog, project_root(prolog)).
user:file_search_path(src, prolog(src)).
user:file_search_path(tests, prolog(tests)).

:-doc_browser.

:-use_module(src(indexed_terms)).
:-consult(tests('indexed_terms.plt')).

edit_files:-
	edit(project_root(load_project))
	,edit('pack.pl')
	,edit(src(indexed_terms))
	,edit(tests('indexed_terms.plt'))
	,edit(tests(test_helpers))
	.
% Comment out to disable opening project files in the IDE at startup.
:-edit_files.

% Comment out to disable loading and running tests at startup.
:-load_test_files([]).
:-run_tests.

% Large iterms (A > 1_000_000) may require a larger stack.
%:-set_prolog_stack(global, limit(2**9*10**6)).
:-prolog_stack_property(global, limit(X))
  ,format('Global stack limit ~D~n',[X]).
