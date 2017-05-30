:-prolog_load_context(directory, Dir)
  ,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
%user:file_search_path(lib, project_root(lib)).
%user:file_search_path(tests, project_root(src)).

:-doc_browser.

:-use_module(src(indexed_terms)).

edit_files:-
	edit(project_root(load_project))
	,edit(src(indexed_terms))
	,edit(src('indexed_terms.plt'))
	,edit(src(test_helpers))
	.
:-edit_files.

:-load_test_files([]).
:-run_tests.

% Large iterms (A > 1_000_000) may require a larger stack.
%:-set_prolog_stack(global, limit(2**9*10**6)).
:-prolog_stack_property(global, limit(X))
  ,format('Global stack limit ~D~n',[X]).
