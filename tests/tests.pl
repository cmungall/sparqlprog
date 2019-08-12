
:- load_files([
    tests/basic_test,
    tests/builtins_test,
    tests/search_test,
    tests/dataframe_test,
    tests/labelutils_test,
    tests/builtins_test,
    tests/prolog_test,
    tests/search_viz_test,
    tests/owl_test
], [ if(not_loaded) ]).
