
install_requirements :-
        writeln(loading),
        ensure_loaded(pack),
        Opts=[interactive(false)],
        forall(requires(X),
               pack_install(X, Opts)).

install_optional_packs :-
        writeln(loading),
        ensure_loaded(pack),
        ensure_loaded(packplus),
        Opts=[interactive(false)],
        \+ \+ clause(optional_pack(_),_),
        forall(optional_pack(X),
               pack_install(X, Opts)).

install_this :-
        Opts=[interactive(false)],
        install_requirements,
        install_optional_packs,
        name(X),
        pack_install(X,Opts).

install_this_full :-
        install_this,
        install_optional_packs.
