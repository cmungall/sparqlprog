# SPARQLProg installation

## Install from pack

1. Install SWI-Prolog from http://www.swi-prolog.org

2. Install using SWI pack installer

    $ swipl
    ? pack_install(sparqlprog).

## Install from GitHub

1. Install SWI-Prolog from http://www.swi-prolog.org

2. Install requirements

    swipl -g "[install],install_requirements"

3. Get the latest sparqlprog source from github. No installation steps are
required. Just add it to your path (changing the directory if necessary):

    `export PATH=$PATH:$HOME/sparqlprog/bin`

You may also need to ensure the library is added to your path

The easiest way is to create a symlink:

    ~/lib/swipl/pack/sparqlprog -> ~/repos/sparqlprog
