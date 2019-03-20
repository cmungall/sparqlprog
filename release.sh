# STEPS:
#  - update pack.pl
#  - commit and remember to push
#  - make a new release on GH
# run below, modifying version
pack-release.pl cmungall/sparqlprog `swipl -l pack.pl -g "version(V),writeln(V),halt."`
