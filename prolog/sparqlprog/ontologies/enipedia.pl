:- module(enipedia,
          [
           powerplant/1,

           primary_fuel_type/2,
           fuel_type_ppc/2,
           
           kg_co2pmwh/2,
           kg_co2pmwh_nd/2
           ]).

:- sparql_endpoint( enipedia, 'http://enipedia.tudelft.nl/sparql').

:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(dcterms,'http://purl.org/dc/terms/').

:- rdf_register_prefix(cat,'http://enipedia.org/wiki/Category:').
:- rdf_register_prefix(prop,'http://enipedia.org/wiki/Property:').

powerplant(X) :- rdf(X,rdf:type,cat:'Powerplant').
primary_fuel_type(P,F) :- rdf(P,prop:'Primary_fuel_type',F).

fuel_type_ppc(F,Num) :-
        aggregate_group(count(P),[F],primary_fuel_type(P,F),Num).


kg_co2pmwh(P,Q) :- rdf(P,prop:'Property:Intensity_kg_CO2_per_MWh_elec',Q).
kg_co2pmwh_nd(P,Q) :- rdf(P,prop:'Property:Intensitynextdecade_kg_CO2_per_MWh_elec',Q).

