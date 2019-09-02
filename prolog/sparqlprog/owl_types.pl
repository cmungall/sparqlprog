:- module(owl_types,
          [
           ]).

:- use_module(library(typedef)).

% ====================
% RDF TYPES
% ====================

:- type iri_ref ---> curie ; atomic_iri.

:- type atomic_iri == atom.
:- type curie ---> iri_prefix : iri_local_part.
:- type iri_prefix == atom.
:- type iri_local_part == atom.

:- type blank_node == atom.
:- type rdf_resource ---> iri_ref ; blank_node.
:- type rdf_resource_or_literal ---> rdf_resource ; rdf_literal.
:- type rdf_literal ---> rdf_plain_literal ; rdf_lang_literal ; rdf_typed_literal.
:- type rdf_plain_literal == string.
:- type rdf_lang_literal ---> '@'(string,atom).   % todo - consider enum
:- type rdf_typed_literal ---> '^^'(string,xsd_type).
:- type xsd_type ---> iri_ref.

:- type rdf_triple ---> rdf(rdf_resource, iri_ref, rdf_resource_or_literal).
:- type rdf_quad ---> rdf(rdf_resource, iri_ref, rdf_resource_or_literal, rdf_resource).
:- type rdf_fact ---> rdf_triple ; rdf_quad.

% ====================
% OWL TYPES
% ====================

:- type owl_class_expression ---> owl_restriction ; owl_class.
:- type owl_class ---> iri_ref.
:- type owl_restriction ---> owl_some_values_from ; owl_all_values_from.
:- type owl_some_values_from ---> some(owl_property_expression, owl_class_expression).
:- type owl_all_values_from ---> some(owl_property_expression, owl_class_expression).

:- type owl_property_expression ---> owl_restriction ; owl_property.
:- type owl_property ---> iri_ref.


