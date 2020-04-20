/** domain Domain describes set of facts.

    Domains are the building blocks for reasoning about the world and
    describing the relations between different domains.

    A namespaces are unique and can have one or more domains.
    A domain can belong to only one namespace.

    Relationship between two facts can be defined between:
    - two facts in same domain
    - two facts across domain but same namespace.
    - two facts across namespaces.
*/

:- module(domain,
          namespace_domain_key/3,
          namespace_domain_key_value/4,
          query_constraints/4,
          explain/5).


/**
  * namespace_key(++Namespace:atom, ?domain:atom, ?key:atom) is nondet.
  * 
  * List all keys across domains and namespaces.
  **/


/**
  * namespace_domain_key_value((++Namespace:atom, ?domain:atom, ?key:atom, ?value:term) is nondet.)
  *
  * List/test values for a given key in a domain.
  **/

