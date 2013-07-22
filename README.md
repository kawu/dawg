Directed acyclic word graphs
============================

The library implements *directed acyclic word graphs* (DAWGs) internally
represented as *minimal acyclic deterministic finite-state automata*.

The `Data.DAWG.Dynamic` module provides fast insert and delete operations
which can be used to build the automaton on-the-fly.  The automaton from
the `Data.DAWG.Static` module has lower memory footprint and provides
static hashing functionality.
