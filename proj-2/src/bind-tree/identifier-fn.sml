(* identifier-fn.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Functor for generating unique classes of identifiers.
 *)

functor IdentifierFn () : sig

    type t

    (* define a new variable with the given name *)
    val new : Atom.atom -> t

    (* the source-code name of the identifier *)
    val nameOf : t -> string
    (* a unique string representation of the identifier *)
    val toString : t -> string

    (* comparisons *)
    val compare : t * t -> order
    val same : t * t -> bool

    (* finite sets of identifiers *)
    structure Set : ORD_SET where type Key.ord_key = t
    (* finite maps with identifier keys *)
    structure Map : ORD_MAP where type Key.ord_key = t
    (* hash tables with identifier keys *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = ID of {
        name : string,          (* name of identifier *)
        stamp : Stamp.t         (* unique stamp used to distinguish binding occurrences *)
      }

    fun new name = ID{
            name = Atom.toString name,
            stamp = Stamp.new()
          }

    fun nameOf (ID{name, ...}) = name

    fun toString (ID{name, stamp, ...}) = name ^ Stamp.toString stamp

    fun compare (ID{stamp = id1, ...}, ID{stamp = id2, ...}) = Stamp.compare(id1, id2)

    fun same (ID{stamp = id1, ...}, ID{stamp = id2, ...}) = Stamp.same(id1, id2)

    structure Set = RedBlackSetFn (
      struct
        type ord_key = t
        val compare = compare
      end)
    structure Map = RedBlackMapFn (
      struct
        type ord_key = t
        val compare = compare
      end)
    structure Tbl = HashTableFn (
      struct
        type hash_key = t
        fun hashVal (ID{stamp, ...}) = Stamp.hash stamp
        val sameKey = same
      end)

  end
