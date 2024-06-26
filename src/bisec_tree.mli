
(** Functorial interface. *)

module type Point =
sig
  (** A point. *)
  type t

  (** [dist] must be a distance.
      Be careful with the implementation of your distance
      (dist x x = 0.0, triangular inequality must hold,
      NaN is not a proper distance, etc). *)
  val dist: t -> t -> float
end

(** Heuristics to find good vantage (reference) points.
    One_band is faster (at tree construction time),
    but the resulting tree should be less efficient for queries.
    Two_bands (recommended) is more expensive (at tree construction time)
    but queries should be faster.
    Heuristics inspired by
    "Near Neighbor Search in Large Metric Spaces";
    Sergey Brin; November 20, 1995. *)
type vp_heuristic = One_band | Two_bands

(** To reach a point in the bst, you need to follow a path.
    A path is a list of directions. *)
type direction = Left | Right

(** Part of a point's address in the bst. *)
type step = L of float (* dist to l_vp *)
          | R of float (* dist to r_vp *)

val string_of_addr: step list -> string

module Make: functor (P: Point) ->
sig
  (** A Bisector Tree (BST). *)
  type t

  (** [create k h points] create the BST containing all [points],
      using bucket size [k] and heuristic [h].
      You can provide an optional [progress_callback]
      function to give some feedback to the user
      when indexing many points. If provided, [progress_callback]
      will be called upon progression of the indexing as
      [progress_callback x y].
      [x] is the current number of points that have been indexed
      and [y] is the total number of points to index.
      The default [progress_callback] function does nothing. *)
  val create: ?progress_callback:(int -> int -> unit) ->
    int -> vp_heuristic -> P.t array -> t

  (* (\** [create_sample sample_size nprocs points]
   *     create a bst using only a subset of [sample_size] from [points].
   *     Remaining points are adressed in parallel using [nprocs],
   *     then sequentially added to the previously created bst. *\)
   * val create_sample: int -> int -> P.t array -> t *)

  (** [sample_distances n points] get distances found in [n] pairs
      of randomly-chosen points. The result is sorted. *)
  val sample_distances: int -> P.t array -> float array

  (** [nearest_neighbor q bst] return the distance along with the nearest
      neighbor to query point [q] in [bst]. Warning: there may be several
      points at this distance from [q] in [bst],
      but a single (arbitrary) one is returned.
      If you are not happy with this, use a point type that is
      deduplicated (i.e. a point that holds the info for all points with
      the same coordinates). *)
  val nearest_neighbor: P.t -> t -> P.t * float

  (** [neighbors q tol bst] return all points in [bst] within
      [tol] distance from query point [q].
      I.e. all points returned are within [(d <= tol)]
      distance from [q]. *)
  val neighbors: P.t -> float -> t -> P.t list

  (** [any_neighbor q tol bst] tell if the range query
      [neighbors q tol bst] would return some neighbors,
      but much faster than doing the actual range query. *)
  val any_neighbor: P.t -> float -> t -> bool

  (** [partition q tol bst] is like [neighbors], but a pair
      [(xs, ys)] is returned, such that [(d <= tol)]
      for any [x] and [(d > tol)] for any [y]. *)
  val partition: P.t -> float -> t -> P.t list * P.t list

  (** [to_list bst] return the list of points inside [bst],
      in an unspecified order. *)
  val to_list: t -> P.t list

  (** [length bst] return the number of elements inside [bst].
      I.e. how many points are indexed by this [bst]. *)
  val length: t -> int

  (** [is_empty bst] test if [bst] is empty. *)
  val is_empty: t -> bool

  (** [find q bst] return the first point with distance to [q] = 0.0.
      @raise Not_found if no such element exists.
      Warning: there may be several
      points at this distance from [q] in [bst],
      but a single (arbitrary) one is returned. *)
  val find: P.t -> t -> P.t

  (** [mem q bst] return true if [q] can be found in [bst],
      false otherwise. *)
  val mem: P.t -> t -> bool

  (** [root bst] return the first point found in [bst]
      (either a bucket's vantage point or a node's left vantage point).
      @raise Not_found if [is_empty bst]. *)
  val root: t -> P.t

  (** [check bst] test the invariant of [bst].
      Should always be true.
      If invariant doesn't hold, then this library has a bug
      or your [P.dist] function is not a proper metric. *)
  val check: t -> bool

  (** [inspect bst] extract the vantage points of [bst]
      in an unspecified order. *)
  val inspect: t -> P.t list

  (** alias for [inspect] *)
  val vantage_points: t -> P.t list

  (** [dump max_depth bst] list points and paths to reach
      them in the [bst], going down up to [max_depth]. *)
  val dump: int -> t -> (direction list * P.t list) list

  (** [get_addr q bst] find the address of [q] in [bst]. *)
  val get_addr: P.t -> t -> step list

  (** [add p addr bst] add point [p] to [bst] at given address [addr].
      [addr] _must_ be a valid address in [bst]. Call
      [get_addr p bst] to get a valid address for [p] in [bst]. *)
  val add: P.t -> step list -> t -> t

  (** [to_string bst] create a string representation/summary for [bst] *)
  val to_string: t -> string

  (** [simplify bst] compute the hierarchical simplification of
      the point set contained in [bst]. If [bst] was not constructed
      with [k > 1], it is stupid to call [simplify].
      For example, if you want to reduce the size of your point set by at
      least 10. First, construct a bst with k=10.
      Then, call [simplify] on it.
      The result is a list of points lists.
      You should average each points list in order to get the
      simplified point set. Note that if your points carry a payload,
      during averaging the payload might also need to be weighted or averaged
      in some way (depending on your application).*)
  val simplify: t -> (P.t list) list

end
