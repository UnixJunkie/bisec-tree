
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

(** Heuristic to find good vantage (reference) points. *)
type vp_heuristic = One_band | Two_bands

(** To reach a point in the vpt, you need to follow a path.
    A path is a list of directions. *)
type direction = Left | Right

module Make: functor (P: Point) ->
sig
  (** A Bisector Tree (BST). *)
  type t

  (** [create k h points] create the BST containing all [points],
      using bucket size [k] and heuristic [h]. *)
  val create: int -> vp_heuristic -> P.t array -> t

  (** [par_create nprocs k h points] create in parallel
      the BST containing all [points],
      using bucket size [k] and heuristic [h].
      [nprocs] must be a power of two.
      Be warned that scalability is extremely poor. *)
  val par_create: int -> int -> vp_heuristic -> P.t array -> t

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

  (** [dump max_depth bst] list points and paths to reach
      them in the [vpt], going down up to [max_depth]. *)
  val dump: int -> t -> (direction list * P.t list) list
end
