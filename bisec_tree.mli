
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

(** We use a heuristic to find good vantage (reference) points;
    Good n => we will try to find a double normal using
    n optimization steps at most. Optimization stops as soon as a
    double normal is found.
    Currently, we only support n = 1 or 2. *)
type quality = Good of int

module type Config = sig
  (** Bucket size. If there are n <= k points left, we put them
      in the same bucket. Else, we continue constructing
      the tree recursively.
      This should save storage space and accelerate queries.
      The best value for k is probably dataset and application dependent.
      k = 0 => the tree is not bucketized.
      k = 50 might be a good default value. *)
  val k: int

  (** Vantage point finding heuristic quality. *)
  val q: quality
end

module Make: functor (P: Point) (C: Config) ->
sig
  (** A Bisector Tree (BST). *)
  type t

  (** [create points] create the BST containing all points. *)
  val create: P.t array -> t

  (** [sample_distances n points] sample distances between points
      in a random sample of size [n]. *)
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
end
