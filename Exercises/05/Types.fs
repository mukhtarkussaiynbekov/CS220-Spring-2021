namespace CS220

type Shape =
  /// A circle of a radius.
  | Circle of float
  /// A square with a side length.
  | Square of float
  /// A triangle with side lengths.
  | Triangle of float * float * float

/// My own list.
type MyList<'T> =
  | Nil
  | Cons of 'T * MyList<'T>

/// We represent a matrix as a list of lists. Each sublist represents a row of
/// the matrix. For example, [ [1; 2; 3]; [4; 5; 6]; [7; 8; 9] ] indicates a
/// matrix with three columns and rows. The first row of the matrix contains 1,
/// 2, and 3. The second row of the matrix contains 4, 5, and 6. And the third
/// row contains 7, 8, and 9.
type 'a SqMatrix = 'a list list

module SqMatrix =
  let init (rows: 'a list list): 'a SqMatrix =
    match rows with
    | [] -> [ [] ]
    | _ ->
      let len = List.length rows
      let check =
        List.fold (fun b r -> b && (List.length r = len)) true rows
      if check then rows else failwith "Not a square matrix"