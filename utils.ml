let split_string s c =
  let rec impl i =
    if i >= String.length s
    then []
    else
      try
	let e = String.index_from s i c in
	String.sub s i (e - i) :: impl (succ e)
      with Not_found ->
	  [String.sub s i (String.length s - i)]
  in impl 0
