let float_to_string num =
  if Float.is_integer num then Printf.sprintf "%.1f" num
  else Printf.sprintf "%.15g" num

let float_value_to_string num = Printf.sprintf "%.15g" num
let opt_get_or opt default = match opt with Some v -> v | None -> default
