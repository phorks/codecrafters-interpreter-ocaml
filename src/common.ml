let float_to_string num =
  if Float.is_integer num then Printf.sprintf "%.1f" num
  else Printf.sprintf "%.15g" num