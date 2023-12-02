let next_line channel =
  try
    let line = input_line channel in
    Some line
  with | End_of_file ->
    None
