FUNCTION rnd, val, digit

  st = size(val, /type)
  IF st EQ 5 THEN ten = 10d ELSE ten = 10.

  IF n_elements(digit) EQ 0 THEN digit = 0
  return, round(val*ten^digit)*ten^(-digit)

END 
