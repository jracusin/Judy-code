FUNCTION smult,len,st

  stout=''
  IF n_elements(st) EQ 0 THEN st=' '
  FOR i=0,len-1 DO stout=stout+st

return,stout
END 
