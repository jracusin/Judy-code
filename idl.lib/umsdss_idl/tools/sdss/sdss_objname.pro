FUNCTION sdss_objname, run, rerun, camcol, field, id

  rstr = run2string(run)
  rrstr = ntostr(rerun)
  cstr = ntostr(camcol)
  fstr = field2string(field)
  idstr = id2string(id)

  object_name = rstr+'-'+rrstr+'-'+cstr+'-'+fstr+'-'+idstr
  
  return, object_name

END 
