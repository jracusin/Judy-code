function sunpos_today

  jdnow=systime(/julian)
  sunpos,jdnow,ra,dec

return,[ra,dec]
end 
