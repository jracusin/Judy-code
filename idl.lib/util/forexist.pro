function forexist,stuff

  n=n_elements(stuff)
  answer=lonarr(n)
  for i=0,n-1 do answer[i]=exist(stuff[i])

  return,answer
end
