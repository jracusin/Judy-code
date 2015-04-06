function ntostr2,num,sigfig
  
  dig=10d^sigfig-1
  sf=round(num*dig)
  str=ntostr(round(
