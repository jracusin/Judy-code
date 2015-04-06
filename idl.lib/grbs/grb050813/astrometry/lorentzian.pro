
function lorentzian, x, ctr=_ctr,sig=_sig,nrm=_nrm

if keyword_set(_ctr) then ctr=_ctr else ctr=0d
if keyword_set(_sig) then sig=_sig else sig=1d
if keyword_set(_nrm) then nrm=_nrm else nrm=1d

return,abs((nrm/!dpi)*sig)/((x-ctr)^2 + sig^2)
end

