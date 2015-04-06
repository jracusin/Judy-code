function parinfo_struct,n

parinfo = replicate({value:0.D, $
                     fixed:0, $
                     limited:[0,0], $
                     limits:[0.D,0], $
                     parname:'',$
                     step:0d,$
                     relstep:0d,$
                     mpside:0,$
                     mpminstep:0d,$
                     mpmaxstep:0d,$
                     tied:'' }, n)

return,parinfo
end 
