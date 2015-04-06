pro run_sn1987a_sims
  
  cd,'/Volumes/Firewire1/racusin/sn1987a/simulations/'
  
  dir=['0deg_torus/','45deg_torus/','45deg_torus_4spot/']
  models=['ring','ptsrc','bilat','lobes']
  
  for i=2,2 do begin
     cd,dir[i]
     for j=0,3 do begin
        if not exist(models[j]) then spawn,'mkdir '+models[j]
        ring=0 & ptsrc=0 & bilat=0 & lobes=0
        case j of
           0: ring=1
           1: ptsrc=1
           2: bilat=1
           3: lobes=1
        endcase 
        sn1987a_sims,i,ring=ring,ptsrc=ptsrc,bilat=bilat,lobes=lobes
        
        modir=models[j]+'_3'
        spawn,'mv '+models[j]+' '+modir
     endfor 
     cd,'..'
  endfor 
  return
end 
