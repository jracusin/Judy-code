pro grb051221b_lc

files=['sw00173904991xwtw2po_cl.evt','sw00173904991xpcw2po_cl.evt'];,'file2pc','file3pc']

src_ra=312.396337618
src_dec=53.0367286513
back_ra=312.3289;312.54664
back_dec=52.993487;53.044503
src_rad=[20,20];,20,20,10,10,10,10]     ;radius in pixels of the source
back_rad=[60,60];,20,20,20,20,20,20]    ;radius in pixels of the background
ct_bin=[10,10];,20,20,10,10,10,10]    ;counts per bin
type=[0,1];,0,1,1,1,1,1]       ;1=PC, 0=WT
BAT_time=date2met('2005-355-20:03:20')
plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0
end
