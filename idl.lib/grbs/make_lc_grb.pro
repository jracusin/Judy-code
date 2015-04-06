@plot_lc_prelease15
pro make_lc_grb

  files=['',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '',$
         '']


src_ra=
src_dec=
back_ra=
back_dec=
src_rad=[]     ;radius in pixels of the source
back_rad=[]    ;radius in pixels of the background
ct_bin=[]    ;counts per bin
type=[]       ;1=PC, 0=WT
BAT_time=date2met('')
plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0

set_plot,'x'

end
