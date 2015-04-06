@plot_lc_prelease15
pro grb060526_lc

  files=['sw00211957991xwtw2po_cl.evt','sw00211957991xpcw2po_cl.evt']


src_ra='15:31:18.370'
src_dec='00:17:05.91'
back_ra='15:31:10.198'
back_dec='00:11:05.24'
src_rad=[20,20]     ;radius in pixels of the source
back_rad=[30,60]    ;radius in pixels of the background
ct_bin=[100,50]    ;counts per bin
type=[0,1]       ;1=PC, 0=WT
BAT_time=date2met('2006-146-16:28:30')
plot_lc,type,files,src_ra,src_dec,src_rad,back_ra,back_dec,back_rad,ct_bin,BAT_time,0,1.,1,0

set_plot,'x'

end
