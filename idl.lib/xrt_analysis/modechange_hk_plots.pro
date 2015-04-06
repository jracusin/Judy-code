; $Id: modechange_hk_plots.pro 1.0 2002/01/18 17:25:30 burrows $
;
;author	ljc
;date	feb17,1995
;	file to plot XRT modechange housekeeping data.  This routine plots
;	all of the HK channels on the printer.
;
; Rev:
;   07/06/04 by JLR: made quicklook work
;	05/31/04 by JLR: changed time bin input to widget instead of prompt
;	05/28/04 by JLR: made keyword noprint work
;	11/04/03 by DNB: changed scaling for time plot
;	06/13/03 by DNB: put all plots in one file
;	06/12/03 by DNB: fixed error in assignment of plot names to channels
;		for plot pages 3 and 4.  Also re-arranged tube heater
;		plots to match locations on tube (matches Heater_Layout
;		spreadsheet).
;	03/28/03 by DNB: modified output Postscript plot file names.
;	04/12/02 by DNB: major changes to plot order to group plots in
;		more convenient order
;	04/02/02 by DNB: changed horizontal scaling of plots
;	02/23/02 by DNB: updated channel assignments for these plots
;
; $Log: modechange_hk_plots.pro $
;


pro	modechange_hk_plots,xmin,xmax
;	xmin and xmax are output variables

@pass1_common

filenm=''
;
filenm = hk_fits_file

print, 'Reading MC FITS file ...' 
;
data=readfits(filenm,h)
;
;set up variables for 12 channels of interest
time=data(0:0,*)
max_time = max(time)
scale = fix(alog10(max_time))
times = time/(10.0^scale)
units = '(10^'+strtrim(string(scale),2)+' s)'

print,'Opening interactive plot window.  Select min and max time bins'
print,'    for use in modechange plots...'
show_plot = 2
xsize = n_elements(time)
channels = indgen(xsize)

if not keyword_set(quick_look_mode) then begin 
   plotter,show_plot,channels,time,title='Select min and max time bins for modechange',$
      xtitle='Time bin #', ytitle='S/C time (seconds)',/noblock
   
   title='Input file has time bins from 0 - '+strtrim(string(xsize),2)
   
   tmp=dialog_input(prompt=['Time bin min:','Time bin max:'],/integer,nfields=2, $
                    title=title,xsize=400,initial=[1,xsize-1])
   xmin=tmp[0]
   xmax=tmp[1]
endif else begin 
   xmin=1
   xmax=xsize-1
endelse 
   
;print,'Input file has time bins from 0 - ', strtrim(string(xsize),2)
;read,'Enter min and max time bins to use for modechange: ', xmin, xmax
;xmin = max([xmin, 0])
;xmax = min([xmax, xsize-1])

;get header information?

!P.Multi=[0,3,4]
;
print,' '
print,'MODECHANGE_HK_PLOTS: Generating modechange plots...'
print,' '

set_plot,'ps'
device, /landscape

;	plot,***********************
ymin = min(times[xmin:xmax]) 
ymax = max(times[xmin:xmax])
plot,times[xmin:xmax],title='S/C Time',xtitle='Sample #', $
	ytitle='S/C Time '+units, yrange=[ymin,ymax]

; Count rate
ymin = min(data[4,xmin:xmax]) - 0.5
ymax = max(data[4,xmin:xmax]) + 0.5
plot,times[xmin:xmax],data[4,xmin:xmax],$
		title='XRT Count Rate',  xtitle='S/C Time '+units, $
		ytitle='(cps)', yrange=[ymin,ymax], psym=3

i=5
;ymin = min(data[i,*]) - 0.5
;ymax = max(data[i,*]) + 0.5
ymin = 0
ymax = 100
plot,times[xmin:xmax],data[i,xmin:xmax], $
		title='XRT State Flag', xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax]

xlabel = times[xmax] + 0.1*(times[xmax] - times[xmin])
xyouts,/data,xlabel,10.0,'Auto'
xyouts,/data,xlabel,28.0,'Manual'
xyouts,/data,xlabel,60.0,'RED'

i=6
ymin = min(data[i,xmin:xmax]) - 0.5
ymax = max(data[i,xmin:xmax]) + 0.5
xmc = fltarr(2*(xmax-xmin+1)-1)
ymc = xmc
j=0
for k=xmin,xmax do begin
	xmc[j] = times[k]
	ymc[j] = data[i,k]
	if (k lt xmax) then begin
		xmc[j+1] = times[k+1]
		ymc[j+1] = data[i,k]
	endif
	j = j+2
endfor
plot,xmc,ymc, title='XRT Mode Flag', xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax], psym=10

i=7
ymin = min(data[i,xmin:xmax]) - 0.5
ymax = max(data[i,xmin:xmax]) + 0.5
x = fltarr(2*(xmax-xmin+1)-1)
y = x
j=0
for k=xmin,xmax do begin
	x[j] = times[k]
	y[j] = data[i,k]
	if (k lt xmax) then begin
		x[j+1] = times[k+1]
		y[j+1] = data[i,k]
	endif
	j = j+2
endfor
plot,x,y, title='XRT Waveform', xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax], psym=10

acs_flags = data[8,*]
plot,times[xmin:xmax],acs_flags[xmin:xmax], $
		title='ACS Flags', xtitle='S/C Time '+units, $
		ytitle='', yrange=[-0.2,15.2]


flags = ['ACS: IS_SETTLED', 'ACS: IS_IN_10_ARCMIN',$
		'ACS: IN_SAA', 'ACS: Safe Mode Flag']
for i=9,12 do begin
	ymin = -0.1
	ymax = 1.1
	plot,times[xmin:xmax],data[i,xmin:xmax], $
		title=flags[i-9], xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax]
endfor

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 1 of 2) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime



;
;***************************************************************
; Now, do second page
;***************************************************************
;
!P.Multi=[0,1,2]


plot,times[xmin:xmax],acs_flags[xmin:xmax], $
		title='ACS Flags', xtitle='S/C Time '+units, $
		ytitle='', yrange=[-0.2,15.2]

xlabel = times[xmax] - 0.2*(times[xmax] - times[xmin])
xyouts,/data,xlabel,0.0,'1=SETTLED'
xyouts,/data,xlabel,5.0,'2=IN_10_ARCMIN'
xyouts,/data,xlabel,10.0,'4=IN SAA'
xyouts,/data,xlabel,15.0,'8=SAFEHOLD'


i=6
plot,xmc,ymc, title='XRT Mode Flag', xtitle='S/C Time '+units, $
		ytitle='', yrange=[0.0,10.0], psym=10

xlabel = times[xmax] + 0.01*(times[xmax] - times[xmin])
xyouts,/data,xlabel,0.75,'Null'
xyouts,/data,xlabel,1.75,'SIM'
xyouts,/data,xlabel,2.75,'LIM'
xyouts,/data,xlabel,3.75,'PU'
xyouts,/data,xlabel,4.75,'LR'
xyouts,/data,xlabel,5.75,'WT'
xyouts,/data,xlabel,6.75,'PC'
xyouts,/data,xlabel,7.75,'Raw'
xyouts,/data,xlabel,8.75,'Bias'


XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 2 of 2) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime

;close device and print plots
device,/close
if keyword_set(print_plots) then $
   print,'MODECHANGE_HK_PLOTS: Sending plots to the printer'
filename = filein + '.modechange_plots.ps'
spawn,'mv idl.ps ' + filename
if keyword_set(print_plots) then spawn,'lp -d ' + filename
;spawn,'gzip ' + filename


set_plot,'X'

;
!P.Multi=0	; reset to one plot per page

end
