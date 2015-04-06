;+
; NAME:
;	SUBPIXRES.PRO
;
; PURPOSE:
;	Improvement of the spatial resolution of CHANDRA/ACIS data.	
;
; USAGE:
;	subpixres, 'input.fits', 'output.fits' [, ccdidset=ccdidset]
;
; EXPLANATION:
;      Applying this IDL procedure improves the spatial resolution by
;      10-20% without losing statistics. This procedure is especially 
;      useful for resolving small scale structure on the order of 
;      several pixels. The details and the basic idea is described in 
;      Tsunemi et al. (2001, ApJ, 554, 494), and the revised method 
;      (this program) is described in Mori et al. (2001, in ASP
;      Conf. Ser. 251, New Century of X-Ray Astronomy). See also
;      http://vtec.ess.sci.osaka-u.ac.jp/~mori/chandra/subpixel_resolution.html
;
; DESCRIPTION:
;	1) This IDL procedure is for only CHANDRA/ACIS data.
;	2) Since this IDL procedure just modifies the sky x, y columns 
;	   of the split pixel events (ASCA grade 2346), there is no effect
;	   on the further analysis like spectroscopic and lightcurve studies.
;          Further improvement on the image can be done using a
;          maximum-likelihood deconvolution method. 
;
; MODIFICATION HISTORY:
;       Initial release 	    05/23/01, K. Mori
;					      mori@crab.astro.psu.edu
;       Modified to work for an event file which have a multiple CCDID events 
;		          	    06/03/01, K. Mori
;       Modified to work on CCD6
;		          	    01/28/03, K. Mori
;	Replaced for loop with case statements to improve speed
;                                   08/14/03, M. F. Corcoran
;	Modified a bug which was made in previous release
;                                   10/18/03, K. Mori
;
; CAUTION
;	This should NOT be applied to CCDID 4589.
;-
;
PRO subpixres, infile, outfile, ccdidset=ccdidset


;; Check input
if N_params() NE 2 then begin         
  print,' 				    '
  print, "subpixres, 'input.fits', 'output.fits'[, ccdidset=ccdidset]      "
  print,' 				    '       
  retall
endif


;; Read the primary and extension headers
pheader  = headfits( infile )
e1header = headfits( infile, EXTEN=1 )
e2header = headfits( infile, EXTEN=2 )


;; Read input file
evt = mrdfits( infile, 1 )


;; Extract the key parameters
rollangle   = fxpar( e1header, 'ROLL_NOM' )
nevent      = fxpar( e1header, 'NAXIS2' )


;; Print the number of the events
print, '			'
print, '	#events is',nevent
print, '			'


;; Calculate the offset
theta = (rollangle + 45.) / 180. * double(!PI)
offset1 = 1 / sqrt(2) * sin( theta ) 
offset2 = 1 / sqrt(2) * cos( theta ) 

phi = rollangle / 180. * double(!PI)
offset3 = 0.5 * sin( phi ) 
offset4 = 0.5 * cos( phi ) 

if n_elements(ccdidset) eq 0 then ccdidset=[0,1,2,3,6,7]
for k=0, n_elements(ccdidset)-1 do begin
  ccdid=ccdidset(k)
  print,'processing events of CCDID',ccdid
  case 1 of 		
   ((ccdid eq 1) or (ccdid eq 3)): begin
       i=where(evt.ccd_id eq ccdid)
       case 1 of 
           i(0) lt 0: print,'  Event List does not contain CCDID',ccdid
           else: begin
               j=where((evt(i).fltgrade eq 11) or (evt(i).fltgrade eq 10)) 
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset1
                   evt(i(j)).y=evt(i(j)).y+offset2
               endif
               j=where((evt(i).fltgrade eq 22) or (evt(i).fltgrade eq 18))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset2
                   evt(i(j)).y=evt(i(j)).y-offset1
               endif
               j=where((evt(i).fltgrade eq 104) or (evt(i).fltgrade eq 72))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset2
                   evt(i(j)).y=evt(i(j)).y+offset1
               endif
               j=where((evt(i).fltgrade eq 208) or (evt(i).fltgrade eq 80))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset1
                   evt(i(j)).y=evt(i(j)).y-offset2
               endif
               j=where(evt(i).fltgrade eq 2)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset4
                   evt(i(j)).y=evt(i(j)).y-offset3
               endif
               j=where(evt(i).fltgrade eq 8)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset3
                   evt(i(j)).y=evt(i(j)).y+offset4
               endif
               j=where(evt(i).fltgrade eq 16)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset3
                   evt(i(j)).y=evt(i(j)).y-offset4
               endif
               j=where(evt(i).fltgrade eq 64)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset4
                   evt(i(j)).y=evt(i(j)).y+offset3
               endif
           end
       endcase
   end
   ((ccdid eq 0) or (ccdid eq 2)): begin
       i=where(evt.ccd_id eq ccdid)
       case 1 of
           i(0) lt 0: print,'  Event List does not contain CCDID',ccdid
           else: begin		
               j=where((evt(i).fltgrade eq 208) or (evt(i).fltgrade eq 80)) 
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset1
                   evt(i(j)).y=evt(i(j)).y+offset2
               endif
               j=where((evt(i).fltgrade eq 104) or (evt(i).fltgrade eq 72))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset2
                   evt(i(j)).y=evt(i(j)).y-offset1
               endif
               j=where((evt(i).fltgrade eq 22) or (evt(i).fltgrade eq 18))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset2
                   evt(i(j)).y=evt(i(j)).y+offset1
               endif
               j=where((evt(i).fltgrade eq 11) or (evt(i).fltgrade eq 10))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset1
                   evt(i(j)).y=evt(i(j)).y-offset2
               endif
               j=where(evt(i).fltgrade eq 2)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset4
                   evt(i(j)).y=evt(i(j)).y+offset3
               endif
               j=where(evt(i).fltgrade eq 8)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset3
                   evt(i(j)).y=evt(i(j)).y-offset4
               endif
               j=where(evt(i).fltgrade eq 16)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset3
                   evt(i(j)).y=evt(i(j)).y+offset4
               endif
               j=where(evt(i).fltgrade eq 64)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset4
                   evt(i(j)).y=evt(i(j)).y-offset3
               endif
           end
       endcase
   end
   ((ccdid eq 6) or (ccdid eq 7)): begin
       i=where(evt.ccd_id eq ccdid)
       case 1 of
           i(0) lt 0: print,'  Event List does not contain CCDID',ccdid
           else: begin		
               j=where((evt(i).fltgrade eq 11) or (evt(i).fltgrade eq 10)) 
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset2
                   evt(i(j)).y=evt(i(j)).y+offset1
               endif
               j=where((evt(i).fltgrade eq 22) or (evt(i).fltgrade eq 18))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset1
                   evt(i(j)).y=evt(i(j)).y+offset2
               endif
               j=where((evt(i).fltgrade eq 104) or (evt(i).fltgrade eq 72))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset1
                   evt(i(j)).y=evt(i(j)).y-offset2
               endif
               j=where((evt(i).fltgrade eq 208) or (evt(i).fltgrade eq 80))
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset2
                   evt(i(j)).y=evt(i(j)).y-offset1
               endif
               j=where(evt(i).fltgrade eq 2)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset3
                   evt(i(j)).y=evt(i(j)).y+offset4
               endif
               j=where(evt(i).fltgrade eq 8)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset4
                   evt(i(j)).y=evt(i(j)).y+offset3
               endif
               j=where(evt(i).fltgrade eq 16)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x+offset4
                   evt(i(j)).y=evt(i(j)).y-offset3
               endif
               j=where(evt(i).fltgrade eq 64)
               if j(0) ge 0 then begin
                   evt(i(j)).x=evt(i(j)).x-offset3
                   evt(i(j)).y=evt(i(j)).y-offset4
               endif
           end
       endcase
   end
   else: print,'CCD specified was : ',ccdid," This routine only works for ccd's 0,1,2,3,6,7"
   endcase
  endfor


;; Write output file
writefits, outfile, 0, pheader
mwrfits, evt, outfile, e1header
fits_info,infile,n_ext=n_ext,/silent
for i=2,n_ext do begin
   st=mrdfits(infile,i,h)
   mwrfits,st,outfile,h
   endfor
return
end
