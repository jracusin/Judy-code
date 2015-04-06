;=============================================================================
; $Id: match_xy.pro 3510 2009-08-07 17:58:00Z psb6 $
; Catalog Matching Program 
; Patrick Broos, November 1993, December 2005

; The input structure array cat_slave must contain the following tags/fields:
;   ID: (optional) a unique long integer identifier
;   X,Y: independent position coordinates.  These do not have to be Cartesian.
;   X_ERR, Y_ERR: standard deviations for X & Y
; A known offset error in cat_slave can be specified via XSHIFT_SLAVE,YSHIFT_SLAVE.
;
;
; The input/output data structure match_state holds three types of information. 
; (1) a master catalog consisting of at least these fields:
;   ID:  long integer identifier
;   X,Y: independent position coordinates.  These do not have to be Cartesian.
;   X_ERR, Y_ERR: standard deviations for X & Y
;
; The match_state is initialized and a master catalog is established via this call:
;       match_xy, match_state, cat_master, cat_name, /INIT
;
; (2) a copy of all the catalogs we've processed.  Note that any XSHIFT_SLAVE,YSHIFT_SLAVE values that are passed in are ADDED to the X & Y columns of these catalogs.
;
; (3) match information consisting of a Primary Match (PM) structure array and a Secondary Match (SM) structure array.  
;The PM contains an entry for each source in (1):
;   IDm, IDs: a pair of matching ID tags from the master & slave catalogs
;   deltaX, deltaY: offsets between matching entries (master - slave)
;   rank: see discussion below
;   type: {0: isolated source, 1: successful Primary Match, 2: Secondary match, 3: failed Primary match, }
;
; Every entry "a1" in catalog 1 has a unique Primary Match: the entry "a2" in catalog 2 that has the highest match rank.  The Primary Match <a1,a2> is "unsuccessful" if there exists another Primary Match involving either a1 or a2 ,<a1,b2> or <b2,a2>, which has a higher rank.  In other words, Primary Matches are geedy -- once <a1,a2> has been accepted as a successful match then neither a1 nor a2 can participate in another successful primary match.  We're trying to build a self-consistent hypothesis for the correspondence between the two catalogs.  Thus, if we hypothesize that a1 & a2 are the same object then it makes no sense to also hypothesize that a1 & b2 are the same object, even if a1 is the closest object to b2.  Rather it makes more sense to hypothesize that b2 has no match.;
;
; The SM contains a variable number of entries recording secondary matches between (1) and the slave catalog:


; The significance_threshold parameter specifies the formal "significance" required for a match, i.e. our decision criterion is to REJECT match candidates that fall in a region of parameter space containing an area (probability) of (1-significance_threshold parameter).
; The largest significance_threshold that can be handled seems to be 
;    significance_threshold=(1D - 1D-16)

; If the output parameter UNION_CAT is supplied then the master catalog is extended to include anything in cat_slave that is not matched.  For matched sources the position is taken from cat_slave if its more accurate.

; Some references on the catalog matching problem:
; http://sundog.stsci.edu/first/APM/HTML/node5.html


;=============================================================================
PRO match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type
isolated_type           = 0
failed_primary_type     = 1
successful_primary_type = 2
secondary_type          = 3
init_type               = 4
return
end


PRO match_xy, match_state, cat_slave_p, cat_name, significance_threshold, $
	            XSHIFT_SLAVE=xshift_slave, YSHIFT_SLAVE=yshift_slave, $
              INIT=init, QUIET=quiet, $
              UNION_CAT=union_cat, UNION_REG=union_reg, USE_SLAVE_AS_TEMPLATE=use_slave_as_template, $
              PREFER_SLAVE=prefer_slave, PREFER_MASTER=prefer_master, $
              NGOOD=Ngood, NSECONDARY=Nsecondary, _EXTRA=extra_params
            
COMMON match_xy, idex, idey, idx, idy, idxe, idye, idsx, idsy, idsxy, idr1, idr2

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

if (n_elements(xshift_slave) EQ 1) then xshift_slave=xshift_slave[0] else xshift_slave=0.
if (n_elements(yshift_slave) EQ 1) then yshift_slave=yshift_slave[0] else yshift_slave=0.

Nslave     = n_elements(cat_slave_p)

f_nan    = !VALUES.F_NAN

;; Make sure the catalog has an ID column.
if (total(strmatch(tag_names(cat_slave_p),'ID')) GT 0) then begin
  ; Check the existing ID column for problems.
  id_type = size(cat_slave_p.ID, /TNAME) 
  if (id_type NE 'INT') and (id_type NE 'LONG') then begin
    print, 'ERROR (match_xy): column "ID" in catalog '+cat_name+' must be INT or LONG type!'
    retall
  endif
  
  if (n_elements(uniq(cat_slave_p.ID,sort(cat_slave_p.ID))) NE Nslave) then begin
    print, 'ERROR (match_xy): column "ID" in catalog '+cat_name+' must be unique!'
    retall
  endif
    
  cat_slave = cat_slave_p
  
endif else begin
  ; Add an ID column to the catalog.
  cat_slave    = replicate(create_struct('ID', 0L, cat_slave_p[0]), Nslave)
  struct_assign, cat_slave_p, cat_slave
  cat_slave.ID = 1+lindgen(Nslave)
endelse

label_available = (total(strmatch(tag_names(cat_slave),'LABEL')) EQ 1)
LABEL = label_available ? cat_slave.LABEL : cat_slave.ID

;; Make sure all the positions and errors are good numbers.
ind = where((finite(cat_slave.X) AND finite(cat_slave.Y) AND finite(cat_slave.X_ERR) AND finite(cat_slave.Y_ERR)) EQ 0, count)
if (count GT 0) then begin
  print, 'ERROR (match_xy): the following sources in catalog '+cat_name+' have some bad X,Y,X_ERR,Y_ERR values:'
  forprint, LABEL, cat_slave.X, cat_slave.Y, cat_slave.X_ERR, cat_slave.Y_ERR, SUBSET=ind
  print, 'Match was aborted!'
  retall
endif                                                                 

ind = where(((cat_slave.X_ERR GT 0) AND (cat_slave.Y_ERR) GT 0) EQ 0, count)
if (count GT 0) then begin
  print, 'WARNING (match_xy): the following sources in catalog '+cat_name+' have non-positive X_ERR,Y_ERR values:'
  forprint, LABEL, cat_slave.X, cat_slave.Y, cat_slave.X_ERR, cat_slave.Y_ERR, SUBSET=ind
  print, 'You may have read your catalog incorrectly, or your catalog may be missing some error information.'
endif

if ~keyword_set(quiet) && (Nslave GT 1) then begin
  dataset_1d, idex, cat_slave.X_ERR, DATASET=cat_name, XTIT='X_ERR [pixels]'
  dataset_1d, idey, cat_slave.Y_ERR, DATASET=cat_name, XTIT='Y_ERR [pixels]'
endif



; Apply offsets to a COPY of the slave catalog so we don't change it in the caller.
cat_slave.X = cat_slave.X + xshift_slave
cat_slave.Y = cat_slave.Y + yshift_slave

distance_shifted = sqrt(xshift_slave^2 + yshift_slave^2)
if (distance_shifted GT 0) then begin
  print, cat_name, distance_shifted, F='(%"Moving catalog %s a distance of %0.2f pixels.")'
endif

if keyword_set(init) then begin
  ;; Create a brand new match_state and copy the positions from the supplied catalog.
  Ncat = 1

  match_state = { ID:cat_slave.ID, LABEL:LABEL, X:cat_slave.X, Y:cat_slave.Y, X_ERR:cat_slave.X_ERR, Y_ERR:cat_slave.Y_ERR, cat_names:strarr(Ncat), sig_thresholds:dblarr(Ncat), catalogs:ptrarr(Ncat), match_primary:ptrarr(Ncat), match_secondary:ptrarr(Ncat) } 
endif else begin
  ; Disallow a slave catalog named the same as the master.
  if (cat_name EQ match_state.cat_names[0]) then begin
    print, 'ERROR (match_xy): the name of this slave catalog, '+cat_name+', conflicts with the name of the master catalog.'
    retall
  endif
endelse

Nmaster    = n_elements(match_state.X)
Nsecondary = 0L


dum = {PMATCH, IDm:0L, IDs:0L, deltaX:f_nan, deltaY:f_nan, rank:f_nan, type:isolated_type}
dum = {SMATCH, IDm:0L, IDs:0L, deltaX:f_nan, deltaY:f_nan, rank:f_nan }
match_primary   = replicate({PMATCH},Nmaster)
match_secondary = replicate({SMATCH},Nmaster)


if keyword_set(init) then begin
  ; Match slave_cat to itself with type set to init_type, then return.
  match_primary.IDm  = match_state.ID
  match_primary.IDs  = match_state.ID
  match_primary.type = init_type
  
  ptr_free, match_state.catalogs[0], match_state.match_primary[0]

  match_state.sig_thresholds[0]= 0
  match_state.cat_names    [0] = cat_name
  match_state.catalogs     [0] = ptr_new(cat_slave)
  match_state.match_primary[0] = ptr_new(match_primary)

  return
endif

;; Pull out some vectors from match_state and cat_slave structures to speed up execution.
match_state_ID = match_state.ID
cat_slave_ID   = cat_slave.ID

Xs     = cat_slave.X
Ys     = cat_slave.Y
sig_Xs = cat_slave.X_ERR
sig_Ys = cat_slave.Y_ERR
var_Xs = sig_Xs^2
var_Ys = sig_Ys^2


; Initialize match_primary to be isolated sources from match_state.
match_primary.IDm  = match_state_ID
match_primary.type = isolated_type




;; Let the "null hypothesis" H0 be that these two position measurements are from a single source located at an unknown true position (Xtrue,Ytrue).  We assume we have 4 independent random variables, two X positions and two Y positions.  We need to choose a scheme for rejecting H0, i.e. for declaring that two catalog entries just do not plausibly match.  This means dividing the joint distribution into "acceptance" and "rejection" regions.

;; IF WE KNEW (Xtrue,Ytrue) then we could compute a confidence region on the 4-D jointly Gaussian distribution where the 4 axes are independent.  A simple region would be a "rectangle" in 4-D with an area equal to the significance desired; we would require that EACH measurement fall within a simple 1-D confidence interval for that axis. 

;; BUT, we DO NOT know (Xtrue, Ytrue).  Estimating Xtrue,Ytrue from the data is not appropriate since it will always be "near" the two observed positions.   Thus the more straighforward approach is to examine the offsets between the two observations directly.  Consider two new random variables representing the offset between the observed X & Y positions:
;; deltaX = abs(Xs - Xm)
;; deltaY = abs(Ys - Ym)
;; These are distributed as zero-mean Normals with variance equal to the sum of the two variances. The joint distributuion of these is a 2-D Gaussian.  An natural boundary of the confidence region would be a contour of the distribution, i.e. an ellipse.  All we would need to do is determine the value of the distribution at the appropriate contour, and then if the likelihood of the data exceeds that threshold then the match is accepted.   Unfortunately I cannot find publication of a clear derivation of the relationship between such a "confidence ellipse" (even in 2D) and the integrated probability enclosed.
;;  
;; Thus instead I'm going to use a confidence region that's a rectangle in this 2-D space.  In other words I'll require that each of the deltaX,deltaY offsets fall within a simple 1-D confidence interval for that axis.  If each 1-D confidence interval has a significance (area under the marginal 1-D PDF) of S then the significance of the corresponding confidence region in 2D (area under the 2-D PDF) will be S^2.  So for example to achieve a significance of 0.99 in 2D we require a significance of sqrt(0.99) = 0.99^(0.5) = 0.994987 for each 1-D interval.
;;
;; It is convenient to scale our deltaX and deltaY values to that they are in units of standard deviations.  Then, our confidence region becomes a square on the standard 2-D normal distribution, equivalent to a symmetric interval on the standard 1-D normal distribution.  Thus, our computation is distilled down to finding an integration limit "z_acceptable" such that the integral of the standard 1-D normal distribution over [-z_acceptable:+z_acceptable] is equal to sqrt(significance_threshold).
;;
;; The area in the two leftover tails of the distribution is simply (1-sqrt(significance_threshold)), and thus the area of the upper tail is simply (1-sqrt(significance_threshold)) / 2.  That expression is relevant because the IDL function gauss_cvf() will return the left-hand boundary of the upper tail of the standard Gaussian that has a specified area.  Thus, we can compute z_acceptable via:
;;;
;;;     z_acceptable = gauss_cvf((1 - sqrt(significance_threshold)) / 2.0 )


;; 
;;
;; BEWARE.  One may be tempted to use an alternate approach, namely to compute for EACH 4-tuple the integral of the 4D PDF within the rectangular region (in 4D) bounded by that 4-tuple, and then compare that area to a significance threshold (e.g. 0.99).  In this approach the rectangles can have wild shapes, depending on the data values, as opposed to the "compact" shape of the fixed "acceptance region" used in the first approach. Maybe there's some correspondence between the two approaches, but the second one seems wrong to me.

significance_threshold = double(significance_threshold)

z_acceptable = gauss_cvf((1 - sqrt(significance_threshold)) / 2.0 )
help, z_acceptable

report_frac = [.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0]
t0 = systime(1)
;; Loop through primary catalog computing rank of every match that's judged to be plausible.
for ii=0L,Nmaster-1 do begin
;  if ((ii MOD 1000) EQ 0) then print, ii, ii / (systime(1)-t0)

  if (float(ii)/Nmaster GT report_frac[0]) then begin
    if ~keyword_set(quiet) then print, report_frac[0]*100, F='(%"Processed %d%% of master sources.")'
    report_frac = shift(report_frac,-1)
  endif
  
  IDm    = match_state.ID[ii]
  Xm     = match_state.X[ii]
  Ym     = match_state.Y[ii]
  
  sig_Xm = match_state.X_ERR[ii]
  sig_Ym = match_state.Y_ERR[ii]
  
  var_Xm = sig_Xm^2
  var_Ym = sig_Ym^2
  
  ; Compute best estimate of true position if H0 is true.  See Bevington section 4.1
  ; This is not actually used in the matching algorithm.
  ;Xtrue = (Xm*var_Xs + Xs*var_Xm) / (var_Xs + var_Xm)
  ;Ytrue = (Ym*var_Ys + Ys*var_Ym) / (var_Ys + var_Ym)

  ; Find the data points (source pairs) that fall in the "acceptance region" we've established.
  ; To get acceptable performance with large catalogs we're going to need to code this to minimize calculations ...
  
  ; Find the subset of slave sources that are acceptibly close to this master on the X-axis by
  ; normalizing the deltaX random variable, representing offsets between the two measurements, by the appropriate
  ; "sigma" and then comparing to a threshold.
  Zx = abs(Xm - Xs)/sqrt(var_Xm + var_Xs)
  
  one_axis_good_index = where(Zx LT z_acceptable, Ngood)
  if (Ngood EQ 0) then continue
  
  ; Similarly, normalize and threshold deltaY for the slave sources with acceptable X offsets..
  Zx = Zx[one_axis_good_index]
  Zy = abs(Ym - Ys[one_axis_good_index])/sqrt(var_Ym + var_Ys[one_axis_good_index])

  ind                 = where(Zy LT z_acceptable, Ngood)
  if (Ngood EQ 0) then continue

  Zx = Zx[ind]
  Zy = Zy[ind]

  two_axis_good_index = one_axis_good_index[ind]

  ; We now have a set of matches that are all judged to be acceptable, i.e. to not be too extremely improbably.  However later we are going to have to rank these proposed matches in order to choose which to accept.  I think the most obvious metric to use for this ranking is the "likelihood", i.e. the height of the 4D jointly Gaussian PDF evaluated at the data 4-tuple.  Consider for a moment how this ranking method will behave when we have a pair of sources exactly coincident, i.e. the 4-tuple of data lies at the peak of the 4D PDF.  If all the measurements are very accurate (small sigma) then the PDF will be narrow and the peak will be high, producing a high rank.  On the other hand if the measurements are very inaccurate the PDF will be broad and the peak will be low, producing a low rank.  That seems to match ones intuition for a well-behaved ranking scheme.
  
  ; In the code below the vectors named rank, deltaX, deltaY, and IDs have Ngood elements.
  IDs = cat_slave[two_axis_good_index].ID
  
  ; It's convenient to use the log of the likelihood as the ranking metric.
  rank = -(Zx^2 + Zy^2)
  
  ; Compute position offsets which are reported in match structures (for use in estimating coordinate system transformations).
  ; We compute master - slave to get the same sign on these offsets as is neede for XSHIFT/YSHIFT inputs.
  deltaX = match_state.X[ii] - Xs[two_axis_good_index] 
  deltaY = match_state.Y[ii] - Ys[two_axis_good_index] 
  
  if (Ngood EQ 1) then begin
    ; There's only one possible match, the Primary Match.
    primary_ind = 0
  endif else begin
    ; The best one is the Primary Match.
    dum  = max(rank, primary_ind)
    
    ; The rest are secondary matches.
    flag = replicate(1B, Ngood)
    flag[primary_ind] = 0
    secondary_ind = where(flag)
    
    ; Allocate more space in all_records when necessary.
    NtoAdd = Ngood-1
    if ((Nsecondary+NtoAdd) GT n_elements(match_secondary)) then begin
      print, f='(%"extending match_secondary array...")'

      match_secondary = [match_secondary,replicate({SMATCH}, NtoAdd > 4000)]
    endif

    ; Save the secondary matches.    
    new_records = replicate({SMATCH}, NtoAdd)
    new_records.IDm        = IDm
    new_records.IDs        = IDs      [secondary_ind]
    new_records.deltaX     = deltaX   [secondary_ind]
    new_records.deltaY     = deltaY   [secondary_ind]
    new_records.rank       = rank     [secondary_ind]
    
    match_secondary[Nsecondary] = new_records
                    Nsecondary +=  NtoAdd
  endelse
  
  ; Save the Primary Match, marking it as "successful" for now.
  match_primary[ii] = {PMATCH, IDm, IDs[primary_ind], deltaX[primary_ind], deltaY[primary_ind], rank[primary_ind], type:successful_primary_type}
endfor ;ii


;; Consider success of Primary Matches in order of rank.
; The strategy here is that when a source from list 1, P1, is judged to match
; a source from list 2, P2, then all other potential matches involving P1 and P2
; are DISABLED.  
; In other words a given source can NOT participate in multiple matches.
; We do however record "secondary matches" so one can tell if a reported match is
; unique/ambiguous.
;
; The potential downside of this approach is that a spurious match involving 
; source P1 will remove P1 from consideration in subsequent matches, possibly  
; preventing its true match from being recorded.  We minimize this problem by 
; processing matches sorted by their match distance, on the assumption that
; close matches are more reliable than larger ones.

sort_ind = reverse(sort(match_primary.rank))

for ii=0L,Nmaster-1 do begin
  match = match_primary[sort_ind[ii]]
  
  ; If this match is successful then all others involving this slave entry are NOT successful.
  if (match.type EQ successful_primary_type) then begin
  
    fail = where((match_primary.type EQ successful_primary_type) AND $
                 (match_primary.IDs  EQ match.IDs), count)
                 
    if (count EQ 0) then message, 'Bug found!'
    
    if (count GT 1) then begin
      ; Remove the current match from the fail list.
      fail = fail[ where(fail NE sort_ind[ii]) ]
      match_primary[fail].type = failed_primary_type

      print, match.IDm, match.IDs, F='(%"Successful primary match (%d,%d) forced the failure of these primary matches:")'
      forprint, match_primary[fail].IDm, match_primary[fail].IDs
    endif

  endif ;(successful_primary_type)
endfor ;ii


;; Store everything in match_state.  Overwrite any existing entry for this catalog.
ind = where(match_state.cat_names EQ cat_name, count)
if (count EQ 0) then begin
  ; Look for an empty spot.
  ind = where(match_state.cat_names EQ '', count)
endif

if (count EQ 0) then begin
  ; Need to enlarge match_state.
  Ncat = 1+n_elements(match_state.cat_names)
  ind  = Ncat-1
  temp_cat = match_state
  fa = fltarr(Nmaster)

  match_state = { ID:lonarr(Nmaster), LABEL:strarr(Nmaster), X:fa, Y:fa, X_ERR:fa, Y_ERR:fa, cat_names:strarr(Ncat), sig_thresholds:dblarr(Ncat), catalogs:ptrarr(Ncat), match_primary:ptrarr(Ncat), match_secondary:ptrarr(Ncat) } 
  
  struct_assign, temp_cat, match_state
endif

ptr_free, match_state.catalogs[ind], match_state.match_primary[ind], match_state.match_secondary[ind]
match_state.sig_thresholds[ind]= significance_threshold
match_state.cat_names    [ind] = cat_name
match_state.catalogs     [ind] = ptr_new(cat_slave)
match_state.match_primary[ind] = ptr_new(match_primary)
if (Nsecondary GT 0) then $
  match_state.match_secondary[ind] = ptr_new(match_secondary[0:Nsecondary-1])


;; Form the union of the master cat and cat_slave if desired.
if arg_present(union_cat) then begin
  cat_master = *match_state.catalogs[0]
  
  ; Use the specified catalog as the template for the union.
  ; Create a union_cat entry with all fields nulled.
  null_union_record = keyword_set(use_slave_as_template) ? cat_slave[0] : cat_master[0]
  struct_assign, {zzz:0}, null_union_record

  union_cat = replicate(null_union_record, Nmaster)

  ; Start with the master catalog.
  struct_assign, cat_master, union_cat
  
  label_available = (total(strmatch(tag_names(cat_master),'LABEL')) EQ 1) && (total(strmatch(tag_names(cat_slave),'LABEL')) EQ 1)
  
  slave_unmatched_flag = replicate(1B, Nslave)
  
  ; Examine the matches.
  good = where(match_primary.type EQ successful_primary_type, Ngood)
  if (Ngood GT 0) then begin
    for ii=0L,Ngood-1 do begin
      match = match_primary[good[ii]]
      
      ind_m  = where(match_state_ID EQ match.IDm)
      ind_s  = where(  cat_slave_ID EQ match.IDs)
      master = union_cat[ind_m]
      slave  = cat_slave[ind_s]

      if (ind_m[0] NE good[ii]) then message, 'Order is suspect!'
      
      ; Mark the slave entry as matched.
      slave_unmatched_flag[ind_s] = 0
      
      ; Choose which position is to be carried forward.

      ; Create a union_cat entry with all fields nulled.
      this_union_record = null_union_record
      
      if keyword_set(prefer_master) then begin
      endif else if keyword_set(prefer_slave) then begin
        ; Use the slave position.
        if label_available then print, master.LABEL, slave.LABEL, F="(%'%s -> %s')" 
        struct_assign, slave, this_union_record
        union_cat[ind_m] = this_union_record
      endif else begin
        ; Update the master position if the slave is better.
        if ((slave.X_ERR^2+slave.Y_ERR^2) LT (master.X_ERR^2+master.Y_ERR^2)) then begin
          ; Replace the master entry in the union with the slave entry.
          if label_available then print, master.LABEL, slave.LABEL, F="(%'%s -> %s')" 
          struct_assign, slave, this_union_record
          union_cat[ind_m] = this_union_record
        endif
      endelse
    endfor ;ii
  endif
  
  ; Append any unmatched cat_slave entries to union_cat.
  extra = where(slave_unmatched_flag, Nextra)
  if (Nextra GT 0) then begin
    ; Enlarge union_cat with enough null rows to hold extra slave sources.
    temp         = temporary(union_cat)
    union_cat    = replicate(null_union_record, Nmaster+Nextra)
    union_cat[0] = temp
    
    ; Copy the extra slave sources into union_cat.
    for ii=0L,Nextra-1 do begin
      this_union_record = null_union_record
      struct_assign, cat_slave[extra[ii]], this_union_record
      union_cat[Nmaster+ii] = this_union_record
    endfor ;ii
  endif
  
  ; We have to regenerate unique ID tags.
  ; Even if the catalog was not expanded, an entry could have been replaced by the slave.
  union_cat.ID = lindgen(Nmaster+Nextra)

  print, Nextra, Nmaster+Nextra, F="(%'Appended %d sources to form union catalog with %d sources.')"  
     
  if keyword_set(union_reg) then begin
    print, 'Union catalog is depicted in the ds9 region file '+union_reg
    catalog_ds9_interface, union_cat, union_reg, /WRITE_REGFILE, _EXTRA=extra_params
  endif
endif ;/UNION
    
    

good = where(match_primary.type EQ successful_primary_type, Ngood)
print
print, Ngood,      ' primary matches succeeded'
print, Nsecondary, ' secondary matches'
return
end ; match_xy





;=============================================================================
;;; The input match_state is from match_xy routine.
;;;
;;; The optional boolean vector ANALYSIS_MASK must have the same number of elements 
;;; as the master catalog; only those source where ANALYSIS_MASK is true will
;;; participate in the offset analysis.
;;; 
;;; /CUSTOM1 enables some extra plots useful for ACIS work..
;;; Supplying ARCSEC_PER_PIXEL value makes plots use arcsecond units.
;;;
;;; The output composite_cat is a catalog with columns from all the catalogs in match_state.
;;; The output isolated_flag is a boolean vector marking the sources with no counterparts in any catalog.
;=============================================================================

PRO match_xy_analyze, match_state, ANALYSIS_MASK=analysis_mask, $
                      CUSTOM1=custom1, ARCSEC_PER_PIXEL=arcsec_per_pixel, QUIET=quiet, SKIP_REGIONS=skip_regions, $
                        
                      composite_cat, isolated_flag, $
                      XSHIFT_MEDIAN=xshift_median, YSHIFT_MEDIAN=yshift_median, XSHIFT_MEAN=xshift_mean, YSHIFT_MEAN=Yshift_mean

COMMON match_xy

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

f_nan    = !VALUES.F_NAN

Nmaster  = n_elements(match_state.X)
Ncat     = n_elements(match_state.catalogs)
master_name = match_state.cat_names[0]

if (n_elements(analysis_mask) EQ 0) then begin
  analysis_mask = replicate(1B, Nmaster)
endif 
if (n_elements(analysis_mask) NE Nmaster) then begin
  print, 'ERROR (match_xy_analyze): ANALYSIS_MASK has the wrong number of elements!'
  return
endif

if ~keyword_set(skip_regions) then skip_regions=0



;; Build a null-filled composite catalog to hold data from all matches.
composite_record = {ID:0L, X:f_nan, Y:f_nan, X_ERR:f_nan, Y_ERR:f_nan}
tag_offset = 5

for jj=0L,Ncat-1 do begin
  cat_name  =  match_state.cat_names[jj]
  cat_slave = *match_state.catalogs [jj]

  ; Create a structure matching the slave catalog but with all fields nulled.
  null_slave_record = cat_slave[0]
  struct_assign, {zzz:0}, null_slave_record
  
  ; Prepend fields from the match record.
  cat_record        = create_struct('IsNull',1B, 'deltaX',f_nan, 'deltaY',f_nan, 'deltaX_error',f_nan, 'deltaY_error',f_nan, 'rank',f_nan, 'type',isolated_type, 'num_SM',0L, null_slave_record)
  
  composite_record  = create_struct(composite_record, cat_name, cat_record)
endfor ;jj

composite_cat = replicate(composite_record, Nmaster)
composite_cat.ID    = match_state.ID
composite_cat.X     = match_state.X
composite_cat.Y     = match_state.Y
composite_cat.X_ERR = match_state.X_ERR
composite_cat.Y_ERR = match_state.Y_ERR


;; These keep track of the type of match each master entry was involved across all catalogs
master_isolated_t   = replicate(0B,Nmaster)
master_failed_t     = replicate(0B,Nmaster)
master_successful_t = replicate(0B,Nmaster)

num_SM      = intarr(Ncat,Nmaster)

;; Figure out the name of the actual parameter corresponding to the formal parameter "composite_cat".
;; We will use this name to construct names for the region files we are creating.
composite_name = strlowcase((routine_names(composite_cat, ARG_NAME=(-1)))[0])
if (composite_name EQ '') then composite_name = 'composite_cat'

ind_wrap = indgen(Ncat) mod 6
symbol_list = (['diamond','cross','circle'      ,'box'    ,'X'     ,'arrow'])[ind_wrap]
color_list  = (['cyan'   ,'red'  ,'DodgerBlue'  ,'magenta','yellow','white'])[ind_wrap]

;; Open a ds9 region file summarizing the results.
regfile1 = composite_name+'.reg'
regfile2 = composite_name+'_pm.reg'

if ~skip_regions then begin
  openw, regunit1, regfile1, /GET_LUN
  ;openw, regunit2, regfile2, /GET_LUN
  printf, regunit1, "# Region file format: DS9 version 3.0"
  ;printf, regunit2, "# Region file format: DS9 version 3.0"
  
  comment1 = "# " + strjoin(match_state.cat_names+' ('+symbol_list+')'  ,', ') + "; successful primary match (green), failed primary match (red), secondary match (magenta), isolated master source (cyan diamond), unused slave source (blue)"
  printf, regunit1, comment1
  printf, regunit1
  printf, regunit1, min(match_state.X), max(match_state.Y), comment1, F='(%"text %f %f # text={%s} color=red")'
  
  
  ;comment2 = "# " + strjoin(match_state.cat_names+' ('+color_list+' '+symbol_list+')'  ,', ') + '; matches are green'
  ;printf, regunit2, comment2
  ;printf, regunit2
  ;printf, regunit2, min(match_state.X), max(match_state.Y), comment2, F='(%"text %f %f # text={%s} color=red")'
endif


;; For coding convenience the master catalog is replicated in both 
;; {match_state.ID, match_state.X/Y, match_state.X_ERR/Y_ERR} and 
;; in *match_state.catalogs[0] (where the matches appear as type "init_type"). 
;;
;; The loop below does two jobs: making ds9 regions for slave sources involved in matches, 
;; and adding catalog entries to composite_cat.
;; On the first iteration (jj EQ 0) the match type should be "init_type" and the ds9 regions are skipped.
;; We don't start the loop at 1 because we want to do the second job for all catalogs.

for jj=0L,Ncat-1 do begin
  z_acceptable = gauss_cvf((1 - sqrt(match_state.sig_thresholds[jj])) / 2.0 )
  if (jj GT 0) then help, z_acceptable

  cat_name  =  match_state.cat_names[jj]
  cat_slave = *match_state.catalogs [jj]
  Nslave         = n_elements(cat_slave)
  slave_written  = bytarr(Nslave)

  ; Write error circles to region file.
;  !TEXTUNIT = regunit1
;  forprint, TEXTOUT=5, /NoComm, cat_slave.X, cat_slave.Y, 0.5*(cat_slave.X_ERR + cat_slave.Y_ERR),  F='(%"circle %f %f %f # tag={error circle} color=Black")'

  ;; Pull out some vectors from match_state and cat_slave structures to speed up execution.
  match_state_ID = match_state.ID
  cat_slave_ID   = cat_slave.ID
  
  
  ;; Process the Primary Match table.  
  match_primary = *match_state.match_primary[jj]
  ; By sorting the successful matches ahead of the unsuccessful and using the slave_written flag array below we can get the slave colors right (i.e. a successful slave is guaranteed to be green even if it is also involved in another match).
  ind_good = where((match_primary.type EQ successful_primary_type) OR $
                   (match_primary.type EQ init_type), Ngood, COMPLEMENT=ind_bad, NCOMPLEMENT=Nbad)

  if     (Ngood EQ 0) then ind=ind_bad $
  else if (Nbad EQ 0) then ind=ind_good $
  else                     ind=[ind_good,ind_bad]   

  t0 = systime(1)
  report_frac = [.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0]
  for ii=0L,Nmaster-1 do begin
;    if ((ii MOD 1000) EQ 0) then print, ii, ii / (systime(1)-t0)
  
    if (float(ii)/Nmaster GT report_frac[0]) then begin
      if ~keyword_set(quiet) then print, cat_name, report_frac[0]*100, F='(%"Processed %s columns/regions for %d%% of master sources.")'
      report_frac = shift(report_frac,-1)
    endif
    
    match  = match_primary[ind[ii]]
    
    save_slave_entry = 0
    
    ind_m  = where(match_state_ID EQ match.IDm)
    ind_s  = where(cat_slave_ID   EQ match.IDs, count)

    if (count GT 0) then begin
      slave = cat_slave[ind_s] 
      deltaX_error = sqrt(match_state.X_ERR[ind_m]^2 + slave.X_ERR^2)
      deltaY_error = sqrt(match_state.Y_ERR[ind_m]^2 + slave.Y_ERR^2)
    endif else begin
      slave = 0
      deltaX_error = !VALUES.F_NAN
      deltaY_error = !VALUES.F_NAN      
    endelse

    if (ind_m[0] NE ind[ii]) then message, 'Order is suspect!'


    if (match.type EQ isolated_type) then begin
      ; No slave region file to write.
      master_isolated_t[ind_m] = 1
      
    endif else if (match.type EQ init_type) then begin
      ; No slave region file to write; just write the slave data to the composite cat.
      save_slave_entry = 1
      
      ; Suppress writing any ds9 regions for this cat.
      slave_written[ind_s] = 1

    endif else begin
      ; Successful and failed PMs here.
      save_slave_entry = 1
      ; Write the slave region and a line segment showing the match.
      if (match.type EQ successful_primary_type) then begin
        ; For a successful match the slave and line are green; use red for unsuccessful.
        color = 'green'
        ltag  = 'primary match, '+cat_name+', good'
        thick = 1
        
        master_successful_t[ind_m] = 1

      endif else begin
        color = 'red'
        ltag  = 'primary match, '+cat_name+', failed'
        thick = 4
        
        master_failed_t[ind_m] = 1 
      endelse

      if (NOT slave_written[ind_s]) then begin
        ; In reg file #1 write a cross point for the slave involved in this PM.
        ; The color designated successful or failed.
        slave_written[ind_s] = 1
        if ~skip_regions then printf, regunit1, symbol_list[jj], slave.x, slave.y, 'position, '+cat_name+', matched', color,   F='(%"%s  point %f %f # tag={%s} color=%s")'
        
        ; In reg file #2 write the catalog-specific symbol in green for successful PM.
;        if (match.type EQ successful_primary_type) then $
;          if ~skip_regions then printf, regunit2, symbol_list[jj], slave.x, slave.y, 'position, '+cat_name+', matched', 'green', F='(%"%s point %f %f # tag={%s} color=%s")'
      endif
 
      ; In reg file #1 write a line segment connecting master and slave to depict the primary match.
      ; Extend the line to the point where the match would have failed (i.e. to the edge of the match rectangle surrounding the master)
      ; to give some visual indication of significance of the match.
      max_deltaX_for_match = z_acceptable * deltaX_error
      max_deltaY_for_match = z_acceptable * deltaY_error
      line_scaling         = min(/NAN, abs([(max_deltaX_for_match/match.deltaX),(max_deltaY_for_match/match.deltaY)]))
      
      if (line_scaling GT 1000) || ~finite(line_scaling) then begin
        ; The match is very close. 
        ; The master and slave entries might very well be identical, if the slave was earlier used to construct the master catalog.
        ; If that's the case, then we don't want any line graphic at all.
      endif else begin
        ; Scale the match line to the desired length.
        x0 = match_state.x[ind_m]
        y0 = match_state.y[ind_m]
        
        x1 = x0 - line_scaling*match.deltaX
        y1 = y0 - line_scaling*match.deltaY
        if ~skip_regions then printf, regunit1, x0, y0, x1, y1, ltag, color, thick, F='(%"line %f %f %f %f # tag={%s} color=%s width=%d")'
      endelse
      
    endelse
    
    if save_slave_entry then begin
      ; Write slave entry to composite catalog plus fields from match record.
      
      record = composite_cat[ind_m].(tag_offset+jj)
      struct_assign, slave, record
      record.IsNull = 0
      record.deltaX = match.deltaX
      record.deltaY = match.deltaY
      
      record.deltaX_error = deltaX_error  
      record.deltaY_error = deltaY_error  
      
      record.rank   = match.rank
      record.type   = match.type
      
      composite_cat[ind_m].(tag_offset+jj) = record
    endif
  endfor ;ii

  
  ; Process the Secondary Match table.
  ; Consult slave_written flag before writing slave entry.
  if ptr_valid(match_state.match_secondary[jj]) then begin
    match_secondary = *match_state.match_secondary[jj]
    
    for ii=0L,n_elements(match_secondary)-1 do begin
      match  = match_secondary[ii]
      ind_m  = where(match_state_ID EQ match.IDm)
      ind_s  = where(cat_slave_ID   EQ match.IDs)
      slave  = cat_slave[ind_s]
      
      num_SM[jj,ind_m] = 1+ num_SM[jj,ind_m]
      
      deltaX_error = sqrt(match_state.X_ERR[ind_m]^2 + slave.X_ERR^2)
      deltaY_error = sqrt(match_state.Y_ERR[ind_m]^2 + slave.Y_ERR^2)

      color = 'magenta'
    
      if (NOT slave_written[ind_s]) then begin
        ; In reg file #1 write a cross point for the slave involved in this SM.
        slave_written[ind_s] = 1
        if ~skip_regions then printf, regunit1, symbol_list[jj], slave.x, slave.y, 'position, '+cat_name+', matched',color,          F='(%"%s  point %f %f # tag={%s} color=%s")'
        
        ; In reg file #2 we don't show SMs, so just write this as an "unused" source using the catalog-specific symbol & color
;        if ~skip_regions then printf, regunit2, symbol_list[jj], slave.x, slave.y, 'position, '+cat_name+', unused', color_list[jj], F='(%"%s point %f %f # tag={%s} color=%s")'
      endif
      
      ; In reg file #1 write a line segment connecting master and slave to depict the secondary match.
      ; Extend the line to the point where the match would have failed (i.e. to the edge of the match rectangle surrounding the master)
      ; to give some visual indication of significance of the match.
      max_deltaX_for_match = z_acceptable * deltaX_error
      max_deltaY_for_match = z_acceptable * deltaY_error
      line_scaling         = min(/NAN, abs([(max_deltaX_for_match/match.deltaX),(max_deltaY_for_match/match.deltaY)]))
      
      if (line_scaling GT 1000) || ~finite(line_scaling) then begin
        ; The match is very close. 
        ; The master and slave entries might very well be identical, if the slave was earlier used to construct the master catalog.
        ; If that's the case, then we don't want any line graphic at all.
      endif else begin
        ; Scale the match line to the desired length.
        x0 = match_state.x[ind_m]
        y0 = match_state.y[ind_m]
        
        x1 = x0 - line_scaling*match.deltaX
        y1 = y0 - line_scaling*match.deltaY
        if ~skip_regions then printf, regunit1, x0, y0, x1, y1, 'secondary match, '+cat_name, color, F='(%"line %f %f %f %f # tag={%s} color=%s")'
      endelse
    endfor ;ii
    
    composite_cat.(tag_offset+jj).num_SM = reform(num_SM[jj,*])
  endif ;ptr_valid()
  
  
  ; Write the unused slave entries.
  ind = where(slave_written EQ 0, count)
  for ii=0L,count-1 do begin
    slave  = cat_slave [ind[ii]]
  
    ; In reg file #1 all unused are blue.
    if ~skip_regions then printf, regunit1, symbol_list[jj], slave.x, slave.y, 'position, '+cat_name+', unused',                 F='(%"%s  point %f %f # tag={%s} color=DodgerBlue")'
    
    ; In reg file #2 use the catalog-specific symbol & color
;    if ~skip_regions then printf, regunit2, symbol_list[jj], slave.x, slave.y, 'position, '+cat_name+', unused', color_list[jj], F='(%"%s point %f %f # tag={%s} color=%s")'
  endfor

endfor ;jj


;; Add the master entries to the region file.

; Identify the sources with no match in ANY catalog.
isolated_flag = master_isolated_t AND NOT (master_failed_t OR master_successful_t)


if ~keyword_set(quiet) then print, F='(%"\nThese master catalog entries should be reviewed:")'

label_available = (total(strmatch(tag_names(match_state),'LABEL')) EQ 1)

; Since ds9 center-justifies text, we must offset the labels for readability, say by 0.2% of the field.
margin = ((max(match_state.x) - min(match_state.x)) > (max(match_state.y) - min(match_state.y))) / 500.
for ii=0L,Nmaster-1 do begin
  if (float(ii)/Nmaster GT report_frac[0]) then begin
    if ~keyword_set(quiet) then print, report_frac[0]*100, F='(%"Processed %d%% of master sources.")'
    report_frac = shift(report_frac,-1)
  endif
  if label_available then $
  LABEL = match_state.LABEL[ii]
  x     = match_state.x    [ii]
  y     = match_state.y    [ii]
  ID    = match_state.ID   [ii]

  color1=''
  color2=''
  
  ; We only warn about isolated sources if they have no match in ANY catalog.
  if (isolated_flag[ii]) then begin
      color1 = color_list[0]
      color2 = color_list[0]
      ptag  = master_name+', isolated'
      if ~keyword_set(quiet) then print, id, x, y, ' isolated'
      
      ; Draw circular region showing zone where a counterpart with zero position error would have matched.
      radius = z_acceptable * (match_state.X_ERR[ii] < match_state.Y_ERR[ii])
      if ~skip_regions then printf, regunit1, x, y, radius, color_list[0], F='(%"circle %f %f %f # tag={match domain} color=%s")'
  endif
  
  ; If PM failed in ANY catalog then we want to report it, but if PM was EVER successful then we want to display it that way.
  if (master_failed_t[ii]) then begin
      color1 = 'red'
      color2 = color_list[0]
      ptag  = master_name+', failed'
      if ~keyword_set(quiet) then print, id, x, y, ' PM failed'
  endif

  if (master_successful_t[ii]) then begin
      color1 = 'green'
      color2 = 'green'
      ptag   = master_name + (analysis_mask[ii] ? ', matched & analyzed' : ', matched')
  endif
  
  if (color1 NE '') then begin
    ; Write a diamond region for the master. 
    ; In reg file #1 the color designates isolated, failed PM, successful PM.
    ; In reg file #2 the color designates (isolated,failed PM) vs. successful PM.
;   if ~skip_regions then printf, regunit2, symbol_list[0], x, y, 'position, '+ptag, ID, color2, F='(%"%s point %f %f # tag={%s} text={%d} color=%s")'
    if ~skip_regions then printf, regunit1, symbol_list[0], x, y, 'position, '+ptag, label_available ? LABEL : strtrim(ID,2), color1, F='(%"%s point %f %f # tag={%s} text={%s} color=%s")'
  endif else message, 'Bug ???'
endfor; ii
if ~skip_regions then free_lun, regunit1
;if ~skip_regions then free_lun, regunit2

total_SM = total(num_SM,1)
ind = where(total_SM GT 0, count)
if ~keyword_set(quiet) && (count GT 0) then begin
  print
  print, 'These master catalog entries have secondary matches:'
  forprint, match_state.ID[ind], match_state.X[ind], match_state.Y[ind], total_SM[ind]
endif

if ~keyword_set(quiet) && ~skip_regions then begin
  print
  print, 'The region file ', regfile1, ' shows details of matching using these symbols and colors:'
  print, comment1
  print
;  print, 'The region file ', regfile2, ' shows each catalog with a different symbol with matches marked in green:'
;  print, comment2
;  print
endif

;; Finally, show some match statistics to help the user choose catalog offsets.
for jj=0L,Ncat-1 do begin
  cat_name  =  match_state.cat_names[jj]
  
  composite_cat_section = composite_cat.(tag_offset+jj)

  mask = (composite_cat_section.type EQ successful_primary_type) AND analysis_mask  
  
  good_ind = where( mask, Nanalysis, COMPLEMENT=bad_ind, NCOMPLEMENT=Nignore )

  if (jj EQ 0) then begin
  endif else if (Nanalysis EQ 0) then begin
    print, cat_name, F='(%"\nWARNING! The slave catalog %s has no matches that can estimate an offset from the master catalog!")'
    xshift_median=0
    yshift_median=0
    xshift_mean  =0
    Yshift_mean  =0
  endif else begin
    ;; Examine the deltaX,deltaY offsets.
    deltaX = composite_cat_section.deltaX
    deltaY = composite_cat_section.deltaY
    
    deltaX_error = composite_cat_section.deltaX_error
    deltaY_error = composite_cat_section.deltaY_error
    
    ; The data for the catalog entries we do NOT want to analyze are set to NaN, rather than filtered away, so that the observer can read off indexes in dataset_2d.
    if (Nignore GT 0) then begin
      deltaX[bad_ind]       = f_nan
      deltaY[bad_ind]       = f_nan
      deltaX_error[bad_ind] = f_nan
      deltaY_error[bad_ind] = f_nan
    endif
    radius = sqrt(deltaX^2 + deltaY^2)
    print, 'Median radial offset:', median(radius)
    
    deltaX_title = 'Xmaster - Xslave'
    deltaY_title = 'Ymaster - Yslave'
    radius_title = 'match distance'
    
    if keyword_set(arcsec_per_pixel) then begin
      deltaX       = arcsec_per_pixel * deltaX
      deltaY       = arcsec_per_pixel * deltaY
      deltaX_error = arcsec_per_pixel * deltaX_error
      deltaY_error = arcsec_per_pixel * deltaY_error
      deltaX_title = deltaX_title + ' [arcsec]'
      deltaY_title = deltaY_title + ' [arcsec]'
      radius_title = radius_title + ' [arcsec]'
    endif

    if (Nanalysis GT 1) then begin
      dataset_1d, idx, deltaX,  DATASET=cat_name, XTIT=deltaX_title
      dataset_1d, idy, deltaY,  DATASET=cat_name, XTIT=deltaY_title
      dataset_1d, idr2, radius, DATASET=cat_name, XTIT=radius_title
      
      dataset_1d, idsx, abs(deltaX) / deltaX_error, DATASET=cat_name, XTIT='|Xoffset|/Xsigma'
      dataset_1d, idsy, abs(deltaY) / deltaY_error, DATASET=cat_name, XTIT='|Yoffset|/Ysigma'
      
      z_enclosing_match = (abs(deltaX) / deltaX_error) > (abs(deltaY) / deltaY_error)
      dataset_1d, idsxy, z_enclosing_match, DATASET=cat_name, XTIT='Z limit required for match (|Xoffset|/Xsigma > |Yoffset|/Ysigma)'
      print, 'Conversion from a "Z limit" (see plot) to the "significance" input to match_xy is: (1D - 2*gauss_pdf(Z))^2'
;      
;      significance_threshold_required = (1D - 2*gauss_pdf(z_enclosing_match))^2
;      
;      dataset_1d, idsxy, alog10(significance_threshold_required), DATASET=cat_name, XTIT='log of significance_threshold required for match'
      

      dataset_2d, idxe, abs(deltaX) / deltaX_error, deltaX_error, NAN_VALUES=[0,0], DATASET=cat_name, XTIT='|Xoffset|/Xsigma', YTIT='Xsigma', PSYM=1
      dataset_2d, idye, abs(deltaY) / deltaY_error, deltaY_error, NAN_VALUES=[0,0], DATASET=cat_name, XTIT='|Yoffset|/Ysigma', YTIT='Ysigma', PSYM=1
    endif
    
    ;; ------------------------------------------------------------------------
    ; It's important to make the partvelvec plot have unity aspect.
    ; Estimate the plot region size in device units.
    xrange = minmax(composite_cat.X)
    yrange = minmax(composite_cat.Y)
    xlen_est = !D.X_SIZE - !D.X_CH_SIZE * total( !X.margin )
    ylen_est = !D.Y_SIZE - !D.Y_CH_SIZE * total( !Y.margin )
  
    ; Enlarge the axis ranges to center desired region and have 1-1 aspect.
    pixel_size = max( [(xrange[1] - xrange[0]) / xlen_est, $
                       (yrange[1] - yrange[0]) / ylen_est] )
                
    xrange = ((xrange[0]+xrange[1]) / 2.) + $
                        pixel_size * xlen_est * [-0.5,0.5]
    
    yrange = ((yrange[0]+yrange[1]) / 2.) + $
                        pixel_size * ylen_est * [-0.5,0.5]

    rescale = (xrange[1]-xrange[0]) / median(deltaX[good_ind])
    if (Nanalysis GT 1) then begin
      window, jj, xsize=1800,ysize=1400
      partvelvec, deltaX[good_ind], deltaY[good_ind], composite_cat[good_ind].X, composite_cat[good_ind].Y, TITLE='Vector offsets (magnified) for analyzed matches', XRANGE=xrange, YRANGE=yrange, XSTYLE=1, YSTYLE=1, LENGTH=0.12
    endif

    print, Nanalysis, cat_name, F='(%"\nThe observed offsets (for the specified %d matches) from the slave catalog %s to the master catalog have been plotted.")'
    print, 'To improve the matching, re-run match_xy and supply XSHIFT/YSHIFT values which will be ADDED to the slave catalog.'
    print, 'Some suggested shifts are:'
    
    ; This is our own routine for estimating uncertainties on the median.
    median_with_ci, deltaX[good_ind], xshift_median, limit_lower, limit_upper, actual_confidence_level
    xshift_median_error = 0.5*(limit_upper-limit_lower)
    
    median_with_ci, deltaY[good_ind], yshift_median, limit_lower, limit_upper, actual_confidence_level
    yshift_median_error = 0.5*(limit_upper-limit_lower)
    
    print, xshift_median, yshift_median, xshift_median_error, yshift_median_error, F='(%"median     : XSHIFT=%0.3f, YSHIFT=%0.3f (PIXELS); 1-sigma errors: %0.3f %0.3f")'
                                                   
    ; This is a sigma-clipped mean routine in the Astro Library.
    resistant_mean, deltaX[good_ind], 3.0, xshift_mean, xshift_mean_error, num_rejectedX
    resistant_mean, deltaY[good_ind], 3.0, yshift_mean, yshift_mean_error, num_rejectedY
     
    print, xshift_mean, yshift_mean, xshift_mean_error, yshift_mean_error, Nanalysis-num_rejectedX, Nanalysis-num_rejectedY, F='(%"robust mean: XSHIFT=%0.3f, YSHIFT=%0.3f (PIXELS); 1-sigma errors: %0.3f %0.3f ; # of matches used: %d %d")'
    
    if keyword_set(custom1) then begin
      theta  = (composite_cat.acis.theta)
      dataset_2d, idr1, theta, radius, PSYM=1, DATASET=cat_name, XTIT='theta [arcmin]', YTIT=radius_title
    endif
  endelse
endfor ;jj

return
end ; match_xy_analyze


;=============================================================================
; Estimate various performance rates of the matching algorithm via Monte Carlo simulations.
; This code is performing two different MC simulations simultaneously.

; One simulation estimates the false-positive (and true-negative) rate under the assumption that NO actual counterpart exists.

; The other simulation estimates the false-positive and false-negative (and true-positive) rates under the assumption that a counterpart (fake) does exist.

;=============================================================================
PRO match_xy_simulate, match_state, cat_name, Nsim

COMMON match_xy_simulate, num_cp_correct_match, num_cp_wrong_match, num_cp_false_negative, num_bkg_false_positive, num_bkg_true_negative, id1, id2

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

Nmaster    = n_elements(match_state.X)

;; Find this catalog in match_state.
cat_ind = (where(match_state.cat_names EQ cat_name, count))[0]
if (count EQ 0) then begin
  print, 'ERROR (match_xy_simulate): catalog not found.'
  return
endif

significance_threshold = match_state.sig_thresholds[cat_ind]
cat_slave              = *(match_state.catalogs      [cat_ind])
match_primary          = *(match_state.match_primary [cat_ind])

;; Pull out some vectors from match_state and cat_slave structures to speed up execution.
cat_slave_ID   = cat_slave.ID
match_state_ID = match_state.ID


;; Remove the primary matches from cat_slave in an attempt to eliminate the population of true counterparts.
;; We'll be adding fake counterparts to cat_slave later; the total number of sources will be approximately unchanged.
;; We adopt the position errors of the actual matches as the population of position errors to use later for generating fake counterparts.
retain_ind = replicate(1B, n_elements(cat_slave))
for ii=0L,Nmaster-1 do begin
  if (match_primary[ii].type EQ successful_primary_type) then begin
    retain_ind[ where(cat_slave_ID EQ match_primary[ii].IDs) ] = 0
  endif
endfor

X_err_population = cat_slave[where(retain_ind EQ 0)].X_ERR
Y_err_population = cat_slave[where(retain_ind EQ 0)].Y_ERR

dataset_1d, id1, X_err_population
dataset_1d, id1, Y_err_population

cat_slave = cat_slave[where(retain_ind)]
help, cat_slave

;; Choose a minimum catalog offset sufficient to randomize the phase between master and slave.
min_offset = 4 * (max(abs(match_primary.DELTAX)) > max(abs(match_primary.DELTAY)))

;; Generate random catalog offsets.
theta  = 2*!PI*random(Nsim)
radius = (min_offset + min_offset*random(Nsim))

xshift_slave = radius*sin(theta)
yshift_slave = radius*cos(theta)

dataset_2d, id2, xshift_slave, yshift_slave, PSYM=3


;; SIMULATION LOOP
num_cp_correct_match   = lonarr(Nsim)
num_cp_wrong_match     = lonarr(Nsim)
num_cp_false_negative  = lonarr(Nsim)
num_bkg_false_positive = lonarr(Nsim)
num_bkg_true_negative  = lonarr(Nsim)

for ii=0L,Nsim-1 do begin
  ;; Generate fake counterparts for the master sources.  

  ; Create a structure matching the slave catalog but with all fields nulled.
  null_slave_record = cat_slave[0]
  struct_assign, {zzz:0}, null_slave_record
  
  cat_fake = replicate(null_slave_record, Nmaster)
  
  ; Assign IDs that are unique and easy to associate with the master source IDs.  
  cat_fake.ID = -(match_state.ID)
  
  ; Generate random counterpart position errors.
  N = n_elements(X_err_population)
  ind = (N-1) < floor(N*random(Nmaster))
  cat_fake.X_ERR = X_err_population[ind]
  cat_fake.Y_ERR = Y_err_population[ind]
  
  ; Generate random counterpart offsets from master positions.
  ; The offset random variables are Normal with variance equal to 
  ; the sum of the variances of master and slave (as assumed in match_xy routine).
  deltaX = sqrt(match_state.X_ERR^2 + cat_fake.X_ERR^2) * random(Nmaster, /NORMAL)
  deltaY = sqrt(match_state.Y_ERR^2 + cat_fake.Y_ERR^2) * random(Nmaster, /NORMAL)

  ; Their positions include the negative of the catalog offset so that
  ; they will end up matching the master positions.
  cat_fake.X = match_state.X - xshift_slave[ii] + deltaX
  cat_fake.Y = match_state.Y - Yshift_slave[ii] + deltaY
  
  
  ;; Perform the match using the augmented slave catalog.
  sim_cat_name = cat_name + '_SIM'
  match_xy, match_state, [cat_slave,cat_fake], sim_cat_name, significance_threshold, $
	    XSHIFT_SLAVE=xshift_slave[ii], YSHIFT_SLAVE=yshift_slave[ii], /QUIET

  ;; Find this catalog in match_state.
  sim_cat_ind = (where(match_state.cat_names EQ sim_cat_name, count))[0]
  if (count EQ 0) then begin
    print, 'ERROR (match_xy_simulate): catalog not found.'
    return
  endif
  help, sim_cat_ind

  ;; Tablulate interesting classes of match results.
  cp_correct_match  = bytarr(Nmaster)  ; Fake counterpart is primary match.
  cp_wrong_match    = bytarr(Nmaster)  ; Bkg source is primary match.
  cp_false_negative = bytarr(Nmaster)  ; No match found.
  
  bkg_false_positive= bytarr(Nmaster) ; Primary or a secondary match is bkg source
 ;bkg_true_negative = bytarr(Nmaster) ; NOT bkg_false_positive

  match_primary = *(match_state.match_primary [sim_cat_ind])
  for jj=0L,Nmaster-1 do begin
    match = match_primary[jj]
    if (match.type EQ successful_primary_type) then begin
      ind_m = where(match_state_ID EQ match.IDm)
      if (match.IDm EQ -(match.IDs)) then begin
        ; Primary match was the fake counterpart.
        cp_correct_match[ind_m] = 1 
      endif else begin
        ; Primary match was a background source.
        cp_wrong_match    [ind_m] = 1
        bkg_false_positive[ind_m] = 1
      endelse
    endif else cp_false_negative[jj] = 1 ; No match found.
  endfor ;jj

  ; For a source with a secondary match, it must be the case that the 
  ; no-counterpart simulation would have produced a bkg_false_positive outcome.
  if ptr_valid(match_state.match_secondary[sim_cat_ind]) then begin
    match_secondary = *(match_state.match_secondary[sim_cat_ind])
    
    for jj=0L,n_elements(match_secondary)-1 do begin
      match  = match_secondary[jj]
      ind_m  = where(match_state_ID EQ match.IDm)
      bkg_false_positive[ind_m] = 1
    endfor ;jj
  endif
  
  ; Test the flags for obvious inconsistencies.
  test = (cp_correct_match AND cp_wrong_match) OR (cp_wrong_match AND cp_false_negative) OR (cp_false_negative AND cp_correct_match)
  if (total(test) GT 0) then message, 'WRONG!'
  
  test = (cp_correct_match OR cp_wrong_match OR cp_false_negative) EQ 0
  if (total(test) GT 0) then message, 'WRONG!'

  num_cp_correct_match  [ii] = total(cp_correct_match,  /INTEGER)
  num_cp_wrong_match    [ii] = total(cp_wrong_match,    /INTEGER)
  num_cp_false_negative [ii] = total(cp_false_negative, /INTEGER)
  num_bkg_false_positive[ii] = total(bkg_false_positive,/INTEGER)
  num_bkg_true_negative [ii] = Nmaster - num_bkg_false_positive[ii]
endfor ;ii sim loop


;; Test the tabulations for obvious problems.
if (total((num_cp_correct_match+num_cp_wrong_match+num_cp_false_negative) NE Nmaster) NE 0) then message, 'WRONG!'

info, num_cp_correct_match
info, num_cp_wrong_match
info, num_cp_false_negative
info, num_bkg_false_positive
info, num_bkg_true_negative
print
print, 'Nmaster = ', Nmaster

return
end


;=============================================================================

; Smaller values of SIGNIFICANCE_THRESHOLD force matches to be more certain.

;=============================================================================
PRO match_xy_estimate_offset, observation_cat, reference_cat, SIGNIFICANCE_THRESHOLD=significance_threshold, $
      OBS_XSHIFT=obs_xshift, OBS_YSHIFT=obs_yshift

if ~keyword_set(significance_threshold) then significance_threshold=0.99

; Peform an initial match.  We will (confusingly) define the observation catalog to be the 
; "master" in this run, even though it's the one with the wrong astrometry, as a
; trick to improve runtime since the observation will typically have fewer sources
; than the reference catalog.  We'll make sure to keep this straight
; when applying the offsets later.
match_xy, hyper_cat, observation_cat, 'OBS', /QUIET, /INIT
match_xy, hyper_cat, reference_cat  , 'REF', /QUIET, significance_threshold


; Construct a composite catalog, obs_ref, containing all the observed sources
; plus information from the matching reference sources.
print, F="(%'\n========================\nANALYSIS OF ALL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, /QUIET, /SKIP_REGIONS 

; Analyze the matches in order to recommend a shift to be applied to the slave
; catalog, which is confusingly the reference catalog here!
; The parameter ANALYSIS_MASK should be 1 (true) for high-quality sources that 
; should be included in the offset analysis.
print, F="(%'\n=============================\nANALYSIS OF FIDUCIAL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, /QUIET, /SKIP_REGIONS, $
  ANALYSIS_MASK=((obs_ref.OBS.NOT_FIDUCIAL EQ 0) AND (obs_ref.REF.NOT_FIDUCIAL EQ 0)), $
  XSHIFT_MEAN=xshift_total, YSHIFT_MEAN=yshift_total


; Repeat the matching with those offsets supplied via the XSHIFT/YSHIFT
; parameters. The matcher will ADD those offsets to the slave catalog before
; performing the match. The number of matches may change a little, especially if
; the offsets between the two catalogs were large.
match_xy, hyper_cat, reference_cat  , 'REF', /QUIET, significance_threshold, XSHIFT=xshift_total, YSHIFT=yshift_total

; Re-analyze the offsets of the matched high-quality sources to determine how much more shifting is required.
print, F="(%'\n========================\nANALYSIS OF ALL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, /QUIET, /SKIP_REGIONS 
print, F="(%'\n=============================\nANALYSIS OF FIDUCIAL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, /QUIET, /SKIP_REGIONS, isolated_flag, $
  ANALYSIS_MASK=((obs_ref.OBS.NOT_FIDUCIAL EQ 0) AND (obs_ref.REF.NOT_FIDUCIAL EQ 0)), $
  XSHIFT_MEAN=xshift_extra, YSHIFT_MEAN=yshift_extra

xshift_total += xshift_extra
yshift_total += yshift_extra


; Repeat the matching with the revised shifts, and analyze one last time to report how much residual shift remains.
match_xy, hyper_cat, reference_cat  , 'REF', /QUIET, significance_threshold, XSHIFT=xshift_total, YSHIFT=yshift_total
print, F="(%'\n========================\nANALYSIS OF ALL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, /QUIET
print, F="(%'\n=============================\nANALYSIS OF FIDUCIAL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, /QUIET, /SKIP_REGIONS, isolated_flag, $
  ANALYSIS_MASK=((obs_ref.OBS.NOT_FIDUCIAL EQ 0) AND (obs_ref.REF.NOT_FIDUCIAL EQ 0))


; Report the brightest unmatched sources.
if (total(strmatch(tag_names(obs_ref),'NET_COUNTS')) GT 0) then begin
  ind = where(isolated_flag AND (obs_ref.OBS.NOT_FIDUCIAL EQ 0), count)
  if (count GT 0) then begin
    print, F='(%"\n Below are the brightest unmatched sources.\n  ID COUNTS     (X,Y)")'
    sort_ind = reverse(ind[sort((obs_ref.OBS.NET_COUNTS)[ind])])
    forprint, SUBSET=sort_ind[0:(10<count)-1], obs_ref.OBS.ID, obs_ref.OBS.NET_COUNTS, round(obs_ref.OBS.X), round(obs_ref.OBS.Y), F='(%"%4d   %4d  (%d,%d) ")'
  endif
endif

; Save the session.
save, FILE='obs_ref.sav'
print, F='(%"\n Match data structures saved to obs_ref.sav.")'

; Report the actual shift that should be supplied to the CIAO tool wcs_update.
obs_xshift = -xshift_total
obs_yshift = -yshift_total
print, obs_xshift, obs_yshift, F='(%"\n We suggest that you use wcs_update to shift the observation by deltax=%0.3f deltay=%0.3f pixels.")'
print, 'Our estimate of the residual offset that will remain is shown in the last iteration above, and in the plots.'
return
end



PRO test2, significance_threshold

dim=100
x=10*indgen(dim)
y=x
make_2d,x,y
Nentries = n_elements(x)
x=reform(x,Nentries)
y=reform(y,Nentries)

entry = {ID:0L, X:0., Y:0., X_ERR:0., Y_ERR:0. }
cat = replicate(entry,Nentries)
cat.ID = 1+indgen(Nentries)

X_ERR = 0.01
Y_ERR = 0.05
cat.X  = x + X_ERR*random(Nentries, /NORMAL)
cat.Y  = y + Y_ERR*random(Nentries, /NORMAL)
cat.X_ERR = X_ERR
cat.Y_ERR = Y_ERR

match_xy, match_state, cat, 'one', /INIT

X_ERR = 0.5
Y_ERR = 0.1
cat.X  = x + X_ERR*random(Nentries, /NORMAL)
cat.Y  = y + Y_ERR*random(Nentries, /NORMAL)
cat.X_ERR = X_ERR
cat.Y_ERR = Y_ERR

match_xy, match_state, cat, 'two', significance_threshold

 
return
end



PRO test1, cat1, cat2, match_primary, match_secondary

record = {source, ID:0L, X:0.0, Y:0.0, X_ERR:1.0, Y_ERR:1.0}
cat2 = replicate({source},12)
x=indgen(4)
y=indgen(3)
make_2d,x,y
cat2.x=reform(x+1,12)
cat2.y=reform(y+1,12)
cat2.id=indgen(12)
cat2.X_ERR=0.5
cat2.Y_ERR=1

plot,cat2.x,cat2.y,line=0,psym=4,xrange=[0,4],yrange=[0,4]

cat1 = replicate({source},3)
cat1.x=[2.5,2.5,2.5]
cat1.y=[1.5,2,2.5]
cat1.id=indgen(3)
cat1.X_ERR=1
cat1.Y_ERR=0.5
oplot,cat1.x,cat1.y,line=0,psym=2

significance_threshold=0.1
match_xy,         cat1, cat2, significance_threshold, match_primary, match_secondary

match_xy_analyze, cat1, cat2, significance_threshold, match_primary, match_secondary
return
end





;=============================================================================
; Interactive Editing of Catalogs
;
; It's often useful to be able to prune catalogs interactively, e.g. while displayed
; on top of data in ds9.  
; The two routines below allow you to represent a catalog as a ds9 region file, 
; prune it in ds9, then read it back into IDL and apply that pruning to the 
; actual catalog.

; You are  ONLY allowed to DELETE regions in ds9.  You may not add or move regions!
;
; The ds9 property "tag={ID%d}" is used to refererence a region back to the catalog.
;
;=============================================================================
PRO catalog_ds9_interface, cat, reg_filename, $
                           WRITE_REGFILE=write_regfile, OMIT_LABELS=omit_labels, $
                           PRUNE_CATALOG=prune_catalog

catalog_name_available = (total(strmatch(tag_names(cat),'CATALOG_NAME')) EQ 1)
label_available        = (total(strmatch(tag_names(cat),'LABEL')) EQ 1)

if keyword_set(write_regfile) then begin
  
  openw, regunit1, reg_filename, /GET_LUN
  printf, regunit1, "# Region file format: DS9 version 3.0"

  Nentries = n_elements(cat)
  tags     = tag_names(cat)
  tag_flag = strmatch(tags, 'SHAPE', /FOLD) OR strmatch(tags, 'R', /FOLD) OR strmatch(tags, 'ROTANG', /FOLD)
  is_fits_region = total(tag_flag) EQ 3
  
  if catalog_name_available then begin
    catalog_name = strtrim(cat.CATALOG_NAME,2)
    catalog_tag  = 'tag={'+catalog_name+'}'
    
    ; Try to assign different colors to each catalog name, based on frequency of the name.
    color_list =  ['red','green','cyan','magenta','yellow','DodgerBlue','Cornsilk','Goldenrod','Chocolate','DarkSalmon', 'Tan','Peru','Sienna','Salmon', 'SandyBrown','DarkGoldenrod','Brown','IndianRed','SlateBlue']

    num_colors = n_elements(color_list)
    colors     = replicate(color_list[num_colors-1],Nentries)
  
    name_list  = catalog_name[uniq(catalog_name, sort(catalog_name))]
    num_names  = n_elements(name_list)
    name_count = lonarr(num_names)
    for ii=0, num_names-1 do begin
      name_count[ii] = total(/INT, strmatch(catalog_name, name_list[ii]))
    endfor
    name_list = name_list[reverse(sort(name_count))]
    print,  F="(%'\n    COLOR  CATALOG')"
    for ii=0, num_names-1 do begin
      this_color = color_list[ii < (num_colors-1)]
      colors[where(strmatch(catalog_name, name_list[ii]))] = this_color
      print, this_color, name_list[ii], F="(%'%9s  %s')"
    endfor

  endif else begin
    colors      = replicate('green',Nentries)
    catalog_tag = replicate('',Nentries)
  endelse ; ~catalog_name_available
  
  if keyword_set(omit_labels) then label=strarr(Nentries) $
  else begin
    if label_available then label = string(strtrim(cat.LABEL,2),  F='(%" text={%s} ")') $
    else                    label = string(        cat.ID      ,  F='(%" text={%d} ")')
  endelse
  
  for ii=0L,Nentries-1 do begin
    s=cat[ii]
        
    if is_fits_region then begin
      ; Silly wavdetect can have ellipse radii that are NaN or zero, which can be invisible in ds9!
      ; The max() calls below take care of both.
      printf, regunit1, s.x, s.y, max([2, s.r[0]]), max([2, s.r[1]]), s.rotang, s.ID, catalog_tag[ii], colors[ii], label[ii], $
        F='(%"ellipse %f %f %f %f %f # tag={ID%d} %s color=%s %s")'
    endif else begin
      printf, regunit1, s.x, s.y, s.ID, catalog_tag[ii], colors[ii], label[ii], $
        F='(%"cross point %f %f # tag={ID%d} %s color=%s %s")'
    endelse
  endfor ;ii
  
  free_lun, regunit1
  print, 'Regions written to '+reg_filename
endif


if keyword_set(prune_catalog) then begin
  ; Read the lines from the region file.
  num_lines = file_lines(reg_filename)
  lines = strarr(num_lines)  
  openr, regunit1, reg_filename, /GET_LUN
  readf, regunit1, lines
  free_lun, regunit1
  
  ; Find the lines with  and without ID tags.
  retain_ID = reform( (stregex(lines,'tag=\{ID([0-9]+)',/SUB,/EXT))[1,*] )
  
  region_ind = where(retain_ID NE '', COMPLEMENT=ignored_ind, NCOMPLEMENT=num_ignored)
  
  retain_ID = long(retain_ID[region_ind])
  
  ; Report any interesting lines in the region file that we're ignoring.
  if (num_ignored GT 0) then begin
    lines = lines[ignored_ind]
    ind = where(stregex(/BOOLEAN, lines, '(^#)|(^global)|(^physical)') EQ 0, count)
    if (count GT 0) then begin
      print, F="(%'\nWARNING!  The following lines in the region file were ignored.\nYou are not allowed to ADD regions via this interface!')"
      forprint, lines, SUBSET=ind
    endif
  endif
  
  
  Nentries = n_elements(cat)
  retain_row = bytarr(Nentries)
  cat_ID = cat.ID
  for ii=0,n_elements(retain_ID)-1 do begin
    ind = where(cat_ID EQ retain_ID[ii], count)
    if (count EQ 0) then begin
      print, 'ERROR: region file contained ID tag not found in catalog:', retain_ID[ii]
      retall
    endif
    
    if (count GT 1) then begin
      print, 'ERROR: catalog has duplicated ID value: ', retain_ID[ii]
      retall
    endif
    
    if retain_row[ind] then begin
      print, 'ERROR: region file has duplicated ID value: ', retain_ID[ii]
      retall
    endif

    retain_row[ind] = 1
  endfor ;ii
    
  retain_ind = where(retain_row, COMPLEMENT=prune_ind, NCOMPLEMENT=prune_count)
  print, prune_count, F='(%"Pruned %d sources:")'
  if (prune_count GT 0) then begin
    if catalog_name_available then catalog_name = strtrim(cat.CATALOG_NAME,2) $
    else                           catalog_name = replicate('', Nentries)
  
    if label_available then label = string(strtrim(cat.LABEL,2),  F='(%" label: %s ")') $
    else                    label = string(        cat.ID      ,  F='(%" ID: %d ")')
  
    forprint, SUBSET=prune_ind, label, catalog_name
  
    cat = cat[retain_ind]
  endif
  
endif

return
end ; catalog_ds9_interface


;=============================================================================
; Tool to reproject an existing match_xy catalog onto a new tangent plane.

; The existing catalog must have RA and DEC columns.

; The match_events_fn input provides a definition for the new sky coordinate system.
;=============================================================================

PRO match_xy_reproject_cat, cat, match_events_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, cat.RA, cat.DEC, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
cat.X  = X+1
cat.Y  = Y+1

return
end


;=============================================================================
; Function to build a catalog from vectors (id, ra, dec, ra_err, dec_err).

; Input id is a unique long integer identifier for each source, or a scalar 0 to generate sequence numbers.

; Inputs ra_err, dec_err are 1-sigma single-axis position uncertainties, in arcseconds.

; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_generic_cat, id, ra, dec, ra_err, dec_err, match_events_fn, NAME=cat_name

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

Nentries = n_elements(ra)
if ~keyword_set(id) then id = 1+lindgen(Nentries)


;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
X_ERR =  ra_err / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = dec_err / 0.492 ; (0.492 arcsec/skypix)

CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'generic_cat' 

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','RA','DEC','CATALOG_NAME']
tag_formats = 'J,F,F,F,F,D,D,A'                 

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources created.'
return, cat
end


;=============================================================================
; Example code to read an ASCII catalog that has sexigesimal (J2000) celestial coordinates 
; stored in six columns: RAhour, RAmin, RAsec, Decdeg, Decmin, Decsec
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

; Modify as needed to accomodate whatever other columns

; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_sexigesimal_cat, catalog_fn, match_events_fn, NAME=cat_name

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

column_names = ['name','p1','p2','p3','p4','p5','p6']

column_formats = 'A,D,D,D,D,D,D'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/null/NaN/g'' %s > %s")')
cmd = string(strjoin(column_names,','), column_formats, F='(%"readcol, ''temp.txt'', %s, F=''%s'' ")') + ', COMMENT="\"' 
print, cmd
dum=execute(cmd)

Nentries = n_elements(p1)
id = 1+lindgen(Nentries)

; Convert from sexigesimal to decimal.
ra  = tenv(p1,p2,p3)*15.D
dec = tenv(p4,p5,p6)

;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
 ra_err = 0.5 ; arcsec
dec_err = 0.5 ; arcsec
X_ERR =  ra_err / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = dec_err / 0.492 ; (0.492 arcsec/skypix)

CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'sexigesimal_cat' 

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','CATALOG_NAME',column_names]
tag_formats = 'J,F,F,F,F,A,'                              +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources created.'
return, cat
end


;=============================================================================
; Function to read a catalog represented as a ds9 region file with celestial coordinates.
; Sources can be represented by circle or ellipse regions.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_ds9_cat, region_filename, match_events_fn, NAME=cat_name
             
;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn


  ; Read the lines from the region file.
  num_lines    = file_lines(region_filename)
  lines        = strarr(num_lines)  
  openr, regunit1, region_filename, /GET_LUN
  readf, regunit1, lines
  free_lun, regunit1
  
  ra   = dblarr(num_lines)
  dec  = dblarr(num_lines)
  region_type = strarr(num_lines)
  
  ; Parse "ellipse" regions for RA & DEC values.
  ; This regular expression should work for positive or negative DEC, 
  ; space or comma separation between parameters, 
  ; and with or without () in the region specification.
  result = stregex(lines,'ellipse[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
  tmp_ra  = double(reform(result[1,*]))
  tmp_dec = double(reform(result[2,*]))
  ind = where(tmp_ra NE 0 AND tmp_dec NE 0, count)
  if (count GT 0) then begin
    ra  [ind] = tmp_ra [ind]
    dec [ind] = tmp_dec[ind]
    region_type[ind] = 'ellipse'
  endif

    ; Parse "circle" regions for RA & DEC values.
  result = stregex(lines,'circle[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
  tmp_ra  = double(reform(result[1,*]))
  tmp_dec = double(reform(result[2,*]))
  ind = where(tmp_ra NE 0 AND tmp_dec NE 0, count)
  if (count GT 0) then begin
    ra  [ind] = tmp_ra [ind]
    dec [ind] = tmp_dec[ind]
    region_type[ind] = 'circle'
  endif

  ind = where(ra NE 0 AND dec NE 0, count)
  if (count EQ 0) then begin
    print, 'ERROR (build_ds9_cat): no source coordinates could be parsed'
    return, 0
  endif

  ra  = ra  [ind]
  dec = dec [ind]
  region_type= region_type[ind]
;forprint, ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)


;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; We arbitrarily assign a 1" error.
X_ERR = 1.0 / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR

CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'source_ds9' 

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','RA','DEC','REGION_TYPE','CATALOG_NAME']
tag_formats = 'J,F,F,F,F,D,D,A,A'                 

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a standard wavdetect FITS catalog
; The optional match_events_fn input provides a new definition for the sky coordinate system.
;
; Weak and off-axis sources are flagged as NOT_FIDUCIAL.
;=============================================================================
FUNCTION build_wavdetect_cat, catalog_fn, match_events_fn, NAME=cat_name, QUIET=quiet

;; Read FITS catalog.
cat = mrdfits(catalog_fn, 1)
Nentries = n_elements(cat)

;; The (X_ERR,Y_ERR) from wavdetect are merely statistical errors for the mean event position within the extraction region.  
;; Systematic errors are not represented.
;; These errors can get unrealistically small  which causes us to miss matches that are clearly legitimate.
;; We choose to arbitrarily add in 0.2" error to ACIS positions to help deal with this.
systematic_error = 0.2  / 0.492 ; (0.492 arcsec/skypix)
cat.X_ERR = sqrt(cat.X_ERR^2 + systematic_error^2)
cat.Y_ERR = sqrt(cat.Y_ERR^2 + systematic_error^2)
                                                                                                                               

if keyword_set(match_events_fn) then begin
  ;; Convert celestial coordinates to a new tangent plane.

  ;; Read event file header to define a different sky coordinate system.
  theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
  if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn
  
   ; Build astrometic structure from data header.
  tbinfo, theader, tb_str
  colnames = strlowcase( strtrim(tb_str.ttype,2) )
  wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
  wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')
  
  wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
  wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY
  
  make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                             CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]
  
  ad2xy, cat.ra, cat.dec, event2wcs_astr, x, y
  ; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
  cat.X  = X+1
  cat.Y  = Y+1
endif ;keyword_set(match_events_fn) 

if keyword_set(cat_name) then begin
  if (total(strmatch(tag_names(cat),'CATALOG_NAME')) EQ 0) then begin
    ; We need to add a CATALOG_NAME column to the catalog.
    temp_cat = replicate(create_struct(cat[0], 'CATALOG_NAME', cat_name), Nentries)
    struct_assign, cat, temp_cat
    cat = temp_cat
    cat.CATALOG_NAME = cat_name
  endif else print, 'WARNING!  The catalog already has a CATALOG_NAME tag; ignoring your NAME keyword.'
endif ;keyword_set(cat_name)



;; Flag sources that would not make good astrometric fiducials.
;; Since some wavdetect errors seem to come out to be zero, we
;; also use a threshold on NET_COUNTS to be sure to omit very weak sources.
;; Since wavdetect positions probably have a systematic error off-axis we 
;; arbitrarily use only ACIS sources less than 5' off-axis.
temp_cat = replicate(create_struct(cat[0], 'NOT_FIDUCIAL', 0B), Nentries)
struct_assign, cat, temp_cat
cat = temp_cat
theta = sqrt((cat.X - 4096)^2 + (cat.Y - 4096)^2) * 0.492 / 60. ; (0.492 arcsec/skypix)
cat.NOT_FIDUCIAL = (cat.NET_COUNTS LE 10) OR (THETA GT 5)

if ~keyword_set(quiet) then begin
  help, cat, /st
  print, Nentries, (100.0 * total(/INT, cat.not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'
endif
return, cat
end


;=============================================================================
; Function to build a catalog from the FITS table produced by ACIS Extract 
; in the COLLATED_FILENAME stage.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_AE_cat, collated_fn, match_events_fn

tb = mrdfits(collated_fn, 1)

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, tb.RA, tb.DEC, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The (EX_DATA,EY_DATA) from AE are merely statistical errors for the mean event position within the extraction region.  
;; Systematic errors (e.g. the extraction region placed in the wrong location) are not represented.
;; These AE errors can get unrealistically small (e.g. 0.07 skypix for a 120 count source on-axis) which causes us to miss matches that are clearly legitimate.
;; We choose to arbitrarily add in 0.2" error to ACIS positions to deal with this.
systematic_error = 0.2  ; arcsec
X_ERR = sqrt(tb.ERR_DATA^2 + systematic_error^2) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR


;; Evaluate if the reported position is suspect.
not_fiducial = bytarr(Nentries)


;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL']

entry = create_struct(tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B)
cat   = replicate(entry, Nentries)

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)

ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a standard NOMAD catalog in FITS format obtained through Vizier.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_NOMAD_cat, catalog_fn, match_events_fn

tb = mrdfits(catalog_fn, 1)

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, tb.RAJ2000, tb.DEJ2000, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of mas.
X_ERR = (tb.E_RAJ2000/1000.) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = (tb.E_DEJ2000/1000.) / 0.492 ; (0.492 arcsec/skypix)

; It's hard to believe, but all the 2MASS sources in NOMAD report zero for the position error.
; Thus, we're going to assign the median 2MASS position error that we found in the M17 field (0.16 skypix)
; when errors are missing in NOMAD.
default_err = 0.16 ;skypix
ind = where(X_ERR EQ 0, count)
if (count GT 0) then begin
  X_ERR[ind] = default_err
  print, count, default_err, F='(%"%d missing X-position errors were assumed to be %0.2f skypix")'
endif
ind = where(Y_ERR EQ 0, count)
if (count GT 0) then begin
  Y_ERR[ind] = default_err
  print, count, default_err, F='(%"%d missing Y-position errors were assumed to be %0.2f skypix")'
endif



;; Evaluate the complex criteria which seem to be involved in deciding if the reported position is suspect.
temp = tb.Xflags
;temp[where(temp EQ '')] = '0'
Xflags = lonarr(Nentries)
reads, temp, Xflags, F='(Z)'


;; The NOMAD documentation at Vizier and email with Dr. Levine on the NOMAD team indicate
;; that we should be able to simply use the column "R" ("recommended astrometric standard") 
;; in the catalog to decide if a source is clean or not. 
;; However, only a few percent of the NOMAD sources seem to be "recommended".
;; Thus, we choose to roll our own quality metric based on our interpretation of the NOMAD flags.

; Bit flags in Xflags column, shown in hex.
;  00001 = UBBIT   : Fails Blaise's test for USNO-B1.0 star
;  00002 = TMBIT   : Fails Roc's test for clean 2MASS star
;  00004 = YB6     : Included in YB6 catalog (Y)
;  00008 = 2MASS   : Included in 2MASS catalog (M)
;  00010 = TYBIT   : Astrometry comes from Tycho2 catalog (T)
;  00020 = XRBIT   : Alternative correlations for same (RA,Dec)
;  00040 = ITMBIT  : Alternative correlations for same 2MASS ID
;  00080 = IUCBIT  : Alternative correlations for same UCAC-2 ID
;  00100 = ITYBIT  : Alternative correlations for same Tycho2 ID
;  00200 = OMAGBIT : Blue magnitude from O (not J) plate (o)
;  00400 = EMAGBIT : Red magnitude from E (not F) plate (e)
;  00800 = TMONLY  : Object found only in 2MASS catalog (M)
;  01000 = HIPAST  : Ast from Hipparcos catalog (H)
;  02000 = SPIKE   : USNO-B1.0 diffraction spike bit set
;  04000 = TYCONF  : Tycho2 confusion flag set
;  08000 = BSCONF  : Bright star has nearby faint source
;  10000 = BSART
  
UBBIT  = (Xflags AND '00001'X ) NE 0
TMBIT  = (Xflags AND '00002'X ) NE 0
XRBIT  = (Xflags AND '00020'X ) NE 0
ITMBIT = (Xflags AND '00040'X ) NE 0
IUCBIT = (Xflags AND '00080'X ) NE 0
ITYBIT = (Xflags AND '00100'X ) NE 0
TYCONF = (Xflags AND '04000'X ) NE 0
SPIKE  = (Xflags AND '02000'X ) NE 0
TYCONF = (Xflags AND '04000'X ) NE 0
BSCONF = (Xflags AND '08000'X ) NE 0
BSART  = (Xflags AND '10000'X ) NE 0

; Values of the single-character column "r" indicating the origin of the reported position.
;      B = USNO-B1.0 (Monet et al., Cat. I/284)
;      C = UCAC2 (Zacharias et al., Cat. I/289)
;      M = 2MASS catalog of point sources (Cutri et al., Cat. II/246)
;      Y = YB6 Catalog (USNO, unpublished)
;      T = Tycho-2 Catalog (Hog et al., 2000, Cat. I/259)
;      H = Hipparcos catalog (Table I/239/hip_main)
;      o = Palomar-I blue (O) plate (for Bmag)
;      e = Palomar-I red  (E) plate (for Rmag)

fromUSNOB = (tb.R EQ 'B')      
from2MASS = (tb.R EQ 'M')      
fromTYCHO = (tb.R EQ 'T')      

bad1 = fromUSNOB AND (UBBIT or SPIKE)

bad2 = from2MASS AND TMBIT

bad3 = fromTYCHO AND TYCONF

not_fiducial = bad1 OR bad2 OR bad3 OR BSCONF OR BSART OR XRBIT OR ITMBIT OR IUCBIT OR ITYBIT

CATALOG_NAME = 'NOMAD'


;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)


ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end



;=============================================================================
; Function to read any VizieR catalog in FITS format with the computed position columns _RAJ2000 and _DEJ2000.

; Supply the catalog name via the NAME input---to be used in ds9 region tags.

; Supply strings containing IDL expressions for the position errors, in units of arcseconds, via RA_ERROR_EXPRESSION and DEC_ERROR_EXPRESSION.  In those expressions, the catalog structure array is named "tb".

; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_vizier_cat, catalog_fn, match_events_fn, RA_ERROR_EXPRESSION=ra_error_expression, DEC_ERROR_EXPRESSION=dec_error_expression, NAME=cat_name

tb = mrdfits(catalog_fn, 1)

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, tb._RAJ2000, tb._DEJ2000, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
if NOT execute('X_ERR='+ ra_error_expression) then message,  'RA_ERROR_EXPRESSION cannot be evaluated.'
if NOT execute('Y_ERR='+dec_error_expression) then message, 'DEC_ERROR_EXPRESSION cannot be evaluated.'

X_ERR /= 0.492 ; (0.492 arcsec/skypix)
Y_ERR /= 0.492 ; (0.492 arcsec/skypix)


;; Evaluate if the reported position is suspect.
not_fiducial = bytarr(Nentries)
      
CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'vizier_cat' 


;; Build a suitable structure.
tag_names   = ['X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end



;=============================================================================
; Function to read a standard 2MASS catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

; The match_events_fn input provides a definition for the sky coordinate system.
;
; Photometry is converted to the CIT color system.
; http://www.astro.caltech.edu/~jmc/2mass/v3/transformations/
; http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec6_4b.html

;=============================================================================
FUNCTION build_2mass_cat, catalog_fn, match_events_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn


; The 2MASS catalog has lots of columns, but readcol.pro can handle only 25, so we omit the last columns which happen to be unimportant.
column_names = ['ra','dec','clon','clat','err_maj','err_min','err_ang','designation',$
                'j_m','j_cmsig','j_msigcom','j_snr',$
                'h_m','h_cmsig','h_msigcom','h_snr',$
                'k_m','k_cmsig','k_msigcom','k_snr',$
                'ph_qual','rd_flg','bl_flg','cc_flg','ndet']  ;,'gal_contam','mp_flg']

column_formats = 'D,D,A,A,D,D,I,A,'+$
                 'D,D,D,D,'+$
                 'D,D,D,D,'+$
                 'D,D,D,D,'+$
                 'A,A,A,A,A'  ;,I,I'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed -e ''s/null/NaN/g'' -e ''s/ - /NaN/g'' %s > %s")')

cmd = "readcol, 'temp.txt'," + strjoin(column_names,',') + ', F=column_formats, COMMENT="\"'
print, cmd
dum=execute(cmd)

;forprint, ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)


;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The err_maj and err_min columns are error ellipse axes in arcseconds which we choose to average.
X_ERR = ((err_maj+err_min)/2) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR

;; Transform 2mass photometry to CIT
;; Code derived by Kosta in /bulk/tecumseh1/patb/projects/M17/data/Broos2007/counterparts/Kosta_analysis/calculate_avlbolmass_2mass.pro
JH_cit = 0.92*(j_m-h_m) + 0.043
HK_cit = (h_m-k_m) - 0.034
JK_cit = 0.936*(j_m-k_m) + 0.0187

K_cit = k_m + 0.019 - 0.000936*(j_m-k_m)
J_cit = k_m + 0.935*(j_m-k_m) + 0.0187
H_cit = k_cit + (h_m-k_m) - 0.034

CATALOG_NAME = 'TWOMASS'

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR', 'CATALOG_NAME', 'J_cit', 'H_cit', 'K_cit', 'JH_cit', 'HK_cit', 'JK_cit', column_names]
tag_formats = 'J,F,F,F,F,A,F,F,F,F,F,F,'                 +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end




;=============================================================================
; Function to read a GLIMPSE catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; NOTE that the catalog columns assumed here are NOT the standards ones in Gator
; because readcol.pro can accept only 25 columns.
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

;;; See http://data.spitzer.caltech.edu/popular/glimpse/20050415_enhanced_v1/Documents/glimpse_dataprod_v1.5.pdf for formats.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_glimpse_cat, catalog_fn, match_events_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn


column_names = ['designation','ra','dec','dra','ddec','csf',$
                'mag_J','dJ_m',$
                'mag_H','dH_m',$
                'mag_Ks','dKs_m',$
                'mag3_6','d3_6m',$
                'mag4_5','d4_5m',$
                'mag5_8','d5_8m',$
                'mag8_0','d8_0m',$
                'sqf_3_6','sqf_4_5','sqf_5_8','sqf_8_0']

column_formats = 'A,D,D,F,F,I,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'I,I,I,I'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/null/NaN/g'' %s > %s")')
cmd = string(strjoin(column_names,','), column_formats, F='(%"readcol, ''temp.txt'', %s, F=''X,%s'' ")') + ', COMMENT="\"' 
print, cmd
dum=execute(cmd)

;forprint, designation,ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)

;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1


;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of arcsec.
X_ERR = (dra)  / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = (ddec) / 0.492 ; (0.492 arcsec/skypix)

CATALOG_NAME = 'GLIMPSE'

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','CATALOG_NAME',column_names]
tag_formats = 'J,F,F,F,F,A,'                   +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end



;=============================================================================
; Function to read an MSX catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; NOTE that the catalog columns assumed here are NOT the standards ones in Gator
; because readcol.pro can accept only 25 columns.
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

;;; See http://irsa.ipac.caltech.edu/applications/Gator/GatorAid/MSX/readme.html for formats.
; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_msx_cat, catalog_fn, match_events_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn


column_names = ['name','ra','dec','ePos','xPos','posPA', $
                'B1','q_B1','e_B1',$
                'B2','q_B2','e_B2',$
                'A','q_A','e_A',$
                'C','q_C','e_C',$
                'D','q_D','e_D',$
                'E','q_E','e_E' ]

column_formats = 'A,D,D,F,F,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/-99.0/NaN/g'' %s > %s")')
cmd = string(strjoin(column_names,','), column_formats, F='(%"readcol, ''temp.txt'', %s, F=''%s'' ")') + ', COMMENT="\"' 
print, cmd
dum=execute(cmd)

;forprint, designation,ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)

;; Convert celestial to the ACIS tangent plane.

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, ra, dec, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1


;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of arcsec.
;; I'm just averaging the in-scan and cross-scan uncertainties, rather than trying
;; to project them onto celestial coordinates.
X_ERR = mean([ePos,xPos]) / 0.492 ; (0.492 arcsec/skypix)
Y_ERR = X_ERR

CATALOG_NAME = 'MSX'

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','CATALOG_NAME',column_names]
tag_formats = 'J,F,F,F,F,A,'                   +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a Galactic Plane Survey "gpsSource" UKIDSS catalog in FITS format obtained via http://surveys.roe.ac.uk:8080/wsa/region_form.jsp

; The number of result rows written to files is limited and depends on how many columns have been requested i.e.
;   maximum rows written to file = nint(15000 / no. columns) x 1000
; To avoid hard limits on the catalog size, the following subset of the table columns is requested, via the "Select" field:
;   SOURCEID,CUEVENTID,FRAMESETID,RA,DEC,SIGRA,SIGDEC,EPOCH,MURA,MUDEC,SIGMURA,SIGMUDEC,CHI2,NFRAMES,CX,CY,CZ,HTMID,L,B,LAMBDA,ETA,PRIORSEC,JMHPNT,JMHPNTERR,HMK_1PNT,HMK_1PNTERR,H2_1MK_1PNT,H2_1MK_1PNTERR,JMHEXT,JMHEXTERR,HMK_1EXT,HMK_1EXTERR,H2_1MK_1EXT,H2_1MK_1EXTERR,MERGEDCLASSSTAT,MERGEDCLASS,PSTAR,PGALAXY,PNOISE,PSATURATED,JHALLMAG,JHALLMAGERR,JPETROMAG,JPETROMAGERR,JPSFMAG,JPSFMAGERR,JSERMAG2D,JSERMAG2DERR,JAPERMAG3,JAPERMAG3ERR,JAPERMAG4,JAPERMAG4ERR,JAPERMAG6,JAPERMAG6ERR,JGAUSIG,JELL,JPA,JERRBITS,JDEBLEND,JCLASS,JCLASSSTAT,JPPERRBITS,JSEQNUM,JOBJID,JXI,JETA,HHALLMAG,HHALLMAGERR,HPETROMAG,HPETROMAGERR,HPSFMAG,HPSFMAGERR,HSERMAG2D,HSERMAG2DERR,HAPERMAG3,HAPERMAG3ERR,HAPERMAG4,HAPERMAG4ERR,HAPERMAG6,HAPERMAG6ERR,HGAUSIG,HELL,HPA,HERRBITS,HDEBLEND,HCLASS,HCLASSSTAT,HPPERRBITS,HSEQNUM,HOBJID,HXI,HETA,K_1HALLMAG,K_1HALLMAGERR,K_1PETROMAG,K_1PETROMAGERR,K_1PSFMAG,K_1PSFMAGERR,K_1SERMAG2D,K_1SERMAG2DERR,K_1APERMAG3,K_1APERMAG3ERR,K_1APERMAG4,K_1APERMAG4ERR,K_1APERMAG6,K_1APERMAG6ERR,K_1GAUSIG,K_1ELL,K_1PA,K_1ERRBITS,K_1DEBLEND,K_1CLASS,K_1CLASSSTAT,K_1PPERRBITS,K_1SEQNUM,K_1OBJID,K_1XI,K_1ETA
;
; UKIDS column defintions can be found at http://surveys.roe.ac.uk/wsa/www/wsa_browser.html
; The table type (e.g. gpsSource) returned by the query is shown in the FITS header.

; The match_events_fn input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_UKIDSS_cat, catalog_fn, match_events_fn

tb = mrdfits(catalog_fn, 1)

; Convert celestial coordinates from radians to degrees.
tb.RA  *= !RADEG
tb.DEC *= !RADEG

; This source database has not been cleaned much.  
; Following the example in Appendix 3 of Lucas et al., MNRAS 391, 136-163 (2008), 
; we are going to remove here these really serious problems:
;   - detections classified as noise
;   - secondary detections of the same source
PriOrSec   = strtrim(tb.PriOrSec,2)
framesetID = strtrim(tb.framesetID,2)
ind = where((tb.mergedClass NE 0) AND ((PriOrSec EQ '0') OR (PriOrSec EQ framesetID)))
tb = tb[ind]

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
theader = headfits(match_events_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + match_events_fn

; Build astrometic structure from data header.
tbinfo, theader, tb_str
colnames = strlowcase( strtrim(tb_str.ttype,2) )
wcs_object, xwcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'x')
wcs_object, ywcs, /INIT, HEADER=theader, COLNUM=1+where(colnames EQ 'y')

wcs_object, xwcs, /GET, CTYP=ctypeX, CRVL=crvalX, CRPX=crpixX, CDLT=cdeltX
wcs_object, ywcs, /GET, CTYP=ctypeY, CRVL=crvalY, CRPX=crpixY, CDLT=cdeltY

make_astr, event2wcs_astr, DELTA=[cdeltX,cdeltY], CTYPE=[ctypeX,ctypeY], $
                           CRPIX=[crpixX,crpixY], CRVAL=[crvalX,crvalY]

ad2xy, tb.RA, tb.DEC, event2wcs_astr, x, y
; Must add 1 because ad2xy is thinking of 0-based IDL array indexes.
X  = X+1
Y  = Y+1

;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of degrees.
X_ERR = 0 > ((tb.sigRA *3600.) / 0.492) ; (0.492 arcsec/skypix)
Y_ERR = 0 > ((tb.sigDec*3600.) / 0.492) ; (0.492 arcsec/skypix)

; In Data Release 2 position errors are missing, so we'll assign something arbitrarily.
default_err = 0.2 ;skypix
ind = where(X_ERR EQ 0, count)
if (count GT 0) then begin
  X_ERR[ind] = default_err
  print, count, default_err, F='(%"%d missing X-position errors were assumed to be %0.2f skypix")'
endif
ind = where(Y_ERR EQ 0, count)
if (count GT 0) then begin
  Y_ERR[ind] = default_err
  print, count, default_err, F='(%"%d missing Y-position errors were assumed to be %0.2f skypix")'
endif


;; Evaluate if the reported position is suspect.
;; Some important flag bit definitions are at http://surveys.roe.ac.uk/wsa/ppErrBits.html
not_fiducial = bytarr(Nentries)

CATALOG_NAME = 'UKIDSS'


;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)


ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end

