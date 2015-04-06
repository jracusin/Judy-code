;
;******************************* COPYRIGHT 1992 ********************************
;		The Regents of the University of California::
;	This software was produced under a U.S. Government contract
;	(w-7405-eng-36) by the Los Alamos National Laboratory, which is
;	operated by the University of California for the United States
;	Department of Energy. The U.S. Government is licensed to use,
;	reproduce, and distribute this software. Permission is granted
;	to the public to copy and use this software without charge,
;	provided that this notice and any statement of authorship are
;	reproduced on all copies. Neither the Government nor the
;	University makes any warranty, express or implied, or assumes
;	any liability or responsibility for the use of this software.
;*******************************************************************************
;+
; NAME:		nn_learn.pro
;
; PURPOSE:	Learning step to cluster data using neural network techniques.
;
; CATEGORY:	Data processing - cluster analysis
;
; CALLING SEQUENCE:	weights = nn_learn(data,max_val,[BLR=blr],[ELR=elr],
;	[N_EVENT=n_event],[N_PASS=n_pass],[N_PARAM=n_param],[N_CLUST=n_clust]
;
; INPUTS:	data = data set to be clustered, max_val = maximum range of each
;		parameter in the data set (to normalize the weights).
;
; OPTIONAL INPUT PARAMETERS:
;	blr = begining learning rate, elr = ending learning rate
;	n_pass = # of iterations of the learning pass
;	n_param = # of parameters in data set - 1st dimension of data set
;	n_event = # of events in learning set - 2nd dimension of data set
;	n_clust = # of clusters to find - arbitrary (?)
;
; KEYWORD PARAMETERS:	none
;
; OUTPUTS:	result = an array of weights describing the cluster centers.
;
; OPTIONAL OUTPUT PARAMETERS:	none
;
; COMMON BLOCKS:	none
;
; SIDE EFFECTS: This approach has an inherent weakness in that it must be
;		set to find a specific number of clusters; It will find
;		that number of clusters in the data set - regardless.
;
; RESTRICTIONS:	This routine has not been rigorously tested on different types
;		of data. It "appears" to work on flow cytometry data.
;
; PROCEDURE:	A limited subset of a larger data set is presented to this
;		routine as a training set to condition the neural network. The
;		result is a set of weights which describe the centers of the
;		resolved clusters.
;
; MODIFICATION HISTORY:	Initial algorithm: Mark Naiver (Univ of Texas - Austin)
;	Date last modified ==>  1 March 93 : RCH [LANL]
;	Contact: Robb Habbersett (505/667-0296 or robb@big-geek.lanl.gov)
;-
;-------------------------------------------------------------------------------
function NN_LEARN,data,max_val,blr=blr,elr=elr,n_pass=n_pass,n_param=n_param,$
                  n_clust=n_clust,n_event=n_event

  if n_elements(data) eq 0 then begin ;do some error checking
     print,'No data received!?!'
     return,-1
  endif

  d_dimens = size(data)			;check data array dimensions
  if d_dimens(0) ne 2 then begin
     print,'Data array must have two dimensions!'
     return,-1
  endif

;	check on optional parameters
  if n_elements(n_param) eq 0 then n_param = d_dimens(1)
  if n_elements(n_event) eq 0 then n_event = d_dimens(2)
  if n_elements(n_clust) eq 0 then n_clust = n_param +1
  if n_elements(n_pass) eq 0 then n_pass = 50
  if n_elements(blr) eq 0 then blr = 0.5
  if n_elements(elr) eq 0 then elr = 0.1
  if n_elements(max_val) ne n_param then begin
     print,"Max_val doesn't fit data array!"
     return, -1
  endif

  learn_rate = blr              ;initial 'learning rate'
  delta_learn = (blr - elr)/n_pass
  one_row = replicate(1.,1,n_param)     ;these arrays permit matrix...
  one_col = replicate(1.,1,n_clust)     ;multiplication instead of loops
  weights = randomu(s,n_param,n_clust)	;random cluster weights

  count = intarr(n_clust)       ;Keep track of how often each cluster "wins"

  if widget_info(/active) eq 1 then widget_control,/hourglass else $
     print,'Clustering learning set - please wait!'

  for i=0, n_clust-1 do $		; Normalize the weights
     weights(*,i) = (weights(*,i)/total(weights(*,i))) * max_val

  for j=1, n_pass do begin
     lse_error = 0.
     for event = 0, n_event-1 do begin
;	    vector from current event to all cluster centers(weights)
        vector = data(*,event) # one_col - weights
;	    Calculate distance from current event to all cluster centers
        distance = one_row # abs(vector)
;	    Winner is the cluster that is closest to the event
        winner = where(distance eq min(distance))
;	    Update the count for each cluster
        count(winner) = count(winner) + 1
;	    Update the error value. (Distance squared)
        lse_error = lse_error + distance(winner) ^ 2.
;	    Do not update ALL weights, just the winner's weights
        weights(*,winner)=learn_rate * vector(*,winner) + weights(*,winner)
     endfor                                     ;endfor each event
     learn_rate = learn_rate - delta_learn		;Update learning rate
  endfor                                        ;endfor each pass

  print,'Cluster counts:' & print, count
  return, weights
end                             ;end NN_LEARN
;

