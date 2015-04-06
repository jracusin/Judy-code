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
; NAME:		nn_clust.pro
;
; PURPOSE:	tag events with cluster #
;
; CATEGORY:	data processing - cluster analysis
;
; CALLING SEQUENCE:	result = NN_CLUST(data,weights,[N_PARAM=n_param],
;			[N_EVENT=n_event],[N_CLUST=n_clust])
;
; INPUTS: data = data set to be clustered, weights = array of weights which
;	  describe cluster centers (produced by the function NN_LEARN)
;
; OPTIONAL INPUT PARAMETERS:
;	n_param = # of parameters in data set - 1st dimension of data set
;	n_event = # of events in data set - 2nd dimension of the data set
;	n_clust = # of clusters to find - arbitrary
;
; KEYWORD PARAMETERS:	none
;
; OUTPUTS:	result = integer array of cluster assignment of each event
;
; OPTIONAL OUTPUT PARAMETERS:	none
;
; COMMON BLOCKS:	none
;
; SIDE EFFECTS:	none (?)
;
; RESTRICTIONS:	Read the header of 'nn_learn.pro'.
;
; PROCEDURE: The data set is processed against the weights array and a cluster
;	     # assignment is made for each event in the input data set based
;	     on the weights array which describe the center of each cluster.
;
; MODIFICATION HISTORY:	initial algorithm: Mark Naiver (Univ of Texas - Austin)
;	Date last modified ==>  1 March 93 : RCH [LANL]
;	Contact: Robb Habbersett (505/667-0296 or robb@big-geek.lanl.gov)
;-
;-------------------------------------------------------------------------------
function NN_CLUST,data,weights,n_param=n_param,n_clust=n_clust,n_event=n_event

;	IDL routine to cluster data using neural network techniques.
;	The array 'weights', created in the procedure 'nn_learn',
;	is used by this procedure to tag each data group with a cluster #.

  if n_elements(data) eq 0 then begin ;do some error checking
     print,'No data received!?!'
     return,-1
  endif

  w_dimens = size(weights)          ;weights array dimensions
  d_dimens = size(data)             ;data array dimensions

  if d_dimens(0) ne 2 then begin ;data set must be 2D
     print,'Data array must have two dimensions!'
     return,-1
  endif

  if n_elements(n_param) eq 0 then n_param = d_dimens(1)
  if n_elements(n_event) eq 0 then n_event = d_dimens(2)
  if n_elements(n_clust) eq 0 then n_clust = w_dimens(2)

;	set up processing arrays

  one_row = replicate(1.,1,n_param)     ;these arrays permit matrix...
  one_col = replicate(1.,1,n_clust)     ;multiplication instead of loops
  cluster_n = intarr(n_event)           ;array for cluster #s

  if widget_info(/active)ne 0 then widget_control,/hourglass else $
     print,'Processing data array - please wait!'
  
  for event = 0, n_event -1 do begin ;processing loop
;	  Vector from current event to all cluster centers(weights)
     vector = data(*,event) # one_col - weights
;	  Calculate distance from current event to all cluster centers
     distance = one_row # abs(vector)
;	  Winner is the cluster that is closest to the event
     cluster_n(event) = where(distance eq min(distance))
  endfor                        ;endfor each event

  print,'Subsets identified by neural net.'

  return,cluster_n
end                             ;end NN_CLUST
;

