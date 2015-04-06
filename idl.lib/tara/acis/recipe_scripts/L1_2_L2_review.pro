;;; Routine to execute the IDL parts of the L1->L2 recipe.

;;; $Id: L1_2_L2_review.pro 3110 2008-07-02 14:41:26Z patb $

@acis_extract_tools

PRO L1_2_L2_review


    compare_event_lists, 'acis.astrometric.evt1', 'acis.astrometric.calibrated.evt1'

    bt_asol = mrdfits('temp.asol1', 1, header_asol )
    bt_evt  = mrdfits('acis.astrometric.calibrated.clean.evt',   1, header_evt )
    time = bt_asol.TIME - sxpar(header_asol, 'TSTART')
    RA=bt_asol.RA  &  DEC=bt_asol.DEC  &  ROLL=bt_asol.ROLL
    title = 'TIME - TSTART'
    function_1d,id4, time, RA,   XTIT=title
    function_1d,id5, time, DEC,  XTIT=title
    function_1d,id6, time, ROLL, XTIT=title
    index = 0 > value_locate(bt_asol.TIME, bt_evt.TIME)
    print, max(abs((bt_asol.TIME)[index] - bt_evt.TIME)), F='(%"\nMaximum time between an event and an aspect sample is %0.1f seconds.")'
    
    old = mrdfits('acis.astrometric.calibrated.clean.evt'       ,1)
    new = mrdfits('acis.astrometric.calibrated.clean.subpix.evt',1)
    num_events = n_elements(new)
    if (n_elements(old) NE num_events) then message, 'STOP!  The number of events has changed!!!'
    dataset_1d,id7, new.X-old.X, XTIT='X, new-old', DENSITY_TITLE='Subpixel Adjustments' 
    dataset_1d,id8, new.Y-old.Y, XTIT='Y, new-old', DENSITY_TITLE='Subpixel Adjustments'
    
return
end

