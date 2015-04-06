pro concat_evt_060222,filelist,event_str_sort,gti_str_sort,bad_str,sper=sper

;routine to read in a list of Swift events files, both PC and WT, 
;concatenate them into a single structure and resort the resulting 
;events structure in correct time order

;Form of the structure is:
;time,x,y,rawx,rawy,detx,dety,pha,pi,grade,status,ftype
;ftype=0:WT 1:PC

;input is filelist, a strarr of cleaned events file names
tag_descript_evt='D,I,I,I,I,I,I,J,J,I,B(2),I,I,I,F,F,F'              
tagnames_evt=['time','x','y','rawx','rawy','detx','dety','pha','pi','grade','status','ftype','fnum','pileup','fact1','fact2','fact3']
tag_descript_gti='D,D,I'
tagnames_gti=['start','stop','fnum']
tag_descript_bad='I,I,I,I,I,B(2),I'
tagnames_bad=['rawx','rawy','amp','type','yextent','badflag','fnum']

tot_evts=0L
tot_gtis=0L
tot_bads=0L
if keyword_set(sper) then bpext=5 else bpext=3
for n=0,n_elements(filelist)-1 do begin ; loop to determine how many elements the final evt structure will have
    data1=mrdfits(filelist(n),1,hd1)   ;reads in the events
    tot_evts=tot_evts+n_elements(data1)
    data2=mrdfits(filelist(n),2,hd2)   ;reads in the GTIs
    help,data2
    tot_gtis=tot_gtis+n_elements(data2)
    data3=mrdfits(filelist(n),bpext,hd3)   ;reads in the badpix
    tot_bads=tot_bads+n_elements(data3)
endfor

create_struct, event_str, 'event_str', tagnames_evt, tag_descript_evt,dimen=1
event_str = replicate(event_str,tot_evts)
create_struct, gti_str, 'gti_str', tagnames_gti, tag_descript_gti,dimen=1
gti_str = replicate(gti_str,tot_gtis)
create_struct, bad_str, 'bad_str', tagnames_bad, tag_descript_bad,dimen=1
bad_str = replicate(bad_str,tot_bads)

evt_ctr=0L
gti_ctr=0L
bad_ctr=0L
for n=0,n_elements(filelist)-1 do begin;loop over each input evt file and put the records into the total evt structure
    data0=mrdfits(filelist(n),0,hd0)   ;reads in the nullheader
    data1=mrdfits(filelist(n),1,hd1)   ;reads in the events
    data2=mrdfits(filelist(n),2,hd2)   ;reads in the GTIs
    data3=mrdfits(filelist(n),bpext,hd3)   ;reads in the badpix
    if sxpar(hd0,'DATAMODE') eq 'WINDOWED' then mode=0
    if sxpar(hd0,'DATAMODE') eq 'PHOTON  ' then mode=1
    m=0l
    while m lt n_elements(data1) do begin
;    for m=0,n_elements(data1)-1 do begin
        event_str(evt_ctr).time=data1(m).time
        event_str(evt_ctr).x=data1(m).x
        event_str(evt_ctr).y=data1(m).y
        event_str(evt_ctr).rawx=data1(m).rawx
        event_str(evt_ctr).rawy=data1(m).rawy
        event_str(evt_ctr).detx=data1(m).detx
        event_str(evt_ctr).dety=data1(m).dety
        event_str(evt_ctr).pha=data1(m).pha
        if not keyword_set(sper) then begin
           event_str(evt_ctr).pi=data1(m).pi
           event_str(evt_ctr).grade=data1(m).grade
        endif else  event_str(evt_ctr).grade=0
        event_str(evt_ctr).status=data1(m).status
        event_str(evt_ctr).ftype=mode
        event_str(evt_ctr).fnum=n
        event_str(evt_ctr).pileup=0
        event_str(evt_ctr).fact1=1.
        event_str(evt_ctr).fact2=1.
        event_str(evt_ctr).fact3=1.
                
        evt_ctr=evt_ctr+1L
        m=m+1l
    endwhile
;    endfor
    for m=0,n_elements(data2)-1 do begin
        gti_str(gti_ctr).start=data2(m).start
        gti_str(gti_ctr).stop=data2(m).stop
        gti_str(gti_ctr).fnum=n
        gti_ctr=gti_ctr+1L
    endfor
    for m=0,n_elements(data3)-1 do begin
        bad_str(bad_ctr).rawx=data3(m).rawx
        bad_str(bad_ctr).rawy=data3(m).rawy
        bad_str(bad_ctr).amp=data3(m).amp
        bad_str(bad_ctr).type=data3(m).type
        bad_str(bad_ctr).yextent=data3(m).yextent
        bad_str(bad_ctr).badflag=data3(m).badflag
        bad_str(bad_ctr).fnum=n
        bad_ctr=bad_ctr+1L
    endfor
endfor

;Now sort the structure so records are monotonically increasing
evt_str_sort_ind=sort(event_str.time)
event_str_sort=event_str(evt_str_sort_ind)
gti_str_sort_ind=sort(gti_str.start)
gti_str_sort=gti_str(gti_str_sort_ind)

;check the output
;!p.multi=[0,2,2]
;plot,event_str_sort.time,yrange=[min(event_str_sort.time),max(event_str_sort.time)],psym=3
;plot,gti_str_sort.start,yrange=[min(event_str_sort.time),max(event_str_sort.time)],psym=3
;plot,gti_str_sort.stop,yrange=[min(event_str_sort.time),max(event_str_sort.time)],psym=3
;plot,gti_str_sort.stop-gti_str_sort.start,yrange=[1,3000],psym=3,/ylog

;event_str_sort=event_str_sort(0:25000)
;gti_str_sort=gti_str_sort(0:380)
;bad_str
end
    
