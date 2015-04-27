pro brackets,teams,events

  g=create_struct('team_slot',0,'event',0,'round',0,'win',0,'lose',0)
  g=replicate(g,100)

  nteams=teams
  ngames=0
  k=0
  for i=0,events-1 do begin 
     nrounds=alog(nteams)/alog(2.)+1
     if i eq 2 then begin 
        nrounds=nrounds+1
        nteams=nteams*2
     endif 
     for j=0,nrounds-1 do begin 
        g[k:k+nteams-1].event=i+1
        g[k:k+nteams-1].team_slot=indgen(nteams)+k+1
        g[k:k+nteams-1].round=j+1+i
        k=k+nteams
        nteams=nteams/2.
     endfor
     nteams=teams/(2*(i+1))
  endfor 

  w=where(g.event ne 0,nw)
  g=g[w]
;  colprint,g.event,g.team_slot,g.round
  
  ;;; go through each team slot and figure out where that team goes if they
  ;;; win or lose

  ts=intarr(n_elements(g))
  for i=0,n_elements(g)-1 do begin
     ;;; if win
     w=where(g.event eq g[i].event and g.round eq g[i].round+1 and ts lt 2,nw)
     ts[w[0]]=ts[w[0]]+1
     g[i].win=g[w[0]].team_slot
     if g[i].round eq events+1 then g[i].win=-1
     ;;; if lose
     up=g[i].round-1
     l=where(g.event eq g[i].event+1+up and g.round eq g[i].round+1 and ts lt 2,nl)
     if nl gt 0 then begin 
        if g[i].round lt 3 then begin 
           ts[l[0]]=ts[l[0]]+1
           g[i].lose=g[l[0]].team_slot
        endif else g[i].lose=-1
     endif else g[i].lose=-1
  endfor 

  fname='~/Desktop/Personal/curling/bracket_'+ntostr(teams)+'teams_'+ntostr(events)+'events'
  begplot,name=fname+'.ps',/color
  !p.multi=[0,1,events]
  mround=max(g.round)
  p=0
  for i=0,events-1 do begin 
     for j=0,mround-1 do begin 
        w=where(g.event eq i+1 and g.round eq j+1,nw)
        if j eq 0 then plot,[0,mround],[0,teams/(i+1.)],/nodata,xrange=[0,mround],yrange=[0,teams/(i+1.)+1],/xsty,/ysty 
        for k=0,nw-1 do begin
           oplot,j+[0,1.],[k+0.5,k+0.5]*teams/nw/(i+1.)
           if k mod 2 eq 0 then oplot,j+[1.,1.],[k+0.5,k+1.5]*teams/nw/(i+1.)
           xyouts,j+0.15,(k+0.5)*teams/nw/(i+1.)+0.2,ntostr(p+1)+' (W'+ntostr(g[p].win)+',L'+ntostr(g[p].lose)+')',/data,charsize=0.5
           p=p+1
        endfor 
     endfor 
;     oplot,j+[0.2,0.8],[1.,1.]*teams/nw/(i+1.)
     legend,'Event '+ntostr(g[w[0]].event),box=0,/top,/right,charsize=1.
  endfor 

  endplot
  spawn,'ps2pdf '+fname+'.ps '+fname+'.pdf'

stop

     draws=alog(teams)/alog(2) ;; if 2^draws=teams (8,16,32,64)
     
  

  return
end 
pro bracket_nogo
  t.event[0]=1
  ;; draw 1
  even=indgen(teams/2)*2
  odd=indgen(teams/2)*2+1
  t[even].record[0]='w'
  t[odd].record[0]='l'
  w=where(t.record[0] eq 'w',nw)
  l=where(t.record[0] eq 'l',nl)
  t[w].event[1]=1
  t[l].event[1]=2

  ;; draw 2
  even=indgen(nw/2)*2
  odd=indgen(nl/2)*2+1
  t[w[even]].record[1]='w'
  t[w[odd]].record[1]='l'
  t[l[even]].record[1]='w'
  t[l[odd]].record[1]='l'
  ww=where(t.record[0] eq 'w' and t.record[1] eq 'w')
  t[ww].event[2]=1
  wl=where(t.record[0] eq 'w' and t.record[1] eq 'l')
  t[wl].event[2]=3
  lw=where(t.record[0] eq 'l' and t.record[1] eq 'w')
  t[lw].event[2]=2
  ll=where(t.record[0] eq 'l' and t.record[1] eq 'l')
  t[ll].event[2]=4

;;; win - win = a
;;; win - lose = c
;;; lose - win = b
;;; lose - lose = d

  ;;; display bracket
  w1=where(t.event[0] eq 1)
  plot,[0,1],[0,teams],/nodata
  xyouts,0.02,teams-(indgen(teams)+0.5),'team '+t[w1].team,/data
  for i=0,teams-1 do oplot,[0.1,0.2],[i+0.5,i+0.5]
  w2=where(t.event[1] eq 1)
end

pro curling_draw_schedule

;;; initial conditions
  teams=8
  sheets=4
  events=4
  draws=3
  ngames=draws*sheets
  dsets=teams/sheets/2.
  
;; teams
  t=create_struct('team','','club','','event',intarr(draws),'record',strarr(draws))
  t=replicate(t,teams)
  t.team=ntostr(indgen(teams))
  ;; record = 'w', 'l', 'o'

;;; games
  g=create_struct('team1','','team2','','sheet','','draw',0,'winner',0,'event',0,'status','')
  g=replicate(g,ngames)

;;; bracket
  
  



  stop
  return
end 
