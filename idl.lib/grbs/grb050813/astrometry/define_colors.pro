function color24, r, g, b
return, r + g*256l + b*256l^2
end

function color16, r, g, b
; don't understand why this works, but it seems to (for me)
return, color24(r,g,b)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function define_colors

common define_colors, colors

; stolen from tek_color
r = bytscl([ 0,100,100,0,0,0,100,100,100,60,0,0,55,100,33,67, $
	100,75,45,17,25,50,75,100,67,40,17,17,17,45,75,90])
g = bytscl([ 0,100,0,100,0,100,0,100,50,83,100,50,0,0,33,67, $
	100,100,100,100,83,67,55,33,90,90,90,67,50,33,17,9])
b = bytscl([ 0,100,0,0,100,100,83,0,0,0,60,100,83,55,33,67, $
	33,45,60,75,83,83,83,90,45,55,67,90,100,100,100,100])

if !d.n_colors gt 1.6d7 then begin
    ; True color
    colors=create_struct(name='define_colors', $
                         'white',color24(255,255,255), $
                         'black',color24(0,0,0), $
                         'plot',color24(255,255,255), $
                         'red',color24(255,0,0), $
                         'green',color24(0,255,0), $
                         'blue',color24(0,0,255), $
                         'cyan',color24(0,255,255), $
                         'magenta',color24(255,0,255), $
                         'yellow',color24(255,255,0), $
                         'orange',color24(r[8],g[8],b[8]), $
                         'dkgreen',color24(r[9],g[9],b[9]), $
                         'seagreen',color24(r[10],g[10],b[10]), $
                         'ltblue',color24(r[11],g[11],b[11]), $
                         'purple',color24(r[12],g[12],b[12]), $
                         'hotpink',color24(r[13],g[13],b[13]), $
                         'dkgray',color24(r[14],g[14],b[14]), $
                         'ltgray',color24(r[15],g[15],b[15]), $
                         'gray',color24(r[15],g[15],b[15]), $
                         'ltyellow',color24(r[16],g[16],b[16]), $
                         'lily',color24(r[17],g[17],b[17]), $
                         'clover',color24(r[18],g[18],b[18]), $
                         'glacier',color24(r[19],g[19],b[19]), $
                         'babyblue',color24(r[20],g[20],b[20]), $
                         'heather',color24(r[21],g[21],b[21]), $
                         'lilac',color24(r[22],g[22],b[22]), $
                         'pink',color24(r[23],g[23],b[23]), $
                         'fens',color24(r[24],g[24],b[24]), $
                         'ltgreen',color24(r[25],g[25],b[25]), $
                         'aquamarine',color24(r[26],g[26],b[26]), $
                         'medblue',color24(r[27],g[27],b[27]), $
                         'sky',color24(r[28],g[28],b[28]), $
                         'mauve',color24(r[29],g[29],b[29]), $
                         'ltpurple',color24(r[30],g[30],b[30]), $
                         'ltmagenta',color24(r[31],g[31],b[31]))

endif else if !d.n_colors gt 6.5d4 then begin
    ; 16-bit color
    colors=create_struct(name='define_colors', $
                         'white',color16(255,255,255), $
                         'black',color16(0,0,0), $
                         'plot',color24(255,255,255), $
                         'red',color16(255,0,0), $
                         'green',color16(0,255,0), $
                         'blue',color16(0,0,255), $
                         'cyan',color16(0,255,255), $
                         'magenta',color16(255,0,255), $
                         'yellow',color16(255,255,0), $
                         'orange',color16(r[8],g[8],b[8]), $
                         'dkgreen',color16(r[9],g[9],b[9]), $
                         'seagreen',color16(r[10],g[10],b[10]), $
                         'ltblue',color16(r[11],g[11],b[11]), $
                         'purple',color16(r[12],g[12],b[12]), $
                         'hotpink',color16(r[13],g[13],b[13]), $
                         'dkgray',color16(r[14],g[14],b[14]), $
                         'ltgray',color16(r[15],g[15],b[15]), $
                         'gray',color16(r[15],g[15],b[15]), $
                         'ltyellow',color16(r[16],g[16],b[16]), $
                         'lily',color16(r[17],g[17],b[17]), $
                         'clover',color16(r[18],g[18],b[18]), $
                         'glacier',color16(r[19],g[19],b[19]), $
                         'babyblue',color16(r[20],g[20],b[20]), $
                         'heather',color16(r[21],g[21],b[21]), $
                         'lilac',color16(r[22],g[22],b[22]), $
                         'pink',color16(r[23],g[23],b[23]), $
                         'fens',color16(r[24],g[24],b[24]), $
                         'ltgreen',color16(r[25],g[25],b[25]), $
                         'aquamarine',color16(r[26],g[26],b[26]), $
                         'medblue',color16(r[27],g[27],b[27]), $
                         'sky',color16(r[28],g[28],b[28]), $
                         'mauve',color16(r[29],g[29],b[29]), $
                         'ltpurple',color16(r[30],g[30],b[30]), $
                         'ltmagenta',color16(r[31],g[31],b[31]))
endif else begin
    ; Indexed Color
    tek_color
    colors=create_struct(name='define_colors', $
                         'white',255l, $
                         'black',0l, $
                         'plot',255l, $
                         'red'  ,2l, $
                         'green',3l, $
                         'blue' ,4l, $
                         'cyan' ,5l, $
                         'magenta' ,6l, $
                         'yellow' ,7l, $
                         'orange' ,8l, $
                         'dkgreen' ,9l, $
                         'seagreen' ,10l, $
                         'ltblue' ,11l, $
                         'purple' ,12l, $
                         'hotpink' ,13l, $
                         'dkgray' ,14l, $
                         'ltgray' ,15l, $
                         'gray'   ,15l, $
                         'ltyellow' ,16l, $
                         'lily'   ,17l, $
                         'clover' ,18l, $
                         'glacier',19l, $
                         'babyblue',20l, $
                         'heather' ,21l, $
                         'lilac'   ,22l, $
                         'pink'    ,23l, $
                         'fens'    ,24l, $
                         'ltgreen' ,25l, $
                         'aquamarine' ,26l, $
                         'medblue' ,27l, $
                         'sky'     ,28l, $
                         'mauve'   ,29l, $
                         'ltpurple',30l, $
                         'ltmagenta' ,31l)
endelse

; Default plot color is white, so switch if necessary
if colors.plot eq !p.background then colors.plot=colors.black

return,colors
end
