Pro target_table

;This program will update the CUBIC observation plan data table, in the Obs_Plan.html file, using
; an ASCII file(obs_plans.txt). 

						;Assigning global variables to the file names used.

temp_htmlfile = 'Temp.html'

asciifile = pickfile(path='/pkg/xray/cubic/progs/planning/Obs_Plans/', /read, filter= '*.txt', $
	TITLE='Select input text file', /NOCONFIRM)

htmlfile = pickfile(path='/bulk/www/xray/cubic/targets/',/read, filter= '*.html', $
	TITLE='Select HTML file for modification', /NOCONFIRM)	

openr, Homepage, htmlfile,/GET_LUN	        ;Open existing html file

openw, Temppage, temp_htmlfile,/GET_LUN		;Open a temp html file inorder to manipulate 
						; code prior to updating existing html file.
Code = ''					;One line string variable.

Flag = '<!- Insert Table Here ->'		;Comment to indicate where table starts in
						; existing html file.
EndFlag = '<!- Table Ends Here ->'		;Comment for end of table.

num_fields = 8					;Number of fields/colums existing in table.

REPEAT BEGIN				;Read existing html file until comment(Flag) indicates
					; where code for table should be, while coping it to
	readf, Homepage, Code		; the temp html file.
	
	printf, Temppage, Code

	IF EOF(Homepage) THEN BEGIN		;Checks to see if correct file was selected, if not 
						; discards temp html file and stops execution.
		spawn, 'rm '+temp_htmlfile

		stop, 'The wrong file was selected!'

	ENDIF

ENDREP UNTIL Code eq Flag

n = numlines(asciifile)		  	;Count the number of lines in ascii file with pre-existing
					; function numlines.
s = strarr(n)				;Array of strings equal to the number of lines in ascii file.

s2 = strarr(n,num_fields)		;Array of strings equal to the number of lines in ascii file
					; by the number of fields.
openr, txt, asciifile,/GET_LUN		;Open ascii file.

readf, txt, s				;Read each line of text file into string array, s.

FOR line_num = 0, n-1 DO BEGIN		;Loop through all the lines of the ascii file that are in s.

	line = s(line_num)

	start = 0			;Position variable indicating the begining of each field.
	
	FOR field_num = 0, num_fields-1 DO BEGIN	;Loop through each line of text

		del = STRPOS(line,'|', start)		; finding each delimiter('|') in that line,

		length = del - start			; determine the length of the field,

		temp = STRMID(line,start,length)	; extracting each field from the line,

		field = STRTRIM(temp,2)			; trimming off extra blanks from each field,

		s2(line_num,field_num) = field		; and placing each field into s2 for use in 
							; creating the new table in the html file.
		start = del  + 1			;Start of the next field = end of the last 
							; one in that line.
	ENDFOR

ENDFOR

FOR html_line = 0, n-1 DO BEGIN			;Loop through s2, placing the appropriate field
						; into the html code which creates the data table
	printf, Temppage, '<tr>'		; and places it into the temp html file.

		FOR html_fields = 0, num_fields-1 DO BEGIN
		
			printf, Temppage,$
			 '      <td align=center>' +$
			 s2(html_line,html_fields)+ '</td>'

		ENDFOR

	printf, Temppage, '</tr>'

ENDFOR

flush, Temppage

REPEAT BEGIN				;Read over the existing code for the table until 
					; comment(EndFlag) indicates where code for the table
	readf, Homepage, Code		; should end.

ENDREP UNTIL Code eq EndFlag

printf, Temppage, Code			;Prints EndFlag into the temp html file.

REPEAT BEGIN				;Read existing html file until end of file while writing to
					; temp html file.
	readf, Homepage, Code
	
	printf, Temppage, Code

ENDREP UNTIL EOF (Homepage)

CLOSE, txt

flush, Homepage

flush, Temppage

CLOSE, Homepage

CLOSE, Temppage

command = 'mv ' +temp_htmlfile+ ' ' +htmlfile	;Tells the unix to copyover the existing html file with 
						; the temp html file and then remove it.
spawn, command

END

