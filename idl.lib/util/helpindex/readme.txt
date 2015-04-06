!!! This program only works with Unix. !!!

-----------------------------------------------------
Step 1.  Installation
-----------------------------------------------------
a) Single user installation:

After unpacking the files a directory named 
helpindex is created.  Add this directory to 
IDL's search path.  
For example (csh and sh syntax):

setenv IDL_PATH "<IDL_DEFAULT>":/home/borsholm/helpindex
export IDL_PATH="<IDL_DEFAULT>":/home/borsholm/helpindex

Add an alias to idlindex if you want to launch it from
the Unix shell (csh and sh syntax):

alias idlindex /home/borsholm/helpindex/index
alias idlindex=/home/borsholm/helpindex/index

b) Install for all users of the IDL installation 
 (root permissions required)

Move files into the idl distribution:

  mv *.gz /usr/local/rsi/idl_6.0/help
  mv index.pro /usr/local/rsi/idl_6.0/lib
  mv index /usr/local/rsi/idl_6.0/bin


Make symbolic link to launch from Unix shell:
  ln -s /usr/local/rsi/idl_6.0/bin/index /usr/local/bin/idlindex

-----------------------------------------------------
Step 2.  How to launch helpindex.
-----------------------------------------------------

From Unix shell type for example:
% idlindex &

From IDL type for example:
IDL> index


-----------------------------------------------------
Step 4. How to use.
-----------------------------------------------------

Start typing the beginning of the topic you are looking
for.  Special characters:

Enter = Go to the topic in Acrobat Reader
Tab   = Change focus to the other widget.
C-p   = previous line
C-n   = next line
C-f   = next match
C-b   = previous match
C-v   = page down
C-g   = page up
C-q   = quit

You can also use the mouse button to click on a topic
in the list and the selection should move. 

Select the pdf document to be searched in the dropdown
list.  The default is the Master Index.
