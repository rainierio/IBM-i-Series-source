Copying Source Files into the IFS - IBM
    
You can copy your main source physical file to an integrated file system (IFS) file :
Example :
CPYTOSTMF FROMMBR('/QSYS.LIB/S104120X.LIB/LNCLOSREBK.FILE/INI3102.MBR') TOSTMF('/home/S104120X/LNCLOSREBK/INI3102.rpgle')


You can copy your integrated file system (IFS) file  to an main source physical file : 
CPYFRMSTMF FROMSTMF('/home/S104120X/XCMPLNCLOS.cl') TOMBR('/qsys.lib/S104120X.lib/MISC.file/$XCMPLNCLOS.mbr') 
MBROPT(*ADD) STMFCODPAG(*PCASCII) ENDLINFMT(*CRLF) 

