     H/COPY QSYSREFRLE,COPYRIGHTH
     H/COPY QMBSREFRLE,MBQTXHH
      **==============================================================
      *  Progran Name: LN85923          REF.NO:
      *  DESCRIPTION.: Subfile - Loan Close and Rebook Uploaded
      *  DATE CREATED: 28/04/2022
      *  AUTHOR......: Rainier Letidjawa
      **==============================================================
      * LN85923 IS A PART OF SILVERLAKE SYSTEM (R)
      *    (C) COPYRIGHT 1996-2022
      **==============================================================
      *  MODIFICATION LOGS
      *  NAME  INIT   DATE      REMARKS
      *  ----  -----  --------  -----------------------------------
      **==============================================================
     **
     FINLNCLRBF IF   E           K DISK    PREFIX(BF_)
     ** MBASE-Common D-Spec for Maint File Handler
     D/COPY QMBSREFRLE,MBQTXHD
     **
     ** I/R struc
     D DSLN85923I    E DS                  EXTNAME(LN85923I) PREFIX(PR)
     D DSLN85923R    E DS                  EXTNAME(LN85923R) PREFIX(PW)
     **
     **
     D DSWROW        E DS                  EXTNAME(LN85923R)
     D                                     OCCURS(999)
     **
     ** def constant
     **
     **=================================================
     ** def variable for SQL
     **=================================================
     **===========================================
     ** def parameter/keylist
     **===========================================
     **===========================================
     ** main logic
     **===========================================
     **
     ** initialise, map header,
     C                   EXSR      SRINIT
     **
     ** main
     C                   SELECT
     ** action inquiry
     C                   WHEN      HDACCD      = 'I'
     C                   EXSR      SRINQ
     C                   OTHER
     ** error
     C                   EVAL      ERCODE      = 'MBM2000'
     C                   EVAL      P8MDTA      = HDACCD
     C                   EXSR      SRSENDERR
     **
     C                   ENDSL
     ** re-map header output
     C                   EXSR      SREXIT
     **
     **===========================================
     ** sr - inq record
     **===========================================
     C     SRINQ         BEGSR
     **
     ** map input response
     C                   EVAL      DSLN85923I = INSTRS
     C                   EVAL      WLEN        = %SIZE(DSLN85923R)
     C                   EVAL      WSTRLEN     = 0
     ** reset dswrow
     C                   FOR       WX          = 1 to %ELEM(DSWROW)
     C     WX            OCCUR     DSWROW
     C                   RESET                   DSWROW
     C                   ENDFOR
     ** wll - handle hdnrec exception
     C                   IF        HDNREC     >
     C                             (%SIZE(MB_MBDETL) / %SIZE(DSWROW))
     C                   EVAL      HDNREC     =
     C                             (%SIZE(MB_MBDETL) / %SIZE(DSWROW))
     C                   ENDIF
     **
     **
     C                   EXSR      SRCVTIN
     C                   IF        WERROR      = 'Y'
     C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
     ** start validation
     C                   EXSR      SRVALIDATE
     C                   IF        WERROR      = 'Y'
     C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
     ** process history file
     C                   EXSR      SRPROC1
     C                   IF        WERROR      = 'Y'
     C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
     C     ERINQ         ENDSR
     **===========================================
     ** sr - validation
     **===========================================
     C     SRVALIDATE    BEGSR
     **
     ** default error "Y"
     C                   EVAL      WERROR      = 'Y'
     **
     C                   IF        PRINDATE    = *Blanks
     C                   EVAL      ERCODE      = 'IFS0001'
     C                   EVAL      ERFLDN      = 'INDATE'
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        PRINREFN    = *Blanks
     C                   EVAL      ERCODE      = 'IFS0002'
     C                   EVAL      ERFLDN      = 'INREFN'
     C                   LEAVESR
     C                   ENDIF     
     **
     ** If no error reset flag to "N"
     C                   EVAL      WERROR      = 'N'
     **
     C                   ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRPROC1       BEGSR
     **
     C                   EVAL      WERROR      = 'N'
     **
     C                   EXSR      SRSQLPREP
     **
     C                   EXSR      SRSQLSTMT
     **
     C                   EXSR      SRSQLOPEN
     **
     C                   EXSR      SRSQLFETCH
     **
     C                   EXSR      SRSQLCLOSE
     **
     C                   IF        HDNREC      = 0
     C                   EVAL      ERCODE      = 'MBM2001'
     C                   EVAL      P8MDTA      = PSPGM
     C                   EVAL      WERROR      = 'Y'
     C                   EVAL      HDNREC      = 1
     C                   LEAVESR
     C                   ENDIF
     **
     C     ERPROC1       ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRCVTIN       BEGSR
     **
     ** default error "Y"
     C                   EVAL      WERROR      = 'Y'
     ** If no error reset flag to "N"
     C                   EVAL      WERROR      = 'N'
     **
     C     ERCVTIN       ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRCVTOUT      BEGSR
     **
     **
     C     ERCVTOUT      ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRSQLPREP     BEGSR
      *
     C
     C                   EVAL      WFLDN = '*'
     C
     C                   EVAL      WFROM = ' FROM INLNCLRBF'
      *
     C                   EVAL      WCONDN = ' = '
      *
     C                   EVAL      WHERE = ' WHERE INDATE'+ WCONDN +
     C                                       wapos+%CHAR(PRINDATE)+wapos
     C
     C                                     'AND INREFN'+ WCONDN +
     C                                      wapos+PRINREFN+wapos+
     C
      *
     C                   EVAL      WORDER = ' ORDER BY INSEQ DESC'
      *
     C                   ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRSQLSTMT     BEGSR
     **
     C                   EVAL      WSTMT = 'SELECT ' +  %TRIMR(WFLDN)
     C                                     + %TRIMR(WFROM)
     C                                     + %TRIMR(WHERE)
     C                                     + %TRIMR(WORDER)
     **
     C     ERSQLSTMT     ENDSR
     **==========================================
     ** SR -
     **==========================================
     C     SRSQLFETCH    BEGSR
     **
     C/EXEC SQL
     C+ FETCH RELATIVE :WRRN FROM C1 FOR :HDNREC ROWS INTO :DSWROW
     C/END-EXEC
     **
     C                   EVAL      OTSTRS=*BLANKS
     **
     C                   IF        SQLSTT = SqlEOF
     C                   EVAL      HDNREC      = 0
     C                   LEAVESR
     C                   ENDIF
     **
     **
     C     1             DO        HDNREC        WX                4 0
     C     WX            OCCUR     DSWROW
     C                   IF        DSWROW      <> *allx'00' AND
     C                             DSWROW      <> *Blanks
     C                   EXSR      SREXTOTH
     C                   EVAL      %SUBST(OTSTRS:WSTRLEN+1:WLEN)
     C                             = DSWROW
     C                   EVAL      WSTRLEN = WSTRLEN + WLEN
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     **
     C                   EVAL      HDNREC       = WX - 1
     **
     C     ERSQLFETCH    ENDSR
     **==========================================
     ** SR -
     **==========================================
     C     SRSQLCLOSE    BEGSR
     **
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     **
     C     ERSQL         ENDSR
     **==========================================
     C****************************************************************
     C/COPY QMBSREFRLE,MBQTXHC
     ** REM-Common C-Spec for SFL
     C/COPY QREMREFRLE,RMQTXSSQL
     **===========================================
     **
     **===========================================
     C     SRINZSR       BEGSR
     **
     C     ERINZSR       ENDSR
     **===========================================
     ** sr - application exit program
     **===========================================
     C     SREXITAPP     BEGSR
     **
     C     EREXITAPP     ENDSR
     **===========================================
     C     SREXTOTH      BEGSR
     C* Validate to other file here if needed
     C                   ENDSR
