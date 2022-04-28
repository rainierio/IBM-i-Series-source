     H/COPY QSYSREFRLE,COPYRIGHTH
     H/COPY QMBSREFRLE,MBQTXHH
      **==============================================================
      *  Progran Name: IFS95010          REF.NO:
      *  DESCRIPTION.: Subfile - Intfile inquiry by download code and reference
      *  DATE CREATED: 30/08/2011
      *  AUTHOR......:
      **==============================================================
      * IFS95010  IS A PART OF SILVERLAKE SYSTEM (R)
      *    (C) COPYRIGHT 1996-2009
      **==============================================================
      *  MODIFICATION LOGS
      *  NAME  INIT   DATE      REMARKS
      *  ----  -----  --------  -----------------------------------
      **==============================================================
     **
     FINTDTE    IF   E           K DISK    PREFIX(N_)
     FINTDTL    IF   E           K DISK    PREFIX(N_)
     ** MBASE-Common D-Spec for Maint File Handler
     D/COPY QMBSREFRLE,MBQTXHD
     **
     ** I/R struc
     D DSIFS95010I   E DS                  EXTNAME(IFS95010I) PREFIX(PR)
     D DSIFS95010R   E DS                  EXTNAME(IFS95010R) PREFIX(PW)
     **
     **
     D DSWROW        E DS                  EXTNAME(IFS95010R)
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
     C                   EVAL      DSIFS95010I = INSTRS
     C                   EVAL      WLEN        = %SIZE(DSIFS95010R)
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
OS01 C                   EXSR      SRCVTIN
     C                   IF        WERROR      = 'Y'
OS01 C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
     ** start validation
     C                   EXSR      SRVALIDATE
     C                   IF        WERROR      = 'Y'
OS01 C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
     ** process history file
     C                   EXSR      SRPROC1
     C                   IF        WERROR      = 'Y'
OS01 C                   EXSR      SRSENDERR
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
OS01 C                   EVAL      WERROR      = 'Y'
     **
     C                   IF        PRINFIID    = *Blanks
     C                   EVAL      ERCODE      = 'IFS0003'
     C                   EVAL      ERFLDN      = 'INFIID'
     C                   LEAVESR
     C                   ENDIF
     **
     C                   IF        PRINFNAM    = *Blanks
     C                   EVAL      ERCODE      = 'IFS0006'
     C                   EVAL      ERFLDN      = 'INFNAM'
     C                   LEAVESR
     C                   ENDIF
     **
     **
     **
OS01 ** If no error reset flag to "N"
OS01 C                   EVAL      WERROR      = 'N'
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
OS01 ** sr -
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
OS01 ** sr -
     **============================================
     C     SRCVTOUT      BEGSR
     **
     **
     C     ERCVTOUT      ENDSR
     **============================================
OS01 ** sr -
     **============================================
     C     SRSQLPREP     BEGSR
      *
     C
     C                   EVAL      WFLDN = 'INFIID,' +
     C                                     'INFNAM, INDATE,' +
     C                                     'INFSEQ, INSEQN,' +
     C                                     'INRSEQN'
     C
     C                   EVAL      WFROM = ' FROM INTDTL'
      *
     C                   EVAL      WCONDN = ' = '
      *
     C                   EVAL      WHERE = ' WHERE INFIID'+ WCONDN +
     C                                       wapos+PRINFIID+wapos+
     C
     C                                     'AND INFNAM'+ WCONDN +
     C                                      wapos+PRINFNAM+wapos+
     C
     C                                     'AND INFSEQ'+ WCONDN +
     C                                      wapos+%CHAR(PRINFSEQ)+wapos+
     C
     C                                      'AND INDATE' + WCONDN +
     C                                      wapos+%CHAR(PRINDATE)+wapos
      *
     C                   EVAL      WORDER = ' ORDER BY INDATE DESC'
      *
     C                   ENDSR
     **============================================
OS01 ** sr -
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
‚lsl1C                   EXSR      RTVappNm
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
OS01 C/COPY QMBSREFRLE,MBQTXHC
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
‚lsl1 ****************************************************************
      * SUBROUTINE: RTVappNM
      * PURPOSE   : Retrieve Any fields
      ****************************************************************
 ‚   C     RTVappNM      BEGSR
     C
‚sl1 C                   ENDsr
     **===========================================
     C     SREXTOTH      BEGSR
     **
     C                   EVAL      N_INFIID  =  PRINFIID
     C                   EVAL      N_INFNAM  =  PRINFNAM
     C                   EVAL      N_INDATE  =  PRINDATE
     C                   EVAL      N_INFSEQ  =  PRINFSEQ
     C                   EVAL      N_INSEQN  =  INSEQN
     C                   EVAL      N_INRSEQN =  INRSEQN
     **
     C     KYINTDTE      KLIST
     C                   KFLD                    N_INFIID
     C                   KFLD                    N_INFNAM
     C                   KFLD                    N_INDATE
     C                   KFLD                    N_INFSEQ
     C                   KFLD                    N_INSEQN
     C                   KFLD                    N_INRSEQN
     **
     C     KYINTDTE      CHAIN     INTDTE                             40
      *
     C                   IF        *IN40 = '0'
     C                   EVAL      INERROR = 'Y'
     C                   ELSE
     C                   EVAL      INERROR = 'N'
     C                   ENDIF
      *
     C                   ENDSR
