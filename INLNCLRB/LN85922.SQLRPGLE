     H/COPY QSYSREFRLE,COPYRIGHTH
     F*****************************************************************
     F* Program Name..: LN85922                                       *
     F* Description...: Loan Close and Rebook Inquiry                 *
     F* Date Created..: 28 April 2022                                 *
     F* Author........: Rainier Letidjawa                             *
     F* Ref no........:                                               *
     F*****************************************************************
     F* LN85922      is part of Silverlake Integrated Banking System *
     F* Copyright 1996 by Silverlake System Sdn Bhd Kuala Lumpur,     *
     F* Malaysia.                                                     *
     F*****************************************************************
     F* Modification Logs:                                            *
     F* Init  RefN     Date    Description                            *
     F* ----  ----     ----    -----------                            *
     F*                                                               *
      *****************************************************************
     FINTFILEL9 IF   E           K DISK    prefix(M_)
     ** MBASE-Common D-Spec for Maint File Handler
     D/COPY QMBSREFRLE,MBQTXHD
     ** I/R struc
     D DS85922I      E DS                  EXTNAME(LN85922I) PREFIX(PR)
     D DS85922R      E DS                  EXTNAME(LN85922R)
     **
     D DSIROW        E DS                  EXTNAME(LN85922I) PREFIX(PI)
     D DSWROW        E DS                  EXTNAME(LN85922R)
     D                                     OCCURS(999) PREFIX(PW)
     **
     DDSINFIID         DS
     D  INOUT                  1      2
     D  CONTS                  3     20
     ** def constant
     D PRLBDS          DS
     D  INFILE                83     92
     D  LENSSFXRP            125    126B 0
     D  DB_RRN               397    400I 0
     **
     D  wCont          S              1S 0
     D  wDone          S              1A
     D WHDNREC         S                   LIKE(HDNREC)
     **
     **===========================================
     ** main logic
     **===========================================
     **
     ** initialise, map header,
     C                   EXSR      SRINIT
     C
     C                   CLEAR                   DS85922I
     C                   CLEAR                   DS85922R
     C                   CLEAR                   OTSTRS
     C                   eval      *in42='0'
     C
     C
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
     C                   EVAL      wCont = *ZEROS
     C                   EVAL      DS85922I   = INSTRS
     C*                  EVAL      DS85922R   = DS85922I
     C                   EVAL      WLEN        = %SIZE(DS85922R)
     C                   EVAL      WSTRLEN     = 0
     C                   EVAL      WHDNREC     = HDNREC + 1
     C                   FOR       WX          = 1 to %ELEM(DSWROW)
     C     WX            OCCUR     DSWROW
     C                   RESET                   DSWROW
     C                   ENDFOR
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
     **
     ** If no error reset flag to "N"
     C                   EVAL      WERROR      = 'N'
     **
     C                   ENDSR
     **============================================
     ** sr - process
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
     C                   IF        WERROR      = 'Y'
     C                   LEAVESR
     C                   ENDIF
      **
     C                   EXSR      SROUTDATA
     **
     C                   EXSR      SRSQLCLOSE
     **
     C                   IF        HDNREC      = 0
     C                   EVAL      ERCODE      = 'MBM2001'
     C                   EVAL      P8MDTA      = PSPGM
     C                   EVAL      WERROR      = 'Y'
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
     ** return search key
     C*                  EVAL      DS85922I   = DS85922R
     C*                  EVAL      WHDVFMT_KEY = DS85922I
     C*                  EVAL      HDVFMT      = WHDVFMT
     **
     C     ERCVTOUT      ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRSQLPREP     BEGSR
      *
     C                   EVAL      WFLDN='INFIID,'+
     C                                   'INREFN,' +
     C                                   'INFNAM,' +
     C                                   'INFSEQ,' +
     C                                   'INSTS,' +
     C                                   'INTOTR,' +
     C                                   'INTTHD,' +
     C                                   'INDATE,' +
     C                                   'INTIME,' +
     C                                   'INUSRID,' +
     C                                   'INPGM,' +
     C                                   'INUPUSR, ' +
Jon1 C                                   'INERM1'
     C
     C                   EVAL      WFROM = ' FROM INTFILEL9 '
      *
     C                   EVAL      WHERE  =  ' WHERE '
     C
      /FREE
                    IF PRINFIID <> ' ' AND PRINDATE <> ' ';
                       WHERE = %TRIMR(WHERE) +
                               ' INFIID = '+WAPOS+ PRINFIID +WAPOS+
                               ' AND INDATE = '+WAPOS+ PRINDATE +WAPOS;
                       wDone = '1';

                    ELSEIF PRINFIID <> ' ' AND PRINDATE = ' ';
                       WHERE = %TRIMR(WHERE) +
                               ' INFIID =' + WAPOS + PRINFIID +WAPOS;
                       wDone = '1';

                    ELSE;
                       WHERE = %TRIMR(WHERE) +
                               ' INFIID =' + WAPOS + PRINFIID +WAPOS;
                       wDone = '1';
                    ENDIF;
      /END-FREE
     C
      *
     C                   EVAL      WORDER = ' ORDER BY INDATE DESC, +
     C                                      INTIME DESC'
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
     C                   EVAL      WRRN  = 1
     **
     C     ERSQLSTMT     ENDSR
     **==========================================
     ** SR -
     **==========================================
     C*    SRSQLOPEN     BEGSR
     **
     **C/EXEC SQL
     **C+PREPARE S1 FROM :WSTMT
     **C/END-EXEC

     ** declare cursor
     **C/exec sql
     **C+ DECLARE C1 DYNAMIC SCROLL CURSOR FOR S1
     **C/end-exec
     **
     **C/EXEC SQL
     **C+ OPEN C1
     **C/END-EXEC
     **
     C*    ERSQLOPEN     ENDSR
     **==========================================
     ** SR -
     **==========================================
     C     SROUTDATA     BEGSR
     **
     C     1             DO        HDNREC        WX                4 0
     C     WX            OCCUR     DSWROW
     C                   IF        DSWROW      <> *allx'00' AND
     C                             DSWROW      <> *Blanks
     C                   EXSR      SREXTOTH
     C                   EXSR      SRCVTOUT
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
     C     EROUTDATA     ENDSR
     **==========================================
     ** SR -
     **==========================================
     C*    SRSQLCLOSE    BEGSR
     **
     **C/EXEC SQL
     **C+ CLOSE C1
     **C/END-EXEC
     **
     C*    ERSQL         ENDSR
     **==========================================
     C****************************************************************
     C/COPY QMBSREFRLE,MBQTXHC
     ** LOANS-Common C-Spec for Maint File Handler
     C*/COPY QBWCREFRLE,CFQTXHC
     ** LOANS-Common C-Spec for Maint File Handler
     C*/COPY QBWCREFRLE,CFQRTVPC
     C/COPY QSYSREFRLE,SSQTXSSQL
     **===========================================
     **
     **===========================================
     C*    SRINZSR       BEGSR
     **
     C*                  READ      RCFPAR1
     **
     ** CIF - Common C-Spec for Initialize Routine
     C*/COPY QBWCREFRLE,CFQINZR
     C*    ERINZSR       ENDSR
     **===========================================
     ** sr - application exit program
     **===========================================
     C*    SREXITAPP     BEGSR
     **
     C*    EREXITAPP     ENDSR
     **===========================================
     C     SREXTOTH      BEGSR
     C
     C                   if        *in42='1'  or PRINFIID = *Blanks
     C                   leavesr
     C                   endif
     C
     C
     C     KYRtn1Rec     klist
     C                   kfld                    PRINFIID
     C                   kfld                    PRINDATE
     C
     C     KYRtn1Rec     chain     rintfile                           41
     C*                  eval      WX        = 0
     C                   eval      wCont =  1
     C                   if        *in41 = '0'  and PRINDATE <> *BLANK and
     C                                     wCont = 1
     C
     C                   eval      WX        = 1
     C                   eval      HDNREC    = 1
     C
     C                   eval      PRINFIID  =   M_INFIID
     C                   eval      PRINDATE  =   M_INDATE
     C
     C                   eval      *in42 = '1'
     C                   eval      wCont = 2
     C                   endif
     C                   ENDSR
     ****************************************************************
      *------------------------ END OF CODING ----------------------*
