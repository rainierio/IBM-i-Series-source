     H/COPY QSYSREFRLE,COPYRIGHTH
     F*****************************************************************
     F* Program Name..: IFS95005                                      *
     F* Description...: IF I/O Interface Subfile Listing              *
     F* Date Created..: 25 May 2011                                   *
     F* Author........: Chia Ji Shon                                  *
     F* 2nd Author....: Johnson Lim                                   *
     F* Ref no........:                                               *
     F*****************************************************************
     F* IFS95005      is part of Silverlake Integrated Banking System *
     F* Copyright 1996 by Silverlake System Sdn Bhd Kuala Lumpur,     *
     F* Malaysia.                                                     *
     F*****************************************************************
     F* Modification Logs:                                            *
     F* Init  RefN     Date    Description                            *
     F* ----  ----     ----    -----------                            *
     F*                                                               *
      *****************************************************************
     FINTFILEL5 IF   E           K DISK    prefix(M_)
     ** MBASE-Common D-Spec for Maint File Handler
OS01 D/COPY QMBSREFRLE,MBQTXHD
     ** I/R struc
     D DS95005I      E DS                  EXTNAME(IFS95005I) PREFIX(PR)
     D DS95005R      E DS                  EXTNAME(IFS95005R)
     **
     D DSIROW        E DS                  EXTNAME(IFS95005I) PREFIX(PI)
     D DSWROW        E DS                  EXTNAME(IFS95005R)
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
OS01 C                   EXSR      SRINIT
     C
     C                   CLEAR                   DS95005I
     C                   CLEAR                   DS95005R
     C                   CLEAR                   OTSTRS
     C                   eval      *in42='0'
     C
     C
     **
     ** main
     C                   SELECT
     ** action inquiry
OS01 C                   WHEN      HDACCD      = 'I'
     C                   EXSR      SRINQ
     C                   OTHER
     ** error
     C                   EVAL      ERCODE      = 'MBM2000'
     C                   EVAL      P8MDTA      = HDACCD
     C                   EXSR      SRSENDERR
     **
     C                   ENDSL
     ** re-map header output
OS01 C                   EXSR      SREXIT
     **
     **===========================================
     ** sr - inq record
     **===========================================
     C     SRINQ         BEGSR
     **
     ** map input response
     C                   EVAL      wCont = *ZEROS
     C                   EVAL      DS95005I   = INSTRS
     C*                  EVAL      DS95005R   = DS95005I
     C                   EVAL      WLEN        = %SIZE(DS95005R)
     C                   EVAL      WSTRLEN     = 0
     C                   EVAL      WHDNREC     = HDNREC + 1
     C                   FOR       WX          = 1 to %ELEM(DSWROW)
     C     WX            OCCUR     DSWROW
     C                   RESET                   DSWROW
     C                   ENDFOR
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
     **
OS01 ** If no error reset flag to "N"
OS01 C                   EVAL      WERROR      = 'N'
     **
     C                   ENDSR
     **============================================
     ** sr - process
     **============================================
     C     SRPROC1       BEGSR
     **
     C                   EVAL      WERROR      = 'N'
     **
OS01 C                   EXSR      SRSQLPREP
     **
OS01 C                   EXSR      SRSQLSTMT
     **
OS01 C                   EXSR      SRSQLOPEN
     **
OS01 C                   EXSR      SRSQLFETCH
     C                   IF        WERROR      = 'Y'
     C                   LEAVESR
     C                   ENDIF
      **
     C                   EXSR      SROUTDATA
     **
OS01 C                   EXSR      SRSQLCLOSE
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
OS01 ** sr -
     **============================================
     C     SRCVTIN       BEGSR
     **
     ** default error "Y"
OS01 C                   EVAL      WERROR      = 'Y'
OS01 ** If no error reset flag to "N"
OS01 C                   EVAL      WERROR      = 'N'
     **
     C     ERCVTIN       ENDSR
     **============================================
OS01 ** sr -
     **============================================
     C     SRCVTOUT      BEGSR
     **
     ** return search key
     C*                  EVAL      DS95005I   = DS95005R
     C*                  EVAL      WHDVFMT_KEY = DS95005I
     C*                  EVAL      HDVFMT      = WHDVFMT
     **
     C     ERCVTOUT      ENDSR
     **============================================
OS01 ** sr -
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
     C                   EVAL      WFROM = ' FROM INTFILEL5 '
      *
     C                   EVAL      WHERE  =  ' WHERE '
     C
      /FREE
                    IF PRINFIID = ' ' AND PRINREFN = '';
                       WHERE = %TRIMR(WHERE) +
                               ' INFIID = '+WAPOS+WAPOS+ 'AND' +
                               ' INREFN = '+WAPOS+WAPOS+ 'AND' +
                               ' INSRC = '+WAPOS+WAPOS;
                       wDone = '1';

                    ELSEIF PRINFIID <> ' ' AND PRINREFN = '';
                       WHERE = %TRIMR(WHERE) +
                               ' INFIID =' + WAPOS + PRINFIID +WAPOS   +
                               ' AND INSRC LIKE '+WAPOS+ 'ETP%' +WAPOS +
                               ' AND (INREFN <>' + WAPOS+WAPOS + ')';
                       wDone = '1';

                    ELSE;
                       WHERE = %TRIMR(WHERE) +
                               ' INREFN='
                               +WAPOS+%TRIM(PRINREFN)       +WAPOS
                               +' OR  (INREFN='
                               +WAPOS+%TRIM(PRINREFN)       +WAPOS
                               + ')';
                    ENDIF;
      /END-FREE
     C
      *
     C                   EVAL      WORDER = ' ORDER BY INDATE DESC, +
     C                                      INTIME DESC'
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
OS01 C/COPY QMBSREFRLE,MBQTXHC
     ** LOANS-Common C-Spec for Maint File Handler
OS01 C*/COPY QBWCREFRLE,CFQTXHC
     ** LOANS-Common C-Spec for Maint File Handler
OS01 C*/COPY QBWCREFRLE,CFQRTVPC
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
     C                   kfld                    PRINREFN
     C
     C     KYRtn1Rec     chain     rintfile                           41
     C*                  eval      WX        = 0
     C                   eval      wCont =  1
     C                   if        *in41 = '0'  and PRINREFN <> *BLANK and
     C                                     wCont = 1
     C
     C                   eval      WX        = 1
     C                   eval      HDNREC    = 1
     C
     C                   eval      PRINFIID  =   M_INFIID
     C                   eval      PRINREFN  =   M_INREFN
     C                   eval      PRINFNAM  =   M_INFNAM
     C                   eval      PRINFSEQ  =   M_INFSEQ
     C
     C                   eval      *in42 = '1'
     C                   eval      wCont = 2
     C                   endif
     C                   ENDSR
     ****************************************************************
      *------------------------ END OF CODING ----------------------*
