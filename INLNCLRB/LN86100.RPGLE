OS01 H/COPY QSYSREFRLE,COPYRIGHTH
OS01 H/COPY QMBSREFRLE,MBQTXHH
      ****************************************************************
      *  FILE NAME...: LN86100           REF.NO:                     *
      *  DESCRIPTION.: SMT - PENDING TRANSACTION INQUIRY             *
      *  DATE CREATED: 06.12.2011                                    *
      *  AUTHOR......: Kenneth Lam                                   *
      ****************************************************************
      * LN86100    IS A PART OF SILVERLAKE SYSTEM (R)                *
      *    (C) COPYRIGHT 2000                                        *
      ****************************************************************
      *  MODIFICATION LOGS                                           *
      *  NAME  INIT   DATE      REMARKS                              *
      *  ----  -----  --------  -----------------------------------  *
      *  CWEI  SS01   18/06/10  ADD FILTER MXMTYP <> 'F' EXCLUDE SMF *
      ****************************************************************
      *  MODIFICATION LOGS CIMB R1                                   *
      *  NAME  INIT   DATE      REMARKS                              *
      *  ----  -----  --------  -----------------------------------  *
      *  LCC   R1001  13/02/12  no need to check search by Acc No.   *
      ****************************************************************
      *  MODIFICATION LOGS                                           *
      *  NAME  INIT   DATE      REMARKS                              *
      *  ----  -----  --------  -----------------------------------  *
      *  LSA   M2001  09/06/13  Add routine to chain for tranche account for MDAMT1
      ****************************************************************
      *-- FILE DECLARATION --*
     FLNMHDRLF  IF   E           K DISK
     F*M2001LNMSPLL1  IF   E           K DISK    RENAME(RLNMHDR:RLNMSPL)
M2001FLNMSPLL1  IF   E           K DISK    RENAME(RLNMHDR:RLNMSPLL1)
     F                                     PREFIX(S)
M2001FLNMSPL    IF   E           K DISK    RENAME(RLNMHDR:RLNMSPL)
M2001F                                     PREFIX(S)
     FLNMMOD    IF   E           K DISK    RENAME(RLNMHDR:RLNMMOD)
     F                                     PREFIX(M)
     FLNPRTX    IF   E           K DISK
     FLNMAST    IF   E           K DISK
     FLNTNEW    IF   E           K DISK    RENAME(RLNMAST:RLNTNEW)
     FLNMRAI    IF   E           K DISK
      *
      *-- VARIABLE DECLARATION --*
     ** MBASE-Common D-Spec for Maint File Handler
     D/COPY QMBSREFRLE,MBQTXHD
     ** LOANS-Common D-Spec for Maint File Handler
     D/COPY QLNSREFRLE,LNQTXHD
     D/COPY QLNSREFRLE,LNQCVTXD
     **
     D* Convert lower case to upper case.
     D  LOCASE         C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D  UPCASE         C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     **
     ** I/R struc
     D DS86100I      E DS                  EXTNAME(LN86100I) PREFIX(PR1)
     D DS86100R      E DS                  EXTNAME(LN86100R)
     **
     D  WFilter        S              1
     **
     D* Convert lower case to upper case.
     D  CHKS1          S                   LIKE(MDSNME)
     D  CHKD1          S                   LIKE(MDSNME)
     D  S1ACNO         S                   LIKE(MDACCT)
     D  S1TCID         S                   LIKE(MDTCID)
     D  S1RBC          S                   LIKE(MDBR  )
     D  S1BKN          S                   LIKE(MDBANK)
     **===========================================
     **-- PROGRAM MAINLINE --*
     **===========================================
      *-- KEY LISTS --*
     C     KYLNMHDR      KLIST
     C                   KFLD                    PR1MDAXTC
     **
     C     KYLNMHDRMORE  KLIST
     C                   KFLD                    PR1MDAXTC
     C                   KFLD                    PR1MDACCT
     C                   KFLD                    PR1MDBATN
     C                   KFLD                    PR1MDSEQN
     C                   KFLD                    PR1MDPODR
     **
     C     KYLNMAST      KLIST
     C                   KFLD                    MDACCT
     C                   KFLD                    MDACTY
     **
     ** initialise, map header,
     C                   EXSR      SRINIT
     ** main
     C                   SELECT
      *
     C                   WHEN      HDACCD      = 'I'
     C                   EXSR      SRINQ
     C                   OTHER
     ** error
     C                   EVAL      ERCODE = 'MBM2000'
     C                   EVAL      P8MDTA = HDACCD
     C                   EXSR      SRSENDERR
      *
     C                   ENDSL
      *
     ** re-map header output
     C                   EXSR      SREXIT
     **
     **===========================================
     ** sr -
     **===========================================
     CSR   SRINQ         BEGSR
     **
     C                   EVAL      DS86100I   = INSTRS
     C                   EVAL      WLEN     = %SIZE(DS86100R)
     C                   EVAL      WSTRLEN  = 0
     **
     C                   EXSR      SRCVTIN
     ** start validation
     C                   EXSR      SRVALIDATE
     C                   IF        WERROR = 'Y'
     C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
      *-- READ FIRST RECORDS --*
     C                   IF        HDMREC = 'Y'
     C     KYLNMHDRMORE  SETGT     RLNMHDR                            31
     C                   ELSE
     C     KYLNMHDR      SETLL     RLNMHDR                            31
     C                   ENDIF
     **
     C     KYLNMHDR      READE     RLNMHDR                                31
     **
     C                   IF        *IN31 = *ON
      *-- ERROR RETURN --*
     C                   EVAL      ERCODE = 'MBM2001'
     C                   EVAL      P8MDTA = PSPGM
     C                   EXSR      SRSENDERR
     C                   ELSE
     **
     C                   EVAL      COUNTR = *ZEROS
      *-- CREATE OCCURANCES RESPONSE --*
     C                   DOW       (*IN31 = *OFF) AND
     C                             (COUNTR < HDNREC)
     ** rtv add info
     C                   EXSR      SrFilter
     C                   IF        WFilter    <> 'Y'
     C                   EXSR      SRPROC1
     C                   EXSR      SRLNMRAI
     C                   EXSR      SRCVTOUT
     **
     C                   EVAL      %SUBST(OTSTRS:WSTRLEN+1:WLEN)
     C                             = DS86100R
     **
     C                   EVAL      WSTRLEN = WSTRLEN + WLEN
     C                   EVAL      COUNTR = COUNTR + 1
     C                   ENDIF
     **
     C     KYLNMHDR      READE     RLNMHDR                                31
     **
     C*                  ENDIF
      *
     C                   ENDDO
     **
     C                   IF        *IN31 = *OFF
     C                   EVAL      HDMREC = 'Y'
     C                   ELSE
     C                   EVAL      HDMREC = 'N'
     C                   ENDIF
     ** to indicate how many records send out
     C                   EVAL      HDNREC = COUNTR
      *
     C                   ENDIF
     **
     C                   IF        HDNREC = 0
     C                   EVAL      ERCODE = 'MBM2001'
     C                   EVAL      P8MDTA = PSPGM
     C                   EXSR      SRSENDERR
     C                   LEAVESR
     C                   ENDIF
     **
     CSR                 ENDSR
     **============================================
     ** sr - Validation
     **============================================
     C     SRVALIDATE    BEGSR
     **
     C                   EVAL      WERROR      = 'Y'
     **
     C                   EVAL      WERROR      = 'N'
     **
     CSR                 ENDSR
     **============================================
     ** sr - SRPROC1 - RETRIVE INFORMATION
     **============================================
     C     SRPROC1       BEGSR
     **
     C                   EVAL      D1TDSC      = *BLANKS
     C                   EVAL      D1AMT       = *ZEROS
     ** Trx description
     C     MDAXTC        CHAIN     RLNPRTX
     C                   IF        NOT %FOUND(LNPRTX)
     C                   GOTO      ENDSRPROC1
     C                   ENDIF
     ** Covert Text
     C                   IF        LNSPTF     <> 'N'
     C                   MOVEL     MXTDES        InTxt
     C                   EXSR      SrCvtTxt
     C                   MOVEL     InTxt         MXTDES
     C                   ENDIF
     **
     C                   EXSR      SrHdrFmt
     ** Amount
     C                   IF        MXMTYP     <> 'N'
     C                   EXSR      SrSplMrgAmt
     C                   ELSE
     C                   EVAL      D1AMT       = MDAMT1
     C                   ENDIF
     **
     C     ENDSRPROC1    ENDSR
     **============================================
     ** sr - SRLNMRAI - Get checker id from LNMRAI
     **============================================
     C     SRLNMRAI      BEGSR
     **
     C     KYLNMRAI      KLIST
     C                   KFLD                    MDACCT
     C                   KFLD                    MDACTY
     C                   KFLD                    MDTCID
     C                   KFLD                    MDTCD8
     C                   KFLD                    MDBATN
     C
     C     KYLNMRAI      CHAIN     RLNMRAI                            90
     C                   IF        *IN90 = *ON
     C                   EVAL      MRRQID = *BLANKS
     C                   EVAL      MRRQI2 = *BLANKS
     C                   ENDIF
     **
     C     ERLNMRAI      ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRCVTIN       BEGSR
     **
     ** if more rec
     C                   IF        HDMREC      = 'Y'
     C                   EVAL      WHDVFMT     = HDVFMT
     C                   EVAL      DS86100I    = WHDVFMT_KEY
     **
     **
     C                   ENDIF
     **
     C                   EVAL      MDTCD8      = PR1MDTCD8
     C                   EVAL      S1TCID      = PR1MDTCID
     C                   EVAL      S1ACNO      = PR1MDACCT
     C                   EVAL      S1RBC       = PR1MDCBRN
     C                   EVAL      S1BKN       = PR1MDBANK
     **
     C     ERCVTIN       ENDSR
     **============================================
     ** sr -
     **============================================
     C     SRCVTOUT      BEGSR
     **
     C                   EVAL      OMDMODE     = MDMODE
     **
     ** return search key(last record unique key)
     C**                 EVAL      DS86100I    = DS86100R
     C                   MOVEL     DS86100R      DS86100I
     C                   EVAL      WHDVFMT_KEY = DS86100I
     C                   EVAL      HDVFMT      = WHDVFMT
     **
     C     ERCVTOUT      ENDSR
     **
     **============================================
     ** sr - Split/Mereg Amount
     **============================================
     C     SrSplMrgAmt   BEGSR
     **
     C     KYLNMSP       KLIST
     C                   KFLD                    MDACCT
     C                   KFLD                    MDACTY
     C                   KFLD                    MDTCID
     C                   KFLD                    MDTCD8
     C                   KFLD                    MDBATN
     **
     C*M2001     KYLNMSP       SETLL     RLNMSPL
     C*M2001     KYLNMSP       READE     RLNMSPL
M2001C     KYLNMSP       SETLL     RLNMSPLL1
M2001C     KYLNMSP       READE     RLNMSPLL1                              91
M2001C                   IF        *IN91 = '0'
     C                   DOW       NOT %EOF(LNMSPLL1)
     **
     C                   IF        SMDCODE     = 'E' OR
     C                             SMDCODE     = 'P' OR
     C                             SMDCODE     = 'D' OR
     C                             MXMTYP      = 'B' OR
     C                             MXMTYP      = 'F'
     C                   EVAL      D1AMT       = D1AMT + SMDAMT1
     C                   IF        SMDCODE     = 'P'
     C     KYLNMSP       CHAIN     RLNMMOD
     C                   EVAL      MDMODE      = MMDMODE
     C                   ENDIF
     C                   ENDIF
     **
     C*M2001     KYLNMSP       READE     RLNMSPL
M2001C     KYLNMSP       READE     RLNMSPLL1
     C                   ENDDO
     **
M2001C                   ELSE
M2001C                   EXSR      SrSplMrgAmtM
M2001C                   ENDIF
     C                   ENDSR
     **============================================
     ** sr - filter record base on slection
     **============================================
     C     SrFilter      BEGSR
     **
     C
     C                   EVAL      WFilter     = 'Y'
     C
     C     MDAXTC        CHAIN     RLNPRTX
     C                   IF        MXMTYP    =  'F'
     C                   GOTO      ErFilter
     C                   ENDIF
     ** Bank Search
     C                   IF        MDBANK     <> S1BKN  AND
     C                             S1BKN      <> *ZEROS
     C                   GOTO      ErFilter
     C                   ENDIF
     ** Branch Search
     C                   IF        MDCBRN     <> S1RBC  AND
     C                             S1RBC      <> *ZEROS
     C                   GOTO      ErFilter
     C                   ENDIF
     ** A/c number search
     C*R1001             IF        S1ACNO      > *ZEROS AND
     C*R1001                       MDACCT     <> S1ACNO
     C*R1001             GOTO      ErFilter
     C*R1001             ENDIF
     ** Loan info
     C     KYLNMAST      CHAIN     RLNMAST                            91
     C   91KYLNMAST      CHAIN     RLNTNEW                            91
     C                   IF        NOT *IN91
     C*R1006             EVAL      MDSNME      = FSNAME
     C                   EVAL      OPARTID     = PARTID
     C                   EVAL      OMCLNID     = MCLNID
     C                   ENDIF
     **
     C                   IF        APTYPE    =  'G'
     C                   GOTO      ErFilter
     C                   ENDIF
     **
     C                   EVAL      WFilter     = 'N'
     **
     C     ErFilter      ENDSR
M2001**============================================
     ** sr - Split/Mereg Amount for tranche account
M2001**============================================
     C     SrSplMrgAmtM  BEGSR
     **
     C     KYLNMSPM      KLIST
     C                   KFLD                    MDACCT
     C                   KFLD                    MDACTY
     C                   KFLD                    MDTCID
     C                   KFLD                    MDTCD8
     C                   KFLD                    MDBATN
     **
     C     KYLNMSPM      SETLL     RLNMSPL
     C     KYLNMSPM      READE     RLNMSPL
     C                   DOW       NOT %EOF(LNMSPL)
     **
     C                   IF        SMDCODE     = 'E' OR
     C                             SMDCODE     = 'P' OR
     C                             SMDCODE     = 'D' OR
     C                             MXMTYP      = 'B' OR
     C                             MXMTYP      = 'F'
     C                   EVAL      D1AMT       = D1AMT + SMDAMT1
     C                   IF        SMDCODE     = 'P'
     C     KYLNMSPM      CHAIN     RLNMMOD
     C                   EVAL      MDMODE      = MMDMODE
     C                   ENDIF
     C                   ENDIF
     **
     C     KYLNMSPM      READE     RLNMSPL
     C                   ENDDO
     **
     C                   ENDSR
      *--------------------------------------------- END OF CODING --*
     C**/COPY QRPGLEREF,MBQAUTCHK
     ** MBASE-Common C-Spec for Maint File Handler
     C/COPY QMBSREFRLE,MBQTXHC
     ** LOANS-Common C-Spec for Maint File Handler
     C/COPY QLNSREFRLE,LNQTXHC
     C/COPY QLNSREFRLE,LNQFMTHDR
