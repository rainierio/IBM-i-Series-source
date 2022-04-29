     H COPYRIGHT(' Copyright Silverlake System Sdn. Bhd. 1993, 2011          ')
     H OPTION(*SRCSTMT:*NODEBUGIO)
     H BNDDIR('DDBNDDIR')
     H/COPY QSYSREFRLE,COPYRIGHTH
     F*****************************************************************
     F* Program Name..: LN86922                                       *
     F* Description...: Loan Close and Rebook Inquiry                 *
     F* Date Created..: 29 April 2022                                 *
     F* Author........: Rainier Letidjawa                             *
     F* Ref no........:                                               *
     F*****************************************************************
     F* LN86922 is part of Silverlake Integrated Banking System       *
     F* Copyright 1996 by Silverlake System Sdn Bhd Kuala Lumpur,     *
     F* Malaysia.                                                     *
     F*****************************************************************
     F* Modification Logs:                                            *
     F* Init  RefN     Date    Description                            *
     F* ----  ----     ----    -----------                            *
     F*                                                               *
      *-- File Declaration --*
     FCFAGRP    IF   E           K DISK    INFDS(INFDS)
     F
      *-- Variable declaration --*
     D/COPY QMBSREFRLE,MB8TXNDS
     D
     D wrkDATE8        S              8  0
     D INFDS           DS
     D  INFILE                83     92
     D  DB_RRN               397    400I 0
     D
     D  lck_FILE       S             10    INZ('CFAGRP')
     D
     D
     D               EUDS                  EXTNAME(SSAPAR)
     D* Input/Output Data Structure
     D dsInput       E DS                  EXTNAME(DD26810I)   PREFIX(pr)
     D dsOutput      E DS                  EXTNAME(DD26810R)   inz
     D/copy qsysrefrle,prSSDATE
     D CFAGRP_key      DS
     D   i_ACCTNO                          Like(ACCTNO) Inz
     D   i_ACTYPE                          Like(ACTYPE) Inz
     D   i_ACCREF                          Like(ACCREF) Inz
     D   i_CFATYP                          Like(CFATYP) Inz
     D
     D*************************
     D* Account Name Structure
     D*************************
     D                 DS
     D NAME1                   1     80
     D NAME2                  81    200
     D CFANA1                  1     40
     D CFANA2                 41     80
     D CFANA3                 81    120
     D CFANA4                121    160
     D CFANA5                161    200
     C
     C*-- Parameter and Key Lists declaration --*
     C     *ENTRY        PLIST
     C                   Parm                    QRECV
     C                   Parm                    QSEND
     C                   Parm                    DSPARM          100
     C
     C     pTXNDFT       PLIST
     C                   Parm                    TXCD             10
     C                   Parm                    PGMERR            1
     C                   Parm                    ACTION
     C                   Parm                    NUMREC
     C                   Parm                    PRBNKN
     C                   Parm                    pFORMAT          10
     C                   Parm                    TDUAL             1
     C                   Parm                    gDATE8            8 0
     C                   Parm                    APLCOD            2
     C
     C     pTXNAUT       PLIST
     C                   Parm                    TXCD             10
     C                   Parm                    gAUTCHK           1
     C
     C     MSTKY2        Klist
     C                   Kfld                    prACCTNO
     C                   Kfld                    prACTYPE
     C*
     C     MSTKY1        Klist
     C                   Kfld                    prACCTNO
     C*
     C
     C
     C
     C* Receive Header
     C                   Eval      MBHEADR = RECVHDR
     C                   Clear                   QSEND
     C                   Clear                   MESGST
     C
     C*-- Program mainline --*
     C                   If        (gLoadOnce = '0')
     C                             OR HDTCOD <> TXCD
     C                   Eval      TXCD = HDTCOD
     C                   Call      'MBTXNDFT'    pTXNDFT
     C                   If        PGMERR <> *BLANKS
     C                   Eval      MESGID = 'MBM0001'
     C                   Movel(p)  HDTCOD        MESSG1
     C                   Exsr      SendErr
     C                   EndIf
     C                   Call      'MBTXNAUT'    pTXNAUT
     C                   Eval      gLoadOnce = '1'
     C                   EndIf
     C
     C                   Select
     C
     C                   When      HDACCD = 'I'
     C                   Exsr      INQSBR
     C                   Other
     C*-- INVALID ACTION CODE --*
     C                   Eval      MESGID = 'MBM2000'
     C                   Eval      MESSG1 = ACTION
     C                   Eval      FINAME = *BLANKS
     C                   Exsr      SendErr
     C
     C                   EndSL
     C
     C                   Eval      HDRIND  = GOOD
     C                   Eval      SENDHDR  = MBHEADR
     C
     C                   Return
     C*-- SUBROUTINE DECLARATION --*
     C****************************************************************
     C*  SendErr - Retrieve Error Messages                           *
     C****************************************************************
     C     SendErr       BegSr
     C
     C                   Call      'M8RTVMSG'
     C                   Parm                    MESGID
     C                   Parm                    HDLGID
     C                   Parm                    MSGDTA
     C                   Parm                    MESGST
     C                   Eval      MBERROR = MESGST
     C                   Eval      ERDESC  = %Trim(HDTCOD)
     C                                       + ':'
     C                                       + ERDESC
     C                   Eval      ERTYPE = 'E'
     C                   Eval      ERFLDN = FINAME
     C                   Eval      OTSTRS = MBERROR
     C                   Eval      HDNERR = ERRCON
     C                   Eval      MSGDTA = *BLANKS
     C
     C                   EVAL      HDRIND = BAD
     C                   EVAL      HDNERR = 1
     C                   Eval      SENDHDR  = MBHEADR
     C                   Return
     C
     C                   EndSr
     C****************************************************************
     C*  SUBROUTINE: INQSBR                                          *
     C*  PURPOSE   : INQUIRY SUBROUTINE TO GET RECORD FROM THE FILES *
     C****************************************************************
     C     INQSBR        BegSr
     C*-- Get input values ------*
     C                   Clear                   dsOutput
     C                   Eval      dsInput       = INSTRS
     C
     C                   Eval      COUNTR = *ZEROS
     C
     C     MSTKY2        CHAIN     RCFAGRP                            31
     C
     C                   If        *IN31 = *ON
     C*-- ERROR Return --*
     C                   Eval      MESGID = 'MBM2001'
     C                   Eval      FINAME = *BLANKS
     C                   Exsr      SendErr
     C                   Else
     C
     C                   Exsr      FmtResp
     C
     C                   Exsr      RcdLock
     C
     C                   EVAL      HDVFMT  = 'S' + CFAGRP_key
     C
     C                   Eval      %SUBST(OTSTRS:1:%SIZE(dsOUTPUT))
     C                             = dsOutput
     C                   Eval      COUNTR = 1
     C
     C                   EndIf
     C
     C                   Eval      HDMREC = 'N'
     C                   Eval      HDNREC = COUNTR
     C
     C                   EndSr
      ****************************************************************
      * SUBROUTINE: FmtResp                                          *
      * PURPOSE   : Format reponse before passing out                *
      ****************************************************************
     C     FmtResp       BEGSR
     C
     C* Return account name in 2 lines
     C                   Eval      CURNAM1 = CFANA1 + CFANA2
     C                   Eval      CURNAM2 = CFANA3 + CFANA4 + CFANA5
     C                   Eval      CURSNME = CFSNME
     C                   Eval      EXTNAM1 = CFANA1 + CFANA2
     C                   Eval      EXTNAM2 = CFANA3 + CFANA4 + CFANA5
     C                   Eval      EXTSNME = CFSNME
     C
     C
     C* Convert timestamp to time
     C                   Eval      WMNLTIM =  pSSTNUM(MNLTIM)
     C
     C                   Eval      i_ACCTNO = ACCTNO
     C                   Eval      i_ACTYPE = ACTYPE
     C                   Eval      i_ACCREF = ACCREF
     C                   Eval      i_CFATYP = CFATYP
     C
     C                   ENDSR
     C*--------------------------------------------- END OF CODING --*
     C/COPY QMBSREFRLE,MB8RCLCK
