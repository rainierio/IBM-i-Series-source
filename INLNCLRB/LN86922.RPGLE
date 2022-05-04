OS01 H/COPY QSYSREFRLE,COPYRIGHTH
OS01 H/COPY QMBSREFRLE,MBQTXHH
     F*****************************************************************
     F* Program Name..: LN86922                                       *
     F* Description...: Loan Close and Header Inquiry                 *
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
     FINLNCLRBFLIF   E           K DISK    INFDS(INFDS)
     F                                     prefix(BF_)
     FCFACCT    IF   E           K DISK    
     FDDMAST    IF   E           K DISK    
     FDDTNEW    IF   E           K DISK    RENAME(RDDMAST:RDDTNEW)
      *-- VARIABLE DECLARATION --*
     ** MBASE-Common D-Spec for Maint File Handler
OS01 D/COPY QMBSREFRLE,MBQTXHD
     ** LOANS-Common D-Spec for Maint File Handler
OS01 D/COPY QLNSREFRLE,LNQTXHD
      *
     ** I/R struc
     D DS86922I      E DS                  EXTNAME(LN86922I) PREFIX(PR)
     D DS86922R      E DS                  EXTNAME(LN86922R) PREFIX(OU)
     **
     C     KYCLBRF       KLIST
     C                   KFLD                    prINPRDT
     C                   KFLD                    prINREFN
     C
     C     KYDDMAST      KLIST
     C                   KFLD                    BF_INDDACCT
     C                   KFLD                    BF_INDDACTP
     C
     C*
     ** initialise, map header,
OS01 C                   EXSR      SRINIT
      *
     C                   SELECT
      *
OS01 C                   WHEN      HDACCD      = 'I'
     C                   EXSR      SRINQ
     C                   OTHER
      *-- INVALID HDACCD CODE --*
     C                   EVAL      ERCODE = 'MBM2000'
     C                   EVAL      P8MDTA = HDACCD
OS01 C                   EXSR      SRSENDERR
      *
     C                   ENDSL
      *
     C                   EXSR      SREXIT
      ****************************************************************
      *  SUBROUTINE: SRINQ                                           *
      *  PURPOSE   : INQUIRY SUBROUTINE TO GET RECORD FROM THE FILES *
      ****************************************************************
     CSR   SRINQ         BEGSR
      *-- BREAK THE KEY VALUES --*
     C                   EVAL      DS86922I   = INSTRS  
     C                   EVAL      DS86922R   = DS86922I
     C
     C                   EXSR      SRCVTIN 
      *
      *-- READ FIRST RECORDS --*
      *
     C     KYCLBRF       CHAIN     RINLNCLRBFL                        31
      *
     C                   IF        *IN31 = *ON
      *-- ERROR RETURN --*
     C                   EVAL      ERCODE = 'MBM2001'
     C                   EVAL      P8MDTA = PSPGM
OS01 C                   EXSR      SRSENDERR
     C                   ELSE
      *-- CREATE KEY RESPONSE --*
     C                   EVAL      COUNTR = *ZEROS
      *-- CREATE OCCURANCES RESPONSE --*
     C                   IF        (*IN31 = *OFF) AND
     C                             (COUNTR < HDNREC)
     **
     C                   EXSR      SRCVTOUT
     C                   EVAL      OTSTRS = DS86822R
      *                                               
     C                   EVAL      COUNTR = COUNTR + 1
     C                   EXSR      SRLOCK1            
     C                   ENDIF                        
     **
     C                   ENDIF
      *
     C                   EVAL      HDMREC = 'N'
     C                   EVAL      HDNREC = COUNTR
      *
     C                   ENDIF
      *
     CSR                 ENDSR
     **============================================
OS01 ** sr - SRCVTOUT
     **============================================
     C     SRCVTOUT      BEGSR
     C                   
     ** DDMAST
     C     KYDDMAST      CHAIN     RDDMAST                            33
     C   33KYDDMAST      CHAIN     RDDTNEW                            33
     C                   IF        *IN33 = *OFF
     C                   EVAL      ouACCTNO = dmACCTNO
     C                   EVAL      ouACTYPE = dmACCTTP
     C                   EVAL      ouDDCTYP = dmDDCTYP
     C                   EVAL      ouSCCODE = dmSCCODE
     C                   EVAL      ouCIFNO  = dmCIFNO         
     C                   EVAL      ouBANKNO = dmBANKNO
     C                   EVAL      ouBRANCH = dmBRANCH
     C                   EVAL      ouSTATUS = dmSTATUS
     C                   EVAL      ouCBAL   = dmCBAL
     C** CFACCT
     C     dmCIFNO       CHAIN     RCFACCT                            34
     C                   IF        *IN34 = *OFF
     C                   EVAL      ouINLNACNM = cfCFFMSH
     C                   ENDIF
     C                   ENDIF
     **
     C                   EVAL      DS86922I    = DS86922R
     C                   EVAL      WHDVFMT_KEY = DS86922I
     C                   EVAL      HDVFMT      = WHDVFMT
     C
     C
     C
     **
     C     ERCVTOUT      ENDSR
      *--------------------------------------------- END OF CODING --*
     ** MBASE-Common C-Spec for Record locking
OS01 C/COPY QMBSREFRLE,MBQLCK
     ** MBASE-Common C-Spec for Maint File Handler
OS01 C/COPY QMBSREFRLE,MBQTXHC
     ** LOANS-Common C-Spec for Maint File Handler
OS01 C/COPY QLNSREFRLE,LNQTXHC
