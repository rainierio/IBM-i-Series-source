     H CVTOPT(*VARCHAR)
      ****************************************************************************
      * Program Name..: INI3102                                                  *
      * Description...: Loan - Close and Rebook eTP Upload                       *
      *                 (RPGLE)                                                  *
      * Date Created..: 15/04/2022                                               *
      * Author........: Rainier Letidjawa                                        *
      ****************************************************************************
      * Modification Logs:                                                       *
      * Init  Ref#     Date    Description                                       *
      ****************************************************************************
     ** Input/Output/Update file
     FSSPAR1    IF   E             DISK
     FINTFILEL5 UF   E           K DISK
     FINTHDR    UF A E           K DISK
     FINTDTL    UF A E           K DISK
     FINTDTE    UF A E           K DISK    PREFIX(E_)
     FINPDWE    IF   E           K DISK
     FINI3102W  IF A E             DISK    Commit
     F                                     Prefix(W_)
     FINI3102I  IF   E             DISK    Prefix(I_)
     FINLNCLRBF UF A E           K DISK    Prefix(B_)
     FINPPAR    IF   E           K DISK
     FLNMAST    IF   E           K DISK
     FLNTNEW    IF   E           K DISK    RENAME(RLNMAST:RLNTNEW)
     FDDMAST    IF   E           K DISK    Prefix(DD_)
     FDDTNEW    IF   E           K DISK    RENAME(RDDMAST:RDDTNEW)
     F                                     Prefix(DD_)
     D**************************************************************************
     D* External input data structure
     D HEADER        E DS                  EXTNAME(INI3102H) PREFIX(H_)
     D DETAILS       E DS                  EXTNAME(INI3102D) PREFIX(D_)
     D TRAILER       E DS                  EXTNAME(INI3102T) PREFIX(T_)
     D* Prototypes
     D INI3102Pr       PR                  Extpgm('INI3102')
     D P9FIID                        20A
     D P9REF                         25A
     D P9UID                         10A
     D P9INMBR                       10A
     D P9ERR                          1A
     D P9APPRV                        1A
     D*
     D INI3102Pr       PI
     D P9FIID                        20A
     D P9REF                         25A
     D P9UID                         10A
     D P9INMBR                       10A
     D P9ERR                          1A
     D P9APPRV                        1A
      *
     d dsSCHVLDTAB     ds
     d  intval                 1     30
     d  schcde                 1      1
     d  tabpos                 2      4  0
     d  schvld                 6     30
      *
     D wParaSet        S              1    INZ('N')
     D Yes             C                   'Y'
     D No              C                   'N'
     D bFac            S               N
     D bLoan           S               N
      *
     D                SDS
     D PSPGM                   1     10
     D PSJOBN                244    253
     D PSUSRN                254    263
     D PSJBNO                264    269  0
      *
     D* Working variables
     D WCDATE          S                   LIKE(INDATE)
     D WCTIME          S              6  0
     D WKCHKSTS        S              2S 0                                       Var for check sts
     D WDTLCOUNT       S              9S 0                                       Var for DTL Seq No
     D wChkSum         S             26S 2                                       Var for Check Sum
     D WHDRSEQ         S              9S 0
     D WTSEQNO         S              9S 0
     D WTRLSEQ         S              9S 0                                       Var for TRL Seq No
     D WRCDTYP         S              2A                                         Var for Record Type
     D WFLG            S              2A   INZ('00')                             Error Flag Number
     D WNUMCHK         C                   '0123456789'                          Numeric Checking
     D wEndFile        S              1A   Inz('N')
     D wINERCD         S                   Like(E_INERCD ) Inz(*blank)
     D wINERVAL        S                   Like(E_INERVAL) Inz(*blank)
     D wINRSEQN        S                   Like(E_INRSEQN) Inz(*zero)
     D wINESEQN        S                   Like(E_INESEQN) Inz(*zero)
     D wFailRcd        S              9S 0 Inz(*zero)
     D wSuccRcd        S              9S 0 Inz(*zero)
     D wInputInt       S             17S 0 Inz(*zero)
     D*
     D V9APPV          S                   LIKE(INUSRID)
     D tempAcct        S             19  0
     D tempDate        S              8  0
     D*
     D                 DS
     D Indata                  1   3002
     D wData_wLen                     5U 0 OVERLAY(INDATA:1)
     D WDATA                       3000    OVERLAY(INDATA:3)

     D* Storing the Error Code
     D V9Index         S              3  0 INZ(0)
     D V9ERCD          S                   Dim(999) Like(INERR)                  Var for Error Code
     D V9ERDESC        S                   Dim(999) Like(INERRD)                 Var for Error Code
     D V9CRITICAL      S              1    Dim(999)                              Var for Crtcl.Flag
     D V9CRTI          S              1                                          Var for Crtcl.Flag

     D xNum03          S              3S 0
     D xNum05          S              5S 0
     D xNum08          S              8S 0
     D xNum09          S              9S 0
     D xNum11          S             11S 0
     D xNum17          S             17S 0
     D xNum19          S             19S 0
     D xNum26          S             26S 0

     D* LNMAST LNTNEW working variables
     D wLNACCTNO       S             19S 0
     D wLNSTATUS       S              1S 0
     D wLNACTYPE       S              1A
     D wLNCURTYP       S              4A
     D wLNCIFNO        S             19S 0
     D wLNSBUCOD       S              4A
     D wLNSUBCOD       S              4A

     D* DDMAST DDTNEW Local working variables
     D wINDDACCT       S             19S 0
     D wDDSTATUS       S              1S 0
     D wDDACTYPE       S              1A
     D wDDCURTYP       S              4A
     D wDDCIFNO        S             19S 0

      /Free
       //*******************************************************************************************
       //  Main Proccesing
       //*******************************************************************************************

          If p9APPRV = 'A';
             Exsr srApprove;
             P9ERR = 'N';
             Commit;
          Else;
            If wParaSet = YES;
              Read RINI3102I;
              Dow Not %Eof(INI3102I) and P9Err <> 'Y' and wEndFile = 'N';
                wINRSEQN += 1;
                Exsr srValidate;
                If wRCDTYP = '99' and wTRLSEQ = 1;             // Ignore line (N+1) after trailer (N)
                  wEndFile = 'Y';
                Endif;
              Read RINI3102I;
              Enddo;
              Exsr srINTHDR;
              Exsr srCommit;
            Endif;
            Exsr srINTFILE;
          Endif;

           *Inlr = *On;
       //***********************************************************************
       // Subroutine : SrCommit;
       // Description:
       //***********************************************************************
         Begsr srCommit;

         If %Eof(INI3102I) and wRCDTYP<>'99';
           wINERCD  = 'INI008';
           Exsr srINTDTE;
           // P9ERR     = 'Y';
         Endif;

         If P9ERR = 'Y';
           Rolbk;
         Else;
           P9ERR = 'N';
           Commit;

          // Write to the log file
          Exsr SrLogfile;

         Endif;

         Endsr;
       //***********************************************************************
       // Subroutine: SrVALIDATE
       // Description: Validate records
       //***********************************************************************
         Begsr SrVALIDATE;

           CLEAR RINTDTE;
           WDATA  = I_INDATA;

           // Check the record type for write the HEADER , DETAILS & TRAILER
           WRCDTYP = %subst(I_INDATA:1:2);

           If wINRSEQN=1 and wRCDTYP<>'00';
             wINERCD  = 'INI013';
             Exsr srINTDTE;
             // P9ERR     = 'Y';
           Else;
             Select;

             // Header Record
             When WRCDTYP ='00';
               Clear HEADER;
               HEADER = I_INDATA;
               WHDRSEQ += 1;
               Exsr SRCHKHDR;
               Exsr SrWriteHdr;
             // Detail Record
             When WRCDTYP = '01';
               Clear DETAILS;
               DETAILS = I_INDATA;
               WDTLCOUNT += 1;
               Exsr SRCHKDTL;
               wData = DETAILS;
               Exsr SrMOVFLD;

             // If Trailer and already have detail records
             When WRCDTYP = '99' AND WDTLCOUNT <> 0;
               Clear TRAILER;
               TRAILER = I_INDATA;
               WTRLSEQ += 1;
               Exsr SRCHKTRL;
               wdata = TRAILER;

             // If Trailer but still no detail records
             when WRCDTYP = '99' AND WDTLCOUNT = 0;
               wINERCD = 'INI010';
               INRMK1  = 'Detail rcd not found';
               Exsr srINTDTE;

             Other;
               wINERCD  = 'INI007';
               wINERVAL = 'RcdTyp:'+wRCDTYP;
               Exsr srINTDTE;
             Endsl;

           // Write into INTDTL
           Exsr srINTDTL;
           Endif;

         Endsr;
       //***********************************************************************
       // Subroutine  : SrCHKHDR
       // Description : Check Header Record
       //***********************************************************************
         Begsr SrCHKHDR;

         // If error record happen other than record 1
         If wHdrSeq <> 1;
          wINERCD    = 'INI001';
          wINERVAL   = 'HeaderID Not valid';
          // P9ERR = 'Y';
          Exsr srINTDTE;
          Leavesr;
         Endif;


         // Check date format
         Test(DE) *ISO0 H_INPRDT;
         If %Error;
          wINERCD    = 'INI002';
          wINERVAL   = 'Process date not valid';
          // P9ERR = 'Y';
          Exsr srINTDTE;
         Endif;

         // Check date
         If H_INPRDT <> %CHAR(SP1NB8);
          wINERCD    = 'INI003';
          wINERVAL   = 'ProcessDt:'+H_INPRDT;
          // P9ERR = 'Y';
          Exsr srINTDTE;
         Endif;

         // Check agreed system source
           If H_INSRCS <> 'INLNCLRB';
            wINERCD    = 'INI005';
            wINERVAL   = 'SourceSys not valid';
         //  P9ERR = 'Y';
            Exsr srINTDTE;
           Endif;

         Endsr;
       //***********************************************************************
       // Subroutine  : SrCHKDTL
       // Description : Check Detail Record
       //***********************************************************************
          Begsr SrCHKDTL;

          Clear RINI3102W;
          Reset RINI3102W;

          // Initialize all Variable
          Exsr srReset;

          // Validate Transaction Type
          If D_INRCDID = *blank;
            wINERCD  = 'INI011'; 
            wINERVAL = 'Trans type '+D_INTRNTYP;
            Exsr srINTDTE;
          Else;
            if D_INTRNTYP <> 'S' AND D_INTRNTYP <> 'D';
            wINERCD  = 'INI011';
            wINERVAL = 'TranType '+D_INTRNTYP;
            Exsr srINTDTE;
            Endif;
          Endif;


          // Validate Loan account number
          If D_INACCTNO <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(D_INACCTNO):1);
          Else;
            WKCHKSTS = 1;
            D_INACCTNO = *all'0';
          Endif;

          If WKCHKSTS > 0;
            wINERCD  = 'INI004';
            wINERVAL = 'LoanAcc '+D_INACCTNO;
            Exsr srINTDTE;
            D_INACCTNO = *all'0';
          Else;
            wLNACCTNO = %Dec(%Trim(D_INACCTNO):19:0);

            // Loan account check
            Chain wLNACCTNO RLNMAST;
            If Not %found(LNMAST);
              Chain wLNACCTNO RLNTNEW;
              If Not %found(LNTNEW);
                wINERCD  = 'INI015';
                wINERVAL = 'LoanAcc '+D_INACCTNO;
                Exsr srINTDTE;
              Endif;
            Endif;

            If %found(LNMAST) Or %found(LNTNEW);
              If STATUS = 1 or STATUS = 4;
                wLNSTATUS = STATUS;
                wLNACTYPE = ACTYPE;
                wLNCURTYP = CURTYP;
                wLNCIFNO  = CIFNO;
                wLNSBUCOD = SBUCOD;
                wLNSUBCOD = SUBCOD;
              Else;
                wINERCD  = 'INI016';
                wINERVAL = 'LoanAcc '+D_INACCTNO;
                Exsr srINTDTE;
              Endif;
            Endif;
          Endif;

          // Validate CASA account number
          If D_INDDACCT <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(D_INDDACCT):1);
          Else;
            WKCHKSTS = 1;
            D_INDDACCT = *all'0';
          Endif;

          If WKCHKSTS > 0;
            wINERCD  = 'INI004';
            wINERVAL = 'CasaAcc '+D_INDDACCT;
            Exsr srINTDTE;
            D_INDDACCT = *all'0';
          Else;
            wINDDACCT = %Dec(%Trim(D_INDDACCT):19:0);

            // CASA account check
            Chain wINDDACCT RDDMAST;
            If Not %found(DDMAST);
              Chain wINDDACCT RDDTNEW;
              If Not %found(DDTNEW);
                wINERCD  = 'INI015';
                wINERVAL = 'CasaAcc '+D_INDDACCT;
                Exsr srINTDTE;
              Endif;
            Endif;

            If %found(DDMAST) Or %found(DDTNEW);
              If DD_STATUS = 1 or DD_STATUS = 4;
                wDDSTATUS = DD_STATUS;
                wDDACTYPE = DD_ACTYPE;
                wDDCURTYP = DD_DDCTYP;
                wDDCIFNO  = DD_CIFNO;

                If wLNCURTYP <> *Blanks and wLNCIFNO <> *Zeros;
                  // Currency type checking
                  If wLNCURTYP <> wDDCURTYP;
                    wINERCD = 'INI012';
                    wINERVAL = 'Different Currency';
                    Exsr srINTDTE;
                  // Loan and CASA CIF checking
                  ElseIf wLNCIFNO <> wDDCIFNO;
                    wINERCD = 'INI012';
                    wINERVAL = 'Different CIF';
                    Exsr srINTDTE;
                  Endif;
                Endif;

              Else;
                wINERCD  = 'INI016';
                wINERVAL = 'CasaAcc '+D_INDDACCT;
                Exsr srINTDTE;
              Endif;
            Endif;

          Endif;

          // Validate Transactions amount
          If D_INTRNAMT <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(D_INTRNAMT):1);
          Else;
            WKCHKSTS = 1;
            D_INTRNAMT = *all'0';
          Endif;

          If WKCHKSTS > 0;
            wINERCD  = 'INI004';
            wINERVAL = 'Amnt '+D_INTRNAMT;
            Exsr srINTDTE;
            D_INTRNAMT = *all'0';
          Else;
            xNum17 = %Dec(%Trim(D_INTRNAMT):17:0);
            D_INTRNAMT = %Editc(xNum17:'X');
          Endif;

          If wINERCD <> *blank;
            wFailRcd += 1;
          Else;
            wSuccRcd += 1;
          Endif;

         Endsr;
       //***********************************************************************
       // Subroutine  : SrMovFld
       // Description : Write into working file
       //***********************************************************************
        Begsr SrMovFld;

          W_INFIID      = INFIID    ;                          // Interface Identifier
          W_INREFN      = INREFN    ;                          // Reference  NUmberier
          W_INFNAM      = INFNAM    ;                          // Filename
          W_UPLDID      = INUPUSR   ;                          // Maker User ID

          If p9APPRV = 'A';
            W_APRVID      = P9UID   ;                          // Checker User ID
            W_INSEQ       = INRSEQN ;                          // Sequence Number
          Else;
            W_APRVID      = *blanks ;                          // Checker User ID
            W_INSEQ       = wINRSEQN;                          // Sequence Number
          Endif;
          W_INRCDID     = *blanks;                             // Record Identifier

          W_INTRNTYP    = D_INTRNTYP;                          // Transaction Type
          W_INACCTNO    = %Int(D_INACCTNO);                    // Loan Accont Number
          W_INLNACTP    = wLNACTYPE;                           // Loan Account Type
          W_INLNACNM    = D_INLNACNM;                          // Loan Account Name
          W_INLNSTAT    = wLNSTATUS;                           // Loan Account Status
          W_INDDACCT    = %Int(D_INDDACCT);                    // CASA Account Number
          W_INDDACTP    = wDDACTYPE;                           // CASA Account Type
          W_INDDACNM    = D_INDDACNM;                          // CASA Account Name
          W_INDDSTAT    = wDDSTATUS;
          W_INCFCIFN    = wLNCIFNO;                            // Loan CIF number
          W_INTRNAMT    = %Dec(D_INTRNAMT:17:2)/100;           // Transaction Amount
          W_INCURTYP    = wDDCURTYP;                           //  CASA Currency Type

          If W_INCURTYP = 'IDR';
            W_INCURFLG = 'L';                                  //  Transaction Currency flag local
          Else;
            W_INCURFLG = 'F';                                  //  Transaction Currency flag foreign
          Endif;

          W_INMDRMRK    = D_INMDRMRK;                          // Remarks
          W_INSBUCDE    = wLNSBUCOD;                           // SBU Code
          W_INSUBCDE    = wLNSUBCOD;                           // SUB Code

          wChkSum       += W_INTRNAMT;                         // Sum all the Trans amount
          W_INPRDT      = INDATE;                              // Interface Processing Date
          W_INPRTM      = INTIME;

           Write RINI3102W;

         Endsr;
       //***********************************************************************
       // Subroutine  : SrLogfile
       // Description : Write into log file - INLNCLRBF
       //***********************************************************************
        Begsr SrLogfile;

          Setll *START INI3102W;
          Read RINI3102W;
          Dow Not %Eof(INI3102W);
            B_INFIID      = W_INFIID;                            // Interface Identifier
            B_INREFN      = W_INREFN;                            // Reference  Number
            B_INFNAM      = W_INFNAM;                            // Filename
            B_INSEQ       = W_INSEQ;                             // Sequence Number
            B_UPLDID      = W_UPLDID;                            // Maker User ID
            B_APRVID      = W_APRVID;                            // Checker User ID
            B_INRCDID     = W_INRCDID;                           // Record Identifier
            B_INTRNTYP    = W_INTRNTYP;                          // Transaction Type
            B_INACCTNO    = W_INACCTNO;                          // Loan Accont Number
            B_INLNACTP    = W_INLNACTP;                          // Loan Account Type
            B_INLNACNM    = W_INLNACNM;                          // Loan Account Name
            B_INLNSTAT    = W_INLNSTAT;                          // Loan Account Status
            B_INDDACCT    = W_INDDACCT;                          // CASA Account Number
            B_INDDACTP    = W_INDDACTP;                          // CASA Account Type
            B_INDDACNM    = W_INDDACNM;                          // CASA Account Name
            B_INDDSTAT    = W_INDDSTAT;                          // CASA Account Status
            B_INCFCIFN    = W_INCFCIFN;                          // CIF Number
            B_INTRNAMT    = W_INTRNAMT;                          // Transaction Amount
            B_INCURTYP    = W_INCURTYP;                          // CASA Currency Type
            B_INCURFLG    = W_INCURFLG;                          // Transaction Currency flag
            B_INMDRMRK    = W_INMDRMRK;                          // Remarks
            B_INSBUCDE    = W_INSBUCDE;                          // SBU Code
            B_INSUBCDE    = W_INSUBCDE;                          // SUB Code
            B_INPRDT      = W_INPRDT;                            // Interface Processing Date
            B_INSTS       = W_INSTS;                             // Interface Status
            B_INERDCD     = W_INERDCD;                           // IFC Error Code
            B_INERDSC     = W_INERDSC;                           // IFC Error Description
            B_INERRDT     = W_INERRDT;                           // IFC Error Date
            B_INPRDT      = W_INPRDT;                            // Interface Processing Date
            B_INPRTM      = W_INPRTM;                            // Interface Processing Time
            B_PRCSTS      = W_PRCSTS;                            // Application Status AA/AB
            B_PRCTIM      = W_PRCTIM;                            // Application Time
            B_ERRCDE      = W_ERRCDE;                            // Error Code
            B_ERRDSC      = W_ERRDSC;                            // Error Description

            Write RINLNCLRBF;
          Read RINI3102W;
          Enddo;
        Endsr;

       //***********************************************************************
       // Subroutine  : srApproveLog
       // Description : Update into log file when approve
       //***********************************************************************
        Begsr srApproveLog;
          Chain (INFIID:INREFN:INRSEQN) RINLNCLRBF;
          If %found(INLNCLRBF);
            B_APRVID = W_APRVID;                                  // Checker User ID
            B_INSTS  = W_INSTS;                                   // Interface Status
            B_PRCSTS = W_PRCSTS;                                  // Application Status AA/AB
            B_INERDCD = W_INERDCD;                                // IFC Error Code
            B_INERDSC = W_INERDSC;                                // IFC Error Description
            B_INERRDT = W_INERRDT;                                // IFC Error Data
            B_INPRDT  = W_INPRDT;                                 // Interface Processing Date
            B_INPRTM  = W_INPRTM;                                 // Interface Processing Time

            // Initial simulation data are not updated here ini INILNCLRBF

            update RINLNCLRBF %fields(B_APRVID:B_INSTS:B_PRCSTS:
            B_INERDCD:B_INERDSC:B_INERRDT:B_INPRDT:B_INPRTM);
          Endif;
        Endsr;
       //***********************************************************************
       // Subroutine  : SRCHKTRL
       // Description : Check Trailer record
       //***********************************************************************
         Begsr SRCHKTRL;

          // Total Records
          If T_INDTL <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(T_INDTL):1);
            If WKCHKSTS = 0;
              xNum09 = %Dec(%Trim(T_INDTL):9:0);
              T_INDTL = %EditC(xNum09:'X');

              If T_INDTL <> %EditC((wDtlCount):'X');
                wINERCD    = 'INI006';
                wINERVAL   = 'InvalidDetailCount: '+%TrimL(T_INDTL:'0')+
                ' <> '+ %triml(%EditC((wDtlCount):'X'):'0');
                // P9Err = 'Y';
                Exsr srINTDTE;
              Endif;
            Else;
              wINERCD    = 'INI006';
              wINERVAL   = 'InvalidDetailCount:'+T_INDTL;
              // P9Err = 'Y';
            Endif;
          Endif;

          // Total Check Sum Amount  - Temporary disabled
       // If T_INCHKSUM <> *blank;
       //   WKCHKSTS = %CHECK(WNUMCHK:%Trim(T_INCHKSUM):1);
       //   If WKCHKSTS = 0;
       //     xNum26 = %Dec(%Trim(T_INCHKSUM):26:0);
       //     T_INCHKSUM = %EditC(xNum26:'X');
       //
       //     If T_INCHKSUM <> %EditC((wChkSum):'X');
       //       wINERCD    = 'INI014';
       //       wINERVAL   = 'InvalidDetailChksum: '+
       //       %Triml(T_INCHKSUM:'0')+' <> '
       //       +%Triml( %EditC((wChkSum):'X'):'0');
                // P9Err = 'Y';
       //       Exsr srINTDTE;
       //     Endif;
       //   Else;
       //     wINERCD    = 'INI007';
       //     wINERVAL   = 'InvalidDetailChksum: '+%Triml(T_INCHKSUM:'0');
              // P9Err = 'Y';
       //     Exsr srINTDTE;
            //Leavesr;
       //   Endif;
       // Endif;


         Endsr;
       //****************************************************************
       // Subroutine: SrReset
       // Description: Reset variable
       //****************************************************************
         Begsr srReset;
           W_INSTS = 'S';
           wINERCD    = *blank;
           W_INERDCD  = *blank;
           W_INERRDT  = *blank;
           Clear tempAcct;
           Clear tempDate;
         Endsr;
       //****************************************************************
       // Subroutine: SrWriteHdr
       // Description:
       //****************************************************************
         Begsr SrWriteHdr;
           INSEQN = 1;
           Write RINTHDR;
         Endsr;
       //****************************************************************
       // Subroutine: SrINTHDR
       // Description:
       //****************************************************************
         Begsr srINTHDR;

          setll (P9FIID:P9REF) Intfilel5;
          if %equal(intfilel5);
           read(n) intfilel5;
          endif;

          Chain (INFIID:INFNAM:INDATE:INFSEQ:INSEQN) RINTHDR;
          If %found(INTHDR);
           If P9ERR = 'Y';
            INDTLN = *zero;
            INRMK1 = 'File Structure Err';
           Else;
            INDTLN = wINRSEQN;                         //Total incl hdr & trl  & trl
           Endif;
           If not %Eof(INI3102I) and wEndFile = 'Y';
            wINERCD  = 'INI009';
            wINERVAL = 'TRL:99:Ignore R(N+1)';
            INRMK1   = 'TRL:99:Ignore R(N+1)';
            Exsr srINTDTE;
           Endif;
           update RINTHDR %fields(INDTLN:INRMK1);
          Endif;

         Endsr;
       //****************************************************************
       // Subroutine  : SrINTDTL
       // Description : Keep Track every record into INTDTL file
       //****************************************************************
         Begsr srINTDTL;
           INSEQN     = 1;
           INRSEQN    = wINRSEQN  ;
           wData_wLen = %Len(%Trimr(wdata));
           WRite RINTDTL;
         Endsr;
       //****************************************************************
       // Subroutine: SrINTDTE
       // Description:
       //****************************************************************
         Begsr srINTDTE;

           W_INSTS  = 'R';
           W_PRCSTS = 'AB';

           V9Index = *zero;
           V9Index = %LookUp(wINERCD:V9ErCd);
           // V9CRTI = V9CRITICAL(V9Index);
           Monitor;
           P9ERR = V9CRITICAL(V9Index);
           On-Error;
           P9ERR = 'Y';
             wINERVAL = 'Parameter Error : ' + wINERCD;
             wINERCD  = 'ER9999';
           EndMon;
           If V9INDEX <> 0;
             W_INERDCD  = wINERCD;
             W_INERDSC  = V9ERDESC(V9INDEX);
             W_INERRDT  = wINERVAL;
           Endif;

           If p9APPRV <> 'A';
             E_INFIID = INFIID;
             E_INFNAM = INFNAM;
             E_INFSEQ = INFSEQ;
             E_INDATE = INDATE;
             E_INTIME = INTIME;
             E_INSEQN = 1;
             E_INIFERR = 'Y';
             E_INERCD  = wINERCD;
             E_INERVAL = wINERVAL;
             E_INRSEQN = wINRSEQN;
             E_INESEQN=wINESEQN+1;
             Write RINTDTE;
             wINESEQN=E_INESEQN;
           Endif;

         Endsr;
       //****************************************************************
       // Subroutine : srINTFILE
       // Description:
       //****************************************************************
         Begsr srINTFILE;

          Chain (P9FIID:P9REF) RINTFILE;
          If %found(INTFILEL5);
             INPGM  = 'INI3102C';
             If P9ERR = 'Y';
               INTTHD = 0;
               INTOTR = 0;
               INSTS  = '2';                                      // reject
             else;
               INTOTR = wINRSEQN - 2;
               INSTS  = '3';                                      // success
             Endif;
             update RINTFILE %fields(INPGM:INTTHD:INTOTR:INSTS);
          Endif;

         Endsr;
       //***********************************************************************
       // Subroutine: SrApprove
       // Description:
       //***********************************************************************
          Begsr srApprove;

           Setll (INFIID:INFNAM:INDATE:INFSEQ) RINTDTL;
           Reade (INFIID:INFNAM:INDATE:INFSEQ) RINTDTL;
           Dow Not %Eof(INTDTL);
            If %subst(wdata:1:2) <> '00' and %subst(wdata:1:2) <> '99';
          // Chain (INFIID:INFNAM:INDATE:INFSEQ:INSEQN:INRSEQN) RINTDTE;
          // If not %found (INTDTE);
              DETAILS = wdata;
              Exsr srChkDtl;
              Exsr srMovFld;
              Exsr srApproveLog;
          // Endif;
            Endif;
           Reade (INFIID:INFNAM:INDATE:INFSEQ) RINTDTL;
           Enddo;

          Endsr;
       //***********************************************************************
       // Subroutine: *INZSR
       // Description: Initialization
       //***********************************************************************
         Begsr *INZSR;

              READ SSPAR1;
              INFIID = P9FIID;                                 // INTFILE's keys value
              INREFN = P9REF;

              Setll (INFIID:INREFN) IntfileL5;
              if %equal(IntfileL5);
               read(n) IntfileL5;
              endif;

              WCDATE = %INT(%CHAR(%DATE():*ISO0));
              WCTIME = %INT(%CHAR(%TIME():*HMS0));

              // Load error codes into table
              Setll (INFIID) RINPDWE;
              Reade (INFIID) RINPDWE;
              DOW INFIID = INDWNC And Not %Eof(INPDWE);

               V9INDEX = V9INDEX + 1;
               V9ERCD(V9INDEX) = INERR;
               V9CRITICAL(V9INDEX) = INCRTI;
               V9ERDESC(V9INDEX) = INERRD;
               wParaSet = YES;

              Reade (INFIID) RINPDWE;
              Enddo;
         ENDSR;
       //***********************************************************************
      /End-Free
