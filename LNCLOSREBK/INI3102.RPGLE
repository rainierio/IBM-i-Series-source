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
     FINI3102WL1IF   E           K DISK    RENAME(RINI3102W:RINI3102W1)
     F                                     Prefix(V_)
     FINPPAR    IF   E           K DISK
     FLNMAST    IF   E           K DISK
     FLNTNEW    IF   E           K DISK    RENAME(RLNMAST:RLNTNEW)
     FDDMAST    IF   E           K DISK    Prefix(DD_)
     FDDTNEW    IF   E           K DISK    RENAME(RDDMAST:RDDTNEW)
     F                                     Prefix(DD_)
     FLNPARX    IF   E           K DISK
     FLNINFM    IF   E           K DISK
     FCLCOLD    IF   E           K DISK
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

     // Local working variables
     D wLNSTATUS       S              1S 0 
     D wLNACTYPE       S              1A
     D wLNCURTYP       S              4A
     D wLNCIFNO        S             19S 0
     D wLNSBUCOD       S              4A
     D wLNSUBCOD       S              4A
     
     // Local working variables
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
          wINERVAL   = 'HeaderID:'+I_INDATA;
          // P9ERR = 'Y';
          Exsr srINTDTE;
          Leavesr;
         Endif;


         // Check date format
         Test(DE) *ISO0 H_INPRDT;
         If %Error;
          wINERCD    = 'INI002';
          wINERVAL   = 'ProcessDt:'+H_INPRDT;
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
           If H_INSRCS <> 'INLNR1';
            wINERCD    = 'INI005';
            wINERVAL   = 'SourceSys:'+H_INSRCS;
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
            wINERVAL = 'Transaction Type is mandatory'+D_INRCDID;
            Exsr srINTDTE;
          Else;
            if D_INRCDID <> 'S' AND D_INRCDID <> 'D';
            wINERCD  = 'INI012';
            wINERVAL = 'Only S or D are allowed'+D_INRCDID;
            Exsr srINTDTE;
            Endif;
          Endif;


          // Validate Loan account number
          If D_INACCTNO <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(D_INACCTNO):1);
          Else;
            D_INACCTNO = *all'0';
            WKCHKSTS = %CHECK(WNUMCHK:D_INACCTNO:1);
          Endif;

          If WKCHKSTS > 0;
            wINERCD  = 'INI004';
            wINERVAL = 'Loan Acc No: '+D_INACCTNO;
            Exsr srINTDTE;
            D_INACCTNO = *all'0';
          Else;
            xNum19 = %Dec(%Trim(D_INACCTNO):19:0);
            D_INACCTNO = %EditC(xNum19:'X');
            
            // Loan account check
            Chain D_INACCTNO LNMAST;
            If Not %found(LNMAST);                    
              Chain D_INACCTNO LNTNEW;                
              If Not %found(LNTNEW);                  
                wINERCD  = 'INI015'; 
                wINERVAL = 'Loan Acc No: '+D_INLFACCT;
                Exsr srINTDTE;                        
              Endif;                                  
            Endif;                                      

            If %found(LNMAST) Or %found(LNTNEW);
              wLNSTATUS = STATUS;
              wLNACTYPE = ACTYPE; 
              wLNCURTYP = CURTYP;
              wLNCIFNO  = CIFNO; 
              wLNSBUCOD = SBUCOD;
              wLNSUBCOD = SUBCOD;
            Endif;

          Endif;

          // Loan account number is mandatory
          If D_INACCTNO = *all'0';
            wINERCD  = 'INI011 ';
            wINERVAL = 'Loan Acc No: '+ D_INACCTNO;
            Exsr srINTDTE;
          Endif;

          // Validate CASA account number
          If D_INDDACCT <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(D_INDDACCT):1);
          Else;
            D_INDDACCT = *all'0';
            WKCHKSTS = %CHECK(WNUMCHK:D_INDDACCT:1);
          Endif;

          If WKCHKSTS > 0;
            wINERCD  = 'INI004';
            wINERVAL = 'CASA Acc No: '+D_INDDACCT;
            Exsr srINTDTE;
            D_INACCTNO = *all'0';
          Else;
            xNum19 = %Dec(%Trim(D_INDDACCT):19:0);
            D_INDDACCT = %EditC(xNum19:'X');

            // CASA account check
            Chain D_INDDACCT DDMAST;
            If Not %found(DDMAST);                    
              Chain D_INDDACCT DDTNEW;                
              If Not %found(DDTNEW);                  
                wINERCD  = 'INI015';
                wINERVAL = 'CASA Acc No: '+D_INDDACCT;
                Exsr srINTDTE;                        
              Endif;                                  
            Endif;                                      

            If %found(DDMAST) Or %found(DDTNEW);
              wDDSTATUS = DD_STATUS;
              wDDACTYPE = DD_ACTYPE;
              wDDCURTYP = DD_CURTYP;
              wDDCIFNO = DD_CIFNO;
            Endif;
                          
          Endif;

          // CASA account is mandatory
          If D_INDDACCT = *all'0';
            wINERCD  = 'INI011 ';
            wINERVAL = 'CASA Acc No: '+ D_INDDACCT;
            Exsr srINTDTE;
          Endif;

          // Validate Transactions amount
          If D_INTRNAMT <> *blank;
            WKCHKSTS = %CHECK(WNUMCHK:%Trim(D_INTRNAMT):1);
          Else;
            D_INTRNAMT = *all'0';
            WKCHKSTS = %CHECK(WNUMCHK:D_INTRNAMT:1);
          Endif;

          If WKCHKSTS > 0;
            wINERCD  = 'INI004';
            wINERVAL = 'Transaction amount: '+D_INTRNAMT;
            Exsr srINTDTE;
            D_INTRNAMT = *all'0';
          Else;
            xNum17 = %Dec(%Trim(D_INTRNAMT):17:0);
            D_INTRNAMT = %EditC(xNum17:'X');
          Endif;

          // Transaction Amount is mandatory
          If D_INTRNAMT = *all'0';
            wINERCD  = 'INI011 ';  
            wINERVAL = 'Transaction amnt: '+ D_INTRNAMT;
            Exsr srINTDTE;
          Endif;

          // Loan and CASA CIF checking
          If wLNCIFNO <> wDDCIFNO;
            wINERCD = 'INI012';
            wINERVAL = 'Account must be under the same CIF'+wLNCIFNO+' '+wDDCIFNO;
            Exsr srINTDTE;
          Endif

          // Currency type checking
          If wLNCURTYP <> wDDCURTYP;
            wINERCD = 'INI012';
            wINERVAL = 'Account currency type must be the same'+wLNCURTYP+' '+wDDCURTYP;
            Exsr srINTDTE;
          Endif

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
          W_INRCDID     = " ";                                 // Record Identifier

          W_INTRNTYP    = D_INTRNTYP;                          // Transaction Type
          W_INACCTNO    = %Dec(D_INACCTNO:19:0);               // Loan Accont Number
          W_INLNACTP    = wLNACTYPE;                           // Loan Account Type 
          W_INLNACNM    = D_INLNACNM;                          // Loan Account Name
          W_INDDACCT    = %Dec(D_INDDACCT:19:0);               // CASA Account Number
          W_INDDACTP    = wDDACTYPE;                           // CASA Account Type
          W_INDDACNM    = D_INDDACNM;                          // CASA Account Name
          W_INTRNAMT    = %Dec(D_INTRNAMT:17:0)/100;           // Transaction Amount
          W_INCURTYP    = wDDCURTYP;                           //  CASA Currency Type

          If W_INCURTYP = 'IDR';
            W_INCURFLG = 'L';                                  //  Transaction Currency flag local
          Else;
            W_INCURFLG = 'F';                                  //  Transaction Currency flag foreign
          Endif
          
          W_INMDRMRK    = D_INMDRMRK;                          // Remarks
          W_INSBUCDE    = wLNSBUCOD;                           // SBU Code
          W_INSUBCDE    = wLNSUBCOD;                           // SUB Code

          wChkSum += W_INTRNAMT;                               // Sum all the Trans amount


          W_INPRDT        = INDATE;                              // Interface Processing Date
          //W_INFCDT      = %dec(%date());                       // File Processing Date
          // W_INTIME     = %dec(%TIME());                       // Interface Timestamp
          //W_INTIME      = INTIME;                              // Interface Timestamp
          //W_INTIMESTMP  = %TIMESTAMP();                        // Interface Timestamp

           Write RINI3102W;

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
