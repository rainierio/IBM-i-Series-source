     H CVTOPT(*VARCHAR)
      ****************************************************************************
      * Program Name..: INI3102A                                                 *
      * Description...:                                                          *
      *                 (RPGLE Application Reconsolication Program)              *
      * Date Created..: 11 Apr 2022                                              *
      * Author........: Rainier Letidjawa                                        *
      ****************************************************************************
     ** Input/Output/Update file 
     FINI3102W  IF   E           K DISK
     FINTFILEL5 UF   E           K DISK
     FINTDTE    IF A E           K DISK
     FINPDWE    IF   E           K DISK    prefix(E_)
     D**************************************************************************
     D* Prototypes
     D INI3102Pr       PR                  Extpgm('INI3102A')                    
     D P9FIID                        20A
     D P9REF                         25A
     D P9RCNERR                       1A
     D*
     D INI3102Pr       PI
     D P9FIID                        20A
     D P9REF                         25A
     D P9RCNERR                       1A
     D* Additional Field
     D Counter         S              9S 0
      *
      /Free
       //*******************************************************************************************
       //  Main Proccesing
       //*******************************************************************************************

        Setll (P9FIID:P9REF) INTFILEL5;
        If %equal(INTFILEL5);
        Reade (P9FIID:P9REF) INTFILEL5;
          Dow not %eof(INTFILEL5);
            Exsr srProcess;
            Exsr srUpdatErr;                 // return error indicator
            Reade (P9FIID:P9REF) INTFILEL5;
          Enddo;
        Endif;

        *Inlr = *on;

       //*******************************************************************************************
       //  srProcess;
       //  Update error from core apps
       //*******************************************************************************************

          Begsr SrProcess;
            Setll (P9FIID:P9REF) INI3102W;
            If %equal (INI3102W);
              Reade (P9FIID:P9REF) INI3102W;
              Dow not %eof (INI3102W);
                If INSTS = 'S' and PRCSTS <> 'AA' and PRCSTS <> *BLANKS;
                  INFIID = INFIID;          // Interface Identifier
                  INFNAM = INFNAM;          // Interface Filename
                  INFSEQ = INFSEQ;          // Interface File Sequence
                  INDATE = INDATE;          // Creation Dt YYYYMMDD
                  INTIME = INTIME;          // Creation Time HH:MM:SS
                  INSEQN = 1;               // Block Sequence No.
                  INRSEQN = INSEQ;          // Record Sequence No.
                  INESEQN = Counter + 1;    // Error Sequence No.
                  INIFERR = 'N';            // Interface Error Y/N
                  INERVAL = %trim(ERRDSC);  // Error Data
                  INERCD = ERRCDE;          // Error Codes
                  Write RINTDTE;
                Endif;
                Reade (P9FIID:P9REF) INI3102W;
              Enddo;
            Endif;
          Endsr;
       //*******************************************************************************************
       //  srUpdatErr;
       //  Return error indicator if one of the record has error
       //*******************************************************************************************

          Begsr srUpdatErr;
            Setll (P9FIID:P9REF) INI3102W;
            If %equal (INI3102W);
              Reade (P9FIID:P9REF) INI3102W;
              Dow not %eof (INI3102W);
                If INSTS = 'R' OR PRCSTS = 'AB';
                  P9RCNERR = 'Y';
                  Leave;
                Endif;
                Reade (P9FIID:P9REF) INI3102W;
              Enddo;
            Endif;
          Endsr;

      /End-Free
