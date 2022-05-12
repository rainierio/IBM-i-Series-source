     H COPYRIGHT('█ Copyright Silverlake System Sdn. Bhd. 1993, 2008          ')
     H CVTOPT(*VARCHAR)                                                         
     H DFTACTGRP(*NO)                                                           
     H ACTGRP(*CALLER)                                                                                                                 
     H OPTION(*SRCSTMT:*NODEBUGIO)                                              
      **************************************************************************
      * Program Name : INI4102                                                 *
      * Description  : Loan Close and Rebook Upload Summary Report             *
      * Created By   : Rainier Letidjawa                                       *
      * Date Created : 11/04/2022                                              *
      **************************************************************************
      *                                                                        *
      * INI4102 is part of Silverlake Integrated Banking System                *
      * Copyright 2020 by Silverlake System Sdn Bhd Kuala Lumpur,Malaysia.     *
      *                                                                        *
      **************************************************************************
      *  Modification Logs:                                                    *
      *                                                                        *
      *  ----  ----- --------   ---------------------------------------------- *
      *  Name  Init  Date       Remarks (Description)                          *
      *  ----  ---- --------    ------------------------                       *
      *                                                                        *
      *                                                                        *
      **************************************************************************
      **  INPUT   **
     FINTFILEL5 IF   E           K DISK    PREFIX(I_)
     FINI3102W  IF   E           K DISK    PREFIX(W_)
     FINTDTEL3  IF   E           K DISK    PREFIX(E_)
     FINPDWE    IF   E           K DISK    PREFIX(P_)
     FRDREPT    IF   E           K DISK    PREFIX(RD_)
     FMNUUSR    IF   E           K DISK    PREFIX(MN_)
     FSSPAR1    IF   E           K DISK
      **  OUTPUT  **
     FINI4102P1 O    E             PRINTER OFLIND(IN98) PREFIX(S_)
     FINI4102P2 O    E             PRINTER OFLIND(IN99) PREFIX(R_)
     ** Prototype **
     D INI4102ePr      Pr                  EXTPGM('INI4102')
     D P9APPR                         1A
     D P9IMID                        20A
     D P9BREF                        25A
     D INI4102ePr      Pi
     D P9APPR                         1A
     D P9IMID                        20A
     D P9BREF                        25A
      * Working Variables
     D IN98            S               N
     D IN99            S               N
      * FOR SUCCESSFUL--------
     D WSPAGNO         S              4S 0 INZ(0)
     D WSCOUNT         S              9S 0 INZ(0)
     D W#TTLAMT        S             17S 2 INZ(0)
     D W#TTLINT        S             17S 2 INZ(0)
     D W#TTLLATE       S             17S 2 INZ(0)
      * FOR FAILED --------
     D WRPAGNO         S              4S 0 INZ(0)
     D WFCOUNT         S              9S 0 INZ(0)
     D W#TFRCD         S              9S 0 INZ(0)
     D W#RCD           S              9S 0 INZ(0)
     D C#APPR          S              1A   INZ(*BLANKS)
     **
     D RDSIZE          S              5S 0 INZ(0)
     D* Copy Book
     D/COPY QRDSSRCRLE,RDFORMATD
      /Free
       //***********************************************************************
       //     Main Program
       //***********************************************************************

        Read INI3102W;

       //Compose Uploader Report
        If (P9APPR = 'U');
          WSPAGNO += 1;
          Exsr SrRptFmt;
          Write HEADER1;
          Exsr SrSuccSHdr;
          Write DTLHDR1;

          Dow Not %Eof (INI3102W);
            Exsr SrReport;
          Read INI3102W;
          Enddo;

          If WSCOUNT = 0;
            Exsr SrNoRec;
          Endif;
          Exsr SrTrailer;
        Endif;

       //Compose approver Report
        If (P9APPR = 'A');
          WRPAGNO += 1;
          Exsr SrFRptFmt;
          Write HEADER2;
          Exsr SrFailSHdr;
          Write DTLHDR2;

          Dow Not %Eof (INI3102W);
            Exsr SrFReport;
          Read INI3102W;
          Enddo;          

          If WFCOUNT = 0;
            Exsr SrFNoRec;
          Endif;

          Exsr SrFTrailer;
        Endif;

        Exsr SrEndRpt;

        *Inlr = *On;
       //****************************************************************
       // Subroutine  : SrRptFmt
       // Description : Successful Report format control
       //****************************************************************

         Begsr SrRptFmt;

           RDPAGNO = WSPAGNO;                                   // From QRDSSRCR
           RDRPTNAM  = 'INI4102P1';                             // Report Name
           Callp rdreport( rdline198        :
                           %size(rdline198) :
                           'RDJOB198'       :
                           rdinputds        :
                           RDOUTPUTDS);

           S_RDLINE1 = line198_1;
           S_RDLINE2 = line198_2;
           S_RDLINE3 = line198_3;
           S_RDLINE4 = line198_4;
           Chain ('IF':'INI4102C':RDRPTNAM) RRDREPT;
           If %Found (RDREPT);
              S_RDLINE5 =  %trim(RD_REPTTL);
           Else;
              S_RDLINE5 = line198_5;
           Endif;
           S_RDLINE6 = line198_6;
           S_RDLINE7 = line198_7;
         Endsr;
       //****************************************************************
       // Subroutine  : SrFRptFmt
       // Description : Faield Report format control
       //****************************************************************
         Begsr SrFRptFmt;

           RDPAGNO = WRPAGNO;                                   // From QRDSSRCR
           RDRPTNAM  = 'INI4102P2';                             // Report Name
           Callp rdreport( rdline198        :
                           %size(rdline198) :
                           'RDJOB198'       :
                           rdinputds        :
                           RDOUTPUTDS);
           Chain ('IF':'INI4102C':RDRPTNAM) RRDREPT;
           R_RDLINE1 = line198_1;
           R_RDLINE2 = line198_2;
           R_RDLINE3 = line198_3;
           R_RDLINE4 = line198_4;
           R_RDLINE5 = %trim(RD_REPTTL);
           R_RDLINE6 = line198_6;
         Endsr;
       //****************************************************************
       // Subroutine  : SrSuccSHdr
       // Description : Subroutine for SubHeader - Successful report
       //****************************************************************
         Begsr SrSuccSHdr;

         If P9APPR <> 'A';
           S_V9LINE = 'UPLOADER - VALIDATION';
         Elseif P9APPR = 'A';
           S_V9LINE = 'APPROVER - TRANSACTION POSTING';
         Endif;

         Chain (P9IMID:P9BREF) RINTFILE;
         If %Found (INTFILEL5);
            S_V9REFN = %Trim(I_INFNAM) + '/' + %Trim(I_INREFN);
            If P9APPR = 'A';
              S_V9UID = %Trim(I_INUPUSR) + ' / ' + %Trim(I_INLUID);
            Else;
              S_V9UID = %Trim(I_INUPUSR);
            Endif;
         Endif;
         Write SUBHDR13;

         If P9APPR = 'A';
            Write SUBHDR15;
            Write SUBHDR16;
         Else;
            Write SUBHDR14;
         Endif;

         Endsr;
       //****************************************************************
       // Subroutine  : SrFailSHdr
       // Description : Subroutine for SubHeader - Failed report
       //****************************************************************
         Begsr SrFailSHdr;

         If P9APPR <> 'A';
            R_V9LINE = 'UPLOADER - VALIDATION';
         Elseif P9APPR = 'A';
            R_V9LINE = 'APPROVER - TRANSACTION POSTING';
         Endif;

         Chain (P9IMID:P9BREF) RINTFILE;
         If %Found (INTFILEL5);
            R_V9REFN = %Trim(I_INFNAM) + '/' + %Trim(I_INREFN);
            If P9APPR = 'A';
              R_V9UID = %Trim(I_INUPUSR) + ' / ' + %Trim(I_INLUID);
            Else;
              R_V9UID = %Trim(I_INUPUSR);
            Endif;
         Endif;
         Write SUBHDR23;

         If P9APPR = 'A';
            Write SUBHDR25;
         Else;
            Write SUBHDR24;
         Endif;

         Endsr;
       //****************************************************************
       // Subroutine  : SrReport
       // Description : Populate Successful Report
       //****************************************************************
         Begsr SrReport;

              // Write on a new line if not enough place for footer
              Exsr SrNwLine98;
              WSCOUNT += 1;                                     // Successful Count
              Exsr srDetail;
              Write DETAIL1;

         Endsr;
       //****************************************************************
       // Subroutine  : srFReport
       // Description : approver succes and failed record
       //****************************************************************
         Begsr SrFReport;

              // Write on a new line if not enough place for footer
              Exsr SrNwLine99;
              If P9APPR = 'A';
                 Exsr SrADetail;
                 WFCOUNT += 1;
              Endif;

         Endsr;
       //****************************************************************
       // Subroutine  : SrNwLine98
       // Description : Write on a new line if not enough place for footer
       //****************************************************************
         Begsr SrNwLine98;
              If In98;
                   Exsr SrOvrflow;
                   In98 = *Off;
              Endif;
         Endsr;
       //****************************************************************
       // Subroutine  : SrOvrflow
       // Description : Overflow Control for successful report
       //****************************************************************
         Begsr SrOvrflow;
              WSPAGNO += 1;
           // Get Header Details
              Exsr SrRptFmt;
              Write HEADER1;
              Exsr SrSuccSHDR;
              Write DTLHDR1;
         Endsr;

       //****************************************************************
       // Subroutine  : SrNwLine99
       // Description : Write on a new line if not enough place for footer
       //****************************************************************
         Begsr SrNwLine99;
              If In99;
                   Exsr SrROvrflow;
                   In99 = *Off;
              Endif;
         Endsr;
       //****************************************************************
       // Subroutine  : SrROvrflow
       // Description : Overflow Control for Rejected Report
       //****************************************************************
         Begsr SrROvrflow;
              WRPAGNO += 1;
           // Get Header Details
              Exsr SrFRptFmt;
              Write HEADER2;
              Exsr SrFailSHdr;
              Write DTLHDR2;
         Endsr;
       //****************************************************************
       // Subroutine  : SrDetail
       // Description : Subroutine for Success Report Detail
       //****************************************************************
         Begsr SrDetail;

         S_V9RUNDGT = %Trim(%Char(WSCOUNT));
         S_V9SEQ         =  W_INSEQ;
         S_V9INTRNTYP    =  W_INTRNTYP;
         S_V9INACCTNO    =  W_INACCTNO;
         S_V9INDDACCT    =  W_INDDACCT;
         S_V9INTRNAMT    =  W_INTRNAMT;
         S_V9INCURTYP    =  W_INCURTYP;

         If W_INSTS = 'R' and W_ERRCDE = *Blanks;
            S_V9INSTS       =  W_INSTS;
            S_V9INERDCD     =  %TRIM(W_INERRDT);
         Elseif W_INSTS = 'S' and W_ERRCDE <> *Blanks and W_PRCSTS = 'AB';
            S_V9INSTS       =  'R';
            S_V9INERDCD     =  %TRIM(W_ERRDSC);
          Else;
            S_V9INSTS       =  W_INSTS;
            S_V9INERDCD     =  %TRIM(W_INERRDT);
         Endif;

        Endsr;
       //****************************************************************
       // Subroutine  : SrADetail
       // Description : Subroutine for Failed Report Detail - Approver
       //****************************************************************
         Begsr SrADetail;

         R_V9RUNDGT = %Trim(%Char(WSCOUNT));
         R_V9SEQ         =  W_INSEQ;
         R_V9INTRNTYP    =  W_INTRNTYP;
         R_V9INACCTNO    =  W_INACCTNO;
         R_V9INDDACCT    =  W_INDDACCT;
         R_V9INTRNAMT    =  W_INTRNAMT;
         R_V9INCURTYP    =  W_INCURTYP;
         R_V9INSBUCDE    =  W_INSBUCDE;
         R_V9INSUBCDE    =  W_INSUBCDE;

         If W_INSTS = 'R' and W_ERRCDE = *Blanks;
            R_V9INSTS       =  W_INSTS;
            R_V9INERDCD     =  %TRIM(W_INERRDT);
         Elseif W_INSTS = 'S' and W_ERRCDE <> *Blanks and W_PRCSTS = 'AB';
            R_V9INSTS       =  'R';
            R_V9INERDCD     =  %TRIM(W_ERRDSC);
          Else;
            R_V9INSTS       =  W_INSTS;
            R_V9INERDCD     =  %TRIM(W_INERRDT);
         Endif;

         Write DETAIL2;


         Endsr;
       //****************************************************************
       // Subroutine  : SrMDetail
       // Description : Subroutine for Failed Report Detail - Maker
       //****************************************************************
         Begsr SrMDetail;

         R_V9SEQ = E_INRSEQN;
         If W#RCD <> E_INRSEQN;
             W#RCD = E_INRSEQN;
             W#TFRCD += 1;
         Endif;

          CHAIN (P9IMID:P9BREF:E_INRSEQN) RINI3102W;
          If %Found (INI3102W);
             R_V9INACCTNO = W_INACCTNO;
          Else;
             R_V9INACCTNO = *ZERO;
          Endif;

          Chain (I_INFIID:E_INERCD) RINPDWE;
          If %Found (INPDWE);
            If E_INERVAL <> *blanks;
              R_V9INERDCD = %Trim(E_INERCD) + ':' + %Trim(P_INERRD)
                     + ' (' + %Trim(E_INERVAL) +')';
            Else;
              R_V9INERDCD = %Trim(E_INERCD) + ':' + %Trim(P_INERRD);
            Endif;
          Else;
            R_V9INERDCD = %Trim(E_INERCD) + ' (' + %Trim(E_INERVAL) +')';
          Endif;
         WFCOUNT  +=1;
         R_V9RUNDGT = %Char(WFCOUNT);
         Write DETAIL2;

         Endsr;
       //****************************************************************
       // Subroutine  : SrNoRec
       // Description : No Record Control
       //****************************************************************
         Begsr SrNoRec;
              WSPAGNO = 1;
              Write NOREC1;
         Endsr;
       //****************************************************************
       // Subroutine  : SrTrailer
       // Description : Compose Total for Uploader Report
       //****************************************************************
         Begsr SrTrailer;

         S_V9TTLRCD  = WSCOUNT;

         Exsr SrNwLine98;
         Write TRAILER11;
         Exsr SrNwLine98;
         Write TRAILER12;
         Exsr SrNwLine98;
         Write TRAILER13;
         %SUBST(S_R#ENDSL:89:21) = 'End of Report';
         Exsr SrNwLine98;
         Write ENDSRPT;

         Endsr;
       //****************************************************************
       // Subroutine  : SrFNoRec
       // Description : No Record Control
       //****************************************************************
         Begsr SrFNoRec;
              WRPAGNO = 1;
              Write NOREC2;
         Endsr;
       //****************************************************************
       // Subroutine  : SrFTrailer
       // Description : Compose Total for Approver Report
       //****************************************************************
         Begsr SrFTrailer;

         R_V9TFRCD  = WFCOUNT;
         Exsr SrNwLine99;
         Write TRAILER21;
         Exsr SrNwLine99;
         Exsr SrNwLine99;
         Write TRAILER22;
         Exsr SrNwLine99;
         Write TRAILER23;
         %SUBST(R_R#ENDSL:89:21) = 'End of Report';
         Write ENDFRPT;

         Endsr;
       //****************************************************************
       // Subroutine  : SrEndRpt
       //****************************************************************
         Begsr SrEndRpt;

            Write ENDRPT1;

         Endsr;
       //***********************************************************************
       //     Subroutine       : *INZSR
       //***********************************************************************
          BEGSR *INZSR;

          If P9APPR = 'A';
             C#APPR = P9APPR;
          Else;
             C#APPR = 'U';
          Endif;

           Chain (P9IMID:P9BREF) RINTFILE;
           If %Found (INTFILEl5);
             // Route to the correct branch
             If P9APPR <> 'A';
               // Uploader report goes to Maker ID
               Chain (I_INUPUSR) RMNUUSR;
             Else;
               // Approver report goes to Approver ID
               Chain (I_INLUID) RMNUUSR;
             Endif;

             If %Found (MNUUSR);
               RDBNK = MN_BNKNBR;
               RDBRN = MN_BRNNBR;
             Endif;
           Endif;

           Read RSSPAR1;
               RDPROCD8  = SP1NB8;
               RDPTHRU8  = SP1NB8;

          ENDSR;
      /End-free
