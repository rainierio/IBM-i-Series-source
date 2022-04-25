      /* ---------------------------------------------------------------- */
      /* Program to copy AS4000 source file members to IFS                */
      /* ---------------------------------------------------------------- */
      /* If you don't have file that lists all your source file members,  */
      /* you can list all objects to an outfile with command              */
      /* DSPOBJD OBJ(S104120X/MISC) OBJTYPE(*ALL) OUTPUT(*OUTFILE)       */
      /* OUTFILE(QTEMP/MYFILE)                                            */
      /* then use values ODSRCL, ODSRCF, and ODSRCM in program below      */
      /* ---------------------------------------------------------------  */

      PGM

      DCLF       FILE(XASRCMBR) RCDFMT(QWHFDMBR)
      DCL        VAR(&FROMMBR) TYPE(*CHAR) LEN(60)
      DCL        VAR(&TOFILE)  TYPE(*CHAR) LEN(60)
      DCL        VAR(&NEW_LIB1) TYPE(*CHAR) LEN(60)
      DCL        VAR(&NEW_LIB2) TYPE(*CHAR) LEN(60)

      /* Create starting directory in IFS */
        CRTDIR     DIR('/home/S104120X/')
        MONMSG     MSGID(CPF0000)
        CRTDIR     DIR('/TMP/')
        MONMSG     MSGID(CPF0000)

      /* Loop through records */
NEXTRCD:
        RCVF
        MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(EOF))


         /*  Define new 'library' and 'member' directory in IFS */

        CHGVAR    VAR(&NEW_LIB1) VALUE('''/tmp/src/'  +
                                *TCAT &MBLIB         +
                                *TCAT '''')

        CHGVAR    VAR(&NEW_LIB2) VALUE('''/tmp/src/'  +
                                *TCAT &MBLIB         +
                                *TCAT '/'            +
                                *TCAT &MBFILE        +
                                *TCAT '''')

         /*   Define where source file member is coming from            */
         /*   Note we use:                                              */
         /*   /QSYS.LIB/MYMBR.MBR/MYSRCFILE.FILE/MYSRCMBR.MBR Notation  */

        CHGVAR     VAR(&FROMMBR) VALUE('''/QSYS.LIB/' +
                                  *TCAT   &MBLIB      +
                                  *TCAT  '.LIB/'      +
                                  *TCAT   &MBFILE     +
                                  *TCAT   '.FILE/'    +
                                  *TCAT   &MBNAME     +
                                  *TCAT   '.MBR''')


         /* Define a destination file in IFS */

         CHGVAR  VAR(&TOFILE) VALUE('''/TMP/SRC/'     +
                                   *TCAT &MBLIB       +
                                   *TCAT '/'          +
                                   *TCAT &MBFILE      +
                                   *TCAT '/'          +
                                   *TCAT &MBNAME      +
                                   *TCAT '.txt''')


         /* Make sure 'file' and'member' directories exist on IFS */

         CRTDIR     DIR(&NEW_LIB1)
         MONMSG     MSGID(CPF0000)
         CRTDIR     DIR(&NEW_LIB2)
         MONMSG     MSGID(CPF0000)

         /* Copy members to IFS */
         CPYTOSTMF  +
         FROMMBR(&FROMMBR) +
         TOSTMF(&TOFILE) +
         STMFOPT(*REPLACE) STMFCODPAG(*PCASCII)

         MONMSG     MSGID(CPF0000)

        /* DLYJOB   DLY(5) */

         GOTO NEXTRCD
EOF:

   ENDPGM