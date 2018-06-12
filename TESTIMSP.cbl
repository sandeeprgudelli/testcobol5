       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TESTIMSP.
       AUTHOR.       SANDEEP.
       INSTALLATION. EMPIRE BLUE CROSS BLUE SHIELD.
       DATE-COMPILED.
      *
      *****************************************************************
      ****                                                         ****
      ****                  MODULE OVERVIEW OF                     ****
      ****                       TESTIMSP                          ****
      ****  "SENIOR PLAN DIRECT PAY DELINQUENT 30 - 59 DAY REPORT" ****
      *****************************************************************
      ****
      ****   PURPOSE:   CREATE A BI-WEEKLY REPORT FILE FOR SENIOR PLAN
      ****              DP DELNQ  30 - 59 DAY REPORT"
      ****
      ****   INPUTS :   GROUP DATABASE , SUBSCRIBER DATABASE ,BILLING
      ****                                                    DATABASE
      ****   OUTPUTS:   REPORT FILE FOR REPORT FILE FOR MEDIBLUE
      ****              DELINQUENT MEMBERS
      ******************************************************************
      ******************************************************************
      * MODIFICATION LOG                                               *
      * ================                                               *
      * DATE      NAME         DESCRIPTION                    CONTROL# *
      * --------- ------------ --------------------------------------- *
      * 05/23/13  MUKESH K     INITIAL RELEASE               CQ448437  *
      *                                                                *
      ******************************************************************
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT REPORT-FILE              ASSIGN TO RPTCON.

       DATA DIVISION.
       FILE SECTION.

       FD  REPORT-FILE
           LABEL RECORDS STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  REPORT-FILE-REC              PIC X(80).


       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(40)
           VALUE 'WORKING STORAGE BEGINS HERE FOR TESTIMSP'.

       01  WS-DATE-FIELDS.
           05  WS-CURR-DATE               PIC 9(06).
           05  WS-CURR-PDATE              PIC 9(08).
           05  WS-CURR-QDATE              PIC 9(08).
           05  WS-GROUP-CUT-OFF           PIC 9(07).
           05  WS-DATE-CONVERT            PIC 9(08).
           05  WS-CC-YY                   PIC 9(04).
           05  WS-DATEX                   PIC 9(7).
           05  WS-DATEXX                  REDEFINES WS-DATEX   PIC X(7).
           05  WS-DATE-MMDDYY.
               15 WS-DA-MM   PIC X(2) VALUE SPACES.
               15 WS-DA-DD   PIC X(2) VALUE SPACES.
               15 WS-DA-YY   PIC X(2) VALUE SPACES.

       01  WS-TEMP-SUB-CNT               PIC 9(9) VALUE ZEROES.

       01  WS-NT10-REN-DT                PIC 9(07) VALUE ZEROS.
       01  WS-NT10-REN-DT1 REDEFINES WS-NT10-REN-DT.
           05 WS-NT10-RENDT-C            PIC 9(01).
           05 WS-NT10-RENDT-YY           PIC 9(02).
           05 WS-NT10-RENDT-MM           PIC 9(02).
           05 WS-NT10-RENDT-DD           PIC 9(02).
       01  MAIL-CURR-PREV-MISC.
           05 MAIL-GRP-NUM-CURR             PIC X(06) VALUE SPACES.
           05 MAIL-SUB-GRP-CURR             PIC X(03) VALUE SPACES.
           05 MAIL-GRP-NUM-PREV             PIC X(06) VALUE SPACES.
           05 MAIL-SUB-GRP-PREV             PIC X(03) VALUE SPACES.
       01  OUT-BILL-STOP                    PIC X(18) VALUE SPACES.
       01  ED-STATUS                        PIC 9(04) COMP VALUE ZERO.
       01  ED-OUTPUT.
           05  ED-1X-CODE                   PIC X(01).
           05  ED-99X-DESC                  PIC X(99).
       01  ED-TABLE                         PIC X(8).
       01  DECODE-NUMBER                    PIC 9(4)  COMP VALUE ZERO.
       01  WS-MATCH-FOUND                 PIC X(01) VALUE 'N'.
       01  WS-PKG-MATCH-FOUND             PIC X(01) VALUE 'N'.
       01  WS-PKG-END-REACHED             PIC X(01) VALUE 'N'.
       01  WS-MATCH                       PIC  X(01).
       01  WS-SNT21-FLAG                  PIC X(01) VALUE 'N'.
       01  WS-IDPLAN-DESC                 PIC X(200).

       01 WS-GRPPKG-DATA.
          05 WS-GRPPKG-TBL OCCURS 100 TIMES INDEXED BY PKG-INDEX.
             10 WS-TBL-GROUP              PIC X(9).
             10 WS-PKG-NO                 PIC 9(3).
             10 WS-PROD-COMB-CD           PIC 9(4) COMP.

       01  CTY-TABLE.
           05  CTY-OUTPUT-DESC    PIC X(36).
           05  CTY-LAST-ENTRY     PIC 9(4) COMP  VALUE ZEROES.
           05  CTY-ENTRY          OCCURS 100 TIMES
                                           INDEXED BY WS-INDEX.
              10 CTY-INPUT        PIC X(4).
              10 CTY-OUTPUT       PIC X(18).

       01  CODE-TABLE-PARAMETERS.
           05  ENCDEC-TABLE-NAMES.
               10  STUCYSVC             PIC X(08)  VALUE 'STUCYSVC'.
           05  ENCDEC-STATUS            PIC  9(4)  COMP VALUE 0.
           05  B-THIRTEEN               PIC 9(04) COMP SYNC VALUE 13.
       01 WS-PHONE-CHK                      PIC X(1) VALUE 'N'.
       01 WS-PROD-COMB                      PIC X(06).
       01 WS-PKG-NO-NUM                     PIC 9(03) COMP-3.
       01  SUBR-STOP-TBL                PIC X(8)  VALUE 'FTZSUSTP'.
       01  IDPLANNM                     PIC X(08) VALUE 'IDPLANNM'.
       01  STOP-TABLE-CALL              PIC X(8)  VALUE '        '.
       01  BILL-STOP-TBL                PIC X(8)  VALUE 'FTZBLSTP'.
       01  BILL-STAT-TRANSLATION.
           05  BILL-STAT-NUMBER         PIC 9(4)  COMP.
           05  FILLER       REDEFINES BILL-STAT-NUMBER.
               10  FILLER              PIC X.
               10  BILL-STAT-CD        PIC X.

       01  BILL-STOP-TRANSLATION.
           05  BILL-STOP-NUMBER         PIC 9(4)  COMP.
           05  FILLER       REDEFINES BILL-STOP-NUMBER.
               10  FILLER              PIC X.
               10  BILL-STOP-CD        PIC X.
       01  BILL-STOP-ARGUE.
           05  BILL-STOP-OUTPUT        PIC XX.
           05  BILL-STOP-DESC          PIC X(23).
       01 WS-CHECK-PROD                     PIC X(02).
       01  WS-TEMP-SUB                      PIC X(15).
       01  WS-VALID-BIR                       PIC  X(01).
       01  WS-GROUP                     PIC X(6) VALUE SPACES.          01590007
           88  MEDIBLUE      VALUE '920097'  '920099'                   CR105921
                                   '910099'  '930099'                   01610000
                                   '930097'  '940097'                   01610000
                                   '940099'  '910097'.                  01610000

       01  WS-TEMP-REN-DT1               PIC 9(07) VALUE ZEROS.
       01  WS-TEMP-REN-DT REDEFINES  WS-TEMP-REN-DT1.
           05 WS-TEMP-RENDT-C            PIC 9(01).
           05 WS-TEMP-RENDT-YY           PIC 9(02).
           05 WS-TEMP-RENDT-MM           PIC 9(02).
           05 WS-TEMP-RENDT-DD           PIC 9(02).
       01  WS-SUB-GRP-MATCH              PIC X(1)  VALUE 'N'.
       01  WS-NT10-EFF-DT                PIC 9(07) VALUE ZEROS.
       01  WS-NT10-EFF-DT1 REDEFINES WS-NT10-EFF-DT.
           05 WS-NT10-EFFDT-C            PIC 9(01).
           05 WS-NT10-EFFDT-YY           PIC 9(02).
           05 WS-NT10-EFFDT-MM           PIC 9(02).
           05 WS-NT10-EFFDT-DD           PIC 9(02).
       01  WS-LAST-TRM-DT                PIC 9(06) VALUE ZEROES.
       01  WS-TEMP-TRM-DT                PIC 9(07) VALUE ZEROES.
       01  WS-TMP-CONTRACT-NO           PIC 9(5).
       01  WS-END-OF-VSGPPKA             PIC X(1) VALUE 'N'.
       01  WS-NT10-DATE-FLAG             PIC X(1) VALUE 'N'.
       01  WS-GA-EFF-DT                  PIC 9(7) VALUE ZEROS.
       01  WS-WRITE-FLAG                 PIC X(1) VALUE 'N'.
       01  WS-VSGPSUP-FLAG                   PIC X(01)  VALUE SPACES.
       01  WS-PRCS-CONT                  PIC X(1) VALUE SPACES.
       01  WS-PKG-SUB-CNT                PIC 9(07)  VALUE 0.
       01  WS-TOTAL-SUBS                 PIC 9(07)  VALUE 0.
       01  NTUCLSRK                      PIC X(8)  VALUE 'NTUCLSRK'.
       01  WS-1ST-TIME                   PIC X(01) VALUE 'Y'.
       01  WS-2ND-TIME                   PIC X(01) VALUE 'Y'.
       01  WS-MNT08-FOUND                PIC X(01) VALUE SPACES.
       01  WS-END-MEDNOTE                PIC X(01) VALUE SPACES.
       01  IMS-WORK-FIELDS.
           05  IMS-STATUS-CODE            PIC X(02).
           05  IMS-CALL-FUNC1             PIC X(04).
           05  IMS-CALL-FUNC2             PIC X(04).
           05  GU                         PIC X(04) VALUE  'GU  '.
           05  GN                         PIC X(04) VALUE  'GN  '.
           05  GNP                        PIC X(04) VALUE  'GNP '.
       77  B-ONE                  COMP    PIC 9(4)  VALUE 1.
       77  B-ZERO                 COMP    PIC 9(4)  VALUE 0.

       01  WS-CNOTES                    PIC 9(2).
           88  WS-CNOTE-08                       VALUE 08.
       01  WS-CNOTE-CODE                PIC 9(4) COMP VALUE ZERO.
       01  FILLER REDEFINES WS-CNOTE-CODE.
           05  FILLER                   PIC X(01).
           05  WS-CNOTE-CD-BINARY       PIC X(01).

       01  WS-WORK-FIELDS.
           05  WS-PROD-VAR-CD             PIC X(3).
           05  P901-LINKED-GRP            PIC X(9).
           05  WS-FOUND                   PIC X.
           05  WS-COM-WRITE               PIC X  VALUE SPACES.
           05  WS-I                       PIC 99.
           05  WS-TEMP-PKG-NO             PIC 9(3) VALUE ZEROS.         01240074
           05  WS-AGE-USE                 PIC 99.
           05  B-FIVE                     PIC S9(4) COMP VALUE +5.
           05  B-5                        PIC S9(4) COMP VALUE +5.
           05  B-05                       PIC  9(4) COMP VALUE  5.
           05  B-14                       PIC 9(4) COMP VALUE 14.
           05  BFOURTEEN                  PIC 9(4) COMP SYNC VALUE 14.
           05  WS-MBR-COUNT               PIC 9(08) VALUE ZERO.
           05  ZIP-DISPLAY                PIC 9(05).
           05  IDP-INDEX                  PIC 9(4)  COMP.
           05  WS-DATE.
               07  WS-CYY                 PIC 9(03).
               07  WS-CYY-REDEF REDEFINES
                   WS-CYY.
                   09  WS-CENTURY         PIC 9(01).
                   09  WS-YEAR            PIC 9(02).
               07  WS-MONTH               PIC 9(02).
               07  WS-DAY                 PIC 9(02).
           05  WS-DATE-REDEF REDEFINES WS-DATE PIC 9(07).
           05  WS-HOLD-LST-NM             PIC X(15).
           05  WS-SB-DOB                  PIC 9(07) COMP-3.
           05  WS-TEMP-TOT-AMNT-DUE       PIC ZZZZZZZZZZ9.99-.
           05  WS-TEMP-UNALLOC-AMNT       PIC ZZZZZZZZ9.99-.
           05  WS-REC-AMT                 PIC 9(11)V99 COMP-3.
           05  WS-REC-AMT-TOTAL           PIC 9(11)V99 COMP-3.
           05  WS-REC-AMT-NUMERIC         PIC ZZZZZZZZZZ9.99-.
           05  WS-REC-AMT-CHK             PIC 9(11)V99.
           05  WS-ALOC-DT                 PIC 9(7).
           05  WS-NT10-TERM-DT            PIC 9(7).
           05  WS-CONV-RENWL-DT           PIC X(7).
           05  WS-NO-MORE-CASH            PIC X(01) VALUE SPACES.
           05  WS-NO-MORE-BILL            PIC X(01) VALUE SPACES.
           05  WS-DUMMY-PROCESSED         PIC X(01) VALUE SPACES.
           05  WS-NO-MORE-BIL-PTR         PIC X(01) VALUE SPACES.
           05  RETURN-STATUS              PIC  9(4)  COMP.
           05  RETURN-STATUS-CD           PIC 9(4)  COMP.
           05  WS-RETURN-CODE             PIC 9(08) VALUE ZERO.
           05  WS-IND-CNT                 PIC 9(7)  VALUE  ZEROES.
           05  WS-GRP-COUNT               PIC 9(08) VALUE ZERO.
           05  WS-FAL-COUNT               PIC 9(08) VALUE ZERO.
           05  WS-NO-FAL-COUNT            PIC 9(08) VALUE ZERO.
           05  WS-IND-CNT1                PIC 9(3)  VALUE  ZEROES.
           05  WS-IND-CNT2                PIC 9(3)  VALUE  ZEROES.
           05  WS-DIFFER-PCC-CNT          PIC 9(3)  VALUE  ZEROES.
           05  WS-GA-GROUP                PIC X(06).
           05  WS-GA-SUB-GRP              PIC X(03).
           05  WS-GPNOT1-PAGE             PIC X(03).
           05  WS-GNT92-FND               PIC X(01) VALUE SPACES.
           05  WS-SUB-ID                  PIC X(09) VALUE SPACES.
           05 WS-SUB-ALT-ID               PIC X(09) VALUE SPACES.
           05 GRP-SUB-MEM-STATUS          PIC X(1) VALUE 'Y'.
              88 GRP-SUB-MEM-FOUND                     VALUE 'Y'.
              88 GRP-SUB-MEM-NOT-FOUND                 VALUE 'N'.
           05 WS-PREM-TMP-DT              PIC 9(07).
           05  H-GW-SUBSCRIBER-DOB        PIC 9(08) VALUE ZEROS.
           05  H-CURRENT-DATE.
                10  H-CURRENT-DT-YY       PIC 9(2)      VALUE ZEROS.
                10  H-CURRENT-DT-MM       PIC 9(2)      VALUE ZEROS.
                10  H-CURRENT-DT-DD       PIC 9(2)      VALUE ZEROS.


           05  WS-SB-BRTH-DT-NUM          PIC 9(8) VALUE ZEROS.
           05  WS-SB-BRTH-DT-ANUM    REDEFINES
                                       WS-SB-BRTH-DT-NUM.
                15  WS-SB-BRTH-YEAR.
                    20  WS-SB-BRTH-CENT    PIC X(02).
                    20  WS-SB-BRTH-YR      PIC X(02).
                15  WS-SB-BRTH-MO          PIC X(02).
                15  WS-SB-BRTH-DA          PIC X(02).
           05  WS-SB-BRTH-DT-CYYMMDD.
                15  WS-SB-DOB-YEAR.
                    20  WS-SB-DOB-CENT     PIC X(01).
                    20  WS-SB-DOB-YR       PIC X(02).
                15  WS-SB-DOB-MO           PIC X(02).
                15  WS-SB-DOB-DA           PIC X(02).
           05  H-AGE-STARTDATE-CYYMMDD     PIC 9(07).
           05  WS-MEMB-CYY                 PIC 9(03).
           05  WS-CURR-CYY                 PIC 9(03).
           05  WS-AGE-CURR                 PIC 9(03).
           05  BILL-FREQ-TBL               PIC X(8)  VALUE 'GTZBLFRQ'.
           05  CONTRACT-TYPE-TBL           PIC X(8)  VALUE 'GTZCONTY'.
           05  CONTRACT-TYPE-TRANSLATION.
               10  CONTRACT-TYPE-NUMBER     PIC 9(4)  COMP.
               10  CONTRACT-TYPE-TRANSLATED REDEFINES
                                            CONTRACT-TYPE-NUMBER.
                   15  CONTRACT-TYPE-CD     PIC XX.
           05  CONTRACT-TYPE-ARGUE.
               10  CONTRACT-TYPE-INPUT      PIC X(03) VALUE SPACES.
               10  CONTRACT-TYPE-DESC       PIC X(31) VALUE SPACES.

       01  PACK-DUE-DATE                    PIC 9(8).
       01  PACK-DUE-DT     REDEFINES PACK-DUE-DATE.
           05  FILLER                       PIC 99.
           05  DUE-YY                       PIC 99.
           05  DUE-MM                       PIC 99.
           05  DUE-DD                       PIC 99.

       01  CONVERTED-DATE.
         05  CONVERTED-MM               PIC XX.
         05  FILLER                     PIC X      VALUE '/'.
         05  CONVERTED-DD               PIC XX.
         05  FILLER                     PIC X      VALUE '/'.
         05  CONVERTED-YY               PIC XX.

       01  BIR-ROOT-KEY.
           05  FILLER                   PIC X(9).
           05  BIR-ROOT-DATE            PIC 9(7) COMP-3.
           05  BIR-ROOT-SUB             PIC X(15).
       01  SUB                          PIC S9(4) VALUE  0 COMP.
       01  SUB1                         PIC 9(04) VALUE ZERO.
       01  SUB2                         PIC 9(07) VALUE ZERO.
       01  SAVE-BIR-PTR.
           05 FILLER                    PIC X(09) VALUE SPACES.
           05 SAVE-DUE-DATE             PIC 9(7) COMP-3 VALUE 0.
           05 FILLER                    PIC X(15) VALUE SPACES.
       01  WS-SB-BRTH-DT                PIC X(07).
       01  WS-COMPUTE-DOB.
              10  WS-MM                 PIC X(02) VALUE SPACES.
              10  FILLER                PIC X(01) VALUE '/'.
              10  WS-DD                 PIC X(02) VALUE SPACES.
              10  FILLER                PIC X(01) VALUE '/'.
              10  WS-YY-CC              PIC X(02) VALUE SPACES.
              10  WS-YY1                PIC X(02) VALUE SPACES.

       01  WS-REGION-CODE               PIC X(02).
       01  WS-COUNTY-CODE               PIC X(02).
       01  WS-SH-IND                    PIC X(01) VALUE SPACES.

       01  WS-RPT-FILE-REC.
           05 WS-MIG-GRP-KEY              PIC X(09) VALUE SPACES.
           05 FILL1                       PIC X(1)  VALUE SPACES.
           05 WS-MIG-NO-ENR-PKG           PIC X(03) VALUE SPACES.
           05 FILL2                       PIC X(1)  VALUE SPACES.
           05 WS-MIG-INACT-SUBS           PIC X(09) VALUE SPACES.
           05 FILL3                       PIC X(1)  VALUE SPACES.
           05 WS-MIG-LTST-TRM-DT          PIC X(06) VALUE SPACES.
           05 FILL4                       PIC X(01) VALUE SPACES.
           05 WS-LAST-TRM-CYYMMDD         PIC X(7)  VALUE SPACES.
           05 FILL5                       PIC X(42) VALUE SPACES.


       01  WS-RPT-FILE-HDR.
           05 WS-SUBR-FST-NME-HD   PIC X(10) VALUE 'FIRST NAME'.
           05 FILL1-HD             PIC X(1)  VALUE  ';'.
           05 WS-SUBR-LST-NME-HD   PIC X(15) VALUE 'LAST  NAME     '.
           05 FILL2-HD             PIC X(1)  VALUE ';'.
           05 WS-ADDRESS1-HD       PIC X(25) VALUE
                                           'ADDRESS LINE-1           '.
           05 FILL3-HD             PIC X(4) VALUE '   ;'.
           05 WS-ADDRESS2-HD       PIC X(25) VALUE
                                           'ADDRESS LINE-2           '.

           05 FILL4-HD             PIC X(4)  VALUE '   ;'.
           05 WS-CITY-NM-HD        PIC X(16) VALUE 'CITY NAME       '.
           05 FILL5-HD             PIC X(1)  VALUE ';'.
           05 WS-STATE-CODE-HD     PIC X(02) VALUE 'ST'.
           05 FILL6-HD             PIC X(1)  VALUE ';'.
           05 WS-COUNTY-NAME-HD    PIC X(18) VALUE 'COUNTY NAME       '.
           05 FILL7-HD             PIC X(1)  VALUE ';'.
           05 WS-ZIP-CODE-HD       PIC X(05) VALUE 'ZIPCD'.
           05 FILL8-HD             PIC X(1)  VALUE ';'.
           05 WS-SUBSCRIBR-ID-HD   PIC X(09) VALUE 'SUB-ID   '.
           05 FILL9-HD             PIC X(1)  VALUE ';'.
           05 WS-H-C-I-D-HD        PIC X(9)  VALUE  'H-C-I-D  '.
           05 FILL10-HD            PIC X(1)  VALUE ';'.
           05 WS-PHONE-HD          PIC X(18) VALUE 'PHONE NUMBER      '.
           05 FILL11-HD            PIC X(11) VALUE '          ;'.
           05 WS-SUB-DOB-HD        PIC X(10) VALUE 'BIRTH DATE'.
           05 FILL12-HD            PIC X(1)  VALUE ';'.
           05 WS-PLAN-NM-HD        PIC X(10) VALUE 'PLAN NAME '.
           05 FILLER-XX            PIC X(190) VALUE SPACES.
           05 FILL13-HD            PIC X(1)  VALUE ';'.
           05 WS-PLAN-TYP-HD       PIC X(06) VALUE 'PLNTYP'.
           05 FILL14-HD            PIC X(1)  VALUE ';'.
           05 WS-BRAND-NM-HD       PIC X(06) VALUE 'BRAND '.
           05 FILL15-HD            PIC X(1)  VALUE ';'.
           05 WS-BILL-FREQ-HD      PIC X(14) VALUE 'BILL FREQUENCY'.
           05 FILLYY               PIC X(86) VALUE SPACES.
           05 FILL16-HD            PIC X(1)  VALUE ';'.
           05 WS-PAYMENT-TYPE-HD   PIC X(06) VALUE 'PAYTYP'.
           05 FILL17-HD            PIC X(1)  VALUE ';'.
           05 WS-TOTAL-AMNT-DUE-HD PIC X(14) VALUE 'TOTAL DUE AMNT'.
           05 FILL18-HD            PIC X(1)  VALUE ';'.
           05 WS-UNALLOC-CASH-HD   PIC X(14) VALUE 'UNALLOC   CASH'.
           05 FILL19-HD            PIC X(1)  VALUE ';'.
           05 WS-PREM-DUE-DT-HD    PIC X(08) VALUE 'DUE DATE'.
           05 FILL20-HD            PIC X(1)  VALUE ';'.

       01  WS-RPT-FILE-UND         PIC X(552)  VALUE SPACES.

       01 REPORT-MAIL-FILE-TRL.
          05  WS-TRAILER-DETAIL1            PIC X(23)  VALUE
              'TOTAL NUMBER OF RECORDS'.
          05  WS-TRAILER-FILLER1            PIC X(01)  VALUE ';'.
          05  FILLER                        PIC X(524) VALUE SPACES.

       01  WS-ADDRESS1-TMP                PIC X(28) VALUE SPACES.
       01  WS-ADDRESS2-TMP                PIC X(28) VALUE SPACES.

       01  WS-EMAILRPT-DLM                PIC X     VALUE ';'.

       01 WS-PKG-ACT-FOUND                PIC X(1)   VALUE 'N'.
       01 WS-PKG-ACT-NOT-FOUND            PIC X(1)   VALUE 'N'.

       01 GRP-SUB-STATUS                  PIC X(1) VALUE 'Y'.           03081000
          88 GRP-SUB-FOUND                         VALUE 'Y'.           03082000
          88 GRP-SUB-NOT-FOUND                     VALUE 'N'.           03083000

       01  GROUP-SELECT                   PIC X.

       01  CLS-RISK-TABLE-NAMES.
           05  CLS-RISK-TRANSLATION.
                 10  CLS-RISK-NUMBER      PIC  9(4)  COMP.
           05  CLS-RISK-STATUS-ARGUE.
                 10  CLS-RISK-STATUS-OUTPUT   PIC XX.

       01 CS90-TABLE-TRANS-FIELDS.
          05  PRDCDTBL                   PIC X(08) VALUE 'NTUPRDCD'.
          05  NTUPVCTB                   PIC X(8)  VALUE 'NTUPVCTB'.
          05  TAG1                       PIC 9(04) COMP.
          05  BFOUR                      PIC 9(04) COMP SYNC VALUE 4.
          05  BFIVE                      PIC 9(04) COMP SYNC VALUE 5.
          05  BTEN                       PIC 9(4) COMP SYNC VALUE 10.
          05  BSIX                       PIC 9(04) COMP SYNC VALUE 6.
          05  BZERO                      PIC 9(04) COMP SYNC VALUE 0.
          05  B5                         PIC 9(04) COMP SYNC VALUE 5.
          05  RET-STATUS                 PIC 9(04) COMP.
          05  NUMERIC-DISPLAY            PIC 9(03)  VALUE ZEROES.

       01  COUNTERS.
           05  WS-SUB                      PIC 9(1) VALUE ZEROS.
           05  WS-REC-FOUND                PIC X(1) VALUE 'N'.
           05  EXTRACT-RCDS-OUT            PIC 9(09) VALUE 0.
           05  SUBSCRIBERS-FOUND           PIC 9(09) VALUE 0.
           05  MEMBERS-FOUND               PIC 9(09) VALUE 0.
           05  SUBSCRIBERS-READ            PIC 9(09) VALUE 0.
           05  MEMBERS-READ                PIC 9(09) VALUE 0.
           05  IX-PKG                      PIC 9(03) VALUE 0.
           05  WS-PHYSICAL-REC-CNT         PIC S9(7) VALUE +0.
           05  WS-GPNOT-OPT-CNT            PIC 9(7) VALUE ZEROS.
           05  WS-GPNOT-OPT-CNT1           PIC 9(7) VALUE ZEROS.
           05  GROUP-BENIFIT-PKGS          OCCURS 999 TIMES
                                           INDEXED BY IX-BPKG.
               10  GROUP-BPKG-DATA.
                   15  TBL-GB-PKG-NO           PIC 9(03) COMP-3.
                   15  TBL-GB-PROD-COMB-CD     PIC 9(04) COMP.

      * COPYBOOK FOR ALTERNATE ID MODULE S9SSSEX.
       01  SSN-ID-PARM.
           COPY S9SSSEXC.
      * ADDITIONAL PARAMETERS FOR CALLING ALTERNATE ID MODULE.
           COPY S9MB274C.
           COPY S9MAGEIO.
           COPY S9ZIPIO.
           COPY S9MZPTBL.

       01  ST-VSSPBIPC.
           COPY VSSPBIPC.

       01  VSFPBIR-SEGMENT.
           COPY VSFPBIRC.

       01  VSFPCSH-SEGMENT.
           COPY VSFPCSHC.

       01  VSGPGRU-SEGMENT.
           COPY VSGPGRUC.

       01  VSSPNTE-SEGMENT.
           COPY VSSPNTEC.

       01  VSGPPAC-SEGMENT.
           COPY VSGPPACC.

       01  VSGPPKG-SEGMENT.
           COPY VSGPPKGC.

       01  GRP-SUB-PTR.                                                 05230010
           COPY VSGPSUPC.                                               05240010

       01  VSGPPKA-SEGMENT.
           COPY VSGPPKAC.

       01  GO-VSGPNTEC.                                                 05170031
           COPY VSGPNTEC.                                               05180031

       01  SUBSCRIBER-ROOT-SEGMENT.                                     05040000
           COPY VSSPSUBC.

       01  SUB-MEMBER-SEGMENT.
           COPY VSSPMEMC.

       01  SUBSCRIBER-ADDRESS-SEGMENT.
           COPY VSSPADRC.

       01  SO-VSSVGRPC.
           COPY VSSVGRPC.

       01  VSBPMED-SEGMENT.
           COPY VSBPMEDC.

       01  VSBPNTE-SEGMENT.
           COPY VSBPNTEC.


       01 PROD-COMB-TRANS.
           COPY PRODTBL.

       01   GRP-NTE-FOUND-NOT-FOUND      PIC X(05) VALUE SPACES.
            88 GROUP-NTE-FOUND                     VALUE 'FOUND'.
            88 GROUP-NTE-NOT-FOUND                 VALUE 'NOT  '.

       01  VSSPSUB-SSA-QUAL.
           05  FILLER                PIC X(08) VALUE 'VSSPSUB '.
           05  SSA-SUB-CMDCD         PIC X(02) VALUE '*-'.
           05  SSA-SUB-LPAREN        PIC X(01) VALUE '('.
           05  FILLER                PIC X(08) VALUE 'SUBRIDD1'.
           05  FILLER                PIC X(02) VALUE 'EQ'.
           05  SSA-SUB-ID-KEY.
               10  SSA-SUB-ID-NO         PIC X(12).
               10  SSA-SUB-ID-CK         PIC X(03).
           05  FILLER                PIC X(01) VALUE ')'.

       01  SSA-VSGPNTE-UNQ               PIC X(9) VALUE 'VSGPNTE  '.    02810043
       01  VSSPBIP-SSA-UNQ               PIC X(9) VALUE 'VSSPBIP  '.    02810043
       01  VSSPMEM-SSA-UNQUAL.
           05  FILLER                    PIC X(8) VALUE 'VSSPMEM '.
           05  FILLER                    PIC X(01) VALUE ' '.

       01  VSGPGRU-SSA-QUAL.
           04  FILLER                    PIC X(8)    VALUE 'VSGPGRU '.
           04  FILLER                    PIC X       VALUE '('.
           04  FILLER                    PIC X(8)    VALUE 'GRUPKEYI'.
           04  VSGPGRU-OPR               PIC X(2)    VALUE '= '.
           04  VSGPGRU-KEY               PIC X(9).
           04  FILLER                    PIC X       VALUE ')'.


       01  VSGPGRU-SSA-UNQUAL.
           05  FILLER                    PIC X(08) VALUE 'VSGPGRU '.
           05  FILLER                    PIC X(01) VALUE ' '.

       01  VSSPNTE-SSA-UNQ.
           05  FILLER                    PIC  X(09) VALUE 'VSSPNTE  '.
       01  SSA-SUB-GRP-VSSLGRU.
           05  FILLER                    PIC X(11) VALUE 'VSSLGRU *D-'.
           05  FILLER                    PIC X(11) VALUE '(GRUPKEYI= '.
           05  SSA-SUBS-GROUP.
                08  GRPNO                PIC X(6).
                08  SUBGRPNO             PIC XXX.
           05  FILLER                    PIC X     VALUE ')'.
       01  VSBPNTE-SSA-UNQUAL.
           05  FILLER                PIC X(08) VALUE 'VSBPNTE'.
           05  FILLER                  PIC X(01) VALUE ' '.


       01  SSA-VSFPBIR-ROOT.                                            02990000
           05  SSA-BIR-SEG-NAME           PIC X(08) VALUE 'VSFPBIR '.   03000000
           05  SSA-BIR-LEFT-PAREN         PIC X(01) VALUE '('.          03010000
           05  SSA-BIR-FIELD-NAME         PIC X(08) VALUE 'BILLICMI'.   03020000
           05  SSA-BIR-OPER               PIC X(02) VALUE 'GE'.         03030000
           05  SSA-BIR-KEY.                                             03040000
               10  VSFPBIR-GRPKEY         PIC X(9).
               10  VSFPBIR-DUEDT          PIC 9(7)     COMP-3.
               10  VSFPBIR-SUBR           PIC X(15)    VALUE SPACE.

           05  SSA-RIGHT-PAREN            PIC X(01) VALUE ')'.          03050000

       01  SSA-VSFPBIR.
         03  SGN-VSFPBIR           PICTURE X(8) VALUE 'VSFPBIR '.
         03  CCA-VSFPBIR           PICTURE X VALUE '*'.
         03  CCF-VSFPBIR           PICTURE X VALUE '-'.
         03  CCL-VSFPBIR           PICTURE X VALUE '-'.
         03  CCD-VSFPBIR           PICTURE X VALUE '-'.
         03  CCN-VSFPBIR           PICTURE X VALUE '-'.
         03  CCC-VSFPBIR           PICTURE X VALUE '-'.
         03  CCU-VSFPBIR           PICTURE X VALUE '-'.
         03  CCV-VSFPBIR           PICTURE X VALUE '-'.
         03  CCP-VSFPBIR           PICTURE X VALUE '-'.
         03  BQ-VSFPBIR            PICTURE X VALUE '('.
         03  FLN-VSFPBIR           PICTURE X(8) VALUE 'BILLICMI'.
         03  RO-VSFPBIR            PICTURE XX VALUE '= '.
         03  VALUE-VSFPBIR         PICTURE X(28).
         03  EQ-VSFPBIR            PICTURE X VALUE ')'.

       01  SSA-VSFPBIR-BOOLEAN.
           03  SGN-VSFPBIR-BOOLEAN          PIC X(8)   VALUE 'VSFPBIR '.
           03  CCA-VSFPBIR-BOOLEAN          PIC X(01)  VALUE '*'.
           03  CCP-VSFPBIR-BOOLEAN          PIC X(01)  VALUE 'P'.
           03  FILLER                       PIC X      VALUE '('.
           03  FILLER                       PIC X(8)   VALUE 'BILLICMI'.
           03  FILLER                       PIC XX     VALUE 'GE'.
           03  FILLER                       PIC X(9)   VALUE SPACES.
           03  FILLER                       PIC X(4)   VALUE LOW-VALUES.
           03  BIR-SUB-KEY-1                PIC X(15).
           03  FILLER                       PIC X      VALUE '&'.
           03  FILLER                       PIC X(8)   VALUE 'BILLICMI'.
           03  FILLER                       PIC XX     VALUE 'LE'.
           03  FILLER                       PIC X(9)   VALUE SPACES.
           03  FILLER                       PIC X(4)  VALUE HIGH-VALUES.
           03  BIR-SUB-KEY-2                PIC X(15).
           03  FILLER                       PIC X      VALUE ')'.

       01  VSFPCSH-SSA-UNQUAL.
           04  FILLER                      PIC X(9)   VALUE 'VSFPCSH  '.

       01  SSA-GRP-PTR                     PIC X(9) VALUE 'VSSVGRP  '.

       01  VSGPBIL-SSA-UNQ.
           04  FILLER                      PIC  X(08) VALUE 'VSGPBIL '.
           04  FILLER                      PIC  X(03) VALUE SPACES.

       01 SSA-GROUP-PACKAGE.
          05  SSA-PAC-SEGNAME             PIC X(08)  VALUE 'VSGPPAC '.
          05  SSA-PAC-L-CALL-TYPE         PIC X(03)  VALUE '*--'.
          05  SSA-PAC-L-PAREN             PIC X(01)  VALUE '('.
          05  SSA-PAC-FIELD-NAME          PIC X(08)  VALUE 'NULLPKGN'.
          05  SSA-PAC-OPER                PIC X(02)  VALUE ' ='.
          05  SSA-PAC-PKG                 PIC 9(03)  COMP-3.
          05  SSA-PAC-R-PAREN             PIC X(01)  VALUE ')'.

       01  VSGPPAC-SSA-UNQUAL.
           05  FILLER                  PIC X(08) VALUE 'VSGPPAC '.
           05  FILLER                  PIC X(01) VALUE ' '.

       01  VSGPPKA-SSA-UNQUAL.
           05  FILLER                  PIC X(08) VALUE 'VSGPPKA '.
           05  FILLER                  PIC X(01) VALUE ' '.

       01  VSGPSUP-SSA-UNQUAL.
           05  FILLER                  PIC X(08) VALUE 'VSGPSUP '.
           05  VSGPSUP-CMDCD           PIC X(02) VALUE '*-'.
           05  FILLER                  PIC X(01) VALUE ' '.

       01  VSGPPKG-SSA-UNQUAL.
           05  FILLER                  PIC X(08) VALUE 'VSGPPKG '.
           05  VSGPPKG-CMDCD           PIC X(02) VALUE '*-'.
           05  FILLER                  PIC X(01) VALUE ' '.

       01  VSGPBIL-SSA-UNQUAL.
           05  FILLER                  PIC X(08) VALUE 'VSGPBIL '.
           05  VSGPBIL-CMDCD           PIC X(02) VALUE '*-'.
           05  FILLER                  PIC X(01) VALUE ' '.

       01  VSGPNTE-SSA-UNQUAL.
           05  FILLER                  PIC X(08) VALUE 'VSGPNTE '.
           05  FILLER                  PIC X(01) VALUE ' '.

        01  VSSPADR-SSA-UNQUAL.
            05  FILLER                    PIC X(08) VALUE 'VSSPADR '.
            05  FILLER                    PIC X(01) VALUE ' '.

       01  VSGPNTE-SSA-Q.
           04  FILLER                     PIC  X(08) VALUE 'VSGPNTE'.
           04  VSGPNTE-CMD-CD             PIC  X(03) VALUE '*--'.
           04  FILLER                     PIC  X(09) VALUE '(GPDFNTEI'.
           04  VSGPNTE-OPER1              PIC  X(02) VALUE '>='.
           04  VSGPNTE-KEY1.
               08  VSGPNTE-PAGE1          PIC  9(03) COMP-3.
               08  VSGPNTE-NOTE1          PIC  9(03) COMP-3 VALUE 000.
           04  VSGPNTE-BOOLEAN            PIC  X(01) VALUE '*'.
           04  FILLER                     PIC  X(08) VALUE 'GPDFNTEI'.
           04  VSGPNTE-OPER2              PIC  X(02) VALUE '<='.
           04  VSGPNTE-KEY2.
               08  VSGPNTE-PAGE2          PIC  9(03) COMP-3.
               08  VSGPNTE-NOTE2          PIC  9(03) COMP-3 VALUE 999.
           04  FILLER                     PIC  X(01) VALUE ')'.


       01  SSA-VSFPCSH.
         03  SGN-VSFPCSH           PICTURE X(8) VALUE 'VSFPCSH '.
         03  CCA-VSFPCSH           PICTURE X VALUE '*'.
         03  CCF-VSFPCSH           PICTURE X VALUE '-'.
         03  BQ-VSFPCSH            PICTURE X VALUE '('.
         03  FLN-VSFPCSH           PICTURE X(8).
         03  FLN-VSFPCSH           PICTURE X(8) VALUE 'UNALSRLN'.
         03  RO-VSFPCSH            PICTURE XX VALUE '= '.
         03  VALUE-VSFPCSH         PICTURE 9(8)         COMPUTATIONAL.
         03  EQ-VSFPCSH            PICTURE X VALUE ')'.

        01  VSBPMED-SSA-QUAL.
            05  SGN-VSBPMED           PIC X(08) VALUE 'VSBPMED '.
            05  CCA-VSBPMED           PIC X(03) VALUE '*--'.
            05  BP-VSBPMED            PIC X(01) VALUE '('.
            05  FLN-VSBPMED           PIC X(08) VALUE 'GPDFNTE1'.
            05  RO-VSBPMED            PIC X(02) VALUE ' ='.
            05  KEY-VSBPMED.
                10  VSBPMED-SSA-SUBRIDD1  PIC X(15) VALUE SPACES.
                10  VSBPMED-SSA-NULLDEPN  PIC 9(03) COMP-3 VALUE 0.
            05  EP-VSBPMED            PIC X(01) VALUE ')'.








       LINKAGE SECTION.

       01  IO-PCB                    PIC XX.

       01  VFGGRP10-PCB.
           05  GRP-DBD-NAME                PIC X(08).
           05  GRP-SEG-LEVEL               PIC X(02).
           05  GRP-DLI-STATUS-CODE         PIC X(02).
           05  GRP-PROC-OPT                PIC X(04).
           05  GRP-RESERVED-AREA           PIC S9(05) COMP.
           05  GRP-SEG-NAME                PIC X(08).
           05  GRP-KEY-LENGTH              PIC S9(05) COMP.
           05  GRP-NUMBER-SEGS             PIC S9(05) COMP.
           05  GRP-KEY-FEEDBACK            PIC X(100).

       01  VFSSUB10-PCB.
           05  SUB-DBD-NAME          PIC  X(08).
           05  SUB-SEG-LEVEL         PIC  X(02).
           05  SUB-DLI-STATUS-CODE   PIC  X(02).
           05  SUB-PROC-OPT          PIC  X(04).
           05  SUB-RESERVED-AREA     PIC S9(05) COMP.
           05  SUB-SEG-NAME          PIC  X(08).
           05  SUB-KEY-LENGTH        PIC S9(05) COMP.
           05  SUB-NUMBER-SEGS       PIC S9(05) COMP.
           05  SUB-KEY-FEEDBACK      PIC  X(100).

       01  BIR-PCB-AREA.
           05  BIR-DBD-NAME               PIC X(08).
           05  BIR-SEG-LEVEL              PIC X(02).
           05  BIR-DLI-STATUS-CODE        PIC X(02).
               88  BIR-GOOD-CALL              VALUE '  '.
               88  BIR-SEG-NOT-FOUND          VALUE 'GE'.
               88  BIR-END-OF-DB              VALUE 'GB'.
               88  BIR-OTHER-SEG              VALUE 'GA' 'GK'.
           05  BIR-PROC-OPT               PIC X(04).
           05  BIR-RESERVED-AREA          PIC S9(05) COMP.
           05  BIR-SEG-NAME               PIC X(08).
           05  BIR-KEY-LENGTH             PIC S9(05) COMP.
           05  BIR-NUMBER-SEGS            PIC S9(05) COMP.
           05  BIR-KEY-FEEDBACK           PIC X(24).

       01  MED-NOTE-PCB-AREA.
           03  FILLER                     PIC X(10).
           03  MNOTE-STATUS-CODE          PIC X(2).
               88  MNOTE-SUCCESSFUL                VALUE '  '.
               88  MNOTE-SEG-NOT-FOUND             VALUE 'GE'.
               88  MNOTE-END-OF-DB                 VALUE 'GB'.
               88  MNOTE-HI-BOUNDARY-CROSSED       VALUE 'GA'.
               88  MNOTE-SEG-BOUNDARY-CROSSED      VALUE 'GK'.
           03  FILLER                     PIC X(8).
           03  MNOTE-SEGMENT-NAME         PIC X(8).
           03  MNOTE-KFA-LENGTH           PIC S9(5) COMP.
           03  FILLER                     PIC X(4).
           03  MNOTE-FEEDBACK             PIC X(19).


       PROCEDURE DIVISION.
           ENTRY 'DLITCBL' USING IO-PCB
                                 VFGGRP10-PCB
                                 VFSSUB10-PCB
                                 BIR-PCB-AREA
                                 MED-NOTE-PCB-AREA.
           DISPLAY '********************************************'
           DISPLAY '********************************************'
           DISPLAY '************* TESTIMSP *********************'
           DISPLAY '********************************************'
           DISPLAY '********************************************'.

       000000-MAINLINE.

           OPEN OUTPUT REPORT-FILE.

           INITIALIZE  REPORT-FILE-REC.

           PERFORM 0100-SET-CURR-DATES.

           PERFORM 0900-GET-GROUP-ROOT.

           PERFORM 1000-EXTRACT-GROUPS THRU 1000-EXIT
              UNTIL GRP-DLI-STATUS-CODE  > SPACES.

           PERFORM 9900-END-OF-PROCESSING.

       0000-MAINLINE-CLOSE-DOWN.

           PERFORM 900000-DISPLAY-STATS.
           CLOSE REPORT-FILE.

      ****** ********* ************ ************ ************ **********

       000000-MAINLINE-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.

      ************** **************** *************** ******************
       0100-SET-CURR-DATES.

           ACCEPT WS-CURR-DATE  FROM DATE
           MOVE    20           TO WS-DATE-CONVERT(1:2)
           MOVE WS-CURR-DATE    TO WS-DATE-CONVERT(3:6)
           MOVE WS-DATE-CONVERT TO WS-CURR-PDATE
           MOVE WS-DATE-CONVERT TO WS-CURR-QDATE

           DISPLAY '********************************************'
           DISPLAY '********************************************'
           DISPLAY '*** PROCESS DATE = ' WS-DATE-CONVERT
           DISPLAY '********************************************'
           DISPLAY '********************************************'.

      *----+----+----+----+----+----+----+----+----+----+----+----+----*
      *----+----+----+----+----+----+----+----+----+----+----+----+----*
       0900-GET-GROUP-ROOT.
           CALL 'CBLTDLI' USING GN
                          VFGGRP10-PCB
                          VSGPGRU-SEGMENT
                          VSGPGRU-SSA-UNQUAL.


           IF GRP-DLI-STATUS-CODE  = '  ' OR 'GE' OR 'GB'
              NEXT SENTENCE
           ELSE
              MOVE 50 TO WS-RETURN-CODE
              DISPLAY '*                                          *'
              DISPLAY '*           0900-GET-GROUP-ROOT         ****'
              DISPLAY '****       ABNORMAL  TERMINATION        ****'
              DISPLAY '**** STATUS CODE      = ' GRP-DLI-STATUS-CODE
              DISPLAY '**** SSA              = ' VSGPGRU-SSA-UNQUAL
              DISPLAY '*                                          *'
              DISPLAY '********************************************'
              GO TO 0000-MAINLINE-CLOSE-DOWN.

       1000-EXTRACT-GROUPS.

           IF GA-GS-ACTIVE
              PERFORM 1000A-CONTINUE-GRP-EXTRACT.
              PERFORM 0900-GET-GROUP-ROOT.

       1000-EXIT. EXIT.

       1000A-CONTINUE-GRP-EXTRACT.

               MOVE GA-GRP-KEY-ID       TO  VSGPGRU-KEY
                                            WS-GROUP
               MOVE SPACES              TO  GRP-DLI-STATUS-CODE

               MOVE ZEROES              TO SUB2
               MOVE ZEROES              TO SUB1
               INITIALIZE                  WS-GRPPKG-DATA
                                           WS-TEMP-PKG-NO
               PERFORM 1001-GET-GRPPKG UNTIL
                         GRP-DLI-STATUS-CODE  > SPACES


               MOVE GA-GRP-KEY-ID  TO VSGPGRU-KEY
               MOVE SPACES         TO GRP-DLI-STATUS-CODE
               MOVE 'N'            TO WS-VSGPSUP-FLAG
               PERFORM 6000-GET-SUB-POINTER THRU 6000-EXIT
                                         UNTIL WS-VSGPSUP-FLAG = 'Y'.
       1001-GET-GRPPKG.

           MOVE SPACES             TO GRP-DLI-STATUS-CODE
           MOVE GA-GRP-KEY-ID      TO VSGPGRU-KEY
           INITIALIZE VSGPPAC-SEGMENT

           CALL 'CBLTDLI' USING GN
                                VFGGRP10-PCB
                                VSGPPAC-SEGMENT
                                VSGPGRU-SSA-QUAL
                                VSGPPAC-SSA-UNQUAL.

           IF GRP-DLI-STATUS-CODE  = '  '
              IF GB-PS-ACTIVE AND (WS-TEMP-PKG-NO NOT = GB-PKG-NO)
                    MOVE ZEROES  TO WS-TEMP-SUB-CNT
                    MOVE ZEROES  TO SUB1
                    PERFORM GB-OCCR-CNT-NO TIMES
                         ADD +1                 TO SUB1
                         ADD GB-SUBR-CNT (SUB1) TO WS-TEMP-SUB-CNT
                    END-PERFORM
      ** for group loading zero enrollment packages only to array
                IF (WS-TEMP-SUB-CNT = 0)
                   ADD  +1                  TO SUB2
                   MOVE GA-GRP-KEY-ID       TO WS-TBL-GROUP(SUB2)
                   MOVE GB-PKG-NO           TO WS-TEMP-PKG-NO
                   MOVE WS-TEMP-PKG-NO      TO WS-PKG-NO(SUB2)
                   MOVE GB-PROD-COMB-CD     TO WS-PROD-COMB-CD(SUB2)
                END-IF
              END-IF
           ELSE
              IF GRP-DLI-STATUS-CODE  = 'GE' OR 'GB'
                 CONTINUE
              ELSE
                 DISPLAY 'ERROR IN GRP PKG SEGMENT CALL!!!!' "-"
                         '1001-GET-GRPPKG'
                 DISPLAY 'STATUS-CODE: ' GRP-DLI-STATUS-CODE
                 DISPLAY 'SSA1       : ' VSGPGRU-SSA-QUAL
                 DISPLAY 'SSA2       : ' VSGPPAC-SSA-UNQUAL
                 MOVE 53              TO WS-RETURN-CODE
                 GO TO   0000-MAINLINE-CLOSE-DOWN.


       6000-GET-SUB-POINTER.

           MOVE SPACES       TO GRP-DLI-STATUS-CODE
           CALL 'CBLTDLI' USING GN                                      08710011
                                VFGGRP10-PCB                            08720009
                                GRP-SUB-PTR                             08730010
                                VSGPGRU-SSA-QUAL                        08740011
                                VSGPSUP-SSA-UNQUAL                      08750010

           IF GRP-DLI-STATUS-CODE = SPACES                              08760040

              IF GD-INACTIVE
                  MOVE  'N'            TO  WS-PKG-ACT-FOUND
                  PERFORM                 1100-GET-SUB-PKG-SEG
                  MOVE GD-SUBR-ID-KEY  TO SSA-SUB-ID-KEY
                  SET  GRP-SUB-FOUND   TO TRUE
              END-IF
           ELSE                                                         08800014
              IF GRP-DLI-STATUS-CODE = 'GE' OR 'GB'                     08810014
                 MOVE "Y"              TO WS-VSGPSUP-FLAG
              ELSE                                                      08830014
                 DISPLAY 'GROUP NO. : ' GA-GRP-KEY-ID
                 DISPLAY 'GRP-DLI-STATUS-CODE  : ' GRP-DLI-STATUS-CODE
                 DISPLAY 'PROBLEM WITH VSGPSUP SEGMENT'
                 GO        TO 0000-MAINLINE-CLOSE-DOWN                  06147100
              END-IF                                                    08970014
           END-IF.
       6000-EXIT. EXIT.

       1100-GET-SUB-PKG-SEG.
           INITIALIZE GRP-DLI-STATUS-CODE
                      WS-PKG-ACT-FOUND
                      VSGPPKG-SEGMENT

           CALL 'CBLTDLI' USING GNP
                                VFGGRP10-PCB
                                VSGPPKG-SEGMENT
                                VSGPGRU-SSA-QUAL
                                VSGPPKG-SSA-UNQUAL.

           IF GRP-DLI-STATUS-CODE  = '  '
                MOVE  ZEROS        TO WS-PKG-NO-NUM
                MOVE  GS-PKG-NO    TO WS-PKG-NO-NUM
                MOVE  'N'          TO WS-PKG-MATCH-FOUND
                MOVE  'N'          TO WS-PKG-END-REACHED

                SET PKG-INDEX  TO 1
                PERFORM 6001-SEARCH-PKG-TABLE
                          UNTIL (WS-PKG-MATCH-FOUND  = 'Y') OR
                                (WS-PKG-END-REACHED  = 'Y')

                 IF ((WS-PKG-MATCH-FOUND = 'Y') AND
                     (GS-PKG-TERM-DT < 2991231))
                     MOVE ZEROES                TO WS-TEMP-TRM-DT
                     MOVE ZEROES                TO WS-LAST-TRM-DT
                     MOVE GS-PKG-TERM-DT        TO WS-TEMP-TRM-DT
                     MOVE WS-TEMP-TRM-DT(4:2)   TO WS-LAST-TRM-DT(1:2)
                     MOVE WS-TEMP-TRM-DT(6:2)   TO WS-LAST-TRM-DT(3:2)
                     MOVE WS-TEMP-TRM-DT(2:2)   TO WS-LAST-TRM-DT(5:2)
                     PERFORM 0001-FORMAT-REPORTS
                 END-IF
           ELSE
            IF GRP-DLI-STATUS-CODE  = 'GE' OR 'GB'
               MOVE  'Y'                 TO WS-PKG-ACT-FOUND
               DISPLAY 'Error in sub-pkg segment '
            ELSE
              MOVE 52 TO WS-RETURN-CODE
              DISPLAY '****                                    ****'
              DISPLAY '****        1100-GET-GROUP-PKG-SEG      ****'
              DISPLAY '****      INVALID IMS STATUS CODE       ****'
              DISPLAY '****       ABNORMAL  TERMINATION        ****'
              DISPLAY '**** STATUS CODE      = ' GRP-DLI-STATUS-CODE
              DISPLAY '**** SSA1             = ' VSGPGRU-SSA-QUAL
              DISPLAY '**** SSA2             = ' VSGPPAC-SSA-UNQUAL
              DISPLAY '*                                          *'
              DISPLAY '********************************************'
              GO TO 0000-MAINLINE-CLOSE-DOWN.

       0100-EXIT. EXIT.

       6001-SEARCH-PKG-TABLE.

               SEARCH WS-GRPPKG-TBL
                  AT END
                       MOVE 'Y' TO WS-PKG-END-REACHED
                  WHEN ((WS-PKG-NO(PKG-INDEX)    = WS-PKG-NO-NUM) AND
                        (WS-TBL-GROUP(PKG-INDEX) = GA-GRP-KEY-ID))
                        MOVE  'Y'  TO WS-PKG-MATCH-FOUND.



       0001-FORMAT-REPORTS.
           MOVE SPACES                TO WS-RPT-FILE-REC
           MOVE GA-GRP-KEY-ID         TO WS-MIG-GRP-KEY
           MOVE WS-PKG-NO-NUM         TO WS-MIG-NO-ENR-PKG
           MOVE GD-SUBR-ID-KEY(4:9)   TO WS-MIG-INACT-SUBS
           MOVE WS-LAST-TRM-DT        TO WS-MIG-LTST-TRM-DT
           MOVE WS-TEMP-TRM-DT        TO WS-LAST-TRM-CYYMMDD
           WRITE REPORT-FILE-REC  FROM WS-RPT-FILE-REC

           MOVE SPACES TO  WS-RPT-FILE-REC
           MOVE ZEROES TO  WS-PKG-NO-NUM
           MOVE ZEROES TO  WS-LAST-TRM-DT.
       0001-EXIT.EXIT.


       9900-END-OF-PROCESSING.

       900000-DISPLAY-STATS.
           DISPLAY '********************************************'.
           DISPLAY '****------------------------------------****'.
           DISPLAY '****     TESTIMSP COMPLEATED SUCCESSFULLY***'.
           DISPLAY '****------------------------------------****'.
           DISPLAY '****------------------------------------****'.
           DISPLAY '****------------------------------------****'.
           DISPLAY '**** RETURN CODE           = ' WS-RETURN-CODE.
           DISPLAY '****------------------------------------****'.
           DISPLAY '****------------------------------------****'.
           DISPLAY '****         TESTIMSP SHUT DOWN         ****'.
           DISPLAY '****------------------------------------****'.
           DISPLAY '********************************************'.
