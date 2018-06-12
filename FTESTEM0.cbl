      *****************************************************************  
      *                                                                 
      *                TSR CONSULTING SERVICES, INC.                    
      *                                                                 
      *                       516-231-0333                              00006
      *                                                                 00007
      ***************************************************************** 00008
      *                                                                 00009
      ************** ATTENTION! ATTENTION! ATTENTION!                   00010
      ***                                                               00011
      ***                                                               00012
      *** THIS PROGRAM HAS BEEN CONVERTED FOR YEAR 2000 COMPLIANCE ON   00013
      ***                         01/29/1998.                           00014
      ***                                                               00015
      *** ANY MAINTENANCE DONE TO THIS PROGRAM THAT WILL RESULT IN THE  00016
      ***                                                               00017
      *** ADDITION OF NEW DATE FIELDS IN ANY FORM OR FORMAT SHOULD BE   00018
      ***                                                               00019
      *** DISCUSSED FIRST WITH YOUR MANAGER IN ORDER TO DETERMINE IF    00020
      ***                                                               00021
      *** THE PROGRAM SHOULD BE RECONVERTED BEFORE TESTING.             00022
      ***                                                               00023
      ***************************************************************** 00024
      ***   PROGRAM MODIFICATION:  BY JAB2I0   - 11/07/05           ***
      ***   REASON:  CR#107890
      ***   ADD RBRVS% TO TO BACK END REPORTING FOR IRS
      *****************************************************************
      *****************************************************************
      ***   PROGRAM MODIFICATION:  BY MAJ6I0   - 07/12/05           ***
      ***   REASON:  CR#108219
      ***   IDENTIFY AND PROCESS THE CDHP2 CLAIMS IN IRS.
      ***   CDHP2 CLAIMS CAN BE IDENTIFY BY HR-CORE-PROCESS-IND = 'CP'
      ***   FOR HOSPITAL CLAIMS AND MS-CMS-INDICATOR = 'CP' FOR MEDICAL
      **    CLAIMS IN VSAM CLAIM HISTORY FILE,
      ***   WHICH ARE PASSED TO IRPC-SYSTEM-IND ON ELIGIBLE FILE.
      ***   CDHP2 SHOULD BE REPORTED ON HRA AMOUNT COLUMN ON CHARGE
      ***   REGISTERS
      *****************************************************************
      ***   PROGRAM MODIFICATION:  BY MAJ6I0   - 11/21/05
      ***   REASON:  CR#111953 (MULTIPURSE)
      ***   IDENTIFY AND PROCESS THE HSA   CLAIMS IN IRS.
      ***   HSA AMOUNT FIELD WE GET FROM ICHIS & MANREP WHICH IS
      ***   FINALLY WE WILL REPORT ON CHARGE REGISTERS
      *****************************************************************
      ***   PROGRAM MODIFICATION:  BY VIN0I0   - 02/23/06
      ***   REASON:  IR#309075
      ***   COPYBOOK HAS BEEN CHANGED TO ADD A NEW VARIABLE
      ***   MS-TR-RECORD-COUNT1 FROM POSITION 120 TO 125 BYTES
      ***   TO REMOVE THE TRUNCATION PROBLEM OF THE TRAILER RECORDS.
      *****************************************************************
      ***   PROGRAM MODIFICATION:  BY MAJ6I0   - 03/13/06
      ***   REASON:  CR#118043
      ***   IRS VSAM DATABASE EXPANSION, VSAM FILE EXPANDED FROM
      ***   800 TO 850 BYTES(KEY IS EXPANDED FROM 22 TO 24 BYTES).
      *****************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY SRD4I0   - 05/16/06              *
      ***   REASON:  CR#117642                                         *
      ***   INCLUDE WP-MARKET-SEGMENT TO LAST 2 BYTES OF AGENCY CODE   *
      ***   ALSO INCLUDE 4 BYTES NUMERIC SIC CODE                      *
      ***   DONE AS PART OF WP SEGMENTATION-IRS                        *
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY SRD4I0   - 10/26/06              *
      ***   REASON:  CR#118886                                         *
      ***   TO INCLUDE NATIONAL PROVIDER ID (NPI) AND PASS THE SAME TO *
      ***   IFRS.                                                      *
      ***   DONE AS PART OF CR#118886 - IRS TO PASS NPI TO IFRS        *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY MAJ6I0   - 11/21/06              *
      ***   REASON:  CR#126674                                         *
      ***   ITS ACCESS FEE EXCLUSION FROM PAID-AMOUNT.                 *
      ***   LOGIC IS ADDED TO SET AN INDICATOR OF 'Y' TO ACCESS-FEE-IND*
      ***   TO IDENTIFY A CLAIM FOR WHICH ITS ACCESS FEE NEED TO BE    *
      ***   EXCLUDED FROM PAID-AMOUNT. THIS INDICATOR 'Y' WOULD BE USED*
      ***   LATER IN THE IRS SYSTEM TO EXCLUDE ITS ACCESS FEE          *
      ***   REASON:  CR#127537                                         *
      ***   IDENTIFY EMERGENCY ROOM CLAIMS FOR CT MED ADV              *
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY SRD4I0   - 05/08/08              *
      ***   REASON:  CR#140269                                         *
      ***   TO INCLUDE NIA PRORATE AMOUNT FROM ICHIS & PASS THE SAME   *
      ***   TO IFRS.                                                   *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY SRD4I0   - 10/13/08              *
      ***   REASON:  CR#139676                                         *
      ***   TO INCLUDE VOLUNTARY INDICATOR FOR BVV AND PASS THE SAME   *
      ***   TO IFRS.                                                   *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY VIN0I0   - 12/09/08              *
      ***   REASON:  CR#146587                                         *
      ***   TO INCLUDE MCS PROMPT PAY AMOUNT                           *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY AB64689  - 19/05/09              *
      ***   REASON:  CR#151788                                         *
      ***   TO PASS DEPENDENT NUMBER DOWN TO IRS VSAM FILE             *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY AB64689  - 27/01/10              *
      ***   REASON:  CR#147553  -  NCN PROJECT                         *
      ***   TO INCLUDE NCN INDICATOR AND COMMISSION FROM MANREP AND    *
      ***   PASS THE SAME TO IFRS                                      *
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY AB64689  - SEPTEMBER 2010        *
      ***   REASON:  CR#160671  -  NCN EXPANSION                       *
      ***            CR#161042  -  APR DRG PROJECT                     *
      *** - TO INCLUDE NCN INDICATOR,COMMISSION AND GROUP FEE FROM     *
      ***   ICHIS AND GROUP FEE FROM MANREP , PASS THE SAME TO IFRS    *
      *** - TO PASS THE SEVERITY OF ILLNESS (SOI) FROM ICHIS TO IFRS   *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION:  BY SARIW0   - DECEMBER 2012         *
      ***   REASON:  CQ#284606  -  RSI INDICATOR                       *
      *** - TO INCLUDE RSI INDICATOR FROM ICHIS AND MANREP, PASS THE   *
      ***   SAME TO IFRS                                               *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: MARCH 2013                      *
      ***   PROGRAMMER: KHYATI                                         *
      ***   ADDDED NARROW NETWORKS INDICATOR                           *
      ***   TO INCLUDE NARROW NETWORKS INDICATOR FROM ICHIS AND MANREP,*
      ***   PASS THE SAME TO IFRS AS PER CQ342710                      *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: APRIL 2013                      *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   BLOOM LG PROJECT                                           *
      ***   TO INCLUDE BLOOM EXHCHANGE INDICATOR FROM ICHIS & MANREP,  *
      ***   PASS THE SAME TO IFRS AS PER CQ377400                      *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: AUG   2013                      *
      ***   PROGRAMMER: MAHESH                                         *
      ***   EFT      PROJECT                                           *
      ***   TO INCLUDE EFT INDICATOR             FROM ICHIS & MANREP,  *
      ***   PASS THE SAME TO IFRS AS PER CQ496568                      *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: OCT  2013                       *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   IRS ICD10 EXPANSION                                        *
      ***   EXPANDED ICHIS COPYBOOK FROM 6550 BYTES TO 7550, MANREP    *
      ***   COPYBOOK FROM 900 BYTES TO 1500 BYTES AND  IRS COPYBOOK    *
      ***   FROM 850 BYTES TO 970 BYTES TO ACCOMODATE ICD10 FIELDS AS  *
      ***   PER CR171180.                                              *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: AUG   2014                      *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   DECARE EFT PROJECT                                         *
      ***   TO INCLUDE EFT INDICATOR & TRACE NUMBER FOR DENTAL FEED,   *
      ***   PASS THE SAME TO IFRS AS PER CQ989332                      *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: MAR   2015                      *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   ITS PROJECT                                                *
      ***   TO ADD ITS SUPPLEMENTAL AMOUNT FOR HOSPITAL AND MEDICAL,   *
      ***   PASS THE SAME TO IFRS AS PER CQ1370889                     *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: APRIL 2015                      *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   EBF PROJECT                                                *
      ***   TO INCLUDE EBF INDICATOR FROM ICHIS & MANREP, AND PASS THE *
      ***   SAME TO IFRS, RECON AND IRS ELIGIBLE FILE.                 *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: OCTOBER 2015                    *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   LOCAL ACCESS FEE PROJECT                                   *
      ***   TO ADD LOCAL ACCESS FEE FIELD FOR HOSPITAL AND MEDICAL,    *
      ***   PASS THE SAME TO IFRS AS PER CQ1707133                     *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: JANUARY 2017                    *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   POOLING REPORT CHANGES PROJECT                             *
      ***   TO ADD PLACE OF SERVICE FIELD FOR HOSPITAL AND MEDICAL,    *
      ***   PASS THE SAME TO IFRS AS PER RTC269752                     *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: JANUARY 2017                    *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   POOL P AMOUNT CHANGES                                      *
      ***   TO ADD POOL P AMOUNT FIELD FOR HOSPITAL AND MEDICAL, AND   *
      ***   PASS THE SAME TO IFRS AS PER RTC319073                     *
      ******************************************************************
      ******************************************************************
      ***   PROGRAM MODIFICATION DATE: JANUARY 2017                    *
      ***   PROGRAMMER: SANDEEP                                        *
      ***   TOTAL CHARGES AMOUNT CHANGES                               *
      ***   TO ADD TOTAL CHARGES FIELD FOR HOSPITAL AND MEDICAL, AND   *
      ***   PASS THE SAME TO IFRS AS PER RTC274627                     *
      ******************************************************************
      ******************************************************************
000100 IDENTIFICATION DIVISION.                                         00025
000200 PROGRAM-ID. FTESTEM0.                                            00026
000300 AUTHOR. E. VOLYNSKY.                                             00027
000400*                                                                 00028
000800*  FOR INSTITUTIONAL CLAIMS:                                      00029
000900*                                                                 00030
001000*  THIS PROGRAM IDENTIFIES SENIOR CARE PRESSO, RIDER AND          00031
001100*  SUPPLEMENTAL PAYMENTS, SETTING AN INDICATOR ON THE RECORD.     00032
001200*                                                                 00033
001300*  FOR \ENTAL CLAIMS:                                             00034
001400*                                                                 00035
001500*  TOTALS ARE PROVIDED FOR INPUT TRANSACTIONS WHICH ARE BYPASSED  00036
001600*  OUTRIGHT DUE TO THEIR HAVING ZEROS IN THEIR PAID AMOUNT FIELDS.00037
001700*                                                                 00038
001800*                                                                 00039
001900*  FOR DRUG CLAIMS:                                               00040
002000*                                                                 00041
002100*  TOTALS ARE PROVIDED FOR INPUT TRANSACTIONS WHICH ARE BYPASSED  00042
002200*  OUTRIGHT DUE TO THEIR HAVING ZEROS IN THEIR PAID AMOUNT FIELDS.00043
002300*                                                                 00044
002400*  FOR MEDICAL CLAIMS:                                            00045
002500*                                                                 00046
002600*  THIS PROGRAM IDENTIFIES SENIOR CARE WITH AN INDICATOR.  IT ALSO00047
002700*  PRODUCES CONTROL TOTALS FOR MAIL ORDER DRUG CLAIMS WHICH ARE   00048
002800*  BEING RE-ROUTED TO OUTPUT FILE FN981OTH FOR LATER INPUT TO     00049
002900*  PROGRAM FTESTEM0.  FTESTEM0 WILL PROCESS THESE CSS FILE CLAIMS 00050
003000*  AND OUTPUT THEM UNDER THE DRUG LINE OF BUSINESS. ADDITIONALLY, 00051
003100*  TOTALS ARE ALSO PROVIDED FOR ANY INPUT TRANSACTIONS WHICH WILL 00052
003200*  BE DROPPED FROM FURTHER PROCESSING BECAUSE THEY HAVE ZEROS     00053
003300*  IN THEIR PAID AMOUNT FIELDS.                                   00054
003400*                                                                 00055
003500******************************************************            00056
003600**** COMPREHENSIVE REPORTING DATA EXTRACT PROGRAM ****            00057
003700******************************************************            00058
003800** THIS MONTHLY PAID CLAIMS DATA EXTRACT PROGRAM    **            00059
003900** (FTESTEM0) IS DESIGNED TO CREATE PAID CLAIM      **            00060
004000** EXTRACT RECORDS FOR USE IN THE COMPREHENSIVE     **            00061
004100** CHARGE REGISTERS SYSTEM AND THE GROUP TRIANGLE   **            00062
004200** REPORTING SYSTEM.                                **            00063
004300**                                                  **            00064
004400**   --------------     -------    ---------------- **            00065
004500**   FEEDING SYSTEM     JOBNAME    LINE OF BUSINESS **            00066
004600**   --------------     -------    ---------------- **            00067
004700**   I.C.H.I.S.         HRNYPYMT   INSTITUTIONAL    **            00068
004800**   MANREP             MSCLMREF   SURG/MEDICAL     **            00069
004900**   MANREP             MSCLMREF   MAJOR MEDICAL    **            00070
005000**   DENTAL             --------   DENTAL           **            00071
005100**   DRUG               --------   DRUG             **            00072
005200******************************************************            00073
005300***************** OVERVIEW****************************            00074
005400*                                                                 00075
005500******************************************************            00076
005600******************************************************            00077
005700                                                                  00078
005800     EJECT                                                        00079
005900 ENVIRONMENT DIVISION.                                            00080
006000 CONFIGURATION SECTION.                                           00081
006100 SOURCE-COMPUTER. IBM-370.                                        00082
006200 OBJECT-COMPUTER. IBM-370.                                        00083
006300 SPECIAL-NAMES.                                                   00084
006400     C01 IS TO-NEW-PAGE.                                          00085
006500                                                                  00086
006600 INPUT-OUTPUT SECTION.                                            00087
006700 FILE-CONTROL.                                                    00088
006800     SELECT I-HRD88DTE-DATE-FILE        ASSIGN TO UT-S-HRD88DTE.  00089
006900     SELECT I-COMBINED-PAYMENTS-FILE    ASSIGN TO UT-S-HR501NOT.  00090
006900     SELECT I-COMBINED-REJECT-FILE      ASSIGN TO UT-S-HR501REJ.  00091
006900     SELECT I-COMBINED-PAY950-FILE      ASSIGN TO UT-S-HR505CRT.  00092
007000     SELECT I-MCS-PAID-CLAIMS-FILE      ASSIGN TO UT-S-FN982MCS.  00093
007100     SELECT I-DENTAL-PAID-CLAIMS-FILE   ASSIGN TO UT-S-FN982DEN.  00094
007100*-CR#126674 ITS ACCESS FEE EXCLUSION CHANGES START                00094
007100     SELECT I-ACCESS-FEE-GROUP-FILE     ASSIGN TO UT-S-FNAFEE.    00094
007100*-CR#126674 ITS ACCESS FEE EXCLUSION CHANGES END                  00094
007300     SELECT O-COMMON-LOB-PAID-CLAIMS    ASSIGN TO UT-S-FN982PCO.  00096
007400     SELECT O-OTHER-MEDICAL-FEED-CLAIMS ASSIGN TO UT-S-FN982OTH.  00097
007500     SELECT O-PAID-CLAIM-CONTROL-REPORT ASSIGN TO UT-S-FN982CTL.  00098
007600                                                                  00099
007700     EJECT                                                        00100
007800 DATA DIVISION.                                                   00101
007900 FILE SECTION.                                                    00102
008000                                                                  00103
008100 FD  I-HRD88DTE-DATE-FILE                                         00104
008200     RECORDING MODE IS F                                          00105
008300     BLOCK CONTAINS 0 RECORDS                                     00106
008400     RECORD CONTAINS 80 CHARACTERS                                00107
008500     LABEL RECORDS ARE STANDARD                                   00108
008600     DATA RECORD IS HR-PROD-DATE-REC.                             00109
008700 01  HR-PROD-DATE-REC                        PIC X(80).           00110
008800                                                                  00111
008900 FD  I-COMBINED-PAYMENTS-FILE                                     00112
009000     BLOCK CONTAINS 0 RECORDS                                     00113
009100     LABEL RECORD ARE STANDARD                                    00114
009200     DATA RECORD IS I-COMBINED-PAYMENTS-REC.                      00115
       01  I-COMBINED-PAYMENTS-REC                 PIC X(7550).         00116
009400                                                                  00117
008900 FD  I-COMBINED-REJECT-FILE                                       00118
009000     BLOCK CONTAINS 0 RECORDS                                     00119
009100     LABEL RECORD ARE STANDARD                                    00120
009200     DATA RECORD IS I-COMBINED-REJECT-REC.                        00121
009300 01  I-COMBINED-REJECT-REC                 PIC X(260).            00122
009400                                                                  00123
008900 FD  I-COMBINED-PAY950-FILE                                       00124
009000     BLOCK CONTAINS 0 RECORDS                                     00125
009100     LABEL RECORD ARE STANDARD                                    00126
009200     DATA RECORD IS I-COMBINED-PAY950-REC.                        00127
       01  I-COMBINED-PAY950-REC                 PIC X(7600).           00128
009400                                                                  00129
009500 FD  I-MCS-PAID-CLAIMS-FILE                                       00130
009600     BLOCK CONTAINS 0 RECORDS                                     00131
009700     LABEL RECORD ARE STANDARD                                    00132
009800     DATA RECORD IS I-MCS-PAID-CLAIM-REC.                         00133
       01  I-MCS-PAID-CLAIM-REC                  PIC X(1500).           00134
010000                                                                  00135
010000                                                                  00136
010100 FD  I-DENTAL-PAID-CLAIMS-FILE                                    00137
010200     BLOCK CONTAINS 0 RECORDS                                     00138
010300     LABEL RECORD ARE STANDARD                                    00139
010400     DATA RECORD IS I-DEN-PAID-CLAIM-REC.                         00140
010500 01  I-DEN-PAID-CLAIM-REC                    PIC X(400).          00141
010600                                                                  00142
010600*-CR#126674 ITS ACCESS FEE EXCLUSION CHANGES START                00142
010600 FD  I-ACCESS-FEE-GROUP-FILE                                      00142
010600     RECORDING MODE IS F                                          00142
010600     BLOCK CONTAINS 0 RECORDS                                     00142
010600     RECORD CONTAINS 80 CHARACTERS                                00142
010600     LABEL RECORDS ARE STANDARD                                   00142
010600     DATA RECORD IS I-ACCESS-FEE-GROUP-REC.                       00142
010600 01  I-ACCESS-FEE-GROUP-REC    PIC X(80).                         00142
010600*-CR#126674 ITS ACCESS FEE EXCLUSION CHANGES END                  00142
011400     EJECT                                                        00150
011500 FD  O-OTHER-MEDICAL-FEED-CLAIMS                                  00151
011600     BLOCK CONTAINS 0 RECORDS                                     00152
011700     LABEL RECORD ARE STANDARD                                    00153
011800     DATA RECORD IS O-MCS-FEED-CLAIM-REC.                         00154
      *01  O-MCS-FEED-CLAIM-REC                    PIC X(900).          00155
       01  O-MCS-FEED-CLAIM-REC                    PIC X(1500).         00155
012000                                                                  00156
012100 FD  O-PAID-CLAIM-CONTROL-REPORT                                  00157
012200     BLOCK CONTAINS 0 RECORDS                                     00158
012300     LABEL RECORD ARE STANDARD                                    00159
012400     DATA RECORD IS O-PD-CLM-CONTROL-REPORT.                      00160
012500 01  O-PD-CLM-CONTROL-REPORT                 PIC X(133).          00161
012600                                                                  00162
013300                                                                  00163
013400     EJECT                                                        00164
012700 FD  O-COMMON-LOB-PAID-CLAIMS                                     00165
012800     BLOCK CONTAINS 0 RECORDS                                     00166
RS1298**** RECORD CONTAINS 664 CHARACTERS                               00167
RS1298*- IRS VSAM EXPANSION CHANGES START                               00168
RS1298*    RECORD CONTAINS 839 CHARACTERS                               00168
      *    RECORD CONTAINS 887 CHARACTERS                               00168
           RECORD CONTAINS 1007 CHARACTERS                              00168
RS1298*- IRS VSAM EXPANSION CHANGES END                                 00168
013000     LABEL RECORD ARE STANDARD                                    00169
013100     DATA RECORD IS O-PD-CLM-FILE.                                00170
RS1298*01  O-PD-CLM-RECORD                         PIC X(664).          00171
RS1298*- IRS VSAM EXPANSION CHANGES START                               00168
RS1298*01  O-PD-CLM-RECORD                         PIC X(839).          00172
RS1298*01  O-PD-CLM-RECORD                         PIC X(887).          00172
       01  O-PD-CLM-RECORD                         PIC X(1007).
RS1298*- IRS VSAM EXPANSION CHANGES END                                 00168
013300                                                                  00173
013400     EJECT                                                        00174
013500 WORKING-STORAGE SECTION.                                         00175
                                                                        00176
C21TSR*************** START OF CATCH21 DATA ITEMS *****************     00177
                                                                        00178
C21TSR     COPY C21WS56.                                                00179
C21TSR     05  C21WS-COMPARE-PAID-DATE        PIC X(6).                 00180
C21TSR     05  C21WS-COMPARE-INCURRED-DATE    PIC X(6).                 00181
C21TSR     05  C21WS-90                       PIC X(2).                 00182
C21TSR     05  C21H3-YEAR                     PIC X(02).                00183
C21TSR     05  C21HRCP-ACTN-YR                PIC XX.                   00184
C21TSR     05  C21HRCR-ACTN-YR                PIC XX.                   00185
C21TSR     05  C21SEL-PAID-YEAR               PIC X(02).                00186
C21TSR     05  C21DT-GREG-DATE6               PIC X(6).                 00187
C21TSR     05  C2196M02  PIC X(8)  VALUE 'C2196M02'.                    00188
C21TSR     05  C2140C02  PIC X(8)  VALUE 'C2140C02'.                    00189
C21TSR     05  C2110C06  PIC X(8)  VALUE 'C2110C06'.                    00190
C21TSR     05  C21-STATUS   PIC X  VALUE '0'.                           00191
                                                                        00192
C21TSR******************* END OF CATCH21 DATA *********************     00193
                                                                        00194
                                                                        00195
013600     SKIP2                                                        00196
013700 01  FILLER                                  PIC X(40) VALUE      00197
013800       'WORKING STORAGE STARTS HERE FOR FTESTEM0'.                00198
013900     SKIP2                                                        00199
014000 01  WS-MISC.                                                     00200
014700     05  ECKS                         PIC X VALUE 'X'.            00201
013900     SKIP2                                                        00202
014000 01  WS-POOL-REGION-CODE.                                         00203
039300      05 WS-POOL-REGION-CODE-1    PIC X(01).                      00204
039300      05 WS-POOL-REGION-CODE-2    PIC X(01).                      00205
014000 01  WS-CLAIM-NUMBER.                                             00206
014700     05  WS-ORIGINAL-CLAIM-NUMBER     PIC X(14).                  00207
014800     05  WS-CLAIM-NUMBER REDEFINES WS-ORIGINAL-CLAIM-NUMBER.      00208
014900         10  WS-CLAIM-NUMBER6         PIC X(06).                  00209
014900         10  WS-CLAIM-NUMBER8         PIC X(08).                  00210
014200                                                                  00211
014000 01  WS-UNPACK-CLAIM-NUMBER.                                      00212
014700     05  TEST-BR-ID                   PIC X.                      00213
014800     05  TEST-CASE-ID                 PIC X.                      00214
014900     05  TEST-CASENO-LST6  PIC 9(11) VALUE ZEROES.                00215
014200                                                                  00216
014000 01  WS-UNPACK-REJ-CLAIM-NUMBER.                                  00217
014700     05  REJ-TEST-BR-ID                   PIC X.                  00218
014800     05  REJ-TEST-CASE-ID                 PIC X.                  00219
014800     05  REJ-TEST-MIU-ID                  PIC X.                  00220
014900     05  REJ-TEST-CASENO-LST6  PIC 9(11) VALUE ZEROES.            00221
014200                                                                  00222
 04140 01  WS-AMT-PD-BY-OTH-CARRIER     PIC S9(07)V99 VALUE ZEROES.     00223
014100 01  WS-ENTRY               PIC S9(08) COMP SYNC VALUE ZEROES.    00224
014200                                                                  00225
014300 01  WS-SPLIT-RECORD-SWITCH                PIC X VALUE '0'.       00226
014400     88 CLAIM-PAID-PRIOR-JULY0191  VALUE '0'.                     00227
014500     88 CLAIM-PAID-AFTER-JUNE3091  VALUE '1'.                     00228
014300 01  WS-CS90-PROCESS                       PIC XX VALUE 'CS'.     00232
014400     88 WS-CS90-NEW-YORK-STATE             VALUE 'CN'.            00233
014500     88 WS-CS90-NYS-CENTRALIZATION         VALUE 'CC'.            00234
014500     88 WS-CS90-NYS-UPSTATE                VALUE 'CU'.            00235
014500     88 WS-CS90-MABIL                      VALUE 'CA'.            00236
014500     88 WS-CS90-NYC                        VALUE 'CY'.            00237
014500     88 WS-CORE-CS90-PROCESS                                      00238
014500     VALUE 'CS' 'CN' 'CC' 'CU' 'CA' 'CY'.                         00239
014600 01  WS-CSS-LITERALS.                                             00240
014700     05 WS-PAID-CSS.                                              00241
014800        10 WS-PAID-YR-CSS             PIC XX.                     00242
014900        10 WS-PAID-MO-CSS             PIC XX.                     00243
015000     05 WS-PAID-CSS-RDFN                                          00244
015100        REDEFINES WS-PAID-CSS.                                    00245
015200        10  WS-DATE-CSS                       PIC X(4).           00246
015300            88 SPLIT-AFTER-JUNE3091     VALUE '9107' THRU '9912'. 00247
015400            88 SPLIT-BEFOR-JULY0191     VALUE '0101' THRU '9106'. 00248
014700     05 WS-DRUG-PAID-CSS.                                         00249
014800        10 WS-PAID-DRUG-YR-CSS             PIC XX.                00250
014900        10 WS-PAID-DRUG-MO-CSS             PIC XX.                00251
015000     05 WS-PAID-CSS-RDFN                                          00252
015100        REDEFINES WS-DRUG-PAID-CSS.                               00253
015200        10  WS-DRUG-DATE-CSS                       PIC X(4).      00254
015300            88 SPLIT-AFTER-DEC3196      VALUE '9701' THRU '9912'. 00255
015400            88 SPLIT-BEFOR-JAN0197      VALUE '0101' THRU '9612'. 00256
015500 01  WS-ICHIS-LITERALS.                                           00257
015600     05 WS-PAID-ICHIS.                                            00258
015700        10 WS-PAID-YR-ICHIS           PIC XX.                     00259
015800        10 WS-PAID-MO-ICHIS           PIC XX.                     00260
015900     05 WS-PAID-ICHIS-RDFN                                        00261
016000        REDEFINES WS-PAID-ICHIS.                                  00262
016100        10 WS-DATE-ICHIS              PIC X(4).                   00263
016200           88  ICHIS-AFTER-OCT3191    VALUE '9111' THRU '9912'.   00264
016300           88  ICHIS-BEFOR-NOV0191    VALUE '0101' THRU '9110'.   00265
016400     SKIP3                                                        00266
016500 01  WS-CRITCAL-ERR-MSG.                                          00267
016600     05  PARM-OMISSION-MESSAGE.                                   00268
016700         10 WS-CRITICAL-ERROR-MESSAGE-1  PIC X(50) VALUE          00269
016800            '-> EXECUTION CANCELLED - REQUIRED PARM MISSING.   '. 00270
016900         10 WS-CRITICAL-ERROR-MESSAGE-2  PIC X(50) VALUE          00271
017000            '-> REFER TO PROC FOR PARM VALUE INFORMATION       '. 00272
017100         10 WS-CRITICAL-ERROR-MESSAGE-3  PIC X(50) VALUE          00273
017200            '-> EXECUTION TERMINATED BECAUSE A LOGIC ERROR     '. 00274
017300         10 WS-CRITICAL-ERROR-MESSAGE-4  PIC X(50) VALUE          00275
017400            '-> EXISTS  IN FORMATTING THE OUTPUT RECORD        '. 00276
017500     EJECT                                                        00277
017600 01  WS-WORKING-STORAGE-AREA.                                     00278
017700     05 USER-ABEND-CODE    COMP  SYNC    PIC S9(4)  VALUE +0.     00279
017800                                                                  00280
017900     05  COMPILE-VERSION.                                         00281
018000         10 COMPILE-TIME                 PIC X(8).                00282
018100         10 COMPILE-DATE                 PIC X(12).               00283
018200                                                                  00284
018300     05  DATE-REC.                                                00285
018400         10  CURRENT-MONTH               PIC X(2).                00286
018500         10  CURRENT-DAY                 PIC X(2).                00287
018600         10  CURRENT-YEAR                PIC X(2).                00288
018700                                                                  00289
018800     05  CURRENT-YYMM.                                            00290
018900         10  CURRENT-YY                  PIC 9(02) VALUE 0.       00291
019000         10  CURRENT-MM                  PIC 9(02) VALUE 0.       00292
019100                                                                  00293
018800     05  DESTID-DATE.                                             00290
018900         10  DESTID-CC                  PIC X(02).                00291
 18900         10  DESTID-YY                  PIC X(02).                00291
018900         10  DESTID-MM                  PIC X(02).                00291
018900         10  DESTID-DD                  PIC X(02).                00291
019100                                                                  00293
019200     05  TIME-DAY.                                                00294
019300         10 TIME-HOURS                   PIC X(2).                00295
019400         10 TIME-MINUTES                 PIC X(2).                00296
019500         10 TIME-SECONDS                 PIC X(2).                00297
019600                                                                  00298
019700     05  SAVED-HEADER-DATES.                                      00299
019800         10 WS-CHECK-PAID-DATE-YY        PIC 99  VALUE ZERO.      00300
019900         10 WS-CHECK-PAID-DATE-MM        PIC 99  VALUE ZERO.      00301
020000         10 WS-CHECK-PAID-DATE-DD        PIC 99  VALUE ZERO.      00302
020100                                                                  00303
020200     05 WS-COMPARE-PAID-DATE.                                     00304
020300         10 WS-COMPARE-P-YEAR            PIC XX.                  00305
020400         10 WS-COMPARE-P-MONTH           PIC XX.                  00306
020500         10 WS-COMPARE-P-DAY             PIC XX.                  00307
020600     SKIP2                                                        00308
020700     05 WS-COMPARE-INCURRED-DATE.                                 00309
020800         10 WS-COMPARE-I-YEAR            PIC XX.                  00310
020900         10 WS-COMPARE-I-MONTH           PIC XX.                  00311
021000         10 WS-COMPARE-I-DAY             PIC XX.                  00312
021100     SKIP2                                                        00313
021200     05  COMPILED-MESSAGE.                                        00314
021300         10  WS-MODULE-NAME              PIC X(17) VALUE          00315
021400             'PGM = FTESTEM0 - '.                                 00316
021500                                                                  00317
021600     05  PARM-SUPPLIED-MESSAGE.                                   00318
021700         10  WS-DISPLAY-PARM-OTHER       PIC X(43) VALUE          00319
021800             '          OTHER  PARM VALUE RECEIVED IS--> '.       00320
021700         10  WS-DISPLAY-PARM-MESSAGE     PIC X(43) VALUE          00321
021800             'LINE OF BUSINESS PARM VALUE RECEIVED IS--> '.       00322
021900         10  WS-PARM-CODE                PIC X     VALUE SPACE.   00323
022000                                                                  00324
022100     05  COMPILED-ON-MESSAGE.                                     00325
022200         10  WS-COMPILE-MSG              PIC X(13) VALUE          00326
022300             'COMPILED ON: '.                                     00327
022400         EJECT                                                    00328
022500 01  WS-DEF-MESSAGES.                                             00329
022600         SKIP3                                                    00330
022700     05  WS-DEF-CLM-MSG.                                          00331
022800         10  FILLER   PIC X(19) VALUE ' CLAIM NUMBER:     '.      00332
022900     05  WS-DEF-DTE-MSG.                                          00333
023000         10  FILLER   PIC X(19) VALUE '  DEFAULT DATE IS: '.      00334
023100     05  WS-DEF-SRC-MSG.                                          00335
023200         10  FILLER   PIC X(19) VALUE '   SOURCE CODE IS: '.      00336
023300 01  WS-DEFAULT-COUNTERS.                                         00337
023400         SKIP3                                                    00338
023500     05  WS-DEF-COUNTS.                                           00339
023600         10  FILLER   PIC X(19) VALUE '  -DEFAULT COUNTS- '.      00340
023700     05  WS-DEF-CLAIM.                                            00341
023800         10  FILLER   PIC X(19) VALUE 'DEFAULTED CLAIMS = '.      00342
023900         10  WS-DEF-CLAIM-CNT            PIC 9(09) VALUE 0.       00343
024000     05  WS-DEF-SUBID.                                            00344
024100         10  FILLER   PIC X(19) VALUE 'DEFAULTED SUBIDS = '.      00345
024200         10  WS-DEF-SUBID-CNT            PIC 9(09) VALUE 0.       00346
024300     05  WS-DEF-PDDAY.                                            00347
024400         10  FILLER   PIC X(19) VALUE 'DEFAULTED PD DAY = '.      00348
024500         10  WS-DEF-PDDAY-CNT            PIC 9(09) VALUE 0.       00349
024600     05  WS-DEF-INCDA.                                            00350
024700         10  FILLER   PIC X(19) VALUE 'DEFAULTED INCDAY = '.      00351
024800         10  WS-DEF-INCDA-CNT            PIC 9(09) VALUE 0.       00352
024900     05  WS-DEF-INCYR.                                            00353
025000         10  FILLER   PIC X(19) VALUE 'DEFAULTED INCYR  = '.      00354
025100         10  WS-DEF-INCYR-CNT            PIC 9(09) VALUE 0.       00355
025200     05  WS-DEF-INCMO.                                            00356
025300         10  FILLER   PIC X(19) VALUE 'DEFAULTED INCMO  = '.      00357
025400         10  WS-DEF-INCMO-CNT            PIC 9(09) VALUE 0.       00358
025500     05  WS-DEF-GRPNO.                                            00359
025600         10  FILLER   PIC X(19) VALUE 'DEFAULTED GROUPS = '.      00360
025700         10  WS-DEF-GRPNO-CNT            PIC 9(09) VALUE 0.       00361
025800     05  WS-DEF-LOBNO.                                            00362
025900         10  FILLER   PIC X(19) VALUE 'DEFAULTED LOB #S = '.      00363
026000         10  WS-DEF-LOBNO-CNT            PIC 9(09) VALUE 0.       00364
026100     05  WS-DEF-MSBRK.                                            00365
026200         10  FILLER   PIC X(19) VALUE 'DEFAULTED MSBREAK= '.      00366
026300         10  WS-DEF-MSBRK-CNT            PIC 9(09) VALUE 0.       00367
026400         SKIP2                                                    00368
026500     05  WS-LOG-COUNTS.                                           00369
026600         10  FILLER   PIC X(19) VALUE '  -LOGICAL COUNTS- '.      00370
026700     05  WS-LOG-PDDATE.                                           00371
026800         10  FILLER   PIC X(19) VALUE 'LOGICAL PAIDDATE = '.      00372
026900         10  WS-LOG-ERR-PDDATE-CNT       PIC 9(09) VALUE 0.       00373
027000     05  WS-LOG-PAYDAY.                                           00374
027100         10  FILLER   PIC X(19) VALUE 'LOGICAL PAID DAY = '.      00375
027200         10  WS-LOG-ERR-PAYDAY-CNT       PIC 9(09) VALUE 0.       00376
027300     05  WS-LOG-INCDAY.                                           00377
027400         10  FILLER   PIC X(19) VALUE 'LOGICAL INC DAY  = '.      00378
027500         10  WS-LOG-ERR-INCDAY-CNT       PIC 9(09) VALUE 0.       00379
027600     05  WS-LOG-PAYMON.                                           00380
027700         10  FILLER   PIC X(19) VALUE 'LOGICAL PAY MONTH= '.      00381
027800         10  WS-LOG-ERR-PAYMON-CNT       PIC 9(09) VALUE 0.       00382
027900     05  WS-LOG-INCMON.                                           00383
028000         10  FILLER   PIC X(19) VALUE 'LOGICAL INCMONTH = '.      00384
028100         10  WS-LOG-ERR-INCMON-CNT       PIC 9(09) VALUE 0.       00385
028200                                                                  00386
028300     05  WS-DISPLY.                                               00387
028400         10  WS-DISPLAY-1                PIC X(58) VALUE ALL '*'. 00388
028500 01  WS-DISPLAY-CONSTANTS.                                        00389
028600         10  WS-VALCON-1                 PIC X(58) VALUE          00390
028700     'LINE OF BUSINESS IS:                                      '.00391
028600         10  WS-VALCON-1A                PIC X(58) VALUE          00392
028700     'PROGRAM IS CHANGING THE LINE OF BUSINESS TO:             '. 00393
028800         10  WS-VALCON-2                 PIC X(58) VALUE          00394
028900     'BLANK CLAIM NUMBER SET TO ALL ZEROS FOR SUBSCRIBER ID => '. 00395
029000         10  WS-VALCON-3                 PIC X(58) VALUE          00396
029100     'BLANK SUBSCRIBER ID SET TO ALL ZEROS FOR CLAIM #     =>  '. 00397
029200         10  WS-VALCON-4                 PIC X(58) VALUE          00398
029300     'INVALID PAID DAY SET TO 1ST DAY OF PAID MONTH            '. 00399
029400         10  WS-VALCON-5                 PIC X(58) VALUE          00400
029500     'INVALID INCURRED DAY SET TO PAYMENT DAY                  '. 00401
029600         10  WS-VALCON-6                 PIC X(58) VALUE          00402
029700     'SUBSTITUTED PAID YEAR FOR INVALID INC. YEAR              '. 00403
029800         10  WS-VALCON-7                 PIC X(58) VALUE          00404
029900     'SUBSTITUTED PAID MONTH FOR INVALID INC. MONTH            '. 00405
030000         10  WS-VALCON-8                 PIC X(58) VALUE          00406
030100     'A NON-NUMERIC GROUP WAS ACCEPTED ON EXTRACT - GROUP # ==>'. 00407
030200         10  WS-VALCON-8A                PIC X(58) VALUE          00408
030300     'INVALID LINE OF BUSINESS CODE - CRITICAL ERROR           '. 00409
030400         10  WS-VALCON-8B                PIC X(58) VALUE          00410
030500     'INVALID MED-SURG BREAKDOWN   - CRITICAL ERROR            '. 00411
030600         10  WS-VALCON-8C                PIC X(58) VALUE          00412
030700     'INVALID PAID DAY                                         '. 00413
030800         10  WS-VALCON-8D                PIC X(58) VALUE          00414
030900     'INVALID INCURRED DAY                                     '. 00415
031000         10  WS-VALCON-8E                PIC X(58) VALUE          00416
031100     'INVALID PAID MONTH                                       '. 00417
031200         10  WS-VALCON-8F                PIC X(58) VALUE          00418
031300     'INVALID INCURRED MONTH                                   '. 00419
031400         10  WS-VALCON-9                 PIC X(58) VALUE          00420
031500     'CLAIM INCURRED DATE IS LATER THAN PAID DATE              '. 00421
031600         10  WS-VALCON-10                PIC X(58) VALUE          00422
031700     'CLAIM INCURRED DATE SET TO CLAIM PAID DATE               '. 00423
031800         10  WS-ERROR-CONVERTING-ADJ     PIC X(58) VALUE          00424
031900     'COULD NOT CONVERT IED-DATE,          USING HEADER DATES  '. 00425
032000         10  WS-ERROR-CONVERTING-MODRUG  PIC X(58) VALUE          00426
032100     'COULD NOT CONVERT LAST-PROCESS-DATE, USING HEADER DATES  '. 00427
032200     EJECT                                                        00428
032300 01  WS-MESSAGES-TABLES-ETC.                                      00429
032400     05  ERROR-MESSAGE-TABLE.                                     00430
032500         10  WS-ERROR-MESSAGE-1          PIC X(58) VALUE          00431
032600     '** NO RECORDS WERE FOUND ON THE HOSPITAL FILE.        ***'. 00432
032700                                                                  00433
032800         10  WS-ERROR-MESSAGE-2          PIC X(58) VALUE          00434
032900     '** PAYMENT DATE ON MCS FILE IS NOT EQUAL TO DATE FILE ***'. 00435
033000                                                                  00436
033100                                                                  00437
033200         10  WS-ERROR-MESSAGE-3         PIC X(58) VALUE           00438
033300     '*PAYMENT DATE ON HOSPITAL FILE IS NOT EQUAL TO DATE FILE*'. 00439
033400                                                                  00440
033500         10  WS-ERROR-MESSAGE-4         PIC X(58) VALUE           00441
033600     '*** MCS HEADER DATE IS NOT EQUAL TO DATE FILE         ***'. 00442
033700                                                                  00443
033800         10  WS-ERROR-MESSAGE-5         PIC X(58) VALUE           00444
033900      '****TRAILER RECORD COUNTS ON THE MCS  FILE DO NOT MATCH '. 00445
034000                                                                  00446
034100         10  WS-ERROR-MESSAGE-6         PIC X(58) VALUE           00447
034200     'PROGRAM ACCUMULATED.                                 ****'. 00448
034300                                                                  00449
034400         10  WS-ERROR-MESSAGE-7         PIC X(58) VALUE           00450
034500     '* * * *                DATE FROM PRODUCTION DATE FILE  ='.  00451
034600                                                                  00452
034700         10  WS-ERROR-MESSAGE-8         PIC X(58) VALUE           00453
03480      '* * * PAYMENT DATE FROM COMBINED PAYMENT FILE HR501NOT ='.  00454
034900                                                                  00455
035000         10  WS-ERROR-MESSAGE-9          PIC X(58) VALUE          00456
035100     '** NO RECORDS WERE FOUND ON THE DENTAL   FILE.      ***'.   00457
035200                                                                  00458
035300         10  WS-ERROR-MESSAGE-10         PIC X(58) VALUE          00459
035400      '*PAYMENT DATE ON DENTAL FILE IS NOT EQUAL TO DATE FILE *'. 00460
035500                                                                  00461
035600         10  WS-ERROR-MESSAGE-11        PIC X(58) VALUE           00462
035700      '*PAYMENT DATE ON DENTAL FILE IS NOT EQUAL TO DATE FILE *'. 00463
035800                                                                  00464
035900         10  WS-ERROR-MESSAGE-12        PIC X(58) VALUE           00465
036000     '** DENTAL HEADER DATE IS NOT EQUAL TO DATE FILE       ***'. 00466
036100                                                                  00467
036200         10  WS-ERROR-MESSAGE-13        PIC X(58) VALUE           00468
036300     '***  TRAILER RECORD COUNTS ON DRUG   FILE DO NOT MATCH   '. 00469
036400                                                                  00470
036500         10  WS-ERROR-MESSAGE-14        PIC X(58) VALUE           00471
036600     'PROGRAM ACCUMULATED.                                ****'.  00472
036700                                                                  00473
036800         10  WS-ERROR-MESSAGE-15        PIC X(58) VALUE           00474
036900     '* * *                  DATE FROM PRODUCTION DATE FILE  ='.  00475
037000                                                                  00476
037100         10  WS-ERROR-MESSAGE-16        PIC X(58) VALUE           00477
037200     ' * * * PAYMENT DATE FROM DENTAL PAID CLAIM FILE DEDEDEDE='. 00478
037300                                                                  00479
037400         10  WS-ERROR-MESSAGE-17         PIC X(58) VALUE          00480
037500     '* NO RECORDS WERE FOUND ON THE DRUG     FILE.        ***'.  00481
037600                                                                  00482
037700         10  WS-ERROR-MESSAGE-18         PIC X(58) VALUE          00483
037800     '* PAYMENT DATE ON DRUG   FILE IS NOT EQUAL TO DATE FILE *'. 00484
037900                                                                  00485
038000                                                                  00486
038100         10  WS-ERROR-MESSAGE-19        PIC X(58) VALUE           00487
038200     '* PAYMENT DATE ON DRUG   FILE IS NOT EQUAL TO DATE FILE *'. 00488
038300                                                                  00489
038400         10  WS-ERROR-MESSAGE-20        PIC X(58) VALUE           00490
038500     '** DRUG   HEADER DATE IS NOT EQUAL TO DATE FILE       ***'. 00491
038600                                                                  00492
038700         10  WS-ERROR-MESSAGE-21        PIC X(58) VALUE           00493
038800     '***  TRAILER RECORD COUNTS ON DRUG   FILE DO NOT MATCH   '. 00494
038900                                                                  00495
039000         10  WS-ERROR-MESSAGE-22        PIC X(58) VALUE           00496
039100     'PROGRAM ACCUMULATED.                                 ****'. 00497
039200                                                                  00498
039300         10  WS-ERROR-MESSAGE-23        PIC X(58) VALUE           00499
039400     ' * * *                  DATE FROM PRODUCTION DATE FILE  ='. 00500
039500                                                                  00501
039600         10  WS-ERROR-MESSAGE-24        PIC X(58) VALUE           00502
039700     ' * * *   PAYMENT DATE FROM DRUG PAID CLAIM FILE DGDGDGDG='. 00503
039800                                                                  00504
039900         10  WS-ERROR-MESSAGE-25         PIC X(58) VALUE          00505
040000     '** NO RECORDS WERE FOUND ON THE MEDICAL  FILE.        ***'. 00506
040100                                                                  00507
040200         10  WS-ERROR-MESSAGE-26         PIC X(58) VALUE          00508
040300     '* NO RECORDS WERE FOUND ON THE DENTAL   FILE.      ***'.    00509
040400                                                                  00510
040500         10  WS-ERROR-MESSAGE-27         PIC X(58) VALUE          00511
040600     '* NO RECORDS WERE FOUND ON THE DRUG FILE.            ***'.  00512
040700                                                                  00513
034700         10  WS-ERROR-MESSAGE-28        PIC X(58) VALUE           00514
034800     ' * * * PAYMENT DATE FROM COMBINED PAYMENT FILE HR505CRT ='. 00515
034900                                                                  00516
034700         10  WS-ERROR-MESSAGE-29        PIC X(58) VALUE           00517
034800     ' * * * PAYMENT DATE FROM REJECT   PAYMENT FILE NR3080   ='. 00518
034900                                                                  00519
040800     05  ERROR-MESSAGE-TBL REDEFINES ERROR-MESSAGE-TABLE.         00520
040900         10  WS-ERR-MESSAGE       OCCURS 29 TIMES                 00521
041000                                        INDEXED BY MSG-INDEX.     00522
041100             15  WS-ERROR-MSG-TBL       PIC X(58).                00523
041200     EJECT                                                        00524
041300     05  WS-BALANCING-MESSAGES.                                   00525
041400         10  WS-BALANCE-MESSAGE-1       PIC X(50) VALUE           00526
041500     '***  INSTITUTIONAL INPUT TRAILER WAS BALANCED  ***'.        00527
041600         10  WS-BALANCE-MESSAGE-2       PIC X(50) VALUE           00528
041700     '***  MEDICAL INPUT FILE TRAILER WAS BALANCED   ***'.        00529
041800         10  WS-BALANCE-MESSAGE-3       PIC X(50) VALUE           00530
041900     '***   DENTAL INPUT FILE TRAILER WAS BALANCED   ***'.        00531
042000         10  WS-BALANCE-MESSAGE-4       PIC X(50) VALUE           00532
042100     '***     DRUG INPUT FILE TRAILER WAS BALANCED   ***'.        00533
042200     SKIP2                                                        00534
042300     05  WS-BALANCED-MESSAGE-TBL REDEFINES WS-BALANCING-MESSAGES. 00535
042400         10  WS-BAL-MESSAGE       OCCURS 4 TIMES                  00536
042500                                        INDEXED BY BAL-INDEX.     00537
042600             15 WS-BALANCED-FILE-MESSAGE        PIC X(50).        00538
042700     EJECT                                                        00539
042800     05  ABEND-CODE-TABLE.                                        00540
042900         10  FILLER-1                   PIC S9(03) VALUE +100.    00541
043000         10  FILLER-2                   PIC S9(03) VALUE +101.    00542
043100         10  FILLER-3                   PIC S9(03) VALUE +102.    00543
043200         10  FILLER-4                   PIC S9(03) VALUE +103.    00544
043300         10  FILLER-5                   PIC S9(03) VALUE +104.    00545
043400         10  FILLER-6                   PIC S9(03) VALUE +105.    00546
043500         10  FILLER-7                   PIC S9(03) VALUE +106.    00547
043600         10  FILLER-8                   PIC S9(03) VALUE +107.    00548
043700         10  FILLER-9                   PIC S9(03) VALUE +108.    00549
043800         10  FILLER-10                  PIC S9(03) VALUE +109.    00550
043900         10  FILLER-11                  PIC S9(03) VALUE +110.    00551
044000         10  FILLER-12                  PIC S9(03) VALUE +111.    00552
044100         10  FILLER-13                  PIC S9(03) VALUE +112.    00553
044200         10  FILLER-14                  PIC S9(03) VALUE +113.    00554
044300         10  FILLER-15                  PIC S9(03) VALUE +114.    00555
044400         10  FILLER-16                  PIC S9(03) VALUE +115.    00556
044500         10  FILLER-17                  PIC S9(03) VALUE +200.    00557
044600         10  FILLER-18                  PIC S9(03) VALUE +201.    00558
044700     05  ABEND-CODE-TBL REDEFINES ABEND-CODE-TABLE.               00559
044800         10  WS-ABEND-CODE-TBL    OCCURS 18 TIMES                 00560
044900                                        INDEXED BY ABEND-INDEX.   00561
045000             15  WS-ABEND-CODE           PIC S9(03).              00562
045100     EJECT                                                        00563
045200     05  WS-ABEND-MESSAGES-TABLE.                                 00564
045300         10  WS-BAD-TRLR-1          PIC X(58) VALUE               00565
045400     '** TRAILER RECORD COUNTS ON THE HOSPITAL FILE DO NOT     '. 00566
045500                                                                  00567
045600         10  WS-BAD-TRLR-2          PIC X(58) VALUE               00568
045700     ' MATCH PROGRAM ACCUMULATED                           ****'. 00569
045800                                                                  00570
045900         10  WS-BAD-TRLR-3          PIC X(58) VALUE               00571
046000     '** TOTAL TRAILER RECORD  COUNTS ON THE HOSPITAL FILE   **'. 00572
046100                                                                  00573
046200         10  WS-BAD-TRLR-4          PIC X(58) VALUE               00574
046300     '*TOTAL RECORDS ACCUMULATED BY PROGRAM FOR THE HOSP  FILE*'. 00575
046400                                                                  00576
046500         10  WS-BAD-TRLR-5          PIC X(58) VALUE               00577
046600     '* TOTAL AHS LIABILITY AMOUNTS ON THE TRAILER RECORD   ***'. 00578
046700                                                                  00579
046800         10  WS-BAD-TRLR-6          PIC X(58) VALUE               00580
046900     '* TOTAL AHS LIABILITY AMOUNTS ACCUMULATED BY THE PROGRAM '. 00581
047000                                                                  00582
047100         10  WS-BAD-TRLR-7          PIC X(58) VALUE               00583
047200     '**   TOTAL HOSPITAL CLAIMS COUNTED BY THE PROGRAM      **'. 00584
047300                                                                  00585
047400         10  WS-BAD-TRLR-8          PIC X(58) VALUE               00586
047500     'TOTAL HOSPITAL CLAIMS SELECTED AND BYPASS BY THE PROGRAM '. 00587
047600                                                                  00588
047700         10  WS-BAD-TRLR-9          PIC X(58) VALUE               00589
047800     '*   TOTAL DAYS ACCUMULATED BY THE PROGRAM             ***'. 00590
047900                                                                  00591
048000         10  WS-BAD-TRLR-10         PIC X(58) VALUE               00592
048100     '* TOTAL DAYS SELECTED AND BYPASS BY THE PROGRAM     *****'. 00593
048200                                                                  00594
048300         10  WS-BAD-TRLR-11         PIC X(58) VALUE               00595
048400     '** TRAILER RECORD COUNTS ON THE DENTAL   FILE DO NOT     '. 00596
048500                                                                  00597
048600         10  WS-BAD-TRLR-12         PIC X(58) VALUE               00598
048700     '    MATCH PROGRAM ACCUMULATED                        ****'. 00599
048800                                                                  00600
048900         10  WS-BAD-TRLR-13         PIC X(58) VALUE               00601
049000     '** TOTAL TRAILER RECORD  COUNTS ON THE DENTAL   FILE   **'. 00602
049100                                                                  00603
049200         10  WS-BAD-TRLR-14         PIC X(58) VALUE               00604
049300     '*TOTAL RECORDS ACCUMULATED BY PROGRAM FOR THE DENTAL FILE'. 00605
049400                                                                  00606
049500         10  WS-BAD-TRLR-15         PIC X(58) VALUE               00607
049600     '* TOTAL DOLLAR  PAID  AMOUNTS ON THE TRAILER RECORD   ***'. 00608
049700                                                                  00609
049800         10  WS-BAD-TRLR-16         PIC X(58) VALUE               00610
049900     '*TOTAL DOLLAR PAID   AMOUNTS ACCUMULATED BY THE PROGRAM *'. 00611
050000                                                                  00612
050100         10  WS-BAD-TRLR-17         PIC X(58) VALUE               00613
050200     '**      TOTAL DENTAL   CLAIMS COUNTED BY THE PROGRAM   **'. 00614
050300                                                                  00615
050400         10  WS-BAD-TRLR-18         PIC X(58) VALUE               00616
050500     '*TOTAL DENTAL CLAIMS SELECTED AND BYPASS BY THE PROGRAM *'. 00617
050600                                                                  00618
050700         10  WS-BAD-TRLR-19         PIC X(58) VALUE               00619
050800     '***                                                   ***'. 00620
050900                                                                  00621
051000         10  WS-BAD-TRLR-20         PIC X(58) VALUE               00622
051100     '***                                                  ****'. 00623
051200         10  WS-BAD-TRLR-21         PIC X(58) VALUE               00624
051300     '** TRAILER RECORD COUNTS ON THE DRUG  FILE DO NOT        '. 00625
051400                                                                  00626
051500         10  WS-BAD-TRLR-22         PIC X(58) VALUE               00627
051600     ' MATCH PROGRAM ACCUMULATED                           ****'. 00628
051700                                                                  00629
051800         10  WS-BAD-TRLR-23         PIC X(58) VALUE               00630
051900     '*TOTAL TRAILER RECORD  COUNTS ON THE DRUG     FILE     **'. 00631
052000                                                                  00632
052100         10  WS-BAD-TRLR-24         PIC X(58) VALUE               00633
052200     '*TOTAL RECORDS ACCUMULATED BY PROGRAM FOR THE DRUG  FILE*'. 00634
052300                                                                  00635
052400         10  WS-BAD-TRLR-25         PIC X(58) VALUE               00636
052500     '* TOTAL DOLLAR  PAID  AMOUNTS ON THE TRAILER RECORD   ***'. 00637
052600                                                                  00638
052700         10  WS-BAD-TRLR-26         PIC X(58) VALUE               00639
052800     '** TOTAL DOLLAR PAID  AMOUNTS ACCUMULATED BY THE PROGRAM*'. 00640
052900                                                                  00641
053000         10  WS-BAD-TRLR-27         PIC X(58) VALUE               00642
053100     '**   TOTAL DRUG     CLAIMS COUNTED BY THE PROGRAM      **'. 00643
053200                                                                  00644
053300         10  WS-BAD-TRLR-28         PIC X(58) VALUE               00645
053400     '*TOTAL DRUG  CLAIMS SELECTED AND BYPASS BY THE PROGRAM  *'. 00646
053500                                                                  00647
053600         10  WS-BAD-TRLR-29         PIC X(58) VALUE               00648
053700     '***                                                   ***'. 00649
053800                                                                  00650
053900         10  WS-BAD-TRLR-30         PIC X(58) VALUE               00651
054000     '***                                                  ****'. 00652
054100                                                                  00653
054200     05  WS-ABEND-MESSAGES-TBL REDEFINES WS-ABEND-MESSAGES-TABLE. 00654
054300         10  WS-ABEND-MSG-TABLE OCCURS 30 TIMES                   00655
054400                                        INDEXED BY MSG-INDEX.     00656
054500             15  WS-ABEND-MSG-TBL        PIC X(58).               00657
054600     EJECT                                                        00658
054700     05  WS-MCS-ABEND-MSG-TABLE.                                  00659
054800         10  WS-MCS-ERROR-1         PIC X(58) VALUE               00660
054900     '******     TRAILER RECORD COUNTS ON THE MCS FILE DO NOT  '. 00661
055000                                                                  00662
055100         10  WS-MCS-ERROR-2         PIC X(58) VALUE               00663
055200     ' MATCH PROGRAM ACCUMULATED                           ****'. 00664
055300                                                                  00665
055400         10  WS-MCS-ERROR-3         PIC X(58) VALUE               00666
055500     'TOTAL TRAILER RECORD  COUNTS ON THE  MCS  FILE       ****'. 00667
055600                                                                  00668
055700         10  WS-MCS-ERROR-4         PIC X(58) VALUE               00669
055800     'TOTAL BASIC RECORDS ACCUMULATED FROM THE MCS FILE     ***'. 00670
055900                                                                  00671
056000         10  WS-MCS-ERROR-5         PIC X(58) VALUE               00672
056100     'TOTAL MAJ/MED  RECORDS ACCUMULATED FROM THE MCS FILE  ***'. 00673
056200                                                                  00674
056300         10  WS-MCS-ERROR-6         PIC X(58) VALUE               00675
056400     'TOTAL BASIC LIABILITY AMOUNT ACCUMULATED BY PROGRAM    *'.  00676
056500                                                                  00677
056600         10  WS-MCS-ERROR-7         PIC X(58) VALUE               00678
056700     '* TOTAL MAJ/MED LIABILITY AMOUNT ACCUMULATED BY PROGRAM  '. 00679
056800                                                                  00680
056900         10  WS-MCS-ERROR-8         PIC X(58) VALUE               00681
057000     '** TOTAL UMS LIABILITY AMOUNTS SELECTED FOR BASIC-MED  **'. 00682
057100                                                                  00683
057200         10  WS-MCS-ERROR-9         PIC X(58) VALUE               00684
057300     '** TOTAL UMS LIABILITY AMOUNTS SELECTED FOR MAJOR-MED  **'. 00685
057400                                                                  00686
057500         10  WS-MCS-ERROR-10        PIC X(58) VALUE               00687
057600     '* TOTAL BASIC UMS LIABILITY AMOUNTS BYPASS BY THE PROGRAM'. 00688
057700                                                                  00689
057800         10  WS-MCS-ERROR-11        PIC X(58) VALUE               00690
057900     'TOTAL MAJ MED UMS LIABILITY AMOUNTS BYPASS BY THE PROGRAM'. 00691
058000                                                                  00692
058100         10  WS-MCS-ERROR-12        PIC X(58) VALUE               00693
058200     'TOTAL BASIC M-DRUG LIAB.  AMOUNTS ROUTED BY THE PROGRAM *'. 00694
058300                                                                  00695
058400         10  WS-MCS-ERROR-13        PIC X(58) VALUE               00696
058500     'TOTAL MAJ MED M-DRUG LIAB. AMOUNTS ROUTED BY THE PROGRAM '. 00697
058600                                                                  00698
058700     05  WS-MCS-ABEND-TABLE REDEFINES WS-MCS-ABEND-MSG-TABLE.     00699
058800         10  WS-MCS-ERROR-TABLE OCCURS 13 TIMES                   00700
058900                                        INDEXED BY MSG-INDEX.     00701
059000             15  WS-MCS-ABEND-TBL        PIC X(58).               00702
059100     EJECT                                                        00703
059200     05  WS-DENTAL-ABEND-MSG-TABLE.                               00704
059300         10  WS-DEN-ERROR-1         PIC X(58) VALUE               00705
059400     '******  TRAILER RECORD COUNTS ON THE DENTAL FILE DO NOT  '. 00706
059500                                                                  00707
059600         10  WS-DEN-ERROR-2         PIC X(58) VALUE               00708
059700     ' MATCH PROGRAM ACCUMULATED                           ****'. 00709
059800                                                                  00710
059900         10  WS-DEN-ERROR-3         PIC X(58) VALUE               00711
060000     'TOTAL TRAILER RECORD  COUNTS ON THE DENTAL FILE      ****'. 00712
060100                                                                  00713
060200         10  WS-DEN-ERROR-4         PIC X(58) VALUE               00714
060300     'TOTAL RECORDS ACCUMULATED BY PROGRAM FOR DENTAL  FILE ***'. 00715
060400                                                                  00716
060500         10  WS-DEN-ERROR-5         PIC X(58) VALUE               00717
060600     'TOTAL TRAILER LIABILITY AMOUNTS ON THE DENTAL FILE      *'. 00718
060700                                                                  00719
060800         10  WS-DEN-ERROR-6         PIC X(58) VALUE               00720
060900     '** TOTAL DENTAL LIABILITY AMOUNTS SELECTED BY PROGRAM  **'. 00721
061000                                                                  00722
061100     05  WS-DEN-ABEND-TABLE REDEFINES WS-DENTAL-ABEND-MSG-TABLE.  00723
061200         10  WS-DEN-ERROR-TABLE OCCURS 6 TIMES                    00724
061300                                        INDEXED BY MSG-INDEX.     00725
061400             15  WS-DEN-ABEND-TBL        PIC X(58).               00726
061500     EJECT                                                        00727
061600     05  WS-DRUG-ABEND-MSG-TABLE.                                 00728
061700         10  WS-DRUG-ERROR-1        PIC X(58) VALUE               00729
061800     '******  TRAILER RECORD COUNTS ON THE DRUG   FILE DO NOT  '. 00730
061900                                                                  00731
062000         10  WS-DRUG-ERROR-2        PIC X(58) VALUE               00732
062100     ' MATCH PROGRAM ACCUMULATED                           ****'. 00733
062200                                                                  00734
062300         10  WS-DRUG-ERROR-3        PIC X(58) VALUE               00735
062400     'TOTAL TRAILER RECORD  COUNTS ON THE DRUG   FILE      ****'. 00736
062500                                                                  00737
062600         10  WS-DRUG-ERROR-4        PIC X(58) VALUE               00738
062700     'TOTAL RECORDS ACCUMULATED BY PROGRAM FOR DRUG    FILE ***'. 00739
062800                                                                  00740
062900         10  WS-DRUG-ERROR-5        PIC X(58) VALUE               00741
063000     'TOTAL TRAILER LIABILITY AMOUNTS ON THE DRUG FILE        *'. 00742
063100                                                                  00743
063200         10  WS-DRUG-ERROR-6        PIC X(58) VALUE               00744
063300     '** TOTAL DRUG   LIABILITY AMOUNTS SELECTED BY PROGRAM  **'. 00745
063400                                                                  00746
063500     05  WS-DRUG-ABEND-TABLE REDEFINES WS-DRUG-ABEND-MSG-TABLE.   00747
063600         10  WS-DRUG-ERROR-TABLE OCCURS 6 TIMES                   00748
063700                                        INDEXED BY MSG-INDEX.     00749
063800             15  WS-DRUG-ABEND-TBL       PIC X(58).               00750
063900     EJECT                                                        00751
064000     05  WS-EDITED-DISPLY.                                        00752
064100         10  WS-EDITED-DISPLY-AMTS      PIC ZZZZZZZZZZZZ9.99-.    00753
064200         10  WS-EDIT-DISPLY-AMTS        PIC S9(13)V99 VALUE +0.   00754
064300         10  WS-EDITED-DISPLY-CNTS      PIC ZZZZZZZZZZZZ9-.       00755
064400         10  WS-PAID-DAYS               PIC S9(3)     VALUE +0.   00756
064500                                                                  00757
064600     05  WS-FIXED-VALUES.                                         00758
018500         10  GME-CURRENT-DAY         PIC 9(2)  VALUE 0.           00759
172000         10  WS-GME-DAY-COUNTER      PIC 9(02) VALUE 0.           00760
064700         10  WS-PLUS-ONE             PIC S9(01) VALUE +1.         00761
064800         10  WS-01                   PIC X(02)  VALUE '01'.       00762
064800         10  WS-00                   PIC X(02)  VALUE '00'.       00763
064800         10  WS-DEST-START-DATE      PIC X(08)  VALUE '20020531'. 00763
064900         10  WS-1                    PIC X(01)  VALUE '1'.        00764
064900         10  WS-CRT                  PIC X(01)  VALUE 'C'.        00765
064900         10  WS-NOT                  PIC X(01)  VALUE 'N'.        00766
064900         10  WS-REJ                  PIC X(01)  VALUE 'R'.        00767
065000         10  WS-12                   PIC X(02)  VALUE '12'.       00768
065100         10  WS-31                   PIC X(02)  VALUE '31'.       00769
065200         10  WS-PLUS-FOUR            PIC S9(01) VALUE +4.         00770
065300         10  WS-PLUS-TEN             PIC S9(02) VALUE +10.        00771
065400         10  WS-PLUS-SIXTEEN         PIC S9(02) VALUE +16.        00772
065500         10  WS-PLUS-EIGHTEEN        PIC S9(02) VALUE +18.        00773
065600         10  WS-PLUS-ZERO            PIC S9(01) VALUE +0.         00774
065700         10  WS-ZERO                 PIC 9(01)  VALUE 0.          00775
065800         10  WS-ALL-ZEROS            PIC 9(6)   VALUE 0.          00776
065900         10  WS-ONE                  PIC 9(01)  VALUE 1.          00777
065900         10  WS-19                   PIC X(02)  VALUE '19'.       00777
065900         10  WS-20                   PIC X(02)  VALUE '20'.       00777
066000         10  WS-TWO                  PIC 9(01)  VALUE 2.          00778
066100         10  WS-THREE                PIC 9(01)  VALUE 3.          00779
066200         10  WS-FOUR                 PIC 9(01)  VALUE 4.          00780
066200         10  WS-FIVE                 PIC 9(01)  VALUE 5.          00781
066200         10  WS-SIX                  PIC 9(01)  VALUE 6.          00782
066300         10  WS-HEADER               PIC X(06)  VALUE 'HEADER'.   00783
066400         10  WS-DETAIL               PIC X(06)  VALUE 'DETAIL'.   00784
066500         10  WS-TRAILER              PIC X(07)  VALUE 'TRAILER'.  00785
066600         10  WS-LITERAL-A            PIC X(1)   VALUE 'A'.        00786
066700         10  WS-90                   PIC X(2)   VALUE '90'.       00787
066800         10  WS-LITERAL-01           PIC X(2)   VALUE '01'.       00788
066900         10  WS-LITERAL-1            PIC X(1)   VALUE '1'.        00789
067000         10  WS-MAIL-ORDER-DRUG      PIC X(1)   VALUE '4'.        00790
067000         10  WS-DRUG-PPO             PIC X(2)   VALUE 'DO'.       00791
067000         10  WS-CS90                 PIC X(1)   VALUE 'S'.        00792
065900         10  WS-G                    PIC X(01)  VALUE 'G'.        00793
067000         10  WS-W                    PIC X(1)   VALUE 'W'.        00794
067000         10  WS-R                    PIC X(1)   VALUE 'R'.        00795
067000         10  WS-DRUG-CAREMARK        PIC X(2)   VALUE 'DZ'.       00796
067100         10  WS-NOT-APPLICABLE       PIC X(14)  VALUE             00797
067200                                       '           N/A'.          00798
067300         10  WS-LITERAL-QUESTION-M      PIC X(1) VALUE '?'.       00799
067400         10  WS-LOB-ADJ-CODE-INDICATOR  PIC X(1) VALUE SPACE.     00800
067500             88  CLAIM-IS-QUESTIONABLE  VALUE '?'.                00801
067600     SKIP2                                                        00802
067700     05  WS-COMPUTE-FIELDS.                                       00803
067800         10  WS-COMPUTE-LOB-LIB   PIC S9(13)V99  COMP-3 VALUE +0. 00804
067900         10  WS-REJECT-TOTALS     PIC S9(13)V99  COMP-3 VALUE +0. 00805
067900         10  WS-OUT-LIABILITY     PIC S9(13)V99  COMP-3 VALUE +0. 00806
068000         10  WS-COMPUTE-RECORDS   PIC S9(11)     COMP-3 VALUE +0. 00807
068100         10  WS-COMPUTE-SERVICES  PIC S9(11)     COMP-3 VALUE +0. 00808
068200         10  WS-COMPUTE-CLAIMS    PIC S9(11)     COMP-3 VALUE +0. 00809
068300         10  WS-COMPUTE-DAYS      PIC S9(11)     COMP-3 VALUE +0. 00810
068400         10  WS-POSITIVE-ONE      PIC S9         COMP-3 VALUE +1. 00811
068500         10  WS-FOURTEEN-ZEROS    PIC 9(14)      VALUE 0.         00812
192500         10  WS-ICHIS-SUPP-RECORDS  PIC S9(11)    COMP-3 VALUE +0.00813
172000         10  WS-ICHIS-SUPP-TOTALS   PIC S9(13)V99 COMP-3 VALUE +0.00814
               10  WS-GME-RECORDS-IN      PIC S9(09)    COMP-3 VALUE +0.00815
172000         10  WS-GME-LIABILITY-IN    PIC S9(11)V99 COMP-3 VALUE +0.00816
               10  WS-GME-RECORDS-OUT     PIC S9(09)    COMP-3 VALUE +0.00817
172000         10  WS-GME-LIABILITY-OUT   PIC S9(11)V99 COMP-3 VALUE +0.00818
068600                                                                  00819
068700     05  WS-LOB-HEADING.                                          00820
068800         10  WS-LIT-ICHIS         PIC X(2)  VALUE 'HR'.           00821
068900         10  WS-LIT-DENTAL        PIC X(2)  VALUE 'DE'.           00822
069000         10  WS-LIT-DRUG          PIC X(2)  VALUE 'DG'.           00823
069100         10  WS-LIT-MANREP        PIC X(2)  VALUE 'MS'.           00824
069200         10  WS-LIT-ICHIS-SYS     PIC X(6)  VALUE 'ICHIS '.       00825
069300         10  WS-LIT-DENTAL-SYS    PIC X(6)  VALUE 'DENTAL'.       00826
069400         10  WS-LIT-DRUG-SYS      PIC X(6)  VALUE 'DRUG  '.       00827
069500         10  WS-LIT-MANREP-SYS    PIC X(6)  VALUE 'MANREP'.       00828
069600         10  WS-TOTAL             PIC X(13) VALUE 'TOTAL        '.00829
069700         10  WS-TOTAL-INPUT       PIC X(13) VALUE 'TOTAL INPUT  '.00830
069800         10  WS-INPUT             PIC X(13) VALUE 'INPUT        '.00831
069900         10  WS-SELECTED          PIC X(13) VALUE 'SELECTED     '.00832
069900         10  WS-SUPPLEMENT        PIC X(13) VALUE 'SUPPLEMENTALS'.00833
069900         10  WS-GME               PIC X(13) VALUE 'GME          '.00834
070000         10  WS-GENERATED         PIC X(13) VALUE 'GENERATED    '.00835
070100         10  WS-BYPASS-LIT        PIC X(13) VALUE 'BYPASS       '.00836
070100         10  WS-INCLUDE-LIT       PIC X(13) VALUE 'INCLUDED     '.00837
070200         10  WS-M-DRUG-LIT        PIC X(13) VALUE 'MAIL-ORD DRUG'.00838
070200         10  WS-HOSP-REJ-LIT      PIC X(13) VALUE 'HOSPITAL REJ '.00839
070300         10  WS-REJECT-LIT        PIC X(13) VALUE 'REJECTS      '.00840
070400         10  WS-HOSPITAL          PIC X(13) VALUE 'HOSPITAL FILE'.00841
070500         10  WS-DENTAL            PIC X(13) VALUE 'DENTAL FILE  '.00842
070600         10  WS-DRUG              PIC X(13) VALUE 'DRUG FILE    '.00843
070700         10  WS-PROFESSIONAL      PIC X(13) VALUE 'MEDICAL FILE '.00844
070800         10  WS-BASIC-MED         PIC X(13) VALUE 'BASIC MEDICAL'.00845
070900         10  WS-MAJ-MED           PIC X(13) VALUE 'MAJOR MEDICAL'.00846
071000         10  WS-SUB-TOTAL-A       PIC X(13) VALUE 'PROCESS TOTAL'.00847
071100         10  WS-SUB-TOTAL-B       PIC X(13) VALUE 'PROCESS TOTAL'.00848
071200         10  WS-SUB-TOTAL-C       PIC X(13) VALUE 'PROCESS TOTAL'.00849
071300         10  WS-SUB-TOTAL-D       PIC X(13) VALUE 'PROCESS TOTAL'.00850
071400         10  WS-GRAND-TOTAL       PIC X(13) VALUE 'GRAND TOTAL  '.00851
071500         10  WS-A-B-C-D-TOT       PIC X(13) VALUE '   OUTPUT    '.00852
071600         10  WS-SEPARATOR         PIC X(133)                      00853
071700                                            VALUE ALL '*'.        00854
071800                                                                  00855
071900     05  WS-OUT-PUT-SECTION.                                      00856
072000         10  FILLER               PIC X(50) VALUE                 00857
072100        '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'.     00858
072200         10  FILLER               PIC X(50) VALUE                 00859
072300        '*-*-*     O U T  P U T    *-*-*-*-*-*-*-*-*-*-*-*-'.     00860
072400         10  FILLER               PIC X(33) VALUE                 00861
072500        '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'.                      00862
072600                                                                  00863
072700     05  TABLE-OF-MONTHS.                                         00864
072800         10 FILLER                     PIC X(3)    VALUE 'JAN'.   00865
072900         10 FILLER                     PIC X(3)    VALUE 'FEB'.   00866
073000         10 FILLER                     PIC X(3)    VALUE 'MAR'.   00867
073100         10 FILLER                     PIC X(3)    VALUE 'APR'.   00868
073200         10 FILLER                     PIC X(3)    VALUE 'MAY'.   00869
073300         10 FILLER                     PIC X(3)    VALUE 'JUN'.   00870
073400         10 FILLER                     PIC X(3)    VALUE 'JUL'.   00871
073500         10 FILLER                     PIC X(3)    VALUE 'AUG'.   00872
073600         10 FILLER                     PIC X(3)    VALUE 'SEP'.   00873
073700         10 FILLER                     PIC X(3)    VALUE 'OCT'.   00874
073800         10 FILLER                     PIC X(3)    VALUE 'NOV'.   00875
073900         10 FILLER                     PIC X(3)    VALUE 'DEC'.   00876
074000     05  MONTH-TABLE REDEFINES TABLE-OF-MONTHS.                   00877
074100         10  TBL-MONTH-NAME OCCURS 12 TIMES                       00878
074200                                       PIC X(03).                 00879
074300                                                                  00880
074400     05  WS-CURR-MNTH.                                            00881
074500         10  WS-MONTH                  PIC 9(02).                 00882
074600                                                                  00883
074700     EJECT                                                        00884
074800 01  CNTRL-RPT-HEADING-1.                                         00885
074900     05  FILLER                        PIC X     VALUE SPACES.    00886
075000     05  FILLER                        PIC X(20) VALUE            00887
075100         'REPORT-ID: FN355CTL-'.                                  00888
075200     05  HEADING-1-FEED-SYS-ACC-CODE   PIC X(2)  VALUE SPACES.    00889
075300     05  FILLER                        PIC X(19) VALUE SPACES.    00890
075400     05  FILLER                        PIC X(42) VALUE            00891
075500         'E M P I R E  B L U E  C R O S S  B L U E  '.            00892
075600     05  FILLER                        PIC X(11) VALUE            00893
075700         'S H I E L D'.                                           00894
075800     05  FILLER                        PIC X(38) VALUE SPACES.    00895
075900                                                                  00896
076000 01  CNTRL-RPT-HEADING-2.                                         00897
076100     05  FILLER                        PIC X      VALUE SPACES.   00898
076200     05  FILLER                        PIC X(19)  VALUE           00899
076300         'PROGRAM-ID FTESTEM0'.                                   00900
076400     05  FILLER                        PIC X(27)  VALUE SPACES.   00901
076500     05  FILLER                        PIC X(42)  VALUE           00902
076600         'COMPREHENSIVE PAID CLAIMS REPORTING SYSTEM'.            00903
076700     05  FILLER                        PIC X(44)  VALUE SPACES.   00904
076800                                                                  00905
076900 01  CNTRL-RPT-HEADING-3.                                         00906
077000     05  FILLER                        PIC X      VALUE SPACES.   00907
077100     05  FILLER                        PIC X(16)  VALUE           00908
077200         'PRODUCTION OF:  '.                                      00909
077300     05  H3-CURR-MONTH                 PIC X(3)   VALUE SPACES.   00910
077400     05  FILLER                        PIC X      VALUE SPACES.   00911
077500     05  FILLER                        PIC X(2)   VALUE '19'.     00912
077600     05  H3-YEAR                       PIC X(02)  VALUE SPACES.   00913
077700     05  FILLER                        PIC X(21)  VALUE SPACES.   00914
077800     05  FILLER                        PIC X(44)  VALUE           00915
077900         ' MONTHLY PAID CLAIMS EXTRACT CONTROL REPORT '.          00916
078000     05  FILLER                        PIC X(10)  VALUE SPACES.   00917
078100     05  FILLER                        PIC X(16)  VALUE           00918
078200         'FEEDING SYSTEM: '.                                      00919
078300     05  FILLER                        PIC X(2)  VALUE SPACES.    00920
078400     05  HEADING-3-FEEDING-SYS-NAME    PIC X(6)  VALUE SPACES.    00921
078500     05  FILLER                        PIC X(9)  VALUE SPACES.    00922
078600                                                                  00923
078700 01  CNTRL-RPT-HEADING-4.                                         00924
078800     05  FILLER                        PIC X(04)  VALUE SPACES.   00925
078900     05  FILLER                        PIC X(07)  VALUE 'LINE OF'.00926
079000     05  FILLER                        PIC X(21)  VALUE SPACES.   00927
079100     05  FILLER                        PIC X(05)  VALUE 'TOTAL'.  00928
079200     05  FILLER                        PIC X(18)  VALUE SPACES.   00929
079300     05  FILLER                        PIC X(05)  VALUE 'TOTAL'.  00930
079400     05  FILLER                        PIC X(20)  VALUE SPACES.   00931
079500     05  FILLER                        PIC X(05)  VALUE 'TOTAL'.  00932
079600     05  FILLER                        PIC X(18)  VALUE SPACES.   00933
079700     05  FILLER                        PIC X(05)  VALUE 'TOTAL'.  00934
079800     05  FILLER                        PIC X(14)  VALUE SPACES.   00935
079900     05  FILLER                        PIC X(05)  VALUE 'TOTAL'.  00936
080000     05  FILLER                        PIC X(06)  VALUE SPACES.   00937
080100                                                                  00938
080200 01  CNTRL-RPT-HEADING-5.                                         00939
080300     05  FILLER                        PIC X(03)  VALUE SPACES.   00940
080400     05  FILLER                        PIC X(08)  VALUE           00941
080500         'BUSINESS'.                                              00942
080600     05  FILLER                        PIC X(20)  VALUE SPACES.   00943
080700     05  FILLER                        PIC X(07)  VALUE           00944
080800         'RECORDS'.                                               00945
080900     05  FILLER                        PIC X(16)  VALUE SPACES.   00946
081000     05  FILLER                        PIC X(09)  VALUE           00947
081100         'LIABILITY'.                                             00948
081200     05  FILLER                        PIC X(16)  VALUE SPACES.   00949
081300     05  FILLER                        PIC X(08)  VALUE           00950
081400         'SERVICES'.                                              00951
081500     05  FILLER                        PIC X(15)  VALUE SPACES.   00952
081600     05  FILLER                        PIC X(06)  VALUE           00953
081700         'CLAIMS'.                                                00954
081800     05  FILLER                        PIC X(11)  VALUE SPACES.   00955
081900     05  FILLER                        PIC X(10)  VALUE           00956
082000         'DAYS/VISIT'.                                            00957
082100     05  FILLER                        PIC X(04)  VALUE SPACES.   00958
082200                                                                  00959
082300 01  CNTRL-RPT-HEADING-6.                                         00960
082400     05  FILLER                        PIC X(03)  VALUE SPACES.   00961
082500     05  FILLER                        PIC X(08)  VALUE           00962
082600         '--------'.                                              00963
082700     05  FILLER                        PIC X(20)  VALUE SPACES.   00964
082800     05  FILLER                        PIC X(07)  VALUE           00965
082900         '-------'.                                               00966
083000     05  FILLER                        PIC X(16)  VALUE SPACES.   00967
083100     05  FILLER                        PIC X(09)  VALUE           00968
083200         '---------'.                                             00969
083300     05  FILLER                        PIC X(16)  VALUE SPACES.   00970
083400     05  FILLER                        PIC X(08)  VALUE           00971
083500         '--------'.                                              00972
083600     05  FILLER                        PIC X(15)  VALUE SPACES.   00973
083700     05  FILLER                        PIC X(06)  VALUE           00974
083800         '------'.                                                00975
083900     05  FILLER                        PIC X(11)  VALUE SPACES.   00976
084000     05  FILLER                        PIC X(10)  VALUE           00977
084100         '----------'.                                            00978
084200     05  FILLER                        PIC X(04)  VALUE  SPACES.  00979
084300                                                                  00980
084400 01  WS-CONTROL-REPORT.                                           00981
084500     05  FILLER                        PIC X(02)   VALUE SPACES.  00982
084600     05  WS-PRT-DETAIL-LOB             PIC X(13)   VALUE SPACES.  00983
084700     05  FILLER                        PIC X(09)   VALUE SPACES.  00984
084800     05  WS-PRT-TOTAL-RECORDS          PIC ZZ,ZZZ,ZZZ,ZZ9-.       00985
084900     05  FILLER                        PIC X(04)   VALUE SPACES.  00986
085000     05  WS-PRT-LOB-LIAB               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-. 00987
085100     05  FILLER                        PIC X(09)   VALUE SPACES.  00988
085200     05  WS-PRT-SERVICES               PIC ZZ,ZZZ,ZZZ,ZZ9-.       00989
085300     05  WS-PRINT-SERVICES REDEFINES WS-PRT-SERVICES              00990
085400                                       PIC X(15).                 00991
085500     05  FILLER                        PIC X(06)   VALUE SPACES.  00992
085600     05  WS-PRT-CLAIMS                 PIC ZZ,ZZZ,ZZZ,ZZ9-.       00993
085700     05  WS-PRINT-CLAIMS REDEFINES WS-PRT-CLAIMS                  00994
085800                                       PIC X(15).                 00995
085900     05  FILLER                        PIC X(07)    VALUE SPACES. 00996
086000     05  WS-PRT-DAYS-VISIT             PIC ZZ,ZZZ,ZZZ,ZZ9-.       00997
086100     05  WS-PRINT-DAYS-VISIT REDEFINES WS-PRT-DAYS-VISIT          00998
086200                                       PIC X(15).                 00999
086300     05  FILLER                        PIC X(02)    VALUE SPACES. 01000
086400                                                                  01001
086500     EJECT                                                        01002
086600 01  WS-SWITCHES.                                                 01003
086700     05  WS-EOF-HOSP-FILE           PIC X     VALUE '1'.          01004
086800         88 WS-EOF-HOSP                       VALUE '0'.          01005
086900     05  WS-EOF-MMD-MSG-FILE        PIC X     VALUE '1'.          01006
087000         88 WS-EOF-MMD-MSG                    VALUE '0'.          01007
087100     05  WS-EOF-DENT-FILE           PIC X     VALUE '1'.          01008
087200         88 WS-EOF-DENT                       VALUE '0'.          01009
087300     05  WS-EOF-DRUG-FILE           PIC X     VALUE '1'.          01010
087400         88 WS-EOF-DRUG                       VALUE '0'.          01011
087500     05  WS-BYPASS-IND              PIC X     VALUE '0'.          01012
087600         88  WS-BYPASS                        VALUE '1'.          01013
087700         88  WS-M-DRUG-BYPASS                 VALUE '2'.          01014
087800     05  WS-M-DRUG-INDICATOR        PIC X     VALUE SPACE.        01015
087900         88  M-DRUG-MEDICAL-CLAIM             VALUE '1'.          01016
088000         88  STANDARD-MEDICAL-CLAIM           VALUE '0'.          01017
088100     05  WS-BASIC-DATA-CHECK        PIC X     VALUE '0'.          01018
088200         88  WS-BASIC-ALL-ZERO                VALUE '1'.          01019
088300     05  WS-MAJOR-MED-DATA-CHECK    PIC X     VALUE '0'.          01020
088400         88  WS-MAJ-MED-ALL-ZERO              VALUE '1'.          01021
088500     05  WS-RIDER-MED-DATA-CHECK    PIC X     VALUE '0'.          01022
088600         88  WS-RIDER-MED-ALL-ZERO            VALUE '1'.          01023
088700     05  WS-CRITICAL-ERROR-FLAG     PIC X     VALUE '0'.          01024
088800         88 WS-CRITICAL-ERROR-ON-OUTPUT       VALUE '1'.          01025
088900     05  WS-LOB-ADJUSTMENT-FLAG     PIC X     VALUE '0'.          01026
089000         88 CLAIM-NOT-ADJUSTED                VALUE 'N'.          01027
089100         88 CLAIM-WAS-ADJUSTED                VALUE 'Y'.          01028
089200     05  WS-NO                      PIC X     VALUE 'N'.          01029
089300     05  WS-YES                     PIC X     VALUE 'Y'.          01030
089400                                                                  01031
089500     EJECT                                                        01032
089600 01  WS-COUNTS-AND-TOTALS-TABLE.                                  01033
089700     05  WS-RECORD-COUNTS-TBL.                                    01034
089800         10 IP-HOSP-REC              PIC S9(11) COMP-3.           01035
089900         10 IP-MCS-REC               PIC S9(11) COMP-3.           01036
090000         10 IP-MAJ-MED-REC           PIC S9(11) COMP-3.           01037
090100         10 IP-DEN-REC               PIC S9(11) COMP-3.           01038
090200         10 IP-DRU-REC               PIC S9(11) COMP-3.           01039
090300                                                                  01040
090400         10 SEL-HOSP-REC             PIC S9(11) COMP-3.           01041
090500         10 SEL-MCS-REC              PIC S9(11) COMP-3.           01042
090600         10 SEL-MAJ-MED-REC          PIC S9(11) COMP-3.           01043
090700         10 SEL-DEN-REC              PIC S9(11) COMP-3.           01044
090800         10 SEL-DRU-REC              PIC S9(11) COMP-3.           01045
090900                                                                  01046
091000         10 BYP-HOSP-REC             PIC S9(11) COMP-3.           01047
091100         10 BYP-MCS-REC              PIC S9(11) COMP-3.           01048
091200         10 BYP-MAJ-MED-REC          PIC S9(11) COMP-3.           01049
091300         10 BYP-DEN-REC              PIC S9(11) COMP-3.           01050
091400         10 BYP-DRU-REC              PIC S9(11) COMP-3.           01051
091500                                                                  01052
091600         10 OP-RCD-COUNT             PIC S9(11) COMP-3.           01053
091700                                                                  01054
091800         10 BYP-MCS-M-DRUG-REC       PIC S9(11) COMP-3.           01055
091900         10 BYP-MAJ-MED-M-DRUG-REC   PIC S9(11) COMP-3.           01056
092000                                                                  01057
092100     05  WS-RECORD-COUNTS-TABLE REDEFINES WS-RECORD-COUNTS-TBL.   01058
092200         10 WS-RECORD-COUNTS  OCCURS 18 TIMES                     01059
092300                                     INDEXED BY REC-INDEX.        01060
092400             15 WS-REC-CNTS          PIC S9(11) COMP-3.           01061
092500                                                                  01062
092600                                                                  01063
092700     05  WS-CLAIMS-COUNT-TBL.                                     01064
092800         10 IP-HOSP-CLAIMS           PIC S9(11) COMP-3.           01065
092900         10 IP-MCS-CLAIMS            PIC S9(11) COMP-3.           01066
093000         10 IP-MAJ-MED-CLAIMS        PIC S9(11) COMP-3.           01067
093100         10 IP-DEN-CLAIMS            PIC S9(11) COMP-3.           01068
093200         10 IP-DRU-CLAIMS            PIC S9(11) COMP-3.           01069
093300                                                                  01070
093400         10 SEL-HOSP-CLAIMS          PIC S9(11) COMP-3.           01071
093500         10 SEL-MCS-CLAIMS           PIC S9(11) COMP-3.           01072
093600         10 SEL-MAJ-MED-CLAIMS       PIC S9(11) COMP-3.           01073
093700         10 SEL-DEN-CLAIMS           PIC S9(11) COMP-3.           01074
093800         10 SEL-DRU-CLAIMS           PIC S9(11) COMP-3.           01075
093900                                                                  01076
094000         10 BYP-HOSP-CLAIMS          PIC S9(11) COMP-3.           01077
094100         10 BYP-MCS-CLAIMS           PIC S9(11) COMP-3.           01078
094200         10 BYP-MAJ-MED-CLAIMS       PIC S9(11) COMP-3.           01079
094300         10 BYP-DEN-CLAIMS           PIC S9(11) COMP-3.           01080
094400         10 BYP-DRU-CLAIMS           PIC S9(11) COMP-3.           01081
094500                                                                  01082
094600         10 OP-CLAIMS-COUNT          PIC S9(11) COMP-3.           01083
094700                                                                  01084
094800         10 BYP-MCS-M-DRUG-CLAIMS    PIC S9(11) COMP-3.           01085
094900         10 BYP-MAJ-MED-DRUG-CLAIMS  PIC S9(11) COMP-3.           01086
095000                                                                  01087
095100                                                                  01088
095200     05  CLAIMS-COUNT-TABLE REDEFINES WS-CLAIMS-COUNT-TBL.        01089
095300         10  WS-CLAIMS-COUNT-TBL OCCURS 18 TIMES                  01090
095400                                     INDEXED BY CLAIMS-INDEX.     01091
095500             15 WS-CLAIMS-CNTS       PIC S9(11) COMP-3.           01092
095600                                                                  01093
095700     EJECT                                                        01094
095800     05  WS-SERVICES-COUNT-TBL.                                   01095
095900         10 IP-HOSP-SERVICES         PIC S9(11) COMP-3.           01096
096000         10 IP-MCS-SERVICES          PIC S9(11) COMP-3.           01097
096100         10 IP-MAJ-MED-SERVICES      PIC S9(11) COMP-3.           01098
096200         10 IP-DEN-SERVICES          PIC S9(11) COMP-3.           01099
096300         10 IP-DRU-SERVICES          PIC S9(11) COMP-3.           01100
096400                                                                  01101
096500         10 SEL-HOSP-SERVICES        PIC S9(11) COMP-3.           01102
096600         10 SEL-MCS-SERVICES         PIC S9(11) COMP-3.           01103
096700         10 SEL-MAJ-MED-SERVICES     PIC S9(11) COMP-3.           01104
096800         10 SEL-DEN-SERVICES         PIC S9(11) COMP-3.           01105
096900         10 SEL-DRU-SERVICES         PIC S9(11) COMP-3.           01106
097000                                                                  01107
097100         10 BYP-HOSP-SERVICES        PIC S9(11) COMP-3.           01108
097200         10 BYP-MCS-SERVICES         PIC S9(11) COMP-3.           01109
097300         10 BYP-MAJ-MED-SERVICES     PIC S9(11) COMP-3.           01110
097400         10 BYP-DEN-SERVICES         PIC S9(11) COMP-3.           01111
097500         10 BYP-DRU-SERVICES         PIC S9(11) COMP-3.           01112
097600                                                                  01113
097700         10 OP-SERVICES-COUNT        PIC S9(11) COMP-3.           01114
097800                                                                  01115
097900         10 BYP-MCS-M-DRUG-SVCS      PIC S9(11) COMP-3.           01116
098000         10 BYP-MAJ-MED-M-DRUG-SVCS  PIC S9(11) COMP-3.           01117
098100                                                                  01118
098200                                                                  01119
098300      05 SERVICES-COUNT-TABLE REDEFINES WS-SERVICES-COUNT-TBL.    01120
098400         10  WS-SERVICES-COUNTS  OCCURS 18 TIMES                  01121
098500                                     INDEXED BY SERVICES-INDEX.   01122
098600             15  WS-SERVICES-CNTS    PIC S9(11) COMP-3.           01123
098700                                                                  01124
098800     EJECT                                                        01125
098900     05  WS-DAYS-COUNT-TBL.                                       01126
099000         10 IP-HOSP-DAYS             PIC S9(11) COMP-3.           01127
099100         10 IP-MCS-DAYS              PIC S9(11) COMP-3.           01128
099200         10 IP-MAJ-MED-DAYS          PIC S9(11) COMP-3.           01129
099300         10 IP-DEN-DAYS              PIC S9(11) COMP-3.           01130
099400         10 IP-DRU-DAYS              PIC S9(11) COMP-3.           01131
099500                                                                  01132
099600         10 SEL-HOSP-DAYS            PIC S9(11) COMP-3.           01133
099700         10 SEL-MCS-DAYS             PIC S9(11) COMP-3.           01134
099800         10 SEL-MAJ-MED-DAYS         PIC S9(11) COMP-3.           01135
099900         10 SEL-DEN-DAYS             PIC S9(11) COMP-3.           01136
100000         10 SEL-DRU-DAYS             PIC S9(11) COMP-3.           01137
100100                                                                  01138
100200         10 BYP-HOSP-DAYS            PIC S9(11) COMP-3.           01139
100300         10 BYP-MCS-DAYS             PIC S9(11) COMP-3.           01140
100400         10 BYP-MAJ-MED-DAYS         PIC S9(11) COMP-3.           01141
100500         10 BYP-DEN-DAYS             PIC S9(11) COMP-3.           01142
100600         10 BYP-DRU-DAYS             PIC S9(11) COMP-3.           01143
100700                                                                  01144
100800         10 OP-DAYS-COUNT            PIC S9(11) COMP-3.           01145
100900                                                                  01146
101000         10 FILLER                   PIC S9(11) COMP-3.           01147
101100         10 FILLER                   PIC S9(11) COMP-3.           01148
101200                                                                  01149
101300     05  DAYS-COUNT-TABLE REDEFINES WS-DAYS-COUNT-TBL.            01150
101400         10 WS-DAYS-COUNT  OCCURS 18 TIMES                        01151
101500                                     INDEXED BY DAYS-INDEX.       01152
101600             15 WS-DAYS-CNTS         PIC S9(11) COMP-3.           01153
101700     EJECT                                                        01154
101800     05  WS-LOB-LIABILITY-AMOUNTS-TBL.                            01155
101900                                                                  01156
102000         10 IP-HOSP-LIAB-AMT         PIC S9(13)V99 COMP-3.        01157
102100         10 IP-MCS-LIAB-AMT          PIC S9(13)V99 COMP-3.        01158
102200         10 IP-MAJ-MED-LIAB-AMT      PIC S9(13)V99 COMP-3.        01159
102300         10 IP-DEN-LIAB-AMT          PIC S9(13)V99 COMP-3.        01160
102400         10 IP-DRU-LIAB-AMT          PIC S9(13)V99 COMP-3.        01161
102500                                                                  01162
102600         10 SEL-HOSP-LIAB-AMT        PIC S9(13)V99 COMP-3.        01163
102700         10 SEL-MCS-LIAB-AMT         PIC S9(13)V99 COMP-3.        01164
102800         10 SEL-MAJ-MED-LIAB-AMT     PIC S9(13)V99 COMP-3.        01165
102900         10 SEL-DEN-LIAB-AMT         PIC S9(13)V99 COMP-3.        01166
103000         10 SEL-DRU-LIAB-AMT         PIC S9(13)V99 COMP-3.        01167
103100                                                                  01168
103200         10 BYP-HOSP-LIAB-AMT        PIC S9(13)V99 COMP-3.        01169
103300         10 BYP-MCS-LIAB-AMT         PIC S9(13)V99 COMP-3.        01170
103400         10 BYP-MAJ-MED-LIAB-AMT     PIC S9(13)V99 COMP-3.        01171
103500         10 BYP-DEN-LIAB-AMT         PIC S9(13)V99 COMP-3.        01172
103600         10 BYP-DRU-LIAB-AMT         PIC S9(13)V99 COMP-3.        01173
103700                                                                  01174
103800         10 OP-LIAB-AMT               PIC S9(13)V99 COMP-3.       01175
103900                                                                  01176
104000         10 BYP-MCS-MAN-LIAB-AMT      PIC S9(13)V99 COMP-3.       01177
104100         10 BYP-MCS-MAJ-MED-LIAB-AMT  PIC S9(13)V99 COMP-3.       01178
104200                                                                  01179
104300     05  LOB-LIABILITY-AMOUNTS-TABLE REDEFINES                    01180
104400                                WS-LOB-LIABILITY-AMOUNTS-TBL.     01181
104500         10 WS-LOB-LIAB-AMOUNTS  OCCURS 18 TIMES                  01182
104600                                     INDEXED BY LIAB-INDEX.       01183
104700             15 WS-LOB-LIAB-AMTS      PIC S9(13)V99 COMP-3.       01184
104800    EJECT                                                         01185
104900 01  WS-TRAILER-SAVE-AREAS.                                       01186
105000     10 WS-HOSP-TRLR-RECS             PIC S9(11) COMP-3           01187
105100                                          VALUE +0.               01188
105200     10 WS-HOSP-TRLR-LIAB             PIC S9(13)V99 COMP-3        01189
105300                                          VALUE +0.               01190
105400     10 WS-MCS-TRLR-RECS              PIC S9(11) COMP-3           01191
105500                                          VALUE +0.               01192
105600     10 WS-MCS-COMPUTED-LIAB          PIC S9(13)V99 COMP-3        01193
105700                                          VALUE +0.               01194
105800     10 WS-M-DRUG-RECORD-COUNT        PIC S9(11) COMP-3           01195
105900                                          VALUE +0.               01196
106000     10 WS-DENTAL-TRLR-RECS           PIC S9(11) COMP-3           01197
106100                                          VALUE +0.               01198
106200     10 WS-DENTAL-TRLR-LIAB           PIC S9(13)V99 COMP-3        01199
106300                                          VALUE +0.               01200
106400     10 WS-DRUG-TRLR-RECS             PIC S9(11) COMP-3           01201
106500                                          VALUE +0.               01202
106600     10 WS-DRUG-TRLR-LIAB             PIC S9(13)V99 COMP-3        01203
106700                                          VALUE +0.               01204
106800     EJECT                                                        01205
106900 01  WS-PREV-AREAS.                                               01206
230800     05 WS-DATE-CONVERT-DISPL.                                    01207
107100        10  WS-DATE-YY                  PIC 9(02) VALUE ZERO.     01208
107200        10  WS-DATE-MM                  PIC 9(02) VALUE ZERO.     01209
107300        10  WS-DATE-DD                  PIC 9(02) VALUE ZERO.     01210
107000     05 WS-PREV-ACCT-PAID-DATE.                                   01211
107100        10  WS-PREV-ACC-PAID-DATE-YY    PIC 9(02) VALUE ZERO.     01212
107200        10  WS-PREV-ACC-PAID-DATE-MM    PIC 9(02) VALUE ZERO.     01213
107300        10  WS-PREV-ACC-PAID-DATE-DD    PIC 9(02) VALUE ZERO.     01214
107400     05 WS-PREV-PAID-DENT-DATE.                                   01215
107500        10  WS-PREV-PAID-DENT-MM        PIC 9(02) VALUE ZERO.     01216
107600        10  WS-PREV-PAID-DENT-DD        PIC 9(02) VALUE ZERO.     01217
107700        10  WS-PREV-PAID-DENT-YY        PIC 9(02) VALUE ZERO.     01218
107800     05 WS-PREV-INC-DENT-DATE.                                    01219
107900        10  WS-PREV-INC-DENT-MM         PIC 9(02) VALUE ZERO.     01220
108000        10  WS-PREV-INC-DENT-DD         PIC 9(02) VALUE ZERO.     01221
108100        10  WS-PREV-INC-DENT-YY         PIC 9(02) VALUE ZERO.     01222
108200     05 WS-PREV-PAID-DRUG-DATE.                                   01223
108300        10  WS-PREV-PAID-DRUG-MM        PIC 9(02) VALUE ZERO.     01224
108400        10  WS-PREV-PAID-DRUG-DD        PIC 9(02) VALUE ZERO.     01225
108500        10  WS-PREV-PAID-DRUG-YY        PIC 9(02) VALUE ZERO.     01226
108600     05 WS-PREV-INC-DRUG-DATE.                                    01227
108700        10  WS-PREV-INC-DRUG-MM         PIC 9(02) VALUE ZERO.     01228
108800        10  WS-PREV-INC-DRUG-DD         PIC 9(02) VALUE ZERO.     01229
108900        10  WS-PREV-INC-DRUG-YY         PIC 9(02) VALUE ZERO.     01230
109000     05 WS-PREV-PAID-DATE.                                        01231
109100        10  WS-PREV-PAID-YY             PIC 9(02) VALUE ZERO.     01232
109200        10  WS-PREV-PAID-MM             PIC 9(02) VALUE ZERO.     01233
109300        10  WS-PREV-PAID-DD             PIC 9(02) VALUE ZERO.     01234
109400     05 WS-PREV-INC-DATE.                                         01235
109500        10  WS-PREV-INC-YY             PIC 9(02) VALUE ZERO.      01236
109600        10  WS-PREV-INC-MM             PIC 9(02) VALUE ZERO.      01237
109700        10  WS-PREV-INC-DD             PIC 9(02) VALUE ZERO.      01238
109800     05 WS-PREV-INC-PERIOD REDEFINES WS-PREV-INC-DATE.            01239
109900        10  WS-PREV-INC-YY-MM          PIC 9(04).                 01240
110000        10  WS-PREV-INCURR-DD          PIC 9(02).                 01241
110100                                                                  01242
110200     05 WS-PREV-PAID-DAY          PIC 9(2)  VALUE ZERO.           01243
110200     05 WS-PREV-REC-CLAIM-CNTR    PIC S9(1) COMP-3 VALUE +0.      01244
           05 WS-PREV-CLM-ST-IND        PIC X(1) VALUE SPACES.
110300     05 WS-PREV-CLAIM-NO          PIC 9(14) VALUE ZEROES.         01245
110400     05 WS-IRFN-CLAIM-NO               PIC X(14) VALUE SPACES.    01246
110500     05 WS-PREV-DENT-SERVICE           PIC X     VALUE SPACES.    01247
110600     05 WS-PREV-DRUG-SERVICE           PIC X     VALUE SPACES.    01248
110700     05 WS-PREV-EDS-TOS                PIC X     VALUE SPACES.    01249
110800     05 WS-PREV-ACCOM-CODE REDEFINES                              01250
110900           WS-PREV-EDS-TOS.                                       01251
111000        10 ACCOMODATION-CODE           PIC X.                     01252
111100     05  WS-MCS-PREV-SAVE-AREA.                                   01253
111200       10  WS-PREV-ASO-IND             PIC X.                     01254
111300       10  WS-PREV-SOURCE-CODE         PIC X.                     01255
111400       10  WS-PREV-ACT-PLN-CODE        PIC X.                     01256
111500       10  WS-PREV-GROUP-NUMBER        PIC 9(07) VALUE ZEROS.     01257
111600       10  WS-PREV-GROUP-NUM REDEFINES WS-PREV-GROUP-NUMBER.      01258
111700           15  WS-PREV-GRP-NUM-1ST-POS PIC 9(1).                  01259
111800           15  WS-PREV-GRP-NUM         PIC 9(6).                  01260
111900       10  WS-PREV-SUB-DIVISION        PIC X(3) VALUE SPACE.      01261
112000       10  WS-PREV-SUB-IDENT           PIC X(14) VALUE ZERO.      01262
112100       10  WS-PREV-FIRST-INITIAL       PIC X(1) VALUE SPACE.      01263
112200       10  WS-PREV-LAST-NAME           PIC X(15) VALUE SPACES.    01264
112300       10  WS-PREV-SNR-CARE-IND        PIC X(1) VALUE SPACE.      01265
112400       10  WS-PREV-POS-IND             PIC X(1) VALUE SPACE.      01266
112400       10  WS-PREV-CMS-IND             PIC X(2) VALUE SPACE.      01267

112500     EJECT                                                        01268
112600  01 WS-SUMMARY-AREA.                                             01269
112700     05  WS-SUMMARY-AHS-PAID              PIC S9(11)V99 COMP-3    01270
112800                                                        VALUE +0. 01271
112900     05  WS-SUMMARY-DAYS-VISITS           PIC S9(11)    COMP-3    01272
113000                                                        VALUE +0. 01273
113100     05  WS-SUMMARY-HOSP-CLAIM-COUNT      PIC S9(11)    COMP-3    01274
113200                                                        VALUE +0. 01275
113300     05  WS-SUMMARY-DENTAL-PAID           PIC S9(11)V99 COMP-3    01276
113400                                                        VALUE +0. 01277
113500     05  WS-SUMMARY-DENTAL-SERVICES       PIC S9(11)    COMP-3    01278
113600                                                        VALUE +0. 01279
113700     05  WS-SUMMARY-DENTAL-CLAIM-COUNT    PIC S9(11)    COMP-3    01280
113800                                                        VALUE +0. 01281
113900     05  WS-SUMMARY-DRUG-PAID             PIC S9(11)V99 COMP-3    01282
114000                                                        VALUE +0. 01283
114100     05  WS-SUMMARY-DRUG-SERVICES         PIC S9(11)    COMP-3    01284
114200                                                        VALUE +0. 01285
114300     05  WS-SUMMARY-DRUG-CLAIM-COUNT      PIC S9(11)    COMP-3    01286
114400                                                        VALUE +0. 01287
114500     05  WS-SUMMARY-BASIC-PAID            PIC S9(11)V99 COMP-3    01288
114600                                                        VALUE +0. 01289
114700     05  WS-SUMMARY-BASIC-SERVICES        PIC S9(11)    COMP-3    01290
114800                                                        VALUE +0. 01291
114900     05  WS-SUMMARY-BASIC-CLAIM-COUNT     PIC S9(11)    COMP-3    01292
115000                                                        VALUE +0. 01293
115100     05  WS-SUMMARY-MAJOR-MED-PAID        PIC S9(11)V99 COMP-3    01294
115200                                                        VALUE +0. 01295
115300     05  WS-SUMMARY-MAJOR-MED-SERVICES    PIC S9(11)    COMP-3    01296
115400                                                        VALUE +0. 01297
115500     05  WS-SUMMARY-MAJ-MED-CLAIM-COUNT   PIC S9(11)    COMP-3    01298
115600                                                        VALUE +0. 01299
115700     05  WS-SUMMARY-RIDER-MED-PAID        PIC S9(11)V99 COMP-3    01300
115800                                                        VALUE +0. 01301
115900     05  WS-SUMMARY-RIDER-MED-SERVICES    PIC S9(11)    COMP-3    01302
116000                                                        VALUE +0. 01303
116100     05  WS-SUMMARY-RIDER-MED-CLAIM-CNT   PIC S9(11)    COMP-3    01304
116200                                                        VALUE +0. 01305
116300     05  WS-SUMMARY-CLAIM-COUNT           PIC S9(11)    COMP-3    01306
116400                                                        VALUE +0. 01307
116500     EJECT                                                        01308
116600  01 WS-REJECTS-AREA.                                             01309
116700     05  WS-REJECT-RIDER-AMT                 PIC S9(11)V99 COMP-3 01310
116800                                             VALUE +0.            01311
116900     05  WS-REJECT-CNTS                      PIC S9(11) COMP-3    01312
117000                                             VALUE +0.            01313
117100     05  WS-REJECT-MAJOR-MED-AMT             PIC S9(11)V99 COMP-3 01314
117200                                             VALUE +0.            01315
117300     05  WS-REJECT-BASIC-AMT                 PIC S9(11)V99 COMP-3 01316
117400                                             VALUE +0.            01317
117500 01  WS-LOB-ALPHA-LIT.                                            01318
117600     05  FILLER                      PIC X(8) VALUE 'HOSPITAL'.   01319
117700     05  FILLER                      PIC X(8) VALUE 'DENTAL  '.   01320
117800     05  FILLER                      PIC X(8) VALUE 'DRUG    '.   01321
117900     05  FILLER                      PIC X(8) VALUE 'MEDICAL '.   01322
118000 01  WS-LOB-VALUES REDEFINES WS-LOB-ALPHA-LIT.                    01323
118100     05  LINE-OF-BUSINESS-NAME       OCCURS 4 TIMES PIC X(8).     01324
118200 01  WS-LOB-LIT.                                                  01325
118300     05  WS-HOSPITAL-LOB-LIT              PIC X         VALUE '1'.01326
118400     05  WS-DENTAL-LOB-LIT                PIC X         VALUE '2'.01327
118500     05  WS-DRUG-LOB-LIT                  PIC X         VALUE '3'.01328
118600     05  WS-BASIC-MED-LOB-LIT             PIC X         VALUE '4'.01329
118700     05  WS-MAJOR-MED-LOB-LIT             PIC X         VALUE '5'.01330
118200 77  WS-PGM-COUNT                         PIC 9(13).              01325
118800     EJECT                                                        01331
118200*- ACCESS FEE CHANGES START                                       01325
118200 77  WS-ACCESS-RECORDS                    PIC 9(4) VALUE ZERO.    01325
118200 77  WS-PLUS-1                            PIC 9(4) VALUE 1.       01325
118800     EJECT                                                        01331
048200 01  ACCESS-FEE-TABLE.
048300     05  ACCESS-FEE-GROUP OCCURS 9999 TIMES
048300            DEPENDING ON WS-ACCESS-RECORDS INDEXED BY FEE-IX.
048400         10  AFEE-GROUP         PIC X(06).
048500         10  AFEE-SUBDIV        PIC X(03).
      *
       01  WS-WORK-AREAS.
           05  ARE-THERE-MORE-RECORDS    PIC  X(1)    VALUE 'Y'.
               88 MORE-RECORDS                        VALUE 'Y'.
               88 NO-MORE-RECORDS                     VALUE 'N'.
      *-
008700 01  I-ACCESS-FEE-GROUP-RECORD.                                   00110
008700     10 WS-ACCESS-FEE-GROUP                  PIC X(06).           00110
008700     10 WS-ACCESS-FEE-SUBDIV                 PIC X(03).           00110
008700     10 FILLER                               PIC X(71).           00110
008800                                                                  00111
118200*- ACCESS FEE CHANGES END                                         01325
118900 ++INCLUDE HRIC2600                                               01332
119000     EJECT                                                        01333
118900 ++INCLUDE HRMERC2650                                             01334
119000     EJECT                                                        01335
000400     COPY CM7983A.                                                01344
119200     EJECT                                                        01337
119300 ++INCLUDE HRPRODDATE                                             01340
119400     EJECT                                                        01341
143500 01  SEL-PAID-CLAIMS-RECORD.                                      01343
000400     COPY FNSELFIX REPLACING   ==:S:==  BY ==SEL==                01344
000500                               ==:H:==  BY ==HR==.                01345
119600     EJECT                                                        01346
RS1298     COPY FN800VAR REPLACING   ==:I:==  BY ==SEL==                01347
000500                               ==:H:==  BY ==HR==                 01348
000500                               ==:HC:== BY ==HRCR==               01349
000700                               ==:M:==  BY ==MS==                 01350
000800                               ==:D:==  BY ==DT==                 01351
000900                               ==:R:==   BY ==DR==                01352
001000                               ==:RR:==  BY ==REJ==               01353
001000                               ==:MM:==  BY ==MMM==.              01354
119800     EJECT                                                        01355
RS1298     COPY FN800HD1 REPLACING   ==:S:==  BY ==SEL==                01356
000500                               ==:H:==  BY ==HR==.                01357
119800     EJECT                                                        01358
000400     COPY CMREJCLM.                                               01359
119800     EJECT                                                        01360
119700 ++INCLUDE FNMCSPCS                                               01361
120000     EJECT                                                        01365
119900 ++INCLUDE MSMCSS17                                               01366
120000     EJECT                                                        01367
120100 ++INCLUDE FNCSSHDR                                               01368
120200     EJECT                                                        01369
120300 ++INCLUDE FNCSSTRL                                               01370
120400     EJECT                                                        01371
120500     COPY NRX140C1.                                               01372
120600     EJECT                                                        01373
120700 LINKAGE SECTION.                                                 01374
120800 01  LS-PARMS-AREA.                                               01375
120900     SKIP3                                                        01376
121000     05  PARM-LEN                        PIC S9(02) COMP SYNC.    01377
121100     SKIP3                                                        01378
121200     05  LS-LINE-OF-BUSINESS-CODE        PIC 9 VALUE 0.           01379
121300     SKIP1                                                        01380
121400         88 LS-NULL-BUSINESS-LINE        VALUE 0.                 01381
121300     SKIP1                                                        01382
121400         88 LS-HOSPITAL-LINE-LOB         VALUE 1.                 01383
121500     SKIP1                                                        01384
121600         88 LS-DENTAL-LINE-LOB           VALUE 2.                 01385
121700     SKIP1                                                        01386
121800         88 LS-DRUG-LINE-LOB             VALUE 3.                 01387
121900     SKIP1                                                        01388
122000         88 LS-MEDICAL-LINE-LOB          VALUE 4.                 01389
122100     SKIP1                                                        01390
122200         88 LS-VALID-BUSINESS-LINE       VALUE 1 THRU 4.          01391
122300     SKIP2                                                        01392
122400     05  LS-OTHER-LINE-FEED              PIC X VALUE SPACE.       01393
122500     SKIP1                                                        01394
122200         88 LS-CHARGE-HOSPITAL-LOB          VALUE '1'.            01395
122500     SKIP1                                                        01396
122200         88 LS-REJECT-HOSPITAL-LOB          VALUE '2'.            01397
122500     SKIP1                                                        01398
122200         88 LS-VALID-OTHER-LINE          VALUE '1' '2'.           01399
122500     SKIP1                                                        01400
122400     05  LS-PARM-TO-ABEND-CODE           PIC X VALUE SPACE.       01401
122500     SKIP1                                                        01402
122600         88 LS-ABEND-ON-CRITICAL-ERROR   VALUE 'Y'.               01403
122700     EJECT                                                        01404
122800 PROCEDURE DIVISION USING LS-PARMS-AREA.                          01405
122900     SKIP3                                                        01406
123000 0000-MAINLINE.                                                   01407
123100     SKIP3                                                        01408
123200     PERFORM A-0050-SHOW-PARM-AND-COMP-DATE.                      01409
123300     SKIP3                                                        01410
123400     IF (NOT LS-VALID-BUSINESS-LINE                               01411
123400         AND NOT   LS-VALID-OTHER-LINE)                           01412
123500            PERFORM A-0100-ABEND-ROUTINE                          01413
123600     SKIP1                                                        01414
123700     ELSE                                                         01415
123800        PERFORM A-1000-HOUSEKEEPING-ROUTINE.                      01416
123900     SKIP3                                                        01417
124000     IF LS-HOSPITAL-LINE-LOB                                      01418
124100        PERFORM B-1000-PROCESS-HOSPITAL-FILE                      01419
124200     SKIP3                                                        01420
124300     ELSE                                                         01421
122200     IF LS-CHARGE-HOSPITAL-LOB                                    01422
124100        PERFORM BA-1000-PROCESS-HOSPITAL-FILE                     01423
124200     SKIP3                                                        01424
124300     ELSE                                                         01425
122200     IF LS-REJECT-HOSPITAL-LOB                                    01426
124100        PERFORM BB-1000-PROCESS-HOSPITAL-FILE                     01427
124200     SKIP3                                                        01428
124300     ELSE                                                         01429
124400     IF LS-MEDICAL-LINE-LOB                                       01430
124500        PERFORM C-1000-PROCESS-MCS-CSS-FILE                       01431
124600     SKIP3                                                        01432
124700     ELSE                                                         01433
124800     IF LS-DENTAL-LINE-LOB                                        01434
124900        PERFORM D-1000-PROCESS-DENTAL-FILE.                       01435
125000     SKIP3                                                        01436
125500     IF LS-VALID-BUSINESS-LINE                                    01441
125500        OR LS-VALID-OTHER-LINE                                    01442
125600     SKIP2                                                        01443
125700        PERFORM A-0200-SHOW-DEFAULTS                              01444
125800        PERFORM X-1000-END-OF-JOB-ROUTINE                         01445
125900        CLOSE O-COMMON-LOB-PAID-CLAIMS                            01446
126000            O-PAID-CLAIM-CONTROL-REPORT.                          01447
126100     SKIP3                                                        01448
                                                                        01449
                                                                        01450
126200     GOBACK.                                                      01451
126300     EJECT                                                        01452
126400 A-0050-SHOW-PARM-AND-COMP-DATE.                                  01453
126500************************************************************      01454
126600*  PURPOSE:                                                *      01455
126700*         TO SHOW THE VALUE OF THE PARM SUPPLIED TO PROGRAM*      01456
126800*         TO SUPPLY THE LAST COMPILED DATE INFORMATION     *      01457
126900*         PRIOR TO ATTEMPTING PROGRAM EXECUTION.           *      01458
127000************************************************************      01459
127100     SKIP1                                                        01460
127200     MOVE LS-LINE-OF-BUSINESS-CODE    TO WS-PARM-CODE.            01461
127300     DISPLAY WS-DISPLAY-PARM-MESSAGE WS-PARM-CODE.                01462
127400     DISPLAY WS-VALCON-1.                                         01463
127500     IF LS-HOSPITAL-LINE-LOB                                      01464
127600           MOVE WS-LIT-ICHIS-SYS TO HEADING-3-FEEDING-SYS-NAME    01465
127700           MOVE WS-LIT-ICHIS TO HEADING-1-FEED-SYS-ACC-CODE       01466
127800           DISPLAY LINE-OF-BUSINESS-NAME (1).                     01467
127900     IF LS-DENTAL-LINE-LOB                                        01468
128000           MOVE WS-LIT-DENTAL-SYS TO HEADING-3-FEEDING-SYS-NAME   01469
128100           MOVE WS-LIT-DENTAL TO HEADING-1-FEED-SYS-ACC-CODE      01470
128200           DISPLAY LINE-OF-BUSINESS-NAME (2).                     01471
128700     IF LS-MEDICAL-LINE-LOB                                       01476
128800           MOVE WS-LIT-MANREP-SYS TO HEADING-3-FEEDING-SYS-NAME   01477
128900           MOVE WS-LIT-MANREP TO HEADING-1-FEED-SYS-ACC-CODE      01478
129000           DISPLAY LINE-OF-BUSINESS-NAME (4).                     01479
129100     SKIP1                                                        01480
129200     MOVE LS-OTHER-LINE-FEED         TO  WS-PARM-CODE.            01481
129300     DISPLAY  WS-DISPLAY-PARM-OTHER  WS-PARM-CODE.                01482
129300     DISPLAY  WS-VALCON-1A.                                       01483
129100     SKIP1                                                        01484
122200     IF LS-CHARGE-HOSPITAL-LOB                                    01485
118300           MOVE WS-HOSPITAL-LOB-LIT      TO WS-PARM-CODE          01486
127600           MOVE WS-LIT-ICHIS-SYS TO HEADING-3-FEEDING-SYS-NAME    01487
127700           MOVE WS-LIT-ICHIS TO HEADING-1-FEED-SYS-ACC-CODE       01488
127800           DISPLAY LINE-OF-BUSINESS-NAME (1)                      01489
129100     ELSE                                                         01490
122200     IF LS-REJECT-HOSPITAL-LOB                                    01491
118300           MOVE WS-HOSPITAL-LOB-LIT      TO WS-PARM-CODE          01492
127600           MOVE WS-LIT-ICHIS-SYS TO HEADING-3-FEEDING-SYS-NAME    01493
127700           MOVE WS-LIT-ICHIS TO HEADING-1-FEED-SYS-ACC-CODE       01494
127800           DISPLAY LINE-OF-BUSINESS-NAME (1).                     01495
129100     SKIP1                                                        01496
129200     MOVE WHEN-COMPILED TO COMPILE-VERSION.                       01497
129300     DISPLAY  WS-DISPLAY-1.                                       01498
129400     DISPLAY WS-MODULE-NAME.                                      01499
129500     DISPLAY WS-COMPILE-MSG.                                      01500
129600     DISPLAY COMPILE-VERSION.                                     01501
129700     DISPLAY  WS-DISPLAY-1.                                       01502
129800     EJECT                                                        01503
129900 A-0100-ABEND-ROUTINE.                                            01504
130000************************************************************      01505
130100*  PURPOSE:                                                *      01506
130200*         TERMINATE EXECUTION IMMEDIATELY DUE TO CRITICAL  *      01507
130300*         INPUT ERROR - NO PARM PASSED TO DRIVE EXECUTION. *      01508
130400************************************************************      01509
130500     SKIP1                                                        01510
130600     MOVE WS-ABEND-CODE(17) TO USER-ABEND-CODE                    01511
130700     DISPLAY USER-ABEND-CODE                                      01512
130800     DISPLAY WS-CRITICAL-ERROR-MESSAGE-1                          01513
130900     DISPLAY WS-CRITICAL-ERROR-MESSAGE-2                          01514
131000     PERFORM Z-CALL-BOMBER.                                       01515
131100     EJECT                                                        01516
131200 A-0200-SHOW-DEFAULTS.                                            01517
131300************************************************************      01518
131400*  PURPOSE:                                                *      01519
131500*         PROVIDE TOTALS RELATED TO DATA ELEMENT DEFAULTS  *      01520
131600*         TAKEN DURING PROCESSING OF INPUT CLAIMS DATA.    *      01521
131700************************************************************      01522
131800     SKIP1                                                        01523
131900         DISPLAY  SPACES                                          01524
132000         DISPLAY  SPACES                                          01525
132100         DISPLAY  WS-DEF-COUNTS                                   01526
132200         DISPLAY  SPACES                                          01527
132300         DISPLAY  WS-DEF-CLAIM                                    01528
132400         DISPLAY  WS-DEF-SUBID                                    01529
132500         DISPLAY  WS-DEF-PDDAY                                    01530
132600         DISPLAY  WS-DEF-INCDA                                    01531
132700         DISPLAY  WS-DEF-INCYR                                    01532
132800         DISPLAY  WS-DEF-INCMO                                    01533
132900         DISPLAY  WS-DEF-GRPNO                                    01534
133000         DISPLAY  WS-DEF-LOBNO                                    01535
133100         DISPLAY  WS-DEF-MSBRK                                    01536
133200         DISPLAY  SPACES                                          01537
133300         DISPLAY  SPACES                                          01538
133400         DISPLAY  WS-LOG-COUNTS                                   01539
133500         DISPLAY  SPACES                                          01540
133600         DISPLAY  WS-LOG-PDDATE                                   01541
133700         DISPLAY  WS-LOG-PAYDAY                                   01542
133800         DISPLAY  WS-LOG-INCDAY                                   01543
133900         DISPLAY  WS-LOG-PAYMON                                   01544
134000         DISPLAY  WS-LOG-INCMON.                                  01545
134100      EJECT                                                       01546
134200 A-1000-HOUSEKEEPING-ROUTINE.                                     01547
134300************************************************************      01548
134400*  PURPOSE:                                                *      01549
134500*         INITIALIZE ALL WORKING STORAGE COUNTERS,         *      01550
134600*         CREATES WORK DATE AND TIME FOR REPORT HEADING.   *      01551
134700************************************************************      01552
134800     SKIP1                                                        01553
134900     OPEN OUTPUT O-COMMON-LOB-PAID-CLAIMS                         01554
135000                 O-PAID-CLAIM-CONTROL-REPORT.                     01555
135100     SKIP1                                                        01556
135200     PERFORM  A-1400-INITIALIZE-COUNTS-RTN                        01557
135300              VARYING REC-INDEX                                   01558
135400              FROM WS-PLUS-ONE BY WS-PLUS-ONE                     01559
135500              UNTIL REC-INDEX GREATER THAN WS-PLUS-EIGHTEEN.      01560
135600                                                                  01561
135700     PERFORM  A-1500-INITIALIZE-COUNTS-RTN                        01562
135800              VARYING CLAIMS-INDEX                                01563
135900              FROM WS-PLUS-ONE BY WS-PLUS-ONE                     01564
136000              UNTIL CLAIMS-INDEX GREATER THAN WS-PLUS-EIGHTEEN.   01565
136100                                                                  01566
136200     PERFORM  A-1600-INITIALIZE-COUNTS-RTN                        01567
136300             VARYING SERVICES-INDEX                               01568
136400             FROM WS-PLUS-ONE BY WS-PLUS-ONE                      01569
136500             UNTIL SERVICES-INDEX GREATER THAN WS-PLUS-EIGHTEEN.  01570
136600                                                                  01571
136700     PERFORM  A-1700-INITIALIZE-COUNTS-RTN                        01572
136800              VARYING DAYS-INDEX                                  01573
136900              FROM WS-PLUS-ONE BY WS-PLUS-ONE                     01574
137000              UNTIL DAYS-INDEX GREATER THAN WS-PLUS-EIGHTEEN.     01575
137100                                                                  01576
137200     PERFORM  A-1800-INITIALIZE-COUNTS-RTN                        01577
137300              VARYING LIAB-INDEX                                  01578
137400              FROM WS-PLUS-ONE BY WS-PLUS-ONE                     01579
137500              UNTIL LIAB-INDEX GREATER THAN WS-PLUS-EIGHTEEN.     01580
137600                                                                  01581
137700     PERFORM  A-1900-INIT-MS-PAYMENT-TABLE                        01582
137800              VARYING MS-IX-1                                     01583
137900              FROM WS-PLUS-ONE BY WS-PLUS-ONE                     01584
138000              UNTIL MS-IX-1 GREATER THAN WS-PLUS-FOUR.            01585
138100                                                                  01586
138200     PERFORM  A-1950-INIT-MS-CO-PAYMENT                           01587
138300              VARYING MS-IX-2                                     01588
138400              FROM WS-PLUS-ONE BY WS-PLUS-ONE                     01589
138500              UNTIL MS-IX-2 GREATER THAN WS-PLUS-FOUR.            01590
138600                                                                  01591
138700     PERFORM A-1100-PROCESS-PROD-DATE-FILE.                       01592
138700*- ACCESS FEE CHANGES START                                       01592
138700     PERFORM A-1200-PROCESS-ACCESS-FEE-FILE.                      01592
138700*- ACCESS FEE CHANGES END                                         01592
138700 A-1200-PROCESS-ACCESS-FEE-FILE.                                  01592
138800                                                                  01593
139900     OPEN INPUT   I-ACCESS-FEE-GROUP-FILE.                        01604
           PERFORM UNTIL NO-MORE-RECORDS
           READ I-ACCESS-FEE-GROUP-FILE  INTO I-ACCESS-FEE-GROUP-RECORD
                 AT END
                    MOVE 'N' TO ARE-THERE-MORE-RECORDS
                 NOT AT END
                   ADD 1         TO  WS-ACCESS-RECORDS
                   MOVE WS-ACCESS-FEE-GROUP
                                 TO AFEE-GROUP (WS-ACCESS-RECORDS)
                   MOVE WS-ACCESS-FEE-SUBDIV
                                 TO AFEE-SUBDIV (WS-ACCESS-RECORDS)
             END-READ
           END-PERFORM.
      *
139900     CLOSE  I-ACCESS-FEE-GROUP-FILE.                              01604
138900     EJECT                                                        01594
139000 A-1100-PROCESS-PROD-DATE-FILE.                                   01595
139100                                                                  01596
139200*************************************************************     01597
139300*  PURPOSE:                                                 *     01598
139400*         OPEN AND READ THE ICHIS PRODUCTION DATE FILE      *     01599
139500*         CREATE HEADER RECORD FOR O/P FILE.                *     01600
139600*         CLOSE THE FILE.                                   *     01601
139700*************************************************************     01602
139800     SKIP1                                                        01603
139900     OPEN INPUT   I-HRD88DTE-DATE-FILE.                           01604
140000     SKIP1                                                        01605
140100     READ I-HRD88DTE-DATE-FILE                                    01606
140200          INTO HR-COMMON-PROD-DATE-RECORD.                        01607
140300     SKIP1                                                        01608
140400     MOVE HR-PROD-DATE-GREGORIAN-MO    TO CURRENT-MONTH           01609
140500                                          WS-PAID-MO-ICHIS        01610
140600                                          WS-PAID-MO-CSS          01611
140600                                          WS-PAID-DRUG-MO-CSS     01612
140700                                          CURRENT-MM              01613
140800                                          WS-MONTH.               01614
140900     SKIP1                                                        01615
141000     MOVE TBL-MONTH-NAME(WS-MONTH)     TO H3-CURR-MONTH.          01616
141100     SKIP1                                                        01617
141200     MOVE HR-PROD-DATE-GREGORIAN-DAY   TO CURRENT-DAY.            01618
141300     SKIP1                                                        01619
141200     MOVE CURRENT-DAY           TO GME-CURRENT-DAY.               01620
141300     SKIP1                                                        01621
141400     MOVE HR-PROD-DATE-GREGORIAN-YR    TO CURRENT-YEAR            01622
141500                                          WS-PAID-YR-ICHIS        01623
141600                                          WS-PAID-YR-CSS          01624
141600                                          WS-PAID-DRUG-YR-CSS     01625
C21TSR                                          CURRENT-YY H3-YEAR      01626
                                                                        01627
C21TSR     CALL  C2196M02  USING H3-YEAR  H3-YEAR  C21-BASE-OFF         01629
C21TSR               C21-WORK-AREA.                                     01630
                                                                        01631
141800     SKIP1                                                        01632
141900     IF SPLIT-AFTER-JUNE3091                                      01633
142000        MOVE WS-LITERAL-1   TO WS-SPLIT-RECORD-SWITCH.            01634
142100     SKIP1                                                        01635
142200     PERFORM A-1150-OUTPUT-HEADER-RTN.                            01639
142300     SKIP1                                                        01640
142400     CLOSE I-HRD88DTE-DATE-FILE.                                  01641
142500                                                                  01642
142600     EJECT                                                        01643
142700 A-1150-OUTPUT-HEADER-RTN.                                        01644
142800                                                                  01645
142900******************************************************************01646
143000*    PURPOSE :                                                   *01647
143100*             FORMAT VALUES ON THE HEADER RECORD AND             *01648
143200*             WRITE HEADER RECORD ON THE EXTRACT FILE.           *01649
143300******************************************************************01650
143400                                                                  01651
143500      MOVE  LOW-VALUES    TO   SEL-PAID-CLAIMS-RECORD.            01652
143600      MOVE  LOW-VALUES    TO   SEL-HEADER-RECORD-IDENT            01653
143700                               SEL-PAID-CLAIMS-HEADER-RECORD.     01654
143800     SKIP1                                                        01655
143900      MOVE  WS-HEADER     TO   SEL-HEADER-RECORD-INDICATOR.       01656
144000     SKIP1                                                        01657
144100      MOVE  CURRENT-YEAR    TO   SEL-HEADER-FILE-PAID-YEAR.       01658
144200     SKIP1                                                        01659
144300      MOVE  CURRENT-MONTH   TO   SEL-HEADER-FILE-PAID-MONTH.      01660
144400     SKIP1                                                        01661
144500      PERFORM X-3000-WRITE-RTN.                                   01662
144600     EJECT                                                        01663
144700 A-1400-INITIALIZE-COUNTS-RTN.                                    01664
144800                                                                  01665
144900******************************************************************01666
145000*    PURPOSE :                                                   *01667
145100*             INITIALIZE NUMERIC WORKING STORAGE AREAS           *01668
145200*             WHICH ACCUMULATE TOTALS FOR NUMBER OF RECORDS      *01669
145300******************************************************************01670
145400                                                                  01671
145500     MOVE WS-PLUS-ZERO TO   WS-REC-CNTS(REC-INDEX).               01672
145600                                                                  01673
145700     EJECT                                                        01674
145800 A-1500-INITIALIZE-COUNTS-RTN.                                    01675
145900                                                                  01676
146000******************************************************************01677
146100*    PURPOSE :                                                   *01678
146200*             INITIALIZE NUMERIC WORKING STORAGE AREAS           *01679
146300*             WHICH ACCUMULATE TOTALS FOR NUMBER OF CLAIMS       *01680
146400******************************************************************01681
146500                                                                  01682
146600     MOVE WS-PLUS-ZERO TO   WS-CLAIMS-CNTS(CLAIMS-INDEX).         01683
146700     EJECT                                                        01684
146800 A-1600-INITIALIZE-COUNTS-RTN.                                    01685
146900                                                                  01686
147000******************************************************************01687
147100*    PURPOSE :                                                   *01688
147200*             INITIALIZE NUMERIC WORKING STORAGE AREAS           *01689
147300*             WHICH ACCUMULATE TOTALS FOR NUMBER OF SERVICES     *01690
147400******************************************************************01691
147500                                                                  01692
147600     MOVE WS-PLUS-ZERO TO   WS-SERVICES-CNTS(SERVICES-INDEX).     01693
147700     EJECT                                                        01694
147800 A-1700-INITIALIZE-COUNTS-RTN.                                    01695
147900                                                                  01696
148000******************************************************************01697
148100*    PURPOSE :                                                   *01698
148200*             INITIALIZE NUMERIC WORKING STORAGE AREAS           *01699
148300*             WHICH ACCUMULATE TOTALS FOR NUMBER OF DAYS/VISITS  *01700
148400******************************************************************01701
148500                                                                  01702
148600     MOVE WS-PLUS-ZERO TO   WS-DAYS-CNTS(DAYS-INDEX).             01703
148700     EJECT                                                        01704
148800 A-1800-INITIALIZE-COUNTS-RTN.                                    01705
148900                                                                  01706
149000******************************************************************01707
149100*    PURPOSE :                                                   *01708
149200*             INITIALIZE NUMERIC WORKING STORAGE AREAS           *01709
149300*             WHICH ACCUMULATE TOTALS FOR AMOUNT PAID (LIABILITY)*01710
149400******************************************************************01711
149500                                                                  01712
149600     MOVE WS-PLUS-ZERO TO   WS-LOB-LIAB-AMTS(LIAB-INDEX).         01713
149700     EJECT                                                        01714
149800 A-1900-INIT-MS-PAYMENT-TABLE.                                    01715
149900                                                                  01716
150000******************************************************************01717
150100*    PURPOSE :                                                   *01718
150200*             INITIALIZE MS-PAYMENT-TABLE AREA                   *01719
150300*             WHICH ACCUMULATE TOTALS FOR WS-PAYMENT-AMOUN       *01720
150400******************************************************************01721
150500                                                                  01722
150600     MOVE SPACES        TO MS-COV-TYPE-LVL(MS-IX-1).              01723
150700     MOVE ZEROES        TO MS-PKG-OR-RIDER-NO(MS-IX-1).           01724
150800     MOVE ZEROES        TO MS-PAID-UNITS(MS-IX-1).                01725
150900     MOVE ZEROES        TO MS-APPROVED-AMT(MS-IX-1).              01726
151000     MOVE ZEROES        TO MS-DEDUCTED-AMT (MS-IX-1).             01727
151100     MOVE ZEROES        TO MS-COB-LVL(MS-IX-1).                   01728
151200     MOVE ZEROES        TO MS-COIN-AMT(MS-IX-1).                  01729
151300     EJECT                                                        01730
151400 A-1950-INIT-MS-CO-PAYMENT.                                       01731
151500                                                                  01732
151600******************************************************************01733
151700*    PURPOSE :                                                   *01734
151800*             INITIALIZE MS-PAYMENT-TABLE AREA                   *01735
151900*             WHICH ACCUMULATE TOTALS FOR WS-PAYMENT-AMOUN       *01736
152000******************************************************************01737
152100                                                                  01738
152200     MOVE ZEROES       TO   MS-CO-PAY-AMT(MS-IX-2).               01739
152300     EJECT                                                        01740
152400 B-1000-PROCESS-HOSPITAL-FILE.                                    01741
152500******************************************************************01742
152600*    PURPOSE :                                                   *01743
152700*             INITIATE ROUTINE ACTIVITIES FOR PROCESSING AND     *01744
152800*             BALANCING THE MONTHLY INSTITUTIONAL CLAIMS FILE    *01745
152900******************************************************************01746
153000     SKIP3                                                        01747
153100     PERFORM B-1100-OPEN-PROCESS-CLOSE.                           01748
153200     EJECT                                                        01749
153300 B-1100-OPEN-PROCESS-CLOSE.                                       01750
153400     SKIP3                                                        01751
153500     OPEN INPUT  I-COMBINED-PAYMENTS-FILE.                        01752
153600     SKIP3                                                        01753
153700     PERFORM B-1110-READ-HOSP-FIRST-TIME.                         01754
153800     SKIP3                                                        01755
153900     PERFORM B-1135-HOSP-DATE-CHECK-RTN.                          01756
154000     SKIP3                                                        01757
154100     PERFORM B-1130-READ-HOSP-FILE-RTN                            01758
154200        UNTIL WS-EOF-HOSP.                                        01759
154300     SKIP3                                                        01760
154400     PERFORM B-1500-BAL-HOSPITAL-FILE.                            01761
154500     SKIP3                                                        01762
154600     CLOSE I-COMBINED-PAYMENTS-FILE.                              01763
154700     EJECT                                                        01764
154800 B-1110-READ-HOSP-FIRST-TIME.                                     01765
154900******************************************************************01766
155000*    PURPOSE :                                                   *01767
155100*             READ 1 RECORD FROM THE INSTITUTIONAL FILE TO       *01768
155200*             CHECK PAID DATE ON RECORD                          *01769
155300******************************************************************01770
155400     SKIP3                                                        01771
155500     READ I-COMBINED-PAYMENTS-FILE INTO                           01772
155600            HRCP-COMBINED-PAYMENTS-RECORD AT END                  01773
155700                 MOVE WS-ZERO TO WS-EOF-HOSP-FILE.                01774
155800     SKIP3                                                        01775
155900     IF WS-EOF-HOSP                                               01776
156000         MOVE WS-ABEND-CODE(1) TO USER-ABEND-CODE                 01777
156100         DISPLAY  WS-DISPLAY-1                                    01778
156200         DISPLAY WS-ERROR-MSG-TBL(1)                              01779
156300         DISPLAY USER-ABEND-CODE                                  01780
156400         DISPLAY  WS-DISPLAY-1                                    01781
156500         DISPLAY WS-DISPLAY-1                                     01782
156600         PERFORM Z-CALL-BOMBER.                                   01783
156700     EJECT                                                        01784
156800 B-1130-READ-HOSP-FILE-RTN.                                       01785
156900     SKIP2                                                        01786
157000***************************************************************   01787
157100*    PURPOSE :                                                *   01788
157200*             READS THE HOSPITAL FILE AND ACCUMULATES         *   01789
157300*             RECORD COUNTS, CLAIMS COUNT DAYS COUNT AND      *   01790
157400*             AHS LIABILITY AMOUNTS.                          *   01791
157500***************************************************************   01792
157600     SKIP2                                                        01793
157700     IF HRCP-TRAILER-CONSTANT                                     01794
157800        PERFORM B-1135-HOSP-DATE-CHECK-RTN                        01795
157900        ADD  HRCP-FILE-PAYMENT-CTR     TO WS-HOSP-TRLR-RECS       01796
158000        ADD  HRCP-FILE-TOTAL-LIAB      TO WS-HOSP-TRLR-LIAB       01797
158100      ELSE                                                        01798
158200        PERFORM B-1135-HOSP-DATE-CHECK-RTN                        01799
158300        PERFORM B-1200-ACCUM-IP-REC-RTN                           01800
158400        PERFORM B-1300-HOSP-SEL-PD-CLM-RECORD                     01801
158500        IF NOT WS-BYPASS                                          01802
158600            PERFORM B-1405-HOSP-OUTPUT-DETAIL-REC                 01803
158600            PERFORM X-3000-WRITE-RTN                              01804
158500        ELSE                                                      01805
158600            PERFORM B-1405-HOSP-OUTPUT-DETAIL-REC                 01806
158600            MOVE    ECKS TO SEL-AUDIT-IND                         01807
158600            PERFORM X-3000-WRITE-RTN.                             01808
158700     SKIP3                                                        01809
158800     MOVE WS-ZERO             TO WS-BYPASS-IND.                   01810
158900     SKIP3                                                        01811
159000     READ I-COMBINED-PAYMENTS-FILE INTO                           01812
159100            HRCP-COMBINED-PAYMENTS-RECORD AT END                  01813
159200                 MOVE WS-ZERO TO WS-EOF-HOSP-FILE.                01814
                                                                        01815
159300     EJECT                                                        01816
159400 B-1135-HOSP-DATE-CHECK-RTN.                                      01817
159500******************************************************************01818
159600*    PURPOSE :                                                   *01819
159700*             MATCH THE RECORD PAYMENT DATE TO THE DATE FIELDS   *01820
159800*             FOR THE CURRENT PRODUCTION CYCLE.                  *01821
159900******************************************************************01822
160000     SKIP3                                                        01823
160100      IF HRCP-ACTN-YR NOT EQUAL CURRENT-YEAR                      01824
160200       OR HRCP-ACTN-MO NOT EQUAL CURRENT-MONTH                    01825
160300          MOVE WS-ABEND-CODE(2) TO USER-ABEND-CODE                01826
160400          DISPLAY USER-ABEND-CODE                                 01827
160500          DISPLAY WS-ERROR-MSG-TBL(3)                             01828
160600          DISPLAY WS-ERROR-MSG-TBL(7) CURRENT-YEAR CURRENT-MONTH  01829
160700          DISPLAY WS-ERROR-MSG-TBL(8) HRCP-ACTN-YR HRCP-ACTN-MO   01830
160800          PERFORM Z-CALL-BOMBER.                                  01831
160900                                                                  01832
161000     EJECT                                                        01833
161100 B-1200-ACCUM-IP-REC-RTN.                                         01834
161200     SKIP2                                                        01835
161300***************************************************************   01836
161400*    PURPOSE :                                                *   01837
161500*             ACCUMULATE TOTALS FOR:                          *   01838
161600*             RECORD COUNTS, CLAIM COUNTS, DAYS COUNTS AND    *   01839
161700*             AHS LIABILITY AMOUNTS.                          *   01840
161800***************************************************************   01841
161900     SKIP2                                                        01842
           IF HRCP-GME-PROCESS                                          01843
              ADD WS-PLUS-ONE         TO WS-GME-RECORDS-IN              01844
              ADD HRCP-AHS-PAYMT      TO WS-GME-LIABILITY-IN.           01845
161900     SKIP2                                                        01846
162000     ADD WS-POSITIVE-ONE TO      WS-REC-CNTS(1).                  01847
162100     SKIP2                                                        01848
162200     ADD HRCP-AHS-PAYMT TO   WS-LOB-LIAB-AMTS(1).                 01849
162300     SKIP2                                                        01850
162400     ADD HRCP-CASE-CTR  TO   WS-CLAIMS-CNTS(1).                   01851
162500     SKIP2                                                        01852
162600     ADD HRCP-FULL-DAYS  HRCP-DISCOUNT-DAYS TO                    01853
162700                         WS-DAYS-CNTS(1).                         01854
162800     EJECT                                                        01855
162900 B-1300-HOSP-SEL-PD-CLM-RECORD.                                   01856
163000                                                                  01857
163100***************************************************************   01858
163200*    PURPOSE :                                                *   01859
163300*                                                             *   01860
163400*      BYPASS INPUT RECORDS FROM FURTHER PROCESSING AND       *   01861
163500*      ALSO ACCUMULATE TOTALS FOR BYPASS RECORDS.             *   01862
163600*                                                             *   01863
163700***************************************************************   01864
163800     SKIP2                                                        01865
163900     IF HRCP-AHS-PAYMT EQUAL TO WS-PLUS-ZERO                      01866
164000        PERFORM B-1305-CHECK-HRA-HSA-AMOUNTS                      01867
164000        PERFORM B-1305-CHECK-HOSP-BYPASS.                         01867
164100***     MOVE WS-ONE        TO   WS-BYPASS-IND.                    01868
164200     SKIP2                                                        01869
164300     IF WS-BYPASS                                                 01870
164400        ADD WS-POSITIVE-ONE TO  WS-REC-CNTS(11)                   01871
164500        ADD HRCP-CASE-CTR  TO   WS-CLAIMS-CNTS(11)                01872
164600        ADD HRCP-AHS-PAYMT TO   WS-LOB-LIAB-AMTS(11)              01873
164700        ADD HRCP-FULL-DAYS  HRCP-DISCOUNT-DAYS TO                 01874
164800                                WS-DAYS-CNTS(11).                 01875
164900     EJECT                                                        01876
164000 B-1305-CHECK-HRA-HSA-AMOUNTS.                                    01867
164000        IF HRCP-CDHP-HRA-AMOUNT NOT NUMERIC                       01867
164000           MOVE ZEROS       TO HRCP-CDHP-HRA-AMOUNT               01867
164000        END-IF.                                                   01867
164000        IF HRCP-CDHP-HSA-AMOUNT NOT NUMERIC                       01867
164000           MOVE ZEROS       TO HRCP-CDHP-HSA-AMOUNT               01867
164000        END-IF.                                                   01867
165000 B-1305-CHECK-HOSP-BYPASS.                                        01877
165100*-    CR#108219 CHANGES START                                     01878
165100*-    THE BELOW LOGIC IS ADDED TO NOT SET THE AUDIT-IND TO '4'    01878
165100*-    & TO NOT TO SET RECORD-TYPE TO '6' FOR CDHP 2ND CLAIMS      01878
165100*-    ON VSAM CLAIM HISTORY FILE                                  01878
165100*-    SO THAT THESE CLAIMS SHOULD NOT BE CONSIDERED AS REJECTED   01878
165100     IF HRCP-NON-POOL-PAYMT     =   WS-PLUS-ZERO  AND             01878
165200        HRCP-FLEX-DEDUCT-AMT    =   WS-PLUS-ZERO  AND             01879
165300        HRCP-FLEX-CO-PAY-AMT    =   WS-PLUS-ZERO  AND             01880
165400        HRCP-FLEX-CO-INSUR-AMT     =   WS-PLUS-ZERO  AND          01881
165500        HRCP-OTHER-DEDUCTIBLE-AMT  =   WS-PLUS-ZERO  AND          01882
027210        HRCP-CAP-PAY-VENDOR-AMT     =   WS-PLUS-ZERO  AND         02676
165600        HRCP-TOT-HOSP-BILL         =   WS-PLUS-ZERO  AND          01883
165700        HRCP-TOT-CHGD-TO-PAT       =   WS-PLUS-ZERO AND           01884
165800        HRCP-CORE-PROCESS-IND      = 'CP'                         01885
165800             CONTINUE   ELSE                                      01885
165100*-    CR#108219 CHANGES END                                       01878
165100     IF HRCP-NON-POOL-PAYMT     =   WS-PLUS-ZERO  AND             01878
165200        HRCP-FLEX-DEDUCT-AMT    =   WS-PLUS-ZERO  AND             01879
165300        HRCP-FLEX-CO-PAY-AMT    =   WS-PLUS-ZERO  AND             01880
165400        HRCP-FLEX-CO-INSUR-AMT     =   WS-PLUS-ZERO  AND          01881
165500        HRCP-OTHER-DEDUCTIBLE-AMT  =   WS-PLUS-ZERO  AND          01882
027210        HRCP-CAP-PAY-VENDOR-AMT     =   WS-PLUS-ZERO  AND         02676
165600        HRCP-TOT-HOSP-BILL         =   WS-PLUS-ZERO  AND          01883
165600        HRCP-CDHP-HRA-AMOUNT       =   WS-PLUS-ZERO  AND          01883
165600        HRCP-CDHP-HSA-AMOUNT       =   WS-PLUS-ZERO  AND          01883
165700        HRCP-TOT-CHGD-TO-PAT       =   WS-PLUS-ZERO               01884
165800        MOVE WS-ONE                TO  WS-BYPASS-IND.             01885
165900 B-1405-HOSP-OUTPUT-DETAIL-REC.                                   01886
166000***************************************************************   01887
166100*    PURPOSE :                                                *   01888
166200*             PREPARE TO WRITE THE OUTPUT DETAIL RECORD       *   01889
166300***************************************************************   01890
166400     PERFORM B-1410-ACCUM-OUTPUT-COUNTS.                          01891
166500     PERFORM B-1420-FORMAT-FIELDS.                                01892
166600     PERFORM B-1430-ATTACH-INDICATORS.                            01893
166800     EJECT                                                        01894
166900 B-1410-ACCUM-OUTPUT-COUNTS.                                      01895
167000***************************************************************   01896
167100*    PURPOSE :                                                *   01897
167200*             ACCUMULATE TOTALS ON FIELDS IN THE DETAIL RECORD*   01898
167300***************************************************************   01899
167400     SKIP3                                                        01900
167500         ADD WS-POSITIVE-ONE        TO  WS-REC-CNTS(6)            01901
167600                                        WS-REC-CNTS(16)           01902
167700         ADD HRCP-AHS-PAYMT         TO  WS-LOB-LIAB-AMTS(6)       01903
167800                                        WS-LOB-LIAB-AMTS(16)      01904
167900         ADD HRCP-CASE-CTR          TO  WS-CLAIMS-CNTS(6)         01905
168000                                        WS-CLAIMS-CNTS(16)        01906
168100         ADD HRCP-FULL-DAYS HRCP-DISCOUNT-DAYS TO                 01907
168200                                        WS-DAYS-CNTS(6)           01908
168300                                        WS-DAYS-CNTS(16).         01909
168400     EJECT                                                        01910
168500 B-1420-FORMAT-FIELDS.                                            01911
168600***************************************************************   01912
168700*    PURPOSE :                                                *   01913
168800*             SET UP THE ATTRIBUTES FOR THE DETAIL RECORD     *   01914
168900***************************************************************   01915
169000     SKIP3                                                        01916
143500     MOVE LOW-VALUES  TO  SEL-PAID-CLAIMS-RECORD.                 01917
169100     IF  NOT HRCP-OUT-PATIENT-ACCOM                               01918
169200         MOVE LOW-VALUES             TO SEL-TYPE-OF-SERVICE       01919
169300     ELSE                                                         01920
169400        MOVE ZEROS                   TO SEL-TYPE-OF-SERVICE.      01921
169500     SKIP3                                                        01922
169600     MOVE WS-DETAIL             TO SEL-DETAIL-RCD-INDICATOR.      01923
169700     MOVE SPACES                TO SEL-DETAIL-RCD-IDENTIFICATION. 01924
169800     MOVE HRCP-ACTN-YR               TO SEL-PAID-YEAR             01925
169800                                        DESTID-YY.                01925
169900     MOVE HRCP-ACTN-MO               TO SEL-PAID-MONTH            01926
169800                                        DESTID-MM.                01925
170000     MOVE HRCP-ACTN-DA               TO SEL-DETAIL-PAID-DAY       01927
169800                                        DESTID-DD.                01925
           IF DESTID-YY    >  56                                        01628
              MOVE   WS-19     TO DESTID-CC                             01628
           ELSE                                                         01628
              MOVE   WS-20     TO DESTID-CC.                            01628
170100     MOVE HRCP-ADM-YR                TO SEL-INCURRED-YEAR.        01928
170200     MOVE HRCP-ADM-MO                TO SEL-INCURRED-MONTH.       01929
170300     MOVE HRCP-ADM-DA                TO SEL-DETAIL-INCURRED-DAY.  01930
170400     MOVE WS-HOSPITAL-LOB-LIT        TO SEL-LINE-OF-BUSINESS.     01931
170500     MOVE HRCP-BC-PLAN-COVERAGE-CODE TO SEL-PLAN-CODE.            01932
170600     MOVE HRCP-GROUPNO               TO SEL-GROUP-NUMBER.         01933
170700     MOVE HRCP-SENIOR-CARE-IND       TO SEL-SENIOR-CARE-IND.      01934
170800     MOVE HRCP-SUB-DIVISN            TO SEL-DETAIL-SUB-DIVISION.  01935
170900     MOVE HRCP-CERT-NO               TO SEL-SUBSCRIBER-ID.        01936
171000     COMPUTE WS-PAID-DAYS = HRCP-FULL-DAYS +                      01937
171100                            HRCP-DISCOUNT-DAYS                    01938
171200     MOVE WS-PAID-DAYS               TO SEL-NUMBER-OF-DAYS-VISITS.01939
171300     MOVE HRCP-CASE-CTR           TO SEL-NUMBER-OF-CLAIMS.        01940
171400     IF (HRCP-CORE-CS90-PROCESS   AND HRCP-CS90-PROCESS)          01941
171400       IF  HRCP-CS90-CLAIM-PACKED NUMERIC                         01942
171500           MOVE HRCP-BR-ID          TO   TEST-BR-ID               01943
                 MOVE HRCP-CASE-ID        TO   TEST-CASE-ID             01944
171500           MOVE HRCP-CS90-CLAIM-PACKED TO TEST-CASENO-LST6        01945
171600           MOVE WS-UNPACK-CLAIM-NUMBER TO SEL-DETAIL-CLAIM-NUMBER 01946
171400       ELSE                                                       01947
171500           MOVE HRCP-BR-ID       TO   HR-BR-ID                    01948
                 MOVE HRCP-CASE-ID     TO   HR-CASE-ID                  01949
171500           MOVE HRCP-CASENO-LST6 TO   HR-CASENO-LST6              01950
171700     ELSE                                                         01951
171800          MOVE HRCP-CORE-SYS-CLAIM-NO TO SEL-DETAIL-CLAIM-NUMBER. 01952
171900     MOVE WS-PLUS-ZERO               TO SEL-NUMBER-OF-SERVICES.   01953
172000     MOVE HRCP-AHS-PAYMT             TO SEL-PAID-CLAIM-LIABILITY. 01954
172100     MOVE HRCP-PATIENT-FIRST-NAME   TO SEL-DETAIL-PATIENT-INITIAL.01955
172200     MOVE SPACES                 TO SEL-DETAIL-PATIENT-LAST-NAME. 01956
172300     MOVE HRCP-PAT-LASTNAME      TO SEL-DETAIL-PATIENT-LAST-NAME. 01957
172400     MOVE SPACE                  TO SEL-DETAIL-MED-SURG-BREAKDOWN 01958
172500     MOVE SPACE                  TO SEL-CSS-SOURCE-CODE.          01959
172600     MOVE SPACE                  TO SEL-LOB-ADJUSTMENT-CODE.      01960
172700     MOVE ZEROES                 TO SEL-ACCOUNT-PAID-DATE.        01961
172800     IF HRCP-BANK-B-CASE                                          01962
172900        MOVE WS-1            TO SEL-BANK-B-INDICATOR              01963
173000     ELSE                                                         01964
173100        MOVE SPACES          TO SEL-BANK-B-INDICATOR.             01965
158500        IF  WS-BYPASS                                             01966
158600            MOVE    ECKS TO SEL-AUDIT-IND                         01967
158500        ELSE                                                      01968
158600            MOVE   SPACES TO SEL-AUDIT-IND.                       01969
173200                                                                  01970
173300     MOVE HRCP-COLLECTION-GRPCDE TO SEL-ICHIS-COLLECTION-CODE.    01971
173400**** ADDED 9/30/91  BY E.VOLYNSKY TO CREATE 349 BYTES RECORD.     01972
173500     IF HRCP-GME-PROCESS                                          01973
                  PERFORM  B-1405-GME-DATE-FIX                          01974
173500            MOVE WS-G                TO SEL-RECORD-TYPE           01975
                  ADD WS-PLUS-ONE          TO WS-GME-RECORDS-OUT        01976
172000            ADD HRCP-AHS-PAYMT       TO WS-GME-LIABILITY-OUT      01977
173500     ELSE                                                         01978
173500            MOVE WS-ONE              TO SEL-RECORD-TYPE.          01979
173600     MOVE HRCP-DISP-CDE              TO HR-DISP-CDE.              01980
173700     MOVE HRCP-MARR-STATUS           TO HR-MARR-STATUS.           01981
173800     MOVE HRCP-TYPE-CONTRACT         TO HR-TYPE-CONTRACT.         01982
173900     MOVE HRCP-TYPE-BUSINESS         TO HR-TYPE-BUSINESS.         01983
174000     MOVE HRCP-COLLECTION-GRPCDE     TO HR-COLLECTION-GRPCDE.     01984
174100     MOVE HRCP-AGE                   TO HR-AGE.                   01985
174200     MOVE HRCP-STILL-IN-IND          TO HR-STILL-IN-IND.          01986
174300     MOVE HRCP-CONTINUATION-IND      TO HR-CONTINUATION-IND.      01987
174400     MOVE HRCP-INDEMNITY-IND         TO HR-INDEMNITY-IND.         01988
174500     MOVE HRCP-ACCOMODATION          TO HR-ACCOMODATION.          01989
174600     MOVE HRCP-LMTD-APPROVAL-IND     TO HR-LMTD-APPROVAL-IND.     01990
174700     IF   HRCP-FULL-DAYS  NUMERIC                                 01991
174800          MOVE HRCP-FULL-DAYS        TO HR-FULL-DAYS              01992
174900     ELSE                                                         01993
175000          MOVE ZEROES                TO HR-FULL-DAYS.             01994
175100     IF   HRCP-DISCOUNT-DAYS NUMERIC                              01995
175200          MOVE HRCP-DISCOUNT-DAYS    TO HR-DISCOUNT-DAYS          01996
175300     ELSE                                                         01997
175400          MOVE ZEROES                TO HR-DISCOUNT-DAYS.         01998
175500     MOVE HRCP-BIRTH-YR              TO  HR-BIRTH-YR.             01999
      ** AS PER JACOB STEINGART'S SUGGESTION THIS MOVE STATEMENT IS
      ** COMMENTED OUT.HE REMOVED HRCP-OP-RATE-TYPE FROM ICHIS COPYBOOK
      ** HRIC2600.AS PER HIS OPINION THIS FIELD IS NOT USED ANYWHERE.
      **THESE FIELDS HAVE BEEN REMOVED TO MAKE ROOM FOR HSA AMOUNT AND
      **CDHP INDICATOR. AS PER CR# 112132.
175600*    MOVE HRCP-OP-RATE-TYPE          TO                           02000
175700*                                    HR-OP-RATE-TYPE.             02001
175800     MOVE HRCP-DRG-CODE-NEW          TO HR-DRG-CODE-NEW.          02002
175900     MOVE HRCP-DRG-STATE-CODE-INDIC                               02003
176000                                     TO HR-DRG-STATE-CODE-INDIC.  02004
176100     MOVE HRCP-DRG-STATE-EXEMPTED-INDIC                           02005
176200                                 TO HR-DRG-STATE-EXEMPTED-INDIC.  02006
176300     MOVE HRCP-DRG-MAJOR-DIAG-CATAGORY                            02007
176400                                  TO HR-DRG-MAJOR-DIAG-CATAGORY.  02008
176500     IF   HRCP-DRG-CODE-INTENSITY-WEIGHT  NUMERIC                 02009
176600          MOVE HRCP-DRG-CODE-INTENSITY-WEIGHT                     02010
176700                                TO HR-DRG-CODE-INTENSITY-WEIGHT   02011
176800     ELSE                                                         02012
176900          MOVE ZEROES        TO HR-DRG-CODE-INTENSITY-WEIGHT.     02013
177000     MOVE HRCP-DRG-CLM-CLASS-INDIC                                02014
177100                                     TO HR-DRG-CLM-CLASS-INDIC.   02015
177200     IF   HRCP-DRG-OL-DAYS NUMERIC                                02016
177300          MOVE HRCP-DRG-OL-DAYS      TO HR-DRG-OL-DAYS            02017
177400     ELSE                                                         02018
177500          MOVE ZEROES                TO HR-DRG-OL-DAYS.           02019
177600     IF HRCP-TOT-HOSP-BILL NUMERIC                                02020
177700        MOVE HRCP-TOT-HOSP-BILL      TO HR-TOT-HOSP-BILL          02021
177800     ELSE                                                         02022
177900        MOVE ZEROES                  TO HR-TOT-HOSP-BILL.         02023
177910*** A FIX BY D.G 11/19/91    HR-TOT-HOSP-BILL.                    02024
178000     IF   HRCP-TOT-CHGD-TO-PAT  NUMERIC                           02025
178100          MOVE HRCP-TOT-CHGD-TO-PAT  TO HR-TOT-CHGD-TO-PAT        02026
178200     ELSE                                                         02027
178300          MOVE ZEROES                TO HR-TOT-CHGD-TO-PAT.       02028
178400     MOVE HRCP-SEX-IND               TO HR-SEX-IND.               02029
178500     MOVE HRCP-SPEC-PROCESS-IND      TO HR-SPEC-PROCESS-IND.      02030
178600     MOVE HRCP-DISCHARGE-DATE        TO HR-DISCHARGE-DATE.        02031
178700     MOVE HRCP-HOSP-CDE              TO HR-HOSP-CDE.              02032
178800     MOVE HRCP-ALBANY-SUPP-IND       TO HR-ALBANY-SUPP-IND.       02033
178900     MOVE HRCP-ECR-IND               TO HR-ECR-IND.               02034
179000     MOVE HRCP-SUPPS-IND             TO HR-SUPPS-IND.             02035
179100     MOVE HRCP-SVCNG-ACCT-IND        TO HR-SVCNG-ACCT-IND.        02036
179200     MOVE HRCP-COBRA-IND             TO HR-COBRA-IND.             02037
179300     MOVE HRCP-ERRCDE                TO HR-ERRCDE.                02038
179400     MOVE HRCP-REFUND-STAT-COB       TO HR-REFUND-STAT-COB.       02039
179500     IF   HRCP-RIDER-PACK-NO NUMERIC                              02040
179600          MOVE  HRCP-RIDER-PACK-NO TO HR-RIDER-PACK-NO            02041
179700     ELSE                                                         02042
179800          MOVE  ZEROES             TO HR-RIDER-PACK-NO.           02043
179900     MOVE HRCP-CASE-CTR              TO HR-CASE-CTR.              02044
180000     MOVE HRCP-CBC-BLOOD-IND         TO HR-CBC-BLOOD-IND.         02045
180100     MOVE HRCP-PATIENT-DIED-IND      TO HR-PATIENT-DIED-IND.      02046
180200     MOVE HRCP-SC-DEDUCT-IND         TO HR-SC-DEDUCT-IND.         02047
180300     MOVE HRCP-SC-ALLOW-IND          TO HR-SC-ALLOW-IND.          02048
180400     MOVE HRCP-PRESSO-IND            TO HR-PRESSO-IND.            02049
180500     MOVE HRCP-ZIP-CODE              TO HR-ZIP-CODE.              02050
180600     MOVE HRCP-ICD9-DIAG-CODE        TO HR-ICD9-DIAG-CODE.        02051
180700     MOVE HRCP-PATIENT-BIRTH-MONTH-DAY                            02052
180800                                   TO HR-PATIENT-BIRTH-MONTH-DAY. 02053
180900     MOVE HRCP-SECONDARY-SUB-ID      TO HR-SECONDARY-SUB-ID.      02054
181000     MOVE HRCP-MAIN-OPERATING-RM-IND                              02055
181100                             TO HR-MAIN-OPERATING-RM-IND.         02056
181200     MOVE HRCP-OP-TREATMENT-CODE     TO HR-OP-TREATMENT-CODE.     02057
181300     MOVE HRCP-DIAG-CLASSIFICATION-CODE                           02058
181400                                  TO HR-DIAG-CLASSIFICATION-CODE. 02059
181500     IF HRCP-NON-POOL-PAYMT     NUMERIC                           02060
181600        MOVE HRCP-NON-POOL-PAYMT     TO HR-NON-POOL-PAYMT         02061
181700     ELSE                                                         02062
181800        MOVE ZEROES                  TO HR-NON-POOL-PAYMT.        02063
181900     MOVE HRCP-FLEX-COST-SHARE-IND   TO HR-FLEX-COST-SHARE-IND.   02064
182000     IF   HRCP-FLEX-DEDUCT-AMT  NUMERIC                           02065
182100          MOVE  HRCP-FLEX-DEDUCT-AMT  TO HR-FLEX-DEDUCT-AMT       02066
182200     ELSE                                                         02067
182300          MOVE  ZEROES                TO HR-FLEX-DEDUCT-AMT.      02068
182400     IF HRCP-FLEX-CO-PAY-AMT   NUMERIC                            02069
182500        MOVE  HRCP-FLEX-CO-PAY-AMT    TO HR-FLEX-CO-PAY-AMT       02070
182600     ELSE                                                         02071
182700        MOVE  ZEROES                  TO HR-FLEX-CO-PAY-AMT.      02072
182800     IF   HRCP-FLEX-CO-INSUR-AMT    NUMERIC                       02073
182900          MOVE HRCP-FLEX-CO-INSUR-AMT   TO HR-FLEX-CO-INSUR-AMT   02074
183000     ELSE                                                         02075
183100          MOVE ZEROES                 TO HR-FLEX-CO-INSUR-AMT.    02076
183200     MOVE HRCP-PRIM-DTE-OF-SURG      TO HR-PRIM-DTE-OF-SURG.      02077
183300     MOVE HRCP-PRIM-PROCEDURE-CODE   TO HR-PRIM-PROCEDURE-CODE.   02078
183400     IF    HRCP-DEDUCTIBLE-AMT  NUMERIC                           02079
183500           MOVE HRCP-DEDUCTIBLE-AMT    TO HR-DEDUCTIBLE-AMT       02080
183600     ELSE                                                         02081
183700           MOVE ZEROES                 TO HR-DEDUCTIBLE-AMT.      02082
183800** A FIX DB D.G 11/19/91  HR-DEDUCTIBLE-AMT.                      02083
183900     IF   HRCP-OTHER-DEDUCTIBLE-AMT  NUMERIC                      02084
184000      MOVE HRCP-OTHER-DEDUCTIBLE-AMT  TO HR-OTHER-DEDUCTIBLE-AMT  02085
184100     ELSE                                                         02086
184200          MOVE ZEROES       TO    HR-OTHER-DEDUCTIBLE-AMT.        02087
184300     MOVE HRCP-CORE-SYS-ORIG-PLAN    TO HR-CORE-SYS-ORIG-PLAN.    02088
184400     MOVE HRCP-CORE-SYS-CLAIM-NO     TO                           02089
184500                           HR-CORE-SYS-CLAIM-NO.                  02090
184600     IF   HRCP-ADJUST-NO  NUMERIC                                 02091
184700          MOVE HRCP-ADJUST-NO  TO  HR-ADJUST-NO                   02092
184800     ELSE                                                         02093
184900          MOVE ZEROES          TO  HR-ADJUST-NO.                  02094
185000     MOVE HRCP-MAJOR-MEDICAL-IND     TO HR-MAJOR-MEDICAL-IND.     02095
185100     MOVE HRCP-CORE-SYS-LOCAL-PROV-ID TO                          02096
185200                                  HR-CORE-SYS-LOCAL-PROV-ID.      02097
185300     MOVE HRCP-MEDICARE-PROV-NO       TO HR-MEDICARE-PROV-NO.     02098
185400*-CR#151788-DEPENDENT NUMBER CHANGE START HERE
183900     MOVE HRCP-DEPENDENT-NUMBER       TO HR-DEPENDENT-NUMBER.     02084
185400*-CR#151788 CHANGE ENDS HERE
185400     IF   HRCP-OTHER-CO-INS-AMOUNT  NUMERIC                       02099
185500         MOVE HRCP-OTHER-CO-INS-AMOUNT TO HR-OTHER-CO-INS-AMOUNT  02100
185600     ELSE                                                         02101
185700       MOVE ZEROES                   TO HR-OTHER-CO-INS-AMOUNT.   02102
185800     MOVE HRCP-PATIENT-FIRST-NAME    TO HR-PATIENT-FIRST-NAME.    02103
185900     MOVE HRCP-OLD-ERG-IND           TO HR-OLD-ERG-IND.           02104
186000     MOVE HRCP-POS-NETWORK-IND       TO HR-POS-NETWORK-IND.       02105
186100     IF   HRCP-PADDE-PATIENT-ID  NUMERIC                          02106
186200          MOVE HRCP-PADDE-PATIENT-ID  TO HR-PADDE-PATIENT-ID      02107
186300     ELSE                                                         02108
186400          MOVE ZEROES                 TO HR-PADDE-PATIENT-ID.     02109
186500     MOVE HRCP-PRIMARY-CARE-PHYSICIAN                             02110
186600                                TO  HR-PRIMARY-CARE-PHYSICIAN.    02111
186700     MOVE  HRCP-DRG-ALT-LEVEL-CARE TO  HR-DRG-ALT-LEVEL-CARE.     02112
186800     IF    HRCP-DRG-ALT-CARE-YYY       NUMERIC                    02113
186900           MOVE  HRCP-DRG-ALT-CARE-YYY                            02114
186900                            TO  HR-DRG-ALT-CARE-YYY               02115
187000     ELSE                                                         02116
187100           MOVE  ZEROES     TO  HR-DRG-ALT-CARE-YYY.              02117
186800     IF    HRCP-DRG-ALT-CARE-DDD       NUMERIC                    02118
186900           MOVE  HRCP-DRG-ALT-CARE-DDD                            02119
186900                            TO  HR-DRG-ALT-CARE-DDD               02120
187000     ELSE                                                         02121
187100           MOVE  ZEROES     TO  HR-DRG-ALT-CARE-DDD.              02122
187200     MOVE  HRCP-DRG-DIAGNOSIS-2    TO  HR-DRG-DIAGNOSIS-2.        02123
187300     MOVE  HRCP-DRG-DIAGNOSIS-3    TO  HR-DRG-DIAGNOSIS-3.        02124
187400     MOVE  HRCP-DRG-DIAGNOSIS-4    TO  HR-DRG-DIAGNOSIS-4.        02125
187500     MOVE  HRCP-DRG-DIAGNOSIS-5    TO  HR-DRG-DIAGNOSIS-5.        02126
187600     MOVE  HRCP-DRG-PROCEDURE-2    TO  HR-DRG-PROCEDURE-2.        02127
187700     MOVE  HRCP-DRG-PROC-DATES-2   TO  HR-DRG-PROC-DATES-2.       02128
187800     MOVE  HRCP-DRG-PROCEDURE-3    TO  HR-DRG-PROCEDURE-3.        02129
187900     MOVE  HRCP-DRG-PROC-DATES-3   TO  HR-DRG-PROC-DATES-3.       02130
188000     MOVE  HRCP-DRG-DISCHARGE-STATUS TO  HR-DRG-DISCHARGE-STATUS. 02131
188100     IF    HRCP-DRG-TYPE-ADMISSION  NUMERIC                       02132
188200        MOVE  HRCP-DRG-TYPE-ADMISSION  TO  HR-DRG-TYPE-ADMISSION  02133
188300     ELSE                                                         02134
188400        MOVE  ZEROES      TO  HR-DRG-TYPE-ADMISSION.              02135
188500     MOVE  HRCP-UBF-ADMIT-DIAG-CODE TO  HR-UBF-ADMIT-DIAG-CODE.   02136
188600     MOVE  HRCP-PMT-VOUCHER-ERROR-CODE                            02137
188700                                   TO  HR-PMT-VOUCHER-ERROR-CODE. 02138
188800     MOVE  HRCP-CORE-PROCESS-IND   TO  HR-CORE-PROCESS-IND.       02139
188900     MOVE  HRCP-TEAM-CARE-REPORTING-IND                           02140
189000                          TO  HR-TEAM-CARE-REPORTING-IND.         02141
189100     MOVE  HRCP-TC-INDICATOR         TO  HR-TC-INDICATOR.         02142
189200     MOVE  HRCP-TC-COINS-MOD         TO  HR-TC-COINS-MOD.         02143
189500**** ADDED 04/30/92 BY E.VOLYNSKY TO CREATE 400 BYTES RECORD.     02144
189300     MOVE  ZEROES                  TO  HRCR-RIDER-CODE.           02145
189400     MOVE  ZEROES                  TO  HRCR-CONVERTED-RIDER-IND.  02146
189400     MOVE  ZEROES                  TO  SEL-HR-SORT-GROUP-NO.      02147
189400     MOVE  WS-NOT                  TO  SEL-HR-CONTROL-IND.        02148
      ***** ADDED NEW FIELDS FOR CONVERSION   **************************02149
      ***** AS OF 01/04/1994               *****************************02150
189300     MOVE  HRCP-EMHC-PROCESSING-IND  TO  HR-EMHC-PROCESSING-IND.  02151
189300     MOVE  HRCP-DIAG-CODE-265        TO  HR-DIAG-CODE-265.        02152
189300     MOVE  HRCP-TC-PENALTY-INDICATOR TO  HR-TC-PENALTY-INDICATOR. 02153
189300     IF    HRCP-TC-PENALTY-CODE   NUMERIC                         02154
189300           MOVE  HRCP-TC-PENALTY-CODE  TO  HR-TC-PENALTY-CODE     02155
189300     ELSE                                                         02156
189300           MOVE  ZEROES                TO  HR-TC-PENALTY-CODE.    02157
189300     MOVE  HRCP-ERG-IND              TO  HR-ERG-IND.              02158
189300     MOVE  HRCP-FINAN-IND            TO  HR-FINAN-IND.            02159
189300     MOVE  HRCP-MATERNITY            TO  HR-MATERNITY.            02160
189300     MOVE  HRCP-HOSPICE-IND          TO  HR-HOSPICE-IND.          02161
189300     MOVE  HRCP-RATE-TYPE-IND        TO  HR-RATE-TYPE-IND.        02162
189400     MOVE  ZEROES                    TO  HR-ERIC-GROUP-NO.        02163
189400     MOVE  SPACES                    TO  HR-ERIC-SUB-DIV.         02164
189300     MOVE  HRCP-REMUN-SAVINGS-CODE-1 TO  HR-REMUN-SAVINGS-CODE-1. 02165
189300     MOVE  HRCP-REMUN-SAVINGS-CODE-2 TO  HR-REMUN-SAVINGS-CODE-2. 02166
189300     MOVE  HRCP-REMUN-SAVINGS-CODE-3 TO  HR-REMUN-SAVINGS-CODE-3. 02167
188100     IF    HRCP-REMUN-COVERED-CHARGES NUMERIC                     02168
189300           MOVE  HRCP-REMUN-COVERED-CHARGES                       02169
189300                                 TO  HR-REMUN-COVERED-CHARGES     02170
188300     ELSE                                                         02171
189300           MOVE  ZEROES          TO  HR-REMUN-COVERED-CHARGES.    02172
188100     IF    HRCP-REMUN-SAVINGS-AMT-1   NUMERIC                     02173
189300           MOVE  HRCP-REMUN-SAVINGS-AMT-1                         02174
189300                                 TO  HR-REMUN-SAVINGS-AMT-1       02175
188300     ELSE                                                         02176
189300           MOVE  ZEROES          TO  HR-REMUN-SAVINGS-AMT-1.      02177
188100     IF    HRCP-REMUN-SAVINGS-AMT-2   NUMERIC                     02178
189300           MOVE  HRCP-REMUN-SAVINGS-AMT-2                         02179
189300                                 TO  HR-REMUN-SAVINGS-AMT-2       02180
188300     ELSE                                                         02181
189300           MOVE  ZEROES          TO  HR-REMUN-SAVINGS-AMT-2.      02182
188100     IF    HRCP-REMUN-SAVINGS-AMT-3   NUMERIC                     02183
189300           MOVE  HRCP-REMUN-SAVINGS-AMT-3                         02184
189300                                 TO  HR-REMUN-SAVINGS-AMT-3       02185
188300     ELSE                                                         02186
189300           MOVE  ZEROES          TO  HR-REMUN-SAVINGS-AMT-3.      02187
188100     IF  HRCP-ADMIN-EXPENSE         NUMERIC                       02188
188200        MOVE HRCP-ADMIN-EXPENSE   TO  HR-ADMIN-EXPENSE            02189
188300     ELSE                                                         02190
188400        MOVE  ZEROES              TO   HR-ADMIN-EXPENSE.          02191
188100     IF  HRCP-ACCESS-FEE            NUMERIC                       02192
189300         MOVE  HRCP-ACCESS-FEE           TO  HR-ACCESS-FEE        02193
188100     ELSE                                                         02194
189300         MOVE  ZEROES                    TO  HR-ACCESS-FEE.       02195
189300     MOVE  HRCP-SC-CROSOVR-IND       TO  HR-SC-CROSOVR-IND.       02196
189300     MOVE  HRCP-PGP                  TO  HR-PGP.                  02197
      ***** ADDED NEW FIELDS *******************************************02198
      ***** AS OF 03/31/1994               *****************************02199
189300     MOVE  HRCP-CHARGE-REGISTER-IND  TO  HR-CHARGE-REGISTER-IND.  02200
189300     MOVE  HRCP-PRODUCT-CODE         TO  HR-PRODUCT-CODE.         02201
189300     MOVE  HRCP-NEGOTD-RATE-IND      TO  HR-NEGOTD-RATE-IND.      02202
189300     IF    HRCP-NEGOTD-NET-SAVINGS-AMT NUMERIC                    02203
189300           MOVE HRCP-NEGOTD-NET-SAVINGS-AMT TO                    02204
189300                HR-NEGOTD-NET-SAVINGS-AMT                         02205
189300     ELSE                                                         02206
189300           MOVE ZEROES TO  HR-NEGOTD-NET-SAVINGS-AMT.             02207
      ***** ADDED NEW FIELDS FOR CONVERSION   **************************02208
      ***** AS OF 05/01/1994               *****************************02209
189300     MOVE  HRCP-REMUN-PAYROLL-LOC    TO  HR-REMUN-PAYROLL-LOC.    02210
      ***** ADDED NEW FIELDS AS OF 8/95       **************************02211
189300     MOVE  HRCP-GROUP-QUARTILE       TO  HR-GROUP-QUARTILE.       02212
189300     MOVE  HRCP-GROUP-TIER           TO  HR-GROUP-TIER.           02213
189300     MOVE  HRCP-GROUP-COUNTY         TO  HR-GROUP-COUNTY.         02214
      ***** ADDED NEW FIELDS AS OF  12/95     **************************02215
189300     MOVE  HRCP-TOPPS-GROUP-NO       TO  HR-TOPPS-GROUP-NO.       02216
189300     MOVE  HRCP-TOPPS-TREATM-TYPE    TO  HR-TOPPS-TREATM-TYPE.    02217
189300     MOVE  HRCP-TOPPS-BENEFIT-PKG    TO  HR-TOPPS-BENEFIT-PKG.    02218
189300     MOVE  HRCP-TOPPS-NETWORK-CHOICE TO  HR-TOPPS-NETWORK-CHOICE. 02219
      ***** ADDED NEW FIELDS AS OF  01/96     **************************02220
189300     MOVE  HRCP-SPC-CHK-PAY-SUBS-IND TO  HR-SPC-CHK-PAY-SUBS-IND. 02221
      ***** ADDED NEW FIELDS AS OF  01/96     **************************02222
189300     MOVE  HRCP-DED-CARRYOVER-IND    TO  HR-DED-CARRYOVER-IND.    02223
      ***** ADDED NEW FIELDS AS OF  01/96     **************************02224
189300     MOVE  HRCP-ORIGINAL-CLAIM-IND   TO  HR-ORIGINAL-CLAIM-IND.   02225
      ***** ADDED NEW FIELDS AS OF  07/96     **************************02226
189300     MOVE  HRCP-ADJUSTM-REASON-CODE  TO HR-ADJUSTM-REASON-CODE.   02227
      ***** ADDED NEW FIELDS AS OF  11/96     **************************02228
189300     MOVE  HRCP-ADM-RCVD-YR              TO HR-ADM-RCVD-YR.       02229
189300     MOVE  HRCP-ADM-RCVD-MO              TO HR-ADM-RCVD-MO.       02230
189300     MOVE  HRCP-ADM-RCVD-DA              TO HR-ADM-RCVD-DA.       02231
189300     MOVE  HRCP-CHGBK-TO-AHS-IND         TO HR-CHGBK-TO-AHS-IND.  02232
189300     IF  HRCP-CORE-SYS-FIN-DAYS    NUMERIC                        02233
189300         MOVE  HRCP-CORE-SYS-FIN-DAYS    TO HR-CORE-SYS-FIN-DAYS  02234
189300     ELSE                                                         02235
189300         MOVE  ZEROES                    TO HR-CORE-SYS-FIN-DAYS. 02236
189300     IF  HRCP-FINANCIAL-CASES      NUMERIC                        02237
189300         MOVE  HRCP-FINANCIAL-CASES      TO HR-FINANCIAL-CASES    02238
189300     ELSE                                                         02239
189300         MOVE  ZEROES                    TO HR-FINANCIAL-CASES.   02240
189300     MOVE  HRCP-DIAG-CDE                 TO HR-DIAG-CDE.          02241
           MOVE  HRCP-NEW-BORN-CODE            TO HR-NEW-BORN-CODE.     02242
039300     MOVE  HRCP-PLS-PROG-CD         TO HR-PLS-PROG-CD.            02243
039300     MOVE  HRCP-PLS-UWRT-CORP-CD    TO HR-PLS-UWRT-CORP-CD.       02244
039300     MOVE  HRCP-PLS-LOB-CD          TO HR-PLS-LOB-CD.             02245
039300     MOVE  HRCP-PLS-CLS-RISK-CD     TO HR-PLS-CLS-RISK-CD.        02246
039300     MOVE  HRCP-PLS-RISK-POOL-CD    TO HR-PLS-RISK-POOL-CD.       02247
039300     MOVE  HRCP-PLS-CONTRACT-TYP-CD TO HR-PLS-CONTRACT-TYP-CD.    02248
039300     IF    HRCP-PLS-CONTRACT-NO  NUMERIC                          02249
039300           MOVE  HRCP-PLS-CONTRACT-NO  TO HR-PLS-CONTRACT-NO      02250
039300     ELSE                                                         02251
039300           MOVE  ZEROES             TO HR-PLS-CONTRACT-NO.        02252
039300     MOVE  HRCP-PLS-COV-TYP-CD      TO HR-PLS-COV-TYP-CD.         02253
039300     MOVE  HRCP-PLS-CATEG-NO        TO HR-PLS-CATEG-NO.           02254
039300     MOVE  HRCP-PLS-BENE-LEV-NO     TO HR-PLS-BENE-LEV-NO.        02255
039300     MOVE  HRCP-PLS-REP-CODE        TO HR-PLS-REP-CODE.           02256
039300     MOVE  HRCP-PLS-PROD-COMB-CODE  TO HR-PLS-PROD-COMB-CODE.     02257
           MOVE  LOW-VALUES               TO HR-PLS-MARKET-SEGMENT.     02258
RS1298***** ADDED NEW FIELDS AS OF  12/98     **************************02259
           MOVE  HRCP-AGENCY-NO           TO HR-PLS-AGENCY-NUMBER.      02264
           MOVE  HRCP-PLS-GRP-KEY-ID      TO HR-PLS-GRP-KEY-ID.         02265
      ***** ADDED NEW FIELDS AS OF  12/96     **************************02266
039300     IF    HRCP-DRG-IL-POOL-A-PYMT  NUMERIC                       02267
039300          MOVE  HRCP-DRG-IL-POOL-A-PYMT TO HR-DRG-IL-POOL-A-PYMT  02268
039300     ELSE                                                         02269
039300          MOVE  ZEROES           TO HR-DRG-IL-POOL-A-PYMT.        02270
039300     IF    HRCP-DRG-OL-POOL-A-PYMT  NUMERIC                       02271
039300           MOVE  HRCP-DRG-OL-POOL-A-PYMT  TO HR-DRG-OL-POOL-A-PYMT02272
039300     ELSE                                                         02273
039300           MOVE  ZEROES            TO HR-DRG-OL-POOL-A-PYMT.      02274
039300     IF    HRCP-DRG-IL-POOL-B-PYMT     NUMERIC                    02275
039300           MOVE  HRCP-DRG-IL-POOL-B-PYMT                          02276
039300                                   TO HR-DRG-IL-POOL-B-PYMT       02277
039300     ELSE                                                         02278
039300           MOVE  ZEROES            TO HR-DRG-IL-POOL-B-PYMT.      02279
039300     MOVE  HRCP-POOL-REGION-CODE    TO WS-POOL-REGION-CODE.       02280
039300     MOVE  WS-POOL-REGION-CODE-2    TO HR-POOL-REGION-CODE.       02281
039300     MOVE  HRCP-GME-GROUP-IND       TO HR-GME-GROUP-IND.          02282
039300     IF    HRCP-GME-SUBSCRIBER-COUNT NUMERIC                      02283
039300           MOVE  HRCP-GME-SUBSCRIBER-COUNT                        02284
039300                                     TO HR-GME-SUBSCRIBER-COUNT   02285
039300     ELSE                                                         02286
039300           MOVE ZEROES               TO HR-GME-SUBSCRIBER-COUNT.  02287
039300     MOVE  HRCP-BLUE-SHIELD-PLAN-CODE TO                          02288
039300                              HR-BLUE-SHIELD-PLAN-CODE.           02289
      ***** ADDED NEW FIELDS AS OF  05/97     **************************02290
039300     IF    HRCP-DRG-IL-POOL-C-PYMT  NUMERIC                       02291
039300          MOVE  HRCP-DRG-IL-POOL-C-PYMT TO HR-DRG-IL-POOL-C-PYMT  02292
039300     ELSE                                                         02293
039300          MOVE  ZEROES           TO HR-DRG-IL-POOL-C-PYMT.        02294
039300     IF    HRCP-DRG-IL-POOL-D-PYMT  NUMERIC                       02295
039300           MOVE  HRCP-DRG-IL-POOL-D-PYMT  TO HR-DRG-IL-POOL-D-PYMT02296
039300     ELSE                                                         02297
039300           MOVE  ZEROES            TO HR-DRG-IL-POOL-D-PYMT.      02298
039300     MOVE  HRCP-CAPITATION-IND     TO HR-CAPITATION-IND.          02299
039300******    ADDED 9/97         **********************************   02300
039300     MOVE  HRCP-MSK-CATEGORY       TO HR-MSK-CATEGORY.            02301
039300     MOVE  HRCP-ACCTG-REPORT-IND   TO HR-ACCTG-REPORT-IND.        02302
039300     MOVE  HRCP-BYPASS-SUPP-AND-POOL-IND                          02303
039300                          TO HR-BYPASS-SUPP-AND-POOL-IND.         02304
           IF DESTID-DATE  > WS-DEST-START-DATE
              MOVE HRCP-DEST-ID    TO HR-DEST-ID
           ELSE
039300        MOVE  HRCP-TOPPS-PROGRAM-NUM    TO HR-TOPPS-PROGRAM-NUM   02305
039300        MOVE  HRCP-TOPPS-SUB-PROG-NUM   TO HR-TOPPS-SUB-PROG-NUM. 02307
039300     IF    HRCP-MCARE-INTEREST-AMT NUMERIC                        02309
039300           MOVE  HRCP-MCARE-INTEREST-AMT                          02310
039300                          TO HR-MCARE-INTEREST-AMT                02311
039300     ELSE                                                         02312
039300           MOVE ZEROES    TO HR-MCARE-INTEREST-AMT.               02313
039300     MOVE  HRCP-TOPPS-MARKET-SEGMENT                              02314
039300                          TO HR-TOPPS-MARKET-SEGMENT.             02315
              PERFORM B-1440-CHECK-OUTPUT-FIELDS.                       02316
      ***** ADDED NEW FIELDS AS OF  01/98     **************************02317
039300     IF    HRCP-DRG-OL-POOL-B-PYMT  NUMERIC                       02318
039300          MOVE  HRCP-DRG-OL-POOL-B-PYMT TO HR-DRG-OL-POOL-B-PYMT  02319
039300     ELSE                                                         02320
039300          MOVE  ZEROES           TO HR-DRG-OL-POOL-B-PYMT.        02321
039300     IF    HRCP-DRG-OL-POOL-C-PYMT  NUMERIC                       02322
039300          MOVE  HRCP-DRG-OL-POOL-C-PYMT TO HR-DRG-OL-POOL-C-PYMT  02323
039300     ELSE                                                         02324
039300          MOVE  ZEROES           TO HR-DRG-OL-POOL-C-PYMT.        02325
039300     IF    HRCP-DRG-OL-POOL-D-PYMT  NUMERIC                       02326
039300          MOVE  HRCP-DRG-OL-POOL-D-PYMT TO HR-DRG-OL-POOL-D-PYMT  02327
039300     ELSE                                                         02328
039300          MOVE  ZEROES           TO HR-DRG-OL-POOL-D-PYMT.        02329
039300     MOVE  HRCP-SCHEDULE-IND      TO HR-SCHEDULE-IND.             02330
            IF   HRCP-NO-DAYS-PROMPT-PAY-INT   NUMERIC                  02331
                 MOVE HRCP-NO-DAYS-PROMPT-PAY-INT   TO                  02332
                                 HR-NO-DAYS-PROMPT-PAY-INT              02333
            ELSE                                                        02334
                 MOVE ZEROES TO HR-NO-DAYS-PROMPT-PAY-INT.              02335
            MOVE HRCP-CLK-START-DAT-PROM-PAY TO                         02336
                 HR-CLK-START-DAT-PROM-PAY.                             02337
            MOVE HRCP-EMHC-IND  TO  HR-EMHC-IND.                        02338
            IF   HRCP-OTHER-STATE-SURCHARGE    NUMERIC                  02339
                 MOVE HRCP-OTHER-STATE-SURCHARGE    TO                  02340
                                 HR-OTHER-STATE-SURCHARGE               02341
            ELSE                                                        02342
                 MOVE ZEROES TO HR-OTHER-STATE-SURCHARGE.               02343
            MOVE HRCP-SOURCE-CODE TO   HR-SOURCE-CODE.                  02344
            MOVE HRCP-SPC-CHK-IND TO   HR-SPC-CHK-IND.                  02345
            MOVE HRCP-PAY-TO-PROVIDER-IND  TO                           02346
                                 HR-PAY-TO-PROVIDER-IND.                02347
            MOVE HRCP-ITS-SCCF-SERIAL-NUMBER TO                         02348
                                 HR-ITS-SCCF-SERIAL-NUMBER.             02349
            MOVE HRCP-NASCO-BANK-ACCT-TYPE   TO                         02350
                                 HR-NASCO-BANK-ACCT-TYPE.               02351
            MOVE HRCP-PRODUCT-CLASSIF-CODE    TO                        02352
                                      HR-PRODUCT-CLASSIF-CODE.          02353
            MOVE HRCP-INTEREST-IND            TO                        02354
                                      HR-INTEREST-IND.                  02355
      ***** ADDED NEW FIELDS AS OF  03/00  BY E.V.  ********************02317
            MOVE HRCP-HOSP-CODE-1-6           TO                        02354
                                      HR-HOSP-CODE-1-6.                 02355
            IF   HRCP-PROVIDER-TAX-ID  NUMERIC                          02354
                 MOVE HRCP-PROVIDER-TAX-ID         TO                   02354
                                      HR-PROVIDER-TAX-ID                02355
            ELSE                                                        02354
                 MOVE ZEROES      TO  HR-PROVIDER-TAX-ID.               02354
      ***** ADDED NEW FIELDS AS OF  06/00  BY E.V.  ********************02317
145500     MOVE HRCP-ITSHOME-ECRP-IND TO  HR-ITSHOME-ECRP-IND.          01672
      ***** ADDED NEW FIELDS AS OF  10/00  BY E.V.  ********************02317
145500       MOVE HRCP-PRODUCT-VARIATION-CODE    TO                     01672
145500                                        HR-PRODUCT-VARIATION-CODE.01672
145500     IF HRCP-CAP-PAY-VENDOR-AMT NUMERIC                           01672
145500        MOVE HRCP-CAP-PAY-VENDOR-AMT    TO HR-CAP-PAY-VENDOR-AMT  01672
145500                                           HR-MAGELLAN-RATE       01672
145500     ELSE                                                         01672
145500        MOVE WS-PLUS-ZERO               TO  HR-CAP-PAY-VENDOR-AMT 01672
145500                                            HR-MAGELLAN-RATE.     01672
      ***** ADDED NEW SRS FIELDS AS OF  10/00  BY E.V.  ****************02317
           MOVE HRCP-PROFITABILITY-CODE        TO                       00634
                                               HR-PROFITABILITY-CODE.   00634
           MOVE HRCP-HI-PROFITABILITY-CODE     TO                       00634
                                               HR-HI-PROFITABILITY-CODE.00634
           MOVE HRCP-BOOK-OF-BUSINESS          TO                       00634
                                               HR-BOOK-OF-BUSINESS.     00634
           MOVE HRCP-PACKAGE-NO         TO    HR-PACKAGE-NO.            00634
           MOVE HRCP-COLLECTION-GRPCDE-OLD   TO                         00634
                                        HR-COLLECTION-GRPCDE-OLD.       00634
           MOVE HRCP-PLS-UWRT-CORP-CD-OLD    TO                         00634
                                        HR-PLS-UWRT-CORP-CD-OLD.        00634
           MOVE HRCP-BC-PLAN-COVERAGE-CODE-OLD  TO                      00634
                                         HR-BC-PLAN-COVERAGE-CODE-OLD.  00634
           MOVE HRCP-BLUE-SHIELD-PLAN-CODE-OLD  TO                      00634
                                         HR-BLUE-SHIELD-PLAN-CODE-OLD.  00634
           IF   HRCP-TC-PENALTY-AMT NUMERIC                             00634
                MOVE HRCP-TC-PENALTY-AMT     TO   HR-TC-PENALTY-AMT     00634
           ELSE                                                         00634
                MOVE ZEROES                  TO   HR-TC-PENALTY-AMT.    00634
           MOVE HRCP-PRODUCT-FUND-CODE  TO  HR-PRODUCT-FUND-CODE.       00634
           MOVE HRCP-RATING-COMB-CODE   TO HR-RATING-COMB.              00634
           MOVE HRCP-PROVIDER-NAME      TO HR-PROVIDER-NAME.            00634
           MOVE HRCP-PROVIDER-ZIP-CODE  TO HR-PROVIDER-ZIP-CODE.        00634
           MOVE HRCP-MEDICAL-REC-18-30  TO HR-MEDICAL-REC-18-30.        00634
           MOVE HRCP-BILLING-IND        TO HR-BILLING-IND.              00634
           IF   HRCP-TOTAL-DIFF-AMT  NUMERIC                            00634
                MOVE HRCP-TOTAL-DIFF-AMT     TO HR-TOTAL-DIFF-AMT       00634
           ELSE                                                         00634
                MOVE ZEROES                  TO HR-TOTAL-DIFF-AMT.      00634
      **** ADDED 12/16/03 BY MAHESH FOR DM RECORDS *****************    02144
           MOVE HRCP-DISEASE-MGMT-IND   TO  HR-DISEASE-MGMT-IND.        02356
           IF   HRCP-DMG-AMERICAN-HEALTHWAY   OR                        02356
                HRCP-DMG-ACCORDANT            OR                        02356
                HRCP-DMG-RMS                                            02356
                MOVE HRCP-AHS-PAYMT          TO  HR-DISEASE-MGMT-RATE   02356

           ELSE                                                         02356
                MOVE ZEROS                   TO  HR-DISEASE-MGMT-RATE.  02356
           MOVE HRCP-HIPAA-ALT-ID       TO  HR-HIPAA-ALT-ID.            02356
           IF   HRCP-CDHP-HRA-AMOUNT  NUMERIC                           02356
                MOVE HRCP-CDHP-HRA-AMOUNT    TO  HR-CDHP-HRA-AMOUNT     02356
           ELSE                                                         02356
                MOVE ZEROES                  TO  HR-CDHP-HRA-AMOUNT.    02356
      **** MULTIPURSE CHANGES START                                     02356
           IF   HRCP-CDHP-HSA-AMOUNT  NUMERIC                           02356
                MOVE HRCP-CDHP-HSA-AMOUNT    TO  HR-CDHP-HSA-AMOUNT     02356
           ELSE                                                         02356
                MOVE ZEROES                  TO  HR-CDHP-HSA-AMOUNT.    02356
      *                                                                 02356
           MOVE HRCP-CDHP-IND           TO  HR-CDHP-IND.                02356
      **** MULTIPURSE CHANGES END                                       02356
      * DONE AS PART OF CR#117642-WP MARKET SEGMENTATION
           MOVE  HRCP-WP-MKT-SEGMENT      TO HR-PLS-WP-MKT-SEGMENT.
      *
           IF   HRCP-SIC-CODE NUMERIC
                MOVE HRCP-SIC-CODE    TO  HR-SIC-CODE
           ELSE
                MOVE ZEROES                  TO  HR-SIC-CODE.
      * NPI CHANGES STARTS HERE.
           MOVE HRCP-NPI-CODE    TO  HR-NPI-CODE.
      * NPI CHANGES ENDS HERE.
      * RSI INDICATOR CHANGES STARTS HERE.
           MOVE HRCP-RSI-IND     TO  HR-RSI-IND.
      * RSI INDICATOR CHANGES ENDS HERE.
      * NARROW NETWORK INDICATOR CHANGES STARTS HERE.
           MOVE HRCP-NARROW-NET-IND     TO    HR-NARROW-NET-IND.
      * NARROW NETWORK INDICATOR CHANGES ENDS HERE.
      * BLOOM EXCHANGE INDICATOR CHANGES STARTS HERE.
           MOVE HRCP-BLOOM-EXCHANGE-IND     TO HR-BLOOM-EXCH-IND.
      * BLOOM EXCHANGE INDICATOR CHANGES ENDS HERE.
      * EFT INDICATOR CHANGES STARTS HERE.
           MOVE HRCP-EFT-IND                TO HR-EFT-IND.
      * EFT INDICATOR CHANGES ENDS HERE.
      * ICD10 PROJECT CHANGES STARTS HERE.
           MOVE HRCP-ICD10-PRIMARY-DIAG-CODE TO
                                      HR-ICD10-PRIMARY-DIAG-CODE.
           MOVE HRCP-ICD9-ICD10-IND         TO HR-ICD9-ICD10-IND.
           MOVE HRCP-ICD10-PRIMARY-PROC-CODE   TO
                                 HR-ICD10-PRIMARY-PROC-CODE.
           MOVE HRCP-ICD10-PRM-PROC-DATE       TO
                                 HR-ICD10-PRM-PROC-DATE.
           IF  HRCP-EXPND-CHECK-NUMBER IS NUMERIC
               MOVE HRCP-EXPND-CHECK-NUMBER
                                    TO HR-EXPND-CHECK-NUMBER
           ELSE MOVE ZEROES         TO HR-EXPND-CHECK-NUMBER.
      * ICD10 PROJECT CHANGES ENDS HERE.
      * ITS PROJECT CHANGES STARTS HERE.
           IF  HRCP-ITS-SUPP-AMT IS NUMERIC
               MOVE HRCP-ITS-SUPP-AMT
                                    TO HR-ITS-SUPP-AMT
           ELSE MOVE ZEROES         TO HR-ITS-SUPP-AMT.
      * ITS PROJECT CHANGES ENDS HERE.
      * EBF PROJECT CHANGES STARTS HERE.
           MOVE HRCP-EBF-IND        TO HR-EBF-IND.
      * EBF PROJECT CHANGES ENDS HERE.
      * CHANGES FOR LOCAL ACCESS FEE PROJECT START.
           IF  HRCP-LOC-ACCESS-FEE IS NUMERIC
               MOVE HRCP-LOC-ACCESS-FEE
                                    TO HR-LOC-ACES-FEE
           ELSE MOVE ZEROES         TO HR-LOC-ACES-FEE.
      * CHANGES FOR LOCAL ACCESS FEE PROJECT END.
      * CHANGES FOR POOLING REPORT PROJECT START.
           MOVE HRCP-PLACE-OF-SERVICE       TO HR-PLACE-OF-SERVICE.
      * CHANGES FOR POOLING REPORT PROJECT END.
      * CHANGES FOR POOL P AMOUNT START.
           IF  HRCP-POOL-P-AMOUNT IS NUMERIC
               MOVE HRCP-POOL-P-AMOUNT
                                    TO HR-POOL-P-AMOUNT
           ELSE MOVE ZEROES         TO HR-POOL-P-AMOUNT.
      * CHANGES FOR POOL P AMOUNT END.
      * CHANGES FOR TOTAL CHARGES START.
           IF  HRCP-TOTAL-CHARGES-1 IS NUMERIC
               MOVE HRCP-TOTAL-CHARGES-1
                                    TO HR-TOT-HOSP-BILL-EXPANDED
           ELSE MOVE ZEROES         TO HR-TOT-HOSP-BILL-EXPANDED.
      * CHANGES FOR TOTAL CHARGES END.
      * CHANGES FOR PC2 PROJECT START
           MOVE HRCP-PC2-PROGRAM-CODE       TO HR-PC2-PROGRAM-CODE.
           MOVE HRCP-PC2-FUND-TYPE          TO HR-PC2-FUND-TYPE.
      * CHANGES FOR PC2 PROJECT END
      * CR#127537 IDENTIFY EMERGENCY ROOM CLAIMS FOR CT MED ADV-START
           MOVE HRCP-LINE-CATEGORY-CODE (1) TO HR-LINE-CATEGORY-CODE.
      * CR#127537 IDENTIFY EMERGENCY ROOM CLAIMS FOR CT MED ADV-END
      *CR#133719 -->
           MOVE HRCP-NIA-CAP-IND TO  HR-NIA-CAP-IND.
      *CR#133719 <--
      *CR#140269-NIA PRORATE AMOUNT CHANGES BEGINS.
           IF  HRCP-CAP-PAY-NIA-AMT IS NUMERIC
                MOVE HRCP-CAP-PAY-NIA-AMT TO
                             HR-CAP-PAY-NIA-AMT
           ELSE
                MOVE +0 TO  HR-CAP-PAY-NIA-AMT.
      *CR#140269-NIA PRORATE AMOUNT CHANGES ENDS.
      *CR#146587-MCS PROMPT PAY AMOUNT CHANGES BEGINS.
           IF  HRCP-MCS-PROMPT-PAY-INT IS NUMERIC
                MOVE HRCP-MCS-PROMPT-PAY-INT TO
                             HR-MCS-PROMPT-PAY-INT
           ELSE
                MOVE +0 TO  HR-MCS-PROMPT-PAY-INT.
      *CR#146587-MCS PROMPT PAY AMOUNT CHANGES ENDS.
      **CR#160671-NCN PROJECT EXPANTION CHANGES BEGIN.
           MOVE HRCP-NCN-INDICATOR TO HR-NCN-INDICATOR.
           IF  HRCP-NCN-FEE IS NUMERIC
               MOVE HRCP-NCN-FEE TO HR-NCN-FEE
           ELSE
               MOVE +0 TO  HR-NCN-FEE.
           IF  HRCP-NCN-GRP-FEE IS NUMERIC
               MOVE HRCP-NCN-GRP-FEE TO HR-NCN-GROUP-FEE
           ELSE
               MOVE +0 TO  HR-NCN-GROUP-FEE.
      **CR#160671-NCN PROJECT EXPANTION CHANGES END.
      **CR#161042-APR DRG PROJECT CHANGES BEGIN.
           MOVE HRCP-PLS-DRG-SVY-IN TO HR-PLS-DRG-SVY-IN.
      **CR#161042-APR DRG PROJECT CHANGES END.
      * ACCESS FEE EXCLUSION START
           PERFORM 1500-ASSIGN-ACCESS-FEE-IND THRU 1500-EXIT.
      * ACCESS FEE EXCLUSION END
      **************************************************************    02356
       1500-ASSIGN-ACCESS-FEE-IND.
           MOVE   SPACES              TO HR-ACCESS-FEE-IND
           SET FEE-IX TO WS-PLUS-1.
           PERFORM 1510-SEARCH-ACCESS-FEE-GROUP THRU 1510-EXIT
                VARYING FEE-IX
               FROM WS-PLUS-1 BY WS-PLUS-1 UNTIL
               FEE-IX > WS-ACCESS-RECORDS.
       1500-EXIT.
            EXIT.
       1510-SEARCH-ACCESS-FEE-GROUP.
            SEARCH ACCESS-FEE-GROUP
            WHEN SEL-GROUP-NUMBER =  AFEE-GROUP (FEE-IX)
            AND  '***'            =  AFEE-SUBDIV (FEE-IX)
                 MOVE 'Y'               TO HR-ACCESS-FEE-IND
            WHEN SEL-GROUP-NUMBER =  AFEE-GROUP (FEE-IX)
            AND  SEL-DETAIL-SUB-DIVISION  =  AFEE-SUBDIV (FEE-IX)
                 MOVE 'Y'               TO HR-ACCESS-FEE-IND.
       1510-EXIT.
            EXIT.
      **************************************************************    02356
       B-1405-GME-DATE-FIX.                                             02356
            IF WS-GME-DAY-COUNTER = GME-CURRENT-DAY                     02357
               MOVE ZEROES  TO WS-GME-DAY-COUNTER.                      02358
                                                                        02359
170000       IF   SEL-DETAIL-PAID-DAY  = WS-PREV-PAID-DAY               02360
                  ADD WS-ONE              TO  WS-GME-DAY-COUNTER        02361
                  MOVE WS-GME-DAY-COUNTER TO  SEL-DETAIL-PAID-DAY       02362
170000       ELSE                                                       02363
170000            MOVE SEL-DETAIL-PAID-DAY TO WS-PREV-PAID-DAY.         02364
158700     SKIP3                                                        02365
189700 B-1430-ATTACH-INDICATORS.                                        02366
189800***************************************************************   02367
189900*    PURPOSE :                                                *   02368
190000*            MOVE DESCRIPTIVE INDICATORS BASED UPON THE INPUT *   02369
190100*             FILE DESCRIPTION 88 LEVEL INDICATORS            *   02370
190200***************************************************************   02371
190300     SKIP3                                                        02372
C21TSR*    IF HRCP-ACTN-YR           LESS THAN WS-90                    02373
                                                                        02374
                                                                        02375
C21TSR     CALL  C2140C02  USING HRCP-ACTN-YR C21HRCP-ACTN-YR           02376
C21TSR               C21-BASE-ON C21-WORK-AREA                          02377
C21TSR     CALL  C2140C02  USING WS-90 C21WS-90 C21-BASE-ON             02378
C21TSR               C21-WORK-AREA                                      02379
C21TSR     IF C21HRCP-ACTN-YR LESS THAN C21WS-90                        02380
190500         MOVE WS-LITERAL-A TO SEL-CLAIM-SERVICE-CATEGORY          02381
190600         IF (HRCP-ALB-INVALID-BEFORE OR                           02382
190700             HRCP-ALB-INVALID-NEVER)                              02383
190800             MOVE HRCP-ALBANY-INVALID-CONV                        02384
190900                  TO SEL-CLAIM-SERVICE-CATEGORY                   02385
191000         ELSE CONTINUE                                            02386
191100     ELSE                                                         02387
191200         MOVE SPACE TO SEL-CLAIM-SERVICE-CATEGORY.                02388
191300     SKIP3                                                        02389
191400         IF HRCP-PRESSO                                           02390
191500             MOVE WS-ONE       TO SEL-ICHIS-PRESSO-FLAG           02391
191600          ELSE                                                    02392
191700             MOVE WS-ZERO      TO SEL-ICHIS-PRESSO-FLAG.          02393
191800     SKIP3                                                        02394
191900         IF HRCP-RIDER                                            02395
192000             MOVE WS-ONE       TO SEL-ICHIS-RIDER-FLAG            02396
192100          ELSE                                                    02397
192200             MOVE WS-ZERO      TO SEL-ICHIS-RIDER-FLAG.           02398
192300     SKIP3                                                        02399
192400         IF HRCP-SUPP                                             02400
192500             MOVE WS-ONE   TO SEL-ICHIS-SUPP-PAYMENT-FLAG         02401
192500             ADD  WS-ONE   TO WS-ICHIS-SUPP-RECORDS               02402
172000             ADD  HRCP-AHS-PAYMT   TO WS-ICHIS-SUPP-TOTALS        02403
192600          ELSE                                                    02404
192700             MOVE WS-ZERO  TO SEL-ICHIS-SUPP-PAYMENT-FLAG.        02405
192800     SKIP2                                                        02406
192900     MOVE HRCP-POS-NETWORK-IND TO SEL-POS-INDICATOR.              02407
193000     EJECT                                                        02408
       B-1440-CHECK-OUTPUT-FIELDS.                                      02409
           IF SEL-DETAIL-CLAIM-NUMBER EQUAL SPACES                      02410
574500       MOVE WS-FOURTEEN-ZEROS TO SEL-DETAIL-CLAIM-NUMBER.         02411
575500     SKIP2                                                        02412
575100     IF SEL-SUBSCRIBER-ID EQUAL SPACES                            02413
575300        MOVE WS-FOURTEEN-ZEROS TO SEL-SUBSCRIBER-ID.              02414
575500     SKIP2                                                        02415
575600     IF SEL-DETAIL-PAID-DAY NOT NUMERIC                           02416
576300        MOVE CURRENT-DAY      TO SEL-DETAIL-PAID-DAY.             02417
575500     SKIP2                                                        02418
575600     IF SEL-DETAIL-PAID-DAY = WS-00                               02419
576300        MOVE CURRENT-DAY      TO SEL-DETAIL-PAID-DAY.             02420
576000     SKIP2                                                        02421
576100     IF SEL-DETAIL-INCURRED-DAY NOT NUMERIC                       02422
576300        MOVE WS-LITERAL-01  TO SEL-DETAIL-INCURRED-DAY.           02423
576500     SKIP2                                                        02424
576100     IF SEL-DETAIL-INCURRED-DAY = WS-00                           02425
576300        MOVE WS-LITERAL-01  TO SEL-DETAIL-INCURRED-DAY.           02426
576500     SKIP2                                                        02427
576600     IF SEL-INCURRED-YEAR NOT NUMERIC                             02428
576800        MOVE SEL-PAID-YEAR     TO SEL-INCURRED-YEAR.              02429
577000     SKIP2                                                        02430
577100     IF SEL-INCURRED-MONTH NOT NUMERIC                            02431
577400        MOVE SEL-PAID-MONTH    TO  SEL-INCURRED-MONTH.            02432
577500     SKIP2                                                        02433
193000     EJECT                                                        02434
193100 B-1500-BAL-HOSPITAL-FILE.                                        02435
193200***************************************************************   02436
193300*    PURPOSE :                                                *   02437
193400*             COMPARE THE INSTITUTIONAL FILE TRAILER RECORD   *   02438
193500*             TOTALS TO THE WORKING STORAGE INPUT FILE TOTALS *   02439
193600*             AND THEN COMPARE THE PROCESSING COUNTS TO THE   *   02440
193700*             EXTRACT FILE OUTPUT RECORD COUNTS               *   02441
193800***************************************************************   02442
193900     SKIP2                                                        02443
194000     IF WS-HOSP-TRLR-RECS      =  WS-REC-CNTS(1)                  02444
194100        AND WS-HOSP-TRLR-LIAB  =  WS-LOB-LIAB-AMTS(1)             02445
194200           PERFORM B-1599-HOSPITAL-FILE-BALANCE                   02446
194300     ELSE                                                         02447
194400         DISPLAY WS-DISPLAY-1                                     02448
194500         MOVE WS-ABEND-CODE(3) TO USER-ABEND-CODE                 02449
194600         DISPLAY USER-ABEND-CODE                                  02450
194700         DISPLAY WS-ABEND-MSG-TBL(1)                              02451
194800         DISPLAY WS-ABEND-MSG-TBL(2)                              02452
194900         DISPLAY WS-DISPLAY-1                                     02453
195000         MOVE WS-HOSP-TRLR-RECS      TO                           02454
195100                               WS-EDITED-DISPLY-CNTS              02455
195200         DISPLAY WS-ABEND-MSG-TBL(3) WS-EDITED-DISPLY-CNTS        02456
195300                                                                  02457
195400         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-CNTS              02458
195500                                                                  02459
195600         COMPUTE WS-COMPUTE-RECORDS    = WS-REC-CNTS(1)           02460
195700         MOVE  WS-COMPUTE-RECORDS  TO  WS-EDITED-DISPLY-CNTS      02461
195800         DISPLAY WS-ABEND-MSG-TBL(4) WS-EDITED-DISPLY-CNTS        02462
195900                                                                  02463
196000         MOVE WS-HOSP-TRLR-LIAB    TO WS-EDITED-DISPLY-AMTS       02464
196100         DISPLAY WS-ABEND-MSG-TBL(5) WS-EDITED-DISPLY-AMTS        02465
196200                                                                  02466
196300         MOVE WS-PLUS-ZERO  TO        WS-EDITED-DISPLY-AMTS       02467
196400         COMPUTE WS-COMPUTE-LOB-LIB   = WS-LOB-LIAB-AMTS(1)       02468
196500         MOVE  WS-COMPUTE-LOB-LIB  TO  WS-EDITED-DISPLY-AMTS      02469
196600                                                                  02470
196700         DISPLAY WS-ABEND-MSG-TBL(6) WS-EDITED-DISPLY-AMTS        02471
196800         DISPLAY WS-DISPLAY-1                                     02472
196900         PERFORM Z-CALL-BOMBER.                                   02473
197000      EJECT                                                       02474
197100 B-1599-HOSPITAL-FILE-BALANCE.                                    02475
197200***************************************************************   02476
197300*    PURPOSE :                                                *   02477
197400*             ISSUE MESSAGE ON SYSOUT THAT FILE INPUT BALANCED*   02478
197500***************************************************************   02479
197600     SKIP2                                                        02480
197700     DISPLAY SPACES.                                              02481
197800     DISPLAY WS-BALANCED-FILE-MESSAGE(1).                         02482
197900     DISPLAY SPACES.                                              02483
198000     EJECT                                                        02484
152400 BA-1000-PROCESS-HOSPITAL-FILE.                                   02485
152500******************************************************************02486
152600*    PURPOSE :                                                   *02487
152700*             INITIATE ROUTINE ACTIVITIES FOR PROCESSING AND     *02488
152800*             BALANCING THE MONTHLY INSTITUTIONAL CLAIMS FILE    *02489
152900******************************************************************02490
153000     SKIP3                                                        02491
153100     PERFORM BA-1100-OPEN-PROCESS-CLOSE.                          02492
153200     EJECT                                                        02493
153300 BA-1100-OPEN-PROCESS-CLOSE.                                      02494
153400     SKIP3                                                        02495
153500     OPEN INPUT  I-COMBINED-PAY950-FILE.                          02496
153600     SKIP3                                                        02497
153700     PERFORM BA-1110-READ-HOSP-FIRST-TIME.                        02498
153800     SKIP3                                                        02499
153900     PERFORM BA-1135-HOSP-DATE-CHECK-RTN.                         02500
154000     SKIP3                                                        02501
154100     PERFORM BA-1130-READ-HOSP-FILE-RTN                           02502
154200        UNTIL WS-EOF-HOSP.                                        02503
154300     SKIP3                                                        02504
154400     PERFORM BA-1500-BAL-HOSPITAL-FILE.                           02505
154500     SKIP3                                                        02506
154600     CLOSE I-COMBINED-PAY950-FILE.                                02507
154700     EJECT                                                        02508
154800 BA-1110-READ-HOSP-FIRST-TIME.                                    02509
154900******************************************************************02510
155000*    PURPOSE :                                                   *02511
155100*             READ 1 RECORD FROM THE INSTITUTIONAL FILE TO       *02512
155200*             CHECK PAID DATE ON RECORD                          *02513
155300******************************************************************02514
155400     SKIP3                                                        02515
155500     READ I-COMBINED-PAY950-FILE INTO                             02516
155600            HRCR-REPORT-RECORD     AT END                         02517
155700                 MOVE WS-ZERO TO WS-EOF-HOSP-FILE.                02518
155800     SKIP3                                                        02519
155900       IF WS-EOF-HOSP                                             02520
156000          MOVE WS-ABEND-CODE(1) TO USER-ABEND-CODE                02521
156100          DISPLAY  WS-DISPLAY-1                                   02522
156200          DISPLAY WS-ERROR-MSG-TBL(1)                             02523
156300          DISPLAY USER-ABEND-CODE                                 02524
156400          DISPLAY  WS-DISPLAY-1                                   02525
156500          DISPLAY WS-DISPLAY-1                                    02526
156600          PERFORM Z-CALL-BOMBER.                                  02527
156700     EJECT                                                        02528
156800 BA-1130-READ-HOSP-FILE-RTN.                                      02529
156900     SKIP2                                                        02530
157000***************************************************************   02531
157100*    PURPOSE :                                                *   02532
157200*             READS THE HOSPITAL FILE AND ACCUMULATES         *   02533
157300*             RECORD COUNTS, CLAIMS COUNT DAYS COUNT AND      *   02534
157400*             AHS LIABILITY AMOUNTS.                          *   02535
157500***************************************************************   02536
157600     SKIP2                                                        02537
155900     IF HRCR-HEADER-VALID                                         02538
157800        PERFORM BA-1135-HOSP-DATE-CHECK-RTN                       02539
155900     ELSE                                                         02540
157700     IF HRCR-TRAILER-VALID                                        02541
157800***     PERFORM BA-1135-HOSP-DATE-CHECK-RTN                       02542
157900        ADD  HRCR-TOTAL-RECORD-COUNT   TO WS-HOSP-TRLR-RECS       02543
158000        ADD  HRCR-TOTAL-LIABILITY      TO WS-HOSP-TRLR-LIAB       02544
158100     ELSE                                                         02545
158200        PERFORM BA-1135-HOSP-DATE-CHECK-RTN                       02546
158300        PERFORM BA-1200-ACCUM-IP-REC-RTN                          02547
158400        PERFORM BA-1300-HOSP-SEL-PD-CLM-RECORD                    02548
158500        IF NOT WS-BYPASS                                          02549
158600           PERFORM BA-1405-HOSP-OUTPUT-DETAIL-REC                 02550
166700           PERFORM X-3000-WRITE-RTN                               02551
158500        ELSE                                                      02552
158600           PERFORM BA-1405-HOSP-OUTPUT-DETAIL-REC                 02553
158600           MOVE ECKS TO SEL-AUDIT-IND                             02554
166700           PERFORM X-3000-WRITE-RTN.                              02555
158700     SKIP3                                                        02556
158800     MOVE WS-ZERO             TO WS-BYPASS-IND.                   02557
158900     SKIP3                                                        02558
159000     READ I-COMBINED-PAY950-FILE INTO                             02559
159100            HRCR-REPORT-RECORD AT END                             02560
159200                 MOVE WS-ZERO TO WS-EOF-HOSP-FILE.                02561
159300     EJECT                                                        02562
159400 BA-1135-HOSP-DATE-CHECK-RTN.                                     02563
159500******************************************************************02564
159600*    PURPOSE :                                                   *02565
159700*             MATCH THE RECORD PAYMENT DATE TO THE DATE FIELDS   *02566
159800*             FOR THE CURRENT PRODUCTION CYCLE.                  *02567
159900******************************************************************02568
160000     SKIP3                                                        02569
155900     IF HRCR-HEADER-VALID                                         02570
              PERFORM BA-1136-CHECK-HEADER-DATES                        02571
           ELSE                                                         02572
160100      IF HRCR-ACTN-YR NOT EQUAL CURRENT-YEAR                      02573
160200       OR HRCR-ACTN-MO NOT EQUAL CURRENT-MONTH                    02574
160300          MOVE WS-ABEND-CODE(2) TO USER-ABEND-CODE                02575
160400          DISPLAY USER-ABEND-CODE                                 02576
160500          DISPLAY WS-ERROR-MSG-TBL(3)                             02577
160600          DISPLAY WS-ERROR-MSG-TBL(7) CURRENT-YEAR CURRENT-MONTH  02578
160700          DISPLAY WS-ERROR-MSG-TBL(28) HRCR-ACTN-YR HRCR-ACTN-MO  02579
160800          PERFORM Z-CALL-BOMBER.                                  02580
160900                                                                  02581
161000     EJECT                                                        02582
       BA-1136-CHECK-HEADER-DATES.                                      02583
160100      IF HRCR-PAID-YY-FROM NOT EQUAL CURRENT-YEAR                 02584
160200       OR HRCR-PAID-MM-FROM NOT EQUAL CURRENT-MONTH               02585
160300          MOVE WS-ABEND-CODE(2) TO USER-ABEND-CODE                02586
160400          DISPLAY USER-ABEND-CODE                                 02587
160500          DISPLAY WS-ERROR-MSG-TBL(3)                             02588
160600          DISPLAY WS-ERROR-MSG-TBL(7) CURRENT-YEAR CURRENT-MONTH  02589
160700          DISPLAY WS-ERROR-MSG-TBL(28)                            02590
                HRCR-PAID-YY-FROM HRCR-PAID-MM-FROM                     02591
                PERFORM Z-CALL-BOMBER.                                  02592
                                                                        02593
161100 BA-1200-ACCUM-IP-REC-RTN.                                        02594
161200     SKIP2                                                        02595
161300***************************************************************   02596
161400*    PURPOSE :                                                *   02597
161500*             ACCUMULATE TOTALS FOR:                          *   02598
161600*             RECORD COUNTS, CLAIM COUNTS, DAYS COUNTS AND    *   02599
161700*             AHS LIABILITY AMOUNTS.                          *   02600
161800***************************************************************   02601
161900     SKIP2                                                        02602
           IF HRCR-GME-PROCESS                                          02603
              ADD WS-PLUS-ONE         TO WS-GME-RECORDS-IN              02604
              ADD HRCR-AHS-PAYMT      TO WS-GME-LIABILITY-IN.           02605
161900     SKIP2                                                        02606
162000     ADD WS-POSITIVE-ONE TO      WS-REC-CNTS(1).                  02607
162100     SKIP2                                                        02608
162200     ADD HRCR-AHS-PAYMT TO   WS-LOB-LIAB-AMTS(1).                 02609
162300     SKIP2                                                        02610
162400     ADD HRCR-CASE-CTR  TO   WS-CLAIMS-CNTS(1).                   02611
162500     SKIP2                                                        02612
162600     ADD HRCR-FULL-DAYS  HRCR-DISCOUNT-DAYS TO                    02613
162700                         WS-DAYS-CNTS(1).                         02614
162800     EJECT                                                        02615
162900 BA-1300-HOSP-SEL-PD-CLM-RECORD.                                  02616
163000                                                                  02617
163100***************************************************************   02618
163200*    PURPOSE :                                                *   02619
163300*                                                             *   02620
163400*      BYPASS INPUT RECORDS FROM FURTHER PROCESSING AND       *   02621
163500*      ALSO ACCUMULATE TOTALS FOR BYPASS RECORDS.             *   02622
163600*                                                             *   02623
163700***************************************************************   02624
163800     SKIP2                                                        02625
163900     IF HRCR-AHS-PAYMT EQUAL TO WS-PLUS-ZERO                      02626
164000        PERFORM BA-1305-CHECK-HRA-HSA-AMOUNTS                     01867
164000        PERFORM BA-1305-CHECK-HOSP-BYPASS.                        02627
164100***     MOVE WS-ONE        TO   WS-BYPASS-IND.                    02628
164200     SKIP2                                                        02629
164300     IF WS-BYPASS                                                 02630
164400        ADD WS-POSITIVE-ONE TO  WS-REC-CNTS(11)                   02631
164500        ADD HRCR-CASE-CTR  TO   WS-CLAIMS-CNTS(11)                02632
164600        ADD HRCR-AHS-PAYMT TO   WS-LOB-LIAB-AMTS(11)              02633
164700        ADD HRCR-FULL-DAYS  HRCR-DISCOUNT-DAYS TO                 02634
164800                                WS-DAYS-CNTS(11).                 02635
164900     EJECT                                                        02636
164000 BA-1305-CHECK-HRA-HSA-AMOUNTS.                                   01867
164000        IF HRCR-CDHP-HRA-AMOUNT NOT NUMERIC                       01867
164000           MOVE ZEROS       TO HRCR-CDHP-HRA-AMOUNT               01867
164000        END-IF.                                                   01867
164000        IF HRCR-CDHP-HSA-AMOUNT NOT NUMERIC                       01867
164000           MOVE ZEROS       TO HRCR-CDHP-HSA-AMOUNT               01867
164000        END-IF.                                                   01867
165000*-                                                                02637
165000 BA-1305-CHECK-HOSP-BYPASS.                                       02637
165100     IF HRCR-NON-POOL-PAYMT     =   WS-PLUS-ZERO  AND             02638
165200        HRCR-FLEX-DEDUCT-AMT    =   WS-PLUS-ZERO  AND             02639
165300        HRCR-FLEX-CO-PAY-AMT    =   WS-PLUS-ZERO  AND             02640
165400        HRCR-FLEX-CO-INSUR-AMT     =   WS-PLUS-ZERO  AND          02641
165500        HRCR-OTHER-DEDUCTIBLE-AMT  =   WS-PLUS-ZERO  AND          02642
027210        HRCR-CAP-PAY-VENDOR-AMT     =   WS-PLUS-ZERO  AND         02676
165600        HRCR-TOT-HOSP-BILL         =   WS-PLUS-ZERO  AND          02643
165700        HRCR-TOT-CHGD-TO-PAT       =   WS-PLUS-ZERO  AND          02644
165700        HRCR-CORE-PROCESS-IND      = 'CP'                         02644
165800             CONTINUE   ELSE                                      02645
165100     IF HRCR-NON-POOL-PAYMT     =   WS-PLUS-ZERO  AND             02638
165200        HRCR-FLEX-DEDUCT-AMT    =   WS-PLUS-ZERO  AND             02639
165300        HRCR-FLEX-CO-PAY-AMT    =   WS-PLUS-ZERO  AND             02640
165400        HRCR-FLEX-CO-INSUR-AMT     =   WS-PLUS-ZERO  AND          02641
165500        HRCR-OTHER-DEDUCTIBLE-AMT  =   WS-PLUS-ZERO  AND          02642
027210        HRCR-CAP-PAY-VENDOR-AMT     =   WS-PLUS-ZERO  AND         02676
165600        HRCR-TOT-HOSP-BILL         =   WS-PLUS-ZERO  AND          02643
165600        HRCR-CDHP-HRA-AMOUNT       =   WS-PLUS-ZERO  AND          02643
165600        HRCR-CDHP-HSA-AMOUNT       =   WS-PLUS-ZERO  AND          02643
165700        HRCR-TOT-CHGD-TO-PAT       =   WS-PLUS-ZERO               02644
165800        MOVE WS-ONE                TO  WS-BYPASS-IND.             02645
165000*-                                                                02637
165900 BA-1405-HOSP-OUTPUT-DETAIL-REC.                                  02646
166000***************************************************************   02647
166100*    PURPOSE :                                                *   02648
166200*             PREPARE TO WRITE THE OUTPUT DETAIL RECORD       *   02649
166300***************************************************************   02650
166400     PERFORM BA-1410-ACCUM-OUTPUT-COUNTS.                         02651
166500     PERFORM BA-1420-FORMAT-FIELDS.                               02652
166600     PERFORM BA-1430-ATTACH-INDICATORS.                           02653
166800     EJECT                                                        02654
166900 BA-1410-ACCUM-OUTPUT-COUNTS.                                     02655
167000***************************************************************   02656
167100*    PURPOSE :                                                *   02657
167200*             ACCUMULATE TOTALS ON FIELDS IN THE DETAIL RECORD*   02658
167300***************************************************************   02659
167400     SKIP3                                                        02660
167500         ADD WS-POSITIVE-ONE        TO  WS-REC-CNTS(6)            02661
167600                                        WS-REC-CNTS(16)           02662
167700         ADD HRCR-AHS-PAYMT         TO  WS-LOB-LIAB-AMTS(6)       02663
167800                                        WS-LOB-LIAB-AMTS(16)      02664
167900         ADD HRCR-CASE-CTR          TO  WS-CLAIMS-CNTS(6)         02665
168000                                        WS-CLAIMS-CNTS(16)        02666
168100         ADD HRCR-FULL-DAYS HRCR-DISCOUNT-DAYS TO                 02667
168200                                        WS-DAYS-CNTS(6)           02668
168300                                        WS-DAYS-CNTS(16).         02669
168400     EJECT                                                        02670
168500 BA-1420-FORMAT-FIELDS.                                           02671
168600***************************************************************   02672
168700*    PURPOSE :                                                *   02673
168800*             SET UP THE ATTRIBUTES FOR THE DETAIL RECORD     *   02674
168900***************************************************************   02675
169000     SKIP3                                                        02676
143500     MOVE LOW-VALUES  TO  SEL-PAID-CLAIMS-RECORD.                 02677
169100     IF  NOT HRCR-OUT-PATIENT-ACCOM                               02678
169200         MOVE LOW-VALUES             TO SEL-TYPE-OF-SERVICE       02679
169300     ELSE                                                         02680
169400        MOVE ZEROS                   TO SEL-TYPE-OF-SERVICE.      02681
169500     SKIP3                                                        02682
169600     MOVE WS-DETAIL                  TO SEL-DETAIL-RCD-INDICATOR. 02683
169700     MOVE SPACES                TO SEL-DETAIL-RCD-IDENTIFICATION. 02684
169800     MOVE HRCR-ACTN-YR               TO SEL-PAID-YEAR             02685
169800                                        DESTID-YY.                01925
169900     MOVE HRCR-ACTN-MO               TO SEL-PAID-MONTH            02686
169800                                        DESTID-MM.                01925
170000     MOVE HRCR-ACTN-DA               TO SEL-DETAIL-PAID-DAY       02687
169800                                        DESTID-DD.                01925
           IF DESTID-YY    >  56                                        01628
              MOVE   WS-19     TO DESTID-CC                             01628
           ELSE                                                         01628
              MOVE   WS-20     TO DESTID-CC.                            01628
170100     MOVE HRCR-ADM-YR                TO SEL-INCURRED-YEAR.        02688
170200     MOVE HRCR-ADM-MO                TO SEL-INCURRED-MONTH.       02689
170300     MOVE HRCR-ADM-DA                TO SEL-DETAIL-INCURRED-DAY.  02690
170400     MOVE WS-HOSPITAL-LOB-LIT        TO SEL-LINE-OF-BUSINESS.     02691
170500     MOVE HRCR-BC-PLAN-COVERAGE-CODE TO SEL-PLAN-CODE.            02692
170600     MOVE HRCR-GROUPNO               TO SEL-GROUP-NUMBER.         02693
170700     MOVE HRCR-SENIOR-CARE-IND       TO SEL-SENIOR-CARE-IND.      02694
170800     MOVE HRCR-SUB-DIVISN            TO SEL-DETAIL-SUB-DIVISION.  02695
170900     MOVE HRCR-CERT-NO               TO SEL-SUBSCRIBER-ID.        02696
171000     COMPUTE WS-PAID-DAYS = HRCR-FULL-DAYS +                      02697
171100                            HRCR-DISCOUNT-DAYS                    02698
171200     MOVE WS-PAID-DAYS               TO SEL-NUMBER-OF-DAYS-VISITS.02699
171300     MOVE HRCR-CASE-CTR              TO SEL-NUMBER-OF-CLAIMS.     02700
171400     IF (HRCR-CORE-CS90-PROCESS   AND HRCR-CS90-PROCESS)          02701
171400       IF  HRCR-CS90-CLAIM-PACKED NUMERIC                         02702
171500           MOVE HRCR-BR-ID          TO   TEST-BR-ID               02703
                 MOVE HRCR-CASE-ID        TO   TEST-CASE-ID             02704
171500           MOVE HRCR-CS90-CLAIM-PACKED TO TEST-CASENO-LST6        02705
171600           MOVE WS-UNPACK-CLAIM-NUMBER TO SEL-DETAIL-CLAIM-NUMBER 02706
171400       ELSE                                                       02707
171500           MOVE HRCR-BR-ID       TO   HR-BR-ID                    02708
                 MOVE HRCR-CASE-ID     TO   HR-CASE-ID                  02709
171500           MOVE HRCR-CASENO-LST6 TO   HR-CASENO-LST6              02710
171700     ELSE                                                         02711
171800          MOVE HRCR-CORE-SYS-CLAIM-NO TO SEL-DETAIL-CLAIM-NUMBER. 02712
171900     MOVE WS-PLUS-ZERO               TO SEL-NUMBER-OF-SERVICES.   02713
172000     MOVE HRCR-AHS-PAYMT             TO SEL-PAID-CLAIM-LIABILITY. 02714
172100     MOVE HRCR-PATIENT-FIRST-NAME   TO SEL-DETAIL-PATIENT-INITIAL.02715
172200     MOVE SPACES                  TO SEL-DETAIL-PATIENT-LAST-NAME.02716
172300     MOVE HRCR-PAT-LASTNAME       TO SEL-DETAIL-PATIENT-LAST-NAME.02717
172400     MOVE SPACE                  TO SEL-DETAIL-MED-SURG-BREAKDOWN 02718
172500     MOVE SPACE                      TO SEL-CSS-SOURCE-CODE.      02719
172600     MOVE SPACE                      TO SEL-LOB-ADJUSTMENT-CODE.  02720
172700     MOVE ZEROES                     TO SEL-ACCOUNT-PAID-DATE.    02721
172800     IF HRCR-BANK-B-CASE                                          02722
172900        MOVE WS-1            TO SEL-BANK-B-INDICATOR              02723
173000     ELSE                                                         02724
173100        MOVE SPACES          TO SEL-BANK-B-INDICATOR.             02725
158500        IF  WS-BYPASS                                             02726
158600            MOVE    ECKS TO SEL-AUDIT-IND                         02727
158500        ELSE                                                      02728
158600            MOVE   SPACES TO SEL-AUDIT-IND.                       02729
173200                                                                  02730
173300     MOVE HRCR-COLLECTION-GRPCDE TO SEL-ICHIS-COLLECTION-CODE.    02731
173500     IF HRCR-GME-PROCESS                                          02732
                  PERFORM BA-1405-GME-DATE-FIX                          02733
173500            MOVE WS-G                TO SEL-RECORD-TYPE           02734
                  ADD WS-PLUS-ONE          TO WS-GME-RECORDS-OUT        02735
172000            ADD HRCR-AHS-PAYMT       TO WS-GME-LIABILITY-OUT      02736
173500     ELSE                                                         02737
173500            MOVE WS-ONE              TO SEL-RECORD-TYPE.          02738
173600     MOVE HRCR-DISP-CDE              TO HR-DISP-CDE.              02739
173700     MOVE HRCR-MARR-STATUS           TO HR-MARR-STATUS.           02740
173800     MOVE HRCR-TYPE-CONTRACT         TO HR-TYPE-CONTRACT.         02741
173900     MOVE HRCR-TYPE-BUSINESS         TO HR-TYPE-BUSINESS.         02742
174000     MOVE HRCR-COLLECTION-GRPCDE     TO HR-COLLECTION-GRPCDE.     02743
174100     MOVE HRCR-AGE                   TO HR-AGE.                   02744
174200     MOVE HRCR-STILL-IN-IND          TO HR-STILL-IN-IND.          02745
174300     MOVE HRCR-CONTINUATION-IND      TO HR-CONTINUATION-IND.      02746
174400     MOVE HRCR-INDEMNITY-IND         TO HR-INDEMNITY-IND.         02747
174500     MOVE HRCR-ACCOMODATION          TO HR-ACCOMODATION.          02748
174600     MOVE HRCR-LMTD-APPROVAL-IND     TO HR-LMTD-APPROVAL-IND.     02749
174700     IF   HRCR-FULL-DAYS  NUMERIC                                 02750
174800          MOVE HRCR-FULL-DAYS        TO HR-FULL-DAYS              02751
174900     ELSE                                                         02752
175000          MOVE ZEROES                TO HR-FULL-DAYS.             02753
175100     IF   HRCR-DISCOUNT-DAYS NUMERIC                              02754
175200          MOVE HRCR-DISCOUNT-DAYS    TO HR-DISCOUNT-DAYS          02755
175300     ELSE                                                         02756
175400          MOVE ZEROES                TO HR-DISCOUNT-DAYS.         02757
175500     MOVE HRCR-BIRTH-YR              TO  HR-BIRTH-YR.             02758
      ** AS PER JACOB STEINGART'S SUGGESTION THIS MOVE STATEMENT IS
      ** COMMENTED OUT.HE REMOVED HRCP-OP-RATE-TYPE FROM ICHIS COPYBOOK
      **HRMERC2650 . AS PER HIS OPINION THIS FIELD IS NOT USED ANYWHERE.
      **THESE FIELDS HAVE BEEN REMOVED TO MAKE ROOM FOR HSA AMOUNT AND
      **CDHP INDICATOR. AS PER CR# 112132.
175600*    MOVE HRCR-OP-RATE-TYPE          TO                           02759
175700*                                    HR-OP-RATE-TYPE.             02760
175800     MOVE HRCR-DRG-CODE-NEW          TO HR-DRG-CODE-NEW.          02761
175900     MOVE HRCR-DRG-STATE-CODE-INDIC                               02762
176000                                     TO HR-DRG-STATE-CODE-INDIC.  02763
176100     MOVE HRCR-DRG-STATE-EXEMPTED-INDIC                           02764
176200                                 TO HR-DRG-STATE-EXEMPTED-INDIC.  02765
176300     MOVE HRCR-DRG-MAJOR-DIAG-CATAGORY                            02766
176400                                  TO HR-DRG-MAJOR-DIAG-CATAGORY.  02767
176500     IF   HRCR-DRG-CODE-INTENSITY-WEIGHT  NUMERIC                 02768
176600          MOVE HRCR-DRG-CODE-INTENSITY-WEIGHT                     02769
176700                                TO HR-DRG-CODE-INTENSITY-WEIGHT   02770
176800     ELSE                                                         02771
176900          MOVE ZEROES        TO HR-DRG-CODE-INTENSITY-WEIGHT.     02772
177000     MOVE HRCR-DRG-CLM-CLASS-INDIC                                02773
177100                                     TO HR-DRG-CLM-CLASS-INDIC.   02774
177200     IF   HRCR-DRG-OL-DAYS NUMERIC                                02775
177300          MOVE HRCR-DRG-OL-DAYS      TO HR-DRG-OL-DAYS            02776
177400     ELSE                                                         02777
177500          MOVE ZEROES                TO HR-DRG-OL-DAYS.           02778
177600     IF HRCR-TOT-HOSP-BILL NUMERIC                                02779
177700        MOVE HRCR-TOT-HOSP-BILL      TO HR-TOT-HOSP-BILL          02780
177800     ELSE                                                         02781
177900        MOVE ZEROES                  TO HR-TOT-HOSP-BILL.         02782
178000     IF   HRCR-TOT-CHGD-TO-PAT  NUMERIC                           02783
178100          MOVE HRCR-TOT-CHGD-TO-PAT  TO HR-TOT-CHGD-TO-PAT        02784
178200     ELSE                                                         02785
178300          MOVE ZEROES                TO HR-TOT-CHGD-TO-PAT.       02786
178400     MOVE HRCR-SEX-IND               TO HR-SEX-IND.               02787
178500     MOVE HRCR-SPEC-PROCESS-IND      TO HR-SPEC-PROCESS-IND.      02788
178600     MOVE HRCR-DISCHARGE-DATE        TO HR-DISCHARGE-DATE.        02789
178700     MOVE HRCR-HOSP-CDE              TO HR-HOSP-CDE.              02790
178800     MOVE HRCR-ALBANY-SUPP-IND       TO HR-ALBANY-SUPP-IND.       02791
178900     MOVE HRCR-ECR-IND               TO HR-ECR-IND.               02792
179000     MOVE HRCR-SUPPS-IND             TO HR-SUPPS-IND.             02793
179100     MOVE HRCR-SVCNG-ACCT-IND        TO HR-SVCNG-ACCT-IND.        02794
179200     MOVE HRCR-COBRA-IND             TO HR-COBRA-IND.             02795
179300     MOVE HRCR-ERRCDE                TO HR-ERRCDE.                02796
179400     MOVE HRCR-REFUND-STAT-COB       TO HR-REFUND-STAT-COB.       02797
179500     IF   HRCR-ECR-NO-PACKED NUMERIC                              02798
179600          MOVE  HRCR-ECR-NO-PACKED TO HR-RIDER-PACK-NO            02799
179700     ELSE                                                         02800
179800          MOVE  ZEROES             TO HR-RIDER-PACK-NO.           02801
179900     MOVE HRCR-CASE-CTR              TO HR-CASE-CTR.              02802
180000     MOVE HRCR-CBC-BLOOD-IND         TO HR-CBC-BLOOD-IND.         02803
180100     MOVE HRCR-PATIENT-DIED-IND      TO HR-PATIENT-DIED-IND.      02804
180200     MOVE HRCR-SC-DEDUCT-IND         TO HR-SC-DEDUCT-IND.         02805
180300     MOVE HRCR-SC-ALLOW-IND          TO HR-SC-ALLOW-IND.          02806
180400     MOVE HRCR-PRESSO-IND            TO HR-PRESSO-IND.            02807
180500     MOVE HRCR-ZIP-CODE              TO HR-ZIP-CODE.              02808
180600     MOVE HRCR-ICD9-DIAG-CODE        TO HR-ICD9-DIAG-CODE.        02809
180700     MOVE HRCR-PATIENT-BIRTH-MONTH-DAY                            02810
180800                                   TO HR-PATIENT-BIRTH-MONTH-DAY. 02811
180900     MOVE HRCR-SECONDARY-SUB-ID      TO HR-SECONDARY-SUB-ID.      02812
181000     MOVE HRCR-MAIN-OPERATING-RM-IND                              02813
181100                             TO HR-MAIN-OPERATING-RM-IND.         02814
181200     MOVE HRCR-OP-TREATMENT-CODE     TO HR-OP-TREATMENT-CODE.     02815
181300     MOVE HRCR-DIAG-CLASSIFICATION-CODE                           02816
181400                                  TO HR-DIAG-CLASSIFICATION-CODE. 02817
181500     IF HRCR-NON-POOL-PAYMT     NUMERIC                           02818
181600        MOVE HRCR-NON-POOL-PAYMT     TO HR-NON-POOL-PAYMT         02819
181700     ELSE                                                         02820
181800        MOVE ZEROES                  TO HR-NON-POOL-PAYMT.        02821
181900     MOVE HRCR-FLEX-COST-SHARE-IND   TO HR-FLEX-COST-SHARE-IND.   02822
182000     IF   HRCR-FLEX-DEDUCT-AMT  NUMERIC                           02823
182100          MOVE  HRCR-FLEX-DEDUCT-AMT  TO HR-FLEX-DEDUCT-AMT       02824
182200     ELSE                                                         02825
182300          MOVE  ZEROES                TO HR-FLEX-DEDUCT-AMT.      02826
182400     IF HRCR-FLEX-CO-PAY-AMT   NUMERIC                            02827
182500        MOVE  HRCR-FLEX-CO-PAY-AMT    TO HR-FLEX-CO-PAY-AMT       02828
182600     ELSE                                                         02829
182700        MOVE  ZEROES                  TO HR-FLEX-CO-PAY-AMT.      02830
182800     IF   HRCR-FLEX-CO-INSUR-AMT    NUMERIC                       02831
182900          MOVE HRCR-FLEX-CO-INSUR-AMT   TO HR-FLEX-CO-INSUR-AMT   02832
183000     ELSE                                                         02833
183100          MOVE ZEROES                 TO HR-FLEX-CO-INSUR-AMT.    02834
183200     MOVE HRCR-PRIM-DTE-OF-SURG      TO HR-PRIM-DTE-OF-SURG.      02835
183300     MOVE HRCR-PRIM-PROCEDURE-CODE   TO HR-PRIM-PROCEDURE-CODE.   02836
183400     IF    HRCR-DEDUCTIBLE-AMT  NUMERIC                           02837
183500           MOVE HRCR-DEDUCTIBLE-AMT    TO HR-DEDUCTIBLE-AMT       02838
183600     ELSE                                                         02839
183700           MOVE ZEROES                 TO HR-DEDUCTIBLE-AMT.      02840
183900     IF   HRCR-OTHER-DEDUCTIBLE-AMT  NUMERIC                      02841
184000      MOVE HRCR-OTHER-DEDUCTIBLE-AMT  TO HR-OTHER-DEDUCTIBLE-AMT  02842
184100     ELSE                                                         02843
184200          MOVE ZEROES       TO    HR-OTHER-DEDUCTIBLE-AMT.        02844
184300     MOVE HRCR-CORE-SYS-ORIG-PLAN    TO HR-CORE-SYS-ORIG-PLAN.    02845
184400     MOVE HRCR-CORE-SYS-CLAIM-NO     TO                           02846
184500                           HR-CORE-SYS-CLAIM-NO.                  02847
184600     IF   HRCR-ADJUST-NO  NUMERIC                                 02848
184700          MOVE HRCR-ADJUST-NO  TO  HR-ADJUST-NO                   02849
184800     ELSE                                                         02850
184900          MOVE ZEROES          TO  HR-ADJUST-NO.                  02851
185000     MOVE HRCR-MAJOR-MEDICAL-IND     TO HR-MAJOR-MEDICAL-IND.     02852
185100     MOVE HRCR-CORE-SYS-LOCAL-PROV-ID TO                          02853
185200                                  HR-CORE-SYS-LOCAL-PROV-ID.      02854
185300     MOVE HRCR-MEDICARE-PROV-NO       TO HR-MEDICARE-PROV-NO.     02855
185400*-CR#151788 DEPENDENT NUMBER CHANGE START HERE
184000     MOVE HRCR-DEPENDENT-NUMBER       TO HR-DEPENDENT-NUMBER.     02085
185400*-CR#151788 CHANGE ENDS HERE
185400     IF   HRCR-OTHER-CO-INS-AMOUNT  NUMERIC                       02856
185500         MOVE HRCR-OTHER-CO-INS-AMOUNT TO HR-OTHER-CO-INS-AMOUNT  02857
185600     ELSE                                                         02858
185700       MOVE ZEROES                   TO HR-OTHER-CO-INS-AMOUNT.   02859
185800     MOVE HRCR-PATIENT-FIRST-NAME    TO HR-PATIENT-FIRST-NAME.    02860
185900     MOVE HRCR-OLD-ERG-IND           TO HR-OLD-ERG-IND.           02861
186000     MOVE HRCR-POS-NETWORK-IND       TO HR-POS-NETWORK-IND.       02862
186100     IF   HRCR-PADDE-PATIENT-ID  NUMERIC                          02863
186200          MOVE HRCR-PADDE-PATIENT-ID  TO HR-PADDE-PATIENT-ID      02864
186300     ELSE                                                         02865
186400          MOVE ZEROES                 TO HR-PADDE-PATIENT-ID.     02866
186500     MOVE HRCR-PRIMARY-CARE-PHYSICIAN                             02867
186600                                TO  HR-PRIMARY-CARE-PHYSICIAN.    02868
186700     MOVE  HRCR-DRG-ALT-LEVEL-CARE TO  HR-DRG-ALT-LEVEL-CARE.     02869
186800     IF    HRCR-DRG-ALT-CARE-YYY       NUMERIC                    02870
186900           MOVE  HRCR-DRG-ALT-CARE-YYY                            02871
186900                            TO  HR-DRG-ALT-CARE-YYY               02872
187000     ELSE                                                         02873
187100           MOVE  ZEROES     TO  HR-DRG-ALT-CARE-YYY.              02874
186800     IF    HRCR-DRG-ALT-CARE-DDD       NUMERIC                    02875
186900           MOVE  HRCR-DRG-ALT-CARE-DDD                            02876
186900                            TO  HR-DRG-ALT-CARE-DDD               02877
187000     ELSE                                                         02878
187100           MOVE  ZEROES     TO  HR-DRG-ALT-CARE-DDD.              02879
187200     MOVE  HRCR-DRG-DIAGNOSIS-2    TO  HR-DRG-DIAGNOSIS-2.        02880
187300     MOVE  HRCR-DRG-DIAGNOSIS-3    TO  HR-DRG-DIAGNOSIS-3.        02881
187400     MOVE  HRCR-DRG-DIAGNOSIS-4    TO  HR-DRG-DIAGNOSIS-4.        02882
187500     MOVE  HRCR-DRG-DIAGNOSIS-5    TO  HR-DRG-DIAGNOSIS-5.        02883
187600     MOVE  HRCR-DRG-PROCEDURE-2    TO  HR-DRG-PROCEDURE-2.        02884
187700     MOVE  HRCR-DRG-PROC-DATES-2   TO  HR-DRG-PROC-DATES-2.       02885
187800     MOVE  HRCR-DRG-PROCEDURE-3    TO  HR-DRG-PROCEDURE-3.        02886
187900     MOVE  HRCR-DRG-PROC-DATES-3   TO  HR-DRG-PROC-DATES-3.       02887
188000     MOVE  HRCR-DRG-DISCHARGE-STATUS TO  HR-DRG-DISCHARGE-STATUS. 02888
188100     IF    HRCR-DRG-TYPE-ADMISSION  NUMERIC                       02889
188200        MOVE  HRCR-DRG-TYPE-ADMISSION  TO  HR-DRG-TYPE-ADMISSION  02890
188300     ELSE                                                         02891
188400        MOVE  ZEROES      TO  HR-DRG-TYPE-ADMISSION.              02892
188500     MOVE  HRCR-UBF-ADMIT-DIAG-CODE TO  HR-UBF-ADMIT-DIAG-CODE.   02893
188600     MOVE  HRCR-PMT-VOUCHER-ERROR-CODE                            02894
188700                                   TO  HR-PMT-VOUCHER-ERROR-CODE. 02895
188800     MOVE  HRCR-CORE-PROCESS-IND   TO  HR-CORE-PROCESS-IND.       02896
188900     MOVE  HRCR-TEAM-CARE-REPORTING-IND                           02897
189000                          TO  HR-TEAM-CARE-REPORTING-IND.         02898
189100     MOVE  HRCR-TC-INDICATOR         TO  HR-TC-INDICATOR.         02899
189200     MOVE  HRCR-TC-COINS-MOD         TO  HR-TC-COINS-MOD.         02900
      ***** ERIC FIELDS     ********************************************02901
189300     MOVE  HRCR-SORT-RIDER-CODE    TO  HRCR-RIDER-CODE.           02902
189400     MOVE  HRCR-CONVRTD-RIDER-IND  TO  HRCR-CONVERTED-RIDER-IND.  02903
189400     MOVE  HRCR-SORT-GROUP-NUMBER  TO  SEL-HR-SORT-GROUP-NO.      02904
189400     MOVE  WS-CRT                  TO  SEL-HR-CONTROL-IND.        02905
      ***** ADDED NEW FIELDS FOR CONVERSION   **************************02906
      ***** AS OF 01/04/1994               *****************************02907
189300     MOVE  HRCR-EMHC-PROCESSING-IND  TO  HR-EMHC-PROCESSING-IND.  02908
189300     MOVE  HRCR-DIAG-CODE-265        TO  HR-DIAG-CODE-265.        02909
189300     IF    HRCR-TC-PENALTY-CODE   NUMERIC                         02910
189300           MOVE  HRCR-TC-PENALTY-CODE  TO  HR-TC-PENALTY-CODE     02911
189300     ELSE                                                         02912
189300           MOVE  ZEROES                TO  HR-TC-PENALTY-CODE.    02913
189300     MOVE  HRCR-TC-PENALTY-INDICATOR TO  HR-TC-PENALTY-INDICATOR. 02914
189300     MOVE  HRCR-ERG-IND              TO  HR-ERG-IND.              02915
189300     MOVE  HRCR-FINAN-IND            TO  HR-FINAN-IND.            02916
189300     MOVE  HRCR-MATERNITY            TO  HR-MATERNITY.            02917
189300     MOVE  HRCR-HOSPICE-IND          TO  HR-HOSPICE-IND.          02918
189300     MOVE  HRCR-RATE-TYPE-IND        TO  HR-RATE-TYPE-IND.        02919
189400     MOVE  HRCR-ERIC-GROUP-NO        TO  HR-ERIC-GROUP-NO.        02920
189400     MOVE  HRCR-ERIC-SUB-DIV         TO  HR-ERIC-SUB-DIV.         02921
189300     MOVE  HRCR-REMUN-SAVINGS-CODE-1 TO  HR-REMUN-SAVINGS-CODE-1. 02922
189300     MOVE  HRCR-REMUN-SAVINGS-CODE-2 TO  HR-REMUN-SAVINGS-CODE-2. 02923
189300     MOVE  HRCR-REMUN-SAVINGS-CODE-3 TO  HR-REMUN-SAVINGS-CODE-3. 02924
188100     IF    HRCR-REMUN-COVERED-CHARGES NUMERIC                     02925
189300           MOVE  HRCR-REMUN-COVERED-CHARGES                       02926
189300                                 TO  HR-REMUN-COVERED-CHARGES     02927
188300     ELSE                                                         02928
189300           MOVE  ZEROES          TO  HR-REMUN-COVERED-CHARGES.    02929
188100     IF    HRCR-REMUN-SAVINGS-AMT-1   NUMERIC                     02930
189300           MOVE  HRCR-REMUN-SAVINGS-AMT-1                         02931
189300                                 TO  HR-REMUN-SAVINGS-AMT-1       02932
188300     ELSE                                                         02933
189300           MOVE  ZEROES          TO  HR-REMUN-SAVINGS-AMT-1.      02934
188100     IF    HRCR-REMUN-SAVINGS-AMT-2   NUMERIC                     02935
189300           MOVE  HRCR-REMUN-SAVINGS-AMT-2                         02936
189300                                 TO  HR-REMUN-SAVINGS-AMT-2       02937
188300     ELSE                                                         02938
189300           MOVE  ZEROES          TO  HR-REMUN-SAVINGS-AMT-2.      02939
188100     IF    HRCR-REMUN-SAVINGS-AMT-3   NUMERIC                     02940
189300           MOVE  HRCR-REMUN-SAVINGS-AMT-3                         02941
189300                                 TO  HR-REMUN-SAVINGS-AMT-3       02942
188300     ELSE                                                         02943
189300           MOVE  ZEROES          TO  HR-REMUN-SAVINGS-AMT-3.      02944
188100     IF  HRCR-ADMIN-EXPENSE         NUMERIC                       02945
188200        MOVE HRCR-ADMIN-EXPENSE   TO  HR-ADMIN-EXPENSE            02946
188300     ELSE                                                         02947
188400        MOVE  ZEROES              TO   HR-ADMIN-EXPENSE.          02948
188100     IF  HRCR-ACCESS-FEE            NUMERIC                       02949
189300         MOVE  HRCR-ACCESS-FEE           TO  HR-ACCESS-FEE        02950
188100     ELSE                                                         02951
189300         MOVE  ZEROES                    TO  HR-ACCESS-FEE.       02952
189300     MOVE  HRCR-SC-CROSOVR-IND       TO  HR-SC-CROSOVR-IND.       02953
189300     MOVE  HRCR-PGP                  TO  HR-PGP.                  02954
189300     MOVE  HRCR-CHARGE-REGISTER-IND  TO  HR-CHARGE-REGISTER-IND.  02955
189300     MOVE  HRCR-PRODUCT-CODE         TO  HR-PRODUCT-CODE.         02956
189300     MOVE  HRCR-NEGOTD-RATE-IND      TO  HR-NEGOTD-RATE-IND.      02957
189300     IF    HRCR-NEGOTD-NET-SAVINGS-AMT NUMERIC                    02958
189300           MOVE HRCR-NEGOTD-NET-SAVINGS-AMT TO                    02959
189300                HR-NEGOTD-NET-SAVINGS-AMT                         02960
189300     ELSE                                                         02961
189300           MOVE ZEROES TO  HR-NEGOTD-NET-SAVINGS-AMT.             02962
      ***** ADDED NEW FIELDS FOR CONVERSION   **************************02963
      ***** AS OF 05/01/1994               *****************************02964
189300     MOVE  HRCR-REMUN-PAYROLL-LOC    TO  HR-REMUN-PAYROLL-LOC.    02965
189300     MOVE  HRCR-GROUP-QUARTILE       TO  HR-GROUP-QUARTILE.       02966
189300     MOVE  HRCR-GROUP-TIER           TO  HR-GROUP-TIER.           02967
189300     MOVE  HRCR-GROUP-COUNTY         TO  HR-GROUP-COUNTY.         02968
189300     MOVE  HRCR-TOPPS-GROUP-NO       TO  HR-TOPPS-GROUP-NO.       02969
189300     MOVE  HRCR-TOPPS-TREATM-TYPE    TO  HR-TOPPS-TREATM-TYPE.    02970
189300     MOVE  HRCR-TOPPS-BENEFIT-PKG    TO  HR-TOPPS-BENEFIT-PKG.    02971
189300     MOVE  HRCR-TOPPS-NETWORK-CHOICE TO  HR-TOPPS-NETWORK-CHOICE. 02972
189300     MOVE  HRCR-SPC-CHK-PAY-SUBS-IND TO  HR-SPC-CHK-PAY-SUBS-IND. 02973
189300     MOVE  HRCR-DED-CARRYOVER-IND    TO  HR-DED-CARRYOVER-IND.    02974
189300     MOVE  HRCR-ORIGINAL-CLAIM-IND   TO  HR-ORIGINAL-CLAIM-IND.   02975
189300     MOVE  HRCR-ADJUSTM-REASON-CODE  TO HR-ADJUSTM-REASON-CODE.   02976
189300     MOVE  HRCR-ADM-RCVD-YR              TO HR-ADM-RCVD-YR.       02977
189300     MOVE  HRCR-ADM-RCVD-MO              TO HR-ADM-RCVD-MO.       02978
189300     MOVE  HRCR-ADM-RCVD-DA              TO HR-ADM-RCVD-DA.       02979
189300     MOVE  HRCR-CHGBK-TO-AHS-IND         TO HR-CHGBK-TO-AHS-IND.  02980
189300     IF  HRCR-CORE-SYS-FIN-DAYS    NUMERIC                        02981
189300         MOVE  HRCR-CORE-SYS-FIN-DAYS    TO HR-CORE-SYS-FIN-DAYS  02982
189300     ELSE                                                         02983
189300         MOVE  ZEROES                    TO HR-CORE-SYS-FIN-DAYS. 02984
189300     IF  HRCR-FINANCIAL-CASES      NUMERIC                        02985
189300         MOVE  HRCR-FINANCIAL-CASES      TO HR-FINANCIAL-CASES    02986
189300     ELSE                                                         02987
189300         MOVE  ZEROES                    TO HR-FINANCIAL-CASES.   02988
189300     MOVE  HRCR-DIAG-CDE                 TO HR-DIAG-CDE.          02989
           MOVE  HRCR-NEW-BORN-CODE            TO HR-NEW-BORN-CODE.     02990
039300     MOVE  HRCR-PLS-PROG-CD         TO HR-PLS-PROG-CD.            02991
039300     MOVE  HRCR-PLS-UWRT-CORP-CD    TO HR-PLS-UWRT-CORP-CD.       02992
039300     MOVE  HRCR-PLS-LOB-CD          TO HR-PLS-LOB-CD.             02993
039300     MOVE  HRCR-PLS-CLS-RISK-CD     TO HR-PLS-CLS-RISK-CD.        02994
039300     MOVE  HRCR-PLS-RISK-POOL-CD    TO HR-PLS-RISK-POOL-CD.       02995
039300     MOVE  HRCR-PLS-CONTRACT-TYP-CD TO HR-PLS-CONTRACT-TYP-CD.    02996
039300     IF    HRCR-PLS-CONTRACT-NO  NUMERIC                          02997
039300           MOVE  HRCR-PLS-CONTRACT-NO  TO HR-PLS-CONTRACT-NO      02998
039300     ELSE                                                         02999
039300           MOVE  ZEROES                TO HR-PLS-CONTRACT-NO.     03000
039300     MOVE  HRCR-PLS-COV-TYP-CD      TO HR-PLS-COV-TYP-CD.         03001
039300     MOVE  HRCR-PLS-CATEG-NO        TO HR-PLS-CATEG-NO.           03002
039300     MOVE  HRCR-PLS-BENE-LEV-NO     TO HR-PLS-BENE-LEV-NO.        03003
039300     MOVE  HRCR-PLS-REP-CODE        TO HR-PLS-REP-CODE.           03004
039300     MOVE  HRCR-PLS-PROD-COMB-CODE  TO HR-PLS-PROD-COMB-CODE.     03005
039300     MOVE  LOW-VALUES               TO HR-PLS-MARKET-SEGMENT.     03006
                                                                        03007
           MOVE  HRCR-AGENCY-NO           TO HR-PLS-AGENCY-NUMBER.      03013
           MOVE  HRCR-PLS-GRP-KEY-ID      TO HR-PLS-GRP-KEY-ID.         03014
                                                                        03015
      ***** ADDED NEW FIELDS AS OF  12/96     **************************03016
039300     IF    HRCR-DRG-IL-POOL-A-PYMT  NUMERIC                       03017
039300          MOVE  HRCR-DRG-IL-POOL-A-PYMT TO HR-DRG-IL-POOL-A-PYMT  03018
039300     ELSE                                                         03019
039300          MOVE  ZEROES           TO HR-DRG-IL-POOL-A-PYMT.        03020
039300     IF    HRCR-DRG-OL-POOL-A-PYMT  NUMERIC                       03021
039300           MOVE  HRCR-DRG-OL-POOL-A-PYMT  TO HR-DRG-OL-POOL-A-PYMT03022
039300     ELSE                                                         03023
039300           MOVE  ZEROES            TO HR-DRG-OL-POOL-A-PYMT.      03024
039300     IF    HRCR-DRG-IL-POOL-B-PYMT     NUMERIC                    03025
039300           MOVE  HRCR-DRG-IL-POOL-B-PYMT                          03026
039300                                   TO HR-DRG-IL-POOL-B-PYMT       03027
039300     ELSE                                                         03028
039300           MOVE  ZEROES            TO HR-DRG-IL-POOL-B-PYMT.      03029
039300     MOVE  HRCR-POOL-REGION-CODE    TO WS-POOL-REGION-CODE.       03030
039300     MOVE  WS-POOL-REGION-CODE-2    TO HR-POOL-REGION-CODE.       03031
039300     MOVE  HRCR-GME-GROUP-IND       TO HR-GME-GROUP-IND.          03032
039300     IF    HRCR-GME-SUBSCRIBER-COUNT NUMERIC                      03033
039300           MOVE  HRCR-GME-SUBSCRIBER-COUNT                        03034
039300                                     TO HR-GME-SUBSCRIBER-COUNT   03035
039300     ELSE                                                         03036
039300           MOVE ZEROES               TO HR-GME-SUBSCRIBER-COUNT.  03037
039300     MOVE  HRCR-BLUE-SHIELD-PLAN-CODE TO                          03038
039300                              HR-BLUE-SHIELD-PLAN-CODE.           03039
      ***** ADDED NEW FIELDS AS OF  05/97     **************************03040
039300     IF    HRCR-DRG-IL-POOL-C-PYMT  NUMERIC                       03041
039300          MOVE  HRCR-DRG-IL-POOL-C-PYMT TO HR-DRG-IL-POOL-C-PYMT  03042
039300     ELSE                                                         03043
039300          MOVE  ZEROES           TO HR-DRG-IL-POOL-C-PYMT.        03044
039300     IF    HRCR-DRG-IL-POOL-D-PYMT  NUMERIC                       03045
039300           MOVE  HRCR-DRG-IL-POOL-D-PYMT  TO HR-DRG-IL-POOL-D-PYMT03046
039300     ELSE                                                         03047
039300           MOVE  ZEROES            TO HR-DRG-IL-POOL-D-PYMT.      03048
039300     MOVE  HRCR-CAPITATION-IND     TO HR-CAPITATION-IND.          03049
039300******    ADDED 9/97         **********************************   03050
039300     MOVE  HRCR-MSK-CATEGORY       TO HR-MSK-CATEGORY.            03051
039300     MOVE  HRCR-ACCTG-REPORT-IND   TO HR-ACCTG-REPORT-IND.        03052
039300     MOVE  HRCR-BYPASS-SUPP-AND-POOL-IND                          03053
039300                          TO HR-BYPASS-SUPP-AND-POOL-IND.         03054
           IF DESTID-DATE  > WS-DEST-START-DATE
              MOVE HRCR-DEST-ID    TO HR-DEST-ID
           ELSE
039300        MOVE  HRCR-TOPPS-PROGRAM-NUM    TO HR-TOPPS-PROGRAM-NUM   02305
039300        MOVE  HRCR-TOPPS-SUB-PROG-NUM   TO HR-TOPPS-SUB-PROG-NUM. 02307
039300     IF    HRCR-MCARE-INTEREST-AMT NUMERIC                        03059
039300           MOVE  HRCR-MCARE-INTEREST-AMT                          03060
039300                          TO HR-MCARE-INTEREST-AMT                03061
039300     ELSE                                                         03062
039300           MOVE ZEROES    TO HR-MCARE-INTEREST-AMT.               03063
039300     MOVE  HRCR-TOPPS-MARKET-SEGMENT                              03064
039300                          TO HR-TOPPS-MARKET-SEGMENT.             03065
           PERFORM BA-1440-CHECK-OUTPUT-FIELDS.                         03066
      ***** ADDED NEW FIELDS AS OF  01/98     **************************03067
039300     IF    HRCR-DRG-OL-POOL-B-PYMT  NUMERIC                       03068
039300          MOVE  HRCR-DRG-OL-POOL-B-PYMT TO HR-DRG-OL-POOL-B-PYMT  03069
039300     ELSE                                                         03070
039300          MOVE  ZEROES           TO HR-DRG-OL-POOL-B-PYMT.        03071
039300     IF    HRCR-DRG-OL-POOL-C-PYMT  NUMERIC                       03072
039300          MOVE  HRCR-DRG-OL-POOL-C-PYMT TO HR-DRG-OL-POOL-C-PYMT  03073
039300     ELSE                                                         03074
039300          MOVE  ZEROES           TO HR-DRG-OL-POOL-C-PYMT.        03075
039300     IF    HRCR-DRG-OL-POOL-D-PYMT  NUMERIC                       03076
039300          MOVE  HRCR-DRG-OL-POOL-D-PYMT TO HR-DRG-OL-POOL-D-PYMT  03077
039300     ELSE                                                         03078
039300          MOVE  ZEROES           TO HR-DRG-OL-POOL-D-PYMT.        03079
039300     MOVE  HRCR-SCHEDULE-IND      TO HR-SCHEDULE-IND.             03080
            IF   HRCR-NO-DAYS-PROMPT-PAY-INT   NUMERIC                  03081
                 MOVE HRCR-NO-DAYS-PROMPT-PAY-INT   TO                  03082
                                 HR-NO-DAYS-PROMPT-PAY-INT              03083
            ELSE                                                        03084
                 MOVE ZEROES TO HR-NO-DAYS-PROMPT-PAY-INT.              03085
            MOVE HRCR-CLK-START-DAT-PROM-PAY TO                         03086
                 HR-CLK-START-DAT-PROM-PAY.                             03087
            MOVE HRCR-EMHC-IND  TO  HR-EMHC-IND.                        03088
            IF   HRCR-OTHER-STATE-SURCHARGE    NUMERIC                  03089
                 MOVE HRCR-OTHER-STATE-SURCHARGE    TO                  03090
                                 HR-OTHER-STATE-SURCHARGE               03091
            ELSE                                                        03092
                 MOVE ZEROES TO HR-OTHER-STATE-SURCHARGE.               03093
            MOVE HRCR-SOURCE-CODE TO   HR-SOURCE-CODE.                  03094
            MOVE HRCR-SPC-CHK-IND TO   HR-SPC-CHK-IND.                  03095
            MOVE HRCR-PAY-TO-PROVIDER-IND  TO                           03096
                                 HR-PAY-TO-PROVIDER-IND.                03097
            MOVE HRCR-ITS-SCCF-SERIAL-NUMBER TO                         03098
                                 HR-ITS-SCCF-SERIAL-NUMBER.             03099
            MOVE HRCR-NASCO-BANK-ACCT-TYPE   TO                         03100
                                 HR-NASCO-BANK-ACCT-TYPE.               03101
            MOVE HRCR-PRODUCT-CLASSIF-CODE    TO                        03102
                                      HR-PRODUCT-CLASSIF-CODE.          03103
            MOVE HRCR-INTEREST-IND            TO                        03104
                                      HR-INTEREST-IND.                  03105
      ***** ADDED NEW FIELDS AS OF  03/00  BY E.V.  ********************02317
            MOVE HRCR-HOSP-CODE-1-6           TO                        02354
                                      HR-HOSP-CODE-1-6.                 02355
            IF   HRCR-PROVIDER-TAX-ID  NUMERIC                          02354
                 MOVE HRCR-PROVIDER-TAX-ID         TO                   02354
                                      HR-PROVIDER-TAX-ID                02355
            ELSE                                                        02354
                 MOVE ZEROES      TO  HR-PROVIDER-TAX-ID.               02354
      ***** ADDED NEW FIELDS AS OF  03/00  BY E.V.  ********************02317
145500     MOVE HRCR-ITSHOME-ECRP-IND TO  HR-ITSHOME-ECRP-IND.          01672
145500                                                                  01672
      ***** ADDED NEW FIELDS AS OF  10/00  BY E.V.  ********************02317
145500       MOVE HRCR-PRODUCT-VARIATION-CODE TO                        01672
145500                                        HR-PRODUCT-VARIATION-CODE.01672
145500     IF HRCR-CAP-PAY-VENDOR-AMT NUMERIC                           01672
145500        MOVE HRCR-CAP-PAY-VENDOR-AMT    TO HR-CAP-PAY-VENDOR-AMT  01672
145500                                           HR-MAGELLAN-RATE       01672
145500     ELSE                                                         01672
145500        MOVE WS-PLUS-ZERO               TO  HR-CAP-PAY-VENDOR-AMT 01672
145500                                            HR-MAGELLAN-RATE.     01672
      ***** ADDED NEW FIELDS AS OF  10/00  BY E.V.  ********************02317
           MOVE HRCR-PROFITABILITY-CODE        TO                       00634
                                               HR-PROFITABILITY-CODE.   00634
           MOVE HRCR-HI-PROFITABILITY-CODE     TO                       00634
                                               HR-HI-PROFITABILITY-CODE.00634
           MOVE HRCR-BOOK-OF-BUSINESS          TO                       00634
                                               HR-BOOK-OF-BUSINESS.     00634
           MOVE  HRCR-PACKAGE-NO       TO    HR-PACKAGE-NO.             00634
           MOVE HRCR-COLLECTION-GRPCDE-OLD   TO                         00634
                                        HR-COLLECTION-GRPCDE-OLD.       00634
           MOVE HRCR-PLS-UWRT-CORP-CD-OLD    TO                         00634
                                        HR-PLS-UWRT-CORP-CD-OLD.        00634
           MOVE HRCR-BC-PLAN-COVERAGE-CODE-OLD  TO                      00634
                                         HR-BC-PLAN-COVERAGE-CODE-OLD.  00634
           MOVE HRCR-BLUE-SHIELD-PLAN-CODE-OLD  TO                      00634
                                         HR-BLUE-SHIELD-PLAN-CODE-OLD.  00634
           IF   HRCR-TC-PENALTY-AMT NUMERIC                             00634
                MOVE HRCR-TC-PENALTY-AMT     TO   HR-TC-PENALTY-AMT     00634
           ELSE                                                         00634
                MOVE ZEROES                  TO   HR-TC-PENALTY-AMT.    00634
           MOVE HRCR-PRODUCT-FUND-CODE TO  HR-PRODUCT-FUND-CODE.        00634
           MOVE HRCR-RATING-COMB-CODE  TO  HR-RATING-COMB.              00634
           MOVE HRCR-PROVIDER-NAME      TO HR-PROVIDER-NAME.            00634
           MOVE HRCR-PROVIDER-ZIP-CODE  TO HR-PROVIDER-ZIP-CODE.        00634
           MOVE HRCR-MEDICAL-REC-18-30  TO HR-MEDICAL-REC-18-30.        00634
           MOVE HRCR-BILLING-IND        TO HR-BILLING-IND.              00634
           IF   HRCR-TOTAL-DIFF-AMT  NUMERIC                            00634
                MOVE HRCR-TOTAL-DIFF-AMT     TO HR-TOTAL-DIFF-AMT       00634
           ELSE                                                         00634
                MOVE ZEROES                  TO HR-TOTAL-DIFF-AMT.      00634
      **** ADDED 12/16/03 BY MAHESH FOR DM RECORDS *****************    02144
           MOVE HRCR-DISEASE-MGMT-IND   TO  HR-DISEASE-MGMT-IND.        02356
           IF   HRCR-DMG-AMERICAN-HEALTHWAY   OR                        02356
                HRCR-DMG-ACCORDANT            OR                        02356
                HRCR-DMG-RMS                                            02356
                MOVE HRCR-AHS-PAYMT          TO  HR-DISEASE-MGMT-RATE   02356

           ELSE                                                         02356
                MOVE ZEROS                   TO  HR-DISEASE-MGMT-RATE.  02356
           MOVE HRCR-HIPAA-ALT-ID       TO  HR-HIPAA-ALT-ID.            02356
           IF   HRCR-CDHP-HRA-AMOUNT  NUMERIC                           02356
                MOVE HRCR-CDHP-HRA-AMOUNT    TO  HR-CDHP-HRA-AMOUNT     02356
           ELSE                                                         02356
                MOVE ZEROES                  TO  HR-CDHP-HRA-AMOUNT.    02356
      **** MULTIPURSE CHANGES START                                     02356
           IF   HRCR-CDHP-HSA-AMOUNT  NUMERIC                           02356
                MOVE HRCR-CDHP-HSA-AMOUNT    TO  HR-CDHP-HSA-AMOUNT     02356
           ELSE                                                         02356
                MOVE ZEROES                  TO  HR-CDHP-HSA-AMOUNT.    02356
      *                                                                 02356
           MOVE HRCR-CDHP-IND           TO  HR-CDHP-IND.                02356
      **** MULTIPURSE CHANGES END                                       02356
      * DONE AS PART OF CR#117642-WP MARKET SEGMENTATION
           MOVE  HRCR-WP-MKT-SEGMENT      TO HR-PLS-WP-MKT-SEGMENT.
      *
           IF   HRCR-SIC-CODE  NUMERIC
                MOVE HRCR-SIC-CODE    TO  HR-SIC-CODE
           ELSE
                MOVE ZEROES                  TO  HR-SIC-CODE.
      * NPI CHANGES STARTS HERE.
           MOVE HRCR-NPI-CODE    TO  HR-NPI-CODE.
      * NPI CHANGES ENDS HERE.
      * RSI INDICATOR CHANGES STARTS HERE.
           MOVE HRCR-RSI-IND     TO  HR-RSI-IND.
      * RSI INDICATOR CHANGES ENDS HERE.
      * NARROW NETWORK INDICATOR CHANGES STARTS HERE.
           MOVE HRCR-NARROW-NET-IND     TO    HR-NARROW-NET-IND.
      * NARROW NETWORK INDICATOR CHANGES ENDS HERE.
      * BLOOM EXCHANGE INDICATOR CHANGES STARTS HERE.
           MOVE HRCR-BLOOM-EXCHANGE-IND     TO HR-BLOOM-EXCH-IND.
      * BLOOM EXCHANGE INDICATOR CHANGES ENDS HERE.
      * EFT INDICATOR CHANGES STARTS HERE.
           MOVE HRCR-EFT-IND                TO HR-EFT-IND.
      * EFT INDICATOR CHANGES ENDS HERE.
      * ICD10 PROJECT CHANGES STARTS HERE.
           MOVE HRCR-ICD10-PRIMARY-DIAG-CODE TO
                                      HR-ICD10-PRIMARY-DIAG-CODE.
           MOVE HRCR-ICD9-ICD10-IND         TO HR-ICD9-ICD10-IND.
           MOVE HRCR-ICD10-PRIMARY-PROC-CODE   TO
                                 HR-ICD10-PRIMARY-PROC-CODE.
           MOVE HRCR-ICD10-PRM-PROC-DATE       TO
                                 HR-ICD10-PRM-PROC-DATE.
           IF  HRCR-EXPND-CHECK-NUMBER IS NUMERIC
               MOVE HRCR-EXPND-CHECK-NUMBER
                                    TO HR-EXPND-CHECK-NUMBER
           ELSE MOVE ZEROES         TO HR-EXPND-CHECK-NUMBER.
      * ICD10 PROJECT CHANGES ENDS HERE.
      * ITS PROJECT CHANGES STARTS HERE.
           IF  HRCR-ITS-SUPP-AMT IS NUMERIC
               MOVE HRCR-ITS-SUPP-AMT
                                    TO HR-ITS-SUPP-AMT
           ELSE MOVE ZEROES         TO HR-ITS-SUPP-AMT.
      * ITS PROJECT CHANGES ENDS HERE.
      * EBF PROJECT CHANGES STARTS HERE.
           MOVE HRCR-EBF-IND        TO HR-EBF-IND.
      * EBF PROJECT CHANGES ENDS HERE.
      * CHANGES FOR LOCAL ACCESS FEE PROJECT START.
           IF  HRCR-LOC-ACCESS-FEE IS NUMERIC
               MOVE HRCR-LOC-ACCESS-FEE
                                    TO HR-LOC-ACES-FEE
           ELSE MOVE ZEROES         TO HR-LOC-ACES-FEE.
      * CHANGES FOR LOCAL ACCESS FEE PROJECT END.
      * CHANGES FOR POOLING REPORT PROJECT START.
           MOVE HRCR-PLACE-OF-SERVICE       TO HR-PLACE-OF-SERVICE.
      * CHANGES FOR POOLING REPORT PROJECT END.
      * CHANGES FOR POOL P AMOUNT START.
           IF  HRCR-POOL-P-AMOUNT IS NUMERIC
               MOVE HRCR-POOL-P-AMOUNT
                                    TO HR-POOL-P-AMOUNT
           ELSE MOVE ZEROES         TO HR-POOL-P-AMOUNT.
      * CHANGES FOR POOL P AMOUNT END.
      * CHANGES FOR TOTAL CHARGES START.
           IF  HRCR-TOTAL-CHARGES-1 IS NUMERIC
               MOVE HRCR-TOTAL-CHARGES-1
                                    TO HR-TOT-HOSP-BILL-EXPANDED
           ELSE MOVE ZEROES         TO HR-TOT-HOSP-BILL-EXPANDED.
      * CHANGES FOR TOTAL CHARGES END.
      * CHANGES FOR PC2 PROJECT START
           MOVE HRCR-PC2-PROGRAM-CODE       TO HR-PC2-PROGRAM-CODE.
           MOVE HRCR-PC2-FUND-TYPE          TO HR-PC2-FUND-TYPE.
      * CHANGES FOR PC2 PROJECT END
      * CR#127537 IDENTIFY EMERGENCY ROOM CLAIMS FOR CT MED ADV-START
           MOVE HRCR-LINE-CATEGORY-CODE (1) TO HR-LINE-CATEGORY-CODE.
      * CR#127537 IDENTIFY EMERGENCY ROOM CLAIMS FOR CT MED ADV-END
      *CR#133719 -->
           MOVE HRCR-NIA-CAP-IND  TO HR-NIA-CAP-IND.
      *CR#133719 <--
      *CR#140269-NIA PRORATE AMOUNT CHANGES BEGINS.
           IF  HRCR-CAP-PAY-NIA-AMT IS NUMERIC
                MOVE HRCR-CAP-PAY-NIA-AMT TO
                             HR-CAP-PAY-NIA-AMT
           ELSE
                MOVE +0 TO  HR-CAP-PAY-NIA-AMT.
      *CR#140269-NIA PRORATE AMOUNT CHANGES ENDS.
      *CR#146587-MCS PROMPT PAY AMOUNT CHANGES BEGINS.
           IF  HRCR-MCS-PROMPT-PAY-INT IS NUMERIC
                MOVE HRCR-MCS-PROMPT-PAY-INT   TO
                             HR-MCS-PROMPT-PAY-INT
           ELSE
                MOVE +0 TO  HR-MCS-PROMPT-PAY-INT.
      *CR#146587-MCS PROMPT PAY AMOUNT CHANGES ENDS.
      **CR#160671-NCN PROJECT EXPANTION CHANGES BEGIN.
           MOVE HRCR-NCN-INDICATOR TO HR-NCN-INDICATOR.
           IF  HRCR-NCN-FEE IS NUMERIC
               MOVE HRCR-NCN-FEE TO HR-NCN-FEE
           ELSE
               MOVE +0 TO  HR-NCN-FEE.
           IF  HRCR-NCN-GRP-FEE IS NUMERIC
               MOVE HRCR-NCN-GRP-FEE TO HR-NCN-GROUP-FEE
           ELSE
               MOVE +0 TO  HR-NCN-GROUP-FEE.
      **CR#160671-NCN PROJECT EXPANTION CHANGES END.
      **CR#161042-APR DRG PROJECT CHANGES BEGIN.
           MOVE HRCR-PLS-DRG-SVY-IN TO HR-PLS-DRG-SVY-IN.
      **CR#161042-APR DRG PROJECT CHANGES END.
      * ACCESS FEE EXCLUSION START                                      02356
           PERFORM 1500-ASSIGN-ACCESS-FEE-IND THRU 1500-EXIT.           02356
      * ACCESS FEE EXCLUSION END                                        02356
      **************************************************************    02356
189600     EJECT                                                        03106
       BA-1405-GME-DATE-FIX.                                            03107
            IF WS-GME-DAY-COUNTER = GME-CURRENT-DAY                     03108
               MOVE ZEROES  TO WS-GME-DAY-COUNTER.                      03109
                                                                        03110
170000       IF   SEL-DETAIL-PAID-DAY  = WS-PREV-PAID-DAY               03111
                  ADD WS-ONE              TO  WS-GME-DAY-COUNTER        03112
                  MOVE WS-GME-DAY-COUNTER TO  SEL-DETAIL-PAID-DAY       03113
170000       ELSE                                                       03114
170000            MOVE SEL-DETAIL-PAID-DAY TO WS-PREV-PAID-DAY.         03115
158700     SKIP3                                                        03116
189700 BA-1430-ATTACH-INDICATORS.                                       03117
189800***************************************************************   03118
189900*    PURPOSE :                                                *   03119
190000*            MOVE DESCRIPTIVE INDICATORS BASED UPON THE INPUT *   03120
190100*             FILE DESCRIPTION 88 LEVEL INDICATORS            *   03121
190200***************************************************************   03122
190300     SKIP3                                                        03123
C21TSR*    IF HRCR-ACTN-YR           LESS THAN WS-90                    03124
                                                                        03125
                                                                        03126
C21TSR     CALL  C2140C02  USING HRCR-ACTN-YR C21HRCR-ACTN-YR           03127
C21TSR               C21-BASE-ON C21-WORK-AREA                          03128
C21TSR     CALL  C2140C02  USING WS-90 C21WS-90 C21-BASE-ON             03129
C21TSR               C21-WORK-AREA                                      03130
C21TSR     IF C21HRCR-ACTN-YR LESS THAN C21WS-90                        03131
190500         MOVE WS-LITERAL-A TO SEL-CLAIM-SERVICE-CATEGORY          03132
190600         IF (HRCR-ALB-INVALID-BEFORE OR                           03133
190700             HRCR-ALB-INVALID-NEVER)                              03134
190800             MOVE HRCR-ALBANY-INVALID-CONV                        03135
190900                  TO SEL-CLAIM-SERVICE-CATEGORY                   03136
191000         ELSE CONTINUE                                            03137
191100     ELSE                                                         03138
191200         MOVE SPACE TO SEL-CLAIM-SERVICE-CATEGORY.                03139
191300     SKIP3                                                        03140
191400         IF HRCR-PRESSO                                           03141
191500             MOVE WS-ONE       TO SEL-ICHIS-PRESSO-FLAG           03142
191600          ELSE                                                    03143
191700             MOVE WS-ZERO      TO SEL-ICHIS-PRESSO-FLAG.          03144
191800     SKIP3                                                        03145
191900         IF HRCR-RIDER                                            03146
192000             MOVE WS-ONE       TO SEL-ICHIS-RIDER-FLAG            03147
192100          ELSE                                                    03148
192200             MOVE WS-ZERO      TO SEL-ICHIS-RIDER-FLAG.           03149
192300     SKIP3                                                        03150
192400         IF HRCR-SUPP                                             03151
192500             MOVE WS-ONE   TO SEL-ICHIS-SUPP-PAYMENT-FLAG         03152
192500             ADD  WS-ONE   TO WS-ICHIS-SUPP-RECORDS               03153
172000             ADD  HRCR-AHS-PAYMT   TO WS-ICHIS-SUPP-TOTALS        03154
192600          ELSE                                                    03155
192700             MOVE WS-ZERO  TO SEL-ICHIS-SUPP-PAYMENT-FLAG.        03156
192800     SKIP2                                                        03157
192900     MOVE HRCR-POS-NETWORK-IND TO SEL-POS-INDICATOR.              03158
193000     EJECT                                                        03159
       BA-1440-CHECK-OUTPUT-FIELDS.                                     03160
           IF SEL-DETAIL-CLAIM-NUMBER EQUAL SPACES                      03161
574500       MOVE WS-FOURTEEN-ZEROS TO SEL-DETAIL-CLAIM-NUMBER.         03162
575500     SKIP2                                                        03163
575100     IF SEL-SUBSCRIBER-ID EQUAL SPACES                            03164
575300        MOVE WS-FOURTEEN-ZEROS TO SEL-SUBSCRIBER-ID.              03165
575500     SKIP2                                                        03166
575600     IF SEL-DETAIL-PAID-DAY NOT NUMERIC                           03167
576300        MOVE CURRENT-DAY      TO SEL-DETAIL-PAID-DAY.             03168
576000     SKIP2                                                        03169
575600     IF SEL-DETAIL-PAID-DAY = WS-00                               03170
576300        MOVE CURRENT-DAY      TO SEL-DETAIL-PAID-DAY.             03171
576000     SKIP2                                                        03172
576100     IF SEL-DETAIL-INCURRED-DAY NOT NUMERIC                       03173
576300        MOVE WS-LITERAL-01  TO SEL-DETAIL-INCURRED-DAY.           03174
576500     SKIP2                                                        03175
576100     IF SEL-DETAIL-INCURRED-DAY = WS-00                           03176
576300        MOVE WS-LITERAL-01  TO SEL-DETAIL-INCURRED-DAY.           03177
576500     SKIP2                                                        03178
576600     IF SEL-INCURRED-YEAR NOT NUMERIC                             03179
576800        MOVE SEL-PAID-YEAR     TO SEL-INCURRED-YEAR.              03180
577000     SKIP2                                                        03181
577100     IF SEL-INCURRED-MONTH NOT NUMERIC                            03182
577400        MOVE SEL-PAID-MONTH    TO  SEL-INCURRED-MONTH.            03183
577500     SKIP2                                                        03184
193000     EJECT                                                        03185
193100 BA-1500-BAL-HOSPITAL-FILE.                                       03186
193200***************************************************************   03187
193300*    PURPOSE :                                                *   03188
193400*             COMPARE THE INSTITUTIONAL FILE TRAILER RECORD   *   03189
193500*             TOTALS TO THE WORKING STORAGE INPUT FILE TOTALS *   03190
193600*             AND THEN COMPARE THE PROCESSING COUNTS TO THE   *   03191
193700*             EXTRACT FILE OUTPUT RECORD COUNTS               *   03192
193800***************************************************************   03193
193900     SKIP2                                                        03194
194000     IF WS-HOSP-TRLR-RECS      =  WS-REC-CNTS(1)                  03195
194100        AND WS-HOSP-TRLR-LIAB  =  WS-LOB-LIAB-AMTS(1)             03196
194200           PERFORM BA-1599-HOSPITAL-FILE-BALANCE                  03197
194300     ELSE                                                         03198
194400         DISPLAY WS-DISPLAY-1                                     03199
194500         MOVE WS-ABEND-CODE(3) TO USER-ABEND-CODE                 03200
194600         DISPLAY USER-ABEND-CODE                                  03201
194700         DISPLAY WS-ABEND-MSG-TBL(1)                              03202
194800         DISPLAY WS-ABEND-MSG-TBL(2)                              03203
194900         DISPLAY WS-DISPLAY-1                                     03204
195000         MOVE WS-HOSP-TRLR-RECS      TO                           03205
195100                               WS-EDITED-DISPLY-CNTS              03206
195200         DISPLAY WS-ABEND-MSG-TBL(3) WS-EDITED-DISPLY-CNTS        03207
195300                                                                  03208
195400         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-CNTS              03209
195500                                                                  03210
195600         COMPUTE WS-COMPUTE-RECORDS    = WS-REC-CNTS(1)           03211
195700         MOVE  WS-COMPUTE-RECORDS  TO  WS-EDITED-DISPLY-CNTS      03212
195800         DISPLAY WS-ABEND-MSG-TBL(4) WS-EDITED-DISPLY-CNTS        03213
195900                                                                  03214
196000         MOVE WS-HOSP-TRLR-LIAB    TO WS-EDITED-DISPLY-AMTS       03215
196100         DISPLAY WS-ABEND-MSG-TBL(5) WS-EDITED-DISPLY-AMTS        03216
196200                                                                  03217
196300         MOVE WS-PLUS-ZERO  TO        WS-EDITED-DISPLY-AMTS       03218
196400         COMPUTE WS-COMPUTE-LOB-LIB   = WS-LOB-LIAB-AMTS(1)       03219
196500         MOVE  WS-COMPUTE-LOB-LIB  TO  WS-EDITED-DISPLY-AMTS      03220
196600                                                                  03221
196700         DISPLAY WS-ABEND-MSG-TBL(6) WS-EDITED-DISPLY-AMTS        03222
196800         DISPLAY WS-DISPLAY-1                                     03223
196900         PERFORM Z-CALL-BOMBER.                                   03224
197000      EJECT                                                       03225
197100 BA-1599-HOSPITAL-FILE-BALANCE.                                   03226
197200***************************************************************   03227
197300*    PURPOSE :                                                *   03228
197400*             ISSUE MESSAGE ON SYSOUT THAT FILE INPUT BALANCED*   03229
197500***************************************************************   03230
197600     SKIP2                                                        03231
197700     DISPLAY SPACES.                                              03232
197800     DISPLAY WS-BALANCED-FILE-MESSAGE(1).                         03233
197900     DISPLAY SPACES.                                              03234
198000     EJECT                                                        03235
198000     EJECT                                                        03236
152400 BB-1000-PROCESS-HOSPITAL-FILE.                                   03237
152500******************************************************************03238
152600*    PURPOSE :                                                   *03239
152700*             INITIATE ROUTINE ACTIVITIES FOR PROCESSING AND     *03240
152800*             BALANCING THE MONTHLY INSTITUTIONAL CLAIMS FILE    *03241
152900******************************************************************03242
153000     SKIP3                                                        03243
153100     PERFORM BB-1100-OPEN-PROCESS-CLOSE.                          03244
153200     EJECT                                                        03245
153300 BB-1100-OPEN-PROCESS-CLOSE.                                      03246
153400     SKIP3                                                        03247
153500     OPEN INPUT  I-COMBINED-REJECT-FILE.                          03248
153600     SKIP3                                                        03249
153700     PERFORM BB-1110-READ-HOSP-FIRST-TIME.                        03250
153800     SKIP3                                                        03251
154100     PERFORM BB-1130-READ-HOSP-FILE-RTN                           03254
154200        UNTIL WS-EOF-HOSP.                                        03255
154300     SKIP3                                                        03256
154400     PERFORM BB-1500-BAL-HOSPITAL-FILE.                           03257
154500     SKIP3                                                        03258
154600     CLOSE I-COMBINED-REJECT-FILE.                                03259
154700     EJECT                                                        03260
154800 BB-1110-READ-HOSP-FIRST-TIME.                                    03261
154900******************************************************************03262
155000*    PURPOSE :                                                   *03263
155100*             READ 1 RECORD FROM THE INSTITUTIONAL FILE TO       *03264
155200*             CHECK PAID DATE ON RECORD                          *03265
155300******************************************************************03266
155400     SKIP3                                                        03267
155500     READ I-COMBINED-REJECT-FILE INTO                             03268
155600            R105-REC               AT END                         03269
155700                 MOVE WS-ZERO TO WS-EOF-HOSP-FILE.                03270
155800     SKIP3                                                        03271
155900       IF WS-EOF-HOSP                                             03272
156000          MOVE WS-ABEND-CODE(1) TO USER-ABEND-CODE                03273
156100          DISPLAY  WS-DISPLAY-1                                   03274
156200          DISPLAY WS-ERROR-MSG-TBL(1)                             03275
156300          DISPLAY USER-ABEND-CODE                                 03276
156400          DISPLAY  WS-DISPLAY-1                                   03277
156500          DISPLAY WS-DISPLAY-1                                    03278
156600          PERFORM Z-CALL-BOMBER.                                  03279
156700     EJECT                                                        03280
156800 BB-1130-READ-HOSP-FILE-RTN.                                      03281
156900     SKIP2                                                        03282
198200***************************************************************   03283
198300*    PURPOSE :                                                *   03284
157200*             READS THE HOSPITAL FILE AND ACCUMULATES         *   03285
157300*             RECORD COUNTS, CLAIMS COUNT DAYS COUNT AND      *   03286
157400*             AHS LIABILITY AMOUNTS.                          *   03287
157500***************************************************************   03288
157600     SKIP2                                                        03289
157700***  IF R105-HEADER-RECORD                                        03290
      ***     PERFORM BB-1136-CHECK-HEADER-DATES                        03291
158100***  ELSE                                                         03292
157700     IF R105-TRAILER-RECORD                                       03293
157900        ADD  R105-TRAILER-REC-COUNT    TO WS-HOSP-TRLR-RECS       03294
158100     ELSE                                                         03295
158300        PERFORM BB-1200-ACCUM-IP-REC-RTN                          03297
158400        PERFORM BB-1300-HOSP-SEL-PD-CLM-RECORD                    03298
158500        IF NOT WS-BYPASS                                          03299
158600           PERFORM BB-1405-HOSP-OUTPUT-DETAIL-REC.                03300
158700     SKIP3                                                        03301
158800     MOVE WS-ZERO             TO WS-BYPASS-IND.                   03302
158900     SKIP3                                                        03303
159000     READ I-COMBINED-REJECT-FILE INTO                             03304
159100            R105-REC           AT END                             03305
159200                 MOVE WS-ZERO TO WS-EOF-HOSP-FILE.                03306
159300     EJECT                                                        03307
161000     EJECT                                                        03328
       BB-1136-CHECK-HEADER-DATES.                                      03329
160100      IF R105-HEADER-CONTROL-YEAR      NOT EQUAL CURRENT-YEAR     03330
160200      OR R105-HEADER-CONTROL-MONTH     NOT EQUAL CURRENT-MONTH    03331
160300          MOVE WS-ABEND-CODE(2) TO USER-ABEND-CODE                03332
160400          DISPLAY 'BB  1136'                                      03333
160400          DISPLAY USER-ABEND-CODE                                 03334
160500          DISPLAY WS-ERROR-MSG-TBL(3)                             03335
160600          DISPLAY WS-ERROR-MSG-TBL(7) CURRENT-YEAR CURRENT-MONTH  03336
160700          DISPLAY WS-ERROR-MSG-TBL(29) R105-APRV-YR               03337
160700          DISPLAY WS-ERROR-MSG-TBL(29) R105-APRV-MO               03338
160800          PERFORM Z-CALL-BOMBER.                                  03339
                                                                        03340
161100 BB-1200-ACCUM-IP-REC-RTN.                                        03341
161200     SKIP2                                                        03342
161300***************************************************************   03343
161400*    PURPOSE :                                                *   03344
161500*             ACCUMULATE TOTALS FOR:                          *   03345
161600*             RECORD COUNTS, CLAIM COUNTS, DAYS COUNTS AND    *   03346
161700*             AHS LIABILITY AMOUNTS.                          *   03347
161800***************************************************************   03348
161900     SKIP2                                                        03349
162000     ADD WS-POSITIVE-ONE TO      WS-REC-CNTS(1).                  03350
162100     SKIP2                                                        03351
162200     ADD ZEROES         TO   WS-LOB-LIAB-AMTS(1).                 03352
162300     SKIP2                                                        03353
162400***  ADD HRCR-CASE-CTR  TO   WS-CLAIMS-CNTS(1).                   03354
162500     SKIP2                                                        03355
162600***  ADD HRCR-FULL-DAYS  HRCR-DISCOUNT-DAYS TO                    03356
162700***                      WS-DAYS-CNTS(1).                         03357
162800     EJECT                                                        03358
162900 BB-1300-HOSP-SEL-PD-CLM-RECORD.                                  03359
163000                                                                  03360
163100***************************************************************   03361
163200*    PURPOSE :                                                *   03362
163300*                                                             *   03363
163400*      BYPASS INPUT RECORDS FROM FURTHER PROCESSING AND       *   03364
163500*      ALSO ACCUMULATE TOTALS FOR BYPASS RECORDS.             *   03365
163600*                                                             *   03366
163700***************************************************************   03367
163800     SKIP2                                                        03368
163900     IF R105-HOSP-BILL EQUAL TO WS-PLUS-ZERO                      03369
164000        PERFORM BB-1305-CHECK-HOSP-BYPASS.                        03370
164200     SKIP2                                                        03371
164300     IF WS-BYPASS                                                 03372
164400        ADD WS-POSITIVE-ONE TO  WS-REC-CNTS(11)                   03373
164600        ADD ZEROES         TO   WS-LOB-LIAB-AMTS(11).             03374
164500***     ADD HRCR-CASE-CTR  TO   WS-CLAIMS-CNTS(11)                03375
164700***     ADD HRCR-FULL-DAYS  HRCR-DISCOUNT-DAYS TO                 03376
164800***                             WS-DAYS-CNTS(11).                 03377
164900     EJECT                                                        03378
165000 BB-1305-CHECK-HOSP-BYPASS.                                       03379
165100     IF R105-HOME-CARE-ANC-CHRG =   WS-PLUS-ZERO                  03380
165800        MOVE WS-ZERO               TO  WS-BYPASS-IND.             03381
165900 BB-1405-HOSP-OUTPUT-DETAIL-REC.                                  03382
166000***************************************************************   03383
166100*    PURPOSE :                                                *   03384
166200*             PREPARE TO WRITE THE OUTPUT DETAIL RECORD       *   03385
166300***************************************************************   03386
166400     PERFORM BB-1410-ACCUM-OUTPUT-COUNTS.                         03387
166500     PERFORM BB-1420-FORMAT-FIELDS.                               03388
166600     PERFORM BB-1430-ATTACH-INDICATORS.                           03389
166700     PERFORM X-3000-WRITE-RTN.                                    03390
166800     EJECT                                                        03391
166900 BB-1410-ACCUM-OUTPUT-COUNTS.                                     03392
167000***************************************************************   03393
167100*    PURPOSE :                                                *   03394
167200*             ACCUMULATE TOTALS ON FIELDS IN THE DETAIL RECORD*   03395
167300***************************************************************   03396
167400     SKIP3                                                        03397
167500         ADD WS-POSITIVE-ONE        TO  WS-REC-CNTS(6)            03398
167600                                        WS-REC-CNTS(16)           03399
167700         ADD ZEROES                 TO  WS-LOB-LIAB-AMTS(6)       03400
167800                                        WS-LOB-LIAB-AMTS(16).     03401
167900****     ADD HRCR-CASE-CTR          TO  WS-CLAIMS-CNTS(6)         03402
168000****                                    WS-CLAIMS-CNTS(16)        03403
168100****     ADD HRCR-FULL-DAYS HRCR-DISCOUNT-DAYS TO                 03404
168200****                                    WS-DAYS-CNTS(6)           03405
168300****                                    WS-DAYS-CNTS(16).         03406
168400     EJECT                                                        03407
168500 BB-1420-FORMAT-FIELDS.                                           03408
168600***************************************************************   03409
168700*    PURPOSE :                                                *   03410
168800*             SET UP THE ATTRIBUTES FOR THE DETAIL RECORD     *   03411
168900***************************************************************   03412
169000     SKIP3                                                        03413
143500     MOVE LOW-VALUES  TO  SEL-PAID-CLAIMS-RECORD.                 03414
169400        MOVE ZEROS                   TO SEL-TYPE-OF-SERVICE.      03415
169500     SKIP3                                                        03416
169600     MOVE WS-DETAIL                  TO SEL-DETAIL-RCD-INDICATOR. 03417
169700     MOVE SPACES                TO SEL-DETAIL-RCD-IDENTIFICATION. 03418
           PERFORM  BB-1425-CHECK-DATES.                                03419
170400     MOVE WS-HOSPITAL-LOB-LIT        TO SEL-LINE-OF-BUSINESS.     03420
170500     MOVE R105-CORE-SYS-ORIG-PLAN    TO SEL-PLAN-CODE.            03421
170600     MOVE R105-GROUP                 TO SEL-GROUP-NUMBER.         03422
170700     MOVE LOW-VALUES                 TO SEL-SENIOR-CARE-IND.      03423
170800     MOVE LOW-VALUES                 TO SEL-DETAIL-SUB-DIVISION.  03424
170900     MOVE R105-CERTIFICATE           TO SEL-SUBSCRIBER-ID.        03425
170900     IF SEL-SUBSCRIBER-ID EQUAL SPACES                            03426
574500         MOVE WS-FOURTEEN-ZEROS TO SEL-SUBSCRIBER-ID.             03427
171200     MOVE ZEROES                     TO SEL-NUMBER-OF-DAYS-VISITS.03428
171300     MOVE ZEROES                     TO SEL-NUMBER-OF-CLAIMS.     03429
      ******* CODING ADDED BY E.V. 04/11/96 TO CORRECT CLAIM-NUMBER*****03430
171600     IF   R105-BRANCH-ID  = WS-CS90                               03431
                MOVE R105-CASE-NO-PACKED TO REJ-TEST-CASENO-LST6        03432
                MOVE REJ-TEST-CASENO-LST6 TO SEL-DETAIL-CLAIM-NUMBER    03433
171600     ELSE                                                         03434
                PERFORM  BB-1424-CHECK-CLAIM.                           03435
      ******* CODING ADDED BY E.V. 04/11/96 TO CORRECT CLAIM-NUMBER*****03436
171600     IF  SEL-DETAIL-CLAIM-NUMBER  EQUAL SPACES                    03437
574500         MOVE WS-FOURTEEN-ZEROS TO SEL-DETAIL-CLAIM-NUMBER.       03438
171900     MOVE WS-PLUS-ZERO               TO SEL-NUMBER-OF-SERVICES.   03439
172000     MOVE ZEROES                     TO SEL-PAID-CLAIM-LIABILITY. 03440
172100     MOVE R105-PT-INITIAL           TO SEL-DETAIL-PATIENT-INITIAL.03441
172200     MOVE SPACES                  TO SEL-DETAIL-PATIENT-LAST-NAME.03442
172300     MOVE R105-PT-LAST-NAME       TO SEL-DETAIL-PATIENT-LAST-NAME.03443
172400     MOVE SPACE                  TO SEL-DETAIL-MED-SURG-BREAKDOWN 03444
72500      MOVE SPACE                      TO SEL-CSS-SOURCE-CODE.      03445
72600      MOVE SPACE                      TO SEL-LOB-ADJUSTMENT-CODE.  03446
72700      MOVE ZEROES                     TO SEL-ACCOUNT-PAID-DATE.    03447
73100         MOVE SPACES          TO SEL-BANK-B-INDICATOR.             03448
173300     MOVE R105-COLL-GROUP        TO SEL-ICHIS-COLLECTION-CODE.    03449
                                                                        03450
003340***************************************************************   03451
003350        MOVE R105-FREQUENCY-CD        TO REJ-FREQUENCY-CD.        03452
003380        MOVE R105-OFFICE              TO REJ-OFFICE.              03453
003390        MOVE R105-CURRENT-STATUS-CODE  TO REJ-CURRENT-STATUS-CODE.03454
003390        IF   R105-CONTRACT-CLASS-CODE  NUMERIC                    03455
003390             MOVE R105-CONTRACT-CLASS-CODE                        03456
003390                              TO REJ-CONTRACT-CLASS-CODE          03457
003390        ELSE                                                      03458
003390             MOVE ZEROES      TO REJ-CONTRACT-CLASS-CODE.         03459
003400        MOVE R105-BRANCH-ID            TO REJ-BRANCH-ID.          03460
003410        MOVE R105-CASE-ID              TO REJ-CASE-ID.            03461
003420        MOVE R105-MIU-IND              TO REJ-MIU-IND.            03462
003430        MOVE R105-CASE-NO              TO REJ-CASE-NO.            03463
003440        MOVE R105-CHP-CTR-DATA         TO REJ-CHP-CTR-DATA.       03464
003480        MOVE R105-HOSP-OR-PLAN         TO REJ-HOSP-OR-PLAN.       03465
003550        MOVE R105-TRANS                TO REJ-TRANS.              03466
003580        MOVE R105-TP-INDICATOR         TO REJ-TP-INDICATOR.       03467
003600        MOVE R105-PT-AGE               TO REJ-PT-AGE.             03468
003620        MOVE R105-ADM-RCVD-DATE        TO REJ-ADM-RCVD-DATE.      03469
003660        MOVE R105-ACCOM                TO REJ-ACCOM.              03470
003680        MOVE R105-PT-ACCTNO            TO REJ-PT-ACCTNO.          03471
003700        MOVE R105-BUS-CD               TO REJ-BUS-CD.             03472
003720        MOVE R105-SEX-MARITAL-CD       TO REJ-SEX-MARITAL-CD.     03473
004090        MOVE R105-SPECIALIZED-PROCESS-IND                         03474
014300                                       TO WS-CS90-PROCESS.        03475
003400        IF   R105-BRANCH-ID     EQUAL WS-CS90                     03476
                    AND WS-CORE-CS90-PROCESS                            03477
00379              MOVE R105-REJ-REAS-PACKED   TO REJ-REJ-REAS-PACKED   03478
003790        ELSE                                                      03479
003400        IF   R105-BRANCH-ID     EQUAL  WS-W                       03480
                    AND WS-CORE-CS90-PROCESS                            03481
00379              MOVE R105-REJ-REAS-PACKED   TO REJ-REJ-REAS-PACKED   03482
003790        ELSE                                                      03483
003400        IF   R105-BRANCH-ID     EQUAL   WS-R                      03484
                    AND WS-CORE-CS90-PROCESS                            03485
00379              MOVE R105-REJ-REAS-PACKED   TO REJ-REJ-REAS-PACKED   03486
003790        ELSE                                                      03487
003400        IF   R105-BRANCH-ID     EQUAL   SPACES                    03488
                    AND WS-CORE-CS90-PROCESS                            03489
00379              MOVE R105-REJ-REAS-PACKED   TO REJ-REJ-REAS-PACKED   03490
003790        ELSE                                                      03491
003790            MOVE R105-REJ-REAS-CD        TO REJ-REJ-REAS-CD.      03492
003800        MOVE R105-REJ-DISP-CD          TO REJ-REJ-DISP-CD.        03493
003820        MOVE R105-NPC-CERT             TO REJ-NPC-CERT.           03494
003840        MOVE R105-PAYROLL-LOC          TO REJ-PAYROLL-LOC.        03495
003860        MOVE R105-STATUS               TO REJ-STATUS.             03496
003870        MOVE R105-CONT                 TO REJ-CONT.               03497
003890        MOVE R105-ICD-8-DIAG-CODE      TO REJ-ICD-8-DIAG-CODE.    03498
003910        MOVE R105-ICD-9-DIAG-CODE      TO REJ-ICD-9-DIAG-CODE.    03499
003920        MOVE R105-DISCHARGE-DATE       TO REJ-DISCHARGE-DATE.     03500
003970        MOVE R105-SORT-ACCOM-CD        TO REJ-SORT-ACCOM-CD.      03501
003980        MOVE R105-CORE-SYS-LOCAL-PROV-NO TO                       03502
003980                                    REJ-CORE-SYS-LOCAL-PROV-NO.   03503
003980        MOVE  R105-MEDICARE-PROV-NO TO REJ-MEDICARE-PROV-NO.      03504
003990******  MOVE R105-TRANS-DATE        TO REJ-TRANS-DATE.            03505
003990        MOVE R105-CLK-START-DAT-PROM-PAY                          03506
003990                       TO REJ-CLK-START-DAT-PROM-PAY.             03507
004040        MOVE R105-MULT-COVERAGE     TO REJ-MULT-COVERAGE.         03508
004050        MOVE R105-CTL-NO            TO REJ-CTL-NO.                03509
004070        MOVE R105-MAT-CD            TO REJ-MAT-CD.                03510
004080        MOVE R105-CASE-SOURCE-CODE  TO REJ-CASE-SOURCE-CODE.      03511
004080        MOVE R105-REPORT-105-FIELD-CODE                           03512
004080                                    TO REJ-REPORT-105-FIELD-CODE. 03513
172000        IF   R105-HOSP-BILL NUMERIC                               03514
172000             MOVE R105-HOSP-BILL  TO REJ-HOSP-BILL                03515
172000        ELSE                                                      03516
172000             MOVE ZEROES      TO REJ-HOSP-BILL.                   03517
004080        IF   R105-HOME-CARE-ANC-CHRG NUMERIC                      03518
004080             MOVE R105-HOME-CARE-ANC-CHRG                         03519
004080                          TO REJ-HOME-CARE-ANC-CHRG               03520
004080        ELSE                                                      03521
004080             MOVE ZEROES  TO REJ-HOME-CARE-ANC-CHRG.              03522
004090        MOVE R105-SPECIALIZED-PROCESS-IND                         03523
004090                                  TO REJ-SPECIAL-PROCESS-IND.     03524
004090        MOVE R105-CORE-PROCESS-CLAIM-ST                           03525
004090                                 TO    REJ-CORE-PROCESS-CLAIM-ST. 03526
004110        MOVE R105-STATUS-CODE    TO REJ-STATUS-CODE.              03527
004130        MOVE R105-GR-AGE         TO REJ-GR-AGE.                   03528
004130        MOVE R105-TOPPS-GROUP-NO TO REJ-TOPPS-GROUP-NO.           03529
004150        MOVE R105-RO-OVER-PD TO REJ-RO-OVER-PD.                   03530
003750        MOVE R105-PAT-FIRST-NAME   TO REJ-PAT-FIRST-NAME.         03531
003610        IF   R105-SAVINGS-AMT-1  NUMERIC                          03532
003610             MOVE R105-SAVINGS-AMT-1  TO REJ-SAVINGS-AMT-1        03533
003610        ELSE                                                      03534
003610             MOVE ZEROES             TO REJ-SAVINGS-AMT-1.        03535
003670        IF   R105-TOTAL-COVERED-CHRGS   NUMERIC                   03536
003670             MOVE R105-TOTAL-COVERED-CHRGS   TO                   03537
003670                                        REJ-TOTAL-COVERED-CHRGS   03538
003670        ELSE                                                      03539
003670             MOVE ZEROES TO       REJ-TOTAL-COVERED-CHRGS.        03540
003710        MOVE R105-RATE-CODE  TO REJ-RATE-CODE.                    03541
00303         MOVE R105-TYP-SAV-IND-1  TO REJ-TYP-SAV-IND-1.            03542
003610        IF   R105-SAVINGS-AMT-2  NUMERIC                          03543
003610             MOVE R105-SAVINGS-AMT-2   TO REJ-SAVINGS-AMT-2       03544
003610        ELSE                                                      03545
003610             MOVE ZEROES             TO REJ-SAVINGS-AMT-2.        03546
00303         MOVE R105-TYP-SAV-IND-2  TO REJ-TYP-SAV-IND-2.            03547
003610        IF   R105-SAVINGS-AMT-3   NUMERIC                         03548
003610             MOVE R105-SAVINGS-AMT-3 TO REJ-SAVINGS-AMT-3         03549
003610        ELSE                                                      03550
003610             MOVE ZEROES             TO REJ-SAVINGS-AMT-3.        03551
00303         MOVE R105-TYP-SAV-IND-3  TO REJ-TYP-SAV-IND-3.            03552
004140        IF   R105-AMT-PD-BY-OTH-CARRIER NUMERIC                   03553
004140             MOVE R105-AMT-PD-BY-OTH-CARRIER     TO               03554
 04140                             WS-AMT-PD-BY-OTH-CARRIER             03555
 04140             MOVE      WS-AMT-PD-BY-OTH-CARRIER  TO               03556
004140                             REJ-AMT-PD-BY-OTH-CARRIER            03557
004140        ELSE                                                      03558
004140            MOVE ZEROES TO            REJ-AMT-PD-BY-OTH-CARRIER.  03559
004140        IF   R105-ADJ-NUM               NUMERIC                   03560
004140             MOVE R105-ADJ-NUM               TO                   03561
004140                                  REJ-ADJ-NUM                     03562
004140        ELSE                                                      03563
004140            MOVE ZEROES TO    REJ-ADJ-NUM.                        03564
004140        MOVE  R105-BYPASS-SUPP-AND-POOL     TO                    03565
004140                     REJ-BYPASS-SUPP-AND-POOL.                    03566
004140        MOVE  R105-MSK-CATEGORY  TO  REJ-MSK-CATEGORY.            03567
004140        MOVE  R105-MCARE-INT-IND TO  REJ-MCARE-INT-IND.           03568
004140        MOVE  R105-MARKET-SEGMENT TO REJ-MARKET-SEGEMNT.          03568
004140        MOVE  R105-ACCT-TYP       TO REJ-ACCOUNT-TYPE.            03568
004140        MOVE  R105-DEST-ID        TO REJ-DEST-ID.                 03568
173500        MOVE WS-SIX                     TO SEL-RECORD-TYPE.       03569
189600     EJECT                                                        03570
       BB-1424-CHECK-CLAIM.                                             03571
014700          MOVE R105-CORE-SYS-CLAIM-NO                             03572
014700                                 TO  WS-ORIGINAL-CLAIM-NUMBER.    03573
014900     IF  WS-CLAIM-NUMBER6  EQUAL SPACES                           03574
014900        MOVE WS-CLAIM-NUMBER8 TO SEL-DETAIL-CLAIM-NUMBER          03575
014900     ELSE                                                         03576
171600        MOVE R105-CORE-SYS-CLAIM-NO TO SEL-DETAIL-CLAIM-NUMBER.   03577
189600     EJECT                                                        03578
       BB-1425-CHECK-DATES.                                             03579
581600     SKIP2                                                        03580
581700     MOVE ZEROS                 TO WS-COMPARE-PAID-DATE           03581
581800                                   WS-COMPARE-INCURRED-DATE.      03582
160100     IF  R105-APRV-YR          NOT EQUAL CURRENT-YEAR             03583
160200         OR R105-APRV-MO       NOT EQUAL CURRENT-MONTH            03584
170100         MOVE R105-REJ-APRV-DATE     TO REJ-ACTUAL-APRV-DATE      03585
170100         MOVE CURRENT-YEAR       TO SEL-PAID-YEAR                 03586
170200         MOVE CURRENT-MONTH      TO SEL-PAID-MONTH                03587
583000         MOVE WS-01              TO SEL-DETAIL-PAID-DAY           03588
160100     ELSE                                                         03589
582400         MOVE R105-APRV-YR          TO SEL-PAID-YEAR              03590
582500         MOVE R105-APRV-MO          TO SEL-PAID-MONTH             03591
582600         MOVE R105-APRV-DA          TO SEL-DETAIL-PAID-DAY.       03592
581900     SKIP2                                                        03593
582000     MOVE R105-APRV-YR        TO WS-COMPARE-P-YEAR                03594
582100     MOVE R105-APRV-MO        TO WS-COMPARE-P-MONTH               03595
582200     MOVE R105-APRV-DA        TO WS-COMPARE-P-DAY.                03596
582300     SKIP2                                                        03597
582400     MOVE R105-ADM-YR         TO WS-COMPARE-I-YEAR                03598
582500     MOVE R105-ADM-MO         TO WS-COMPARE-I-MONTH               03599
582600     MOVE R105-ADM-DA         TO WS-COMPARE-I-DAY.                03600
582700     SKIP2                                                        03601
582800     IF WS-COMPARE-I-YEAR  = ZERO AND                             03602
582900        WS-COMPARE-I-MONTH = ZERO AND                             03603
583000        WS-COMPARE-I-DAY   = ZERO                                 03604
582900        MOVE WS-COMPARE-INCURRED-DATE  TO REJ-ACTUAL-INCUR-DATE   03605
170100        MOVE CURRENT-YEAR       TO SEL-INCURRED-YEAR              03606
170200        MOVE CURRENT-MONTH      TO SEL-INCURRED-MONTH             03607
583000        MOVE WS-01              TO SEL-DETAIL-INCURRED-DAY        03608
582800     ELSE                                                         03609
582400        MOVE R105-ADM-YR        TO SEL-INCURRED-YEAR              03610
582500        MOVE R105-ADM-MO        TO SEL-INCURRED-MONTH             03611
582600        MOVE R105-ADM-DA        TO SEL-DETAIL-INCURRED-DAY.       03612
582700     SKIP2                                                        03613
C21TSR*    IF WS-COMPARE-PAID-DATE  <  WS-COMPARE-INCURRED-DATE         03614
                                                                        03615
                                                                        03616
C21TSR     CALL  C2110C06  USING WS-COMPARE-PAID-DATE                   03617
C21TSR               C21WS-COMPARE-PAID-DATE C21-BASE-ON C21-WORK-AREA  03618
C21TSR     CALL  C2110C06  USING WS-COMPARE-INCURRED-DATE               03619
C21TSR               C21WS-COMPARE-INCURRED-DATE C21-BASE-ON            03620
C21TSR               C21-WORK-AREA                                      03621
C21TSR     IF C21WS-COMPARE-PAID-DATE < C21WS-COMPARE-INCURRED-DATE     03622
582900        MOVE WS-COMPARE-INCURRED-DATE  TO REJ-ACTUAL-INCUR-DATE   03623
170100        MOVE CURRENT-YEAR       TO SEL-INCURRED-YEAR              03624
170200        MOVE CURRENT-MONTH      TO SEL-INCURRED-MONTH             03625
583000        MOVE WS-01              TO SEL-DETAIL-INCURRED-DAY.       03626
583100     SKIP2                                                        03627
583200     IF WS-COMPARE-P-DAY        LESS THAN WS-01                   03628
583300        OR WS-COMPARE-P-DAY    GREATER THAN WS-31                 03629
583500            MOVE WS-LITERAL-01 TO SEL-DETAIL-PAID-DAY.            03630
583700     SKIP2                                                        03631
583800     IF WS-COMPARE-I-DAY        LESS THAN WS-01                   03632
583900        OR WS-COMPARE-I-DAY    GREATER THAN WS-31                 03633
584100            MOVE WS-LITERAL-01 TO SEL-DETAIL-INCURRED-DAY.        03634
584300     SKIP2                                                        03635
584400     IF WS-COMPARE-P-MONTH      LESS THAN WS-01                   03636
584500        OR WS-COMPARE-P-MONTH  GREATER THAN WS-12                 03637
584700            MOVE WS-MONTH      TO SEL-PAID-MONTH.                 03638
584900     SKIP2                                                        03639
585000     IF WS-COMPARE-I-MONTH      LESS THAN WS-01                   03640
585100        OR WS-COMPARE-I-MONTH  GREATER THAN WS-12                 03641
585300            MOVE WS-MONTH      TO SEL-INCURRED-MONTH.             03642
189600     EJECT                                                        03643
189700 BB-1430-ATTACH-INDICATORS.                                       03644
189800***************************************************************   03645
189900*    PURPOSE :                                                *   03646
190000*            MOVE DESCRIPTIVE INDICATORS BASED UPON THE INPUT *   03647
190100*             FILE DESCRIPTION 88 LEVEL INDICATORS            *   03648
190200***************************************************************   03649
190300     SKIP3                                                        03650
191200         MOVE SPACE TO SEL-CLAIM-SERVICE-CATEGORY.                03651
191300     SKIP3                                                        03652
191700             MOVE WS-ZERO      TO SEL-ICHIS-PRESSO-FLAG.          03653
191800     SKIP3                                                        03654
192200             MOVE WS-ZERO      TO SEL-ICHIS-RIDER-FLAG.           03655
192300     SKIP3                                                        03656
192700             MOVE WS-ZERO  TO SEL-ICHIS-SUPP-PAYMENT-FLAG.        03657
192800     SKIP2                                                        03658
193000     EJECT                                                        03659
193100 BB-1500-BAL-HOSPITAL-FILE.                                       03660
193200***************************************************************   03661
193300*    PURPOSE :                                                *   03662
193400*             COMPARE THE INSTITUTIONAL FILE TRAILER RECORD   *   03663
193500*             TOTALS TO THE WORKING STORAGE INPUT FILE TOTALS *   03664
193600*             AND THEN COMPARE THE PROCESSING COUNTS TO THE   *   03665
193700*             EXTRACT FILE OUTPUT RECORD COUNTS               *   03666
193800***************************************************************   03667
193900     SKIP2                                                        03668
194000     IF WS-HOSP-TRLR-RECS      =  WS-REC-CNTS(1)                  03669
194100***     AND WS-HOSP-TRLR-LIAB  =  WS-LOB-LIAB-AMTS(1)             03670
194200           PERFORM BB-1599-HOSPITAL-FILE-BALANCE                  03671
194300     ELSE                                                         03672
194400         DISPLAY WS-DISPLAY-1                                     03673
194500         MOVE WS-ABEND-CODE(3) TO USER-ABEND-CODE                 03674
194600         DISPLAY USER-ABEND-CODE                                  03675
194700         DISPLAY WS-ABEND-MSG-TBL(1)                              03676
194800         DISPLAY WS-ABEND-MSG-TBL(2)                              03677
194900         DISPLAY WS-DISPLAY-1                                     03678
195000         MOVE WS-HOSP-TRLR-RECS      TO                           03679
195100                               WS-EDITED-DISPLY-CNTS              03680
195200         DISPLAY WS-ABEND-MSG-TBL(3) WS-EDITED-DISPLY-CNTS        03681
195300                                                                  03682
195400         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-CNTS              03683
195500                                                                  03684
195600         COMPUTE WS-COMPUTE-RECORDS    = WS-REC-CNTS(1)           03685
195700         MOVE  WS-COMPUTE-RECORDS  TO  WS-EDITED-DISPLY-CNTS      03686
195800         DISPLAY WS-ABEND-MSG-TBL(4) WS-EDITED-DISPLY-CNTS        03687
195900                                                                  03688
196000         MOVE WS-HOSP-TRLR-LIAB    TO WS-EDITED-DISPLY-AMTS       03689
196100         DISPLAY WS-ABEND-MSG-TBL(5) WS-EDITED-DISPLY-AMTS        03690
196200                                                                  03691
196300         MOVE WS-PLUS-ZERO  TO        WS-EDITED-DISPLY-AMTS       03692
196400         COMPUTE WS-COMPUTE-LOB-LIB   = WS-LOB-LIAB-AMTS(1)       03693
196500         MOVE  WS-COMPUTE-LOB-LIB  TO  WS-EDITED-DISPLY-AMTS      03694
196600                                                                  03695
196700         DISPLAY WS-ABEND-MSG-TBL(6) WS-EDITED-DISPLY-AMTS        03696
196800         DISPLAY WS-DISPLAY-1                                     03697
196900         PERFORM Z-CALL-BOMBER.                                   03698
197000      EJECT                                                       03699
197100 BB-1599-HOSPITAL-FILE-BALANCE.                                   03700
197200***************************************************************   03701
197300*    PURPOSE :                                                *   03702
197400*             ISSUE MESSAGE ON SYSOUT THAT FILE INPUT BALANCED*   03703
197500***************************************************************   03704
197600     SKIP2                                                        03705
197700     DISPLAY SPACES.                                              03706
197800     DISPLAY WS-BALANCED-FILE-MESSAGE(1).                         03707
197900     DISPLAY SPACES.                                              03708
198000     EJECT                                                        03709
198100 C-1000-PROCESS-MCS-CSS-FILE.                                     03710
262900     SKIP3                                                        04053
             PERFORM CA-1100-OPN-PROCESS-CLOSE-FILE
                UNTIL WS-EOF-MMD-MSG.
498000     EJECT                                                        04833
203200 CA-1100-OPN-PROCESS-CLOSE-FILE.                                  04834
203300***************************************************************   04835
203400*    PURPOSE :                                                *   04836
203500*             CONTROL ALL I-O OPERATIONS RELATED TO THE MCS   *   04837
203600*             INPUT FILE AND OPEN FILE FN982OTH FOR ROUTING   *   04838
203700*             MAJOR MEDICAL DRUG CLAIMS TO EXTERNAL PROCESS   *   04839
203800*             FOR ALLOCATION TO THE DRUG LINE OF BUSINESS     *   04840
203900***************************************************************   04841
204000     SKIP2                                                        04842
204100     OPEN INPUT  I-MCS-PAID-CLAIMS-FILE.                          04843
204200     SKIP2                                                        04844
204300     OPEN OUTPUT O-OTHER-MEDICAL-FEED-CLAIMS.                     04845
204400     SKIP2                                                        04846
204500     PERFORM CA-1200-READ-MCS-FIRST-TIME.                         04847
204600     SKIP2                                                        04848
204700     PERFORM CA-1300-INIT-MCS-ACCUM-TOTALS.                       04849
204800     SKIP2                                                        04850
204900     PERFORM CA-1250-READ-MCS-FILE                                04851
205000        UNTIL WS-EOF-MMD-MSG.                                     04852
205100     SKIP2                                                        04853
205200     PERFORM CA-1800-BAL-MCS-FILE.                                04854
205300     SKIP2                                                        04855
205400     CLOSE  I-MCS-PAID-CLAIMS-FILE.                               04856
205500     SKIP2                                                        04857
205600     CLOSE  O-OTHER-MEDICAL-FEED-CLAIMS.                          04858
205700     EJECT                                                        04859
207800 CA-1200-READ-MCS-FIRST-TIME.                                     04860
207900***************************************************************   04861
208000*    PURPOSE :                                                *   04862
208100*             CHECK THE HEADER RECORD ON MCS FILE AND         *   04863
208200*             COMPARE  THE DATE AGAINST DATE FILE.            *   04864
208300***************************************************************   04865
208400     SKIP2                                                        04866
208500                                                                  04867
208600     READ I-MCS-PAID-CLAIMS-FILE INTO                             04868
208700                     MS-CSS-RECORD AT END                         04869
208800                        MOVE WS-ZERO TO WS-EOF-MMD-MSG-FILE.      04870
208900     SKIP2                                                        04871
209000     IF  WS-EOF-MMD-MSG                                           04872
209100         DISPLAY WS-DISPLAY-1                                     04873
209200         DISPLAY WS-ERROR-MSG-TBL(25)                             04874
209300         MOVE WS-ABEND-CODE(4) TO USER-ABEND-CODE                 04875
209400         DISPLAY USER-ABEND-CODE                                  04876
209500         DISPLAY WS-DISPLAY-1                                     04877
209600         PERFORM Z-CALL-BOMBER.                                   04878
209700     EJECT                                                        04879
214100***************************************************************   04880
214200 CA-1250-READ-MCS-FILE.                                           04881
214300**************************************************************    04882
214400****   THIS ROUTINE CONTROLS ALL PROCESSING ACTIVITIES   *****    04883
214500****           FOR THE MCS "CSS" FILE                    *****    04884
214600**************************************************************    04885
214700     SKIP2                                                        04886
214800     IF MS-CSS-HEADER                                             04887
214900        MOVE MS-CSS-RECORD    TO MS-CT-CONTROL-CARD               04888
215000        PERFORM CA-1260-CHECK-MCS-DATES                           04889
215100        PERFORM XA-3100-FEED-OTHER-FILE-RTN.                      04890
215200     SKIP2                                                        04891
215300     IF MS-CSS-DATA-RECORD                                        04892
215400        PERFORM CA-1254-CHECK-FOR-M-DRUG-CLAIM                    04893
215500        PERFORM CA-1255-DATE-EXCEPTION-PROCESS                    04894
215600        PERFORM CA-1260-CHECK-MCS-DATES                           04895
215700        PERFORM CA-1400-ACCUM-MCS-IP-REC                          04896
215800        PERFORM CA-1500-SELECT-OR-BYPASS                          04897
215900     SKIP2                                                        04898
216000        IF NOT WS-BYPASS AND NOT WS-M-DRUG-BYPASS                 04899
216100             PERFORM CA-1275-MCS-CHECK-CONTROL-BRK                04900
216200             PERFORM CA-1610-ACCUM-BASIC-MED-TOTAL                04901
216300             PERFORM CA-1620-ACCUM-MAJOR-MED-TOTAL                04902
216400             PERFORM CA-1630-ACCUM-RIDER-MED-TOTAL                04903
216500             PERFORM CA-1640-WRITE-BASIC-MED-RECORD               04904
216000        ELSE                                                      04905
                IF WS-BYPASS                                            04906
211700             PERFORM CA-1275-MCS-CHECK-CONTROL-BRK                04907
211800             PERFORM CA-1610-ACCUM-BASIC-MED-TOTAL                04908
211900             PERFORM CA-1620-ACCUM-MAJOR-MED-TOTAL                04909
212000             PERFORM CA-1630-ACCUM-RIDER-MED-TOTAL                04910
212100             PERFORM CA-1640-WRITE-BASIC-MED-RECORD.              04911
216800     SKIP2                                                        04912
216900     IF  MS-CSS-TRAILER                                           04913
217000            MOVE MS-CSS-RECORD    TO MS-TR-TRAILER-RECORD         04914
      **IR#309075- VIN0I0- IBM-VIJAYA START CHANGES
217100            ADD MS-TR-RECORD-COUNT1   TO                          04915
217200                                       WS-MCS-TRLR-RECS           04916
      **IR#309075- VIN0I0- IBM-VIJAYA END   CHANGES
217300            COMPUTE  WS-MCS-COMPUTED-LIAB =                       04917
217400                     WS-MCS-COMPUTED-LIAB +                       04918
217500                     WS-LOB-LIAB-AMTS(2) + WS-LOB-LIAB-AMTS(3).   04919
217600     SKIP2                                                        04920
217700     IF  MS-CSS-TRAILER                                           04921
217800            PERFORM XA-3100-FEED-OTHER-FILE-RTN.                  04922
217900     SKIP2                                                        04923
218000     MOVE WS-ZERO   TO WS-BYPASS-IND.                             04924
218100     SKIP2                                                        04925
218200     READ I-MCS-PAID-CLAIMS-FILE INTO                             04926
218300                     MS-CSS-RECORD AT END                         04927
218400                       MOVE WS-ZERO TO WS-EOF-MMD-MSG-FILE.       04928
218500     EJECT                                                        04929
220100 CA-1254-CHECK-FOR-M-DRUG-CLAIM.                                  04930
220200***************************************************************   04931
220300*    PURPOSE :                                                *   04932
220400*             CHECK THE DETAIL RECORD ON MCS FILE TO IDENTIFY *   04933
220500*             THE CLAIM AS STANDARD OR M-DRUG PAID CLAIM.     *   04934
220600***************************************************************   04935
220700     SKIP2                                                        04936
220800     IF  MS-CSS-DATA-RECORD                                       04937
220900         IF  MS-CSS-SOURCE-CODE = WS-MAIL-ORDER-DRUG              04938
                    MOVE WS-ONE    TO WS-M-DRUG-INDICATOR               04940
221100         ELSE                                                     04941
221200               MOVE WS-ZERO     TO WS-M-DRUG-INDICATOR            04942
221300     ELSE                                                         04943
221400               MOVE SPACE       TO WS-M-DRUG-INDICATOR.           04944
221500     EJECT                                                        04945
200000 CA-1254-A-CHECK-FOR-PPO.                                         04952
              MOVE WS-ONE TO WS-M-DRUG-INDICATOR.                       04953
221500     EJECT                                                        04954
200000 CA-1254-B-CHECK-FOR-PPO.                                         04955
                   IF  MS-CORE-MERGE-SYSTEM-IND   = WS-DRUG-PPO         04956
221200                 MOVE WS-ZERO     TO WS-M-DRUG-INDICATOR          04957
221100             ELSE                                                 04958
                      IF MS-CORE-MERGE-SYSTEM-IND  = WS-DRUG-CAREMARK   04959
221000                   MOVE WS-ZERO     TO WS-M-DRUG-INDICATOR        04960
221100                ELSE                                              04961
221000                   MOVE WS-ONE      TO WS-M-DRUG-INDICATOR.       04962
221500     EJECT                                                        04963
223300 CA-1255-DATE-EXCEPTION-PROCESS.                                  04964
223400***************************************************************   04965
223500*    PURPOSE :                                                *   04966
223600*             INITIATE ROUTINES WHICH WILL SUBSTITUTE A VALID *   04967
223700*             GREGORIAN DATE VALUE FOR OMITTED OR UNUSABLE    *   04968
223800*             CHECK PAID-DATE.  UNUSABLE DATES OCCUR ON ANY   *   04969
223900*             PRIOR PAYMENT MONTH ADJUSTMENTS, MANUALLY PAID  *   04970
224000*             MEDICAL CLAIMS AND MAIL-ORDER DRUG CLAIMS.      *   04971
224100***************************************************************   04972
224200     SKIP2                                                        04973
224300     MOVE ZEROS                  TO SEL-ACCOUNT-PAID-DATE.        04974
224400     SKIP2                                                        04975
224500     PERFORM CA-1255-A-FIXUP-FOR-ADJUSTMNTS                       04976
224600     PERFORM CA-1255-B-FIXUP-FOR-MANUAL-MED                       04977
224700     PERFORM CA-1255-C-FIXUP-FOR-MAIL-DRUGS.                      04978
224800     PERFORM CA-1255-D-DATE-DEFAULT.                              04979
224900     EJECT                                                        04980
228500 CA-1255-A-FIXUP-FOR-ADJUSTMNTS.                                  04981
228600***************************************************************   04982
228700*    PURPOSE :                                                *   04983
228800*             STORE  THE CURRENT VALUE OF MS-CSS-CHECK-PAID   *   04984
228900*             DATES AND CONVERT THE IED-DATE TO GREGORIAN     *   04985
229000*             FORMAT, FOR SUBSTITUTION                        *   04986
229100***************************************************************   04987
229200     SKIP2                                                        04988
229300     MOVE WS-NO                        TO WS-LOB-ADJUSTMENT-FLAG  04989
229400     SKIP2                                                        04990
229500     IF  MS-CSS-IED-DATE-DISPL        GREATER THAN WS-PLUS-ZERO   04991
229600       MOVE WS-YES                     TO WS-LOB-ADJUSTMENT-FLAG  04992
229700       MOVE MS-CSS-CHK-YY            TO WS-PREV-ACC-PAID-DATE-YY  04993
229800       MOVE MS-CSS-CHK-MM            TO WS-PREV-ACC-PAID-DATE-MM  04994
229900       MOVE MS-CSS-CHK-DD            TO WS-PREV-ACC-PAID-DATE-DD  04995
230000       MOVE SPACES              TO DT-COMMON-DATE-RTE-PASS-AREA   04996
230100       MOVE MS-CSS-IED-DATE-DISPL              TO DT-DISPL-DATE   04997
230200       MOVE WS-LITERAL-01                        TO DT-FUNCTION   04998
230300       MOVE WS-TWO                            TO DT-OUTPUT-DEST   04999
230400     SKIP2                                                        05000
230500         PERFORM CA-1255-X-CONVERT-DISPLACEMENT                   05001
230600     SKIP2                                                        05002
230700         IF DT-SUCCESSFUL                                         05003
230800           MOVE DT-GREG-DATE6   TO MS-CSS-CHECK-PAID-DATE         05004
230900     SKIP2                                                        05005
231000         ELSE                                                     05006
231100           DISPLAY WS-ERROR-CONVERTING-ADJ                        05007
231200     SKIP2                                                        05008
231300           MOVE WS-CHECK-PAID-DATE-YY TO MS-CSS-CHK-YY            05009
231400           MOVE WS-CHECK-PAID-DATE-MM TO MS-CSS-CHK-MM            05010
231500           MOVE WS-CHECK-PAID-DATE-DD TO MS-CSS-CHK-DD            05011
231600     SKIP2                                                        05012
231700     ELSE                                                         05013
231800             CONTINUE.                                            05014
231600     SKIP2                                                        05015
229500     IF  MS-CSS-DATE-RECEIVED-DISPL    GREATER THAN WS-PLUS-ZERO  05016
230000       MOVE SPACES              TO DT-COMMON-DATE-RTE-PASS-AREA   05017
230100       MOVE MS-CSS-DATE-RECEIVED-DISPL   TO DT-DISPL-DATE         05018
230200       MOVE WS-LITERAL-01                TO DT-FUNCTION           05019
230300       MOVE WS-TWO                       TO DT-OUTPUT-DEST        05020
230500       PERFORM CA-1255-X-CONVERT-DISPLACEMENT                     05021
230600     SKIP2                                                        05022
  0700         IF DT-SUCCESSFUL                                         05023
230800           MOVE DT-GREG-DATE6   TO    WS-DATE-CONVERT-DISPL       05024
231000         ELSE                                                     05025
231300           MOVE ZEROES TO WS-DATE-CONVERT-DISPL.                  05026
231900     EJECT                                                        05027
233600 CA-1255-B-FIXUP-FOR-MANUAL-MED.                                  05028
233700***************************************************************   05029
233800*    PURPOSE :                                                *   05030
233900*             STORE  THE CURRENT VALUE OF MS-CSS-CHECK-PAID   *   05031
234000*             DATES AND SUBSTITUTE THE HEADER RECORD DATES    *   05032
234100***** *********************************************************   05033
234200     SKIP2                                                        05034
234300     IF  MS-CSS-SOURCE-MANUAL-MAJ-MED                             05035
234400       MOVE MS-CSS-CHK-YY           TO WS-PREV-ACC-PAID-DATE-YY   05036
234500       MOVE MS-CSS-CHK-MM           TO WS-PREV-ACC-PAID-DATE-MM   05037
234600       MOVE MS-CSS-CHK-DD           TO WS-PREV-ACC-PAID-DATE-DD   05038
234700     SKIP2                                                        05039
234800         MOVE WS-CHECK-PAID-DATE-YY   TO MS-CSS-CHK-YY            05040
234900         MOVE WS-CHECK-PAID-DATE-MM   TO MS-CSS-CHK-MM            05041
235000         MOVE WS-CHECK-PAID-DATE-DD   TO MS-CSS-CHK-DD.           05042
235100     EJECT                                                        05043
237300 CA-1255-C-FIXUP-FOR-MAIL-DRUGS.                                  05044
237400***************************************************************   05045
237500*    PURPOSE :                                                *   05046
237600*             STORE  THE CURRENT VALUE OF MS-CSS-CHECK-PAID   *   05047
237700*             AND REPLACE IT WITH LAST-PROCESS-DATE           *   05048
237800***************************************************************   05049
237900     SKIP2                                                        05050
235800** ADDED BY E.V       FOR PPO DRUGS                               05051
235800** ADDED BY E.V       FOR CAREMARK CLAIMS                         05052
238000     IF  MS-CSS-SOURCE-CODE = WS-MAIL-ORDER-DRUG                  05053
               PERFORM CA-1255-C-FIXUP-FOR-PPO.                         05054
238100     SKIP2                                                        05055
239300     EJECT                                                        05056
       CA-1255-C-FIXUP-FOR-PPO.                                         05057
238200           MOVE MS-CSS-CHK-YY      TO WS-PREV-ACC-PAID-DATE-YY.   05058
238300           MOVE MS-CSS-CHK-MM       TO WS-PREV-ACC-PAID-DATE-MM.  05059
238400           MOVE MS-CSS-CHK-DD       TO WS-PREV-ACC-PAID-DATE-DD.  05060
238500     SKIP2                                                        05061
231300***        MOVE WS-CHECK-PAID-DATE-YY TO MS-CSS-CHK-YY.           05062
231400***        MOVE WS-CHECK-PAID-DATE-MM TO MS-CSS-CHK-MM.           05063
231500***        MOVE WS-CHECK-PAID-DATE-DD TO MS-CSS-CHK-DD.           05064
238600           MOVE MS-CSS-LPD-YY       TO MS-CSS-CHK-YY.             05065
238700           MOVE MS-CSS-LPD-MM       TO MS-CSS-CHK-MM.             05066
238800           MOVE MS-CSS-LPD-DD       TO MS-CSS-CHK-DD.             05067
238900     SKIP2                                                        05068
239000           ADD WS-PLUS-ONE TO WS-M-DRUG-RECORD-COUNT.             05069
239100     SKIP2                                                        05070
239200           PERFORM XA-3100-FEED-OTHER-FILE-RTN.                   05071
239300     EJECT                                                        05072
243500 CA-1255-D-DATE-DEFAULT.                                          05073
243600***************************************************************   05074
243700*    PURPOSE :                                                *   05075
243800*             STORE  THE CURRENT VALUE OF MS-CSS-CHECK-PAID   *   05076
243900*             AND REPLACE IT WITH LAST-PROCESS-DATE           *   05077
244000*             BUT IF LAST-PROCESS-DATE IS NOT CORRRECT THEN   *   05078
244100*             REPLACE ITH WITH THE MS-CSS-HEADER DATE AND     *   05079
244200*             PLACE CAUSE '?' TO BE INSERTED IN FIELD         *   05080
244300*             SEL-LOB-ADJUSTMENT-CODE.                        *   05081
244400***************************************************************   05082
244500     SKIP2                                                        05083
244600     MOVE SPACE      TO  WS-LOB-ADJ-CODE-INDICATOR.               05084
244700     SKIP2                                                        05085
244800     IF  MS-CSS-DATA-RECORD                                       05086
244900       IF  STANDARD-MEDICAL-CLAIM                                 05087
245000         IF MS-CSS-CHK-YY           NOT EQUAL CURRENT-YEAR        05088
245100            OR MS-CSS-CHK-MM           NOT EQUAL CURRENT-MONTH    05089
245200     SKIP2                                                        05090
245300         MOVE MS-CSS-CHK-YY TO WS-PREV-ACC-PAID-DATE-YY           05091
245400         MOVE MS-CSS-CHK-MM TO WS-PREV-ACC-PAID-DATE-MM           05092
245500         MOVE MS-CSS-CHK-DD TO WS-PREV-ACC-PAID-DATE-DD           05093
245600     SKIP2                                                        05094
231300***        MOVE WS-CHECK-PAID-DATE-YY TO MS-CSS-CHK-YY            05095
231400***        MOVE WS-CHECK-PAID-DATE-MM TO MS-CSS-CHK-MM            05096
231500***        MOVE WS-CHECK-PAID-DATE-DD TO MS-CSS-CHK-DD.           05097
EV5700         MOVE MS-CSS-LPD-YY               TO MS-CSS-CHK-YY        05098
EV5800         MOVE MS-CSS-LPD-MM               TO MS-CSS-CHK-MM        05099
EV5900         MOVE MS-CSS-LPD-DD               TO MS-CSS-CHK-DD.       05100
246000     SKIP2                                                        05101
246100     IF  MS-CSS-DATA-RECORD                                       05102
246200       IF  STANDARD-MEDICAL-CLAIM                                 05103
246300         IF MS-CSS-CHK-YY             NOT EQUAL CURRENT-YEAR      05104
246400            OR MS-CSS-CHK-MM          NOT EQUAL CURRENT-MONTH     05105
246500     SKIP2                                                        05106
246600         DISPLAY WS-DEF-CLM-MSG, MS-CSS-CLAIM-NO,                 05107
246700                 WS-DEF-DTE-MSG, MS-CSS-CHECK-PAID-DATE,          05108
246800                 WS-DEF-SRC-MSG, MS-CSS-SOURCE-CODE               05109
246900     SKIP2                                                        05110
247000         MOVE WS-LITERAL-QUESTION-M                               05111
247100                         TO  WS-LOB-ADJ-CODE-INDICATOR            05112
247200         MOVE WS-CHECK-PAID-DATE-YY   TO MS-CSS-CHK-YY            05113
247300         MOVE WS-CHECK-PAID-DATE-MM   TO MS-CSS-CHK-MM            05114
247400         MOVE WS-CHECK-PAID-DATE-DD   TO MS-CSS-CHK-DD.           05115
247500     EJECT                                                        05116
248500 CA-1255-X-CONVERT-DISPLACEMENT.                                  05117
248600***************************************************************   05118
248700*    PURPOSE :                                                *   05119
248800*            CONVERT A DISPLACEMENT VALUE TO A GREGORIAN      *   05120
248900*            DATE VALUE FOR DETERMINING CHECK-PAID-DATE       *   05121
249000***************************************************************   05122
249100     SKIP2                                                        05123
249200     CALL 'MSX140'  USING DT-COMMON-DATE-RTE-PASS-AREA.           05124
249300     EJECT                                                        05125
252800 CA-1260-CHECK-MCS-DATES.                                         05126
252900***************************************************************   05127
253000*    PURPOSE :                                                *   05128
253100*             CHECK THE HEADER RECORD ON MCS FILE AND         *   05129
253200*             COMPARE  THE DATE AGAINST DATE FILE.            *   05130
253300***************************************************************   05131
253400     SKIP2                                                        05132
253500     IF  MS-CSS-HEADER                                            05133
253600         IF MS-CT-PROD-MM          NOT EQUAL CURRENT-MONTH        05134
253700           OR MS-CT-PROD-YY          NOT EQUAL CURRENT-YEAR       05135
253800            DISPLAY WS-DISPLAY-1                                  05136
253900            DISPLAY WS-ERROR-MSG-TBL(4)                           05137
254000            MOVE WS-ABEND-CODE(6) TO USER-ABEND-CODE              05138
254100            DISPLAY USER-ABEND-CODE                               05139
254200            DISPLAY WS-DISPLAY-1                                  05140
254300            PERFORM Z-CALL-BOMBER                                 05141
254400         ELSE                                                     05142
254500            MOVE MS-CT-PROD-YY          TO WS-CHECK-PAID-DATE-YY  05143
254600            MOVE MS-CT-PROD-MM          TO WS-CHECK-PAID-DATE-MM  05144
254700            MOVE MS-CT-PROD-DD          TO WS-CHECK-PAID-DATE-DD  05145
254800     SKIP2                                                        05146
254900     ELSE CONTINUE.                                               05147
255000                                                                  05148
255100     IF  MS-CSS-DATA-RECORD                                       05149
255200       IF  STANDARD-MEDICAL-CLAIM                                 05150
255300         IF MS-CSS-CHK-YY             NOT EQUAL CURRENT-YEAR      05151
255400            OR MS-CSS-CHK-MM             NOT EQUAL CURRENT-MONTH  05152
255500             DISPLAY WS-DISPLAY-1                                 05153
255600             DISPLAY WS-ERROR-MSG-TBL(2)                          05154
255700             MOVE WS-ABEND-CODE(5) TO USER-ABEND-CODE             05155
255800             DISPLAY USER-ABEND-CODE                              05156
255900             DISPLAY WS-DISPLAY-1                                 05157
256000             PERFORM Z-CALL-BOMBER.                               05158
256100     EJECT                                                        05159
257800 CA-1275-MCS-CHECK-CONTROL-BRK.                                   05160
257900**************************************************************    05161
258000*       MOVE VALUE OF 1 TO THE FIRST TRANSACTION WITHIN EACH      05162
258100*     SERIES OF TRANSACTIONS WHICH CARRY THE SAME CLAIM NUMBER    05163
258200**************************************************************    05164
258300     SKIP2                                                        05165
258400     IF MS-CSS-CLAIM-NO     =       WS-PREV-CLAIM-NO              05166
258500        MOVE WS-PLUS-ZERO TO WS-SUMMARY-CLAIM-COUNT               05167
258600     ELSE                                                         05168
258700        MOVE WS-POSITIVE-ONE TO WS-SUMMARY-CLAIM-COUNT.           05169
258800     SKIP2                                                        05170
258900     IF CLAIM-WAS-ADJUSTED                                        05171
259000        MOVE WS-PLUS-ZERO TO WS-SUMMARY-CLAIM-COUNT.              05172
259100     SKIP2                                                        05173
259200     PERFORM CA-1395-SAVE-OTHER-ELEMENTS.                         05174
259300     EJECT                                                        05175
260900 CA-1300-INIT-MCS-ACCUM-TOTALS.                                   05176
261000***************************************************************   05177
261100*    PURPOSE :                                                *   05178
261200*             INITIALIZE THE ACCUMULATED FIELD FOR MCS FILE.  *   05179
261300***************************************************************   05180
261400     MOVE WS-PLUS-ZERO     TO    WS-SUMMARY-BASIC-PAID            05181
261500                                 WS-SUMMARY-BASIC-SERVICES        05182
261600                                 WS-SUMMARY-BASIC-CLAIM-COUNT     05183
261700                                 WS-SUMMARY-MAJOR-MED-PAID        05184
261800                                 WS-SUMMARY-MAJOR-MED-SERVICES    05185
261900                                 WS-SUMMARY-MAJ-MED-CLAIM-COUNT   05186
262000                                 WS-SUMMARY-RIDER-MED-PAID        05187
262100                                 WS-SUMMARY-RIDER-MED-SERVICES    05188
262200                                 WS-SUMMARY-RIDER-MED-CLAIM-CNT.  05189
262300     EJECT                                                        05190
265200 CA-1395-SAVE-OTHER-ELEMENTS.                                     05191
265300***************************************************************   05192
265400*    PURPOSE :                                                *   05193
265500*             MOVE THE CURRENT RECORD TO SAVE AREA (PREVIOUS) *   05194
265600***************************************************************   05195
265700     SKIP3                                                        05196
265800     MOVE MS-CSS-GROUP-NO           TO   WS-PREV-GROUP-NUMBER.    05197
265900     MOVE MS-CSS-ACT-SUB-DIV        TO   WS-PREV-SUB-DIVISION.    05198
266000     MOVE MS-CSS-CLAIM-NO           TO   WS-PREV-CLAIM-NO.        05199
266100     MOVE MS-CSS-CHK-YY             TO   WS-PREV-PAID-YY.         05200
266200     MOVE MS-CSS-CHK-MM             TO   WS-PREV-PAID-MM.         05201
266300     MOVE MS-CSS-CHK-DD             TO   WS-PREV-PAID-DD.         05202
266400     MOVE MS-CSS-INC-YY             TO   WS-PREV-INC-YY.          05203
266500     MOVE MS-CSS-INC-MM             TO   WS-PREV-INC-MM.          05204
266600     MOVE MS-CSS-INC-DD             TO   WS-PREV-INC-DD.          05205
266700     MOVE MS-CSS-EDS-TOS            TO   WS-PREV-EDS-TOS.         05206
266800     MOVE MS-CSS-ASO-IND            TO   WS-PREV-ASO-IND.         05207
266900     MOVE MS-CSS-SNR-CARE-IND       TO   WS-PREV-SNR-CARE-IND.    05208
267000     MOVE MS-CSS-SOURCE-CODE        TO   WS-PREV-SOURCE-CODE.     05209
267100     MOVE MS-CSS-POINT-OF-SERVICE-IND  TO WS-PREV-POS-IND.        05210
267100     MOVE MS-CORE-MERGE-SYSTEM-IND     TO WS-PREV-CMS-IND.        05211
267200     SKIP3                                                        05212
267300     MOVE MS-CSS-ACTUARIAL-PLAN-CODE TO  WS-PREV-ACT-PLN-CODE.    05213
267400     MOVE MS-CSS-PATIENT-FIRST      TO   WS-PREV-FIRST-INITIAL.   05214
267500     MOVE SPACES                    TO   WS-PREV-LAST-NAME.       05215
267600     MOVE MS-CSS-PATIENT-LAST       TO   WS-PREV-LAST-NAME.       05216
267700     MOVE MS-CSS-SUB-ID             TO   WS-PREV-SUB-IDENT.       05217
267800     MOVE WS-SUMMARY-CLAIM-COUNT    TO   WS-PREV-REC-CLAIM-CNTR.  05218
267900     EJECT                                                        05219
269900 CA-1400-ACCUM-MCS-IP-REC.                                        05220
270000***************************************************************   05221
270100*    PURPOSE :                                                *   05222
270200*             ACCUMULATES THE INPUT COUNTS FOR MCS FILE.      *   05223
270300***************************************************************   05224
270400     SKIP2                                                        05225
270500     ADD WS-POSITIVE-ONE TO  WS-REC-CNTS(2).                      05226
270600     SKIP2                                                        05227
270700     ADD MS-CSS-PAID-AMOUNT-BASIC-ONLY MS-CSS-PAID-AMOUNT-RIDER TO05228
270800                                      WS-LOB-LIAB-AMTS(2).        05229
270900     ADD MS-CSS-UNITS-BASIC-ONLY MS-CSS-UNITS-RIDER TO            05230
271000                                      WS-SERVICES-CNTS(2).        05231
271100     SKIP2                                                        05232
271200     ADD MS-CSS-PAID-AMOUNT-MAJOR-MED TO                          05233
271300                                      WS-LOB-LIAB-AMTS(3).        05234
271400     SKIP2                                                        05235
271500     ADD MS-CSS-UNITS-MAJOR-MED TO                                05236
271600                                      WS-SERVICES-CNTS(3).        05237
271700     EJECT                                                        05238
277300 CA-1500-SELECT-OR-BYPASS.                                        05239
277400***************************************************************   05240
277500*    PURPOSE :                                                *   05241
277600*             IDENTIFIES RECORDS WHICH ARE TO BE BYPASSED AND *   05242
277700*             ACCUMULATES BYPASS    COUNTS FOR MCS FILE.      *   05243
277800***************************************************************   05244
277900     SKIP2                                                        05245
278000     IF  M-DRUG-MEDICAL-CLAIM                                     05246
278100         MOVE WS-TWO     TO WS-BYPASS-IND                         05247
278200     ELSE                                                         05248
278300             PERFORM CA-1510-CHK-DOLLAR-AND-SRV-CNT.              05249
279400     SKIP2                                                        05250
279500     IF  WS-M-DRUG-BYPASS                                         05251
279600      SKIP1                                                       05252
279700          ADD WS-POSITIVE-ONE TO WS-REC-CNTS(17)                  05253
279800      SKIP1                                                       05254
279900          ADD MS-CSS-PAID-AMOUNT-BASIC-ONLY                       05255
280000                MS-CSS-PAID-AMOUNT-RIDER TO                       05256
280100                        WS-LOB-LIAB-AMTS(17)                      05257
280200      SKIP1                                                       05258
280300          ADD MS-CSS-UNITS-BASIC-ONLY MS-CSS-UNITS-RIDER TO       05259
280400                        WS-SERVICES-CNTS(17)                      05260
280500      SKIP1                                                       05261
280600          ADD MS-CSS-PAID-AMOUNT-MAJOR-MED TO                     05262
280700                        WS-LOB-LIAB-AMTS(18)                      05263
280800      SKIP1                                                       05264
280900          ADD MS-CSS-UNITS-MAJOR-MED TO                           05265
281000                        WS-SERVICES-CNTS(18).                     05266
281100     SKIP2                                                        05267
281200     IF  WS-BYPASS                                                05268
281300      SKIP1                                                       05269
281400          ADD WS-POSITIVE-ONE TO WS-REC-CNTS(12)                  05270
281500      SKIP1                                                       05271
281600          ADD MS-CSS-PAID-AMOUNT-BASIC-ONLY                       05272
281700                MS-CSS-PAID-AMOUNT-RIDER TO                       05273
281800                        WS-LOB-LIAB-AMTS(12)                      05274
281900      SKIP1                                                       05275
282000          ADD MS-CSS-UNITS-BASIC-ONLY MS-CSS-UNITS-RIDER TO       05276
282100                        WS-SERVICES-CNTS(12)                      05277
282200      SKIP1                                                       05278
282300          ADD MS-CSS-PAID-AMOUNT-MAJOR-MED TO                     05279
282400                        WS-LOB-LIAB-AMTS(13)                      05280
282500      SKIP1                                                       05281
282600          ADD MS-CSS-UNITS-MAJOR-MED TO                           05282
282700                        WS-SERVICES-CNTS(13).                     05283
282800     EJECT                                                        05284
284500 CA-1510-CHK-DOLLAR-AND-SRV-CNT.                                  05285
284600***************************************************************   05286
284700*    PURPOSE :                                                *   05287
284800*             IDENTIFIES RECORDS WHICH ARE TO BE BYPASSED AND *   05288
284900*             ACCUMULATES BYPASS    COUNTS FOR MCS FILE.      *   05289
285000***************************************************************   05290
285100     SKIP2                                                        05291
285200                                                                  05292
285300     IF MS-CSS-PAID-AMOUNT-BASIC-ONLY      = ZERO  AND            05293
285400          MS-CSS-PAID-AMOUNT-RIDER         = ZERO  AND            05294
285500              MS-CSS-PAID-AMOUNT-MAJOR-MED = ZERO                 05295
285600              SET MCS-IX-1   TO WS-PLUS-ONE                       05296
285700              SET MCS-IX-2   TO WS-PLUS-ONE                       05297
285800              PERFORM CA-1520-CHECK-OTHER-FIELDS.                 05298
285900***               MOVE WS-ONE              TO WS-BYPASS-IND.      05299
286600 CA-1520-CHECK-OTHER-FIELDS.                                      05300
286700     PERFORM CA-1530-LOOK-UP-PAY-TABLE                            05301
286800        VARYING MS-CSS-IX-1 FROM 1 BY 1                           05302
286900        UNTIL MS-CSS-IX-1 > WS-PLUS-FOUR.                         05303
287000   SKIP2                                                          05304
286000     EJECT                                                        05305
288100 CA-1530-LOOK-UP-PAY-TABLE.                                       05306
288200      IF MS-CSS-APPROVED-AMT (MS-CSS-IX-1) = WS-PLUS-ZERO AND     05307
288300         MS-CSS-DEDUCTED-AMT (MS-CSS-IX-1) = WS-PLUS-ZERO AND     05308
288400         MS-CSS-COB-LVL  (MS-CSS-IX-1)     = WS-PLUS-ZERO AND     05309
288500         MS-CSS-COIN-AMT (MS-CSS-IX-1)     = WS-PLUS-ZERO         05310
288600         SET MS-CSS-IX-2  TO WS-PLUS-ONE                          05311
288700         PERFORM CA-1535-LOOK-UP-CO-PAY-TABLE                     05312
288800         VARYING MS-CSS-IX-2 FROM 1 BY 1                          05313
288900         UNTIL MS-CSS-IX-2 > WS-PLUS-FOUR.                        05314
289000   SKIP2                                                          05315
289500 CA-1535-LOOK-UP-CO-PAY-TABLE.                                    05316
289600      IF MS-CSS-CO-PAY-AMT (MS-CSS-IX-2) NUMERIC                  05317
289700         PERFORM CA-1536-CHECK-CO-PAY                             05318
289800      ELSE                                                        05319
289900         PERFORM CA-1540-CHECK-OTHER.                             05320
290000   SKIP2                                                          05321
290320 CA-1536-CHECK-CO-PAY.                                            05322
290330      IF MS-CSS-CO-PAY-AMT (MS-CSS-IX-2) = WS-PLUS-ZERO           05323
290340         PERFORM CA-1540-CHECK-OTHER.                             05324
290800 CA-1540-CHECK-OTHER.                                             05325
290900     IF MS-CSS-MAJ-MED-ALLOWED-AMT = WS-PLUS-ZERO AND             05326
291000        MS-CSS-POS-WITHHELD-AMT    = WS-PLUS-ZERO AND             05327
291100        MS-CSS-ALLOWED-AMT         = WS-PLUS-ZERO                 05328
291100        PERFORM CA-1541-CHECK-STUB                                05329
290900     ELSE                                                         05330
291200        MOVE WS-ZERO              TO WS-BYPASS-IND.               05331
291100 CA-1541-CHECK-STUB.                                              05332
291100        IF MS-CSS-LINE-STATUS   = WS-THREE AND                    05333
291100           MS-CSS-STUB-STATUS   = WS-THREE                        05334
                 CONTINUE                                               05335
              ELSE                                                      05336
291200           MOVE WS-ONE           TO WS-BYPASS-IND.                05337
291300   EJECT                                                          05338
345800 CA-1645-PAYMENT-TABLE.                                           05634
345900      MOVE MS-CSS-COV-TYPE-LVL (MS-CSS-IX-1)                      05635
346000            TO MS-COV-TYPE-LVL (MS-IX-1).                         05636
346100      IF   MS-CSS-PKG-OR-RIDER-NO (MS-CSS-IX-1) NUMERIC           05637
346200           MOVE MS-CSS-PKG-OR-RIDER-NO (MS-CSS-IX-1)              05638
346300            TO MS-PKG-OR-RIDER-NO (MS-IX-1)                       05639
346400      ELSE                                                        05640
346500            MOVE ZEROES TO MS-PKG-OR-RIDER-NO (MS-IX-1).          05641
346600      IF   MS-CSS-PAID-UNITS (MS-CSS-IX-1) NUMERIC                05642
346700           MOVE MS-CSS-PAID-UNITS (MS-CSS-IX-1)                   05643
346800              TO MS-PAID-UNITS (MS-IX-1)                          05644
346900      ELSE                                                        05645
347000           MOVE ZEROES TO MS-PAID-UNITS (MS-IX-1).                05646
347100      IF   MS-CSS-APPROVED-AMT (MS-CSS-IX-1) NUMERIC              05647
347200           MOVE MS-CSS-APPROVED-AMT (MS-CSS-IX-1)                 05648
347300              TO MS-APPROVED-AMT (MS-IX-1)                        05649
347400      ELSE                                                        05650
347500           MOVE ZEROES TO MS-APPROVED-AMT (MS-IX-1).              05651
347600      IF   MS-CSS-DEDUCTED-AMT (MS-CSS-IX-1) NUMERIC              05652
347700           MOVE MS-CSS-DEDUCTED-AMT (MS-CSS-IX-1)                 05653
347800           TO MS-DEDUCTED-AMT (MS-IX-1)                           05654
347900      ELSE                                                        05655
348000           MOVE ZEROES TO MS-DEDUCTED-AMT (MS-IX-1).              05656
348100      IF   MS-CSS-COB-LVL (MS-CSS-IX-1) NUMERIC                   05657
348200           MOVE MS-CSS-COB-LVL (MS-CSS-IX-1)                      05658
348300                   TO MS-COB-LVL (MS-IX-1)                        05659
348400      ELSE                                                        05660
348500           MOVE ZEROES TO MS-COB-LVL (MS-IX-1).                   05661
348600      IF MS-CSS-COIN-AMT (MS-CSS-IX-1) NUMERIC                    05662
348700         MOVE MS-CSS-COIN-AMT (MS-CSS-IX-1)                       05663
348800         TO MS-COIN-AMT (MS-IX-1)                                 05664
348900      ELSE                                                        05665
349000         MOVE ZEROES TO MS-COIN-AMT (MS-IX-1).                    05666
328800     MOVE MS-CSS-PAY-TYPE-IND (MS-CSS-IX-1)                       05667
328800                          TO   MS-PAY-TYPE-IND (MS-IX-1).         05668
349100      SET MS-IX-1  UP BY WS-PLUS-ONE.                             05669
349200 CA-1645-CO-PAY-TABLE.                                            05670
349300      IF   MS-CSS-CO-PAY-AMT  (MS-CSS-IX-2) NUMERIC               05671
349400           MOVE MS-CSS-CO-PAY-AMT  (MS-CSS-IX-2)                  05672
349500                    TO MS-CO-PAY-AMT  (MS-IX-2)                   05673
349600      ELSE                                                        05674
349700           MOVE ZEROES TO  MS-CO-PAY-AMT  (MS-IX-2).              05675
349800      SET MS-IX-2  UP BY WS-PLUS-ONE.                             05676
349900     EJECT                                                        05677
292900 CA-1610-ACCUM-BASIC-MED-TOTAL.                                   05678
291500***************************************************************   05679
291600*    PURPOSE :                                                *   05680
291700*             CALCULATE TOTALS RELATED TO THE BASIC PORTION   *   05681
291800*             OF THE MEDICAL CONTRACT                         *   05682
291900***************************************************************   05683
292000     SKIP3                                                        05684
292100     COMPUTE WS-SUMMARY-BASIC-PAID =                              05685
292200                        WS-SUMMARY-BASIC-PAID                     05686
293800                           + MS-CSS-PAID-AMOUNT-BASIC-ONLY.       05687
292400     SKIP3                                                        05688
292500     COMPUTE WS-SUMMARY-BASIC-SERVICES =                          05689
292600                        WS-SUMMARY-BASIC-SERVICES                 05690
294200                           + MS-CSS-UNITS-BASIC-ONLY.             05691
294300     EJECT                                                        05692
295700 CA-1620-ACCUM-MAJOR-MED-TOTAL.                                   05693
294500***************************************************************   05694
294600*    PURPOSE :                                                *   05695
294700*             CALCULATE TOTALS RELATED TO MAJOR MED PORTION   *   05696
294800*             OF THE MEDICAL CONTRACT                         *   05697
294900***************************************************************   05698
295000     SKIP3                                                        05699
296400     ADD MS-CSS-PAID-AMOUNT-MAJOR-MED TO                          05700
295200                          WS-SUMMARY-MAJOR-MED-PAID.              05701
295300     SKIP3                                                        05702
296700     ADD MS-CSS-UNITS-MAJOR-MED TO                                05703
295500                          WS-SUMMARY-MAJOR-MED-SERVICES.          05704
295600     EJECT                                                        05705
298400 CA-1630-ACCUM-RIDER-MED-TOTAL.                                   05706
295800***************************************************************   05707
295900*    PURPOSE :                                                *   05708
298700*             CALCULATE TOTALS RELATED TO RIDER PORTION       *   05709
296100*             OF THE MEDICAL CONTRACT                         *   05710
296200***************************************************************   05711
296300     SKIP3                                                        05712
299100     ADD MS-CSS-PAID-AMOUNT-RIDER  TO                             05713
299200                          WS-SUMMARY-RIDER-MED-PAID.              05714
299300     SKIP3                                                        05715
299400     ADD MS-CSS-UNITS-RIDER  TO                                   05716
299500                          WS-SUMMARY-RIDER-MED-SERVICES.          05717
299600     SKIP3                                                        05718
299700     EJECT                                                        05719
301500 CA-1640-WRITE-BASIC-MED-RECORD.                                  05720
297100***************************************************************   05721
297200*    PURPOSE :                                                *   05722
300100*             BEFORE WRITING BASIC RECORD, CHECK CONTENTS OF  *   05723
300200*             THE DATA FIELDS. IF THE DATA FIELDS ARE ALL     *   05724
300300*             ZEROS ------- BYPASS THE RECORD.                *   05725
300400***************************************************************   05726
300500     SKIP2                                                        05727
302300     PERFORM CA-1645-BASIC-DATA-CHECK.                            05728
300700     SKIP2                                                        05729
300800***  IF NOT WS-BYPASS                                             05730
300900**      AND NOT WS-BASIC-ALL-ZERO                                 05731
302800     PERFORM CA-1647-SETUP-BASIC-OUTPUT.                          05732
301200         SKIP1                                                    05733
301300     PERFORM X-3000-WRITE-RTN.                                    05734
301400     EJECT                                                        05735
304700 CA-1645-BASIC-DATA-CHECK.                                        05736
301600***************************************************************   05737
301700*    PURPOSE :                                                *   05738
301800*             BEFORE WRITING BASIC RECORD, CHECK CONTENTS OF  *   05739
301900*             THE DATA FIELDS. IF THE DATA FIELDS ARE ALL     *   05740
302000*             ZEROS ------- BYPASS THE RECORD.                *   05741
302100***************************************************************   05742
302200     SKIP2                                                        05743
305500     IF MS-CSS-PAID-AMOUNT-BASIC-ONLY = ZERO                      05744
304100     SKIP1                                                        05745
304200             MOVE WS-ONE  TO WS-BASIC-DATA-CHECK                  05746
305800             PERFORM CA-1646-ACCUM-BASIC-BYPASSES                 05747
304400     ELSE                                                         05748
304500             MOVE WS-ZERO TO  WS-BASIC-DATA-CHECK.                05749
304600     EJECT                                                        05750
307400 CA-1646-ACCUM-BASIC-BYPASSES.                                    05751
304800***************************************************************   05752
304900*    PURPOSE :                                                *   05753
307700*             ACCUMULATE TOTALS FOR BYPASSED BASIC RECORDS    *   05754
305300***************************************************************   05755
305400     SKIP2                                                        05756
308000             ADD MS-CSS-PAID-AMOUNT-BASIC-ONLY                    05757
308100                   TO WS-LOB-LIAB-AMTS(12)                        05758
308200     SKIP2                                                        05759
308300             ADD MS-CSS-UNITS-BASIC-ONLY                          05760
307200                   TO WS-SERVICES-CNTS(12).                       05761
307300     EJECT                                                        05762
309800 CA-1647-SETUP-BASIC-OUTPUT.                                      05763
307500***************************************************************   05764
307600*    PURPOSE :                                                *   05765
310100*             PREPARE THE BASIC MEDICAL RECORD FOR OUTPUT     *   05766
307800***************************************************************   05767
307900     SKIP2                                                        05768
310400     PERFORM CA-1647-10-ACCUM-OUTPUT-COUNTS.                      05769
310500     SKIP2                                                        05770
310600     PERFORM CA-1647-20-FORMAT-FIELDS.                            05771
310700     SKIP2                                                        05772
310800     PERFORM CA-1647-30-ATTACH-INDICATORS.                        05773
310900     EJECT                                                        05774
312500 CA-1647-10-ACCUM-OUTPUT-COUNTS.                                  05775
308700***************************************************************   05776
308800*    PURPOSE :                                                *   05777
312800*      ACCUMULATE TOTALS FOR THE BASIC MEDICAL OUTPUT RECORD  *   05778
309000***************************************************************   05779
309100     SKIP2                                                        05780
311600         ADD WS-POSITIVE-ONE            TO WS-REC-CNTS(7)         05781
311700                                           WS-REC-CNTS(16)        05782
313300         ADD MS-CSS-PAID-AMOUNT-BASIC-ONLY TO WS-LOB-LIAB-AMTS(7) 05783
313400                                           WS-LOB-LIAB-AMTS(16)   05784
313500         ADD MS-CSS-UNITS-BASIC-ONLY   TO  WS-SERVICES-CNTS(7)    05785
313600                                           WS-SERVICES-CNTS(16)   05786
313700         ADD WS-PREV-REC-CLAIM-CNTR    TO  WS-CLAIMS-CNTS(7)      05787
313800                                           WS-CLAIMS-CNTS(16).    05788
372700     ADD MS-CSS-PAID-AMOUNT-MAJOR-MED TO  WS-LOB-LIAB-AMTS(7)     05789
372800                                          WS-LOB-LIAB-AMTS(16)    05790
372900     ADD MS-CSS-UNITS-MAJOR-MED      TO   WS-SERVICES-CNTS(7)     05791
373000                                          WS-SERVICES-CNTS(16)    05792
434900     ADD MS-CSS-PAID-AMOUNT-RIDER      TO WS-LOB-LIAB-AMTS(7)     05793
435000                                          WS-LOB-LIAB-AMTS(16)    05794
435100     ADD MS-CSS-UNITS-RIDER            TO WS-SERVICES-CNTS(7)     05795
435200                                          WS-SERVICES-CNTS(16).   05796
313900     EJECT                                                        05797
330200 CA-1647-20-FORMAT-FIELDS.                                        05798
314100***************************************************************   05799
314200*    PURPOSE :                                                *   05800
314300*      SETUP THE ATTRIBUTES FOR BASIC MEDICAL OUTPUT RECORD   *   05801
314400***************************************************************   05802
314500     SKIP2                                                        05803
143500     MOVE LOW-VALUES  TO  SEL-PAID-CLAIMS-RECORD.                 05804
314600     IF CLAIM-WAS-ADJUSTED                                        05805
314700        MOVE WS-BASIC-MED-LOB-LIT   TO SEL-LOB-ADJUSTMENT-CODE    05806
314800     ELSE                                                         05807
314900     IF CLAIM-IS-QUESTIONABLE                                     05808
315000        MOVE WS-LITERAL-QUESTION-M  TO SEL-LOB-ADJUSTMENT-CODE    05809
315100     ELSE                                                         05810
315200        MOVE SPACE                  TO SEL-LOB-ADJUSTMENT-CODE.   05811
315300     SKIP2                                                        05812
331600     MOVE WS-PREV-GRP-NUM        TO SEL-GROUP-NUMBER              05813
331700     MOVE WS-PREV-SUB-DIVISION   TO SEL-DETAIL-SUB-DIVISION       05814
331800     MOVE WS-PREV-CLAIM-NO       TO SEL-DETAIL-CLAIM-NUMBER       05815
331900     MOVE WS-PREV-ACC-PAID-DATE-YY  TO SEL-ACCOUNT-PAID-DATE-YY   05816
332000     MOVE WS-PREV-ACC-PAID-DATE-MM  TO SEL-ACCOUNT-PAID-DATE-MM   05817
332100     MOVE WS-PREV-ACC-PAID-DATE-DD  TO SEL-ACCOUNT-PAID-DATE-DD   05818
332200     MOVE WS-PREV-PAID-YY        TO SEL-PAID-YEAR                 05819
332300     MOVE WS-PREV-PAID-MM        TO SEL-PAID-MONTH                05820
332400     MOVE WS-PREV-PAID-DD        TO SEL-DETAIL-PAID-DAY           05821
332500     MOVE WS-PREV-INC-YY         TO SEL-INCURRED-YEAR             05822
332600     MOVE WS-PREV-INC-MM         TO SEL-INCURRED-MONTH            05823
332700     MOVE WS-PREV-INC-DD         TO SEL-DETAIL-INCURRED-DAY       05824
332800     MOVE WS-PREV-EDS-TOS        TO SEL-TYPE-OF-SERVICE           05825
332900     MOVE WS-PREV-SNR-CARE-IND   TO SEL-SENIOR-CARE-IND           05826
333000     MOVE WS-PREV-POS-IND        TO SEL-POS-INDICATOR             05827
316800     MOVE WS-PREV-CMS-IND        TO MS-CMS-INDICATOR.             05828
333100     MOVE WS-PREV-SOURCE-CODE    TO SEL-CSS-SOURCE-CODE           05829
333200     SKIP2                                                        05830
333300     MOVE WS-DETAIL              TO SEL-DETAIL-RCD-INDICATOR      05831
333400     MOVE SPACES                 TO SEL-DETAIL-RCD-IDENTIFICATION 05832
333500     IF  MS-CSS-PAYABLE AND  MS-CSS-STUB-STATUS  =  WS-THREE      05833
333600        MOVE WS-BASIC-MED-LOB-LIT   TO SEL-LINE-OF-BUSINESS       05834
333700        MOVE WS-ONE            TO SEL-DETAIL-MED-SURG-BREAKDOWN   05835
333800     ELSE                                                         05836
333900        MOVE WS-BASIC-MED-LOB-LIT   TO SEL-LINE-OF-BUSINESS       05837
334000        MOVE WS-FOUR          TO SEL-DETAIL-MED-SURG-BREAKDOWN    05838
334100        PERFORM  CA-1647-40-CHECK-REJECTS.                        05839
334200     SKIP2                                                        05840
334300     MOVE WS-PREV-ACT-PLN-CODE   TO SEL-PLAN-CODE                 05841
334400     MOVE WS-PREV-SUB-IDENT      TO SEL-SUBSCRIBER-ID             05842
334500     MOVE WS-PREV-FIRST-INITIAL  TO SEL-DETAIL-PATIENT-INITIAL    05843
334600     MOVE WS-PREV-LAST-NAME      TO SEL-DETAIL-PATIENT-LAST-NAME  05844
334700     SKIP2                                                        05845
334800     ADD  MS-CSS-PAID-AMOUNT-BASIC-ONLY    TO WS-OUT-LIABILITY.   05846
340800     ADD  MS-CSS-PAID-AMOUNT-RIDER         TO WS-OUT-LIABILITY.   05847
341000     ADD  MS-CSS-PAID-AMOUNT-MAJOR-MED     TO WS-OUT-LIABILITY.   05848
334900     MOVE WS-OUT-LIABILITY         TO SEL-PAID-CLAIM-LIABILITY.   05849
340800     MOVE ZEROES                           TO WS-OUT-LIABILITY.   05850
335000     MOVE MS-CSS-UNITS-BASIC-ONLY TO SEL-NUMBER-OF-SERVICES       05851
335100     MOVE WS-PREV-REC-CLAIM-CNTR TO SEL-NUMBER-OF-CLAIMS          05852
335200     MOVE WS-PLUS-ZERO           TO SEL-NUMBER-OF-DAYS-VISITS.    05853
335300     MOVE SPACES                 TO SEL-BANK-B-INDICATOR.         05854
335400     MOVE SPACES                 TO SEL-ICHIS-COLLECTION-CODE.    05855
335600     MOVE WS-BASIC-MED-LOB-LIT   TO SEL-RECORD-TYPE.              05856
300800     IF  WS-BYPASS                                                05857
211600             MOVE ECKS TO SEL-AUDIT-IND                           05858
300800     ELSE                                                         05859
211600             MOVE SPACES TO SEL-AUDIT-IND.                        05860
335700     MOVE MS-CSS-CLAIM-VERSION                                    05861
335800                                   TO MS-CLAIM-VERSION.           05862
335900     MOVE MS-CSS-CLAIM-LINE-NO                                    05863
336000                                   TO MS-CLAIM-LINE-NO.           05864
336100     MOVE MS-CSS-IED-DATE-DISPL    TO MS-IED-DATE-DISPL.          05865
336200     MOVE MS-CSS-LAST-PROCESS-DATE                                05866
336300                              TO     MS-LAST-PROCESS-DATE.        05867
336400     MOVE MS-CSS-CHECK-PAID-DATE   TO  MS-CHECK-PAID-DATE.        05868
336500     MOVE MS-CSS-ACTUARIAL-GROUP-TYPE                             05869
336600                   TO   MS-ACTUARIAL-GROUP-TYPE.                  05870
336700     MOVE MS-CSS-MGT-PAY-IND                                      05871
336800                    TO   MS-MGT-PAY-IND.                          05872
336900     MOVE MS-CSS-PATIENT-AGE                                      05873
337000                      TO   MS-PATIENT-AGE.                        05874
337100     MOVE MS-CSS-PATIENT-DOB-DISPL                                05875
337200                       TO  MS-PATIENT-DOB-DISPL.                  05876
337300     MOVE MS-CSS-PATIENT-SEX-REL                                  05877
337400                       TO  MS-PATIENT-SEX-REL.                    05878
337500     MOVE MS-CSS-PATIENT-COB-IND                                  05879
337600                        TO  MS-PATIENT-COB-IND.                   05880
337700     MOVE MS-CSS-PATIENT-FIRST                                    05881
337800                         TO MS-PATIENT-FIRST.                     05882
337900     MOVE MS-CSS-SUB-CNTR-TYPE                                    05883
338000                           TO  MS-SUB-CNTR-TYPE.                  05884
338100     MOVE MS-CSS-ASO-IND     TO MS-ASO-IND.                       05885
338200     MOVE MS-CSS-STUB-STATUS TO MS-STUB-STATUS.                   05886
338300     MOVE MS-CSS-STUB-ERROR  TO MS-STUB-ERROR.                    05887
338400     MOVE MS-CSS-STUB-ERROR-STATUS TO MS-STUB-ERROR-STATUS.       05888
338500     MOVE MS-CSS-PROVIDER-NO                                      05889
338600                          TO MS-PROVIDER-NO.                      05890
338700     MOVE MS-CSS-PROV-CNTR-PRACTICE                               05891
338800                         TO MS-PROV-CNTR-PRACTICE.                05892
338900     MOVE MS-CSS-PROV-PRICING-AREA                                05893
339000                          TO MS-PROV-PRICING-AREA.                05894
339100     MOVE MS-CSS-PROV-SPECIALTY                                   05895
339200                           TO MS-PROV-SPECIALTY.                  05896
339300     MOVE MS-CSS-PROV-STAT                                        05897
339400                           TO MS-PROV-STAT.                       05898
339500     MOVE MS-CSS-CPT4-PROC-CODE                                   05899
339600                             TO MS-CPT4-PROC-CODE.                05900
339700     MOVE MS-CSS-PROC-CODE-MOD1                                   05901
339800                              TO MS-PROC-CODE-MOD1.               05902
339900     MOVE MS-CSS-PROC-CODE-MOD2  TO MS-PROC-CODE-MOD2.            05903
340000     MOVE MS-CSS-MCS-TOS      TO MS-MCS-TOS.                      05904
340100     MOVE MS-CSS-PLACE           TO MS-PLACE.                     05905
340200     MOVE MS-CSS-PRIME-DIAG-CODE  TO MS-PRIME-DIAG-CODE.          05906
340300     MOVE MS-CSS-TO-DOS           TO MS-TO-DOS.                   05907
340400     MOVE MS-CSS-FEE              TO MS-FEE.                      05908
340500     MOVE MS-CSS-LINE-STATUS      TO MS-LINE-STATUS.              05909
340600     MOVE MS-CSS-PAID-AMOUNT-BASIC-ONLY                           05910
340700                           TO MS-PAID-AMOUNT-BASIC-ONLY.          05911
340800     MOVE MS-CSS-PAID-AMOUNT-RIDER                                05912
340900                             TO MS-PAID-AMOUNT-RIDER.             05913
341000     MOVE MS-CSS-PAID-AMOUNT-MAJOR-MED                            05914
341100                             TO MS-PAID-AMOUNT-MAJOR-MED.         05915
341200     MOVE MS-CSS-MAJ-MED-ALLOWED-AMT                              05916
341300                 TO MS-MAJ-MED-ALLOWED-AMT.                       05917
341400     MOVE MS-CSS-HIGHEST-LEVEL  TO MS-HIGHEST-LEVEL               05918
341500                                    WS-ENTRY.                     05919
341600     SET MS-CSS-IX-1  TO WS-PLUS-ONE.                             05920
341700     SET MS-IX-1  TO WS-PLUS-ONE.                                 05921
341800     PERFORM CA-LOAD-PAYMENT-TABLE                                05922
341900     VARYING MS-CSS-IX-1 FROM 1                                   05923
342000**     BY 1 UNTIL MS-CSS-IX-1 > WS-ENTRY.                         05924
342100                BY 1 UNTIL MS-CSS-IX-1 > WS-PLUS-FOUR.            05925
342200     MOVE MS-CSS-POINT-OF-SERVICE-IND                             05926
342300                                     TO  MS-POS-NETWORK-IND.      05927
342400     MOVE MS-CSS-POS-PCP-PROVIDER                                 05928
342500                                     TO  MS-POS-PCP-PROVIDER.     05929
342600     MOVE MS-CSS-POS-PADDE-PATNO                                  05930
342700                                     TO  MS-POS-PADDE-PATNO.      05931
342800     MOVE MS-CSS-POS-WITHHELD-AMT                                 05932
342900                                     TO  MS-POS-WITHHELD-AMT.     05933
343000     MOVE MS-CSS-ALLOWED-AMT                                      05934
343100                                     TO  MS-ALLOWED-AMT.          05935
343200     SET MS-CSS-IX-2  TO WS-PLUS-ONE.                             05936
343300     SET MS-IX-2  TO WS-PLUS-ONE.                                 05937
343400     PERFORM CA-LOAD-CO-PAY-TABLE                                 05938
343500     VARYING MS-CSS-IX-2 FROM 1                                   05939
343600**   BY 1 UNTIL MS-CSS-IX-2 > WS-ENTRY.                           05940
343700     BY 1 UNTIL MS-CSS-IX-2 > WS-PLUS-FOUR.                       05941
343800     MOVE MS-CSS-DEDUCTIBLE-IND                                   05942
343900                   TO MS-DEDUCTIBLE-IND.                          05943
344000     MOVE MS-CSS-PRIME-LINE-ERROR                                 05944
344100                   TO MS-PRIME-LINE-ERROR.                        05945
344200     MOVE MS-CSS-LINE-ERROR-STATUS                                05946
344300                   TO MS-LINE-ERROR-STATUS.                       05947
344400     MOVE MS-CSS-PAYEE-IND                                        05948
344500                              TO MS-PAYEE-IND.                    05949
344600     MOVE MS-CSS-SOURCE-CODE                                      05950
344700                              TO   MS-SOURCE-CODE.                05951
344800     MOVE MS-CSS-GROUP-TYPE    TO   MS-GROUP-TYPE.                05952
344900     MOVE MS-CSS-SECONDARY-ID                                     05953
345000                              TO   MS-SECONDARY-ID.               05954
345100     MOVE MS-CSS-PAYROL-LOC                                       05955
345200                              TO   MS-PAYROL-LOC.                 05956
345100     MOVE MS-CSS-PAYROL-LOC-LAST4                                 05955
345200                              TO   MS-PAYROL-LOC-LAST4.           05956
345300     MOVE MS-CSS-PROV-ZIP-CODE                                    05957
345400                              TO   MS-PROV-ZIP-CODE.              05958
345500     MOVE MS-CSS-PAT-ID                                           05959
345600                              TO   MS-PAT-ID.                     05960
325700     MOVE MS-CSS-TEFRA-COBRA-IND    TO MS-TEFRA-COBRA-IND.        05961
345700     MOVE WS-PLUS-ZERO  TO WS-ENTRY.                              05962
      **** ADDED BY E.VOLYNSKY FOR CONVERSION ON 01/04.94    ***********05963
325800     MOVE MS-CSS-ICD9-CODE             TO MS-ICD9-CODE.           05964
325800     MOVE MS-CSS-SUB-SUFFIX            TO MS-SUB-SUFFIX.          05965
325800     MOVE MS-CSS-CAPITATION-IND        TO MS-CAPITATION-IND.      05966
325800     MOVE MS-CSS-GROUP-COUNTY-CODE     TO MS-GROUP-COUNTY-CODE.   05967
325800     IF   MS-CSS-ADMIN-FEE             NUMERIC                    05968
325800          MOVE MS-CSS-ADMIN-FEE   TO MS-ADMIN-FEE                 05969
325800     ELSE                                                         05970
325800          MOVE ZEROES             TO MS-ADMIN-FEE.                05971
188100     IF  MS-CSS-ACCESS-FEE          NUMERIC                       05972
189300         MOVE  MS-CSS-ACCESS-FEE         TO  MS-ACCESS-FEE        05973
188100     ELSE                                                         05974
189300         MOVE  ZEROES                    TO  MS-ACCESS-FEE.       05975
      **** ADDED BY E.VOLYNSKY FOR NEW FIELDS    05/11.94    ***********05976
325800     MOVE MS-CSS-CS90-CHARGE-REG-IND                              05977
189300                           TO MS-CS90-CHARGE-REG-IND.             05978
325800     MOVE MS-CSS-ITS-INDICATOR                                    05979
189300                           TO MS-ITS-HOME-INDICATOR.              05980
188100     IF  MS-CSS-ORIG-PLAN-NUMBER NUMERIC                          05981
189300         MOVE  MS-CSS-ORIG-PLAN-NUMBER  TO                        05982
189300                   MS-ITS-ORIG-PLAN-NUMBER                        05983
188100     ELSE                                                         05984
189300         MOVE  ZEROES        TO  MS-ITS-ORIG-PLAN-NUMBER.         05985
472800******************************************************************05986
472900**** ADDED BY D.GANOR    FOR NEW FIELDS    08/10.94    ***********05987
473000     IF  MS-CSS-UNITS-BASIC-ONLY   NUMERIC                        05988
473100         MOVE  MS-CSS-UNITS-BASIC-ONLY                            05989
473200                             TO  MS-UNITS-BASIC                   05990
473300     ELSE                                                         05991
473400         MOVE  ZEROES        TO  MS-UNITS-BASIC.                  05992
473500                                                                  05993
473600     IF  MS-CSS-UNITS-RIDER   NUMERIC                             05994
473700         MOVE  MS-CSS-UNITS-RIDER                                 05995
473800                             TO  MS-UNITS-RIDER                   05996
473900     ELSE                                                         05997
474000         MOVE  ZEROES        TO  MS-UNITS-RIDER.                  05998
474100                                                                  05999
474200     IF  MS-CSS-UNITS-MAJOR-MED  NUMERIC                          06000
474300         MOVE  MS-CSS-UNITS-MAJOR-MED                             06001
474400                             TO  MS-UNITS-MAJMED                  06002
474500     ELSE                                                         06003
474600         MOVE  ZEROES        TO  MS-UNITS-MAJMED.                 06004
435400******************************************************************06005
435500**** ADDED BY E.VOLYNSKY FOR NEW FIELDS    08/01.95    ***********06006
437500******************************************************************06007
437200         MOVE  MS-CSS-GROUP-TIER      TO MS-GROUP-TIER.           06008
437200         MOVE  MS-CSS-GROUP-QUARTILE  TO MS-GROUP-QUARTILE.       06009
437200         MOVE  MS-CSS-EDS-TOS-SUPP    TO MS-EDS-TOS-SUPP.         06010
474700                                                                  06011
188100     IF  MS-CSS-CHECK-NO         NUMERIC                          06012
189300         MOVE  MS-CSS-CHECK-NO          TO                        06013
189300                   MS-CHECK-NO                                    06014
188100     ELSE                                                         06015
189300         MOVE  ZEROES        TO  MS-CHECK-NO.                     06016
188100     IF  MS-CSS-COB-PAID-AMT     NUMERIC                          06017
189300         MOVE  MS-CSS-COB-PAID-AMT      TO                        06018
189300                   MS-COB-PAID-AMT                                06019
188100     ELSE                                                         06020
189300          MOVE       ZEROES        TO  MS-COB-PAID-AMT.           06021
060700      MOVE MS-CSS-TOPPS-GROUP-NO       TO  MS-TOPPS-GROUP-NO      06022
060700      MOVE MS-CSS-TOPPS-TREATMENT-TYPE TO  MS-TOPPS-TREATMENT-TYPE06023
060700      MOVE MS-CSS-TOPPS-BENEFIT-PACKAGE                           06024
060700                                 TO MS-TOPPS-BENEFIT-PACKAGE      06025
060700      MOVE MS-CSS-TOPPS-NETWRK-CHOICE-IND  TO                     06026
060700                 MS-TOPPS-NETWRK-CHOICE-IND.                      06027
060700      MOVE MS-CSS-NETWORK-REFERAL-NO   TO  MS-NETWORK-REFERAL-NO  06028
060700      MOVE MS-CSS-FULL-PROVIDER-TYPE   TO  MS-FULL-PROVIDER-TYPE  06029
060700      MOVE MS-CSS-FULL-PROVIDER-ID     TO  MS-FULL-PROVIDER-ID.   06030
189300     MOVE MS-CSS-DEDUCT-CARRYOVER-IND                             06031
189300                             TO MS-DEDUCT-CARRYOVER-IND.          06032
189300     IF   MS-CSS-MISC-INDICATOR  NUMERIC                          06033
189300          MOVE MS-CSS-MISC-INDICATOR  TO MS-MISC-INDICATOR        06034
189300     ELSE                                                         06035
189300       MOVE ZEROES     TO MS-MISC-INDICATOR.                      06036
189300     MOVE MS-CSS-COB-CALC-IND                                     06037
189300                             TO MS-COB-CALC-IND.                  06038
      ******* ADDED AS OF 11/01/96 FOR EXPANDED RECORD **************   06039
00039        IF MS-CSS-POOL-A-AMT-BS NUMERIC                            06040
00039           MOVE MS-CSS-POOL-A-AMT-BS TO MS-POOLA-AMT-BS            06041
00039        ELSE                                                       06042
00039           MOVE ZEROES               TO MS-POOLA-AMT-BS.           06043
00039         IF MS-CSS-POOL-A-AMT-RD NUMERIC                           06044
00039            MOVE MS-CSS-POOL-A-AMT-RD TO MS-POOLA-AMT-RD           06045
00039         ELSE                                                      06046
00039            MOVE ZEROES               TO MS-POOLA-AMT-RD.          06047
00039                                                                   06048
00039         IF MS-CSS-POOL-A-AMT-MM NUMERIC                           06049
00039            MOVE MS-CSS-POOL-A-AMT-MM TO MS-POOLA-AMT-MM           06050
00039         ELSE                                                      06051
00039            MOVE ZEROES               TO MS-POOLA-AMT-MM.          06052
00039                                                                   06053
00039       MOVE MS-CSS-POOLING-COUNTY-CODE TO MS-POOLING-COUNTY-CODE.  06054
00039       MOVE MS-CSS-PROGRAM-CODE    TO MS-PROGRAM-CODE.             06055
00039       MOVE MS-CSS-UNDERWRT-CORP-CD TO MS-UNDERWRT-CORP-CD.        06056
00039       MOVE MS-CSS-LOB              TO  MS-LOB.                    06057
00039       MOVE MS-CSS-CLASS-RISK-CD    TO  MS-CLASS-RISK-CD.          06058
00039       MOVE MS-CSS-RISK-POOL-CD     TO  MS-RISK-POOL-CD.           06059
00039       MOVE MS-CSS-CONTRACT-TYPE    TO   MS-CONTRACT-TYPE.         06060
00027       IF   MS-CSS-CONTRACT-NO  NUMERIC                            06061
00027            MOVE MS-CSS-CONTRACT-NO TO MS-CONTRACT-NO              06062
00027       ELSE                                                        06063
00027            MOVE ZEROES             TO MS-CONTRACT-NO.             06064
00039       MOVE MS-CSS-COVERAGE-TYPE    TO MS-COVERAGE-TYPE.           06065
00039       MOVE MS-CSS-CATEGORY-NO      TO MS-CATEGORY-NO.             06066
00039       MOVE MS-CSS-BENE-LEV-NO      TO MS-BENE-LEV-NO.             06067
00027       IF   MS-CSS-BPI-NUMBER   NUMERIC                            06068
00027            MOVE MS-CSS-BPI-NUMBER  TO MS-BPI-NUMBER               06069
00027       ELSE                                                        06070
00027            MOVE ZEROES             TO MS-BPI-NUMBER.              06071
00039       MOVE MS-CSS-REP-CODE         TO MS-REP-CODE.                06072
00039       MOVE MS-CSS-PROD-COMB-CODE   TO MS-PROD-COMB-CODE.          06073
00039       MOVE MS-CSS-AGENCY-NUMBER    TO MS-AGENCY-NUMBER.           06074
            MOVE MS-CSS-GRP-KEY-ID       TO MS-GRP-KEY-ID.              06077
00039       MOVE MS-CSS-ELECT-SUBMIT-CD  TO MS-ELECT-SUBMIT-CD.         06079
00039       MOVE MS-CSS-FUNDING-ARRANGM  TO MS-FUNDING-ARRANGM.         06080
00039       MOVE MS-CSS-BC-PLAN-CODE     TO MS-BC-PLAN-CODE.            06081
00039       MOVE MS-CSS-NASCO-BANK-ACCT-TYPE                            06082
00039                              TO MS-NASCO-BANK-ACCT-TYPE.          06083
00039       MOVE MS-CSS-SPEC-CARE-COORDINATOR                           06084
00039                              TO MS-SPEC-CARE-COORDINATOR.         06085
00039       MOVE MS-CSS-MSK-CATEGORY   TO MS-MSK-CATEGORY.              06086
00039       MOVE MS-CSS-TOPPS-PROGRAM-NUM TO MS-TOPPS-PROGRAM-NUM.      06087
00039       MOVE MS-CSS-TOPPS-SUB-PROG-NUM TO MS-TOPPS-SUB-PROG-NUM.    06088
00039       MOVE MS-CSS-ORIG-PROC-SYST-CLAIMNO TO                       06089
00039                                   MS-ORIG-PROC-SYST-CLAIMNO.      06090
00039       IF   MS-CSS-INTEREST-AMOUNT       NUMERIC                   06093
00039            MOVE MS-CSS-INTEREST-AMOUNT         TO                 06094
00039                   MS-INTEREST-AMOUNT                              06095
00039       ELSE                                                        06096
00039            MOVE ZEROES TO   MS-INTEREST-AMOUNT.                   06097
            IF   MS-CSS-NO-DAYS-PROMPT-PAY-INT   NUMERIC                06098
                 MOVE MS-CSS-NO-DAYS-PROMPT-PAY-INT   TO                06099
                                 MS-NO-DAYS-PROMPT-PAY-INT              06100
            ELSE                                                        06101
                 MOVE ZEROES TO MS-NO-DAYS-PROMPT-PAY-INT.              06102
            MOVE MS-CSS-CLK-START-DAT-PROM-PAY TO                       06103
                 MS-CLK-START-DAT-PROM-PAY.                             06104
            IF   MS-CSS-SURCHARGE-AMT            NUMERIC                06105
                 MOVE MS-CSS-SURCHARGE-AMT       TO                     06106
                                 MS-SURCHARGE-AMT                       06107
            ELSE                                                        06108
                 MOVE ZEROES TO MS-SURCHARGE-AMT.                       06109
           MOVE MS-CSS-EMHC-IND  TO  MS-EMHC-IND.                       06110
           MOVE MS-CSS-IPA-PANEL-NO   TO MS-IPA-PANEL-NO.               06111
           MOVE MS-CSS-DSU-NUMBER     TO MS-DSU-NUMBER.                 06112
230100     IF   MS-CSS-DATE-RECEIVED-DISPL   NUMERIC                    06113
230100          MOVE MS-CSS-DATE-RECEIVED-DISPL                         06114
230100                                  TO MS-DATE-RECEIVED-DISPL       06115
230100     ELSE                                                         06116
230100          MOVE ZEROES             TO MS-DATE-RECEIVED-DISPL.      06117
230800     MOVE WS-DATE-CONVERT-DISPL  TO MS-DATE-CONVERT-DISPL.        06118
           MOVE MS-CSS-PRODUCT-VARIATION-CODE    TO                     06119
                                     MS-PRODUCT-VARIATION-CODE.         06120
           MOVE MS-CSS-PRODUCT-VARIATION-2BYTE   TO                     06119
                                     MS-PRODUCT-VARIATION-2BYTE.        06120
            MOVE MS-CSS-PRODUCT-CLASSIF-CD        TO                    06121
                                 MS-PRODUCT-CLASSIF-CD.                 06122
            MOVE MS-CSS-INTEREST-INDICATOR    TO                        06123
                                 MS-INTEREST-INDICATOR.                 06124
            MOVE MS-CSS-ENCOUNTER-INDICATOR    TO                       06125
                                 MS-ENCOUNTER-INDICATOR.                06126
      ***ADDED  NEW SRS FIELDS AS OF  10/00                          ***06127
           IF   MS-CSS-PENALTY-AMT NUMERIC                              00634
                MOVE MS-CSS-PENALTY-AMT     TO  MS-PENALTY-AMT          00634
           ELSE                                                         00634
                MOVE ZEROES                 TO  MS-PENALTY-AMT.         00634
            MOVE MS-CSS-PROFITABILITY-CODE     TO                       06125
                                 MS-PROFITABILITY-CODE.                 06126
            MOVE MS-CSS-PROFIT-PRODUCT-HI-LEVEL  TO                     06125
                                 MS-PROFIT-PRODUCT-HI-LEVEL.            06126
            MOVE MS-CSS-BOOK-OF-BUSINESS       TO                       06125
                                 MS-BOOK-OF-BUSINESS.                   06126
            MOVE MS-CSS-PACKAGE-NUMBER     TO  MS-PACKAGE-NUMBER.       06126
            IF   MS-CSS-ACT-PKG-CODE       NUMERIC                      06126
                 MOVE MS-CSS-ACT-PKG-CODE  TO  MS-ACT-PKG-CODE          06126
            ELSE                                                        06126
                 MOVE ZEROES               TO  MS-ACT-PKG-CODE.         06126
            MOVE MS-CSS-BC-PLAN-CODE-OLD   TO  MS-BC-PLAN-CODE-OLD.     06126
            IF   MS-CSS-ACT-PKG-CODE-OLD   NUMERIC                      06126
                 MOVE MS-CSS-ACT-PKG-CODE-OLD  TO  MS-ACT-PKG-CODE-OLD  06126
            ELSE                                                        06126
                 MOVE ZEROES                   TO  MS-ACT-PKG-CODE-OLD. 06126
            MOVE MS-CSS-GROUP-TYPE-OLD     TO  MS-GROUP-TYPE-OLD.       06126
            MOVE MS-CSS-MGT-PAY-IND-OLD    TO  MS-MGT-PAY-IND-OLD.      06126
            MOVE MS-CSS-ACTUAR-GRP-TYPE-OLD                             06126
                                  TO MS-ACTUARIAL-GRP-TYPE-OLD.         06126
            MOVE MS-CSS-UNDRW-CORP-CD-OLD  TO                           06126
                                           MS-UNDERWRT-CORP-CD-OLD.     06126
            MOVE MS-CSS-FUNDING-COMB       TO  MS-PRODUCT-FUND-CD.      06126
            MOVE MS-CSS-RATING-COMB        TO  MS-RATING-COMB.          06126
            MOVE ZEROES                    TO  MS-PKG-OR-RIDER-NO-OLD.  06126
            MOVE MS-CSS-DEST-ID            TO  MS-DEST-ID.              06126
            MOVE MS-CSS-BILLING-IND        TO  MS-BILLING-IND.          06126
            MOVE MS-CSS-BASIC-SRS-LOB      TO  MS-BASIC-SRS-LOB.        06126
            IF   MS-CSS-TOTAL-DIFFERENTIAL-AMT NUMERIC                  06126
                 MOVE MS-CSS-TOTAL-DIFFERENTIAL-AMT                     06126
                                   TO MS-TOTAL-DIFFERENTIAL-AMT         06126
            ELSE                                                        06126
                 MOVE ZEROES       TO MS-TOTAL-DIFFERENTIAL-AMT.        06126
            MOVE MS-CSS-HIPAA-ALT-ID       TO  MS-HIPAA-ALT-ID.         06126
            MOVE MS-CSS-HIPAA-PREFIX       TO  MS-HIPAA-PREFIX.         06126
           IF   MS-CSS-CDHP-HRA-AMOUNT  NUMERIC                         02356
                MOVE MS-CSS-CDHP-HRA-AMOUNT    TO  MS-CDHP-HRA-AMOUNT   02356
           ELSE                                                         02356
                MOVE ZEROES                  TO  MS-CDHP-HRA-AMOUNT.    02356
      **** MULTIPURSE CHANGES  START
           IF   MS-CSS-CDHP-HSA-AMOUNT  NUMERIC
                MOVE MS-CSS-CDHP-HSA-AMOUNT    TO  MS-CDHP-HSA-AMOUNT
           ELSE
                MOVE ZEROES                  TO  MS-CDHP-HSA-AMOUNT.
      *-
            MOVE MS-CSS-CDHP-IND             TO  MS-CDHP-IND.
      **** MULTIPURSE CHANGES  END
      * DONE AS PART OF CR#117642-WP MARKET SEGMENTATION
            MOVE MS-CSS-WP-MARKET-SEGMENT  TO MS-WP-MKT-SEGMENT.
      *
            IF   MS-CSS-WP-SIC-CODE  NUMERIC
                 MOVE MS-CSS-WP-SIC-CODE     TO  MS-SIC-CODE
            ELSE
                 MOVE ZEROES              TO  MS-SIC-CODE.
      *
      * NPI CHANGES STARTS HERE.
           MOVE MS-CSS-NPI-PROVIDER-ID  TO  MS-NPI-CODE.
      * NPI CHANGES ENDS HERE.
      * RSI INDICATOR CHANGES STARTS HERE.
           MOVE MS-CSS-RSI-IND          TO  MS-RSI-IND.
      * RSI INDICATOR CHANGES ENDS HERE.
      * NARROW NETWORK INDICATOR CHANGES STARTS HERE.
           MOVE MS-CSS-NARROW-NET-IND   TO   MS-NARROW-NET-IND.
      * NARROW NETWORK INDICATOR CHANGES ENDS HERE.
      * BLOOM EXCHANGE INDICATOR CHANGES STARTS HERE.
           MOVE MS-CSS-BLOOM-EXCHANGE-IND TO MS-BLOOM-EXCH-IND.
      * BLOOM EXCHANGE INDICATOR CHANGES ENDS HERE.
      * EFT INDICATOR CHANGES STARTS HERE.
           MOVE MS-CSS-EFT-INDICATOR      TO MS-EFT-IND.
      * EFT INDICATOR CHANGES ENDS HERE.
      *
      * ICD10 PROJECT CHANGES STARTS HERE.
           MOVE MS-CSS-ICD10-PRIME-DIAG-CODE  TO
                                        MS-ICD10-PRIME-DIAG-CODE.
           MOVE MS-CSS-ICD9-ICD10-IND         TO MS-ICD9-ICD10-IND.
           MOVE MS-CSS-ICD10-CPT4-PROC-CODE   TO
                                        MS-ICD10-CPT4-PROC-CODE.
      * ICD10 PROJECT CHANGES ENDS HERE.
      * ITS PROJECT CHANGES STARTS HERE.
           IF  MS-CSS-ITS-SUPP-AMT IS NUMERIC
               MOVE MS-CSS-ITS-SUPP-AMT
                                    TO MS-ITS-SUPP-AMT
           ELSE MOVE ZEROES         TO MS-ITS-SUPP-AMT.
      * ITS PROJECT CHANGES ENDS HERE.
      * EBF PROJECT CHANGES STARTS HERE.
           MOVE MS-CSS-EBF-IND      TO MS-EBF-IND.
      * EBF PROJECT CHANGES ENDS HERE.
      * CHANGES FOR LOCAL ACCESS FEE PROJECT START.
           IF  MS-CSS-LOC-ACCESS-FEE IS NUMERIC
               MOVE MS-CSS-LOC-ACCESS-FEE
                                    TO MS-LOC-ACES-FEE
           ELSE MOVE ZEROES         TO MS-LOC-ACES-FEE.
      * CHANGES FOR LOCAL ACCESS FEE PROJECT END.
      * CHANGES FOR POOLING REPORT PROJECT START.
           MOVE MS-CSS-PLACE-OF-SERVICE     TO MS-PLACE-OF-SERVICE.
      * CHANGES FOR POOLING REPORT PROJECT END.
      * CHANGES FOR POOL P AMOUNT START.
           IF  MS-CSS-POOL-P-AMOUNT IS NUMERIC
               MOVE MS-CSS-POOL-P-AMOUNT
                                    TO MS-POOL-P-AMOUNT
           ELSE MOVE ZEROES         TO MS-POOL-P-AMOUNT.
      * CHANGES FOR POOL P AMOUNT END.
      * CHANGES FOR TOTAL CHARGES START.
           IF  MS-CSS-TOTAL-CHARGES-1 IS NUMERIC
               MOVE MS-CSS-TOTAL-CHARGES-1
                                    TO MS-TOTAL-CHARGES-1
           ELSE MOVE ZEROES         TO MS-TOTAL-CHARGES-1.
      * CHANGES FOR TOTAL CHARGES END.
      ** CR#109780 - JAB2I0 - STARTS CHANGES HERE - RBRVS %
           IF   MS-CSS-RBRVS-PCT  NUMERIC                               02356
                MOVE MS-CSS-RBRVS-PCT    TO  MS-RBRVS-PCT               02356
           ELSE                                                         02356
                MOVE ZEROES                  TO  MS-RBRVS-PCT.          02356
      ** CR#109780 - JAB2I0 - ENDS CHANGES HERE - RBRVS %
      **CR#133719 -->
           MOVE MS-CSS-NIA-CAP-IND TO MS-NIA-CAP-IND.
           IF  MS-CSS-CAP-PAY-VENDOR-AMT IS NUMERIC
               MOVE MS-CSS-CAP-PAY-VENDOR-AMT TO
                               MS-CAP-PAY-VENDOR-AMT
           ELSE
               MOVE +0 TO  MS-CAP-PAY-VENDOR-AMT.
      **CR#133719
      **CR#147553-NCN PROJECT CHANGES BEGIN.
           MOVE MS-CSS-NCN-INDICATOR TO MS-NCN-INDICATOR.
           IF  MS-CSS-NCN-FEE IS NUMERIC
               MOVE MS-CSS-NCN-FEE TO MS-NCN-FEE
           ELSE
               MOVE +0 TO  MS-NCN-FEE.
      **CR#147553-NCN PROJECT CHANGES END.
      **CR#160671-NCN PROJECT EXPANTION CHANGES BEGIN.
           IF  MS-CSS-NCN-GRP-FEE IS NUMERIC
               MOVE MS-CSS-NCN-GRP-FEE TO MS-NCN-GROUP-FEE
           ELSE
               MOVE +0 TO  MS-NCN-GROUP-FEE.
      **CR#160671-NCN PROJECT EXPANTION CHANGES END.
      **CR#146587-MCS PROMPT PAY AMOUNT CHANGES BEGINS.
           IF  MS-CSS-MCS-PROMPT-PAY-INT IS NUMERIC
               MOVE MS-CSS-MCS-PROMPT-PAY-INT    TO
                               MS-MCS-PROMPT-PAY-INT
           ELSE
               MOVE +0 TO  MS-MCS-PROMPT-PAY-INT.
      **CR#146587-MCS PROMPT PAY AMOUNT CHANGES ENDS.
      **-CR139676 CHANGES
           MOVE MS-CSS-VOLUNTARY-IND TO MS-VOLUNTARY-IND.
474900* ACCESS FEE EXCLUSION START                                      06127
474900     PERFORM 1600-ASSIGN-ACCESS-FEE-IND THRU 1600-EXIT.           06127
474900* ACCESS FEE EXCLUSION END                                        06127
474900******************************************************************06127
       1600-ASSIGN-ACCESS-FEE-IND.
           MOVE   SPACES              TO MS-ACCESS-FEE-IND
           SET FEE-IX TO WS-PLUS-1.
           PERFORM 1610-SEARCH-ACCESS-FEE-GROUP THRU 1610-EXIT
                VARYING FEE-IX
               FROM WS-PLUS-1 BY WS-PLUS-1 UNTIL
               FEE-IX > WS-ACCESS-RECORDS.
       1600-EXIT.
            EXIT.
       1610-SEARCH-ACCESS-FEE-GROUP.
            SEARCH ACCESS-FEE-GROUP
            WHEN SEL-GROUP-NUMBER =  AFEE-GROUP (FEE-IX)
            AND  '***'            =  AFEE-SUBDIV (FEE-IX)
                 MOVE 'Y'               TO MS-ACCESS-FEE-IND
            WHEN SEL-GROUP-NUMBER =  AFEE-GROUP (FEE-IX)
            AND  SEL-DETAIL-SUB-DIVISION =  AFEE-SUBDIV (FEE-IX)
                 MOVE 'Y'               TO MS-ACCESS-FEE-IND.
       1610-EXIT.
            EXIT.
474900******************************************************************06127
345800 CA-LOAD-PAYMENT-TABLE.                                           06128
345900      MOVE MS-CSS-COV-TYPE-LVL (MS-CSS-IX-1)                      06129
346000            TO MS-COV-TYPE-LVL (MS-IX-1).                         06130
346100      IF   MS-CSS-PKG-OR-RIDER-NO (MS-CSS-IX-1) NUMERIC           06131
346200           MOVE MS-CSS-PKG-OR-RIDER-NO (MS-CSS-IX-1)              06132
346300            TO MS-PKG-OR-RIDER-NO (MS-IX-1)                       06133
346400      ELSE                                                        06134
346500            MOVE ZEROES TO MS-PKG-OR-RIDER-NO (MS-IX-1).          06135
346600      IF   MS-CSS-PAID-UNITS (MS-CSS-IX-1) NUMERIC                06136
346700           MOVE MS-CSS-PAID-UNITS (MS-CSS-IX-1)                   06137
346800              TO MS-PAID-UNITS (MS-IX-1)                          06138
346900      ELSE                                                        06139
347000           MOVE ZEROES TO MS-PAID-UNITS (MS-IX-1).                06140
347100      IF   MS-CSS-APPROVED-AMT (MS-CSS-IX-1) NUMERIC              06141
347200           MOVE MS-CSS-APPROVED-AMT (MS-CSS-IX-1)                 06142
347300              TO MS-APPROVED-AMT (MS-IX-1)                        06143
347400      ELSE                                                        06144
347500           MOVE ZEROES TO MS-APPROVED-AMT (MS-IX-1).              06145
347600      IF   MS-CSS-DEDUCTED-AMT (MS-CSS-IX-1) NUMERIC              06146
347700           MOVE MS-CSS-DEDUCTED-AMT (MS-CSS-IX-1)                 06147
347800           TO MS-DEDUCTED-AMT (MS-IX-1)                           06148
347900      ELSE                                                        06149
348000           MOVE ZEROES TO MS-DEDUCTED-AMT (MS-IX-1).              06150
348100      IF   MS-CSS-COB-LVL (MS-CSS-IX-1) NUMERIC                   06151
348200           MOVE MS-CSS-COB-LVL (MS-CSS-IX-1)                      06152
348300                   TO MS-COB-LVL (MS-IX-1)                        06153
348400      ELSE                                                        06154
348500           MOVE ZEROES TO MS-COB-LVL (MS-IX-1).                   06155
348600      IF MS-CSS-COIN-AMT (MS-CSS-IX-1) NUMERIC                    06156
348700         MOVE MS-CSS-COIN-AMT (MS-CSS-IX-1)                       06157
348800         TO MS-COIN-AMT (MS-IX-1)                                 06158
348900      ELSE                                                        06159
349000         MOVE ZEROES TO MS-COIN-AMT (MS-IX-1).                    06160
328800     MOVE MS-CSS-PAY-TYPE-IND (MS-CSS-IX-1)                       06161
328800                          TO   MS-PAY-TYPE-IND (MS-IX-1).         06162
349100      SET MS-IX-1  UP BY WS-PLUS-ONE.                             06163
349200 CA-LOAD-CO-PAY-TABLE.                                            06164
349300      IF   MS-CSS-CO-PAY-AMT  (MS-CSS-IX-2) NUMERIC               06165
349400           MOVE MS-CSS-CO-PAY-AMT  (MS-CSS-IX-2)                  06166
349500                    TO MS-CO-PAY-AMT  (MS-IX-2)                   06167
349600      ELSE                                                        06168
349700           MOVE ZEROES TO  MS-CO-PAY-AMT  (MS-IX-2).              06169
349800      SET MS-IX-2  UP BY WS-PLUS-ONE.                             06170
349900     EJECT                                                        06171
351900 CA-1647-30-ATTACH-INDICATORS.                                    06172
350200***************************************************************   06173
350300*    PURPOSE :                                                *   06174
350400*            MOVE DESCRIPTIVE INDICATORS BASED UPON THE INPUT *   06175
350500*             FILE DESCRIPTION 88 LEVEL INDICATORS            *   06176
350600*             ( IF ANY APPLY )                                *   06177
350700***************************************************************   06178
350800     SKIP2                                                        06179
C21TSR*    IF SEL-PAID-YEAR          LESS THAN WS-90                    06180
                                                                        06181
                                                                        06182
C21TSR     CALL  C2140C02  USING SEL-PAID-YEAR C21SEL-PAID-YEAR         06183
C21TSR               C21-BASE-ON C21-WORK-AREA                          06184
C21TSR     CALL  C2140C02  USING WS-90 C21WS-90 C21-BASE-ON             06185
C21TSR               C21-WORK-AREA                                      06186
C21TSR     IF C21SEL-PAID-YEAR LESS THAN C21WS-90                       06187
351000         MOVE WS-LITERAL-A TO SEL-CLAIM-SERVICE-CATEGORY          06188
351100     ELSE                                                         06189
351200         MOVE SPACE        TO SEL-CLAIM-SERVICE-CATEGORY.         06190
351300     SKIP3                                                        06191
351400     MOVE SPACES           TO SEL-ICHIS-PRESSO-FLAG               06192
351500                              SEL-ICHIS-RIDER-FLAG                06193
351600                              SEL-ICHIS-SUPP-PAYMENT-FLAG.        06194
351700     MOVE WS-PREV-ASO-IND  TO SEL-MCS-ASO-FLAG.                   06195
353600     MOVE MS-CSS-POINT-OF-SERVICE-IND TO SEL-POS-INDICATOR.       06196
353700     EJECT                                                        06197
354300 CA-1647-40-CHECK-REJECTS.                                        06198
354400     IF MS-CSS-MANUAL-REJECT  OR MS-CSS-REJECT                    06199
354500        ADD WS-PLUS-ONE   TO  WS-REJECT-CNTS                      06200
354600        PERFORM CA-1647-50-CHECK-REJECTS-PAYMT.                   06201
354700     EJECT                                                        06202
356400 CA-1647-50-CHECK-REJECTS-PAYMT.                                  06203
356500     IF MS-CSS-PAID-AMOUNT-RIDER  NUMERIC                         06204
356600        MOVE  MS-CSS-PAID-AMOUNT-RIDER   TO WS-REJECT-RIDER-AMT   06205
356700     ELSE                                                         06206
356800        MOVE  ZEROES                  TO WS-REJECT-RIDER-AMT.     06207
356900     IF  MS-CSS-PAID-AMOUNT-MAJOR-MED NUMERIC                     06208
357000         MOVE MS-CSS-PAID-AMOUNT-MAJOR-MED TO                     06209
357100                      WS-REJECT-MAJOR-MED-AMT                     06210
357200     ELSE                                                         06211
357300         MOVE ZEROES    TO   WS-REJECT-MAJOR-MED-AMT.             06212
357400     IF MS-CSS-PAID-AMOUNT-BASIC-ONLY NUMERIC                     06213
357500        MOVE  MS-CSS-PAID-AMOUNT-BASIC-ONLY TO                    06214
357600                      WS-REJECT-BASIC-AMT                         06215
357700     ELSE                                                         06216
357800        MOVE  ZEROES TO  WS-REJECT-BASIC-AMT.                     06217
357900     EJECT                                                        06218
489800 CA-1800-BAL-MCS-FILE.                                            06219
358100***************************************************************   06220
358200*    PURPOSE :                                                *   06221
482800*             COMPARE THE MEDICAL CLAIM FILE TRAILER RECORD   *   06222
482900*             TOTALS TO THE WORKING STORAGE INPUT FILE TOTALS *   06223
483000*             AND THEN COMPARE THE PROCESSING COUNTS TO THE   *   06224
483100*             EXTRACT FILE OUTPUT RECORD COUNTS               *   06225
483200***************************************************************   06226
483300     SKIP2                                                        06227
483400     IF WS-MCS-TRLR-RECS         = WS-REC-CNTS(2) +               06228
483500                                   WS-REC-CNTS(3)                 06229
483600        AND                                                       06230
483700        WS-MCS-COMPUTED-LIAB =                                    06231
483800                  WS-LOB-LIAB-AMTS(7)  + WS-LOB-LIAB-AMTS(8)      06232
483900                + WS-LOB-LIAB-AMTS(12) + WS-LOB-LIAB-AMTS(13)     06233
484000                + WS-LOB-LIAB-AMTS(17) + WS-LOB-LIAB-AMTS(18)     06234
484100     SKIP2                                                        06235
491500                     PERFORM CA-1899-MEDICAL-FILE-BALANCE         06236
      **IR309075- VIN0I0-IBM- VIJAYA START CHANGES
           COMPUTE WS-PGM-COUNT=WS-REC-CNTS(2) +  WS-REC-CNTS(3)
491500     DISPLAY 'PROGRAM ACCUMULATED COUNT ON MANREP FILE : '        06236
                            WS-PGM-COUNT
491500     DISPLAY 'TRAILER COUNT ON MANREP FILE             : '        06236
491500                      WS-MCS-TRLR-RECS                            06236
      **IR309075- VIN0I0-IBM- VIJAYA END   CHANGES
484300     ELSE                                                         06237
484400         DISPLAY WS-DISPLAY-1                                     06238
484500         MOVE WS-ABEND-CODE(7) TO USER-ABEND-CODE                 06239
484600         DISPLAY USER-ABEND-CODE                                  06240
484700         DISPLAY WS-MCS-ABEND-TBL(1)                              06241
484800         DISPLAY WS-MCS-ABEND-TBL(2)                              06242
484900         DISPLAY WS-DISPLAY-1                                     06243
485000         MOVE WS-MCS-TRLR-RECS         TO                         06244
485100                               WS-EDITED-DISPLY-CNTS              06245
485200         DISPLAY WS-MCS-ABEND-TBL(3) WS-EDITED-DISPLY-CNTS        06246
485300                                                                  06247
485400         MOVE WS-PLUS-ZERO   TO  WS-EDITED-DISPLY-CNTS            06248
485500         MOVE WS-REC-CNTS(2) TO  WS-EDITED-DISPLY-CNTS            06249
485600         DISPLAY WS-MCS-ABEND-TBL(4) WS-EDITED-DISPLY-CNTS        06250
485700                                                                  06251
485800         MOVE WS-PLUS-ZERO   TO  WS-EDITED-DISPLY-CNTS            06252
485900         MOVE WS-REC-CNTS(3) TO  WS-EDITED-DISPLY-CNTS            06253
486000         DISPLAY WS-MCS-ABEND-TBL(5) WS-EDITED-DISPLY-CNTS        06254
486100                                                                  06255
486200         MOVE WS-LOB-LIAB-AMTS(2) TO WS-EDITED-DISPLY-AMTS        06256
486300         DISPLAY WS-MCS-ABEND-TBL(6) WS-EDITED-DISPLY-AMTS        06257
486400         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06258
486500                                                                  06259
486600         MOVE WS-LOB-LIAB-AMTS(3) TO WS-EDITED-DISPLY-AMTS        06260
486700         DISPLAY WS-MCS-ABEND-TBL(7) WS-EDITED-DISPLY-AMTS        06261
486800         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06262
486900                                                                  06263
487000         MOVE WS-LOB-LIAB-AMTS(7) TO WS-EDITED-DISPLY-AMTS        06264
487100         DISPLAY WS-MCS-ABEND-TBL(8) WS-EDITED-DISPLY-AMTS        06265
487200         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06266
487300                                                                  06267
487400         MOVE WS-LOB-LIAB-AMTS(8) TO WS-EDITED-DISPLY-AMTS        06268
487500         DISPLAY WS-MCS-ABEND-TBL(9) WS-EDITED-DISPLY-AMTS        06269
487600         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06270
487700                                                                  06271
487800         MOVE WS-LOB-LIAB-AMTS(12) TO WS-EDITED-DISPLY-AMTS       06272
487900         DISPLAY WS-MCS-ABEND-TBL(10) WS-EDITED-DISPLY-AMTS       06273
488000         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06274
488100                                                                  06275
488200         MOVE WS-LOB-LIAB-AMTS(13) TO WS-EDITED-DISPLY-AMTS       06276
488300         DISPLAY WS-MCS-ABEND-TBL(11) WS-EDITED-DISPLY-AMTS       06277
488400         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06278
488500                                                                  06279
488600         MOVE WS-LOB-LIAB-AMTS(17) TO WS-EDITED-DISPLY-AMTS       06280
488700         DISPLAY WS-MCS-ABEND-TBL(12) WS-EDITED-DISPLY-AMTS       06281
488800         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06282
488900                                                                  06283
489000         MOVE WS-LOB-LIAB-AMTS(18) TO WS-EDITED-DISPLY-AMTS       06284
489100         DISPLAY WS-MCS-ABEND-TBL(13) WS-EDITED-DISPLY-AMTS       06285
489200         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-AMTS              06286
489300                                                                  06287
489400         DISPLAY WS-DISPLAY-1                                     06288
489500                                                                  06289
489600         PERFORM Z-CALL-BOMBER.                                   06290
489700      EJECT                                                       06291
498100 CA-1899-MEDICAL-FILE-BALANCE.                                    06292
489900***************************************************************   06293
490000*    PURPOSE :                                                *   06294
498400*             ISSUE MESSAGE ON SYSOUT THAT FILE INPUT BALANCED*   06295
490500***************************************************************   06296
490600     SKIP2                                                        06297
497700     DISPLAY SPACES.                                              06298
497800     DISPLAY WS-BALANCED-FILE-MESSAGE(2).                         06299
497900     DISPLAY SPACES.                                              06300
497000      EJECT                                                       06301
499100 D-1000-PROCESS-DENTAL-FILE.                                      06302
498200***************************************************************   06303
498300*    PURPOSE :                                                *   06304
499400*             OPEN, PROCESS, BALANCE, AND CLOSE DENTAL CLAIMS *   06305
499500*             INPUT FILE                                      *   06306
499600***************************************************************   06307
499700     SKIP3                                                        06308
499800     OPEN INPUT I-DENTAL-PAID-CLAIMS-FILE.                        06309
499900     SKIP3                                                        06310
500000     PERFORM D-1105-READ-DENTAL-FIRST-TIME.                       06311
500100     SKIP3                                                        06312
500200     PERFORM D-1150-PROCESS-DENTAL                                06313
500300        UNTIL WS-EOF-DENT.                                        06314
500400     SKIP3                                                        06315
500500     PERFORM D-1500-BAL-DENTAL-FILE.                              06316
500600     SKIP3                                                        06317
500700     CLOSE I-DENTAL-PAID-CLAIMS-FILE.                             06318
500800     EJECT                                                        06319
500900 D-1105-READ-DENTAL-FIRST-TIME.                                   06320
501000***************************************************************   06321
501100*    PURPOSE :                                                *   06322
501200*             READS THE DENTAL FILE TO ENSURE NO EMPTY FILE   *   06323
501300*             CONDITION                                       *   06324
501400***************************************************************   06325
501500     SKIP3                                                        06326
501600     READ I-DENTAL-PAID-CLAIMS-FILE INTO                          06327
501700            IR-FN-INPUT-AREA AT END                               06328
501800                 MOVE WS-ZERO TO WS-EOF-DENT-FILE.                06329
501900     SKIP2                                                        06330
502000     IF  WS-EOF-DENT                                              06331
502100         DISPLAY WS-DISPLAY-1                                     06332
502200         DISPLAY WS-ERROR-MSG-TBL(9)                              06333
502300         MOVE WS-ABEND-CODE(8) TO USER-ABEND-CODE                 06334
502400         DISPLAY USER-ABEND-CODE                                  06335
502500         DISPLAY WS-DISPLAY-1                                     06336
502600         PERFORM Z-CALL-BOMBER.                                   06337
502700     EJECT                                                        06338
502800 D-1150-PROCESS-DENTAL.                                           06339
502900***************************************************************   06340
503000*    PURPOSE :                                                *   06341
503100*             READS THE DENTAL FILE AND ACCUMULATES           *   06342
503200*             RECORD COUNTS, CLAIMS COUNT DAYS COUNT AND      *   06343
503300*             AHS LIABILITY AMOUNTS.                          *   06344
503400***************************************************************   06345
503500     SKIP3                                                        06346
503600     IF IR-FN-TRAILER-RECORD                                      06347
503700        ADD IR-FN-TRAILER-RECORD-COUNT TO                         06348
503800             WS-DENTAL-TRLR-RECS                                  06349
503900        ADD IR-FN-TRAILER-AMOUNT-PAID TO                          06350
504000                 WS-DENTAL-TRLR-LIAB.                             06351
504100     SKIP3                                                        06352
504200     IF IR-FN-HEADER-RECORD                                       06353
504300        PERFORM D-1175-CHECK-DENTAL-DATES.                        06354
504400     SKIP3                                                        06355
504500     IF IR-FN-DETAIL-RECORD                                       06356
504600        PERFORM D-1175-CHECK-DENTAL-DATES                         06357
504700        PERFORM D-1200-ACCUM-IP-REC-RTN                           06358
504800        PERFORM D-1300-DENT-SEL-PD-CLM-RECORD                     06359
504900        IF NOT WS-BYPASS                                          06360
505000           PERFORM D-1375-DENTAL-CONTROL-BREAK                    06361
505100           PERFORM D-1405-DENT-SETUP-OUTPUT-REC                   06362
              ELSE                                                      06363
505000           PERFORM D-1375-DENTAL-CONTROL-BREAK                    06364
505100           PERFORM D-1405-DENT-SETUP-OUTPUT-REC.                  06365
505200     SKIP3                                                        06366
505300     MOVE WS-ZERO                 TO WS-BYPASS-IND.               06367
505400     SKIP3                                                        06368
505500     READ I-DENTAL-PAID-CLAIMS-FILE INTO                          06369
505600            IR-FN-INPUT-AREA AT END                               06370
505700                 MOVE WS-ZERO TO WS-EOF-DENT-FILE.                06371
505800     EJECT                                                        06372
505900 D-1175-CHECK-DENTAL-DATES.                                       06373
506000***************************************************************   06374
506100*    PURPOSE :                                                *   06375
506200*      CHECK FOR PRESENCE OF HEADER RECORD AND THEN CHECK     *   06376
506300*      HEADER DATES FOR THE PRODUCTION CYCLE                  *   06377
506400***************************************************************   06378
506500     SKIP2                                                        06379
506600     IF  IR-FN-DETAIL-RECORD                                      06380
506700         IF IR-FN-PAID-YEAR       NOT EQUAL CURRENT-YEAR          06381
506800            OR IR-FN-PAID-MONTH   NOT EQUAL CURRENT-MONTH         06382
               MOVE CURRENT-YEAR     TO IR-FN-PAID-YEAR
               MOVE CURRENT-MONTH    TO IR-FN-PAID-MONTH
               MOVE WS-01            TO IR-FN-PAID-DAY
506900******   DISPLAY WS-DISPLAY-1                                     06383
507000******   DISPLAY WS-ERROR-MSG-TBL(10)                             06384
507100******   MOVE WS-ABEND-CODE(9) TO USER-ABEND-CODE                 06385
507200******   DISPLAY USER-ABEND-CODE                                  06386
507300******   DISPLAY WS-DISPLAY-1                                     06387
507400*******  PERFORM Z-CALL-BOMBER.                                   06388
507500     SKIP2                                                        06389
507600     IF  IR-FN-HEADER-RECORD                                      06390
507700         IF IR-FN-HEADER-DATE-YYMM NOT EQUAL CURRENT-YYMM         06391
                  DISPLAY  'IRFNHEADER   '
                  DISPLAY  'IRFNHEADERDAT'    IR-FN-HEADER-DATE-YYMM
                   DISPLAY  'CURRENT      '   CURRENT-YYMM
507800             DISPLAY WS-DISPLAY-1                                 06392
507900             DISPLAY WS-ERROR-MSG-TBL(12)                         06393
508000             MOVE WS-ABEND-CODE(10) TO USER-ABEND-CODE            06394
508100             DISPLAY USER-ABEND-CODE                              06395
508200             DISPLAY WS-DISPLAY-1                                 06396
508300             PERFORM Z-CALL-BOMBER.                               06397
508400                                                                  06398
508500     EJECT                                                        06399
508600 D-1200-ACCUM-IP-REC-RTN.                                         06400
508700***************************************************************   06401
508800*    PURPOSE :                                                *   06402
508900*      COUNT RECORDS FROM DENTAL SYSTEM MONTHLY PAYMENTS      *   06403
509000*      FILE.                                                  *   06404
509100***************************************************************   06405
509200     SKIP2                                                        06406
509300     ADD WS-POSITIVE-ONE          TO WS-REC-CNTS(4).              06407
509400     SKIP2                                                        06408
509500     ADD IR-FN-AMOUNT-PAID        TO WS-LOB-LIAB-AMTS(4).         06409
509600     SKIP2                                                        06410
509700     ADD IR-FN-NUMBER-OF-SERVICES TO WS-SERVICES-CNTS(4).         06411
509800     EJECT                                                        06412
509900 D-1300-DENT-SEL-PD-CLM-RECORD.                                   06413
510000***************************************************************   06414
510100*    PURPOSE :                                                *   06415
510200*      SELECT RECORDS FROM DENTAL SYSTEM MONTHLY PAYMENTS     *   06416
510300*      FILE. ACCUMULATES TOTALS FOR BYPASS RECORDS.           *   06417
510400***************************************************************   06418
510500     SKIP2                                                        06419
510600     PERFORM D-1350-CHECK-DENT-FOR-BYP.                           06420
510700     SKIP2                                                        06421
510800     IF WS-BYPASS                                                 06422
510900     SKIP1                                                        06423
511000         ADD WS-POSITIVE-ONE          TO WS-REC-CNTS(14)          06424
511100     SKIP1                                                        06425
511200         ADD IR-FN-AMOUNT-PAID        TO WS-LOB-LIAB-AMTS(14)     06426
511300     SKIP1                                                        06427
511400         ADD IR-FN-NUMBER-OF-SERVICES TO WS-SERVICES-CNTS(14).    06428
511500     EJECT                                                        06429
511600 D-1350-CHECK-DENT-FOR-BYP.                                       06430
511700***************************************************************   06431
511800*    PURPOSE :                                                *   06432
511900*       IDENTIFY RECORDS ON INPUT WHICH ARE TO BE BYPASSED    *   06433
512000***************************************************************   06434
512100     SKIP2                                                        06435
512200     IF NOT IR-FN-DENTAL                                          06436
512300        MOVE WS-ONE                   TO   WS-BYPASS-IND.         06437
512400     SKIP2                                                        06438
512500     IF IR-FN-AMOUNT-PAID      =      WS-PLUS-ZERO                06439
512600        PERFORM D-1360-CHECK-SAVE-FIELDS                          06440
512600        PERFORM D-1361-CHECK-CLM-ST.                              06440
           IF IR-FN-AMOUNT-PAID  NOT =      WS-PLUS-ZERO                06439
              PERFORM D-1361-CHECK-CLM-ST.                              06440
512700     EJECT                                                        06441
512600 D-1360-CHECK-SAVE-FIELDS.                                        06442
             IF IR-FN-CHARGED-FEE      = WS-PLUS-ZERO  AND              06443
                IR-FN-COVERED-AMT      = WS-PLUS-ZERO  AND              06444
                IR-FN-DEDUCTIBLE-AMT   = WS-PLUS-ZERO  AND              06445
                IR-FN-COINSURANCE-AMT  = WS-PLUS-ZERO  AND              06446
                IR-FN-COB-AMT          = WS-PLUS-ZERO                   06447
512600          MOVE WS-ONE         TO   WS-BYPASS-IND.                 06448
512700     EJECT                                                        06449
512600 D-1361-CHECK-CLM-ST.                                             06440
             IF IR-FN-CLM-ST-IND   =   WS-TWO                           06443
512600          MOVE WS-ONE         TO   WS-BYPASS-IND.                 06448
512700     EJECT                                                        06449
512800 D-1375-DENTAL-CONTROL-BREAK.                                     06450
512900**************************************************************    06451
513000****   THIS ROUTINE CONTROLS ALL PROCESSING ACTIVITIES   *****    06452
513100****   ASSOCIATED WITH A CHANGE IN THE INPUT FILE DATA   *****    06453
513200*****  ELEMENTS WHICH WILL REQUIRE A SUMMARY RECORD TO   *****    06454
513300****   BE WRITTEN TO THE OUTPUT FILE.                    *****    06455
513400**************************************************************    06456
513500     SKIP2                                                        06457
513600     IF  IR-FN-CLAIM-NUMBER     =      WS-IRFN-CLAIM-NO           06458
513700     SKIP2                                                        06459
513800         MOVE WS-PLUS-ZERO TO                                     06460
513900                           WS-SUMMARY-DENTAL-CLAIM-COUNT          06461
514000     ELSE                                                         06462
514100         MOVE WS-POSITIVE-ONE TO                                  06463
514200                           WS-SUMMARY-DENTAL-CLAIM-COUNT.         06464
514300     SKIP2                                                        06465
514400     PERFORM D-1395-SAVE-OTHER-ELEMENTS.                          06466
514500     EJECT                                                        06467
514600 D-1395-SAVE-OTHER-ELEMENTS.                                      06468
514700***************************************************************   06469
514800*    PURPOSE :                                                *   06470
514900*             MOVE THE CURRENT RECORD TO SAVE AREA (PREVIOUS) *   06471
515000***************************************************************   06472
515100     SKIP3                                                        06473
515200     MOVE IR-FN-GROUP-NUMBER        TO   WS-PREV-GROUP-NUMBER.    06474
515300     MOVE IR-FN-SUB-DIVISION        TO   WS-PREV-SUB-DIVISION.    06475
515400     MOVE IR-FN-CLAIM-NUMBER        TO   WS-IRFN-CLAIM-NO.        06476
515500     MOVE IR-FN-PAID-MONTH          TO   WS-PREV-PAID-DENT-MM.    06477
515600     MOVE IR-FN-PAID-DAY            TO   WS-PREV-PAID-DENT-DD.    06478
515700     MOVE IR-FN-PAID-YEAR           TO   WS-PREV-PAID-DENT-YY.    06479
515800     MOVE IR-FN-INCURRED-MONTH      TO   WS-PREV-INC-DENT-MM.     06480
515900     MOVE IR-FN-INCURRED-DAY        TO   WS-PREV-INC-DENT-DD.     06481
516000     MOVE IR-FN-INCURRED-YEAR       TO   WS-PREV-INC-DENT-YY.     06482
516100     MOVE IR-FN-TYPE-OF-SERVICE     TO   WS-PREV-DENT-SERVICE.    06483
516200     MOVE IR-FN-SENIOR-CARE-COVERAGE TO  WS-PREV-SNR-CARE-IND.    06484
516300     SKIP3                                                        06485
516400     MOVE IR-FN-PLAN-OR-PACKAGE-CODE  TO SEL-PLAN-CODE.           06486
516500     MOVE IR-FN-PATIENT-FIRST-INITIAL TO WS-PREV-FIRST-INITIAL.   06487
516600     MOVE SPACES                    TO   WS-PREV-LAST-NAME.       06488
516700     MOVE IR-FN-PATIENT-LAST-NAME   TO   WS-PREV-LAST-NAME.       06489
516800     MOVE IR-FN-SUBSCRIBER-ID       TO   WS-PREV-SUB-IDENT.       06490
516900     MOVE WS-SUMMARY-DENTAL-CLAIM-COUNT                           06491
517000                                    TO WS-PREV-REC-CLAIM-CNTR.    06492
           MOVE IR-FN-CLM-ST-IND          TO WS-PREV-CLM-ST-IND.

517100     EJECT                                                        06493
517200 D-1405-DENT-SETUP-OUTPUT-REC.                                    06494
517300***************************************************************   06495
517400*    PURPOSE :                                                *   06496
517500*             PREPARE THE DETAIL RECORD FOR OUTPUT            *   06497
517600***************************************************************   06498
517700     SKIP2                                                        06499
517800     PERFORM D-1410-ACCUM-OUTPUT-COUNTS.                          06500
517900     SKIP2                                                        06501
518000     PERFORM D-1420-FORMAT-FIELDS.                                06502
518100     SKIP2                                                        06503
518200     PERFORM D-1430-ATTACH-INDICATORS.                            06504
518300     SKIP2                                                        06505
518400     PERFORM X-3000-WRITE-RTN.                                    06506
518500     EJECT                                                        06507
518600 D-1410-ACCUM-OUTPUT-COUNTS.                                      06508
518700***************************************************************   06509
518800*    PURPOSE :                                                *   06510
518900*             ACCUMULATE TOTALS FROM OUTPUT DETAIL RECORDS    *   06511
519000***************************************************************   06512
519100     SKIP2                                                        06513
519200     ADD WS-POSITIVE-ONE          TO   WS-REC-CNTS(9)             06514
519300                                       WS-REC-CNTS(16)            06515
519400     SKIP2                                                        06516
519500     ADD WS-PREV-REC-CLAIM-CNTR   TO WS-CLAIMS-CNTS(9)            06517
519600                                     WS-CLAIMS-CNTS(16)           06518
519700     SKIP2                                                        06519
519800     ADD IR-FN-AMOUNT-PAID        TO WS-LOB-LIAB-AMTS(9)          06520
519900                                     WS-LOB-LIAB-AMTS(16)         06521
520000     SKIP2                                                        06522
520100     ADD IR-FN-NUMBER-OF-SERVICES TO WS-SERVICES-CNTS(9)          06523
520200                                     WS-SERVICES-CNTS(16).        06524
520300     EJECT                                                        06525
520400 D-1420-FORMAT-FIELDS.                                            06526
520500***************************************************************   06527
520600*    PURPOSE :                                                *   06528
520700*             SET UP THE ATTRIBUTES FOR THE DETAIL RECORD     *   06529
520800***************************************************************   06530
520900     SKIP2                                                        06531
143500     MOVE LOW-VALUES  TO  SEL-PAID-CLAIMS-RECORD.                 06532
521100     MOVE WS-PREV-GROUP-NUMBER     TO  SEL-GROUP-NUMBER.          06533
521200     MOVE WS-PREV-SUB-DIVISION     TO  SEL-DETAIL-SUB-DIVISION.   06534
521300     MOVE WS-IRFN-CLAIM-NO         TO  SEL-DETAIL-CLAIM-NUMBER.   06535
521400     MOVE WS-PREV-PAID-DENT-MM     TO  SEL-PAID-MONTH.            06536
521500     MOVE WS-PREV-PAID-DENT-DD     TO  SEL-DETAIL-PAID-DAY.       06537
521600     MOVE WS-PREV-PAID-DENT-YY     TO  SEL-PAID-YEAR.             06538
521700     MOVE WS-PREV-INC-DENT-MM      TO  SEL-INCURRED-MONTH.        06539
521800     MOVE WS-PREV-INC-DENT-DD      TO  SEL-DETAIL-INCURRED-DAY.   06540
521900     MOVE WS-PREV-INC-DENT-YY      TO  SEL-INCURRED-YEAR.         06541
522000     MOVE SPACE               TO SEL-DETAIL-MED-SURG-BREAKDOWN.   06542
522100     MOVE WS-PREV-DENT-SERVICE     TO SEL-TYPE-OF-SERVICE.        06543
522200     MOVE SPACE                    TO SEL-POS-INDICATOR.          06544
522300     MOVE WS-PREV-SNR-CARE-IND     TO SEL-SENIOR-CARE-IND.        06545
522400     SKIP2                                                        06546
522500     MOVE WS-DETAIL        TO      SEL-DETAIL-RCD-INDICATOR       06547
522600     MOVE SPACES           TO      SEL-DETAIL-RCD-IDENTIFICATION  06548
522700     MOVE WS-DENTAL-LOB-LIT     TO  SEL-LINE-OF-BUSINESS.         06549
522800     SKIP2                                                        06550
523000     MOVE WS-PREV-SUB-IDENT     TO  SEL-SUBSCRIBER-ID.            06551
523100     MOVE WS-PREV-FIRST-INITIAL  TO SEL-DETAIL-PATIENT-INITIAL.   06552
523200     MOVE WS-PREV-LAST-NAME      TO SEL-DETAIL-PATIENT-LAST-NAME. 06553
523300     MOVE WS-PREV-REC-CLAIM-CNTR TO SEL-NUMBER-OF-CLAIMS.         06554
523400     MOVE IR-FN-NUMBER-OF-SERVICES TO SEL-NUMBER-OF-SERVICES.     06555
523500     MOVE ZEROES                TO SEL-NUMBER-OF-DAYS-VISITS.     06556
523600     MOVE IR-FN-AMOUNT-PAID     TO SEL-PAID-CLAIM-LIABILITY.      06557
523700     MOVE SPACE                 TO SEL-DETAIL-MED-SURG-BREAKDOWN. 06558
523800     MOVE SPACE                 TO SEL-CSS-SOURCE-CODE.           06559
523900     MOVE SPACE                 TO SEL-LOB-ADJUSTMENT-CODE.       06560
524000     MOVE ZEROES                TO SEL-ACCOUNT-PAID-DATE.         06561
524100     MOVE SPACES                TO SEL-BANK-B-INDICATOR.          06562
524200     MOVE SPACES                TO SEL-ICHIS-COLLECTION-CODE.     06563
516400     MOVE IR-FN-PLAN-OR-PACKAGE-CODE  TO SEL-PLAN-CODE.           06564
300800     IF  WS-BYPASS                                                06565
211600             MOVE ECKS TO SEL-AUDIT-IND                           06566
300800     ELSE                                                         06567
211600             MOVE SPACES TO SEL-AUDIT-IND.                        06568
524600***************************************************************   06569
524300     MOVE SPACES                 TO SEL-DENTAL-RECORD-SECTION.    06570
524300     MOVE WS-PREV-FIRST-INITIAL  TO DT-PATIENT-FIRST-INITIAL.     06571
010100     MOVE IR-FN-CAPITATION-IND   TO DT-FN-CAPITATION-IND.         06572
010200     MOVE IR-FN-CLAIM-GENERATION TO DT-FN-CLAIM-GENERATION.       06573
010300     MOVE IR-FN-PATIENT-FIRST-NAME                                06574
010300                                 TO DT-FN-PATIENT-FIRST-NAME.     06575
010400     MOVE IR-FN-PATIENT-DOB      TO DT-FN-PATIENT-DOB.            06576
010500     MOVE IR-FN-PATIENT-SEX      TO DT-FN-PATIENT-SEX.            06577
010600     MOVE IR-FN-PATIENT-RELATIONSHIP                              06578
010600                                 TO DT-FN-PATIENT-RELATIONSHIP.   06579
010700     MOVE IR-FN-PATIENT-TWIN-IND TO DT-FN-PATIENT-TWIN-IND.       06580
010800     MOVE IR-FN-SUBSCRIBER-ZIP-CODE                               06581
010800                                 TO DT-FN-SUBSCRIBER-ZIP-CODE.    06582
010900     MOVE IR-FN-DENTIST-ZIP-CODE TO DT-FN-DENTIST-ZIP-CODE.       06583
011000     MOVE IR-FN-DENTIST-COUNTY   TO DT-FN-DENTIST-COUNTY.         06584
011100     MOVE IR-FN-DENTIST-STATUS   TO DT-FN-DENTIST-STATUS.         06585
011200     MOVE IR-FN-LINE-NUMBER      TO DT-FN-LINE-NUMBER.            06586
011300     MOVE IR-FN-PROCEDURE-CODE   TO DT-FN-PROCEDURE-CODE.         06587
011400     MOVE IR-FN-TEETH            TO DT-FN-TEETH.                  06588
011500     MOVE IR-FN-CHARGED-FEE      TO DT-FN-CHARGED-FEE.            06589
011600     MOVE IR-FN-COVERED-AMT      TO DT-FN-COVERED-AMT.            06590
011700     MOVE IR-FN-DEDUCTIBLE-AMT   TO DT-FN-DEDUCTIBLE-AMT.         06591
011800     MOVE IR-FN-COINSURANCE-AMT  TO DT-FN-COINSURANCE-AMT.        06592
011900     MOVE IR-FN-COB-AMT          TO DT-FN-COB-AMT.                06593
012000     MOVE IR-FN-PAYMENT-EXPLANATION                               06594
012000                                 TO DT-FN-PAYMENT-EXPLANATION.    06595
012100     MOVE IR-FN-PAYEE-CODE       TO DT-FN-PAYEE-CODE.             06596
012200     MOVE IR-FN-ECR-CODE          TO DT-FN-ECR-CODE.              06597
012200     MOVE IR-FN-COIN-PERCENTAGE   TO DT-FN-COIN-PERCENTAGE.       06598
012200     MOVE IR-FN-GRP-NAT-LOCAL-IND TO DT-FN-GRP-NAT-LOCAL-IND.     06599
012200     MOVE IR-FN-GRP-RATING-IND    TO DT-FN-GRP-RATING-IND.        06600
012200     MOVE IR-FN-ALT-SUB-ID        TO DT-FN-ALT-SUB-ID.            06601
524600***************************************************************   06602
524700*    NEW FIELDS ADDED BY E.VOLYNSKY AS OF 1/95                *   06603
524600***************************************************************   06604
000310     MOVE IR-FN-DENTIST-ID       TO  DT-FN-DENTIST-ID.            06605
000310     MOVE IR-FN-DENTIST-CHKDIG   TO  DT-FN-DENTIST-CHKDIG.        06606
000310     MOVE IR-FN-DENTIST-MOC      TO  DT-FN-DENTIST-MOC.           06607
000310     MOVE IR-FN-PAYROLL-LOC      TO  DT-FN-PAYROLL-LOC.           06608
000310     MOVE IR-FN-SUBSIDIARY       TO  DT-FN-SUBSIDIARY.            06609
000310     MOVE IR-FN-GROUP-SECTION10  TO  DT-FN-GROUP-SECTION10.       06610
000310     MOVE IR-FN-PPO-CLAIM-IND    TO  DT-FN-PPO-CLAIM-IND.         06611
000310     IF   IR-FN-PPO-NETWORK-IND  NUMERIC                          06612
000310          MOVE IR-FN-PPO-NETWORK-IND  TO  DT-FN-PPO-NETWORK-IND   06613
000310     ELSE                                                         06614
000310          MOVE ZEROES                 TO  DT-FN-PPO-NETWORK-IND.  06615
000310     MOVE IR-FN-PPO-LINE-IND     TO  DT-FN-PPO-LINE-IND.          06616
000310     MOVE IR-FN-CLAIM-REG-ID     TO  DT-FN-CLAIM-REG-ID.          06617
000310     MOVE IR-FN-PRODUCT-CLASSIF-CD                                06618
000310                      TO  DT-FN-PRODUCT-CLASSIF-CODE.             06619
524600***************************************************************   06602
524700*    NEW FIELDS ADDED BY E.VOLYNSKY AS OF 5/2000              *   06603
524600***************************************************************   06604
000310     MOVE IR-FN-PROG-CD          TO  DT-FN-PROG-CD.               06617
           MOVE IR-FN-UWRT-CRP-CD      TO  DT-FN-UWRT-CRP-CD.
           MOVE IR-FN-LOB-CD           TO  DT-FN-LOB-CD.
           MOVE IR-FN-CLR-CD           TO  DT-FN-CLR-CD.
           MOVE IR-FN-RISK-POOL-CD     TO  DT-FN-RISK-POOL-CD.
           MOVE IR-FN-CON-TYP-CD       TO  DT-FN-CON-TYP-CD.
           MOVE IR-FN-COV-TYP-CD       TO  DT-FN-COV-TYP-CD.
           MOVE IR-FN-CON-NO           TO  DT-FN-CON-NO.
           MOVE IR-FN-PROF-BENCAT-CD   TO  DT-FN-PROF-BENCAT-CD.
           MOVE IR-FN-PROD-COMB-CD     TO  DT-FN-PROD-COMB-CD.
           MOVE IR-FN-AGENCY-CODE      TO  DT-FN-AGENCY-NUMBER.
           MOVE IR-FN-AGENT-CODE       TO  DT-FN-AGENT-CODE.
524600****ADDED BY E.VOLYNSKY  AS OF AUGUST 2000                *****   06622
           MOVE IR-FN-PPMT-START-DATE   TO DT-FN-PPMT-START-DATE.
           IF   IR-FN-PPMT-DAYS    NUMERIC
                MOVE IR-FN-PPMT-DAYS    TO DT-FN-PPMT-DAYS
           ELSE
                MOVE ZEROES             TO DT-FN-PPMT-DAYS.
           IF   IR-FN-PPMT-INTEREST-AMT NUMERIC
                MOVE IR-FN-PPMT-INTEREST-AMT TO DT-FN-PPMT-INTEREST-AMT
           ELSE
                MOVE ZEROES                  TO DT-FN-PPMT-INTEREST-AMT.

524600****ADDED BY N.DAVIDOV   AS OF AUGUST 2000                *****   06622
           MOVE WS-PREV-CLM-ST-IND        TO DT-FN-CLM-ST-IND.
524600****ADDED BY E.VOLYNSKY  AS OF OCTOBER 2000               *****   06622
           MOVE IR-FN-PKG-NO              TO DT-FN-PKG-NO.
      *****MOVE IR-FN-MCARE-BEN-CD        TO DT-FN-MCARE-BEN-CD.
           MOVE IR-FN-GRP-COUNTY-CD       TO DT-FN-GRP-COUNTY-CD.
           MOVE IR-FN-PROF-PRODUCT-CD     TO DT-FN-PROF-PRODUCT-CD.
           MOVE IR-FN-HILEVEL-PROD        TO DT-FN-HILEVEL-PROD.
           MOVE IR-FN-BOOK-BUSINESS       TO DT-FN-BOOK-BUSINESS.
********   MOVE IR-FN-PRODUCT-VAR-2BYTE   TO DT-FN-PRODUCT-VAR-2BYTE.
           MOVE IR-FN-PRODUCT-VAR-CD      TO DT-FN-PRODUCT-VAR-CD.
           MOVE IR-FN-GRP-KEY-ID          TO DT-FN-GRP-KEY-ID.
           MOVE IR-FN-FUNDING-COMB        TO DT-FN-PRODUCT-FUND-CD.
           MOVE IR-FN-RATING-COMB         TO DT-FN-RATING-COMB.
           MOVE IR-FN-PRODUCT-VARIATION-5
                                TO DT-FN-PRODUCT-VARIATION-5.
           MOVE IR-FN-RIDER-NUMBER1    TO DT-FN-RIDER-NUMBER1.
           MOVE IR-FN-RIDER-NUMBER2    TO DT-FN-RIDER-NUMBER2.
           MOVE IR-FN-RIDER-NUMBER3    TO DT-FN-RIDER-NUMBER3.
           MOVE IR-FN-RIDER-NUMBER4    TO DT-FN-RIDER-NUMBER4.
           MOVE IR-FN-RIDER-NUMBER5    TO DT-FN-RIDER-NUMBER5.
           MOVE IR-FN-RIDER-NUMBER6    TO DT-FN-RIDER-NUMBER6.
           MOVE IR-FN-RIDER-NUMBER7    TO DT-FN-RIDER-NUMBER7.
           MOVE IR-FN-RIDER-NUMBER8    TO DT-FN-RIDER-NUMBER8.
           MOVE IR-FN-RIDER-NUMBER9    TO DT-FN-RIDER-NUMBER9.
           MOVE IR-FN-DEP-NO           TO DT-FN-DEP-NO.
           MOVE IR-FN-CHECK-CTL-NO     TO DT-FN-CHECK-CTL-NO.
           MOVE IR-FN-ORIG-CLAIM-IND   TO DT-FN-ORIGINAL-CLAIM-IND.
           MOVE IR-FN-HIPAA-ALT-ID     TO DT-FN-HIPAA-ALT-ID.
524600****ADDED BY E.VOLYNSKY  AS OF JUNE 2004                  *****   06622
           MOVE IR-FN-CLM-CNT          TO DT-FN-CLM-CNT.
           MOVE IR-FN-NEW-PROV-LOC-CD  TO DT-FN-NEW-PROV-LOC-CD.
      * DONE AS PART OF CR#117642-WP MARKET SEGMENTATION
           MOVE IR-FN-WP-MKT-SEG       TO  DT-FN-WP-MKT-SEGMENT.
      *
           IF  IR-FN-SIC-CODE  NUMERIC
                MOVE IR-FN-SIC-CODE     TO DT-SIC-CODE
           ELSE
                MOVE ZEROES             TO DT-SIC-CODE.
      *
      * NPI CHANGES FOR DENTAL ARE BACKED OUT AND AS PER THE SUGGESTION
      * FROM JOHN RICUITO WE ARE MOVING SPACES TO DENTAL NPI FIELD,
      * AS THIS FIELD MAY BE REQUIRED IN FUTURE.
      * NPI CHANGES STARTS HERE.
           MOVE SPACES            TO   DT-NPI-CODE.
      * NPI CHANGES ENDS HERE.
      *
989332* CHANGES FOR DECARE EFT PROJECT STARTS HERE
989332     MOVE IR-FN-EFT-IND           TO   DT-EFT-IND.
989332     MOVE IR-FN-TRACE-NUM         TO   DT-TRACE-NBR.
989332* CHANGES FOR DECARE EFT PROJECT ENDS HERE
      * CHANGES FOR NYC HCRA REMEDIATION START
           IF  IR-FN-HCRA-SURCHARG-AMT NUMERIC
               MOVE IR-FN-HCRA-SURCHARG-AMT TO DT-FN-HCRA-SRCH-AMT
           ELSE
               MOVE ZEROES              TO DT-FN-HCRA-SRCH-AMT.
           IF  IR-FN-ONLY-PAID-AMT NUMERIC
               MOVE IR-FN-ONLY-PAID-AMT TO DT-FN-ONLY-PAID-AMT
           ELSE
               MOVE ZEROES              TO DT-FN-ONLY-PAID-AMT.
      * CHANGES FOR NYC HCRA REMEDIATION END
524400     EJECT                                                        06620
524500 D-1430-ATTACH-INDICATORS.                                        06621
524600***************************************************************   06622
524700*    PURPOSE :                                                *   06623
524800*            MOVE DESCRIPTIVE INDICATORS BASED UPON THE INPUT *   06624
524900*             FILE DESCRIPTION 88 LEVEL INDICATORS            *   06625
525000*             ( IF ANY APPLY )                                *   06626
525100***************************************************************   06627
525200     SKIP2                                                        06628
C21TSR*    IF SEL-PAID-YEAR          LESS THAN WS-90                    06629
                                                                        06630
                                                                        06631
C21TSR     CALL  C2140C02  USING SEL-PAID-YEAR C21SEL-PAID-YEAR         06632
C21TSR               C21-BASE-ON C21-WORK-AREA                          06633
C21TSR     CALL  C2140C02  USING WS-90 C21WS-90 C21-BASE-ON             06634
C21TSR               C21-WORK-AREA                                      06635
C21TSR     IF C21SEL-PAID-YEAR LESS THAN C21WS-90                       06636
525400         MOVE WS-LITERAL-A TO SEL-CLAIM-SERVICE-CATEGORY          06637
525500     ELSE                                                         06638
525600         MOVE SPACE        TO SEL-CLAIM-SERVICE-CATEGORY.         06639
525700     SKIP3                                                        06640
525800     MOVE SPACES           TO SEL-ICHIS-PRESSO-FLAG               06641
525900                              SEL-ICHIS-RIDER-FLAG                06642
526000                              SEL-ICHIS-SUPP-PAYMENT-FLAG.        06643
526100     EJECT                                                        06644
526200 D-1500-BAL-DENTAL-FILE.                                          06645
526300***************************************************************   06646
526400*    PURPOSE :                                                *   06647
526500*             COMPARE THE DENTAL CLAIMS FILE TRAILER RECORD   *   06648
526600*             TOTALS TO THE WORKING STORAGE INPUT FILE TOTALS *   06649
526700***************************************************************   06650
526800     SKIP2                                                        06651
526900     IF WS-DENTAL-TRLR-RECS        =  WS-REC-CNTS(4)              06652
527000        AND                                                       06653
527100        WS-DENTAL-TRLR-LIAB        = WS-LOB-LIAB-AMTS(4)          06654
527200     SKIP2                                                        06655
527300            PERFORM D-1599-DENTAL-FILE-BALANCED                   06656
527400     ELSE                                                         06657
527500         DISPLAY WS-DISPLAY-1                                     06658
527600         MOVE WS-ABEND-CODE(11) TO USER-ABEND-CODE                06659
527700         DISPLAY USER-ABEND-CODE                                  06660
527800         DISPLAY WS-DEN-ABEND-TBL(1)                              06661
527900         DISPLAY WS-DEN-ABEND-TBL(2)                              06662
528000         DISPLAY WS-DISPLAY-1                                     06663
528100         MOVE WS-DENTAL-TRLR-RECS        TO                       06664
528200                               WS-EDITED-DISPLY-CNTS              06665
528300         DISPLAY WS-DEN-ABEND-TBL(3) WS-EDITED-DISPLY-CNTS        06666
528400                                                                  06667
528500         MOVE WS-PLUS-ZERO TO  WS-EDITED-DISPLY-CNTS              06668
528600                                                                  06669
528700         COMPUTE WS-COMPUTE-RECORDS    = WS-REC-CNTS(4)           06670
528800         MOVE  WS-COMPUTE-RECORDS  TO  WS-EDITED-DISPLY-CNTS      06671
528900                                                                  06672
529000         DISPLAY WS-DEN-ABEND-TBL(4) WS-EDITED-DISPLY-CNTS        06673
529100                                                                  06674
529200         MOVE WS-DENTAL-TRLR-LIAB                                 06675
529300                                 TO WS-EDITED-DISPLY-AMTS         06676
529400         DISPLAY WS-DEN-ABEND-TBL(5) WS-EDITED-DISPLY-AMTS        06677
529500                                                                  06678
529600         MOVE WS-PLUS-ZERO  TO          WS-EDITED-DISPLY-AMTS     06679
529700         COMPUTE WS-COMPUTE-LOB-LIB   = WS-LOB-LIAB-AMTS(4)       06680
529800         MOVE  WS-COMPUTE-LOB-LIB  TO  WS-EDITED-DISPLY-AMTS      06681
529900                                                                  06682
530000         DISPLAY WS-DEN-ABEND-TBL(6) WS-EDITED-DISPLY-AMTS        06683
530100         DISPLAY WS-DISPLAY-1                                     06684
530200         PERFORM Z-CALL-BOMBER.                                   06685
530300     EJECT                                                        06686
530400 D-1599-DENTAL-FILE-BALANCED.                                     06687
530500***************************************************************   06688
530600*    PURPOSE :                                                *   06689
530700*             ISSUE MESSAGE THAT INPUT FILE HAS BEEN BALANCED *   06690
530800***************************************************************   06691
530900     SKIP2                                                        06692
531000     DISPLAY SPACES.                                              06693
531100     DISPLAY WS-BALANCED-FILE-MESSAGE(3).                         06694
531200     DISPLAY SPACES.                                              06695
531300     EJECT                                                        06696
562900 X-1000-END-OF-JOB-ROUTINE.                                       07033
563000***************************************************************   07034
563100*    PURPOSE :                                                *   07035
563200*             WRITES HEADER DETAIL AND TRAILER FOR BOTH THE   *   07036
563300*             FILES FROM THIS ROUTINE.                        *   07037
563400***************************************************************   07038
563500     SKIP2                                                        07039
563600     PERFORM X-2000-FORMAT-TRAILER.                               07040
563700     SKIP2                                                        07041
563800     PERFORM X-4000-WRITE-CONTROL-REPORT.                         07042
563900     EJECT                                                        07043
564000 X-2000-FORMAT-TRAILER.                                           07044
564100******************************************************************07045
564200*    PURPOSE :                                                   *07046
564300*             AT THE END OF ALL INPUT FILE PROCESSES,            *07047
564400*             WRITE THE TRAILER RECORD ON OUTPUT FILE            *07048
564500******************************************************************07049
564600     SKIP2                                                        07050
564700      MOVE HIGH-VALUES    TO  SEL-PAID-CLAIMS-RECORD.             07051
564800      MOVE HIGH-VALUES    TO  SEL-PC-TRAILER-RECORD.              07052
564900     SKIP2                                                        07053
565000      MOVE WS-TRAILER     TO  SEL-TRAILER-RECORD-INDICATOR.       07054
565100      MOVE HIGH-VALUES    TO  SEL-TRAILER-RCD-ID.                 07055
565200      MOVE CURRENT-YEAR   TO SEL-TRAILER-FILE-PAID-YEAR.          07056
565300      MOVE CURRENT-MONTH  TO SEL-TRAILER-FILE-PAID-MONTH.         07057
565400     SKIP2                                                        07058
565500      MOVE WS-REC-CNTS(16)    TO SEL-TRAILER-FILE-RECORDS.        07059
565600      MOVE WS-LOB-LIAB-AMTS(16) TO SEL-TRAILER-FILE-AMT-PAID.     07060
565700     SKIP2                                                        07061
565800      MOVE WS-REC-CNTS(6)       TO SEL-TRAILER-HOSP-RECORDS.      07062
565900      MOVE WS-LOB-LIAB-AMTS(6)  TO SEL-TRAILER-HOSP-AMT-PAID.     07063
566000     SKIP2                                                        07064
566100      MOVE WS-REC-CNTS(9)       TO SEL-TRAILER-DENT-RECORDS.      07065
566200      MOVE WS-LOB-LIAB-AMTS(9)  TO SEL-TRAILER-DENT-AMT-PAID.     07066
566300     SKIP2                                                        07067
566400      MOVE WS-REC-CNTS(10)      TO SEL-TRAILER-DRUG-RECORDS.      07068
566500      MOVE WS-LOB-LIAB-AMTS(10)  TO SEL-TRAILER-DRUG-AMT-PAID.    07069
566600     SKIP2                                                        07070
566700      MOVE WS-REC-CNTS(7)       TO SEL-TRAILER-SMED-RECORDS.      07071
566800      MOVE WS-LOB-LIAB-AMTS(7)  TO SEL-TRAILER-SMED-AMT-PAID.     07072
566900     SKIP2                                                        07073
567000      MOVE WS-REC-CNTS(8)       TO SEL-TRAILER-MMED-RECORDS.      07074
567100      MOVE WS-LOB-LIAB-AMTS(8)  TO SEL-TRAILER-MMED-AMT-PAID.     07075
567200     SKIP2                                                        07076
567300      MOVE ZEROES               TO SEL-TRAILER-MMM-RECORDS.       07077
567400      MOVE ZEROES               TO SEL-TRAILER-MMM-AMT-PAID.      07078
567500     SKIP2                                                        07079
567600      PERFORM X-3000-WRITE-RTN.                                   07080
567700     EJECT                                                        07081
567800 X-3000-WRITE-RTN.                                                07082
567900***************************************************************   07083
568000*    PURPOSE :                                                *   07084
568100*             WRITES HEADER DETAIL AND TRAILER FOR OUTPUT FILE*   07085
568200***************************************************************   07086
568300     SKIP2                                                        07087
568400     IF SEL-DETAIL-RECORD                                         07088
568500         PERFORM X-3500-VALIDATE-SEL-RECORD.                      07089
568600     SKIP2                                                        07090
568700     WRITE O-PD-CLM-RECORD      FROM                              07091
568800           SEL-PAID-CLAIMS-RECORD.                                07092
568900     SKIP2                                                        07093
569000     EJECT                                                        07094
571100     EJECT                                                        07115
571200 XA-3100-FEED-OTHER-FILE-RTN.                                     07116
571300***************************************************************   07117
571400*    PURPOSE :                                                *   07118
571500*             WRITES HEADER DETAIL AND TRAILER FOR OTHER FEED *   07119
571600***************************************************************   07120
571700     IF MS-CSS-TRAILER                                            07121
571800         MOVE WS-M-DRUG-RECORD-COUNT       TO                     07122
571900               MS-TR-RECORD-COUNT  (1).                           07123
572000     SKIP2                                                        07124
572100     IF MS-CSS-HEADER                                             07125
572200        WRITE O-MCS-FEED-CLAIM-REC      FROM                      07126
572300            MS-CT-CONTROL-CARD                                    07127
572400     ELSE                                                         07128
572500     IF MS-CSS-DATA-RECORD                                        07129
572600        WRITE O-MCS-FEED-CLAIM-REC      FROM                      07130
572700         MS-CSS-RECORD                                            07131
572800     ELSE                                                         07132
572900     IF MS-CSS-TRAILER                                            07133
573000        WRITE O-MCS-FEED-CLAIM-REC      FROM                      07134
573100            MS-TR-TRAILER-RECORD.                                 07135
573200     EJECT                                                        07136
573300 X-3500-VALIDATE-SEL-RECORD.                                      07137
573400***************************************************************   07138
573500*    PURPOSE :                                                *   07139
573600*        VALIDATES AND DEFAULTS INVALID DATA ELEMENTS         *   07140
573700***************************************************************   07141
573800     SKIP2                                                        07142
573900     MOVE WS-ZERO              TO WS-CRITICAL-ERROR-FLAG.         07143
574000     SKIP2                                                        07144
574100     IF SEL-DETAIL-CLAIM-NUMBER  EQUAL SPACES                     07145
574200        IF SEL-HOSPITAL   AND  LS-HOSPITAL-LINE-LOB               07146
574300              MOVE HRCP-CASE-NO   TO SEL-DETAIL-CLAIM-NUMBER      07147
574400        ELSE                                                      07148
574200        IF SEL-HOSPITAL   AND  LS-CHARGE-HOSPITAL-LOB             07149
574300              MOVE HRCR-CASE-NO   TO SEL-DETAIL-CLAIM-NUMBER      07150
574400        ELSE                                                      07151
574200        IF SEL-HOSPITAL   AND  LS-REJECT-HOSPITAL-LOB             07152
171600           MOVE R105-CORE-SYS-CLAIM-NO TO SEL-DETAIL-CLAIM-NUMBER 07153
574400        ELSE                                                      07154
574500              MOVE WS-FOURTEEN-ZEROS TO SEL-DETAIL-CLAIM-NUMBER   07155
574600              ADD WS-POSITIVE-ONE        TO WS-DEF-CLAIM-CNT      07156
                    DISPLAY 'CLAIM NUMBER   ' SEL-DETAIL-CLAIM-NUMBER   07157
574700              MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG    07158
574800     ELSE                                                         07159
574900         CONTINUE.                                                07160
575000     SKIP2                                                        07161
575100     IF SEL-SUBSCRIBER-ID EQUAL SPACES                            07162
575200        ADD WS-POSITIVE-ONE        TO WS-DEF-SUBID-CNT            07163
575300        MOVE WS-FOURTEEN-ZEROS TO SEL-SUBSCRIBER-ID               07164
              DISPLAY 'SUB ID         ' SEL-SUBSCRIBER-ID               07165
575400        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.         07166
575500     SKIP2                                                        07167
575600     IF SEL-DETAIL-PAID-DAY NOT NUMERIC                           07168
575700        ADD WS-POSITIVE-ONE        TO WS-DEF-PDDAY-CNT            07169
575800        MOVE WS-LITERAL-01     TO SEL-DETAIL-PAID-DAY             07170
              DISPLAY 'PAID DAY       ' SEL-DETAIL-PAID-DAY             07171
575900        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.         07172
576000     SKIP2                                                        07173
576100     IF SEL-DETAIL-INCURRED-DAY NOT NUMERIC                       07174
576200        ADD WS-POSITIVE-ONE        TO WS-DEF-INCDA-CNT            07175
576300        MOVE WS-LITERAL-01  TO SEL-DETAIL-INCURRED-DAY            07176
              DISPLAY 'INC  DAY       ' SEL-DETAIL-INCURRED-DAY         07177
576400        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.         07178
576500     SKIP2                                                        07179
576600     IF SEL-INCURRED-YEAR NOT NUMERIC                             07180
576700        ADD WS-POSITIVE-ONE        TO WS-DEF-INCYR-CNT            07181
576800        MOVE SEL-PAID-YEAR     TO SEL-INCURRED-YEAR               07182
              DISPLAY 'INC  YEAR      ' SEL-INCURRED-YEAR               07183
576900        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.         07184
577000     SKIP2                                                        07185
577100     IF SEL-INCURRED-MONTH NOT NUMERIC                            07186
577200        ADD WS-POSITIVE-ONE        TO WS-DEF-INCMO-CNT            07187
577300        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG          07188
              DISPLAY 'INC  MONTH     ' SEL-INCURRED-MONTH              07189
577400        MOVE SEL-PAID-MONTH    TO  SEL-INCURRED-MONTH.            07190
577500     SKIP2                                                        07191
578000     IF NOT SEL-VALID-LINES-OF-BUSINESS                           07192
578100        ADD WS-POSITIVE-ONE        TO WS-DEF-LOBNO-CNT            07193
              DISPLAY 'L  O B         ' SEL-LINE-OF-BUSINESS            07194
578200        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.         07195
578300     SKIP2                                                        07196
578400     IF NOT SEL-DETAIL-MED-SURG-NO                                07197
578500        AND NOT SEL-DETAIL-MED-SURG-FROM-MCS                      07198
578600        ADD WS-POSITIVE-ONE        TO WS-DEF-MSBRK-CNT            07199
578700        MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.         07200
578800     SKIP2                                                        07201
578900     PERFORM X-3550-LOGICAL-DATE-CHECK.                           07202
579000     SKIP2                                                        07203
579100     IF LS-ABEND-ON-CRITICAL-ERROR                                07204
579200        IF WS-CRITICAL-ERROR-ON-OUTPUT                            07205
579300             PERFORM X-3510-ABEND-ROUTINE.                        07206
579400     SKIP2                                                        07207
579500     MOVE WS-ZERO           TO WS-CRITICAL-ERROR-FLAG.            07208
579600     EJECT                                                        07209
579700 X-3510-ABEND-ROUTINE.                                            07210
579800************************************************************      07211
579900*  PURPOSE:                                                *      07212
580000*         TERMINATE EXECUTION IMMEDIATELY DUE TO CRITICAL  *      07213
580100*        OUTPUT ERROR - INVALID FIELDS ON FORMATTED RECORD *      07214
580200************************************************************      07215
580300     SKIP1                                                        07216
580400         MOVE WS-ABEND-CODE(18) TO USER-ABEND-CODE                07217
580500         DISPLAY USER-ABEND-CODE                                  07218
580600         DISPLAY WS-CRITICAL-ERROR-MESSAGE-3                      07219
580700         DISPLAY WS-CRITICAL-ERROR-MESSAGE-4                      07220
580800         PERFORM Z-CALL-BOMBER.                                   07221
580900     EJECT                                                        07222
580900     EJECT                                                        07223
581000 X-3550-LOGICAL-DATE-CHECK.                                       07224
581100******************************************************************07225
581200*    PURPOSE :                                                   *07226
581300*             CHECK TO ENSURE THAT THE PAID DATES DO NOT PRECEDE *07227
581400*             THE DATE THE CLAIM LIABILITY WAS INCURRED          *07228
581500******************************************************************07229
581600     SKIP2                                                        07230
581700     MOVE ZEROS                 TO WS-COMPARE-PAID-DATE           07231
581800                                   WS-COMPARE-INCURRED-DATE.      07232
581900     SKIP2                                                        07233
582000     MOVE SEL-PAID-YEAR         TO WS-COMPARE-P-YEAR              07234
582100     MOVE SEL-PAID-MONTH        TO WS-COMPARE-P-MONTH             07235
582200     MOVE SEL-DETAIL-PAID-DAY   TO WS-COMPARE-P-DAY.              07236
582300     SKIP2                                                        07237
582400     MOVE SEL-INCURRED-YEAR       TO WS-COMPARE-I-YEAR            07238
582500     MOVE SEL-INCURRED-MONTH      TO WS-COMPARE-I-MONTH           07239
582600     MOVE SEL-DETAIL-INCURRED-DAY TO WS-COMPARE-I-DAY.            07240
582700     SKIP2                                                        07241
C21TSR*    IF WS-COMPARE-PAID-DATE  <  WS-COMPARE-INCURRED-DATE         07242
                                                                        07243
                                                                        07244
C21TSR     CALL  C2110C06  USING WS-COMPARE-PAID-DATE                   07245
C21TSR               C21WS-COMPARE-PAID-DATE C21-BASE-ON C21-WORK-AREA  07246
C21TSR     CALL  C2110C06  USING WS-COMPARE-INCURRED-DATE               07247
C21TSR               C21WS-COMPARE-INCURRED-DATE C21-BASE-ON            07248
C21TSR               C21-WORK-AREA                                      07249
C21TSR     IF C21WS-COMPARE-PAID-DATE < C21WS-COMPARE-INCURRED-DATE     07250
582900        ADD WS-POSITIVE-ONE          TO WS-LOG-ERR-PDDATE-CNT     07251
582400        MOVE  WS-COMPARE-P-YEAR      TO SEL-INCURRED-YEAR         07252
582500        MOVE  WS-COMPARE-P-MONTH     TO SEL-INCURRED-MONTH        07253
582600        MOVE  WS-COMPARE-P-DAY       TO SEL-DETAIL-INCURRED-DAY.  07254
583100     SKIP2                                                        07255
583200     IF WS-COMPARE-P-DAY        LESS THAN WS-01                   07256
583300        OR WS-COMPARE-P-DAY    GREATER THAN WS-31                 07257
583400            ADD WS-POSITIVE-ONE      TO WS-LOG-ERR-PAYDAY-CNT     07258
583500            MOVE WS-LITERAL-01 TO SEL-DETAIL-PAID-DAY             07259
583600            MOVE WS-ONE        TO WS-CRITICAL-ERROR-FLAG.         07260
583700     SKIP2                                                        07261
583800     IF WS-COMPARE-I-DAY        LESS THAN WS-01                   07262
583900        OR WS-COMPARE-I-DAY    GREATER THAN WS-31                 07263
584000            ADD WS-POSITIVE-ONE      TO WS-LOG-ERR-INCDAY-CNT     07264
584100            MOVE WS-LITERAL-01 TO SEL-DETAIL-INCURRED-DAY         07265
584200            MOVE WS-ONE        TO WS-CRITICAL-ERROR-FLAG.         07266
584300     SKIP2                                                        07267
584400     IF WS-COMPARE-P-MONTH      LESS THAN WS-01                   07268
584500        OR WS-COMPARE-P-MONTH  GREATER THAN WS-12                 07269
584600            ADD WS-POSITIVE-ONE      TO WS-LOG-ERR-PAYMON-CNT     07270
584700            MOVE WS-MONTH      TO SEL-PAID-MONTH                  07271
584800            MOVE WS-ONE        TO WS-CRITICAL-ERROR-FLAG.         07272
584900     SKIP2                                                        07273
585000     IF WS-COMPARE-I-MONTH      LESS THAN WS-01                   07274
585100        OR WS-COMPARE-I-MONTH  GREATER THAN WS-12                 07275
585200            ADD WS-POSITIVE-ONE      TO WS-LOG-ERR-INCMON-CNT     07276
585300            MOVE WS-MONTH      TO SEL-INCURRED-MONTH              07277
585400            MOVE WS-ONE        TO WS-CRITICAL-ERROR-FLAG.         07278
582200***************************************************************** 07279
582300**** THIS IS A FIX FOR INCURRED DATE = ZEROES     9/21/94         07280
582400**** ADDED BY D.GANOR   WHEN INCURRED DATE = ZEROES               07281
582500****                    DEFAULT TO CURRENT YEAR.                  07282
582600                                                                  07283
582700     SKIP2                                                        07284
582800     IF WS-COMPARE-I-YEAR  = ZERO AND                             07285
582900        WS-COMPARE-I-MONTH = ZERO AND                             07286
583000        WS-COMPARE-I-DAY   = ZERO                                 07287
583100            ADD WS-POSITIVE-ONE    TO WS-LOG-ERR-INCMON-CNT       07288
583200            MOVE CURRENT-YEAR      TO SEL-INCURRED-YEAR           07289
583300            MOVE WS-ONE            TO WS-CRITICAL-ERROR-FLAG.     07290
583400                                                                  07291
583500***************************************************************** 07292
585500     EJECT                                                        07293
585600 X-4000-WRITE-CONTROL-REPORT.                                     07294
585700******************************************************************07295
585800*    PURPOSE :                                                   *07296
585900*             PRINT CONTROL REPORT HEADING.                      *07297
586000*             MOVES ACCUMULATED STORAGE AREA TO PRINT AREA       *07298
586100*             AND PRINTS SETS OF LINES.                          *07299
586200******************************************************************07300
586300                                                                  07301
586400     MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                      07302
586500     WRITE  O-PD-CLM-CONTROL-REPORT FROM CNTRL-RPT-HEADING-1      07303
586600            AFTER ADVANCING TO-NEW-PAGE.                          07304
586700                                                                  07305
586800     MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                      07306
586900     WRITE  O-PD-CLM-CONTROL-REPORT FROM CNTRL-RPT-HEADING-2      07307
587000            AFTER ADVANCING 2.                                    07308
587100                                                                  07309
587200     MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                      07310
587300     WRITE  O-PD-CLM-CONTROL-REPORT FROM CNTRL-RPT-HEADING-3      07311
587400            AFTER ADVANCING 2.                                    07312
587500                                                                  07313
587600     MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                      07314
587700     WRITE  O-PD-CLM-CONTROL-REPORT FROM CNTRL-RPT-HEADING-4      07315
587800            AFTER ADVANCING 3.                                    07316
587900                                                                  07317
588000     MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                      07318
588100     WRITE  O-PD-CLM-CONTROL-REPORT FROM CNTRL-RPT-HEADING-5      07319
588200            AFTER ADVANCING 1.                                    07320
588300                                                                  07321
588400     MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                      07322
588500     WRITE  O-PD-CLM-CONTROL-REPORT FROM CNTRL-RPT-HEADING-6      07323
588600            AFTER ADVANCING 1.                                    07324
588700                                                                  07325
588800     IF LS-HOSPITAL-LINE-LOB                                      07326
588900         PERFORM X-4010-HOSPITAL-PORTION                          07327
589000     ELSE                                                         07328
122200     IF  LS-CHARGE-HOSPITAL-LOB                                   07329
588900         PERFORM X-4010-HOSPITAL-PORTION                          07330
589000     ELSE                                                         07331
122200     IF  LS-REJECT-HOSPITAL-LOB                                   07332
588900         PERFORM X-4010-HOSPITAL-PORTION                          07333
589000     ELSE                                                         07334
589100     IF LS-DENTAL-LINE-LOB                                        07335
589200         PERFORM X-4020-DENTAL-PORTION                            07336
589300     ELSE                                                         07337
589400     IF LS-DRUG-LINE-LOB                                          07338
589500         PERFORM X-4030-DRUG-PORTION                              07339
589600     ELSE                                                         07340
589700     IF LS-MEDICAL-LINE-LOB                                       07341
589800         PERFORM X-4040-SURG-MAJ-MED-PORTION.                     07342
589900     SKIP2                                                        07343
590000     PERFORM X-4050-OUTPUT-TOTAL-PORTION.                         07344
590100     EJECT                                                        07345
590200 X-4010-HOSPITAL-PORTION.                                         07346
590300***************************************************************   07347
590400******       CONTROL REPORT FOR HOSPITAL               ********   07348
590500***************************************************************   07349
590600      MOVE WS-TOTAL-INPUT        TO    WS-PRT-DETAIL-LOB.         07350
590700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07351
590800            AFTER ADVANCING 2.                                    07352
590900      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07353
591000      MOVE SPACES TO WS-CONTROL-REPORT.                           07354
591100                                                                  07355
591200      MOVE WS-HOSPITAL           TO    WS-PRT-DETAIL-LOB.         07356
591300      MOVE WS-REC-CNTS(1)        TO    WS-PRT-TOTAL-RECORDS.      07357
591400      MOVE WS-LOB-LIAB-AMTS(1)   TO    WS-PRT-LOB-LIAB.           07358
591500      MOVE WS-CLAIMS-CNTS(1)     TO    WS-PRT-CLAIMS.             07359
591600      MOVE WS-DAYS-CNTS(1)       TO    WS-PRT-DAYS-VISIT.         07360
591700      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-SERVICES.         07361
591800      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07362
591900            AFTER ADVANCING 1.                                    07363
592000      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07364
592100      MOVE SPACES TO WS-CONTROL-REPORT.                           07365
592200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07366
592300                                                                  07367
592400***************************************************************   07368
592500      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07369
592600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07370
592700                                                                  07371
592800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07372
592900      MOVE SPACES TO WS-CONTROL-REPORT.                           07373
593000                                                                  07374
593100      MOVE WS-SELECTED           TO    WS-PRT-DETAIL-LOB.         07375
593200      MOVE WS-REC-CNTS(6)        TO    WS-PRT-TOTAL-RECORDS.      07376
593300      MOVE WS-LOB-LIAB-AMTS(6)   TO    WS-PRT-LOB-LIAB.           07377
593400      MOVE WS-CLAIMS-CNTS(6)     TO    WS-PRT-CLAIMS.             07378
593500      MOVE WS-DAYS-CNTS(6)       TO    WS-PRT-DAYS-VISIT.         07379
593600      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-SERVICES.         07380
593700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07381
593800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07382
593900      MOVE SPACES TO WS-CONTROL-REPORT.                           07383
594000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07384
594100                                                                  07385
594200***************************************************************   07386
594300      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07387
594400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07388
594500            AFTER ADVANCING 2 LINES.                              07389
594600      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07390
594700      MOVE SPACES TO WS-CONTROL-REPORT.                           07391
594800                                                                  07392
594900      MOVE WS-INCLUDE-LIT   TO    WS-PRT-DETAIL-LOB.              07393
595000      MOVE WS-REC-CNTS(11)        TO    WS-PRT-TOTAL-RECORDS.     07394
595100      MOVE WS-LOB-LIAB-AMTS(11)   TO    WS-PRT-LOB-LIAB.          07395
595200      MOVE WS-CLAIMS-CNTS(11)     TO    WS-PRT-CLAIMS.            07396
595300      MOVE WS-DAYS-CNTS(11)       TO    WS-PRT-DAYS-VISIT.        07397
595400      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-SERVICES.         07398
595500      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07399
595600            AFTER ADVANCING 1 LINE.                               07400
595700      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07401
595800      MOVE SPACES TO WS-CONTROL-REPORT.                           07402
595900***************************************************************   07403
592500      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07404
594400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07405
594500            AFTER ADVANCING 2 LINES.                              07406
592700                                                                  07407
596200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07408
596300      MOVE SPACES TO WS-CONTROL-REPORT.                           07409
596400                                                                  07410
593100      MOVE WS-SUPPLEMENT         TO    WS-PRT-DETAIL-LOB.         07411
192500      MOVE WS-ICHIS-SUPP-RECORDS TO    WS-PRT-TOTAL-RECORDS.      07412
172000      MOVE WS-ICHIS-SUPP-TOTALS  TO    WS-PRT-LOB-LIAB.           07413
593500      MOVE WS-NOT-APPLICABLE     TO    WS-PRT-DAYS-VISIT.         07414
593600      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-SERVICES.         07415
593700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07416
593800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07417
593900      MOVE SPACES TO WS-CONTROL-REPORT.                           07418
594000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07419
595900***************************************************************   07420
592500      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07421
594400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07422
594500            AFTER ADVANCING 2 LINES.                              07423
592700                                                                  07424
596200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07425
596300      MOVE SPACES TO WS-CONTROL-REPORT.                           07426
596400                                                                  07427
593100      MOVE WS-GME                TO    WS-PRT-DETAIL-LOB.         07428
192500      MOVE WS-GME-RECORDS-OUT    TO    WS-PRT-TOTAL-RECORDS.      07429
172000      MOVE WS-GME-LIABILITY-OUT  TO    WS-PRT-LOB-LIAB.           07430
593500      MOVE WS-NOT-APPLICABLE     TO    WS-PRT-DAYS-VISIT.         07431
593600      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-SERVICES.         07432
593700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07433
593800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07434
593900      MOVE SPACES TO WS-CONTROL-REPORT.                           07435
594000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07436
595900***************************************************************   07437
596000      MOVE SPACES                TO    WS-PRT-DETAIL-LOB.         07438
596100      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07439
596200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07440
596300      MOVE SPACES TO WS-CONTROL-REPORT.                           07441
596400                                                                  07442
596500      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07443
596600      MOVE WS-SUB-TOTAL-A        TO    WS-PRT-DETAIL-LOB.         07444
596700      MOVE  WS-REC-CNTS(16)       TO    WS-PRT-TOTAL-RECORDS.     07445
596800                                                                  07446
596900      MOVE  WS-LOB-LIAB-AMTS(16)  TO    WS-PRT-LOB-LIAB.          07447
597000                                                                  07448
597100      MOVE  WS-SERVICES-CNTS(16)  TO  WS-PRT-SERVICES.            07449
597200                                                                  07450
597300      MOVE  WS-CLAIMS-CNTS(16)    TO  WS-PRT-CLAIMS.              07451
597400                                                                  07452
597500      MOVE  WS-DAYS-CNTS(16)      TO  WS-PRT-DAYS-VISIT.          07453
597600                                                                  07454
597700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07455
597800            AFTER ADVANCING 1 LINE.                               07456
597900      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07457
598000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07458
598100      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07459
598200      MOVE SPACES TO WS-CONTROL-REPORT.                           07460
598300      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07461
598400      SKIP3                                                       07462
598500     EJECT                                                        07463
598600 X-4020-DENTAL-PORTION.                                           07464
598700***************************************************************   07465
598800******       CONTROL REPORT FOR DENTAL                 ********   07466
598900***************************************************************   07467
599000      MOVE WS-TOTAL-INPUT        TO    WS-PRT-DETAIL-LOB.         07468
599100      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07469
599200            AFTER ADVANCING 2.                                    07470
599300      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07471
599400      MOVE SPACES TO WS-CONTROL-REPORT.                           07472
599500                                                                  07473
599600      MOVE WS-DENTAL             TO    WS-PRT-DETAIL-LOB.         07474
599700      MOVE WS-REC-CNTS(4)        TO    WS-PRT-TOTAL-RECORDS.      07475
599800      MOVE WS-LOB-LIAB-AMTS(4)   TO    WS-PRT-LOB-LIAB.           07476
599900      MOVE WS-SERVICES-CNTS(4)   TO    WS-PRT-SERVICES.           07477
600000      MOVE WS-CLAIMS-CNTS(4)     TO    WS-PRT-CLAIMS.             07478
600100      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-DAYS-VISIT.       07479
600200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07480
600300            AFTER ADVANCING 1.                                    07481
600400      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07482
600500      MOVE SPACES TO WS-CONTROL-REPORT.                           07483
600600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07484
600700                                                                  07485
600800***************************************************************   07486
600900      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07487
601000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07488
601100                                                                  07489
601200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07490
601300      MOVE SPACES TO WS-CONTROL-REPORT.                           07491
601400                                                                  07492
601500      MOVE WS-SELECTED           TO    WS-PRT-DETAIL-LOB.         07493
601600      MOVE WS-REC-CNTS(9)        TO    WS-PRT-TOTAL-RECORDS.      07494
601700      MOVE WS-LOB-LIAB-AMTS(9)   TO    WS-PRT-LOB-LIAB.           07495
601800      MOVE WS-SERVICES-CNTS(9)   TO    WS-PRT-SERVICES.           07496
601900      MOVE WS-CLAIMS-CNTS(9)     TO    WS-PRT-CLAIMS.             07497
602000      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-DAYS-VISIT.       07498
602100      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07499
602200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07500
602300      MOVE SPACES TO WS-CONTROL-REPORT.                           07501
602400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07502
602500                                                                  07503
602600***************************************************************   07504
602700      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07505
602800      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07506
602900            AFTER ADVANCING 2 LINES.                              07507
603000      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07508
603100      MOVE SPACES TO WS-CONTROL-REPORT.                           07509
603200                                                                  07510
603300      MOVE WS-INCLUDE-LIT   TO    WS-PRT-DETAIL-LOB.              07511
603400      MOVE WS-REC-CNTS(14)        TO    WS-PRT-TOTAL-RECORDS.     07512
603500      MOVE WS-LOB-LIAB-AMTS(14)   TO    WS-PRT-LOB-LIAB.          07513
603600      MOVE WS-SERVICES-CNTS(14)   TO    WS-PRT-SERVICES.          07514
603700      MOVE WS-CLAIMS-CNTS(14)     TO    WS-PRT-CLAIMS.            07515
603800      MOVE WS-NOT-APPLICABLE      TO    WS-PRINT-DAYS-VISIT.      07516
603900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07517
604000            AFTER ADVANCING 1 LINE.                               07518
604100      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07519
604200      MOVE SPACES TO WS-CONTROL-REPORT.                           07520
604300***************************************************************   07521
604400      MOVE SPACES                TO    WS-PRT-DETAIL-LOB.         07522
604500      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07523
604600      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07524
604700      MOVE SPACES TO WS-CONTROL-REPORT.                           07525
604800      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07526
604900      MOVE WS-SUB-TOTAL-B        TO    WS-PRT-DETAIL-LOB.         07527
605000      MOVE  WS-REC-CNTS(16)       TO    WS-PRT-TOTAL-RECORDS.     07528
605100      MOVE  WS-LOB-LIAB-AMTS(16)  TO    WS-PRT-LOB-LIAB.          07529
605200      MOVE  WS-SERVICES-CNTS(16)  TO  WS-PRT-SERVICES.            07530
605300      MOVE  WS-CLAIMS-CNTS(16)    TO  WS-PRT-CLAIMS.              07531
605400      MOVE WS-NOT-APPLICABLE      TO    WS-PRINT-DAYS-VISIT.      07532
605500      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07533
605600            AFTER ADVANCING 1 LINE.                               07534
605700      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07535
605800      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07536
605900      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07537
606000      MOVE SPACES TO WS-CONTROL-REPORT.                           07538
606100      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07539
606200      SKIP3                                                       07540
606300     EJECT                                                        07541
606400 X-4030-DRUG-PORTION.                                             07542
606500***************************************************************   07543
606600******       CONTROL REPORT FOR DRUG                   ********   07544
606700***************************************************************   07545
606800      MOVE WS-TOTAL-INPUT        TO    WS-PRT-DETAIL-LOB.         07546
606900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07547
607000            AFTER ADVANCING 2.                                    07548
607100      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07549
607200      MOVE SPACES TO WS-CONTROL-REPORT.                           07550
607300                                                                  07551
607400      MOVE WS-DRUG               TO    WS-PRT-DETAIL-LOB.         07552
607500      MOVE WS-REC-CNTS(5)        TO    WS-PRT-TOTAL-RECORDS.      07553
607600      MOVE WS-LOB-LIAB-AMTS(5)   TO    WS-PRT-LOB-LIAB.           07554
607700      MOVE WS-SERVICES-CNTS(5)   TO    WS-PRT-SERVICES.           07555
607800      MOVE WS-CLAIMS-CNTS(5)     TO    WS-PRT-CLAIMS.             07556
607900      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-DAYS-VISIT.       07557
608000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07558
608100            AFTER ADVANCING 1.                                    07559
608200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07560
608300      MOVE SPACES TO WS-CONTROL-REPORT.                           07561
608400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07562
608500                                                                  07563
608600***************************************************************   07564
608700      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07565
608800      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07566
608900                                                                  07567
609000      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07568
609100      MOVE SPACES TO WS-CONTROL-REPORT.                           07569
609200                                                                  07570
609300      MOVE WS-SELECTED           TO    WS-PRT-DETAIL-LOB.         07571
609400      MOVE WS-REC-CNTS(10)        TO    WS-PRT-TOTAL-RECORDS.     07572
609500      MOVE WS-LOB-LIAB-AMTS(10)   TO    WS-PRT-LOB-LIAB.          07573
609600      MOVE WS-SERVICES-CNTS(10)   TO    WS-PRT-SERVICES.          07574
609700      MOVE WS-CLAIMS-CNTS(10)     TO    WS-PRT-CLAIMS.            07575
609800      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-DAYS-VISIT.       07576
609900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07577
610000      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07578
610100      MOVE SPACES TO WS-CONTROL-REPORT.                           07579
610200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07580
610300                                                                  07581
610400***************************************************************   07582
610500      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07583
610600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07584
610700            AFTER ADVANCING 2 LINES.                              07585
610800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07586
610900      MOVE SPACES TO WS-CONTROL-REPORT.                           07587
611000                                                                  07588
611100      MOVE WS-INCLUDE-LIT       TO    WS-PRT-DETAIL-LOB.          07589
611200      MOVE WS-REC-CNTS(15)     TO    WS-PRT-TOTAL-RECORDS.        07590
611300      MOVE WS-LOB-LIAB-AMTS(15)   TO    WS-PRT-LOB-LIAB.          07591
611400      MOVE WS-SERVICES-CNTS(15)   TO    WS-PRT-SERVICES.          07592
611500      MOVE WS-CLAIMS-CNTS(15)  TO    WS-PRT-CLAIMS.               07593
611600      MOVE WS-NOT-APPLICABLE   TO    WS-PRINT-DAYS-VISIT.         07594
611700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07595
611800            AFTER ADVANCING 1 LINE.                               07596
611900      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07597
612000      MOVE SPACES TO WS-CONTROL-REPORT.                           07598
612100***************************************************************   07599
612200      MOVE SPACES                TO    WS-PRT-DETAIL-LOB.         07600
612300      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07601
612400      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07602
612500      MOVE SPACES TO WS-CONTROL-REPORT.                           07603
612600                                                                  07604
612700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07605
612800      MOVE WS-SUB-TOTAL-C        TO    WS-PRT-DETAIL-LOB.         07606
612900      MOVE  WS-REC-CNTS(16)       TO    WS-PRT-TOTAL-RECORDS.     07607
613000                                                                  07608
613100      MOVE  WS-LOB-LIAB-AMTS(16)  TO    WS-PRT-LOB-LIAB.          07609
613200                                                                  07610
613300      MOVE  WS-SERVICES-CNTS(16)  TO  WS-PRT-SERVICES.            07611
613400                                                                  07612
613500      MOVE  WS-CLAIMS-CNTS(16)    TO  WS-PRT-CLAIMS.              07613
613600                                                                  07614
613700      MOVE WS-NOT-APPLICABLE      TO    WS-PRINT-DAYS-VISIT.      07615
613800                                                                  07616
613900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07617
614000            AFTER ADVANCING 1 LINE.                               07618
614100      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07619
614200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07620
614300      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07621
614400      MOVE SPACES TO WS-CONTROL-REPORT.                           07622
614500      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07623
614600                                                                  07624
614700     EJECT                                                        07625
614800 X-4040-SURG-MAJ-MED-PORTION.                                     07626
614900***************************************************************   07627
615000******       CONTROL REPORT FOR BASIC                  ********   07628
615100***************************************************************   07629
615200      SKIP2                                                       07630
615300      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07631
615400      MOVE SPACES TO WS-CONTROL-REPORT.                           07632
615500      MOVE WS-TOTAL-INPUT        TO    WS-PRT-DETAIL-LOB.         07633
615600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07634
615700      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07635
615800      MOVE SPACES TO WS-CONTROL-REPORT.                           07636
615900                                                                  07637
616000      MOVE WS-PROFESSIONAL       TO    WS-PRT-DETAIL-LOB.         07638
616100      MOVE WS-REC-CNTS(2)        TO    WS-PRT-TOTAL-RECORDS.      07639
616200                                                                  07640
616300      COMPUTE WS-COMPUTE-LOB-LIB  =  WS-LOB-LIAB-AMTS(2)          07641
616400                                  +  WS-LOB-LIAB-AMTS(3).         07642
616500      COMPUTE WS-COMPUTE-SERVICES = WS-SERVICES-CNTS(2)           07643
616600                                  + WS-SERVICES-CNTS(3).          07644
616700      MOVE  WS-COMPUTE-LOB-LIB  TO  WS-PRT-LOB-LIAB.              07645
616800      MOVE  WS-COMPUTE-SERVICES TO  WS-PRT-SERVICES.              07646
616900                                                                  07647
617000      MOVE WS-NOT-APPLICABLE TO  WS-PRINT-CLAIMS                  07648
617100                                 WS-PRINT-DAYS-VISIT.             07649
617200                                                                  07650
617300      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07651
617400            AFTER ADVANCING 1.                                    07652
617500      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07653
617600      MOVE SPACES TO WS-CONTROL-REPORT.                           07654
617700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07655
617800                                                                  07656
617900      MOVE WS-TOTAL              TO    WS-PRT-DETAIL-LOB.         07657
618000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07658
618100            AFTER ADVANCING 2 LINES.                              07659
618200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07660
618300      MOVE SPACES TO WS-CONTROL-REPORT.                           07661
618400      SKIP2                                                       07662
618500      MOVE WS-INCLUDE-LIT   TO    WS-PRT-DETAIL-LOB.              07663
618600      SKIP2                                                       07664
618700      COMPUTE  WS-COMPUTE-RECORDS   = WS-REC-CNTS(12)             07665
618800                                    + WS-REC-CNTS(13).            07666
618900      COMPUTE WS-COMPUTE-LOB-LIB    = WS-LOB-LIAB-AMTS(12)        07667
619000                                    + WS-LOB-LIAB-AMTS(13).       07668
619100      COMPUTE WS-COMPUTE-SERVICES   = WS-SERVICES-CNTS(12)        07669
619200                                    + WS-SERVICES-CNTS(13).       07670
619300      MOVE  WS-COMPUTE-RECORDS  TO  WS-PRT-TOTAL-RECORDS.         07671
619400      MOVE  WS-COMPUTE-LOB-LIB  TO  WS-PRT-LOB-LIAB.              07672
619500      MOVE  WS-COMPUTE-SERVICES TO  WS-PRT-SERVICES.              07673
619600                                                                  07674
619700      MOVE WS-NOT-APPLICABLE TO   WS-PRINT-CLAIMS                 07675
619800                                 WS-PRINT-DAYS-VISIT.             07676
619900                                                                  07677
620000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07678
620100            AFTER ADVANCING 1 LINE.                               07679
620200      MOVE SPACES TO WS-CONTROL-REPORT.                           07680
620300      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07681
620400      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT                      07682
620500                           WS-CONTROL-REPORT.                     07683
620600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07684
620700                                                                  07685
620800      SKIP2                                                       07686
621000      MOVE WS-REJECT-LIT   TO    WS-PRT-DETAIL-LOB.               07687
621100      SKIP2                                                       07688
621200      COMPUTE  WS-REJECT-TOTALS   =  WS-REJECT-RIDER-AMT          07689
621300                                  +  WS-REJECT-MAJOR-MED-AMT      07690
621400                                  +  WS-REJECT-BASIC-AMT.         07691
621500      MOVE  WS-REJECT-CNTS      TO  WS-PRT-TOTAL-RECORDS.         07692
621600                                                                  07693
621700      MOVE  WS-REJECT-TOTALS    TO  WS-PRT-LOB-LIAB.              07694
621800                                                                  07695
621900      MOVE WS-NOT-APPLICABLE TO  WS-PRINT-CLAIMS                  07696
622000                                 WS-PRINT-SERVICES                07697
622100                                 WS-PRINT-DAYS-VISIT.             07698
622200                                                                  07699
622300      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07700
622400            AFTER ADVANCING 1 LINE.                               07701
622500      MOVE SPACES TO WS-CONTROL-REPORT.                           07702
622600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07703
622700      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT                      07704
622800                           WS-CONTROL-REPORT.                     07705
622900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07706
623000                                                                  07707
623200      MOVE WS-M-DRUG-LIT   TO    WS-PRT-DETAIL-LOB.               07708
623300      COMPUTE  WS-COMPUTE-RECORDS   = WS-REC-CNTS(17)             07709
623400                                    + WS-REC-CNTS(18).            07710
623500      COMPUTE WS-COMPUTE-LOB-LIB    = WS-LOB-LIAB-AMTS(17)        07711
623600                                    + WS-LOB-LIAB-AMTS(18).       07712
623700      COMPUTE WS-COMPUTE-SERVICES   = WS-SERVICES-CNTS(17)        07713
623800                                    + WS-SERVICES-CNTS(18).       07714
623900      MOVE  WS-COMPUTE-RECORDS  TO  WS-PRT-TOTAL-RECORDS.         07715
624000      MOVE  WS-COMPUTE-LOB-LIB  TO  WS-PRT-LOB-LIAB.              07716
624100      MOVE  WS-COMPUTE-SERVICES TO  WS-PRT-SERVICES.              07717
624200                                                                  07718
624300      MOVE WS-NOT-APPLICABLE TO   WS-PRINT-CLAIMS                 07719
624400                                 WS-PRINT-DAYS-VISIT.             07720
624500                                                                  07721
624600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT       07722
624700            AFTER ADVANCING 1 LINE.                               07723
624800      MOVE SPACES TO WS-CONTROL-REPORT.                           07724
624900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07725
625000      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT                      07726
625100                           WS-CONTROL-REPORT.                     07727
625200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07728
625300                                                                  07729
625400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-OUT-PUT-SECTION.     07730
625500      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07731
625600      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07732
625700                                                                  07733
625800      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07734
625900                                                                  07735
626000      MOVE WS-BASIC-MED          TO    WS-PRT-DETAIL-LOB.         07736
626100      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07737
626200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07738
626300      MOVE SPACES TO WS-CONTROL-REPORT.                           07739
626400                                                                  07740
626500      MOVE WS-GENERATED          TO    WS-PRT-DETAIL-LOB.         07741
626600      MOVE WS-REC-CNTS(7)        TO    WS-PRT-TOTAL-RECORDS.      07742
626700      MOVE WS-LOB-LIAB-AMTS(7)   TO    WS-PRT-LOB-LIAB.           07743
626800      MOVE WS-SERVICES-CNTS(7)   TO    WS-PRT-SERVICES.           07744
626900      MOVE WS-CLAIMS-CNTS(7)     TO    WS-PRT-CLAIMS.             07745
627000      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-DAYS-VISIT.       07746
627100      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07747
627200      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07748
627300      MOVE SPACES TO WS-CONTROL-REPORT.                           07749
627400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07750
627500     EJECT                                                        07751
627600***************************************************************   07752
627700******       CONTROL REPORT FOR MAJOR MEDICAL          ********   07753
627800***************************************************************   07754
627900      MOVE WS-MAJ-MED            TO    WS-PRT-DETAIL-LOB.         07755
628000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07756
628100      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07757
628200      MOVE SPACES TO WS-CONTROL-REPORT.                           07758
628300                                                                  07759
628400      MOVE WS-GENERATED          TO    WS-PRT-DETAIL-LOB.         07760
628500      MOVE WS-REC-CNTS(8)        TO    WS-PRT-TOTAL-RECORDS.      07761
628600      MOVE WS-LOB-LIAB-AMTS(8)   TO    WS-PRT-LOB-LIAB.           07762
628700      MOVE WS-SERVICES-CNTS(8)   TO    WS-PRT-SERVICES.           07763
628800      MOVE WS-CLAIMS-CNTS(8)     TO    WS-PRT-CLAIMS.             07764
628900      MOVE WS-NOT-APPLICABLE     TO    WS-PRINT-DAYS-VISIT.       07765
629000      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07766
629100      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07767
629200      MOVE SPACES TO WS-CONTROL-REPORT.                           07768
629300      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07769
629400                                                                  07770
629500      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07771
629600      MOVE SPACES TO WS-CONTROL-REPORT.                           07772
629700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07773
629800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07774
629900                                                                  07775
630000      MOVE WS-SUB-TOTAL-D        TO    WS-PRT-DETAIL-LOB.         07776
630100                                                                  07777
630200      COMPUTE WS-COMPUTE-RECORDS   = WS-REC-CNTS(7)               07778
630300                                   + WS-REC-CNTS(8).              07779
630400                                                                  07780
630500      COMPUTE WS-COMPUTE-LOB-LIB   = WS-LOB-LIAB-AMTS(7)          07781
630600                                   + WS-LOB-LIAB-AMTS(8).         07782
630700                                                                  07783
630800      COMPUTE WS-COMPUTE-SERVICES  = WS-SERVICES-CNTS(7)          07784
630900                                   + WS-SERVICES-CNTS(8).         07785
631000                                                                  07786
631100      COMPUTE WS-COMPUTE-CLAIMS    = WS-CLAIMS-CNTS(7)            07787
631200                                   + WS-CLAIMS-CNTS(8).           07788
631300                                                                  07789
631400      COMPUTE WS-COMPUTE-DAYS      = WS-DAYS-CNTS(7)              07790
631500                                   + WS-DAYS-CNTS(8).             07791
631600      MOVE  WS-COMPUTE-RECORDS  TO  WS-PRT-TOTAL-RECORDS.         07792
631700      MOVE  WS-COMPUTE-LOB-LIB  TO  WS-PRT-LOB-LIAB.              07793
631800      MOVE  WS-COMPUTE-SERVICES TO  WS-PRT-SERVICES.              07794
631900      MOVE  WS-COMPUTE-CLAIMS   TO  WS-PRT-CLAIMS.                07795
632000      MOVE  WS-COMPUTE-DAYS     TO  WS-PRT-DAYS-VISIT.            07796
632100                                                                  07797
632200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07798
632300      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07799
632400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07800
632500      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07801
632600      MOVE SPACES TO WS-CONTROL-REPORT.                           07802
632700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07803
632800                                                                  07804
632900      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07805
633000      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07806
633100      EJECT                                                       07807
633200 X-4050-OUTPUT-TOTAL-PORTION.                                     07808
633300***************************************************************   07809
633400******            END OF REPORT PORTION                ********   07810
633500***************************************************************   07811
633600      MOVE WS-GRAND-TOTAL        TO    WS-PRT-DETAIL-LOB.         07812
633700      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07813
633800      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07814
633900                                                                  07815
634000      MOVE WS-A-B-C-D-TOT    TO    WS-PRT-DETAIL-LOB.             07816
634100                                                                  07817
634200      MOVE  WS-REC-CNTS(16)  TO  WS-PRT-TOTAL-RECORDS.            07818
634300                                                                  07819
634400      MOVE  WS-LOB-LIAB-AMTS(16)  TO  WS-PRT-LOB-LIAB.            07820
634500                                                                  07821
634600      MOVE  WS-SERVICES-CNTS(16)  TO  WS-PRT-SERVICES.            07822
634700                                                                  07823
634800      MOVE  WS-CLAIMS-CNTS(16)    TO  WS-PRT-CLAIMS.              07824
634900                                                                  07825
635000      MOVE  WS-DAYS-CNTS(16)      TO  WS-PRT-DAYS-VISIT.          07826
635100                                                                  07827
635200      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-CONTROL-REPORT.      07828
635300      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07829
635400      WRITE  O-PD-CLM-CONTROL-REPORT FROM WS-SEPARATOR.           07830
635500      MOVE SPACES TO O-PD-CLM-CONTROL-REPORT.                     07831
635600      EJECT                                                       07832
635700 Z-CALL-BOMBER.                                                   07833
635800***************************************************************   07834
635900******            INVOKE BOMBER MODULE                 ********   07835
636000***************************************************************   07836
636100      CALL 'AD0300' USING USER-ABEND-CODE.                        07837
636200      SKIP2                                                       07838
636300***************************************************************   07839
636400******            END OF PROGRAM FTESTEM0              ********   07840
636500***************************************************************   07841
