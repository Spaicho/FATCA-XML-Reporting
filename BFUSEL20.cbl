       IDENTIFICATION DIVISION.
       PROGRAM-ID. BFUSEL20.
       AUTHOR. Essadiq BAICH.

      ******************************************************************
      *      ____________________________________________________      *
      *    _(                                                    )_    *
      *  _(       C R E A T I O N   D U   R E P O R T I N G        )_  *
      * =_                                                          _= *
      *   (_                    F A T C A                         _)   *
      *     (____________________________________________________)     *
      *                                                                *
      *       ________    _     _________    ______       _            *
      *      |_   __  |  / \   |  _   _  | .' ___  |     / \           *
      *        | |_ \_| / _ \  |_/ | | \_|/ .'   \_|    / _ \          *
      *        |  _|   / ___ \     | |    | |          / ___ \         *
      *       _| |_  _/ /   \ \_  _| |_   \ `.___.'\ _/ /   \ \_       *
      *      |_____||____| |____||_____|   `.____ .'|____| |____|      *
      *                                                                *
      *                                                                *
      *               DATE          : 27/10/2014                       *
      *               COMPOSANT     : BFUSEL20                         *
      *               TYPE          : BATCH                            *
      *               AUTEUR        : ESSADIQ BAICH                    *
      *                                                                *
      *             __________________________________________         *
      *                                                                *
      *                                                                *
      * DESCRIPTION   : Ce Batch permet de produire le reporting FATCA *
      *                 sous format XML en exploitant le fichier des   *
      *                 informations et en utilisant le module de      *
      *                 de formatage XML.                              *
      *
      * MODULES UTILISES :                                             *
      *                                                                *
      *________________________________________________________________*
      *          |                                         |           *
      *    NOM   |              DESCRIPTION                |   COPY    *
      *__________|_________________________________________|___________*
      *          |                                         |           *
      * MCCDINAB | Module de gestion des ABENDS            |  CCCDINAB *
      *__________|_________________________________________|___________*
      *          |                                         |           *
      * MCCDBILA | Module de gestion des affichages        |  CCCDBILA *
      *__________|_________________________________________|___________*
      *          |                                         |           *
      * MFUSXL00 | Module de formatage XML                 |  CFUSXL00 *
      *__________|_________________________________________|___________*
      *          |                                         |           *
      * MGDATR03 | Module de manipulation des dates        |  CZGDA03  *
      *__________|_________________________________________|___________*
      *                                                                *
      *                                                                *
      * Fichiers manipulés :                                           *
      *                                                                *
      *________________________________________________________________*
      *          |                                      |   |          *
      *  DBD/DD  |              DESCRIPTION             | A |  COPY    *
      *__________|______________________________________|___|__________*
      *          |                                      |   |          *
      * DFUSLE21 | Fichier des informations reporting   | E | CFUSEL10 *
      *__________|______________________________________|___|__________*
      *          |                                      |   |          *
      * DFUSLS21 | Fichier XML de reporting             | S |          *
      *__________|______________________________________|___|__________*
      *                                                                *
      * DFUSLS22 | Fichier de MAJ de BFUSMJ00           | S |          *
      *__________|______________________________________|___|__________*
      *                                                                *
      *                                                                *
      * Liste des ABENDS émis  par le programme :                      *
      *                                                                *
      *________________________________________________________________*
      *          |                                                     *
      *  ABEND   |              DESCRIPTION                            *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   1000   | PB RECUPERATION DATE DU JOUR VIA MGDATR03           *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   1001   | PB RECUPERATION DATE DU JOUR VIA MGDATR03           *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   2011   | PB OUVERTURE FICHIER DFUSLE21                       *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   2012   | PB LECTURE   FICHIER DFUSLE21                       *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   2013   | PB FERMETURE FICHIER DFUSLE21                       *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   2021   | PB OUVERTURE FICHIER DFUSLS21                       *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   2022   | PB LECTURE   FICHIER DFUSLS21                       *
      *__________|_____________________________________________________*
      *          |                                                     *
      *   2023   | PB FERMETURE FICHIER DFUSLS21                       *
      *__________|_____________________________________________________*
      *                                                                *
      *                                                                *
      * Historique des modifications :                                 *
      *                                                                *
      *________________________________________________________________*
      * AUTEUR !   DATE   !  GAMA  !        MODIFICATIONS              *
      *________!__________!________!___________________________________*
150978*  CGI   !16/04/2015!20150978!Prises en compte des adaptations   *
150978*        !          !        !pour suivantes :                   *
150978*        !          !        !- Gestion du fichier vide          *
150978*        !          !        !- Récupération des références      *
150978*        !          !        !- Gestion de ruptures sur enntité  *
150978*        !          !        !- Gestion des clients récalcitrants*
150978*        !          !        !- Préparation de la MAJ des tables *
150978*        !          !        !  TBREPFUS, TBRBRFUS et TBFIDFUS   *
      *================================================================*
151197*  SBOU  !22/04/2015!20151197!MAJ ECV et Historique pour FATCA3  *
      *================================================================*
151225*  MCHA  !04/05/2015!20151225!TRAITEMENT DES CARATERES SPECIAUX  *
      *================================================================*
151345*  MCHA  !12/06/2015!20151345!AJOUT MAJ ECV RECALCITRANT         *
      ******************************************************************

      *****************************************************************
      *                                                               *
      *            E N V I R O N M E N T  D I V I S I O N             *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *
      * Fichier des informations Reporting
      *
           SELECT DFUSLE21 ASSIGN             TO DFUSLE21
           FILE STATUS FS-E01.

      *
      * Fichier XML du Reporting
      *
           SELECT DFUSLS21 ASSIGN             TO DFUSLS21
           FILE STATUS FS-S01.
150978*
150978* Fichier de MAJ des table TBFIDFUS, TBREPFUS et TBRBRFUS
150978*
150978     SELECT DFUSLS22 ASSIGN             TO DFUSLS22
150978     FILE STATUS FS-S02.

      *****************************************************************
      *                                                               *
      *                   D A T A   D I V I S I O N                   *
      *                                                               *
      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
                                                                        00615099
       FD  DFUSLE21                                                     00616099
           LABEL RECORD STANDARD                                        00617099
           RECORDING MODE F                                             00612099
           BLOCK CONTAINS 0 RECORDS.
                                                                        00630099
       01  FD-DFUSLE21              PIC  X(1000).
                                                                        00615099
       FD  DFUSLS21                                                     00616099
           LABEL RECORD STANDARD                                        00617099
           RECORDING MODE F                                             00620099
           BLOCK CONTAINS 0 RECORDS.

       01  FD-DFUSLS21              PIC  X(1000).

150978 FD  DFUSLS22                                                     00616099
150978     LABEL RECORD STANDARD                                        00617099
150978     RECORDING MODE F                                             00620099
150978     BLOCK CONTAINS 0 RECORDS.
150978
150978 01  FD-DFUSLS22              PIC  X(1500).

      ***************************************
      *                                     *
      *      WORKING STORAGE SECTION        *
      *                                     *
      ***************************************

       WORKING-STORAGE SECTION.

       copy FATCA. 

      *   *******************************
      *   *  variables pour préfixes    *
      *   *                             *
      *   *******************************

      *   Liste des balises dont le préfixe est 'ftc'
      *   les autres balises auront le préfixe par défaut 'sfa'.

       01     WS-PREFIXE-FTC.
        05    WS-L-PREFIXE  PIC  X(03) VALUE 'ftc'.
        05    WS-PREFIXE-ELEMENTS.
         10                PIC  9(04) BINARY VALUE 10.
         10                PIC  X(80) VALUE 'FATCA_OECD' .
         10                PIC  9(04) BINARY VALUE 11.
         10                PIC  X(80) VALUE 'MessageSpec' .
         10                PIC  9(04) BINARY VALUE 5.
         10                PIC  X(80) VALUE 'FATCA' .
         10                PIC  9(04) BINARY VALUE 11.
         10                PIC  X(80) VALUE 'ReportingFI' .
         10                PIC  9(04) BINARY VALUE 14.
         10                PIC  X(80) VALUE 'ReportingGroup'.
         10                PIC  9(04) BINARY VALUE 12.
         10                PIC  X(80) VALUE 'Intermediary' .
         10                PIC  9(04) BINARY VALUE 7.
         10                PIC  X(80) VALUE 'Sponsor' .
         10                PIC  9(04) BINARY VALUE 13.
         10                PIC  X(80) VALUE 'AccountReport'.
         10                PIC  9(04) BINARY VALUE 13.
         10                PIC  X(80) VALUE 'AccountNumber'.
         10                PIC  9(04) BINARY VALUE 13.
         10                PIC  X(80) VALUE 'AccountHolder'.
         10                PIC  9(04) BINARY VALUE 14.
         10                PIC  X(80) VALUE 'AcctHolderType'.
         10                PIC  9(04) BINARY VALUE 12.
         10                PIC  X(80) VALUE 'Organisation'.
         10                PIC  9(04) BINARY VALUE 16.
         10                PIC  X(80) VALUE 'SubstantialOwner'.
         10                PIC  9(04) BINARY VALUE 10.
         10                PIC  X(80) VALUE 'Individual'.
         10                PIC  9(04) BINARY VALUE 7.
         10                PIC  X(80) VALUE 'Payment '.
         10                PIC  9(04) BINARY VALUE 14.
         10                PIC  X(80) VALUE 'AccountBalance'.
         10                PIC  9(04) BINARY VALUE 12.
         10                PIC  X(80) VALUE 'AccountCount '.
         10                PIC  9(04) BINARY VALUE 7.
         10                PIC  X(80) VALUE 'DocSpec '.
         10                PIC  9(04) BINARY VALUE 11.
         10                PIC  X(80) VALUE 'PaymentAmnt'.
         10                PIC  9(04) BINARY VALUE 4.
         10                PIC  X(80) VALUE 'Type'.
         10                PIC  9(04) BINARY VALUE 12.
         10                PIC  X(80) VALUE 'CorrDocRefId'.
         10                PIC  9(04) BINARY VALUE 16 .
         10                PIC  X(80) VALUE 'CorrMessageRefId'.
         10                PIC  9(04) BINARY VALUE 8 .
         10                PIC  X(80) VALUE 'DocRefId'.
         10                PIC  9(04) BINARY VALUE 12.
         10                PIC  X(80) VALUE 'DocTypeIndic'.
         10                PIC  9(04) BINARY VALUE 10.
         10                PIC  X(80) VALUE 'PoolReport'.
         10                PIC  9(04) BINARY VALUE 22.
         10                PIC  X(80) VALUE 'AccountPoolReportType'.
         10                PIC  9(04) BINARY VALUE 12.
         10                PIC  X(80) VALUE 'PoolBalance'.
        05    WS-PREFIXE-ELEMENT-TAB REDEFINES WS-PREFIXE-ELEMENTS.
         10   WS-G-PREFIXE-ELEMENT OCCURS 27.
          15  WS-Q-PREFIXE-ELEMENT     PIC  9(04) BINARY.
          15  WS-L-PREFIXE-ELEMENT     PIC  X(80).
        05    WS-Q-PREFIXE-ELEMENT-TAB PIC  9(04) BINARY VALUE 27.

      ***************************************
      *                                     *
      *      VARIABLES DU TRAVAIL           *
      *                                     *
      ***************************************

      * ----  variable de rupture
      * ----  sur Identifiant de Compte
      * ----  sur Identifiant de Client
       01     WS-I-UNIQ-KAC-RUPT        PIC X(17) VALUE SPACE.
       01     WS-I-UNIQ-KPI-RUPT        PIC X(17) VALUE SPACE.

      * ----  Nombre de chiffres de la fraction décimale des montant
       01     WS-Q-NB-DECIM             PIC 9(01) VALUE 2.

       01     TOP-RUPTURE-CNT-CLI       PIC X(01) VALUE SPACE.
        88    RUPTURE-CNT-CLI                     VALUE '1'.
        88    RUPTURE-CNT-CLI-NON                 VALUE '2'.

       01     WS-G-ARBR-FATCA-OECD.
        05    WS-DOCTYPEINDIC           PIC X(07).
        05    WS-DOCREFID               PIC X(80).

MCHA+  01     L                         PIC  9(08) BINARY VALUE ZERO.
151225 01     I-SP                      PIC  9(08) BINARY VALUE ZERO.
151225 01     CAR-SP                    PIC  X(01) VALUE SPACES.
       01     WS-G-XML.
        05    WS-Q-XML-GENERATE         PIC  9(08) BINARY VALUE ZERO.
size  * 05    WS-L-XML-GENERATE         PIC  X(200000) VALUE SPACE.
size    05    WS-L-XML-GENERATE         PIC  X(22000) VALUE SPACE.
        05    WS-Q-INDT-UNIT            PIC  9(04) BINARY VALUE 2.
        05    WS-Q-INDT-INIT            PIC  9(04) BINARY VALUE 0.
        05    WS-N-XML-DCOP             PIC  9(04) BINARY VALUE 0.
        05    WS-Q-XML-DCOP-INDT        PIC  9(04) BINARY VALUE 0.

      * ----  Tableau pour stocker les lignes xml retournées par
      *       MFUSXL00 en vue d'écriture
        05    WS-Q-XML-TAB              PIC  9(04) BINARY VALUE ZERO.
        05    WS-G-XML-TAB              OCCURS 1000.
         10   WS-Q-XML-LINE             PIC  9(04) BINARY VALUE ZERO.
         10   WS-Q-XML-INDT             PIC  9(04) BINARY VALUE ZERO.
         10   WS-L-XML-LINE             PIC  X(500) VALUE SPACE.

      * ----  Tableau pour stocker les lignes xml de fermeture
      *       ces lignes sont générées au début du programme
      *       mais leurs écriture est différé jusqu'à la fin
        05    WS-Q-XML-TAB-FIN          PIC  9(04) BINARY VALUE ZERO.
        05    WS-G-XML-TAB-FIN          OCCURS 200.
         10   WS-Q-XML-LINE-FIN         PIC  9(04) BINARY VALUE ZERO.
         10   WS-Q-XML-INDT-FIN         PIC  9(04) BINARY VALUE ZERO.
         10   WS-L-XML-LINE-FIN         PIC  X(500) VALUE SPACE.

        05    I                         PIC  9(08) BINARY VALUE ZERO.

      ****************************
      *                          *
      *  Traitement des strings  *
      *                          *
      ****************************

        01    J                         PIC  9(08) BINARY VALUE ZERO.
        01    K                         PIC  9(08) BINARY VALUE ZERO.

        01    ws-b-string-sep           PIC  9(01) VALUE ZERO.

        01    ws-q-string-tab           PIC  9(08) BINARY VALUE ZERO.
        01    ws-g-string-tab.
          05  ws-l-string-in occurs 100 PIC  X(255) value space.

        01    ws-n-string               PIC  9(08) BINARY VALUE ZERO.
        01    ws-q-string-out           PIC  9(08) BINARY VALUE ZERO.
        01    ws-l-string-out           PIC  X(1000) value space.
        01    ws-q-string-buffer        PIC  9(08) BINARY VALUE ZERO.
        01    ws-l-string-buffer        PIC  X(1000) value space.

        01    ws-n-caractere-deb        PIC  9(08) BINARY VALUE ZERO.
        01    ws-n-caractere-fin        PIC  9(08) BINARY VALUE ZERO.
        01    ws-q-deb-fin-1            PIC  9(08) BINARY VALUE ZERO.

        01    ws-q-string-buffer-2      PIC  9(08) BINARY VALUE ZERO.
        01    ws-l-string-buffer-2      PIC  X(1000) value space.

      *************************************************************
      *                                                           *
      *   Zones WS pour stocker les données du fichier en entrée  *
      *                                                           *
      *************************************************************

      *    Compteur d'adresse par client
       01  WS-Q-DATA-DET-ADR                     PIC 9(04) BINARY.

      *    Compteur de bénéficiaires par client
       01  WS-Q-DATA-DET-BNF                     PIC 9(04) BINARY.

       01   WS-G-IDENT-ENREG.

         03   WS-G-IDENT-ENREG.

           05   WS-C-ENR                         PIC X(002).
           05   WS-I-IDENT                       PIC X(017).
           05   WS-N-LIG-IDENT                   PIC 9(003).
           05   WS-C-ENTIT                       PIC X(003).


         03   WS-G-DATA-ENREG.

           05   WS-G-DATA-TET.

            08   WS-G-DATA-TET-END.

             10   WS-C-REF-GIIN                  PIC X(020).

             10   WS-A-APPL                      PIC X(010).

             10   WS-C-MOD-REPORT                PIC X(001).

             10  WS-C-PAYS-EMET-GIN              PIC X(002).            00027400
                                                                        00027500
             10  WS-L-RAISON-SOCIALE             PIC X(060).            00027700
                                                                        00027800
             10  WS-C-PAYS-EMET                  PIC X(002).            00028000
                                                                        00028100
             10  WS-C-PAYS-DEST                  PIC X(002).            00028300
                                                                        00028400
             10  WS-C-CODE-EXPD                  PIC X(002).            00028600
                                                                        00028700
             10  WS-L-CONTACT1                   PIC X(032).            00028900
                                                                        00029000
             10  WS-L-NOM-CONTACT1               PIC X(070).            00029200
                                                                        00029300
             10  WS-N-TEL-CONTACT1               PIC X(020).            00029500
                                                                        00029600
             10  WS-L-MAIL-CONTACT1              PIC X(070).            00029800
MCHA  *      Type de FATCA                                              00030400
"            10  WS-C-TYPE-FATCA                 PIC X(008).            00030500
"     *      Référence initial du fichier de reporting FATCAn           00030800
"            10  WS-I-REF-FIC-INIT               PIC X(055).            00031000
"     *      Date de référence du  fichier de reporting FATCAn          00031300
"            10  WS-D-REF-FIC-INIT               PIC X(026).            00031400
"     *      Référence du fichier de reporting FATCAn                   00031700
"            10  WS-I-REF-FIC                    PIC X(055).            00031900
"     *      FILLER                                                     00032200
MCHA         10 WS-FILLER                       PIC X(540).             00032300
                                                                        00033500
            08   WS-G-DATA-TET-ADR.

             10  WS-C-TYPE-ADR                   PIC X(002).

             10  WS-L-COMM-ADR                   PIC X(032).

             10  WS-C-CPOST                      PIC X(005).

             10  WS-C-PAYS-ADR                   PIC X(002).

             10  WS-L-INTIT-COURR-1              PIC X(038).

             10  WS-L-INTIT-COURR-2              PIC X(038).

             10  WS-L-ADR-LIGNE-1                PIC X(038).

             10  WS-L-ADR-LIGNE-2                PIC X(038).

             10  WS-L-ADR-LIGNE-3                PIC X(038).

             10  WS-L-PAYS-ADR                   PIC X(038).

             10  WS-FILLER                       PIC X(706).            00033400
                                                                        00033500

           05   WS-G-DATA-DET.

            08   WS-G-DATA-DET-CNT.

             10  WS-I-UNIQ-KAC                   PIC X(017).

             10  WS-I-KAC-IBAN                   PIC X(030).

150978       10  WS-I-REF-BLOC                   PIC X(090).

MCHA+        10  WS-I-REF-BLOC-INIT              PIC X(090).
"
"            10  WS-FILLER-CNT                   PIC X(120).
"
"            10  WS-Q-NBR-DEC-SLD                PIC 9(001).
"
"            10  WS-M-MNT-ASS-SLD                PIC 9(018).
"
"            10  WS-C-DEV                        PIC X(003).
"
"            10  WS-Q-LIST-MNT                   PIC 9(001).
"
"            10  WS-T-LIST-MNT OCCURS 4.
"
"                15 WS-C-TYP-MNT                 PIC X(001).
"
"                15 WS-Q-NBR-DEC                 PIC 9(001).
"
"                15 WS-M-MNT-ASS                 PIC 9(018).
MCHA+
MC           10  WS-I-REF-FID-INIT               PIC X(080).            00033400

MC           10  WS-FILLER                       PIC X(435).            00033400
                                                                        00033500

            08   WS-G-DATA-DET-CLT.

             10  WS-I-UNIQ-KPI                   PIC X(017).

             10  WS-C-NTUR-PERS                  PIC X(002).

             10  WS-C-REF-GIIN                   PIC X(020).

             10  WS-L-NOM-NAISS                  PIC X(032).

             10  WS-C-TITRE-CVLTE                PIC X(001).

             10  WS-L-PRENOM                     PIC X(032).

150978       10  WS-L-PRENOM-2                   PIC X(032).

             10  WS-D-NAISS                      PIC X(010).

             10  WS-L-NOM-MRLT                   PIC X(032).

             10  WS-C-PAYS-NLITE                 PIC X(002).

             10  WS-C-AUTRE-PAYS-NLITE           PIC X(002).

             10  WS-C-PAYS-NAISS                 PIC X(002).

             10  WS-C-DEPT-NAISS                 PIC X(002).

             10  WS-L-VILL-NAISS                 PIC X(032).

             10  WS-L-RAIS-SOCIALE               PIC X(060).

MCHA+        10  WS-C-TYPE-CLASS                 PIC X(008).

             10  WS-FILLER                       PIC X(689).            00033400
                                                                        00033500

             10  WS-G-DATA-DET-ADR OCCURS 30.

              15 WS-C-TYPE-ADR                   PIC X(002).

              15 WS-L-COMM-ADR                   PIC X(032).

              15 WS-C-CPOST                      PIC X(005).

              15 WS-C-PAYS-ADR                   PIC X(002).

              15 WS-L-INTIT-COURR-1              PIC X(038).

              15 WS-L-INTIT-COURR-2              PIC X(038).

              15 WS-L-ADR-LIGNE-1                PIC X(038).

              15 WS-L-ADR-LIGNE-2                PIC X(038).

              15 WS-L-ADR-LIGNE-3                PIC X(038).

              15 WS-L-PAYS-ADR                   PIC X(038).

              15 WS-FILLER                       PIC X(706).            00033400
                                                                        00033500
             10  WS-G-DATA-DET-BNF OCCURS 30.

              15 WS-I-UNIQ-KPI-BNF               PIC X(017).

              15 WS-C-REF-GIIN-BNF               PIC X(020).

MCHA+         15 WS-C-TITRE-CVLTE-F              PIC X(001).

              15 WS-L-NOM-NAISS-BNF              PIC X(032).

              15 WS-PRENOM-BNF                   PIC X(032).

              15 WS-D-NAISS-BNF                  PIC X(010).

              15 WS-C-PAYS-ADR-BNF               PIC X(002).

              15 WS-L-COMM-ADR-BNF               PIC X(032).

              15 WS-FILLER                       PIC X(829).            00033400
                                                                        00033500

           05   WS-G-DATA-ENQ.

            08   WS-G-DATA-ENQ-END.

             10   WS-N-CPT                       PIC 9(009).
             10   WS-N-CLT                       PIC 9(009).
             10   WS-FILLER                      PIC X(957).            00027700
                                                                        00027800

      *******************************
      *                             *
      *   Variables d'horodatage    *
      *                             *
      *******************************

      *   Références de compilation
       01 W-REF-COMPIL.
          03 W-DATE-COMPIL.
             05 W-DATE-COMPIL-JJ       PIC X(02) VALUE SPACES.
             05 W-DATE-COMPIL-S1       PIC X(01) VALUE SPACES.
             05 W-DATE-COMPIL-MM       PIC X(02) VALUE SPACES.
             05 W-DATE-COMPIL-S2       PIC X(01) VALUE SPACES.
             05 W-DATE-COMPIL-AA       PIC X(02) VALUE SPACES.

          03 W-HEURE-COMPIL.
             05 W-HEURE-COMPIL-HH      PIC X(02) VALUE SPACES.
             05 W-HEURE-COMPIL-DP1     PIC X(01) VALUE SPACES.
             05 W-HEURE-COMPIL-MM      PIC X(02) VALUE SPACES.
             05 W-HEURE-COMPIL-DP2     PIC X(01) VALUE SPACES.
             05 W-HEURE-COMPIL-SS      PIC X(02) VALUE SPACES.

      *   Date de traitement (date et heure)
       01 WS-D-H-TRT                   PIC X(26) VALUE SPACES.

      *   Date traitement
       01 WS-DATE-TRAITEMENT.
          05 WS-ANNEE-TRAITEMENT       PIC X(4) VALUE SPACES.
          05 FILLER                    PIC X VALUE '-'.
          05 WS-MOIS-TRAITEMENT        PIC X(2) VALUE SPACES.
          05 FILLER                    PIC X VALUE '-'.
          05 WS-JOUR-TRAITEMENT        PIC X(2) VALUE SPACES.

      *   Heure traitement
       01 WS-HEURE-TRAITEMENT.
          05 WS-HH-TRAITEMENT          PIC X(2) VALUE SPACES.
          05 FILLER                    PIC X VALUE ':'.
          05 WS-MM-TRAITEMENT          PIC X(2) VALUE SPACES.
          05 FILLER                    PIC X VALUE ':'.
          05 WS-SS-TRAITEMENT          PIC X(2) VALUE SPACES.

      *   Date du traitement
       01 WS-HORODATAGE-TRT.
          05 W-D-ISO.
             10 W-D-SSAA               PIC 9(04) VALUE ZEROES.
             10 FILLER                 PIC X     VALUE SPACE.
             10 W-D-MM                 PIC 9(02) VALUE ZEROES.
             10 FILLER                 PIC X     VALUE SPACE.
             10 W-D-JJ                 PIC 9(02) VALUE ZEROES.

          05 FILLER                    PIC X     VALUE SPACE.
          05 W-H-ISO.
             10 W-HH                   PIC 9(02) VALUE ZEROES.
             10 FILLER                 PIC X     VALUE SPACE.
             10 W-MN                   PIC 9(02) VALUE ZEROES.
             10 FILLER                 PIC X     VALUE SPACE.
             10 W-SC                   PIC 9(02) VALUE ZEROES.
             10 FILLER                 PIC X(7)  VALUE SPACES.

      *******************************
      *                             *
      *  Variables du travail       *
      *                             *
      *******************************

      *                                                                 00840029
      ***  LES CODES RETOUR DES DIFFéRENTS FICHIERS                     00840199
      *                                                                 00840229
       01  FS-E01                      PIC X(02) VALUE SPACES.          02530199
       01  FS-S01                      PIC X(02) VALUE SPACES.          02530199
150978 01  FS-S02                      PIC X(02) VALUE SPACES.          02530199
                                                                        02530229
       01  WS-LGTH-E01                 PIC 9(09) COMP VALUE ZEROS.      02530199
       01  WS-LGTH-S01                 PIC 9(09) COMP VALUE ZEROS.      02530199
                                                                        02530229
      *   Compteur du nombre de lectures du fichier DFUSLE21
       01 WS-NB-LECT-E01               PIC 9(9) VALUE ZEROES.

      *   Compteur du nombre d'écritures du fichier DFUSLS21
       01 WS-NB-ECRT-S01               PIC 9(9) VALUE ZEROES.

150978*   Compteur du nombre d'écritures du fichier DFUSLS22
150978 01 WS-NB-ECRT-S02               PIC 9(9) VALUE ZEROES.

      *    Compteur d'AccountReport
       01  WS-Q-AccountReport          PIC 9(09) BINARY VALUE ZERO.

150978*    Compteur de PoolReport
150978 01  WS-Q-PoolReport             PIC 9(09) BINARY VALUE ZERO.

      *   Booléen pour indiquer fin du fichier
       01 TOP-FIN-DFUSLE21             PIC X(1)  VALUE SPACE.
          88 FIN-DFUSLE21                        VALUE 'O'.

150978*   Booléen pour indiquer fin du fichier
150978 01 TOP-FIN-TRT-ACCOUNT          PIC X(1)  VALUE SPACE.
150978    88 FIN-TRT-ACCOUNT-OK                  VALUE 'O'.
150978    88 FIN-TRT-ACCOUNT-KO                  VALUE 'N'.

150978*   Booléen pour indiquer présence des comptes
150978 01 TOP-EXIST-ACCOUNT          PIC X(1)  VALUE SPACE.
150978    88 EXIST-ACCOUNT-OK                  VALUE 'O'.
150978    88 EXIST-ACCOUNT-KO                  VALUE 'N'.

      *   Variable numérique de code ABEND
       01 WS-CODE-ABEND                PIC 9(4) VALUE ZERO.

      *   Indice pour lignes à afficher par MCCDBILA
       01 WS-DISP                      PIC 9(2) VALUE ZEROES.



      *******************************
      *                             *
      *  Lignes pour affichages     *
      *                             *
      *******************************

      *   DEBUT DU PROGRAMME
       01 WS-LIGNE-DEBUTPROG.

          05 WS-LIGNE-DEBUTPROG0       PIC X(01) VALUE '*'.
          05 WS-LIGNE-DEBUTPROG1       PIC X(11) VALUE '==         '.
          05 WS-LIGNE-DEBUTPROG2       PIC X(39) VALUE
                           'DEBUT D''EXECUTION DU PROGRAMME BFUSEL20'.
          05 WS-LIGNE-DEBUTPROG3       PIC X(12) VALUE '          =='.
          05 WS-LIGNE-DEBUTPROG4       PIC X(01) VALUE '*'.

      *   AFFICHAGE DES ANOMALIES
       01 WS-LIGNE-ANO1.

          05 WS-LIGNE-ANO10            PIC X(4)  VALUE '*=> '.
          05 WS-LIGNE-ANO11            PIC X(14) VALUE ALL ' '.
          05 WS-LIGNE-ANO12            PIC X(45) VALUE ALL ' '.
          05 WS-LIGNE-ANO13            PIC X(1)  VALUE '*'.

       01 WS-LIGNE-ANO2.
          05 WS-LIGNE-ANO20            PIC X(4)  VALUE '*=> '.
          05 WS-LIGNE-ANO21            PIC X(59) VALUE ALL ' '.
          05 WS-LIGNE-ANO22            PIC X(1)  VALUE '*'.

       01 WS-LIGNE-ANO4.
          05 WS-LIGNE-ANO40            PIC X(4)  VALUE '*=> '.
          05 WS-LIGNE-ANO41            PIC X(16) VALUE ALL ' '.
          05 WS-LIGNE-ANO42            PIC X(43) VALUE ALL ' '.
          05 WS-LIGNE-ANO43            PIC X(1)  VALUE '*'.

       01  WS-LIGNE-ANO0.
           05 WS-LIGNE-ANO00           PIC X(10)  VALUE '*========='.
           05 WS-LIGNE-ANO01           PIC X(44)  VALUE
            '==============    ANOMALIE    =============='.
           05 WS-LIGNE-ANO02           PIC X(10)  VALUE '=========*'.

      *   LIGNE '='
       01 WS-LIGNE-EGAL.

          05 WS-LIGNE-EGAL0            PIC X(01)  VALUE '*'.
          05 WS-LIGNE-EGAL1            PIC X(62)  VALUE ALL '='.
          05 WS-LIGNE-EGAL2            PIC X(01)  VALUE '*'.

      *   LIGNE ' '
       01 WS-LIGNE-VIDE.

          05 WS-LIGNE-VIDE0            PIC X(01)  VALUE '*'.
          05 WS-LIGNE-VIDE1            PIC X(62)  VALUE ALL ' '.
          05 WS-LIGNE-VIDE2            PIC X(01)  VALUE '*'.

      *   LIGNE '*'
       01 WS-LIGNE-ETOILE.

          05 WS-LIGNE-ETOILE0          PIC X(01)  VALUE '*'.
          05 WS-LIGNE-ETOILE1          PIC X(62)  VALUE ALL '*'.
          05 WS-LIGNE-ETOILE2          PIC X(01)  VALUE '*'.

      *   LIGNE 'FIN PROGRAMME '
       01 WS-LIGNE-FINPROG.

          05 WS-LIGNE-FINPROG0         PIC X(01)  VALUE '*'.
          05 WS-LIGNE-FINPROG1         PIC X(23)  VALUE ALL ' '.
          05 WS-LIGNE-FINPROG2         PIC X(16) VALUE 'FIN PROGRAMME'.
          05 WS-LIGNE-FINPROG3         PIC X(23)  VALUE ALL ' '.
          05 WS-LIGNE-FINPROG4         PIC X(01)  VALUE '*'.

      *******************************
      *                             *
      *         Zones COPY          *
      *                             *
      *******************************

      * COPY IDEV
        COPY CWNAAFCT REPLACING 'PROGRAMM' BY =='BFUSEL20'==.
        COPY CWNAABAT.

      * Description du fichier DFUSLE21
       01 ENR-DFUSLE21.
        COPY CFUSEL10 REPLACING ==(P)==    BY ==E01==.

      * Description du fichier DFUSLS21
       01 ENR-DFUSLS21               PIC X(10000).

      * Description de la zone de communication avec MFUSXL00
        COPY CFUSXL00 REPLACING ==(PREF)== BY ==XL00==.

150978* Description de la zone de communication avec BFUSMJ00
150978  COPY CFUSMJ00 REPLACING ==(P)== BY ==MJ00==.

      * COPY du module MCCDBILA
        COPY CCCDBILA REPLACING ==(PREF)== BY ==BILA==.

      * COPY du module MCCDINAB
        COPY CCCDINAB REPLACING ==(PREF)== BY ==INAB==.

      * COPY du module MGDATR03
        COPY CZGDA03.

      *****************************************************************
      *                                                               *
      *            P R O C E D U R E   D I V I S I O N                *
      *                                                               *
      *****************************************************************

       PROCEDURE DIVISION.

      * ----------->  DECLARATIVES

      DDECLARATIVES.
      DTRACING SECTION.
      D    USE FOR DEBUGGING ON ALL PROCEDURES.
      DP10.
      D    DISPLAY 'BFUSEL20:' DEBUG-NAME ' - '   DEBUG-CONTENTS.
      DEND DECLARATIVES.


      * ----------->  Cinématique générale



      *    Initialisation du traitement
           PERFORM INIT-TRAIT

      *    C-ENR = '00'
      *    Traitement des données de l'entité déclarante
      *    effectue :
      *    - Deux lecture du fichier pour entité et adresse
      *    - Ecriture de l'arbre FATCA (sans accountReport)

           PERFORM TRAIT-TET

150978     SET FIN-TRT-ACCOUNT-KO TO TRUE
150978     SET EXIST-ACCOUNT-KO   TO TRUE

      *    Boucle de traitement des comptes
150978     PERFORM UNTIL E01-C-ENR = '90'
                      OR FIN-DFUSLE21

              PERFORM LECT-FICH-E01

150978        IF E01-C-ENR NOT = '30'

                 PERFORM TRAIT-RUPT-CNT-CLI

150978*          IF RUPTURE-CNT-CLI
150978           IF RUPTURE-CNT-CLI AND FIN-TRT-ACCOUNT-KO

      D             DISPLAY ' RUPTURE-CNT-CLI'
      *             Init données AccountReport
                    PERFORM INIT-ARBR-AccountReport
      *             Alim données AccountReport
                    PERFORM ALIM-ARBR-AccountReport
      *             générer AccountReport du compte précédant
                    PERFORM GENER-XML-AccountReport
      *             Ecriture accountReport
                    PERFORM ECRT-WS-G-XML
      D          ELSE
      D             DISPLAY ' PAS DE RUPTURE-CNT-CLI'
                 END-IF

150978           IF E01-C-ENR = '20'
                    PERFORM ALIM-WS-G-DATA-DET
MCHA                SET EXIST-ACCOUNT-OK TO TRUE
150978           END-IF
150978        END-IF

150978        IF E01-C-ENR = '30'
150978*
150978           IF FIN-TRT-ACCOUNT-KO AND EXIST-ACCOUNT-OK
150978              SET FIN-TRT-ACCOUNT-OK TO TRUE
150978D             DISPLAY ' RUPTURE-CNT-CLI'
150978*             Init données AccountReport
150978              PERFORM INIT-ARBR-AccountReport
150978*             Alim données AccountReport
150978              PERFORM ALIM-ARBR-AccountReport
150978*             générer AccountReport du compte précédant
150978              PERFORM GENER-XML-AccountReport
150978*             Ecriture accountReport
150978              PERFORM ECRT-WS-G-XML
150978*             Desactiver le flag de rupture
150978              SET RUPTURE-CNT-CLI-NON TO TRUE
150978           END-IF
150978
150978*==>       Intégration des clients récalcitrants
150978*          Init données PoolReport
150978           PERFORM INIT-ARBR-PoolReport
150978*          Alim données PoolReport
150978           PERFORM ALIM-ARBR-PoolReport
150978*          générer bloc PoolReport
150978           PERFORM GENER-XML-PoolReport
150978*          Ecriture du PoolReport
150978           PERFORM ECRT-WS-G-XML
150978        END-IF
150978
150978        IF E01-C-ENR = '40'
150978*          des tables TBFIDFUS, TBRBRFUS et TBREPFUS
150978           EVALUATE E01-C-ENTIT
150978              WHEN 'FID'
150978                 PERFORM ALIM-TBFIDFUS
150978              WHEN 'REP'
150978                 PERFORM ALIM-TBREPFUS
150978              WHEN 'RBR'
150978                 PERFORM ALIM-TBRBRFUS
151197              WHEN 'LCC'
150978                 PERFORM ALIM-TBLCCFUS
151197              WHEN 'CLI'
MCHA+                  MOVE E01-G-DATA-MAJ-CLI TO MJ00-CFUSMJ00
151197              WHEN 'CLR'
MCHA+                  MOVE E01-G-DATA-MAJ-CLR TO MJ00-CFUSMJ00
150978              WHEN OTHER
150978                 MOVE 4014               TO WS-CODE-ABEND
150978                 PERFORM TRAIT-ABEND
150978           END-EVALUATE
150978
150978           PERFORM ECRT-FICH-S02
150978        END-IF
150978

           END-PERFORM

      *    C-ENR = '99'
      *    Traitement enqueue
150978     IF WS-NB-LECT-E01 NOT = ZERO
              PERFORM TRAIT-ENQ
150978     END-IF

      *    Finalisation du traitement
           PERFORM FIN-TRAIT

           .

      *
      * ----------------------> NIVEAU 001
      *

      *--------------*
       INIT-TRAIT.
      *--------------*

      *    Initialisation des variables du travail
           PERFORM INIT-DATA-WS

      *    Recuperation de la date systeme
           PERFORM RECUP-DATE-HEURE-DEB

      *    Ouverture du fichier DFUSLE21
           PERFORM OUVR-FICH-E01

      *    Ouverture du fichier DFUSLS21
           PERFORM OUVR-FICH-S01

150978*    Ouverture du fichier DFUSLS22
150978     PERFORM OUVR-FICH-S02

      *    Affichage bilan du début d'execution
           PERFORM AFFICH-BILAN-DEB

           .

      *---------*
       TRAIT-TET.
      *---------*

      *    Lecture Ligne entité déclarante
           PERFORM LECT-FICH-E01

150978     IF NOT FIN-DFUSLE21
      *       Alimentation données entité déclarante
              PERFORM ALIM-DATA-WS-TET-END

      *       Lecture Ligne entité déclarante - Adresse
              PERFORM LECT-FICH-E01

      *       Alimentation données entité déclarante -Adresse
              PERFORM ALIM-DATA-WS-TET-ADR

      *       Affichage bilan entité déclarante
              PERFORM AFFICH-BILAN-END

      *       Alimentation de l'arbre FATCA (sans AccountReport)
              PERFORM ALIM-ARBR-FATCA

      *       Generer l'arbre FATCA (sans AccountReport)
              PERFORM GENER-XML-FATCA-OECD

      *       Decouper XML FATCA pour permettre d'insérer les
      *       AccountReport
              PERFORM DECOUP-XML-FATCA-OECD

      *       Ecriture de la structure générale FATCA
              PERFORM ECRT-WS-G-XML
150978     END-IF
           .

      *----------------*
       ALIM-ARBR-FATCA.
      *----------------*

      *    Valoriser DocTypeIndic
           PERFORM VALORISER-DocTypeIndic

      *    Valoriser DocRefId
150978*    PERFORM VALORISER-DocRefId

      *    Balise FATCA-OECD
           PERFORM ALIM-ARBR-FATCA-OECD

      *    Balise MessageSpec
           PERFORM ALIM-ARBR-MessageSpec

      *    Balise ReportingFI
           PERFORM ALIM-ARBR-ReportingFI

      *    Balise ReportingGroup (sauf AccountReport)
           PERFORM ALIM-ARBR-ReportingGroup

           .

      *----------------------*
       VALORISER-DocTypeIndic.
      *----------------------*

      *   Type de déclaration communiquée
      *   valeur possible :
      *     FATCA1  = Données Nouvelles
      *     FATCA2  = Données Corrigées
      *     FATCA3  = Données Annulées
      *     FATCA4  = Données Modifiées
      *     FATCA11 = Nouvelles Données Test
      *     FATCA12 = Données Test Corrigées
      *     FATCA13 = Données Test Annulées
      *     FATCA14 = Données Test Modifiées

           IF WS-C-MOD-REPORT
           OF WS-G-DATA-TET-END = '2'
MCHA+ *       DISPLAY 'WS-C-TYPE-FATCA ' WS-DOCREFID(1:7)
              MOVE WS-DOCREFID(1:7)        TO WS-DOCTYPEINDIC
           ELSE
              MOVE WS-DOCREFID(1:6)        TO WS-DOCTYPEINDIC
           END-IF
           .

      *------------------*
       VALORISER-DocRefId.
      *------------------*

      *    Identifiant du bloc de données
      *    Concatener :
      *    MessageRefId " " Name " reporting FI"
      *    exemple = fatca1-2014-17-01 Assureur A reporting FI

           MOVE SPACE                      TO WS-DOCREFID


           move 0                          to ws-b-string-sep
           move 3                          to ws-q-string-tab
           MOVE WS-DOCTYPEINDIC            TO ws-l-string-in(1)
           MOVE '-'                        TO ws-l-string-in(2)
           MOVE WS-DATE-TRAITEMENT         TO ws-l-string-in(3)

           perform TRAIT-STRING

           move ws-q-string-out            TO ws-q-string-buffer-2
           move ws-l-string-out (1:ws-q-string-out)
                                           TO ws-l-string-buffer-2

           move 1                          to ws-b-string-sep
           move 3                          to ws-q-string-tab
           MOVE ws-l-string-buffer-2(1:ws-q-string-buffer-2)
                                           to ws-l-string-in(1)
           MOVE WS-L-RAISON-SOCIALE
             OF WS-G-DATA-TET-END          TO ws-l-string-in(2)
           MOVE 'reporting FI'             TO ws-l-string-in(3)
           perform TRAIT-STRING
           move ws-l-string-out (1:ws-q-string-out)
                                           TO WS-DOCREFID
150978     MOVE SPACE                      TO WS-DOCREFID
150978D    DISPLAY 'E01-I-REF-FIC :' E01-I-REF-FIC OF E01-G-DATA-TET-END
150978D    DISPLAY 'WS-DOCREFID   :' WS-DOCREFID
150978     MOVE E01-I-REF-FIC OF E01-G-DATA-TET-END TO WS-DOCREFID

           .

      *-------------------*
       ALIM-ARBR-FATCA-OECD.
      *-------------------*

      *    Balise FATCA_OECD

           MOVE '1.0'
                                       TO tech-attr-req-version
           .

      *--------------------*
       ALIM-ARBR-MessageSpec.
      *--------------------*

      *    Balise MessageSpec

           MOVE 1                      TO SendingCompanyIN--C
                                       OF MessageSpec-COUNTERS

           MOVE WS-C-REF-GIIN
             OF WS-G-DATA-TET-END      TO SendingCompanyIN
                                       OF MessageSpec
                                         (SendingCompanyIN--C
                                       OF MessageSpec-COUNTERS)

           MOVE WS-C-PAYS-EMET
             OF WS-G-DATA-TET-END      TO TransmittingCountry
                                       OF MessageSpec

           MOVE WS-C-PAYS-DEST
             OF WS-G-DATA-TET-END      TO ReceivingCountry
                                       OF MessageSpec

           MOVE 'FATCA'                TO MessageType
                                       OF MessageSpec

           MOVE 0                      TO Warning--C
                                       OF MessageSpec-COUNTERS

           MOVE 0                      TO Contact--C
                                       OF MessageSpec-COUNTERS

           MOVE SPACE                  TO MessageRefId
                                       OF MessageSpec


           move 0                      to ws-b-string-sep
           move 3                      to ws-q-string-tab
           MOVE WS-DOCTYPEINDIC        TO ws-l-string-in (1)
           MOVE '-'                    TO ws-l-string-in (2)
           MOVE WS-DATE-TRAITEMENT     TO ws-l-string-in (3)

           perform trait-string

           move ws-l-string-out(1:ws-q-string-out)
                                       TO MessageRefId
                                       OF MessageSpec
           MOVE WS-DOCREFID
                                       TO MessageRefId
                                       OF MessageSpec

MCHA+-*    IF WS-I-REF-FIC-INIT =  SPACES OR LOW-VALUE
MCHA+-     IF WS-DOCREFID(1:7) = 'FATCA1-' OR 'FATCA11'
"             MOVE 0                   TO CorrMessageRefId--C
"                                      OF MessageSpec-COUNTERS
MCHA+-     ELSE
"             MOVE 1                   TO CorrMessageRefId--C
"                                      OF MessageSpec-COUNTERS
"     D       DISPLAY ' WS-I-REF-FIC-INIT 'WS-I-REF-FIC-INIT
"             MOVE WS-I-REF-FIC-INIT
"                OF WS-G-DATA-TET-END  TO CorrMessageRefId
"                                      OF MessageSpec
"                                        (CorrMessageRefId--C
"                                      OF MessageSpec-COUNTERS)
MCHA+-     END-IF

           MOVE SPACE                  TO ReportingPeriod
                                       OF MessageSpec

           STRING WS-A-APPL
               OF WS-G-DATA-TET-END
                  (1:4)
                  '-12-31'
                  DELIMITED BY SIZE  INTO ReportingPeriod
                                       OF MessageSpec


           MOVE SPACE                  TO Timestamp
                                       OF MessageSpec

           STRING WS-DATE-TRAITEMENT
                  'T'
                  WS-HEURE-TRAITEMENT
                  DELIMITED BY SIZE  INTO Timestamp
                                       OF MessageSpec
           .

      *--------------------*
       ALIM-ARBR-ReportingFI.
      *--------------------*

      *    Balise ReportingFI

           MOVE 1                      TO ResCountryCode--C
                                       OF ReportingFI-COUNTERS

SBOU  *    MOVE WS-C-PAYS-ADR
SBOU  *      OF WS-G-DATA-TET-ADR      TO ResCountryCode
SBOU  *                                OF ReportingFI
SBOU  *                                  (ResCountryCode--C
SBOU  *                                OF ReportingFI-COUNTERS)
           MOVE SPACE
                                       TO ResCountryCode
                                       OF ReportingFI
                                         (ResCountryCode--C
                                       OF ReportingFI-COUNTERS)

           MOVE 1                      TO TIN--C
                                       OF ReportingFI-COUNTERS

           MOVE WS-C-REF-GIIN
             OF WS-G-DATA-TET-END      TO tech-text
                                       OF TIN
                                       OF ReportingFI
                                         (TIN--C
                                       OF ReportingFI-COUNTERS)

MCHA+      MOVE WS-C-PAYS-EMET-GIN
MCHA+        OF WS-G-DATA-TET-END      TO tech-attr-opt-issuedBy
MCHA- *    MOVE SPACE                  TO tech-attr-opt-issuedBy
                                       OF TIN
                                       OF ReportingFI
                                         (TIN--C
                                       OF ReportingFI-COUNTERS)

           MOVE 1                      TO Name--C
                                       OF ReportingFI-COUNTERS

           MOVE WS-L-RAISON-SOCIALE
             OF WS-G-DATA-TET-END      TO tech-text
                                       OF Name
                                       OF ReportingFI
                                         (Name--C
                                       OF ReportingFI-COUNTERS)

           MOVE SPACE                  TO tech-attr-opt-nameType
                                       OF Name
                                       OF ReportingFI
                                         (Name--C
                                       OF ReportingFI-COUNTERS)

           MOVE 1                      TO R-Address--C
                                       OF ReportingFI-COUNTERS

           MOVE SPACE                  TO tech-attr-opt-legalAddressType
                                       OF R-Address
                                       OF ReportingFI
                                         (R-Address--C
                                       OF ReportingFI-COUNTERS)

           MOVE WS-C-PAYS-ADR
             OF WS-G-DATA-TET-ADR      TO CountryCode
                                       OF R-Address
                                       OF ReportingFI
                                         (R-Address--C
                                       OF ReportingFI-COUNTERS)

           MOVE 0                      TO AddressFree2--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 1                      TO AddressFix--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO Street--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO SuiteIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO FloorIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO DistrictName--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 0                      TO POB--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 1                      TO PostCode--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE WS-C-CPOST
             OF WS-G-DATA-TET-ADR      TO PostCode
                                       OF R-Address
                                       OF ReportingFI
                                         (R-Address--C
                                       OF ReportingFI-COUNTERS
                                          AddressFix--C
                                       OF ReportingFI-COUNTERS
                                          PostCode--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS)

           MOVE WS-L-COMM-ADR
             OF WS-G-DATA-TET-ADR      TO City
                                       OF R-Address
                                       OF ReportingFI
                                         (R-Address--C
                                       OF ReportingFI-COUNTERS
                                          AddressFix--C
                                       OF ReportingFI-COUNTERS)

           MOVE 0                      TO CountrySubentity--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE 1                      TO AddressFree--C
                                       OF R-Address-COUNTERS
                                       OF ReportingFI-COUNTERS

           MOVE SPACE                  TO AddressFree
                                       OF R-Address
                                       OF ReportingFI
                                         (R-Address--C
                                       OF ReportingFI-COUNTERS
                                          AddressFree--C
                                       OF ReportingFI-COUNTERS)


           move 1                      to ws-b-string-sep
           move 6                      to ws-q-string-tab
           MOVE WS-L-INTIT-COURR-1
             OF WS-G-DATA-TET-ADR      TO ws-l-string-in (1)
           MOVE WS-L-INTIT-COURR-2
             OF WS-G-DATA-TET-ADR      TO ws-l-string-in (2)
           MOVE WS-L-ADR-LIGNE-1
             OF WS-G-DATA-TET-ADR      TO ws-l-string-in (3)
           MOVE WS-L-ADR-LIGNE-2
             OF WS-G-DATA-TET-ADR      TO ws-l-string-in (4)
           MOVE WS-L-ADR-LIGNE-3
             OF WS-G-DATA-TET-ADR      TO ws-l-string-in (5)
           MOVE WS-L-PAYS-ADR
             OF WS-G-DATA-TET-ADR      TO ws-l-string-in (6)

           perform trait-string

           move ws-l-string-out(1:ws-q-string-out)
                                       TO AddressFree
                                       OF R-Address
                                       OF ReportingFI
                                         (R-Address--C
                                       OF ReportingFI-COUNTERS
                                          AddressFree--C
                                       OF ReportingFI-COUNTERS)



           MOVE WS-DOCTYPEINDIC        TO DocTypeIndic
                                       OF ReportingFI

           MOVE WS-DOCREFID            TO DocRefId
                                       OF ReportingFI

MCHA+-D    DISPLAY 'S-I-REF-FIC-INIT ='WS-I-REF-FIC-INIT
MCHA+-     IF WS-DOCREFID(1:7) = 'FATCA1-' OR 'FATCA11'
"             MOVE 0                   TO CorrMessageRefId--C
"                                      OF ReportingFI-COUNTERS
"                                         CorrDocRefId--C
"                                      OF ReportingFI-COUNTERS
MCHA+-     ELSE
"             MOVE 1                   TO CorrMessageRefId--C
"                                      OF ReportingFI-COUNTERS
"                                         CorrDocRefId--C
"                                      OF ReportingFI-COUNTERS
"             MOVE WS-I-REF-FIC-INIT
"                OF WS-G-DATA-TET-END  TO CorrMessageRefId
"                                      OF DocSpec
"                                      OF ReportingFI
"                                        (CorrMessageRefId--C
"                                      OF ReportingFI-COUNTERS)
"                                         CorrDocRefId
"                                      OF DocSpec
"                                      OF ReportingFI
"                                        (CorrDocRefId--C
"                                      OF ReportingFI-COUNTERS)
MCHA+-     END-IF

           .

      *-----------------------*
       ALIM-ARBR-ReportingGroup.
      *-----------------------*

      *    Balise ReportingGroup

           MOVE 0                      TO Sponsor--C
                                       OF ReportingGroup-COUNTERS

           MOVE 0                      TO Intermediary--C
                                       OF ReportingGroup-COUNTERS

           MOVE 0                      TO AccountReport--C
                                       OF ReportingGroup-COUNTERS

           MOVE 0                      TO PoolReport--C
                                       OF ReportingGroup-COUNTERS
           .

      *-------------------*
       GENER-XML-FATCA-OECD.
      *-------------------*

           MOVE ZEROS                  TO WS-Q-XML-GENERATE
           MOVE SPACE                  TO WS-L-XML-GENERATE

           XML
                GENERATE                  WS-L-XML-GENERATE
                FROM                      FATCA_OECD
                COUNT                  IN WS-Q-XML-GENERATE
MCHA  *         WITH  ENCODING 1208
MCHA  *         WITH  XML-DECLARATION
                ON EXCEPTION
                   EVALUATE XML-CODE

                      WHEN 400
                         MOVE 4010               TO WS-CODE-ABEND
                         MOVE SPACE              TO WS-LIGNE-ANO21
                         STRING 'TAILLE VARIABLE RECEVANTE DU XML'
                                ' GENERATE EST INSUFFISANTE'
                         DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
                         MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
                         MOVE  XML-CODE          TO WS-LIGNE-ANO42
                         PERFORM TRAIT-ABEND
                      WHEN 417
                         MOVE 4011               TO WS-CODE-ABEND
                         MOVE SPACE              TO WS-LIGNE-ANO21
                         STRING 'DONNEES A GENERER NON'
                                ' PERMISES (LOW-VALUE)'
                         DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
                         MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
                         MOVE  XML-CODE          TO WS-LIGNE-ANO42
      D                  DISPLAY SPACE
      D                  DISPLAY 'ABEND    : '      WS-CODE-ABEND
      D                  DISPLAY 'XML-CODE : '      XML-CODE
      D                  DISPLAY 'MESSAGE  : '      WS-LIGNE-ANO21
      D                  DISPLAY SPACE
                      WHEN OTHER
                         MOVE 4012               TO WS-CODE-ABEND
                         MOVE SPACE              TO WS-LIGNE-ANO21
                         STRING 'ERREUR LORS DU XML GENERATE '
                         DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
                         MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
                         MOVE  XML-CODE          TO WS-LIGNE-ANO42
                         PERFORM TRAIT-ABEND
                   END-EVALUATE

           END-XML

MCHA  *    DISPLAY 'MCHA ' WS-L-XML-GENERATE
      *    Alimenter les paramètres MFUSXL00
           PERFORM ALIM-XL00-FONC-IN-FATCA

      *    Appel au module MFUSXL00
           PERFORM APPL-XL00

           .

      *-----------------------*
       GENER-XML-AccountReport.
      *-----------------------*

      D    Display 'SubstantialOwner--C : ' SubstantialOwner--C
      D    Display 'R-Address--C Org    : ' R-Address--C OF
      D                                     Organisation-COUNTERS
      D    Display 'R-Address--C Ind    : ' R-Address--C OF
      D                                     Individual-COUNTERS
MCHA++D    Display 'Payment--C          : ' Payment--C

           MOVE ZEROS                  TO WS-Q-XML-GENERATE
           MOVE SPACE                  TO WS-L-XML-GENERATE

           XML
                GENERATE                  WS-L-XML-GENERATE
                FROM                      AccountReport
                                         (AccountReport--C)
                COUNT                  IN WS-Q-XML-GENERATE
MCHA  *         WITH  ENCODING 1208
MCHA  *         WITH  XML-DECLARATION
                ON EXCEPTION
                   EVALUATE XML-CODE

                      WHEN 400
                         MOVE 4010               TO WS-CODE-ABEND
                         MOVE SPACE              TO WS-LIGNE-ANO21
                         STRING 'TAILLE VARIABLE RECEVANTE DU XML'
                                ' GENERATE EST INSUFFISANTE'
                         DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
                         MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
                         MOVE  XML-CODE          TO WS-LIGNE-ANO42
                         PERFORM TRAIT-ABEND
                      WHEN 417
                         MOVE 4011               TO WS-CODE-ABEND
                         MOVE SPACE              TO WS-LIGNE-ANO21
                         STRING 'DONNEES A GENERER NON'
                                ' PERMISES (LOW-VALUE)'
                         DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
                         MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
                         MOVE  XML-CODE          TO WS-LIGNE-ANO42
      D                  DISPLAY SPACE
      D                  DISPLAY 'ABEND    : '      WS-CODE-ABEND
      D                  DISPLAY 'XML-CODE : '      XML-CODE
      D                  DISPLAY 'MESSAGE  : '      WS-LIGNE-ANO21
      D                  DISPLAY SPACE
                      WHEN OTHER
                         MOVE 4012               TO WS-CODE-ABEND
                         MOVE SPACE              TO WS-LIGNE-ANO21
                         STRING 'ERREUR LORS DU XML GENERATE '
                         DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
                         MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
                         MOVE  XML-CODE          TO WS-LIGNE-ANO42
                         PERFORM TRAIT-ABEND
                   END-EVALUATE

           END-XML

MCHA++D    Display 'Payment--C  aprés   : ' Payment--C
      *    Alimenter les paramètres MFUSXL00
           PERFORM ALIM-XL00-FONC-IN-ACCOUNT

      *    Appel au module MFUSXL00
           PERFORM APPL-XL00

           ADD 1 TO WS-Q-AccountReport
           .

150978*-----------------------*
150978 GENER-XML-PoolReport.
150978*-----------------------*
150978
150978     MOVE ZEROS                  TO WS-Q-XML-GENERATE
150978     MOVE SPACE                  TO WS-L-XML-GENERATE
150978
150978     XML
150978          GENERATE                  WS-L-XML-GENERATE
150978          FROM                      PoolReport
150978                                   (PoolReport--C)
150978          COUNT                  IN WS-Q-XML-GENERATE
150978          ON EXCEPTION
150978             EVALUATE XML-CODE
150978
150978                WHEN 400
150978                   MOVE 4010               TO WS-CODE-ABEND
150978                   MOVE SPACE              TO WS-LIGNE-ANO21
150978                   STRING 'TAILLE VARIABLE RECEVANTE DU XML'
150978                          ' GENERATE EST INSUFFISANTE'
150978                   DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
150978                   MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
150978                   MOVE  XML-CODE          TO WS-LIGNE-ANO42
150978                   PERFORM TRAIT-ABEND
150978                WHEN 417
150978                   MOVE 4011               TO WS-CODE-ABEND
150978                   MOVE SPACE              TO WS-LIGNE-ANO21
150978                   STRING 'DONNEES A GENERER NON'
150978                          ' PERMISES (LOW-VALUE)'
150978                   DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
150978                   MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
150978                   MOVE  XML-CODE          TO WS-LIGNE-ANO42
150978D                  DISPLAY SPACE
150978D                  DISPLAY 'ABEND    : '      WS-CODE-ABEND
150978D                  DISPLAY 'XML-CODE : '      XML-CODE
150978D                  DISPLAY 'MESSAGE  : '      WS-LIGNE-ANO21
150978D                  DISPLAY SPACE
150978                WHEN OTHER
150978                   MOVE 4012               TO WS-CODE-ABEND
150978                   MOVE SPACE              TO WS-LIGNE-ANO21
150978                   STRING 'ERREUR LORS DU XML GENERATE '
150978                   DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
150978                   MOVE 'XML-CODE='        TO WS-LIGNE-ANO41
150978                   MOVE  XML-CODE          TO WS-LIGNE-ANO42
150978                   PERFORM TRAIT-ABEND
150978             END-EVALUATE
150978
150978     END-XML
150978
150978*    Alimenter les paramètres MFUSXL00
150978     PERFORM ALIM-XL00-FONC-IN-ACCOUNT
150978
150978*    Appel au module MFUSXL00
150978     PERFORM APPL-XL00
150978
150978     ADD 1 TO WS-Q-PoolReport
150978     .

      *---------------------*
       DECOUP-XML-FATCA-OECD.
      *---------------------*

           MOVE WS-Q-XML-DCOP-INDT    TO WS-Q-INDT-INIT

      D    DISPLAY 'DECOUPAGE BATCH WS-N-XML-DCOP : '
      D                             WS-N-XML-DCOP

           IF WS-N-XML-DCOP = 0
              MOVE 4013               TO WS-CODE-ABEND
              MOVE SPACE              TO WS-LIGNE-ANO21
              STRING 'DECOUPAGE STRUCTURE XML ECHOUE'
              DELIMITED BY SIZE     INTO WS-LIGNE-ANO21
              MOVE SPACE              TO WS-LIGNE-ANO41
              MOVE SPACE              TO WS-LIGNE-ANO42
              PERFORM TRAIT-ABEND
           END-IF

           MOVE ZERO                  TO WS-Q-XML-TAB-FIN

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              IF I > WS-N-XML-DCOP

                 ADD  1                TO WS-Q-XML-TAB-FIN
                 MOVE WS-G-XML-TAB (I) TO WS-G-XML-TAB-FIN
                                         (WS-Q-XML-TAB-FIN)

                 MOVE SPACE            TO WS-L-XML-LINE(I)
                 MOVE ZEROS            TO WS-Q-XML-INDT(I)
                 MOVE ZEROS            TO WS-Q-XML-LINE(I)

              END-IF

           END-PERFORM

           MOVE WS-N-XML-DCOP          TO WS-Q-XML-TAB
           .

      *-----------------*
       ECRT-WS-G-XML.
      *-----------------*


           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              MOVE SPACE                 TO ENR-DFUSLS21
MCHA+1        IF WS-L-XML-LINE(I) (10:16) = 'CorrMessageRefId'
                 INSPECT WS-L-XML-LINE(I) REPLACING ALL 'ftc' BY 'sfa'
MCHA+1        END-IF
              MOVE WS-L-XML-LINE(I)
                  (1:WS-Q-XML-LINE(I))
                                         TO ENR-DFUSLS21
              PERFORM ECRT-FICH-S01

              MOVE SPACE                 TO WS-L-XML-LINE(I)

           END-PERFORM

           MOVE ZERO                     TO WS-Q-XML-TAB

           .

      *---------------------*
       ECRT-WS-G-XML-FIN.
      *---------------------*


           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB-FIN

              MOVE SPACE                 TO ENR-DFUSLS21
              MOVE WS-L-XML-LINE-FIN(I)
                  (1:WS-Q-XML-LINE-FIN(I))
                                         TO ENR-DFUSLS21
              PERFORM ECRT-FICH-S01

              MOVE SPACE                 TO WS-L-XML-LINE-FIN(I)

           END-PERFORM

           MOVE ZERO                     TO WS-Q-XML-TAB-FIN

           .

      *------------------*
       ALIM-WS-G-DATA-DET.
      *------------------*

           EVALUATE E01-C-ENTIT

              WHEN 'CNT'

      *          Initialiser donnees WS détail CNT
                 PERFORM INIT-WS-G-DATA-DET-CNT

      *          Alimenter données détails compte
                 PERFORM ALIM-DATA-WS-DET-CNT

              WHEN 'CLI'

      *          Initialiser donnees WS détail CLI, ADR ET BNF
                 PERFORM INIT-WS-G-DATA-DET-CLT

      *          Alimenter données détails Client
                 PERFORM ALIM-DATA-WS-DET-CLT

              WHEN 'ADR'

      *          Alimenter données détails adresse titulaire du compte
                 PERFORM ALIM-DATA-WS-DET-ADR

              WHEN 'BNF'

      *          Alimenter données détails bénéficiaire du compte
                 PERFORM ALIM-DATA-WS-DET-BNF

              WHEN OTHER

                 MOVE 3017               TO WS-CODE-ABEND
                 PERFORM TRAIT-ABEND

           END-EVALUATE
           .


      *------------------*
       TRAIT-RUPT-CNT-CLI.
      *------------------*

      *    Gestion du rupture sur le compte et client

      D    DISPLAY 'E01-C-ENR           '  E01-C-ENR
      D    DISPLAY 'E01-C-ENTIT         '  E01-C-ENTIT
      D    DISPLAY 'WS-I-UNIQ-KAC-RUPT  '  WS-I-UNIQ-KAC-RUPT
      D    DISPLAY 'WS-I-UNIQ-KPI-RUPT  '  WS-I-UNIQ-KPI-RUPT

           SET  RUPTURE-CNT-CLI-NON    TO TRUE

150978     IF    E01-C-ENR = '20'
           AND ( E01-C-ENTIT ='CNT' OR 'CLI')

      *       si les vriables de la rupture ont déjà été valorisées
              IF  WS-I-UNIQ-KAC-RUPT NOT = SPACE
              AND WS-I-UNIQ-KPI-RUPT NOT = SPACE

      *          Si rupture sur client
                 IF  E01-C-ENTIT ='CLI'
                 AND WS-I-UNIQ-KPI-RUPT NOT = E01-I-UNIQ-KPI
                                         OF   E01-G-DATA-DET-CLT

      D             DISPLAY 'E01-I-UNIQ-KPI      '  E01-I-UNIQ-KPI
      D                                        OF   E01-G-DATA-DET-CLT
                    SET  RUPTURE-CNT-CLI     TO TRUE

                 ELSE
      *             Si rupture sur compte , initialiser client
                    IF  E01-C-ENTIT ='CNT'
                    AND WS-I-UNIQ-KAC-RUPT NOT = E01-I-UNIQ-KAC

      D                DISPLAY 'E01-I-UNIQ-KAC      '  E01-I-UNIQ-KAC
                       SET  RUPTURE-CNT-CLI  TO TRUE
                       MOVE SPACE            TO WS-I-UNIQ-KPI-RUPT

                    END-IF
                 END-IF

              END-IF

      *       Evaluer entité traitée
              EVALUATE E01-C-ENTIT

                 WHEN 'CNT'

                    MOVE E01-I-UNIQ-KAC    TO WS-I-UNIQ-KAC-RUPT

                 WHEN 'CLI'

                    MOVE E01-I-UNIQ-KPI
                      OF E01-G-DATA-DET-CLT
                                           TO WS-I-UNIQ-KPI-RUPT

                 WHEN OTHER

                    CONTINUE

              END-EVALUATE

           END-IF

150978     IF E01-C-ENR = '90' OR '30'

              IF  WS-I-UNIQ-KAC-RUPT NOT = SPACE
              AND WS-I-UNIQ-KPI-RUPT NOT = SPACE

                 SET RUPTURE-CNT-CLI TO TRUE

              END-IF

           END-IF
           .



      *-------------------------*
       INIT-ARBR-AccountReport.
      *-------------------------*

      *    Initialiser contenu Arbre AccountReport
           MOVE 1                      TO AccountReport--C

      *    DocSpec
           MOVE SPACE                  TO DocTypeIndic
                                       OF DocSpec
                                       OF AccountReport
                                         (AccountReport--C)

           MOVE SPACE                  TO DocRefId
                                       OF DocSpec
                                       OF AccountReport
                                         (AccountReport--C)

           MOVE ZEROS                  TO CorrMessageRefId--C
                                       OF AccountReport-COUNTERS

           MOVE ZEROS                  TO CorrDocRefId--C
                                       OF AccountReport-COUNTERS
      *    AccountNumber
           MOVE SPACE                  TO AccountNumber
                                       OF AccountReport
                                         (AccountReport--C)
      *    AccountHolder
           MOVE SPACE                  TO AccountNumber
                                       OF AccountReport
                                         (AccountReport--C)

      *    AcctHolderType
           MOVE ZEROS                  TO AcctHolderType--C
                                       OF AccountReport-COUNTERS

      *    SubstantialOwner
           MOVE SPACE                  TO SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C)

           MOVE ZEROS                  TO SubstantialOwner--C
                                       OF AccountReport-COUNTERS

      *    AccountBalance
           MOVE SPACE                  TO AccountBalance
                                       OF AccountReport
                                         (AccountReport--C)

           MOVE ZEROS                  TO tech-text
                                       OF AccountBalance
                                       OF AccountReport
                                         (AccountReport--C)
      *    Payment
           MOVE ZEROS                  TO Payment--C
                                       OF AccountReport-COUNTERS
           .


      *-----------------------*
       ALIM-ARBR-AccountReport.
      *-----------------------*

      *    Alimenter données compte Arbre AccountReport

      D    DISPLAY 'Debut alimentation AccountReport'
      D    DISPLAY 'Compte : ' WS-I-UNIQ-KAC
      D    DISPLAY 'Client : ' WS-I-UNIQ-KPI OF WS-G-DATA-DET-CLT
      D    DISPLAY '         '

           MOVE 1                      TO AccountReport--C

      D    DISPLAY 'AccountReport--C : '  AccountReport--C

      * DocSpec

           MOVE WS-DOCTYPEINDIC        TO DocTypeIndic
                                       OF DocSpec
                                       OF AccountReport
                                         (AccountReport--C)

      D    DISPLAY 'DocTypeIndic : '      DocTypeIndic
      D                                OF DocSpec
      D                                OF AccountReport
      D                                  (AccountReport--C)

150978*    MOVE WS-DOCREFID            TO DocRefId
150978D    DISPLAY 'WS-I-REF-BLOC :' WS-I-REF-BLOC OF WS-G-DATA-DET-CNT
150978     MOVE WS-I-REF-BLOC OF WS-G-DATA-DET-CNT
150978                                 TO DocRefId
                                       OF DocSpec
                                       OF AccountReport
                                         (AccountReport--C)

      D    Display 'DocRefId : '          DocRefId
      D                                OF DocSpec
      D                                OF AccountReport
      D                                  (AccountReport--C)
MCHA+-     IF WS-DOCREFID(1:7) = 'FATCA1-' OR 'FATCA11'
"             MOVE ZEROS               TO CorrMessageRefId--C
"                                      OF AccountReport-COUNTERS
"                                         CorrDocRefId--C
"                                      OF AccountReport-COUNTERS
"          ELSE
"             MOVE 1                   TO CorrMessageRefId--C
"                                      OF AccountReport-COUNTERS
"                                         CorrDocRefId--C
"                                      OF AccountReport-COUNTERS
"
MC            MOVE WS-I-REF-FIC-INIT   OF WS-G-DATA-TET-END
MC    *       MOVE WS-I-REF-FID-INIT   OF WS-G-DATA-DET-CNT
MC                                     TO CorrMessageRefId
"                                      OF DocSpec
"                                      OF AccountReport
"                                        (AccountReport--C
"                                         CorrMessageRefId--C
"                                      OF AccountReport-COUNTERS)
MC            MOVE WS-I-REF-BLOC-INIT  TO CorrDocRefId
"                                      OF DocSpec
"                                      OF AccountReport
"                                        (AccountReport--C
"                                         CorrDocRefId--C
"                                      OF AccountReport-COUNTERS)
MCHA+-     END-IF
      D    DISPLAY 'CorrDocRefId--C :'    CorrDocRefId--C
      D                                OF AccountReport-COUNTERS

      * AccountNumber

      *    MOVE WS-I-UNIQ-KAC
MCHA+ D    DISPLAY ' AccountNumber ' WS-I-KAC-IBAN
MCHA+      MOVE WS-I-KAC-IBAN
MCHA+        OF WS-G-DATA-DET-CNT      TO AccountNumber
                                       OF AccountReport
                                         (AccountReport--C)

      D    Display 'AccountNumber : '
      D                                   AccountNumber
      D                                OF AccountReport
      D                                  (AccountReport--C)
      * AccountHolder

      D    Display 'WS-C-NTUR-PERS: '     WS-C-NTUR-PERS
      D                                OF WS-G-DATA-DET-CLT

           EVALUATE WS-C-NTUR-PERS
                 OF WS-G-DATA-DET-CLT

              WHEN '01'

      * Individual

                 MOVE 1                TO Individual--C
                 MOVE 0                TO Organisation--C

                 MOVE 1
                                       TO ResCountryCode--C
                                       OF Individual-COUNTERS

SBOU  *          MOVE WS-C-PAYS-NAISS
SBOU  *            OF WS-G-DATA-DET-CLT
SBOU             MOVE SPACES
                                       TO ResCountryCode
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          ResCountryCode--C
                                       OF Individual-COUNTERS)
      * TIN
                 MOVE 1                TO TIN--C
                                       OF Individual-COUNTERS

                    MOVE WS-C-REF-GIIN
                         OF WS-G-DATA-DET-CLT
                                       TO tech-text
                                       OF TIN
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          TIN--C
                                       OF Individual-COUNTERS)
MCHA+            IF WS-C-REF-GIIN OF WS-G-DATA-DET-CLT = SPACES
MCHA+               OR WS-C-REF-GIIN OF WS-G-DATA-DET-CLT = LOW-VALUE
"                   MOVE SPACES
MCHA+                    TO tech-attr-opt-issuedBy
                                       OF TIN
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          TIN--C
                                       OF Individual-COUNTERS)
MCHA+            ELSE
MCHA+               MOVE WS-C-PAYS-EMET-GIN OF WS-G-DATA-TET-END
"                        TO tech-attr-opt-issuedBy
"                                      OF TIN
"                                      OF Individual
"                                        (AccountReport--C
"                                         Individual--C
"                                         TIN--C
"                                      OF Individual-COUNTERS)
MCHA+            END-IF
      * Name

                 MOVE 1                TO Name--C
                                       OF Individual-COUNTERS

                 MOVE 0                TO PrecedingTitle--C
                                       OF Individual-COUNTERS

                 MOVE 1                TO R-Title--C
                                       OF Individual-COUNTERS

                 MOVE WS-C-TITRE-CVLTE
                   OF WS-G-DATA-DET-CLT
                                       TO R-Title
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Name--C
                                       OF Individual-COUNTERS
                                          R-Title--C
                                       OF Individual-COUNTERS)

                 MOVE WS-L-PRENOM
                   OF WS-G-DATA-DET-CLT
                                       TO tech-text
                                       OF FirstName
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Name--C
                                       OF Individual-COUNTERS)

                 MOVE SPACE
                                       TO tech-attr-non-xnlNameType
                                       OF FirstName
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Name--C
                                       OF Individual-COUNTERS)


                 MOVE 0                TO MiddleName--C
                                       OF Individual-COUNTERS

                 MOVE 0                TO NamePrefix--C
                                       OF Individual-COUNTERS

                 MOVE WS-L-NOM-NAISS
                   OF WS-G-DATA-DET-CLT
                                       TO tech-text
                                       OF LastName
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Name--C
                                       OF Individual-COUNTERS)

                 MOVE SPACE
                                       TO tech-attr-non-xnlNameType
                                       OF LastName
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Name--C
                                       OF Individual-COUNTERS)

                 MOVE 0                TO GenerationIdentifier--C
                                       OF Individual-COUNTERS

                 MOVE 0                TO Suffix--C
                                       OF Individual-COUNTERS

                 MOVE 0                TO GeneralSuffix--C
                                       OF Individual-COUNTERS
      * Address
                 MOVE ZERO             TO R-Address--C
                                       OF Individual-COUNTERS

      D          DISPLAY 'Nombre d''adresse du client PP :'
      D                                   WS-Q-DATA-DET-ADR

                 PERFORM VARYING I FROM 1 BY 1
                 UNTIL I > WS-Q-DATA-DET-ADR

                    ADD  1             TO R-Address--C
                                       OF Individual-COUNTERS

                    MOVE SPACE         TO tech-attr-opt-legalAddressType
                                       OF R-Address
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          R-Address--C
                                       OF Individual-COUNTERS)

                    MOVE WS-C-PAYS-ADR
                        OF WS-G-DATA-DET-ADR(I)
                                       TO CountryCode
                                       OF R-Address
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          R-Address--C
                                       OF Individual-COUNTERS)

                    MOVE 0             TO AddressFree2--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 1             TO AddressFix--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO Street--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO SuiteIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO FloorIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO DistrictName--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 0             TO POB--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 1             TO PostCode--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE WS-C-CPOST
                      OF WS-G-DATA-DET-ADR(I)
                                       TO PostCode
                                       OF R-Address
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          R-Address--C
                                       OF Individual-COUNTERS
                                          AddressFix--C
                                       OF Individual-COUNTERS
                                          PostCode--C
                                       OF Individual-COUNTERS)


                    MOVE WS-L-COMM-ADR
                      OF WS-G-DATA-DET-ADR(I)
                                       TO City
                                       OF R-Address
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          R-Address--C
                                       OF Individual-COUNTERS
                                          AddressFix--C
                                       OF Individual-COUNTERS)


                    MOVE 0             TO CountrySubentity--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE 1             TO AddressFree--C
                                       OF R-Address-COUNTERS
                                       OF Individual-COUNTERS

                    MOVE SPACE         TO AddressFree
                                       OF R-Address
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          R-Address--C
                                       OF Individual-COUNTERS
                                          AddressFree--C
                                       OF Individual-COUNTERS)


                    move 1                      to ws-b-string-sep
                    move 6                      to ws-q-string-tab
                    MOVE WS-L-INTIT-COURR-1
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (1)
                    MOVE WS-L-INTIT-COURR-2
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (2)
                    MOVE WS-L-ADR-LIGNE-1
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (3)
                    MOVE WS-L-ADR-LIGNE-2
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (4)
                    MOVE WS-L-ADR-LIGNE-3
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (5)
                    MOVE WS-L-PAYS-ADR
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (6)

                    perform trait-string

                    move ws-l-string-out(1:ws-q-string-out)
                                                TO AddressFree
                                                OF R-Address
                                                OF Individual
                                                  (AccountReport--C
                                                   Individual--C
                                                   R-Address--C
                                                OF Individual-COUNTERS
                                                   AddressFree--C
                                                OF Individual-COUNTERS)
                 END-PERFORM

      * Nationality

                 MOVE 1                TO Nationality--C
                                       OF Individual-COUNTERS

                 MOVE WS-C-PAYS-NLITE
                   OF WS-G-DATA-DET-CLT
                                       TO Nationality
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Nationality--C
                                       OF Individual-COUNTERS)

                 MOVE 2                TO Nationality--C
                                       OF Individual-COUNTERS

                 MOVE WS-C-AUTRE-PAYS-NLITE
                   OF WS-G-DATA-DET-CLT
                                       TO Nationality
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          Nationality--C
                                       OF Individual-COUNTERS)
      * BirthInfo

                 MOVE 1                TO BirthInfo--C
                                       OF Individual-COUNTERS

                 MOVE 1                TO BirthDate--C
                                       OF Individual-COUNTERS

                 MOVE WS-D-NAISS
                   OF WS-G-DATA-DET-CLT
                                       TO BirthDate
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          BirthInfo--C
                                       OF Individual-COUNTERS
                                          BirthDate--C
                                       OF Individual-COUNTERS)

                 MOVE 1                TO City--C
                                       OF Individual-COUNTERS

                 MOVE WS-L-VILL-NAISS
                   OF WS-G-DATA-DET-CLT
                                       TO City
                                       OF BirthInfo
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          BirthInfo--C
                                       OF Individual-COUNTERS
                                          City--C
                                       OF Individual-COUNTERS)

                 MOVE 1                TO CitySubentity--C
                                       OF Individual-COUNTERS

                 MOVE WS-C-DEPT-NAISS
                   OF WS-G-DATA-DET-CLT
                                       TO CitySubentity
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          BirthInfo--C
                                       OF Individual-COUNTERS
                                          CitySubentity--C
                                       OF Individual-COUNTERS)

                 MOVE 1                TO CountryInfo--C
                                       OF Individual-COUNTERS

                 MOVE 1                TO CountryCode--C
                                       OF Individual-COUNTERS

                 MOVE WS-C-PAYS-NAISS
                   OF WS-G-DATA-DET-CLT
                                       TO CountryCode
                                       OF CountryInfo
                                       OF Individual
                                         (AccountReport--C
                                          Individual--C
                                          BirthInfo--C
                                       OF Individual-COUNTERS
                                          CountryInfo--C
                                       OF Individual-COUNTERS
                                          CountryCode--C
                                       OF Individual-COUNTERS)

                 MOVE 0                TO FormerCountryName--C
                                       OF Individual-COUNTERS

      * AcctHolderType

                 MOVE 0                TO AcctHolderType--C


              WHEN '02'

      * Organisation

                 MOVE 0                TO Individual--C
                 MOVE 1                TO Organisation--C

                 MOVE 0
                                       TO ResCountryCode--C
                                       OF Organisation-COUNTERS
      *          MOVE WS-C-PAYS-ADR
      *            OF WS-G-DATA-DET-ADR
      *                                TO ResCountryCode
      *                                OF Organisation
      *                                  (AccountReport--C
      *                                   Organisation--C
      *                                   ResCountryCode--C
      *                                OF Organisation-COUNTERS)

      * TIN
                 MOVE 1                TO TIN--C
                                       OF Organisation-COUNTERS

                 MOVE WS-C-REF-GIIN
                   OF WS-G-DATA-DET-CLT
                                       TO tech-text
                                       OF TIN
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          TIN--C
                                       OF Organisation-COUNTERS)
MCHA  *
150978*          MOVE WS-C-PAYS-FISCL-ETR
150978*            OF WS-G-DATA-DET-CLT TO tech-attr-opt-issuedBy
MCHA+            IF WS-C-REF-GIIN OF WS-G-DATA-DET-CLT = SPACES
MCHA+               OR WS-C-REF-GIIN OF WS-G-DATA-DET-CLT = LOW-VALUE
MCHA+               MOVE SPACES        TO tech-attr-opt-issuedBy
                                       OF TIN
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          TIN--C
                                       OF Organisation-COUNTERS)
                 ELSE
MCHA+               MOVE WS-C-PAYS-EMET-GIN
MCHA+                  OF WS-G-DATA-TET-END   TO tech-attr-opt-issuedBy
                                       OF TIN
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          TIN--C
                                       OF Organisation-COUNTERS)
                 END-IF
      * Name
                 MOVE 1                TO Name--C
                                       OF Organisation-COUNTERS

                 MOVE WS-L-RAIS-SOCIALE
                   OF WS-G-DATA-DET-CLT
                                       TO tech-text
                                       OF Name
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          Name--C
                                       OF Organisation-COUNTERS)

                 MOVE SPACE            TO tech-attr-opt-nameType
                                       OF Name
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          Name--C
                                       OF Organisation-COUNTERS)

      * Address

                 MOVE ZERO             TO R-Address--C
                                       OF Organisation-COUNTERS

      D          DISPLAY 'Nombre d''adresse du client PM : '
      D                                   WS-Q-DATA-DET-ADR

                 PERFORM VARYING I FROM 1 BY 1
                 UNTIL I > WS-Q-DATA-DET-ADR

                    ADD  1             TO R-Address--C
                                       OF Organisation-COUNTERS

                    MOVE SPACE         TO tech-attr-opt-legalAddressType
                                       OF R-Address
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          R-Address--C
                                       OF Organisation-COUNTERS)

                    MOVE WS-C-PAYS-ADR
                      OF WS-G-DATA-DET-ADR(I)
                                       TO CountryCode
                                       OF R-Address
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          R-Address--C
                                       OF Organisation-COUNTERS)

                    MOVE 0             TO AddressFree2--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 1             TO AddressFix--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO Street--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO SuiteIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO FloorIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO DistrictName--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 0             TO POB--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 1             TO PostCode--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE WS-C-CPOST
                      OF WS-G-DATA-DET-ADR(I)
                                       TO PostCode
                                       OF R-Address
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          R-Address--C
                                       OF Organisation-COUNTERS
                                          AddressFix--C
                                       OF Organisation-COUNTERS
                                          PostCode--C
                                       OF Organisation-COUNTERS)


                    MOVE WS-L-COMM-ADR
                      OF WS-G-DATA-DET-ADR(I)
                                       TO City
                                       OF R-Address
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          R-Address--C
                                       OF Organisation-COUNTERS
                                          AddressFix--C
                                       OF Organisation-COUNTERS)


                    MOVE 0             TO CountrySubentity--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE 1             TO AddressFree--C
                                       OF R-Address-COUNTERS
                                       OF Organisation-COUNTERS

                    MOVE SPACE         TO AddressFree
                                       OF R-Address
                                       OF Organisation
                                         (AccountReport--C
                                          Organisation--C
                                          R-Address--C
                                       OF Organisation-COUNTERS
                                          AddressFree--C
                                       OF Organisation-COUNTERS)


                    move 1                      to ws-b-string-sep
                    move 6                      to ws-q-string-tab
                    MOVE WS-L-INTIT-COURR-1
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (1)
                    MOVE WS-L-INTIT-COURR-2
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (2)
                    MOVE WS-L-ADR-LIGNE-1
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (3)
                    MOVE WS-L-ADR-LIGNE-2
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (4)
                    MOVE WS-L-ADR-LIGNE-3
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (5)
                    MOVE WS-L-PAYS-ADR
                      OF WS-G-DATA-DET-ADR(I)   TO ws-l-string-in (6)

                    perform trait-string

                    move ws-l-string-out(1:ws-q-string-out)
                                                TO AddressFree
                                                OF R-Address
                                                OF Organisation
                                                  (AccountReport--C
                                                   Organisation--C
                                                   R-Address--C
                                                OF Organisation-COUNTERS
                                                   AddressFree--C
                                               OF Organisation-COUNTERS)
                 END-PERFORM

      * AcctHolderType
                 MOVE 1                TO AcctHolderType--C

MCHA- *          en fonction de quelle information ?
MCHA+            MOVE WS-C-TYPE-CLASS  TO AcctHolderType
                                         (AccountReport--C
                                          AcctHolderType--C)
      * SubstantialOwner

      D          DISPLAY 'Nombre de beneficiaires        : '
      D                                   WS-Q-DATA-DET-BNF

                 MOVE ZERO             TO SubstantialOwner--C

                 PERFORM VARYING I FROM 1 BY 1
                 UNTIL I > WS-Q-DATA-DET-BNF


                    ADD 1              TO SubstantialOwner--C


      * ResCountryCode

                    MOVE  1            TO ResCountryCode--C
                                       OF SubstantialOwner-COUNTERS

SBOU  *             MOVE WS-C-PAYS-ADR-BNF
SBOU  *               OF WS-G-DATA-DET-BNF(I)
SBOU                MOVE SPACES
                                       TO ResCountryCode
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          ResCountryCode--C
                                       OF SubstantialOwner-COUNTERS)
      * TIN

                    MOVE 1             TO TIN--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE WS-C-REF-GIIN-BNF
                      OF WS-G-DATA-DET-BNF(I)
                                       TO tech-text
                                       OF TIN
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          TIN--C
                                       OF SubstantialOwner-COUNTERS)

MCHA+               IF WS-C-REF-GIIN-BNF
"                     OF WS-G-DATA-DET-BNF(I) = SPACES OR LOW-VALUE
"                      MOVE SPACE         TO tech-attr-opt-issuedBy
"                                      OF TIN
"                                      OF SubstantialOwner
"                                        (AccountReport--C
"                                         SubstantialOwner--C
"                                         TIN--C
"                                      OF SubstantialOwner-COUNTERS)
"                   ELSE
"                      MOVE WS-C-PAYS-EMET-GIN
"                      OF WS-G-DATA-TET-END   TO tech-attr-opt-issuedBy
"                                      OF TIN
"                                      OF SubstantialOwner
"                                        (AccountReport--C
"                                         SubstantialOwner--C
"                                         TIN--C
"                                      OF SubstantialOwner-COUNTERS)
MCHA+               END-IF
      * Name

                    MOVE 1             TO Name--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO PrecedingTitle--C
                                       OF SubstantialOwner-COUNTERS

SBOU  *             MOVE 0             TO R-Title--C
SBOU  *                                OF SubstantialOwner-COUNTERS

SBOU                MOVE 1             TO R-Title--C
SBOU                                   OF SubstantialOwner-COUNTERS
SBOU
MCHA+               MOVE WS-C-TITRE-CVLTE-F OF WS-G-DATA-DET-BNF(I)
MCHA+                                  TO R-Title
SBOU                                   OF SubstantialOwner
SBOU                                     (AccountReport--C
SBOU                                      SubstantialOwner--C
SBOU                                      Name--C
SBOU                                   OF SubstantialOwner-COUNTERS
SBOU                                      R-Title--C
SBOU                                   OF SubstantialOwner-COUNTERS)


                    MOVE WS-PRENOM-BNF OF WS-G-DATA-DET-BNF(I)
                                       TO tech-text
                                       OF FirstName
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          Name--C
                                       OF SubstantialOwner-COUNTERS)
SBOU
SBOU                MOVE SPACE
SBOU                                   TO tech-attr-non-xnlNameType
SBOU                                   OF FirstName
SBOU                                   OF SubstantialOwner
SBOU                                     (AccountReport--C
SBOU                                      SubstantialOwner--C
SBOU                                      Name--C
SBOU                                   OF SubstantialOwner-COUNTERS)

                    MOVE 0             TO MiddleName--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO NamePrefix--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE WS-L-NOM-NAISS-BNF
                      OF WS-G-DATA-DET-BNF(I)
                                       TO tech-text
                                       OF LastName
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          Name--C
                                       OF SubstantialOwner-COUNTERS)

                    MOVE SPACE
                                       TO tech-attr-non-xnlNameType
                                       OF LastName
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          Name--C
                                       OF SubstantialOwner-COUNTERS)

                    MOVE 0             TO GenerationIdentifier--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO Suffix--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO GeneralSuffix--C
                                       OF SubstantialOwner-COUNTERS

      * Address

                    MOVE 1             TO R-Address--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE SPACE         TO tech-attr-opt-legalAddressType
                                       OF R-Address
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          R-Address--C
                                       OF SubstantialOwner-COUNTERS)

                    MOVE WS-C-PAYS-ADR-BNF
                      OF WS-G-DATA-DET-BNF(I)
                                       TO CountryCode
                                       OF R-Address
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          R-Address--C
                                       OF SubstantialOwner-COUNTERS)

                    MOVE 0             TO AddressFree2--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 1             TO AddressFix--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO Street--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO BuildingIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO SuiteIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO FloorIdentifier--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO DistrictName--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO POB--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO PostCode--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE WS-L-COMM-ADR-BNF
                      OF WS-G-DATA-DET-BNF(I)
                                       TO City
                                       OF R-Address
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          R-Address--C
                                       OF SubstantialOwner-COUNTERS
                                          AddressFix--C
                                       OF SubstantialOwner-COUNTERS)


                    MOVE 0             TO CountrySubentity--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO AddressFree--C
                                       OF R-Address-COUNTERS
                                       OF SubstantialOwner-COUNTERS
      * Nationality

                    MOVE 0             TO Nationality--C
                                       OF SubstantialOwner-COUNTERS


                    MOVE 1             TO BirthInfo--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 1             TO BirthDate--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE WS-D-NAISS-BNF
                      OF WS-G-DATA-DET-BNF(I)
                                       TO BirthDate
                                       OF SubstantialOwner
                                         (AccountReport--C
                                          SubstantialOwner--C
                                          BirthInfo--C
                                       OF SubstantialOwner-COUNTERS
                                          BirthDate--C
                                       OF SubstantialOwner-COUNTERS)

                    MOVE 0             TO City--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO CitySubentity--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO CountryInfo--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO CountryCode--C
                                       OF SubstantialOwner-COUNTERS

                    MOVE 0             TO FormerCountryName--C
                                       OF SubstantialOwner-COUNTERS
                 END-PERFORM

              WHEN OTHER

      *          Abend
                 MOVE 4050             TO  WS-CODE-ABEND
                 PERFORM TRAIT-ABEND

           END-EVALUATE


      * AccountBalance

           MOVE WS-C-DEV
             OF WS-G-DATA-DET-CNT      TO tech-attr-req-currCode
                                       OF AccountBalance
                                       OF AccountReport
                                         (AccountReport--C)
           COMPUTE tech-text
                OF AccountBalance
                OF AccountReport (AccountReport--C)
                   = WS-M-MNT-ASS-SLD / 10 ** WS-Q-NBR-DEC-SLD

      *    Payment
           MOVE ZERO                   TO Payment--C
                                       OF AccountReport-COUNTERS
           MOVE ZERO                TO PaymentAmnt--C OF Payment-Amnt
MCHA++*    MOVE ZERO                 TO I
"     *    DISPLAY 'WS-Q-LIST-MNT ' WS-Q-LIST-MNT
"     *    PERFORM VARYING L FROM 1 BY 1
"     *    UNTIL L > WS-Q-LIST-MNT
"     *       DISPLAY 'L           : ' L
"     *       DISPLAY 'MCHA LIST-MNT ' WS-M-MNT-ASS OF WS-T-LIST-MNT(L)
" *
"     *       ADD  1                   TO Payment--C
"     *       MOVE 1                   TO PaymentAmnt--C OF Payment-Amnt
" *
"     *       MOVE WS-C-TYP-MNT
"     *         OF WS-T-LIST-MNT(L)
"     *                          TO R-Type
"     *                          OF Payment
"     *                            (AccountReport--C
"     *                             Payment--C
"     *                          OF AccountReport-COUNTERS)
"     *        MOVE 'EUR'
"     *             TO tech-attr-req-currcode
"     *                OF PaymentAmnt
"     *                OF Payment (AccountReport--C
"     *                           Payment--C OF AccountReport-COUNTERS
"     *                           PaymentAmnt--C OF Payment-Amnt )
"     *
"     *        COMPUTE tech-text
"     *           OF PaymentAmnt
"     *           OF Payment (AccountReport--C
"     *                       Payment--C OF AccountReport-COUNTERS)
"     *           = WS-M-MNT-ASS OF WS-T-LIST-MNT(L)
"     *           / 10 ** WS-Q-NBR-DEC  OF WS-T-LIST-MNT(L)
"     *       DISPLAY 'MCHA WS-M-MNT-ASS ' tech-text OF PaymentAmnt
"     *           OF Payment (AccountReport--C
"     *                       Payment--C OF AccountReport-COUNTERS
"     *                           PaymentAmnt--C OF  Payment-Amnt)
"     *       MOVE ZEROS           TO      tech-text OF PaymentAmnt
"     *           OF Payment (AccountReport--C
"     *                       Payment--C OF AccountReport-COUNTERS
"     *                           PaymentAmnt--C OF Payment-Amnt)
MCHA+ *    END-PERFORM

      D    DISPLAY 'Fin   alimentation AccountReport'
           .

150978*--------------------*
150978 INIT-ARBR-PoolReport.
150978*--------------------*
150978
150978*    Initialiser contenu Arbre PoolReport
150978     MOVE 1                      TO PoolReport--C
150978
150978*    DocSpec
150978     MOVE SPACE                  TO DocTypeIndic
150978                                 OF DocSpec
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978
150978     MOVE SPACE                  TO DocRefId
150978                                 OF DocSpec
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978
150978     MOVE ZEROS                  TO CorrMessageRefId--C
150978                                 OF PoolReport-COUNTERS
150978
150978     MOVE ZEROS                  TO CorrDocRefId--C
150978                                 OF PoolReport-COUNTERS
150978*    AccountCount
150978     MOVE ZEROS                  TO AccountCount
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978*    AccountPoolReportType
150978     MOVE SPACE                  TO AccountPoolReportType
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978
150978*    Devise du solde
150978     MOVE ZEROS                  TO tech-attr-req-currCode
150978                                 OF PoolBalance
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978
150978*    Montant du solde
150978     MOVE ZEROS                  TO tech-text
150978                                 OF PoolBalance
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978     .
150978
150978*--------------------*
150978 ALIM-ARBR-PoolReport.
150978*--------------------*
150978
150978*    Alimenter données compte Arbre PoolReport
150978
150978D    DISPLAY 'Debut alimentation PoolReport'
150978D    DISPLAY '         '
150978
150978     MOVE 1                      TO PoolReport--C
150978
150978D    DISPLAY 'PoolReport--C : '     PoolReport--C
150978
150978* DocSpec
150978
150978     MOVE WS-DOCTYPEINDIC        TO DocTypeIndic
150978                                 OF DocSpec
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978
150978D    DISPLAY 'DocTypeIndic : '      DocTypeIndic
150978D                                OF DocSpec
150978D                                OF PoolReport
150978D                                  (PoolReport--C)
150978
150978*    MOVE WS-DOCREFID            TO DocRefId
150978     MOVE E01-C-REF-BLOC-RECAL OF E01-G-DATA-INF-CLR
150978                                 TO DocRefId
150978                                 OF DocSpec
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978
150978D    Display 'DocRefId : '          DocRefId
150978D                                OF DocSpec
150978D                                OF PoolReport
150978D                                  (PoolReport--C)
150978
150978     MOVE ZEROS                  TO CorrMessageRefId--C
150978                                 OF PoolReport-COUNTERS
150978
150978D    DISPLAY 'CorrMessageRefId--C : '
150978D                                   CorrMessageRefId--C
150978D                                OF PoolReport-COUNTERS
150978
MCHA+-*    IF E01-C-REF-BLOC-RLC-INIT = SPACES OR LOW-VALUE
MCHA+-     IF WS-DOCREFID(1:7) = 'FATCA1-' OR 'FATCA11'
150978        MOVE ZEROS                  TO CorrDocRefId--C
150978                                    OF PoolReport-COUNTERS
MCHA11     ELSE
150978        MOVE 1                      TO CorrDocRefId--C
150978                                    OF PoolReport-COUNTERS
150978        MOVE 1                      TO CorrMessageRefId--C
150978                                    OF PoolReport-COUNTERS
150978        MOVE E01-C-REF-BLOC-RLC-INIT
"                                      TO CorrDocRefId
"                                      OF DocSpec
"                                      OF PoolReport
"                                        (PoolReport--C
"                                         CorrDocRefId--C
"                                      OF PoolReport-COUNTERS )
MC            MOVE WS-I-REF-FIC-INIT   OF WS-G-DATA-TET-END
"                                      TO CorrMessageRefId
"                                      OF DocSpec
"                                      OF PoolReport
"                                        (PoolReport--C
"                                         CorrMessageRefId--C
"                                      OF PoolReport-COUNTERS )
MCHA+-     END-IF
150978
150978D    DISPLAY 'CorrDocRefId--C :'    CorrDocRefId--C
150978D                                OF PoolReport-COUNTERS
150978*    AccountCount
150978*    DISPLAY 'E01-Q-NBR-CPT   :'    E01-Q-NBR-CPT
150978     MOVE E01-Q-NBR-CPT          OF E01-G-DATA-INF-CLR
150978                                 TO AccountCount
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978D    DISPLAY 'AccountCount :'       AccountCount
150978D                                OF PoolReport
150978D                                  (PoolReport--C)
150978*    AccountPoolReportType
150978*    DISPLAY 'E01-C-TYPE-RECAL :'    E01-C-TYPE-RECAL
150978     MOVE E01-C-TYPE-RECAL       OF E01-G-DATA-INF-CLR
150978                                 TO AccountPoolReportType
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978D    DISPLAY 'AccountPoolReportType :' AccountPoolReportType
150978D                                OF PoolReport
150978D                                  (PoolReport--C)
150978
150978*    Devise du solde
150978*    DISPLAY 'E01-C-DEV :'   E01-C-DEV OF E01-G-DATA-INF-CLR
150978     MOVE E01-C-DEV              OF E01-G-DATA-INF-CLR
150978                                 TO tech-attr-req-currCode
150978                                 OF PoolBalance
150978                                 OF PoolReport
150978                                   (PoolReport--C)
150978D    DISPLAY 'tech-attr-req-currCode :'    tech-attr-req-currCode
150978D                                      OF PoolBalance
150978D                                      OF PoolReport
150978D                                        (PoolReport--C)
150978     COMPUTE tech-text
150978          OF PoolBalance
150978          OF PoolReport
150978           (PoolReport--C) = E01-M-MNT OF E01-G-DATA-INF-CLR
151345                                       / 10 ** E01-Q-NBR-DEC
150978                                            OF E01-G-DATA-INF-CLR
150978D    DISPLAY 'tech-text :'          tech-text
150978D                                OF PoolBalance
150978D                                OF PoolReport
150978D                                  (PoolReport--C)
150978     .

      *---------*
       TRAIT-ENQ.
      *---------*

      *    Ecrire les lignes XML de fermeture
           PERFORM ECRT-WS-G-XML-FIN

      *    Lire ligne enqueue
           PERFORM LECT-FICH-E01

      *    Alimenter ligne enqueue
           PERFORM ALIM-DATA-WS-ENQ-END

           .

      *--------------------*
       ALIM-DATA-WS-ENQ-END.
      *--------------------*

      *    S'assurer qu'on est sur l'enqueue du fichier
150978     IF  E01-C-ENR   = '90'
           AND E01-C-ENTIT = 'END'

              MOVE E01-G-DATA-ENQ-END TO WS-G-DATA-ENQ-END

           ELSE

              MOVE 3018               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF

      D    DISPLAY SPACE
      D    DISPLAY 'Enqueue - Nb comptes du fichier : '
      D             WS-N-CPT OF WS-G-DATA-ENQ-END
      D    DISPLAY 'Enqueue - Nb clients du fichier : '
      D             WS-N-CLT OF WS-G-DATA-ENQ-END
      D    DISPLAY SPACE
           .

      *-------------*
       FIN-TRAIT.
      *-------------*

      *    Fermeture du fichier en entrée
           PERFORM FERM-FICH-E01

      *    Fermeture du fichier en sortie
           PERFORM FERM-FICH-S01

150978*    Fermeture du fichier en sortie à destination de BFUSMJ00
150978     PERFORM FERM-FICH-S02

      *    Récuperation de la date / heure de fin de traitement
           PERFORM RECUP-DATE-HEURE-FIN

      *    Affichage du bilan final
           PERFORM AFFICH-BILAN-FIN

           PERFORM FIN-PROGRAMME
           .

      *
      * ----------------------> NIVEAU 002
      *

      *---------------------------*
       ALIM-XL00-FONC-IN-FATCA.
      *---------------------------*

      *    Initialisation de la zone entrée et la zone entrée-sortie
           PERFORM INIT-XL00-FONC-IN

      *    Alimentation des paramètres du module
           MOVE '1'                   TO XL00-B-INDT
           MOVE '1'                   TO XL00-B-NMSP
           MOVE '1'                   TO XL00-B-XSD
           MOVE '1'                   TO XL00-B-PRFX
           MOVE '1'                   TO XL00-B-ATTR
           MOVE '1'                   TO XL00-B-TEXT
MCHA  *    MOVE '0'                   TO XL00-B-VIDE
MCHA       MOVE '1'                   TO XL00-B-VIDE
           MOVE '1'                   TO XL00-B-RESV
           MOVE '1'                   TO XL00-B-DECL
           MOVE '1'                   TO XL00-B-DCOP

      *    paramètres mots réservés
           MOVE  2                    TO XL00-Q-RESV-INDICATIF
           MOVE 'R-'                  TO XL00-L-RESV-INDICATIF

      *    paramètres xml délcaration
           MOVE  5                    TO XL00-Q-DECL-ENCODING
           MOVE 'UTF-8'               TO XL00-L-DECL-ENCODING

      *    paramètres Name space
           MOVE  0                    TO XL00-Q-NMSP

           ADD   1                    TO XL00-Q-NMSP
           MOVE  3                    TO XL00-Q-NMSP-PRFX (XL00-Q-NMSP)
           MOVE 'ftc'                 TO XL00-L-NMSP-PRFX (XL00-Q-NMSP)
           MOVE  22                   TO XL00-Q-NMSP-URN  (XL00-Q-NMSP)
           MOVE 'urn:oecd:ties:fatca:v1'
                                      TO XL00-L-NMSP-URN  (XL00-Q-NMSP)

           ADD   1                    TO XL00-Q-NMSP
           MOVE  3                    TO XL00-Q-NMSP-PRFX (XL00-Q-NMSP)
           MOVE 'sfa'                 TO XL00-L-NMSP-PRFX (XL00-Q-NMSP)
           MOVE  30                   TO XL00-Q-NMSP-URN  (XL00-Q-NMSP)
           MOVE 'urn:oecd:ties:stffatcatypes:v1'
                                      TO XL00-L-NMSP-URN  (XL00-Q-NMSP)

           ADD   1                    TO XL00-Q-NMSP
           MOVE  3                    TO XL00-Q-NMSP-PRFX (XL00-Q-NMSP)
           MOVE 'xsi'                 TO XL00-L-NMSP-PRFX (XL00-Q-NMSP)
           MOVE  41                   TO XL00-Q-NMSP-URN  (XL00-Q-NMSP)
           MOVE 'http://www.w3.org/2001/XMLSchema-instance'
                                      TO XL00-L-NMSP-URN  (XL00-Q-NMSP)

      *    paramètres Schema
           MOVE  40                   TO XL00-Q-XSD-URN
           MOVE 'urn:oecd:ties:fatca:v1 FatcaXML_v1.1.xsd'
                                      TO XL00-L-XSD-URN

      *    paramètres indentation
           MOVE WS-Q-INDT-UNIT        TO XL00-Q-INDT-UNIT
           MOVE WS-Q-INDT-INIT        TO XL00-Q-INDT-INIT

      *    paramètres découpage
           MOVE 14                    TO XL00-Q-DCOP-TAG
           MOVE 'ReportingGroup'      TO XL00-L-DCOP-TAG

      *    paramètres préfixes
           MOVE 3                     TO XL00-Q-PRFX-DFLT
           MOVE 'sfa'                 TO XL00-L-PRFX-DFLT
           MOVE 3                     TO XL00-Q-PRFX-PARM
           MOVE WS-L-PREFIXE          TO XL00-L-PRFX-PARM

           MOVE WS-Q-PREFIXE-ELEMENT-TAB
                                      TO XL00-Q-PRFX

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-PREFIXE-ELEMENT-TAB

              MOVE WS-G-PREFIXE-ELEMENT(I) TO XL00-G-PRFX(I)

           END-PERFORM


      *    données xml brutes à traiter
           MOVE WS-Q-XML-GENERATE     TO XL00-Q-XML-BRUTE
           MOVE WS-L-XML-GENERATE     TO XL00-L-XML-BRUTE

           .

      *-----------------------------*
       ALIM-XL00-FONC-IN-ACCOUNT.
      *-----------------------------*

      *    Initialisation de la zone entrée et la zone entrée-sortie
           PERFORM INIT-XL00-FONC-IN

      *    Alimentation des paramètres du module
           MOVE '1'                   TO XL00-B-INDT
           MOVE '0'                   TO XL00-B-NMSP
           MOVE '0'                   TO XL00-B-XSD
           MOVE '1'                   TO XL00-B-PRFX
           MOVE '1'                   TO XL00-B-ATTR
           MOVE '1'                   TO XL00-B-TEXT
SBOU  *    MOVE '0'                   TO XL00-B-VIDE
SBOU       MOVE '1'                   TO XL00-B-VIDE
           MOVE '1'                   TO XL00-B-RESV
           MOVE '0'                   TO XL00-B-DECL
           MOVE '0'                   TO XL00-B-DCOP

      *    paramètres mots réservés
           MOVE  2                    TO XL00-Q-RESV-INDICATIF
           MOVE 'R-'                  TO XL00-L-RESV-INDICATIF

      *    paramètres préfixes
           MOVE 3                     TO XL00-Q-PRFX-DFLT
           MOVE 'sfa'                 TO XL00-L-PRFX-DFLT
           MOVE 3                     TO XL00-Q-PRFX-PARM
           MOVE WS-L-PREFIXE          TO XL00-L-PRFX-PARM

           MOVE WS-Q-PREFIXE-ELEMENT-TAB
                                      TO XL00-Q-PRFX

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-PREFIXE-ELEMENT-TAB

              MOVE WS-G-PREFIXE-ELEMENT(I) TO XL00-G-PRFX(I)

           END-PERFORM


      *    paramètres indentation
           MOVE WS-Q-INDT-UNIT        TO XL00-Q-INDT-UNIT
           MOVE WS-Q-INDT-INIT        TO XL00-Q-INDT-INIT

      *    données xml brutes à traiter
           MOVE WS-Q-XML-GENERATE     TO XL00-Q-XML-BRUTE
           MOVE WS-L-XML-GENERATE     TO XL00-L-XML-BRUTE

           .

      *-------------*
       APPL-XL00.
      *-------------*

      D    PERFORM AFFICH-XL00-FONC-IN

      *    Appel au module
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!          APPEL MODULE MFUSXL00           !'
      D    DISPLAY '+------------------------------------------+'
           PERFORM SQ-APPEL-XL00
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!        FIN APPEL MODULE MFUSXL00         !'
      D    DISPLAY '+------------------------------------------+'

      D    PERFORM AFFICH-XL00-FONC-OUT

           EVALUATE XL00-CODE-RETOUR

              WHEN 0
              WHEN 4
      *          Valoriser données WS par retour module
                 MOVE XL00-Q-XML-TAB            TO  WS-Q-XML-TAB
                 PERFORM VARYING I FROM 1 BY 1
                 UNTIL   I > XL00-Q-XML-TAB
                    MOVE XL00-G-XML-TAB (I)     TO  WS-G-XML-TAB (I)
                 END-PERFORM

                 MOVE XL00-N-XML-DCOP           TO  WS-N-XML-DCOP
                 MOVE XL00-Q-XML-DCOP-INDT      TO  WS-Q-XML-DCOP-INDT

              WHEN OTHER
                 MOVE 3000                      TO  WS-CODE-ABEND
                 PERFORM TRAIT-ABEND

           END-EVALUATE

           .



      *---------------------*
       INIT-XL00-FONC-IN.
      *---------------------*

      *    Initialisation de la zone entrée et la zone entrée-sortie
           MOVE SPACE                 TO XL00-G-FCT-APPL
           MOVE SPACE                 TO XL00-G-FCT-PARAM-IN
           MOVE ZEROS                 TO XL00-Q-NMSP
           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > 10
              MOVE ZEROS              TO XL00-Q-NMSP-PRFX (I)
              MOVE ZEROS              TO XL00-Q-NMSP-URN  (I)
           END-PERFORM
           MOVE ZEROS                 TO XL00-Q-XSD-URN
           MOVE ZEROS                 TO XL00-Q-PRFX-DFLT
           MOVE ZEROS                 TO XL00-Q-RESV-INDICATIF
           MOVE ZEROS                 TO XL00-Q-XML-BRUTE
           MOVE SPACE                 TO XL00-L-XML-BRUTE
           MOVE ZEROS                 TO XL00-Q-INDT-INIT
           MOVE ZEROS                 TO XL00-Q-INDT-UNIT
           MOVE ZEROS                 TO XL00-Q-DCOP-TAG
           .


      *--------------------------*
       ALIM-DATA-WS-TET-END.
      *--------------------------*

      *    S'assurer qu'on est sur l'entête et la ligne traite une END
150978     IF  E01-C-ENR   = '10'
           AND E01-C-ENTIT = 'END'

              MOVE E01-G-DATA-TET-END TO WS-G-DATA-TET-END

150978*    Valoriser DocRefId
150978        PERFORM VALORISER-DocRefId
           ELSE

              MOVE 3010               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *--------------------------*
       ALIM-DATA-WS-TET-ADR.
      *--------------------------*

      *    S'assurer qu'on est sur l'entête et la ligne traite une ADR
150978     IF  E01-C-ENR   = '10'
           AND E01-C-ENTIT = 'ADR'

              MOVE E01-G-DATA-TET-ADR TO WS-G-DATA-TET-ADR

           ELSE

              MOVE 3011               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *--------------------------*
       ALIM-DATA-WS-DET-CNT.
      *--------------------------*

      *    S'assurer qu'on est sur détail et la ligne traite un CNT
150978     IF  E01-C-ENR   = '20'
           AND E01-C-ENTIT = 'CNT'

150978        MOVE E01-I-UNIQ-KAC OF E01-G-DATA-DET-CNT
150978                            TO WS-I-UNIQ-KAC OF WS-G-DATA-DET-CNT
150978        MOVE E01-I-REF-BLOC OF E01-G-DATA-DET-CNT
150978                            TO WS-I-REF-BLOC OF WS-G-DATA-DET-CNT
MCHA+-        MOVE E01-I-REF-BLOC-INIT OF E01-G-DATA-DET-CNT
MCHA+-                       TO WS-I-REF-BLOC-INIT OF WS-G-DATA-DET-CNT
MCHA+         MOVE E01-I-KAC-IBAN OF E01-G-DATA-DET-CNT
MCHA+                             TO WS-I-KAC-IBAN OF WS-G-DATA-DET-CNT
150978        MOVE E01-Q-NBR-DEC-SLD  OF E01-G-DATA-DET-CNT
150978                         TO WS-Q-NBR-DEC-SLD OF WS-G-DATA-DET-CNT
150978        MOVE E01-M-MNT-ASS-SLD  OF E01-G-DATA-DET-CNT
150978                         TO WS-M-MNT-ASS-SLD OF WS-G-DATA-DET-CNT
150978        MOVE E01-C-DEV      OF E01-G-DATA-DET-CNT
150978                            TO WS-C-DEV      OF WS-G-DATA-DET-CNT
MCHA+         MOVE E01-Q-LIST-MNT TO WS-Q-LIST-MNT
"             MOVE ZERO                TO L
"             PERFORM VARYING L FROM 1 BY 1
"              UNTIL L > E01-Q-LIST-MNT
"     *         DISPLAY  ' L ' E01-C-TYP-MNT   OF E01-T-LIST-MNT (L)
"               MOVE E01-C-TYP-MNT   OF E01-T-LIST-MNT (L)
"                    TO WS-C-TYP-MNT OF WS-T-LIST-MNT (L)
"               MOVE E01-Q-NBR-DEC   OF E01-T-LIST-MNT (L)
"                    TO WS-Q-NBR-DEC OF WS-T-LIST-MNT (L)
"               MOVE E01-M-MNT-ASS   OF E01-T-LIST-MNT (L)
"                    TO WS-M-MNT-ASS OF WS-T-LIST-MNT (L)
MCHA+         END-PERFORM
MC            MOVE E01-I-REF-FID-INIT  OF E01-G-DATA-DET-CNT
MC                           TO WS-I-REF-FID-INIT  OF WS-G-DATA-DET-CNT

           ELSE

              MOVE 3013               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *--------------------------*
       ALIM-DATA-WS-DET-CLT.
      *--------------------------*

      *    S'assurer qu'on est sur détail et la ligne traite un CLI
150978     IF  E01-C-ENR   = '20'
           AND E01-C-ENTIT = 'CLI'

              MOVE E01-G-DATA-DET-CLT TO WS-G-DATA-DET-CLT
           ELSE

              MOVE 3014               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *--------------------------*
       ALIM-DATA-WS-DET-ADR.
      *--------------------------*

      *    S'assurer qu'on est sur détail et la ligne traite une ADR
150978     IF  E01-C-ENR   = '20'
           AND E01-C-ENTIT = 'ADR'

      D       DISPLAY 'WS-Q-DATA-DET-ADR before ++ : ' WS-Q-DATA-DET-ADR
              ADD  1                  TO WS-Q-DATA-DET-ADR
              MOVE E01-G-DATA-DET-ADR TO WS-G-DATA-DET-ADR
                                        (WS-Q-DATA-DET-ADR)
           ELSE

              MOVE 3015               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *--------------------------*
       ALIM-DATA-WS-DET-BNF.
      *--------------------------*

      *    S'assurer qu'on est sur détail et la ligne traite une BNF
150978     IF  E01-C-ENR   = '20'
           AND E01-C-ENTIT = 'BNF'

      D       DISPLAY 'WS-Q-DATA-DET-BNF before ++ : ' WS-Q-DATA-DET-BNF
              ADD  1                  TO WS-Q-DATA-DET-BNF
              MOVE E01-G-DATA-DET-BNF TO WS-G-DATA-DET-BNF
                                        (WS-Q-DATA-DET-BNF)
           ELSE

              MOVE 3016               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .


      *---------------*
       OUVR-FICH-E01.
      *---------------*

      *    OUVERTURE DU FICHIER DFUSLE21
           OPEN INPUT DFUSLE21
      D    DISPLAY 'FS-E01 : ' FS-E01
           IF FS-E01  NOT = '00'
      *       PROBLEME TECHNIQUE - APPEL MODULE ABEND
              MOVE 2011               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *---------------*
       LECT-FICH-E01.
      *---------------*

      *    LECTURE DU FICHIER DFUSLE21
           READ DFUSLE21
150978*    AT END
150978*       SET  FIN-DFUSLE21             TO TRUE
           END-READ

      D    DISPLAY 'FS-E01 : ' FS-E01
           EVALUATE FS-E01

              WHEN '00'
                 MOVE  FD-DFUSLE21          TO ENR-DFUSLE21
                 ADD   1                    TO WS-NB-LECT-E01
MCHA+ *          NETTOYAGE DES CARACTERS SPECIAUX
MCHA+            IF E01-C-ENR = '20'  AND
MCHA+               ( E01-C-ENTIT = 'END' OR
MCHA+                 E01-C-ENTIT = 'ADR' OR
MCHA+                 E01-C-ENTIT = 'CLI' )
MCHA+               PERFORM NETTOY-CAR-SPEC
MCHA+            END-IF
      D          PERFORM AFFICH-FICH-E01

              WHEN '10'
150978*          IF  WS-NB-LECT-E01 < 1 THEN
150978*             MOVE 2014               TO WS-CODE-ABEND
150978*             PERFORM TRAIT-ABEND
150978*          END-IF
150978           SET  FIN-DFUSLE21             TO TRUE
              WHEN OTHER
                 MOVE 2012                  TO WS-CODE-ABEND
                 PERFORM TRAIT-ABEND

           END-EVALUATE

           .

      *---------------*
       FERM-FICH-E01.
      *---------------*

      *    FERMETURE
           CLOSE DFUSLE21
           IF FS-E01  NOT = '00'
      *       PROBLEME TECHNIQUE - APPEL MODULE ABEND
              MOVE 2013               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *---------------*
       OUVR-FICH-S01.
      *---------------*

      *    OUVERTURE DU FICHIER DFUSLS21
           OPEN OUTPUT DFUSLS21
           IF FS-S01  NOT = '00'
      *       PROBLEME TECHNIQUE - APPEL MODULE ABEND
              MOVE 2021               TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

150978*---------------*
150978 OUVR-FICH-S02.
150978*---------------*
150978
150978*    OUVERTURE DU FICHIER DFUSLS21
150978     OPEN OUTPUT DFUSLS22
150978     IF FS-S02  NOT = '00'
150978*       PROBLEME TECHNIQUE - APPEL MODULE ABEND
150978        MOVE 4015               TO WS-CODE-ABEND
150978        PERFORM TRAIT-ABEND
150978
150978     END-IF
150978     .

      *---------------*
       ECRT-FICH-S01.
      *---------------*

      *    ECRITURE DU FICHIER DFUSLS21
           WRITE FD-DFUSLS21    FROM     ENR-DFUSLS21
           END-WRITE

           IF FS-S01    NOT = '00'
               MOVE 2022               TO WS-CODE-ABEND
               PERFORM TRAIT-ABEND
           ELSE
               ADD 1 TO WS-NB-ECRT-S01
           END-IF
           .

150978*---------------*
150978 ECRT-FICH-S02.
150978*---------------*
150978
150978*    ECRITURE DU FICHIER DFUSLS21
150978     WRITE FD-DFUSLS22    FROM     MJ00-CFUSMJ00
150978     END-WRITE
150978
150978     IF FS-S02    NOT = '00'
150978         MOVE 4016               TO WS-CODE-ABEND
150978         PERFORM TRAIT-ABEND
150978     ELSE
150978         ADD 1 TO WS-NB-ECRT-S02
150978     END-IF
150978     .

      *---------------*
       FERM-FICH-S01.
      *---------------*

      *    FERMETURE
           CLOSE DFUSLS21
           IF FS-S01    NOT = '00'
               MOVE 2023               TO WS-CODE-ABEND
               PERFORM TRAIT-ABEND
           END-IF
           .

150978*---------------*
150978 FERM-FICH-S02.
150978*---------------*
150978
150978*    FERMETURE
150978     CLOSE DFUSLS22
150978     IF FS-S02    NOT = '00'
150978         MOVE 4017               TO WS-CODE-ABEND
150978         PERFORM TRAIT-ABEND
150978     END-IF
150978     .


MCHA+ *---------------*
"      NETTOY-CAR-SPEC.
"     *---------------*
"
"     *    Traitement des caracteres spéciaux
"     *
"          PERFORM  VARYING I-SP FROM 1 BY 1
"                   UNTIL I-SP > 1000
"             IF I-SP > 25
"                MOVE ENR-DFUSLE21(I-SP: 1)   TO CAR-SP
"                IF  CAR-SP NOT NUMERIC      AND
"                    CAR-SP NOT ALPHABETIC   AND
"                    CAR-SP NOT = '.'        AND
"                    CAR-SP NOT = '-'        AND
"                    CAR-SP NOT = ','        AND
"                    CAR-SP NOT = SPACE      AND
"                    CAR-SP NOT = LOW-VALUE
"                    MOVE SPACE  TO   ENR-DFUSLE21(I-SP: 1)
"
"                 END-IF
"             END-IF
"          END-PERFORM
MCHA+      .
      *
      * ----------------------> Taitement ABEND
      *

      *------------------*
       TRAIT-ABEND.
      *------------------*

      *    Gestion des Abends
      *    Le code abend est passé à ce parapgraphe par WS-CODE-ABEND
      *    Les donnes complementaires de l'abend (File-Status ...)

      *    Initialisation des lignes de descriptions d'abend
           MOVE ZEROS                   TO INAB-Q-LIST-DISP
           MOVE SPACE                   TO BILA-FONC-IN(13:2400)


           MOVE WS-CODE-ABEND           TO INAB-C-ABEND

      *    Alimentation du message d'erreur selon le code abend
           EVALUATE    WS-CODE-ABEND

      *       1000 : PROBLEME APPEL MODULE MGDATR03 DEBUT
      *       1001 : PROBLEME APPEL MODULE MGDATR03 FIN
              WHEN       1000
              WHEN       1001
                 MOVE    11                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB RECUPERATION DATE DU JOUR VIA MGDATR03'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'RETURN CODE = '     TO      WS-LIGNE-ANO41
                 MOVE RETURN-CODE          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'OP06-C-RET  = '     TO      WS-LIGNE-ANO41
                 MOVE OP06-C-RET           TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (11)

      *       2011 : PB OUVERTURE FICHIER DFUSLE21
              WHEN       2011
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB OUVERTURE FICHIER DFUSLE21 '
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-E01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-E01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *       2012 : PB LECTURE FICHIER DFUSLE21
              WHEN       2012
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB LECTURE FICHIER DFUSLE21 '
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-E01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-E01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *       2013 : PB FERMETURE FICHIER DFUSLE21
              WHEN       2013
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB FERMETURE FICHIER DFUSLE21 '
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-E01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-E01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)


      *       2014 : Fichier DFUSLE21 VIDE
              WHEN       2014
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'FICHIER DFUSLE21 VIDE'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-E01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-E01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *       2021 : PB OUVERTURE FICHIER DFUSLS21
              WHEN       2021
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB OUVERTURE FICHIER DFUSLS21 '
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-S01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-S01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *       2022 : PB ECRITURE FICHIER DFUSLS21
              WHEN       2022
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB ECRITURE FICHIER DFUSLS21 '
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-S01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-S01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *       2023 : PB FERMETURE FICHIER DFUSLS21
              WHEN       2023
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB FERMETURE FICHIER DFUSLS21 '
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'FS-S01   = '        TO      WS-LIGNE-ANO41
                 MOVE FS-S01               TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *       3000 : PB APPEL MFUSXL00
              WHEN       3000
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'PB APPEL MFUSXL00'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'RETURN CODE = '     TO      WS-LIGNE-ANO41
                 MOVE XL00-CODE-RETOUR     TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'C-RET       = '     TO      WS-LIGNE-ANO41
                 MOVE XL00-C-RET           TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'L-C-RET     = '     TO      WS-LIGNE-ANO41
                 MOVE XL00-L-C-RET         TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)

              WHEN       3010
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment données entité déclarante introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3011
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment adresse entité déclarante introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3013
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment Compte attendu mais introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3014
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment Client attendu mais introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3015
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment Adresse client attendu mais introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3016
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment Bénéficiaire attendu mais introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3017
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment détail attendu mais introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       3018
                 MOVE    12                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE
                 'Enregistrment enqueue attendu mais introuvable'
                                           TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE 'E01-C-ENR   = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENR            TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE 'E01-C-ENTIT = '     TO      WS-LIGNE-ANO41
                 MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (9)
                 MOVE 'WS-NB-LECT-E01='    TO      WS-LIGNE-ANO41
                 MOVE WS-NB-LECT-E01       TO      WS-LIGNE-ANO42
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (10)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (11)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (12)
      *
              WHEN       4010
              WHEN       4011
              WHEN       4012
              WHEN       4013
                 MOVE    10                TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
                 MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
                 MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
                 MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
                 MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
                 MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
                 MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)
150978*       4014 : ENTITé différente de FID, REP ou RBR
150978        WHEN       4014
150978           MOVE    10                TO      INAB-Q-LIST-DISP
150978           MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
150978           MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
150978           MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
150978           MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
150978           MOVE
150978           'Lecture d''un ENR 40 avec entité non FID, REP ni RBR'
150978                                     TO      WS-LIGNE-ANO21
150978           MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
150978           MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
150978           MOVE 'ENTITéE  = '        TO      WS-LIGNE-ANO41
150978           MOVE E01-C-ENTIT          TO      WS-LIGNE-ANO42
150978           MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
150978           MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)
150978*       4015 : PB OUVERTURE FICHIER DFUSLS22
150978        WHEN       4015
150978           MOVE    10                TO      INAB-Q-LIST-DISP
150978           MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
150978           MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
150978           MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
150978           MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
150978           MOVE
150978           'PB OUVERTURE FICHIER DFUSLS22 '
150978                                     TO      WS-LIGNE-ANO21
150978           MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
150978           MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
150978           MOVE 'FS-S02   = '        TO      WS-LIGNE-ANO41
150978           MOVE FS-S02               TO      WS-LIGNE-ANO42
150978           MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
150978           MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)
150978*       4016 : PB ECRITURE FICHIER DFUSLS22
150978        WHEN       4016
150978           MOVE    10                TO      INAB-Q-LIST-DISP
150978           MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
150978           MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
150978           MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
150978           MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
150978           MOVE
150978           'PB ECRITURE FICHIER DFUSLS22 '
150978                                     TO      WS-LIGNE-ANO21
150978           MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
150978           MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
150978           MOVE 'FS-S02   = '        TO      WS-LIGNE-ANO41
150978           MOVE FS-S02               TO      WS-LIGNE-ANO42
150978           MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
150978           MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)
150978*       4017 : PB FERMETURE FICHIER DFUSLS22
150978        WHEN       4017
150978           MOVE    10                TO      INAB-Q-LIST-DISP
150978           MOVE WS-LIGNE-ANO0        TO      INAB-L-DISP (1)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (2)
150978           MOVE 'CODE ABEND  = '     TO      WS-LIGNE-ANO11
150978           MOVE WS-CODE-ABEND        TO      WS-LIGNE-ANO12
150978           MOVE WS-LIGNE-ANO1        TO      INAB-L-DISP (3)
150978           MOVE
150978           'PB FERMETURE FICHIER DFUSLS22 '
150978                                     TO      WS-LIGNE-ANO21
150978           MOVE WS-LIGNE-ANO2        TO      INAB-L-DISP (4)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (5)
150978           MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (6)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (7)
150978           MOVE 'FS-S02   = '        TO      WS-LIGNE-ANO41
150978           MOVE FS-S02               TO      WS-LIGNE-ANO42
150978           MOVE WS-LIGNE-ANO4        TO      INAB-L-DISP (8)
150978           MOVE WS-LIGNE-VIDE        TO      INAB-L-DISP (9)
150978           MOVE WS-LIGNE-EGAL        TO      INAB-L-DISP (10)

      *
              WHEN OTHER
                 MOVE    3                 TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (1)
                 MOVE 'ERREUR INCONNUE'    TO      INAB-L-DISP (2)
                 MOVE WS-LIGNE-ETOILE      TO      INAB-L-DISP (3)

           END-EVALUATE

      *    ARRET TRAITEMENT
           CALL 'MCCDINAB' USING INAB-PARAM
           .


      *
      * ----------------------> Taitements organiques (MOVE, DISPLAY ..)
      *

      *-----------------*
      DAFFICH-FICH-E01.
      *-----------------*

      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!      ENREGISTRMENT LU DANS DFUSLE21      !'
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! COMPTEUR ENREG   : ' WS-NB-LECT-E01
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! E01-C-ENR        : ' E01-C-ENR
      D    DISPLAY '! E01-I-IDENT      : ' E01-I-IDENT
      D    DISPLAY '! E01-N-LIG-IDENT  : ' E01-N-LIG-IDENT
      D    DISPLAY '! E01-C-ENTIT      : ' E01-C-ENTIT
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!      FIN ENREGISTRMENT LU DFUSLE21       !'
      D    DISPLAY '+------------------------------------------+'
      D    .


      *---------------------*
      DAFFICH-XL00-FONC-IN.
      *---------------------*

      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!           ZONE ENTREE MFUSXL00           !'
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-B-INDT          : ' XL00-B-INDT
      D    DISPLAY '! XL00-B-NMSP          : ' XL00-B-NMSP
      D    DISPLAY '! XL00-B-XSD           : ' XL00-B-XSD
      D    DISPLAY '! XL00-B-PRFX          : ' XL00-B-PRFX
      D    DISPLAY '! XL00-B-ATTR          : ' XL00-B-ATTR
      D    DISPLAY '! XL00-B-TEXT          : ' XL00-B-TEXT
      D    DISPLAY '! XL00-B-VIDE          : ' XL00-B-VIDE
      D    DISPLAY '! XL00-B-RESV          : ' XL00-B-RESV
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-INDT-INIT     : ' XL00-Q-INDT-INIT
      D    DISPLAY '! XL00-Q-INDT-UNIT     : ' XL00-Q-INDT-UNIT
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-NMSP          : ' XL00-Q-NMSP
      D    DISPLAY '! XL00-Q-NMSP-PRFX     : ' XL00-Q-NMSP-PRFX    (1)
      D    DISPLAY '! XL00-L-NMSP-PRFX     : ' XL00-L-NMSP-PRFX    (1)
      D    DISPLAY '! XL00-Q-NMSP-URN      : ' XL00-Q-NMSP-URN     (1)
      D    DISPLAY '! XL00-L-NMSP-URN      : ' XL00-L-NMSP-URN     (1)
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-XSD-URN       : ' XL00-Q-XSD-URN
      D    DISPLAY '! XL00-L-XSD-URN       : ' XL00-L-XSD-URN
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-PRFX-DFLT     : ' XL00-Q-PRFX-DFLT
      D    DISPLAY '! XL00-L-PRFX-DFLT     : ' XL00-L-PRFX-DFLT
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-C-ATTR-TYPE     : ' XL00-C-ATTR-TYPE
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-C-VIDE-TYPE     : ' XL00-C-VIDE-TYPE
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-RESV-INDICATIF: ' XL00-Q-RESV-INDICATIF
      D    DISPLAY '! XL00-L-RESV-INDICATIF: ' XL00-L-RESV-INDICATIF
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-XML-BRUTE     : ' XL00-Q-XML-BRUTE
      *    DISPLAY '! XL00-L-XML-BRUTE 1-80: ' XL00-L-XML-BRUTE(1:80)
      D    DISPLAY '! XL00-L-XML-BRUTE     : ' XL00-L-XML-BRUTE(1:
      D                                        XL00-Q-XML-BRUTE)
      D    DISPLAY '+------------------------------------------+'
      D    .

      *---------------------*
      DAFFICH-XL00-FONC-OUT.
      *---------------------*

      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!           ZONE SORTIE MFUSXL00           !'
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-CODE-RETOUR     : ' XL00-CODE-RETOUR
      D    DISPLAY '! XL00-C-RET           : ' XL00-C-RET
      D    DISPLAY '! XL00-L-C-RET         : ' XL00-L-C-RET
      D    DISPLAY '+------------------------------------------+'
150978*    DISPLAY '! XL00-Q-XML-TRAIT     : ' XL00-Q-XML-TRAIT
150978*    DISPLAY '! XL00-L-XML-TRAIT 1-80: ' XL00-L-XML-TRAIT(1:80)
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! XL00-Q-XML-TAB       : ' XL00-Q-XML-TAB
      D    DISPLAY '+--------+'
      D    DISPLAY '! XL00-Q-XML-LINE      : ' XL00-Q-XML-LINE(1)
      D    DISPLAY '! XL00-Q-XML-INDT      : ' XL00-Q-XML-INDT(1)
      D    DISPLAY '! XL00-L-XML-LINE      : ' XL00-L-XML-LINE(1)
      D                                     (1:XL00-Q-XML-LINE(1))
      D    DISPLAY '+--------+'
      D    DISPLAY '! XL00-Q-XML-LINE      : ' XL00-Q-XML-LINE(2)
      D    DISPLAY '! XL00-Q-XML-INDT      : ' XL00-Q-XML-INDT(2)
      D    DISPLAY '! XL00-L-XML-LINE      : ' XL00-L-XML-LINE(2)
      D                                     (1:XL00-Q-XML-LINE(2))
      D    DISPLAY '+--------+'
      D    DISPLAY '! XL00-Q-XML-LINE      : ' XL00-Q-XML-LINE(3)
      D    DISPLAY '! XL00-Q-XML-INDT      : ' XL00-Q-XML-INDT(3)
      D    DISPLAY '! XL00-L-XML-LINE      : ' XL00-L-XML-LINE(3)
      D                                     (1:XL00-Q-XML-LINE(3))
      D    DISPLAY '+------------------------------------------+'

           .

      *--------------------*
       INIT-PARAM-MCCDBILA.
      *--------------------*
      *    Initialisation paramètre d'entrée de MCCDBILA
           MOVE ZEROS                   TO BILA-Q-LIST-DISP

           PERFORM VARYING I    FROM 1 BY 1
           UNTIL I > 31
              MOVE SPACE                TO BILA-G-LIST-DISP (I)
           END-PERFORM
           .

      *--------------*
       INIT-DATA-WS.
      *--------------*
      *    Initialisation des variables du travail
           MOVE SPACE                   TO TOP-FIN-DFUSLE21
           MOVE ZEROS                   TO WS-NB-LECT-E01
           MOVE ZEROS                   TO WS-NB-ECRT-S01

           PERFORM INIT-WS-G-DATA-DET

           .

      *-------------------*
       INIT-WS-G-DATA-DET.
      *-------------------*

           PERFORM INIT-WS-G-DATA-DET-CNT

           PERFORM INIT-WS-G-DATA-DET-CLT

           .

      *-------------------*
       INIT-WS-G-DATA-DET-CNT.
      *-------------------*

           MOVE SPACE TO WS-G-DATA-DET-CNT
           MOVE ZERO  TO WS-M-MNT-ASS-SLD

           .

      *-------------------*
       INIT-WS-G-DATA-DET-CLT.
      *-------------------*

           MOVE SPACES TO WS-G-DATA-DET-CLT
           MOVE ZERO  TO WS-Q-DATA-DET-ADR

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > 30
              MOVE SPACE TO WS-G-DATA-DET-ADR(I)
           END-PERFORM

           MOVE ZERO  TO WS-Q-DATA-DET-BNF

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > 30
              MOVE SPACE TO WS-G-DATA-DET-BNF(I)
           END-PERFORM
           .

      *------------*
       TRAIT-STRING.
      *------------*

      *    Traitement des chaines de caractères
      *    Permet d'enlever les espaces avant et après chaque string
      *    et de les concaténer éventuellement
      *    Entrée : tableau de strings et séparateur
      *    Sortie : un string

      D    display 'Traitement des string '
      D    display 'Liste string : '

      D    display 'String Separation : ' ws-b-string-sep

      D    perform varying J from 1 by 1
      D    until J > ws-q-string-tab
      D       display 'String ' J ' : <' ws-L-string-in(J) '>'
      D    end-perform

           move zero  to ws-n-string
           move space to ws-l-string-out
           move zero  to ws-q-string-out
           move space to ws-l-string-buffer
           move zero  to ws-l-string-buffer

           perform varying J from 1 by 1
           until J > ws-q-string-tab

              move zero to ws-n-caractere-deb
              move zero to ws-n-caractere-fin

              perform varying K from 1 by 1
              until K > LENGTH OF WS-L-STRING-in

                 if WS-L-string-in(J)(K:1) not = ' '

                    move K to ws-n-caractere-fin

                    if ws-n-caractere-deb = 0

                       move K to ws-n-caractere-deb

                    end-if

                 end-if

              end-perform

      D       display 'string : ' ws-L-string-in(J)  ' deb : '
      D                           ws-n-caractere-deb ' fin : '
      D                           ws-n-caractere-fin

              if ws-n-caractere-deb > 0

                 move   ws-l-string-out to ws-l-string-buffer
                 move   ws-q-string-out to ws-q-string-buffer
                 move   space           to ws-l-string-out

                compute ws-q-deb-fin-1
                      = ws-n-caractere-fin
                      - ws-n-caractere-deb
                      + 1

                 if ws-n-string < 1

                    string ws-L-string-in(J)(ws-n-caractere-deb:
                                             ws-q-deb-fin-1)

                    delimited by size into ws-l-string-out

                    compute ws-q-string-out
                          = ws-q-deb-fin-1

                 else

                    if ws-b-string-sep = 1

                       string ws-l-string-buffer(1:ws-q-string-buffer)
                              ' '
                              ws-L-string-in(J)(ws-n-caractere-deb:
                                                ws-q-deb-fin-1)

                       delimited by size into ws-l-string-out

                       compute ws-q-string-out
                             = ws-q-string-buffer
                             + 1
                             + ws-q-deb-fin-1

                    else

                       string ws-l-string-buffer(1:ws-q-string-buffer)
                              ws-L-string-in(J)(ws-n-caractere-deb:
                                                ws-q-deb-fin-1)

                       delimited by size into ws-l-string-out

                       compute ws-q-string-out
                             = ws-q-string-buffer
                             + ws-q-deb-fin-1

                    end-if

                 end-if

                 add 1 to ws-n-string

              end-if

      D       display ws-l-string-out(1:ws-q-string-out)

           end-perform


           .

150978*-------------*
150978 ALIM-TBFIDFUS.
150978*-------------*
150978*    Initialisation des données en sortie
150978     PERFORM INIT-TBFIDFUS
150978
150978*    Alimentation d'un enregistrement de création de la TBFIDFUS
150978     MOVE 'MAJ'                          TO MJ00-C-OPE
150978     MOVE 'TBFIDFUS'                     TO MJ00-NOM-TABLE
150978     MOVE E01-I-IDENT-END
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-I-IDENT-END
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-A-APPL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-A-APPL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-I-IDENT-REF-REP
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-I-IDENT-REF-REP
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-I-REF-FIC-INIT
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-I-REF-FIC-INIT
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-D-REF-FIC-INIT
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-D-REF-FIC-INIT
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-I-REF-FIC
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-I-REF-FIC
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-N-AGENT
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-N-AGENT
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-NOM-EMET
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-L-NOM-EMET
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-I-REF-ADM
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-I-REF-ADM
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-C-CLE-HACH
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-C-CLE-HACH
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-D-ACCUSE
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-D-ACCUSE
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-Q-PERS-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-Q-PERS-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-Q-CONTRAT-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-Q-CONTRAT-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-C-TYPE-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-C-TYPE-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-M-TOT-SOL-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-M-TOT-SOL-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-M-TOT-INT-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-M-TOT-INT-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-M-TOT-CES-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-M-TOT-CES-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     MOVE E01-M-TOT-M-DECL
150978       OF E01-G-DATA-DET-FID
150978                                         TO MJ00-M-TOT-M-DECL
150978                                         OF MJ00-ENR-TBFIDFUS
150978     .
151197
151197*-------------*
151197 ALIM-TBLCCFUS.
151197*-------------*
151197*    Initialisation des données en sortie
151197     PERFORM INIT-TBLCCFUS
151197
151197*    Alimentation d'un enregistrement de création de la TBLCCFUS
151197     MOVE 'MAJ'                          TO MJ00-C-OPE
151197     MOVE 'TBLCCFUS'                     TO MJ00-NOM-TABLE
151197*  ==> REPORTABILITé DU CONTRAT
151197     MOVE E01-C-PRTB-CNTRT
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-C-PRTB-CNTRT
151197                                         OF MJ00-ENR-TBLCCFUS
151197
151197*  ==> TIMESTAMP DE CRéATION OU DE MAJ
151197     MOVE E01-D-CRE-MAJ
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-D-CRE-MAJ
151197                                         OF MJ00-ENR-TBLCCFUS
151197*  ==> IDENTIFIANT TECHNIQUE TABLE CLIENT
151197     MOVE E01-I-IDENT-CLT
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-I-IDENT-CLT
151197                                         OF MJ00-ENR-TBLCCFUS
151197
151197*  ==> IDENTIFIANT TECHNIQUE TABLE COMPTE
151197     MOVE E01-I-IDENT-CPT
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-I-IDENT-CPT
151197                                         OF MJ00-ENR-TBLCCFUS
151197
151197*  ==> ETAT CYCLE DE VIE LIEN
151197     MOVE E01-C-ECV-LIEN
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-C-ECV-LIEN
151197                                         OF MJ00-ENR-TBLCCFUS
151197
151197*  ==> CODE ACTION
151197     MOVE E01-C-ACTION
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-C-ACTION
151197                                         OF MJ00-ENR-TBLCCFUS
151197
151197*  ==> CODE MOTIF
151197     MOVE E01-C-MOTIF
151197       OF E01-G-DATA-DET-LCC
151197                                         TO MJ00-C-MOTIF
151197                                         OF MJ00-ENR-TBLCCFUS
151197     .
150978
150978*-------------*
150978 ALIM-TBREPFUS.
150978*-------------*
150978*    Initialisation des données en sortie
150978     PERFORM INIT-TBREPFUS
150978
150978*    Alimentation d'un enregistrement de création de la TBREPFUS
150978     MOVE 'CRE'                          TO MJ00-C-OPE
150978     MOVE 'TBREPFUS'                     TO MJ00-NOM-TABLE
150978     MOVE E01-A-APPL
150978       OF E01-G-DATA-DET-REP
150978                                         TO MJ00-A-APPL
150978                                         OF MJ00-ENR-TBREPFUS
150978     MOVE E01-I-IDENT-REF-REP
150978       OF E01-G-DATA-DET-REP
150978                                         TO MJ00-I-IDENT-REF-REP
150978                                         OF MJ00-ENR-TBREPFUS
150978     MOVE E01-I-IDENT-CPT
150978       OF E01-G-DATA-DET-REP
150978                                         TO MJ00-I-IDENT-CPT
150978                                         OF MJ00-ENR-TBREPFUS
150978     MOVE E01-I-IDENT-CLT
150978       OF E01-G-DATA-DET-REP
150978                                         TO MJ00-I-IDENT-CLT
150978                                         OF MJ00-ENR-TBREPFUS
150978     MOVE E01-I-REF-BLOC
150978       OF E01-G-DATA-DET-REP
150978                                         TO MJ00-I-REF-BLOC
150978                                         OF MJ00-ENR-TBREPFUS
150978     .
150978
150978*-------------*
150978 ALIM-TBRBRFUS.
150978*-------------*
150978*    Initialisation des données en sortie
150978     PERFORM INIT-TBRBRFUS
150978
150978*    Alimentation d'un enregistrement de création de la TBREPFUS
150978     MOVE 'CRE'                          TO MJ00-C-OPE
150978     MOVE 'TBRBRFUS'                     TO MJ00-NOM-TABLE
150978     MOVE E01-I-IDENT-REF-REP
150978       OF E01-G-DATA-DET-RBR
150978                                         TO MJ00-I-IDENT-REF-REP
150978                                         OF MJ00-ENR-TBRBRFUS
150978     MOVE E01-I-IDENT-TYP-REC
150978       OF E01-G-DATA-DET-RBR
150978                                         TO MJ00-I-IDENT-TYP-REC
150978                                         OF MJ00-ENR-TBRBRFUS
150978     MOVE E01-C-REF-BLOC-RECAL
150978       OF E01-G-DATA-DET-RBR
150978                                         TO MJ00-C-REF-BLOC-RECAL
150978                                         OF MJ00-ENR-TBRBRFUS
150978     .
150978
150978*-------------*
150978 INIT-TBFIDFUS.
150978*-------------*
150978*    Initialisation des données CFUSMJ00 de la TBFIDFUS
150978     MOVE SPACES TO MJ00-ENR-TBFIDFUS
150978     MOVE ZERO   TO MJ00-I-IDENT-END     OF MJ00-ENR-TBFIDFUS
150978                    MJ00-I-IDENT-REF-REP OF MJ00-ENR-TBFIDFUS
150978                    MJ00-Q-PERS-DECL     OF MJ00-ENR-TBFIDFUS
150978                    MJ00-Q-CONTRAT-DECL  OF MJ00-ENR-TBFIDFUS
150978                    MJ00-M-TOT-SOL-DECL  OF MJ00-ENR-TBFIDFUS
150978                    MJ00-M-TOT-INT-DECL  OF MJ00-ENR-TBFIDFUS
150978                    MJ00-M-TOT-CES-DECL  OF MJ00-ENR-TBFIDFUS
150978                    MJ00-M-TOT-M-DECL    OF MJ00-ENR-TBFIDFUS
150978     .
150978
150978*-------------*
150978 INIT-TBREPFUS.
150978*-------------*
150978*    Initialisation des données CFUSMJ00 de la TBREPFUS
150978     MOVE SPACES TO MJ00-ENR-TBREPFUS
150978     MOVE ZERO   TO MJ00-I-IDENT-REF-REP OF MJ00-ENR-TBREPFUS
150978                    MJ00-I-IDENT-CPT     OF MJ00-ENR-TBREPFUS
150978                    MJ00-I-IDENT-CLT     OF MJ00-ENR-TBREPFUS
150978     .
150978
150978*-------------*
150978 INIT-TBRBRFUS.
150978*-------------*
150978*    Initialisation des données CFUSMJ00 de la TBRBRFUS
150978     MOVE SPACES TO MJ00-ENR-TBREPFUS
150978     MOVE ZERO   TO MJ00-I-IDENT-REF-REP OF MJ00-ENR-TBRBRFUS
150978                    MJ00-I-IDENT-TYP-REC OF MJ00-ENR-TBRBRFUS
150978     .
151197
151197*-------------*
151197 INIT-TBLCCFUS.
151197*-------------*
151197*    Initialisation des données CFUSMJ00 de la TBLCCFUS
151197     MOVE SPACES TO MJ00-ENR-TBLCCFUS
151197     MOVE ZERO   TO MJ00-I-IDENT-CLT     OF MJ00-ENR-TBLCCFUS
151197                    MJ00-I-IDENT-CPT     OF MJ00-ENR-TBLCCFUS
151197     .

      *
      * ----------------------> Appel des modules auxiliaires
      *

      *---------------*
       APPEL-MCCDBILA.
      *---------------*
      *    Module d'affichage
           MOVE 'BFUSEL20'              TO BILA-C-PGM-APPEL
           MOVE 1                       TO BILA-NO-VERSION
           MOVE WS-DISP                 TO BILA-Q-LIST-DISP
      *    Appel du module de BILAN
           CALL 'MCCDBILA' USING  BILA-PARAM
           .

      *---------------------*
       RECUP-DATE-HEURE-DEB.
      *---------------------*

      *    Récuperation de la date de compilation du composant
           MOVE WHEN-COMPILED               TO W-REF-COMPIL

      *    Récuperation de la date de traitement
           MOVE 'ACD-D-HR'                  TO NOM-PRIMITIVE
           CALL 'MGDATR03'               USING NOM-PRIMITIVE
                                               MGDATR03-PARAM
      *    Contrôle du code retour technique
           IF RETURN-CODE NOT = ZERO OR
              OP06-C-RET  NOT = ZERO
              MOVE 1000                     TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND
           ELSE
      *       Récuperation sous différents formats
              MOVE D-ISO-AAAA OF OP06-T-REF TO WS-ANNEE-TRAITEMENT
              MOVE D-ISO-MM   OF OP06-T-REF TO WS-MOIS-TRAITEMENT
              MOVE D-ISO-JJ   OF OP06-T-REF TO WS-JOUR-TRAITEMENT


              MOVE H-ISO-HH   OF OP06-T-REF TO WS-HH-TRAITEMENT
              MOVE H-MM       OF OP06-T-REF TO WS-MM-TRAITEMENT
              MOVE H-ISO-SS   OF OP06-T-REF TO WS-SS-TRAITEMENT

              MOVE OP06-T-REF               TO WS-HORODATAGE-TRT
              MOVE OP06-T-REF               TO WS-D-H-TRT
      D       DISPLAY 'DATE DE TRAITEMENT : '  WS-HORODATAGE-TRT
           END-IF
           .


      *--------------------*
       RECUP-DATE-HEURE-FIN.
      *--------------------*

      *    Récuperation de la date de compilation du composant
           MOVE WHEN-COMPILED               TO W-REF-COMPIL

      *    Récuperation de la date systeme et timestamp
           MOVE 'ACD-D-HR'                  TO NOM-PRIMITIVE
           CALL 'MGDATR03'               USING NOM-PRIMITIVE
                                               MGDATR03-PARAM

      D    DISPLAY 'RETURN-CODE  : ' RETURN-CODE
      D    DISPLAY 'OP06-C-RET   : ' OP06-C-RET

           IF RETURN-CODE = 0 AND OP06-C-RET = 0

      *       Valorisation de la date WS-DATE-TRAITEMENT
              MOVE D-ISO-AAAA OF OP06-T-REF TO WS-ANNEE-TRAITEMENT
              MOVE D-ISO-MM   OF OP06-T-REF TO WS-MOIS-TRAITEMENT
              MOVE D-ISO-JJ   OF OP06-T-REF TO WS-JOUR-TRAITEMENT

      *       Valorisation de l'heure WS-HEURE-TRAITEMENT
              MOVE H-ISO-HH   OF OP06-T-REF TO WS-HH-TRAITEMENT
              MOVE H-MM       OF OP06-T-REF TO WS-MM-TRAITEMENT
              MOVE H-ISO-SS   OF OP06-T-REF TO WS-SS-TRAITEMENT

           ELSE

              MOVE 1001                     TO WS-CODE-ABEND
              PERFORM TRAIT-ABEND

           END-IF
           .

      *------------------------*
       AFFICH-BILAN-DEB.
      *------------------------*
      * AFFICHAGE DE DEBUT DU TRAITEMENT

      *    INITIALISATION DES PARAMETRES EN ENTREE DU MCCDBILA
           PERFORM INIT-PARAM-MCCDBILA
           MOVE ZEROS                   TO WS-DISP
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-DEBUTPROG      TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
           MOVE '*             CREATION REPORTING FATCA
      -    '          *'                TO BILA-L-DISP(WS-DISP)(1:64)
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
      *    NOM DU PROGRAMME
           MOVE '*    PROGRAMME ............... : BFUSEL20
      -    '        *'                  TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
      *    DATE DE COMPILATION
           MOVE '*    DATE DE COMPILATION ..... :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE '-'                     TO W-DATE-COMPIL-S1
           MOVE '-'                     TO W-DATE-COMPIL-S2
           MOVE W-DATE-COMPIL(4:2)      TO BILA-L-DISP(WS-DISP)(34:2)
           MOVE '-'                     TO BILA-L-DISP(WS-DISP)(36:1)
           MOVE W-DATE-COMPIL(1:2)      TO BILA-L-DISP(WS-DISP)(37:2)
           MOVE '-'                     TO BILA-L-DISP(WS-DISP)(39:1)
           MOVE W-DATE-COMPIL(7:)       TO BILA-L-DISP(WS-DISP)(40:)
           ADD  1                       TO WS-DISP
      *    HEURE DE COMPILATION
           MOVE '*    HEURE DE COMPILATION .... :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE ':'                     TO W-HEURE-COMPIL-DP1
           MOVE ':'                     TO W-HEURE-COMPIL-DP2
           MOVE W-HEURE-COMPIL          TO BILA-L-DISP(WS-DISP)(34:10)
           ADD  1                       TO WS-DISP
      *    DATE DEBUT TRAITEMENT
           MOVE '*    DATE DEBUT TRAITEMENT.... :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-D-H-TRT(9:2)         TO BILA-L-DISP(WS-DISP)(34:2)
           MOVE '-'                     TO BILA-L-DISP(WS-DISP)(36:1)
           MOVE WS-D-H-TRT(6:2)         TO BILA-L-DISP(WS-DISP)(37:2)
           MOVE '-'                     TO BILA-L-DISP(WS-DISP)(39:1)
           MOVE WS-D-H-TRT(1:4)         TO BILA-L-DISP(WS-DISP)(40:4)
           ADD  1                       TO WS-DISP
      *    HEURE DEBUT TRAITEMENT
           MOVE '*    HEURE DE DEBUT TRAITEMENT :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-D-H-TRT(12:2)        TO BILA-L-DISP(WS-DISP)(34:2)
           MOVE ':'                     TO BILA-L-DISP(WS-DISP)(36:1)
           MOVE WS-D-H-TRT(15:2)        TO BILA-L-DISP(WS-DISP)(37:2)
           MOVE ':'                     TO BILA-L-DISP(WS-DISP)(39:1)
           MOVE WS-D-H-TRT(18:2)        TO BILA-L-DISP(WS-DISP)(40:2)
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           PERFORM APPEL-MCCDBILA
           .

      *----------------*
       AFFICH-BILAN-END.
      *----------------*
      * AFFICHAGE DES INFORMATIONS DE L'ENTITE DECLARANTE

      *    INITIALISATION DES PARAMETRES EN ENTREE DU MCCDBILA
           PERFORM INIT-PARAM-MCCDBILA
           MOVE ZEROS                   TO WS-DISP
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
           MOVE '*             ENTITE DECLARANTE TRAITEE
      -    '          *'                TO BILA-L-DISP(WS-DISP)(1:64)
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           ADD  1                       TO WS-DISP
      *    Entité déclarante
           MOVE '*    ENTITE DECLARANTE         :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-L-RAISON-SOCIALE     TO BILA-L-DISP(WS-DISP)(34:28)
           ADD  1                       TO WS-DISP
      *    Année fiscale
           MOVE '*    ANNEE FISCALE             :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-A-APPL               TO BILA-L-DISP(WS-DISP)(34:28)
           ADD  1                       TO WS-DISP
      *    Pays adresse
           MOVE '*    PAYS ADRESSE              :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-C-PAYS-ADR
             OF WS-G-DATA-TET-ADR TO    BILA-L-DISP(WS-DISP)(34:28)
           ADD  1                       TO WS-DISP
      *    Commune
           MOVE '*    COMMUNE                   :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-L-COMM-ADR
             OF WS-G-DATA-TET-ADR       TO BILA-L-DISP(WS-DISP)(34:28)
           ADD  1                       TO WS-DISP
      *    Mode reporting
           MOVE '*    MODE  REPORTING           :
      -    '        *'                  TO BILA-L-DISP(WS-DISP)(1:64)
           IF WS-C-MOD-REPORT = '1'
              MOVE 'NORMALE'            TO BILA-L-DISP(WS-DISP)(34:28)
           ELSE
              MOVE 'TEST'               TO BILA-L-DISP(WS-DISP)(34:28)
           END-IF
           ADD  1                       TO WS-DISP
           MOVE WS-LIGNE-EGAL           TO BILA-L-DISP(WS-DISP)
           PERFORM APPEL-MCCDBILA
           .

      *----------------*
       AFFICH-BILAN-FIN.
      *----------------*
      * ECRITURE BILAN FINAL
           MOVE ZEROS                    TO WS-DISP
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-VIDE            TO BILA-L-DISP(WS-DISP)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-EGAL            TO BILA-L-DISP(WS-DISP)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-FINPROG         TO BILA-L-DISP(WS-DISP)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-EGAL            TO BILA-L-DISP(WS-DISP)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-VIDE            TO BILA-L-DISP(WS-DISP)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-EGAL            TO BILA-L-DISP(WS-DISP)
           ADD  1                        TO WS-DISP
           MOVE '*    DATE  DE FIN TRAITEMENT    :
      -    '        *'                   TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-DATE-TRAITEMENT(9:2)  TO BILA-L-DISP(WS-DISP)(45:2)
           MOVE '-'                      TO BILA-L-DISP(WS-DISP)(47:1)
           MOVE WS-DATE-TRAITEMENT(6:2)  TO BILA-L-DISP(WS-DISP)(48:2)
           MOVE '-'                      TO BILA-L-DISP(WS-DISP)(50:1)
           MOVE WS-DATE-TRAITEMENT(1:4)  TO BILA-L-DISP(WS-DISP)(51:4)
           ADD  1                        TO WS-DISP
           MOVE '*    HEURE DE FIN TRAITEMENT    :
      -    '        *'                   TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-HH-TRAITEMENT         TO BILA-L-DISP(WS-DISP)(45:2)
           MOVE ':'                      TO BILA-L-DISP(WS-DISP)(47:1)
           MOVE WS-MM-TRAITEMENT         TO BILA-L-DISP(WS-DISP)(48:2)
           MOVE ':'                      TO BILA-L-DISP(WS-DISP)(50:1)
           MOVE WS-SS-TRAITEMENT         TO BILA-L-DISP(WS-DISP)(51:4)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-EGAL            TO BILA-L-DISP(WS-DISP)
      *    Nombre de lectures du fichier DFUSLE21
           ADD  1                        TO WS-DISP
           MOVE '*    WS-NB-LECT-E01             :
      -    '        *'                   TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-NB-LECT-E01           TO BILA-L-DISP(WS-DISP)(45:9)
      *    Nombre d'écritures du fichier DFUSLS21
           ADD  1                        TO WS-DISP
           MOVE '*    WS-NB-ECRT-S01             :
      -    '        *'                   TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-NB-ECRT-S01           TO BILA-L-DISP(WS-DISP)(45:9)
      *    Nombre de bloc Account Report
           ADD  1                        TO WS-DISP
           MOVE '*    WS-Q-AccountReport         :
      -    '        *'                   TO BILA-L-DISP(WS-DISP)(1:64)
           MOVE WS-Q-AccountReport       TO BILA-L-DISP(WS-DISP)(45:9)
           ADD  1                        TO WS-DISP
           MOVE WS-LIGNE-EGAL            TO BILA-L-DISP(WS-DISP)
      *--- APPEL MODULE ABEND
           PERFORM APPEL-MCCDBILA
           .

      *--------------*
       FIN-PROGRAMME.
      *--------------*
      * FIN DU PROGRAMME
           GOBACK
           .

      *----------------------------------------------------------------
        COPY CANAAFCT.
        COPY CANAABAT.

      * Procedure appel MFUSXL00
        COPY CANAAAPP REPLACING  'MODULE'    BY  'MFUSXL00',
                                 'VERSION'   BY 01,
                                 ==(PREF)==  BY  ==XL00==,
                                 'MODE'      BY 'RIEN'.
       END PROGRAM BFUSEL20.
