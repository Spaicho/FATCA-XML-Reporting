      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID. BFUSEL10
       AUTHOR. CGI
      ******************************************************************
      *                   PROGRAMME BATCH BFUSEL10                     *
      ******************************************************************
      *                                                                *
      *   NOM DU PROGRAMME     : BFUSEL10                              *
      *                                                                *
      *   LIBELLE DU PROGRAMME : Récupérer informations Reporting.     *
      *                                                                *
      *   CODE APPLICATION     : AP10527 - FUS                         *
      *                                                                *
      *   ACTION               : Récupérer informations Reporting depuis
      *                        : la base FUS.                          *
      *                                                                *
      *   LANGAGE PROGRAMME    : COBOL/DB2/REDEMARRABLE                *
      *                                                                *
      *   DATE CREATION        : 27/10/2014                            *
      *                                                                *
      *   AUTEUR               : ZAJA / ZOUHAIR AJABA  / TMA GTC       *
      *                                                                *
      *   REFERENCE            : 20141525: FUS - EVO - Elaborer        *
      *                        :           Reporting FATCA FR et IGA   *
      *                                                                *
      ******************************************************************
      *                           FONCTION
      ******************************************************************
      *                                                                *
      *             >>>  PROGRAMME BMP REDEMARRABLE  <<<               *
      *                                                                *
      *================================================================*
      * AUTEUR !   DATE   !  GAMA  !            MODIFICATIONS          *
      *================================================================*
150978*  CGI   !16/04/2015!20150978!Prises en compte des adaptations   *
150978*        !          !        !pour suivantes :                   *
150978*        !          !        !- Fichier aiguillage en entrée     *
150978*        !          !        !- Fichiers en sortie géré par entité
150978*        !          !        !- Gestion des clients récalcitrants*
150978*        !          !        !- Préparation de la MAJ des tables *
150978*        !          !        !  TBREPFUS, TBRBRFUS et TBFIDFUS   *
      *================================================================*
151197*  CGI   !22/04/2015!20151197!MAJ ECV et Historique pour FATCA3  *
      *----------------------------------------------------------------*
151197*  AAM   !30/04/2015!20151197!- AJOUT DE LA RECHERCHE DES DONNéES*
151197*        !          !        !  FATCA3 VIA LA TABLE TBHISFUS     *
      *----------------------------------------------------------------*
151345*  MCHA  !12/06/2015!20151345!AJOUT MAJ ECV RECALCITRANT         *
      *================================================================*
      *                                                                *
      ******************************************************************
      *  SYNTHESE DES MODULES UTILISEES PAR CE PROGRAMME               *
      ******************************************************************
      *  MCCDINAB : MODULE D'ARRET ANORMAL ET AFFICHAGE DES INFO ABEND.*
      *  MCCDBILA : MODULE D'AFFICHAGE.                                *
      *  MGDATR03 : CODIFICATION D'UNE DATE.                           *
      *  MFUSTEND : Module d'accesseur TBENDFUS                        *
      *  MFUSTADR : Module d'accesseur TBADRFUS                        *
      *  MFUSTCNT : Module d'accesseur TBCNTFUS                        *
      *  MFUSTLCC : Module d'accesseur TBLCCFUS                        *
      *  MFUSTCLI : Module d'accesseur TBCLIFUS                        *
      *  MFUSTCLP : Module d'accesseur TBCLPFUS                        *
      *  MFUSTCLM : Module d'accesseur TBCLMFUS                        *
      *  MFUSTRUB : Module d'accesseur TBRUBFUS                        *
      *  MFUSTLPP : Module d'accesseur TBLPPFUS                        *
151197*  MFUSTHIS : MODULE D'ACCESSEUR TBHISFUS                        *
151197*  MFUSTHID : MODULE D'ACCESSEUR TBHIDFUS                        *
      *================================================================*
      *                  LISTE DES FICHIERS UTILISES                   *
      *================================================================*
      *                                                                *
      *  ________________________________________________________      *
      * |                |     |     |     |                     |     *
      * |  FICHIERS      |  E  |  S  | E/S |   DESCRIPTION       |     *
      * |________________|_____|_____|_____|_____________________|     *
      * |                |     |     |     |                     |     *
      * |-DFUSLE11       |  X  |     |     |-Contient les données|     *
      * |                |     |     |     | à traiter           |     *
      * |                |     |     |     |                     |     *
      * |-DFUSLS11       |     |  X  |     |-Fichier en sortie   |     *
      * |                |     |     |     |                     |     *
      * |________________|_____|_____|_____|_____________________|     *
      *                                                                *
      *================================================================*
      * CODE ABEND |           INTITULE                                *
      *================================================================*
      *  1000      | PB RECUPERATION DATE DU JOUR VIA MGDATR03         *
      *  1001      | PB RECUPERATION DATE DU JOUR VIA MGDATR03         *
      *  1002      | PB APPEL MODULE MFUSTCNT POUR OP CURSEUR          *
      *  1003      | PB APPEL MODULE MFUSTRUB POUR OP CURSEUR          *
      *  1004      | PB APPEL MODULE MFUSTRUB POUR FE CURSEUR          *
      *  1005      | PB APPEL MODULE MFUSTLCC POUR OP CURSEUR          *
      *  1006      | PB APPEL MODULE MFUSTCLI POUR SELECTION           *
      *  1007      | PB APPEL MODULE MFUSTCLP POUR SELECTION           *
      *  1008      | PB APPEL MODULE MFUSTCLM POUR SELECTION           *
      *  1009      | PB APPEL MODULE MFUSTADR POUR OP CURSEUR          *
      *  1010      | PB APPEL MODULE MFUSTLPP POUR OP CURSEUR          *
      *  1011      | PB APPEL MODULE MFUSTLPP POUR FE CURSEUR          *
      *  1012      | PB APPEL MODULE MFUSTCLP POUR SELECTION           *
      *  1013      | PB APPEL MODULE MFUSTADR POUR SELECTION           *
      *  1014      | PB APPEL MODULE MFUSTLCC POUR FE CURSEUR          *
      *  1015      | PB APPEL MODULE MFUSTCNT POUR FE CURSEUR          *
      *  1016      | PB APPEL MODULE MFUSTEND POUR SELECTION           *
      *  1017      | PB APPEL MODULE MFUSTADR POUR FE CURSEUR          *
      *  1018      | PB APPEL MODULE MFUSTADR POUR FE CURSEUR          *
      *  1019      | PB APPEL MODULE MFUSTLCC POUR CL CURSEUR          *
151197*  1020      | PB APPEL MODULE MFUSTHIS POUR OP CURSEUR          *
151197*  1021      | PB APPEL MODULE MFUSTHIS POUR FE CURSEUR          *
151197*  1022      | PB APPEL MODULE MFUSTHIS POUR CL CURSEUR          *
151197*  1023      | PB APPEL MODULE MFUSTHID POUR OP CURSEUR          *
151197*  1024      | PB APPEL MODULE MFUSTHID POUR FE CURSEUR          *
MCHA++*  1025      | PB ADRESSE ENTITE DECLARANTE                      *
      *  2100      | FICHIER EN ENTREE EST VIDE                        *
      *  2200      | ANOMALIE LECTURE FICHIER DFUSLE11                 *
      *  2300      | ANOMALIE LECTURE FICHIER DFUSAI11                 *
150978*  2410      | ECRITURE DANS FICHIER EN SORTIE DFUSLS11          *
"     *  2420      | ECRITURE DANS FICHIER EN SORTIE DFUSLS12          *
"     *  2430      | ECRITURE DANS FICHIER EN SORTIE DFUSLS13          *
"     *  2440      | ECRITURE DANS FICHIER EN SORTIE DFUSLS14          *
"     *  2450      | ECRITURE DANS FICHIER EN SORTIE DFUSLS15          *
"     *  2460      | ECRITURE DANS FICHIER EN SORTIE DFUSLS16          *
"     *  2470      | ECRITURE DANS FICHIER EN SORTIE DFUSLS17          *
"     *  2480      | ECRITURE DANS FICHIER EN SORTIE DFUSLS18          *
150978*  2490      | ECRITURE DANS FICHIER EN SORTIE DFUSLS19          *
      *================================================================*
       DATE-COMPILED.

      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*

      *----------------------*
       CONFIGURATION SECTION.
      *----------------------*

      *SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER. IBM-370.

      *---------------------*
       INPUT-OUTPUT SECTION.
      *---------------------*

      *================================================================*
       DATA DIVISION.
      *================================================================*

      *------------------------*
       WORKING-STORAGE SECTION.
      *------------------------*

      *------- CLAUSES COPY NECESSAIRES AUX COMPOSANTS NAA BATCH ------*
       COPY CWNAAFCT REPLACING 'PROGRAMM' BY 'BFUSEL10'.
       COPY CWNAABAT.
       COPY CZNAAACP.

      *------------- CLAUSE COPY POUR LE MODULE MGDATR03 --------------*
       COPY CZGDA03.

      *------------- CLAUSE COPY POUR LE MODULE MCCDINAB --------------*
       COPY CCCDINAB REPLACING ==(PREF)== BY ==INAB==.

      *------------- CLAUSE COPY POUR LE MODULE MCCDBILA --------------*
       COPY CCCDBILA REPLACING ==(PREF)== BY ==BILA==.
      *
      *------------- DESCRIPTION DES ZCOM  MODULES ACCESSEURS----------*
      *
      * Accesseur de la table MFUSTEND - TBENDFUS
       COPY CFUSTEND REPLACING ==(PREF)== BY ==TEND==.

      * Accesseur de la table MFUSTANO - TBADRFUS
       COPY CFUSTADR REPLACING ==(PREF)== BY ==TADR==.

      * Accesseur de la table MFUSTCNT - TBCNTFUS
       COPY CFUSTCNT REPLACING ==(PREF)== BY ==TCNT==.

      * Accesseur de la table MFUSTLCC - TBLCCFUS
       COPY CFUSTLCC REPLACING ==(PREF)== BY ==TLCC==.

      * Accesseur de la table MFUSTCLI - TBCLIFUS
       COPY CFUSTCLI REPLACING ==(PREF)== BY ==TCLI==.

      * Accesseur de la table MFUSTCLP - TBCLPFUS
       COPY CFUSTCLP REPLACING ==(PREF)== BY ==TCLP==.

      * Accesseur de la table MFUSTCLM - TBCLMFUS
       COPY CFUSTCLM REPLACING ==(PREF)== BY ==TCLM==.

      * Accesseur de la table MFUSTRUB - TBRUBFUS
       COPY CFUSTRUB REPLACING ==(PREF)== BY ==TRUB==.

      * Accesseur de la table MFUSTLPP - TBLPPFUS
       COPY CFUSTLPP REPLACING ==(PREF)== BY ==TLPP==.

150978* Accesseur de la table MFUSTFID - TBFIDFUS
"      COPY CFUSTFID REPLACING ==(PREF)== BY ==TFID==.
"
"     * Accesseur de la table MFUSTCLR - TBCLRFUS
150978 COPY CFUSTCLR REPLACING ==(PREF)== BY ==TCLR==.


151197* ACCESSEUR DE LA TABLE MFUSTHIS - TBHISFUS
151197 COPY CFUSTHIS REPLACING ==(PREF)== BY ==THIS==.

151197* ACCESSEUR DE LA TABLE MFUSTHID - TBHIDFUS
151197 COPY CFUSTHID REPLACING ==(PREF)== BY ==THID==.

      *------------------- CODES FONCTIONS DL1 ------------------------*
       01  WS-CODES-FONCTIONS.
           05  GU                        PIC X(04) VALUE 'GU  '.
           05  GN                        PIC X(04) VALUE 'GN  '.
           05  GHU                       PIC X(04) VALUE 'GHU '.
           05  ISRT                      PIC X(04) VALUE 'ISRT'.
           05  REPL                      PIC X(04) VALUE 'REPL'.
           05  CHKP                      PIC X(04) VALUE 'CHKP'.
           05  XRST                      PIC X(04) VALUE 'XRST'.
           05  OUV                       PIC X(04) VALUE 'OPEN'.
           05  FERM                      PIC X(04) VALUE 'CLSE'.
      *
      *---------------- DESCRIPTION DES FICHIERS ----------------------*
      *
150978*------------ DESCRIPTION DU FICHIER EN ENTREE -----------------*
"     * STRUCTURE DES FICHIERS FATCA DE L AIGUILLAGE
"      COPY CFUSAI00 REPLACING ==(P)== BY ==AI11==.
"     *
"     *------------- CLAUSE COPY DU FICHIER EN SORTIE EL10-------------*
"      01 EL10-CFUSEL10.
"      COPY CFUSEL10 REPLACING ==(P)== BY ==EL10==.
150978*

      *---------------------- ZONE DE TRAVAIL -------------------------*
      *
      *    CODE ABEND
       01  WS-CODE-ABEND                   PIC 9(4) VALUE ZERO.
       01  WS-SQLCODE-EDIT                 PIC -9(4)9.

      *
      *    WORKINGS INTERMEDIAIRES POUR MCCDBILA
      *
       01  WS-LIGNE.
         05  WS-LIGNE-LIB                  PIC X(30).
         05  WS-LIGNE-VAL                  PIC X(30).

       01  WS-LIGNE-FIN.
         05  WS-LN-LIB-FIN                 PIC X(40).
         05  WS-LN-VAL-FIN                 PIC X(20).

      * LIGNES POUR AFFICHAGE SYSOUT DECORATION
       01  WS-LIGNE-DECO1.
           05 WS-LIGNE-DECO10           PIC X(01)  VALUE '*'.
           05 WS-LIGNE-DECO11           PIC X(58)  VALUE ALL '='.
           05 WS-LIGNE-DECO12           PIC X(01)  VALUE ALL '*'.

       01  WS-LIGNE-DECO2.
           05 WS-LIGNE-DECO20           PIC X(10)  VALUE '*==       '.
           05 WS-LIGNE-DECO21           PIC X(40)  VALUE
           'DEBUT D''EXECUTION DU PROGRAMME BFUSEL10 '.
           05 WS-LIGNE-DECO22           PIC X(10)  VALUE '       ==*'.

       01  WS-LIGNE-DECO0.
           05 WS-LIGNE-DECO01            PIC X(10)  VALUE '*==       '.
           05 WS-LIGNE-DECO02            PIC X(40)  VALUE
           '            FIN DU PROGRAMME            '.
           05 WS-LIGNE-DECO03            PIC X(10)  VALUE '       ==*'.

       01  WS-LIGNE-DECO3.
           05 WS-LIGNE-DECO30           PIC X(10)  VALUE '*==       '.
           05 WS-LIGNE-DECO31           PIC X(40)  VALUE SPACES.
           05 WS-LIGNE-DECO32           PIC X(10)  VALUE '       ==*'.

       01  WS-LIGNE-DECO4.
           05 WS-LIGNE-D-COMP           PIC X(08).
           05 WS-LIGNE-DECO31           PIC X(22)  VALUE
           '                     *'.

       01  WS-LIGNE-DECO5.
           05 WS-LIGNE-H-COMP           PIC X(08).
           05 WS-LIGNE-DECO31           PIC X(22)  VALUE
           '                     *'.

       01  WS-LIGNE-DECO6.
           05 WS-LIGNE-D-TRT            PIC X(10).
           05 WS-LIGNE-DECO31           PIC X(20)  VALUE
           '                   *'.

       01  WS-LIGNE-DECO7.
           05 WS-LIGNE-H-TRT            PIC X(08).
           05 WS-LIGNE-DECO31           PIC X(22)  VALUE
           '                     *'.

       01  WS-LIGNE-DECO8.
           05 WS-LIGNE-FQ-CHKP          PIC X(05).
           05 WS-LIGNE-DECO31           PIC X(25)  VALUE
           '                        *'.

       01  WS-LIGNE-ANO0.
           05 WS-LIGNE-DECO01           PIC X(10)  VALUE '*========='.
           05 WS-LIGNE-DECO02           PIC X(40)  VALUE
           '==============  ANOMALIE  =============='.
           05 WS-LIGNE-DECO03           PIC X(10)  VALUE '=========*'.

       01  WS-LIGNE-ANO1.
           05 WS-LIGNE-ANO10            PIC X(18).
           05 WS-LIGNE-ANO11            PIC X(05).
           05 WS-LIGNE-ANO12            PIC X(37)  VALUE
           '                                    *'.

       01  WS-LIGNE-ANO2.
           05 WS-LIGNE-ANO21            PIC X(50).
           05 WS-LIGNE-ANO22            PIC X(10)  VALUE '         *'.

       01  WS-LIGNE-ANO3.
           05 WS-LIGNE-ANO30            PIC X(21).
           05 WS-LIGNE-ANO31            PIC X(36).
           05 WS-LIGNE-ANO32            PIC X(03)  VALUE
           '  *'.

       01  WS-LIGNE-ANO4.
           05 WS-LIGNE-ANO40            PIC X(10).
           05 WS-LIGNE-ANO41            PIC X(40).
           05 WS-LIGNE-ANO42            PIC X(10)  VALUE
           '         *'.

       01  WS-LIGNE-FIN0.
           05 WS-FIN-PART1              PIC X(11).
           05 WS-FIN-PART2              PIC X(09)  VALUE
           '        *'.

      *    VARIABLES POUR LA DATE ET HEURE
       01  W-HEURE-ENT.
           05  W-D-SYSTEME-ISO             PIC X(10).
           05  W-H-SYSTEME-ISO             PIC X(8).
           05  W-D-H-COMPIL.
             10  W-D-COMPIL                PIC X(8).
             10  W-H-COMPIL                PIC X(8).

150978 01  W-DATE-TIMESTAMP                PIC X(24).

      *    VARIABLES POUR FORMATAGE DATE DE COMPILATION
       01  WS-DATE-COMPIL.
           05  W-D-COMPIL-JJ               PIC X(02).
           05  W-D-COMPIL-FIL1             PIC X(1) VALUE '-'.
           05  W-D-COMPIL-MM               PIC X(02).
           05  W-D-COMPIL-FIL2             PIC X(1) VALUE '-'.
           05  W-D-COMPIL-AA               PIC X(02).


      *
150978 01 WS-PARAM.
           05 WS-A-APPL                    PIC X(10).
           05 WS-C-REF-GIIN                PIC X(20).
           05 WS-C-MODE-REPORT             PIC X.

       01 WS-I-IDENT                       PIC 9(17).
       01 WS-I-IDENT-END                   PIC 9(17).
       01 WS-I-IDENT-END-SV                PIC 9(17).
       01 WS-I-IDENT-END-LU                PIC 9(17).

       01 WS-M-MNT-ASS                     PIC S9(18) COMP-3.
150978
"     *  ANNEE FISCALE
"      01 WS-A-ANNEE-FISC                  PIC 9(04).
"
"     *  IDENTIFIANT INTERNE FICHIER REPORTING
"      01 WS-I-IDENT-REF-REP               PIC 9(17).
"
"
"     *  REFERENCE DU REPORTING INITIAL
"      01 WS-I-REF-FIC-INIT                  PIC X(80).
"
"     *  DATE DU REPORTING INITIAL
"      01 WS-D-REF-FIC-INIT                  PIC X(26).
"
"     *  REFERENCE DU REPORTING
"      01 WS-I-REF-FIC.
"     *    TYPE DE REPORTING
"          03  WS-TYPE-REP.
"            05  WS-TYPE-FATCA               PIC X(06).
"            05  WS-FILLER-01                PIC X(01).
"            05  WS-DATE-TIMESTAMP           PIC X(24).
"            05  WS-FILLER-02                PIC X(01).
"            05  WS-GIIN                     PIC X(20).
"            05  FILLER                      PIC X(01).
"          03  WS-TYPE-REP-TST REDEFINES WS-TYPE-REP.
"            05  WS-TYPE-FATCA-TST           PIC X(07).
"            05  WS-FILLER-01-TST            PIC X(01).
"            05  WS-DATE-TIMESTAMP-TST       PIC X(24).
"            05  WS-FILLER-02-TST            PIC X(01).
"            05  WS-GIIN-TST                 PIC X(20).
"
"     *  REFERENCE DU BLOC DU COMPTE
"      01 WS-I-REF-BLOC-CPT.
"     *    TYPE DE REPORTING
"          03  WS-BLOC-CPT.
"            05  WS-TYPE-FATCA               PIC X(06).
"            05  WS-FILLER-01                PIC X(01).
"            05  WS-DATE-TIMESTAMP           PIC X(24).
"            05  WS-FILLER-02                PIC X(01).
"            05  WS-GIIN                     PIC X(20).
"            05  WS-FILLER-03                PIC X(01).
"            05  WS-I-UNIQ-KAC               PIC X(17).
"            05  WS-FILLER-04                PIC X(01).
"            05  WS-I-UNIQ-KPI               PIC X(17).
"            05  FILLER                      PIC X(01).
"          03  WS-BLOC-CPT-TST REDEFINES WS-BLOC-CPT.
"            05  WS-TYPE-FATCA-TST           PIC X(07).
"            05  WS-FILLER-01-TST            PIC X(01).
"            05  WS-DATE-TIMESTAMP-TST       PIC X(24).
"            05  WS-FILLER-02-TST            PIC X(01).
"            05  WS-GIIN-TST                 PIC X(20).
"            05  WS-FILLER-03-TST            PIC X(01).
"            05  WS-I-UNIQ-KAC-TST           PIC X(17).
"            05  WS-FILLER-04-TST            PIC X(01).
"            05  WS-I-UNIQ-KPI-TST           PIC X(17).
"
"
"     *  REFERENCE DU BLOC RECALCITRANT
"      01 WS-I-REF-BLOC-CLR.
"     *    TYPE DE REPORTING
"          03  WS-BLOC-CLR.
"            05  WS-TYPE-FATCA               PIC X(06).
"            05  WS-FILLER-01                PIC X(01).
"            05  WS-DATE-TIMESTAMP           PIC X(24).
"            05  WS-FILLER-02                PIC X(01).
"            05  WS-I-REF-GIIN               PIC X(20).
"            05  WS-FILLER-03                PIC X(01).
"            05  WS-C-TYPE-RECAL             PIC X(08).
"            05  WS-FILLER-04                PIC X(01).
"            05  WS-C-DEV                    PIC X(03).
"            05  FILLER                      PIC X(01).
"          03  WS-BLOC-CLR-TST REDEFINES WS-BLOC-CLR.
"            05  WS-TYPE-FATCA-TST           PIC X(07).
"            05  WS-FILLER-01-TST            PIC X(01).
"            05  WS-DATE-TIMESTAMP-TST       PIC X(24).
"            05  WS-FILLER-02-TST            PIC X(01).
"            05  WS-I-REF-GIIN-TST           PIC X(20).
"            05  WS-FILLER-03-TST            PIC X(01).
"            05  WS-C-TYPE-RECAL-TST         PIC X(08).
"            05  WS-FILLER-04-TST            PIC X(01).
"            05  WS-C-DEV-TST                PIC X(03).
"     *
"     * LISTE DES ENTITES DECLARANTES
"      01 LST-ENT-DECL                PIC X(20) VALUE SPACES.
"        88 TOP-DECL-01               VALUE  '1G159I.00000.BR.492'.
"        88 TOP-DECL-02               VALUE  '1G159I.00000.LE.250'.
"        88 TOP-DECL-03               VALUE  '1G159I.00352.ME.876'.
"        88 TOP-DECL-04               VALUE  '1G159I.00353.BR.663'.
"        88 TOP-DECL-05               VALUE  '1G159I.00353.ME.250'.
"        88 TOP-DECL-06               VALUE  '1G159I.00356.ME.250'.
"        88 TOP-DECL-07               VALUE  '1G159I.00357.ME.250'.
"        88 TOP-DECL-08               VALUE  '1G159I.00359.ME.540'.
"        88 TOP-DECL-09               VALUE  '1G159I.00360.ME.250'.
"
"     *   COMPTEURS
"      01  WS-COMPTEURS.
"     *    NOMBRE D'ENREGISTREMENT LUS
"          05  WS-CPT-AIG-LUS              PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT LUS 10
"          05  WS-CPT-AIG-LUS-10           PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT LUS 20
"          05  WS-CPT-AIG-LUS-20           PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT LUS
"          05  WS-CPT-PAR-LUS              PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS TOTAL
"          05  WS-CPT-ECRIT                PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS11
"          05  WS-EDT1-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS12
"          05  WS-EDT2-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS13
"          05  WS-EDT3-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS14
"          05  WS-EDT4-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS15
"          05  WS-EDT5-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS16
"          05  WS-EDT6-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS17
"          05  WS-EDT7-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS18
"          05  WS-EDT8-ECRIT               PIC 9(11).
"     *    NOMBRE D'ENREGISTREMENT éCRITS DFUSLS19
"          05  WS-EDT9-ECRIT               PIC 9(11).
"     *    NOMBRE DE PERSONNES DéCLARéES
"          05 WS-Q-PERS-DECL               PIC 9(09).
"     *    NOMBRE DE CONTRATS DéCLARéS
"          05 WS-Q-CONTRAT-DECL            PIC 9(09).
"     *    NOMBRE DE RéCALCITRANT DéCLARéES
"          05 WS-Q-RECAL-DECL              PIC 9(09).
"
"     *   MONTANTS
"      01  WS-MONTANTS.
"     *    Montant total de solde déclaré
"          05 WS-M-TOT-SOL-DECL               PIC 9(018).
"     *    Montant total d'intérêt déclaré
"          05 WS-M-TOT-INT-DECL               PIC 9(018).
"     *    Montant total de solde déclaré
"          05 WS-M-TOT-CES-DECL               PIC 9(018).
"     *    Montant total autre montant déclaré
"          05 WS-M-TOT-M-DECL                 PIC 9(018).
"
"     *   INDiCES
"      01  WS-INDICES.
"     *    Indice I
150978     05  WS-I                        PIC 9(11).
      *   COMPTEUR D'OCCURENCES POUR LES PRISES DE CHECKPOINT
       01  WS-CPT-CHKPT                    PIC 9(5).
      *   COMPTEUR NOMBRE DE CHECKPOINT EFFECTUéS
       01  WS-CPT-NBRE-CHECK               PIC 9(5).
      *   INDICATEUR FIN TRAITEMENT
       01  IND-FIN-FIC                     PIC X(1).
150978 01  IND-FIN-TRT                     PIC X(1).
150978 01  IND-FIN-TRT-LCC                 PIC X(1).
       01  IND-FIN-TRT-RUB                 PIC X(1).
151197 01  IND-FIN-TRT-HIS                 PIC X(1).
151197 01  IND-FIN-TRT-HID                 PIC X(1).
       01  IND-LCC-OK                      PIC X(1).
       01  IND-TRT-ADR                     PIC X(1).
       01  IND-TRT-LPP                     PIC X(1).
      *
151197* ENREGISTREMENT HIS TROUVE
151197 01 ENR-HIS                     PIC X(1) VALUE '0'.
151197   88 ENR-HIS-TRV               VALUE  '1'.
151197   88 ENR-HIS-NN-TRV            VALUE  '0'.


151197* LA VALEUR AVANT LA MODIFICATION DU HID SAUVEGARDéE
151197 01 W-N-OPE               PIC X(18) VALUE SPACES.
151197 01 W-I-UNIQ-KAC          PIC X(17) VALUE SPACES.
MC     01 W-I-RIB-INVAR         PIC X(23) VALUE SPACES.
151197 01 W-C-IBAN              PIC X(30) VALUE SPACES.
151197 01 W-C-DEV               PIC X(3) VALUE SPACES.
151197 01 W-M-MNT-ASS           PIC S9(18) COMP-3 VALUE ZEROS.
151197 01 W-M-MNT-ASS-06        PIC S9(18) COMP-3 VALUE ZEROS.
151197 01 TEMP-M-MNT-ASS        PIC X(18).
151197 01 TEMP-M-MNT-ASS-06     PIC X(18).
151197 01 T1-M-MNT-ASS          PIC 9(18).
151197 01 T1-M-MNT-ASS-06       PIC 9(18).

151197* VALEUR TEMPORAIRE DES DONNEES DE LA TABLE CONTRAT
151197 01 TEMP-I-UNIQ-KAC       PIC X(17) VALUE SPACES.
MC     01 TEMP-I-RIB-INVAR      PIC X(23) VALUE SPACES.
151197 01 TEMP-C-IBAN           PIC X(30) VALUE SPACES.
151197 01 TEMP-C-DEV            PIC X(3) VALUE SPACES.
MC+    01 WS-I-IDENT-1          PIC S9(17) COMP-3.

MC+   * VALEUR TEMPORAIRE DES DONNEES DE LA TABLE CLIENT
"     *      IDENTIFIANT UNIQUE PERSONNE
"      01 W-ENR-CLT-ADR.
"            10 W-I-UNIQ-KPI                   PIC X(017).
"
"     *      NATURE PERSONNE                                            00027000
"            10 W-C-NTUR-PERS                 PIC X(002).
"
"     *      RéFéRENCE FISCALITé éTRANGèRE                              00027000
"            10 W-C-REF-GIIN                  PIC X(020).
"
"     *      NOM PATRONYMIQUE (PP)
"            10 W-L-NOM-NAISS                  PIC X(032).
"
"     *      TITRE CIVILITE (PP)
"            10 W-C-TITRE-CVLTE                PIC X(001).
"
"     *      PRENOM
"            10 W-L-PRNOM                       PIC X(032).
"
"     *      DATE NAISSANCE (PP)
"            10 W-D-NAISS                      PIC X(010).
"
"     *      NOM MARITAL (PP)
"            10 W-L-NOM-MRTL                   PIC X(032).
"
"     *      CODE PAYS NATIONALITE (PP)
"            10 W-C-PAYS-NLITE                 PIC X(002).
"
"     *      CODE PAYS AUTRE NATIONALITE (PP)
"            10 W-C-AUTRE-PAYS-NLITE           PIC X(002).
"
"     *      CODE PAYS DE NAISSANCE (PP)
"            10 W-C-PAYS-NAISS                 PIC X(002).
"
"     *      CODE DEPARTEMENT NAISSANCE (PP)
"            10 W-C-DEPT-NAISS                 PIC X(002).
"
"     *      VILLE DE NAISSANCE (PP)
"            10 W-L-VILL-NAISS                 PIC X(032).
"
"     *      RAISON SOCIALE (PM)
"            10 W-L-RAISON-SOCIALE             PIC X(060).
"
"     *       Classification d'une entité
"            10 W-C-TYPE-CLASS                 PIC X(008).
"
"     *      Données adresses
"     *      NOM COMMUNE ADRESSE
"            10 W-L-COMM-ADR                   PIC X(032).
"
"     *      CODE POSTALE ADRESSE
"            10 W-C-CPOST                      PIC X(005).
"
"     *      CODE PAYS ISO DE L'ADRESSE
"            10 W-C-PAYS-ADR                   PIC X(002).
"
"     *      INTITULé COURRIER LIGNE 1 ADRESSE
"            10 W-L-INTIT-COURR-1              PIC X(038).
"
"     *      INTITULé COURRIER LIGNE 2 ADRESSE
"            10 W-L-INTIT-COURR-2              PIC X(038).
"
"     *      ADRESSE 1
"            10 W-L-ADR-LIGNE-1                PIC X(038).
"
"     *      ADRESSE 2
"            10 W-L-ADR-LIGNE-2                PIC X(038).
"
"     *      ADRESSE 3
"            10 W-L-ADR-LIGNE-3                PIC X(038).
"
"     *      LIBELLé PAYS DE L'ADRESSE
"            10 W-L-PAYS-ADR                   PIC X(038).
MC+
MC+   * VALEUR TEMPORAIRE DES DONNEES DE LA TABLE CLIENT
"     *      IDENTIFIANT UNIQUE PERSONNE
"            10 TEMP-I-UNIQ-KPI                PIC X(017).
"
"     *      NATURE PERSONNE                                            00027000
"            10 TEMP-C-NTUR-PERS              PIC X(002).
"
"     *      RéFéRENCE FISCALITé éTRANGèRE                              00027000
"            10 TEMP-C-REF-GIIN               PIC X(020).
"
"     *      NOM PATRONYMIQUE (PP)
"            10 TEMP-L-NOM-NAISS               PIC X(032).
"
"     *      TITRE CIVILITE (PP)
"            10 TEMP-C-TITRE-CVLTE             PIC X(001).
"
"     *      PRENOM
"            10 TEMP-L-PRNOM                  PIC X(032).
"
"     *      DATE NAISSANCE (PP)
"            10 TEMP-D-NAISS                   PIC X(010).
"
"     *      NOM MARITAL (PP)
"            10 TEMP-L-NOM-MRTL                PIC X(032).
"
"     *      CODE PAYS NATIONALITE (PP)
"            10 TEMP-C-PAYS-NLITE              PIC X(002).
"
"     *      CODE PAYS AUTRE NATIONALITE (PP)
"            10 TEMP-C-AUTRE-PAYS-NLITE        PIC X(002).
"
"     *      CODE PAYS DE NAISSANCE (PP)
"            10 TEMP-C-PAYS-NAISS              PIC X(002).
"
"     *      CODE DEPARTEMENT NAISSANCE (PP)
"            10 TEMP-C-DEPT-NAISS              PIC X(002).
"
"     *      VILLE DE NAISSANCE (PP)
"            10 TEMP-L-VILL-NAISS              PIC X(032).
"
"     *      RAISON SOCIALE (PM)
"            10 TEMP-L-RAISON-SOCIALE          PIC X(060).
"
"     *       Classification d'une entité
"            10 TEMP-C-TYPE-CLASS              PIC X(008).
"
"     *      Données adresses
"     *      NOM COMMUNE ADRESSE
"            10 TEMP-L-COMM-ADR                PIC X(032).
"
"     *      CODE POSTALE ADRESSE
"            10 TEMP-C-CPOST                   PIC X(005).
"
"     *      CODE PAYS ISO DE L'ADRESSE
"            10 TEMP-C-PAYS-ADR                PIC X(002).
"
"     *      INTITULé COURRIER LIGNE 1 ADRESSE
"            10 TEMP-L-INTIT-COURR-1           PIC X(038).
"
"     *      INTITULé COURRIER LIGNE 2 ADRESSE
"            10 TEMP-L-INTIT-COURR-2           PIC X(038).
"
"     *      ADRESSE 1
"            10 TEMP-L-ADR-LIGNE-1             PIC X(038).
"
"     *      ADRESSE 2
"            10 TEMP-L-ADR-LIGNE-2             PIC X(038).
"
"     *      ADRESSE 3
"            10 TEMP-L-ADR-LIGNE-3             PIC X(038).
"
"     *      LIBELLé PAYS DE L'ADRESSE
"            10 TEMP-L-PAYS-ADR                PIC X(038).
MC+
      *  Le nombre de décimaux du montant associé au compte
       01 WS-Q-NBR-DEC                 PIC S9(1) VALUE ZERO.
      *
      *------- ZONE DE SAUVEGARDE POUR LES PRISES DE CHECKPOINT -------*
      *
       01  W999-SAUVE-LG                   PIC 9(9) BINARY.
       01  W999-SAUVE.
           05  W999-I-IDENT-END            PIC 9(17).
150978     05  W999-CPT-AIG-LUS            PIC 9(11).
"          05  W999-CPT-AIG-LUS-10         PIC 9(11).
"          05  W999-CPT-AIG-LUS-20         PIC 9(11).
"          05  W999-CPT-PAR-LUS            PIC 9(11).
"          05  W999-CPT-ECRIT              PIC 9(11).
"          05  W999-EDT1-ECRIT             PIC 9(11).
"          05  W999-EDT2-ECRIT             PIC 9(11).
"          05  W999-EDT3-ECRIT             PIC 9(11).
"          05  W999-EDT4-ECRIT             PIC 9(11).
"          05  W999-EDT5-ECRIT             PIC 9(11).
"          05  W999-EDT6-ECRIT             PIC 9(11).
"          05  W999-EDT7-ECRIT             PIC 9(11).
"          05  W999-EDT8-ECRIT             PIC 9(11).
"          05  W999-EDT9-ECRIT             PIC 9(11).
"          05  W999-A-APPL                 PIC X(10).
"          05  W999-I-IDENT-REF-REP        PIC 9(17).
"          05  W999-NB-CHECKPOINT          PIC 9(05).
"          05  W999-Q-PERS-DECL            PIC 9(09).
"          05  W999-Q-CONTRAT-DECL         PIC 9(09).
"          05  W999-Q-RECAL-DECL           PIC 9(09).
"          05  W999-M-TOT-SOL-DECL         PIC 9(018).
"          05  W999-M-TOT-INT-DECL         PIC 9(018).
"          05  W999-M-TOT-CES-DECL         PIC 9(018).
"          05  W999-M-TOT-M-DECL           PIC 9(018).
"          05  W999-ENT-DECL               PIC X(20).
"          05  W999-I-REF-FIC              PIC X(53).
"          05  W999-I-REF-BLOC-CLR         PIC X(66).
150978     05  W999-I-REF-BLOC-CPT         PIC X(89).

150978 01  WS-C-MOD-REPORT                 PIC X(01).
150978 01  SYSIN-80.
150978     05  SYSIN-TYPE-FATCA            PIC X(07).
150978     05  SYSIN-FILLER                PIC X(13).
150978 01  SYSIN-TYPE-FATCA13              PIC X(09).
      *
      *----------------*
       LINKAGE SECTION.
      *----------------*

      *
      *------------------------- IO-PCB -------------------------------*
      *** PCB TP demandes dans la norme theke 2334
       01  PCB-ALT              PIC X(32).
       01  PCB-EXP              PIC X(32).
      *
      *
       01  IO-PCB.
           05 PCB-IO-NOM                 PIC X(08).
           05 FILLER                     PIC X(02).
           05 PCB-IO-CODRET              PIC X(02).
           05 PCB-IO-DATE                PIC S9(07) COMP-3.
           05 PCB-IO-HEURE               PIC S9(07) COMP-3.
      *
      *-------------------- PCB DES FICHIERS --------------------------*
      *

      * FICHIER DFUSLE11

       01  PCB-DFUSLE11.
           05 DBDNAME-FUSEL01        PIC X(08).
           05 NIV-FUSEL01            PIC X(02).
           05 STAT-FUSEL01           PIC X(02).
           05 PROCOPT-FUSEL01        PIC X(04).
           05 FILLER                 PIC X(20).
           05 KEY-FUSEL01            PIC X(08).
           05 FILLER                 PIC X(20).


      * FICHIER DFUSLS11

       01  PCB-DFUSLS11.
           05 DBDNAME-FUSEL10        PIC X(08).
           05 NIV-FUSEL10            PIC X(02).
           05 STAT-FUSEL10           PIC X(02).
           05 PROCOPT-FUSEL10        PIC X(04).
           05 FILLER                 PIC X(20).
           05 KEY-FUSEL10            PIC X(08).
           05 FILLER                 PIC X(20).


150978* FICHIER DFUSLS12
"
"      01  PCB-DFUSLS12.
"          05 DBDNAME-FUSEL20        PIC X(08).
"          05 NIV-FUSEL20            PIC X(02).
"          05 STAT-FUSEL20           PIC X(02).
"          05 PROCOPT-FUSEL20        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL20            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS13
"
"      01  PCB-DFUSLS13.
"          05 DBDNAME-FUSEL30        PIC X(08).
"          05 NIV-FUSEL30            PIC X(02).
"          05 STAT-FUSEL30           PIC X(02).
"          05 PROCOPT-FUSEL30        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL30            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS14
"
"      01  PCB-DFUSLS14.
"          05 DBDNAME-FUSEL40        PIC X(08).
"          05 NIV-FUSEL40            PIC X(02).
"          05 STAT-FUSEL40           PIC X(02).
"          05 PROCOPT-FUSEL40        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL40            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS15
"
"      01  PCB-DFUSLS15.
"          05 DBDNAME-FUSEL50        PIC X(08).
"          05 NIV-FUSEL50            PIC X(02).
"          05 STAT-FUSEL50           PIC X(02).
"          05 PROCOPT-FUSEL50        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL50            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS16
"
"      01  PCB-DFUSLS16.
"          05 DBDNAME-FUSEL60        PIC X(08).
"          05 NIV-FUSEL60            PIC X(02).
"          05 STAT-FUSEL60           PIC X(02).
"          05 PROCOPT-FUSEL60        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL60            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS17
"
"      01  PCB-DFUSLS17.
"          05 DBDNAME-FUSEL70        PIC X(08).
"          05 NIV-FUSEL70            PIC X(02).
"          05 STAT-FUSEL70           PIC X(02).
"          05 PROCOPT-FUSEL70        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL70            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS18
"
"      01  PCB-DFUSLS18.
"          05 DBDNAME-FUSEL80        PIC X(08).
"          05 NIV-FUSEL80            PIC X(02).
"          05 STAT-FUSEL80           PIC X(02).
"          05 PROCOPT-FUSEL80        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL80            PIC X(08).
"          05 FILLER                 PIC X(20).
"
"     * FICHIER DFUSLS19
"
"      01  PCB-DFUSLS19.
"          05 DBDNAME-FUSEL90        PIC X(08).
"          05 NIV-FUSEL90            PIC X(02).
"          05 STAT-FUSEL90           PIC X(02).
"          05 PROCOPT-FUSEL90        PIC X(04).
"          05 FILLER                 PIC X(20).
"          05 KEY-FUSEL90            PIC X(08).
150978     05 FILLER                 PIC X(20).


      *------------------- PROCEDURE DIVISION -------------------------*
      *
       PROCEDURE DIVISION USING IO-PCB,
                                PCB-ALT
                                PCB-EXP
                                PCB-DFUSLE11
                                PCB-DFUSLS11
150978                          PCB-DFUSLS12
"                               PCB-DFUSLS13
"                               PCB-DFUSLS14
"                               PCB-DFUSLS15
"                               PCB-DFUSLS16
"                               PCB-DFUSLS17
"                               PCB-DFUSLS18
150978                          PCB-DFUSLS19.
      *
      *
      * CODE UTILE à L'AFFICHAGE DE LA SYSOUT EN MODE DEBUG
      *
      DDECLARATIVES.
      DTRACING SECTION.
      D    USE FOR DEBUGGING ON ALL PROCEDURES.
      DP10.
      D    DISPLAY 'BFUSEL10 / ' DEBUG-LINE ' / ' DEBUG-NAME.
      DEND DECLARATIVES.
      *
      *
       DEBUT.
           PERFORM 1000-DEBUT-PGM
           PERFORM 2000-TRT-PGM
           PERFORM 4000-FIN-PGM
           .

      *
      *====================*
      * DEBUT DU PROGRAMME *
      *====================*
       1000-DEBUT-PGM.
      *
      *--> INITIALISATION DU MECANISME DE REDEMARRAGE
           PERFORM 1100-INIT-REDEMARRAGE

      *--> INITIALISATION DES VARIABLES DE TRAVAIL
           PERFORM 1200-INITIALISATIONS

      *--> TEST DE REDEMARRAGE DU PROGRAMME
           PERFORM 1300-TEST-REDEMARRAGE

150978*--> LECTURE DU FICHIER SYSIN
150978     PERFORM 1301-LECTURE-SYSIN

      *--> AFFICHAGE COMPTE DE DEBUT D'EXECUTION
           PERFORM 1400-AFFICHAGE-DEBUT

150978*--> 1ERE LECTURE DU FICHIER AIGUILLAGE
150978*    PERFORM 1500-LECTURE-FICHIER-AIG
           .
      *                         ***                             *
      *                         ***                             *
      *---------------------------------------------------------*
      *                   PARAGRAPHES 1XXX
      *---------------------------------------------------------*

      *=========================================================*
      * INITIALISATIONS OBLIGATOIRES POUR LES BMP REDEMARRABLES *
      *=========================================================*
       1100-INIT-REDEMARRAGE.
      *
           PERFORM SQ-INIT-PGM-BATCH

      *--> RECUPERATION EN SYSIN DE LA FREQUENCE DES CHECKPOINTS
           PERFORM SQ-LECTURE-FREQCHKP

           MOVE LENGTH OF W999-SAUVE  TO W999-SAUVE-LG

      *--> DETERMINATION DU MODE DE TRAITEMENT
           PERFORM SQ-RESTART
           .

      *
      *==============================*
      * INITIALISATION DES VARIABLES *
      *==============================*
       1200-INITIALISATIONS.
      *
      *    INITIALISATION DES PARAMETRES DES MODULES D'AFFICHAGE
      *    EN SYSOUT
      *
           MOVE 'BFUSEL10'               TO  BILA-C-PGM-APPEL
                                             INAB-C-PGM-APPEL
           MOVE  1                       TO  BILA-NO-VERSION
                                             INAB-NO-VERSION
      *
      *    INITIALISATION INDICE DE FIN TRAITEMENT
           MOVE  'N'                     TO  IND-FIN-TRT
150978     MOVE  ZERO                    TO  WS-I-IDENT
           .

      *
      *==============================*
      * TEST DE REDEMARRAGE          *
      *==============================*
       1300-TEST-REDEMARRAGE.
      *
           IF W999-GCXT-INDICE = 'R'
      *-----> LANCEMENT EN MODE REPRISE
              PERFORM 1310-MODE-REPRISE

      *
150978        MOVE  W999-I-IDENT-END     TO   WS-I-IDENT-END-SV
150978        MOVE  W999-I-IDENT-REF-REP TO   WS-I-IDENT-REF-REP
              MOVE  W999-CPT-AIG-LUS     TO   WS-CPT-AIG-LUS
150978        MOVE  W999-CPT-AIG-LUS-10  TO   WS-CPT-AIG-LUS-10
"             MOVE  W999-CPT-AIG-LUS-20  TO   WS-CPT-AIG-LUS-20
"             MOVE  W999-CPT-ECRIT       TO   WS-CPT-ECRIT
"             MOVE  W999-EDT1-ECRIT      TO   WS-EDT1-ECRIT
"             MOVE  W999-EDT2-ECRIT      TO   WS-EDT2-ECRIT
"             MOVE  W999-EDT3-ECRIT      TO   WS-EDT3-ECRIT
"             MOVE  W999-EDT4-ECRIT      TO   WS-EDT4-ECRIT
"             MOVE  W999-EDT5-ECRIT      TO   WS-EDT5-ECRIT
"             MOVE  W999-EDT6-ECRIT      TO   WS-EDT6-ECRIT
"             MOVE  W999-EDT7-ECRIT      TO   WS-EDT7-ECRIT
"             MOVE  W999-EDT8-ECRIT      TO   WS-EDT8-ECRIT
"             MOVE  W999-EDT9-ECRIT      TO   WS-EDT9-ECRIT
"             MOVE  W999-A-APPL          TO   WS-A-APPL
"             MOVE  W999-NB-CHECKPOINT   TO   WS-CPT-NBRE-CHECK
"             MOVE  W999-Q-PERS-DECL     TO   WS-Q-PERS-DECL
"             MOVE  W999-Q-CONTRAT-DECL  TO   WS-Q-CONTRAT-DECL
"             MOVE  W999-Q-RECAL-DECL    TO   WS-Q-RECAL-DECL
"             MOVE  W999-M-TOT-SOL-DECL  TO   WS-M-TOT-SOL-DECL
"             MOVE  W999-M-TOT-INT-DECL  TO   WS-M-TOT-INT-DECL
"             MOVE  W999-M-TOT-CES-DECL  TO   WS-M-TOT-CES-DECL
"             MOVE  W999-M-TOT-M-DECL    TO   WS-M-TOT-M-DECL
"             MOVE  W999-ENT-DECL        TO   LST-ENT-DECL
"             MOVE  W999-I-REF-FIC       TO   WS-I-REF-FIC
"             MOVE  W999-I-REF-BLOC-CLR  TO   WS-I-REF-BLOC-CLR
150978        MOVE  W999-I-REF-BLOC-CPT  TO   WS-I-REF-BLOC-CPT

              IF W999-CPT-ECRIT = ZERO OR LOW-VALUE
                 MOVE  ZERO                 TO   WS-CPT-AIG-LUS
                                                 WS-CPT-AIG-LUS-10
                                                 WS-CPT-AIG-LUS-20
                                                 WS-CPT-ECRIT
                                                 WS-EDT1-ECRIT
                                                 WS-EDT2-ECRIT
                                                 WS-EDT3-ECRIT
                                                 WS-EDT4-ECRIT
                                                 WS-EDT5-ECRIT
                                                 WS-EDT6-ECRIT
                                                 WS-EDT7-ECRIT
                                                 WS-EDT8-ECRIT
                                                 WS-EDT9-ECRIT
                                                 WS-CPT-PAR-LUS
                                                 WS-CPT-NBRE-CHECK
                                                 WS-Q-PERS-DECL
                                                 WS-Q-CONTRAT-DECL
                                                 WS-Q-RECAL-DECL
                                                 WS-M-TOT-SOL-DECL
                                                 WS-M-TOT-INT-DECL
                                                 WS-M-TOT-CES-DECL
                                                 WS-M-TOT-M-DECL
              END-IF

150978*-->    1ERE LECTURE DU FICHIER AIGUILLAGE
150978        PERFORM 1500-LECTURE-FICHIER-AIG-REP
           ELSE
      *-----> LANCEMENT EN MODE NORMAL
              MOVE  ZERO                 TO   WS-CPT-AIG-LUS
                                              WS-CPT-AIG-LUS-10
                                              WS-CPT-AIG-LUS-20
                                              WS-CPT-ECRIT
150978                                        WS-EDT1-ECRIT
"                                             WS-EDT2-ECRIT
"                                             WS-EDT3-ECRIT
"                                             WS-EDT4-ECRIT
"                                             WS-EDT5-ECRIT
"                                             WS-EDT6-ECRIT
"                                             WS-EDT7-ECRIT
"                                             WS-EDT8-ECRIT
"                                             WS-EDT9-ECRIT
"                                             WS-CPT-PAR-LUS
"                                             WS-CPT-NBRE-CHECK
"                                             WS-Q-PERS-DECL
"                                             WS-Q-CONTRAT-DECL
"                                             WS-Q-RECAL-DECL
"                                             WS-M-TOT-SOL-DECL
"                                             WS-M-TOT-INT-DECL
"                                             WS-M-TOT-CES-DECL
"                                             WS-M-TOT-M-DECL
150978
      *-----> LANCEMENT EN MODE REPRISE
              PERFORM 1320-MODE-NORMAL

150978*-->    1ERE LECTURE DU FICHIER AIGUILLAGE
150978        PERFORM 1500-LECTURE-FICHIER-AIG

           END-IF
      *
           .
      *
      *========================================*
      * LANCEMENT DU PROGRAMME EN MODE REPRISE *
      *==============================*
150978 1301-LECTURE-SYSIN.
"     *    LECTURE SYSIN
"          ACCEPT SYSIN-80
MCHA!!     IF SYSIN-80(1:9) = 'FATCA31A '
"             MOVE 'FATCA3 '  TO SYSIN-TYPE-FATCA
"          END-IF
"          IF SYSIN-80(1:9) = 'FATCA31A1'
"             MOVE 'FATCA13'  TO SYSIN-TYPE-FATCA
"          END-IF
"          IF SYSIN-80(1:9) = 'FATCA31I '
"             MOVE 'FATCA1 '  TO SYSIN-TYPE-FATCA
"          END-IF
"          IF SYSIN-80(1:9) = 'FATCA31I1'
"             MOVE 'FATCA11'  TO SYSIN-TYPE-FATCA
"          END-IF
"          MOVE SYSIN-80(1:9) TO SYSIN-TYPE-FATCA13
"          MOVE '1'           TO SYSIN-TYPE-FATCA13(7:1)
"     *
150978     .
      *
      *========================================*
      * LANCEMENT DU PROGRAMME EN MODE REPRISE *
      *========================================*
       1310-MODE-REPRISE.
      *
      * AFFICHAGE DANS LA SYSOUT DU MODE DE LANCEMENT

      * LANCEMENT EN MODE REPRISE
           MOVE  1                       TO   BILA-Q-LIST-DISP
           MOVE  'LANCEMENT EN MODE REPRISE '
                                         TO   WS-LIGNE-LIB
           MOVE  SPACE
                                         TO   WS-LIGNE-VAL
           MOVE  WS-LIGNE                TO   BILA-L-DISP (1)

      * APPEL MCCDBILA
           CALL 'MCCDBILA'         USING   BILA-PARAM
150978D    DISPLAY 'W999-I-IDENT-END     :'W999-I-IDENT-END
150978D    DISPLAY 'W999-I-IDENT-REF-REP :'W999-I-IDENT-REF-REP
150978D    DISPLAY 'W999-CPT-AIG-LUS     :'W999-CPT-AIG-LUS
150978D    DISPLAY 'W999-CPT-AIG-LUS-10  :'W999-CPT-AIG-LUS-10
150978D    DISPLAY 'W999-CPT-AIG-LUS-20  :'W999-CPT-AIG-LUS-20
150978D    DISPLAY 'W999-CPT-ECRIT       :'W999-CPT-ECRIT
150978D    DISPLAY 'W999-EDT1-ECRIT      :'W999-EDT1-ECRIT
150978D    DISPLAY 'W999-EDT2-ECRIT      :'W999-EDT2-ECRIT
150978D    DISPLAY 'W999-EDT3-ECRIT      :'W999-EDT3-ECRIT
150978D    DISPLAY 'W999-EDT4-ECRIT      :'W999-EDT4-ECRIT
150978D    DISPLAY 'W999-EDT5-ECRIT      :'W999-EDT5-ECRIT
150978D    DISPLAY 'W999-EDT6-ECRIT      :'W999-EDT6-ECRIT
150978D    DISPLAY 'W999-EDT7-ECRIT      :'W999-EDT7-ECRIT
150978D    DISPLAY 'W999-EDT8-ECRIT      :'W999-EDT8-ECRIT
150978D    DISPLAY 'W999-EDT9-ECRIT      :'W999-EDT9-ECRIT
150978D    DISPLAY 'W999-A-APPL          :'W999-A-APPL
150978D    DISPLAY 'W999-NB-CHECKPOINT   :'W999-NB-CHECKPOINT
150978D    DISPLAY 'W999-Q-PERS-DECL     :'W999-Q-PERS-DECL
150978D    DISPLAY 'W999-Q-CONTRAT-DECL  :'W999-Q-CONTRAT-DECL
150978D    DISPLAY 'W999-Q-RECAL-DECL    :'W999-Q-RECAL-DECL
150978D    DISPLAY 'W999-M-TOT-SOL-DECL  :'W999-M-TOT-SOL-DECL
150978D    DISPLAY 'W999-M-TOT-INT-DECL  :'W999-M-TOT-INT-DECL
150978D    DISPLAY 'W999-M-TOT-CES-DECL  :'W999-M-TOT-CES-DECL
150978D    DISPLAY 'W999-M-TOT-M-DECL    :'W999-M-TOT-M-DECL
150978D    DISPLAY 'W999-ENT-DECL        :'W999-ENT-DECL
150978D    DISPLAY 'W999-I-REF-FIC       :'W999-I-REF-FIC
150978D    DISPLAY 'W999-I-REF-BLOC-CLR  :'W999-I-REF-BLOC-CLR
150978D    DISPLAY 'W999-I-REF-BLOC-CPT  :'W999-I-REF-BLOC-CPT
           .
      *
      *============================================================*
      * LANCEMENT DU PROGRAMME EN MODE NORMAL (PAS DE REDEMARRAGE) *
      *============================================================*
       1320-MODE-NORMAL.
      *
      * AFFICHAGE DANS LA SYSOUT DU MODE DE LANCEMENT
      *
           MOVE  1                       TO   BILA-Q-LIST-DISP
           MOVE  'LANCEMENT EN MODE NORMAL '
                                         TO   WS-LIGNE-LIB
           MOVE  SPACE                   TO   WS-LIGNE-VAL

           MOVE  WS-LIGNE                TO   BILA-L-DISP (1)

      * APPEL MCCDBILA
           CALL 'MCCDBILA'         USING   BILA-PARAM
           .

      *==================================================*
      * RECHERCHE DES INFORMATIONS DE L'ENTITé DéCLARANTE*
      *==================================================*
       1330-RECH-INF-END.
      *
      * RECHERCHE ET éCRITURE DES INFORMATIONS END
      *
      * ==> ACCéDER LA BASE FUS TABLE DES END
           PERFORM 1350-SEL-INF-END

      * ==> ALIMENTATION DE LA LIGNE NUMéRO ZéRO
           PERFORM 1361-ALIM-INF-END

MCHA+-D    DISPLAY 'SYSIN-TYPE-FATCA ' SYSIN-TYPE-FATCA
MCHA+-     IF SYSIN-TYPE-FATCA NOT =  'FATCA1 '
MCHA+-        AND SYSIN-TYPE-FATCA NOT =  'FATCA11'
"     * ==> ALIMENTATION DONNEES FATCA 2 3 4
"             PERFORM 1361-TRT-FATCA234
MCHA+-     END-IF

      * ==> ECRITURE LIGNE IDENTIFIANT END
           PERFORM 1360-ECRIRE-OUT

150978* ==> ALIMENTATION LIGNE IDENTIFIANT FID
150978     PERFORM 1362-ALIM-REF-FID
150978
150978* ==> ECRITURE LIGNE IDENTIFIANT FID
150978     PERFORM 1363-INS-REF-FID
150978
150978* ==> récupérer la rédérence IDENTIFIANT FID ajouté en séquence
150978     PERFORM 1364-RECUP-REF-FID

      * ==> ACCéDER LA BASE FUS TABLE DES ADRESSE
           PERFORM 1370-SEL-INF-ADR

      * ==> ALIMENTATION DE LA LIGNE DES ADRESSES
           PERFORM 1381-ALIM-INF-ADR

      * ==> ECRITURE LIGNE ADRESSE
           PERFORM 1360-ECRIRE-OUT
           .


      *                   ***                            *
      *==================================================*
      * ACCèS à LA BASE FUS (TBENDFUS)                   *
      *==================================================*
       1350-SEL-INF-END.
      *
      * APPEL MODULE MFUSTEND POUR UNE SELECTION
      * DANS LA TABLE TBFUSEND
      *
150978     MOVE 'SE'                  TO ACCE-TYPE-REQUETE              03753799
"          MOVE '01-SEL'              TO ACCE-NOM-FONCTION
"          MOVE WS-A-APPL             TO TEND-A-APPL
150978     MOVE AI11-I-IDENT-END      TO TEND-I-IDENT-END
      D    DISPLAY ' TEND-A-APPL      ==> ' TEND-A-APPL
      D    DISPLAY ' TEND-I-IDENT-END ==> ' TEND-I-IDENT-END

      * APPEL MFUSTEND
           PERFORM SQ-ACCES-TEND
      * CONTROLE CODE RETOUR
           EVALUATE ACCE-CODE-RETOUR
      * ==>   SI OK
              WHEN ZERO
150978           MOVE TEND-C-REF-GIIN TO LST-ENT-DECL
              WHEN OTHER
      * ==>   SI ERREUR APPEL MODULE MFUSTEND
                 MOVE 1016                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .

      *                         *****                               *  *
150978*==================================================*
"     * INSERT LIGNE FICHIER DECLARATIF                  *
"     *==================================================*
"      1362-ALIM-REF-FID.
"     *
"     * ALIMENTATION DES DONNEES TFID
"     *
"     *    Initialisation des donnees du module MFUSTFID                03753799
"          MOVE SPACES                       TO TFID-PARAM
"                                               ACCE-TYPE-REQUETE
"                                               ACCE-NOM-FONCTION
"                                               TFID-N-AGENT
"                                               TFID-L-NOM-EMET
"                                               TFID-I-REF-ADM
"                                               TFID-N-AGENT
"                                               TFID-C-CLE-HACH
"                                               TFID-C-CLE-HACH
"                                               TFID-C-CLE-HACH
"          MOVE '0001-01-01'                 TO TFID-D-ACCUSE
"          MOVE ' '                          TO TFID-C-TYPE-DECL
"          MOVE ZEROS                        TO TFID-I-IDENT-END
"                                               TFID-I-IDENT-REF-REP
"                                               TFID-Q-PERS-DECL
"                                               TFID-Q-CONTRAT-DECL
"                                               TFID-M-TOT-SOL-DECL
"                                               TFID-M-TOT-INT-DECL
"                                               TFID-M-TOT-CES-DECL
"                                               TFID-M-TOT-M-DECL
"                                               TFID-NO-VERSION
"                                               ACCE-CODE-RETOUR
"          MOVE TEND-I-IDENT-END             TO TFID-I-IDENT-END
"          MOVE WS-A-APPL                    TO TFID-A-APPL
"          MOVE WS-I-REF-FIC                 TO TFID-I-REF-FIC
"          MOVE WS-I-REF-FIC
"                                            TO TFID-I-REF-FIC-INIT
"          MOVE W-DATE-TIMESTAMP
"                                            TO TFID-D-REF-FIC-INIT
"          .
"
"     *==================================================*
"     * INSERT LIGNE FICHIER DECLARATIF                  *
"     *==================================================*
"      1363-INS-REF-FID.
"     *
"     * APPEL MODULE MFUSTFID POUR UNE INSERTION
"     * DANS LA TABLE TBFUSFID
"     *
"          MOVE 'IN'                  TO ACCE-FONCTION                  03753799
"     D    DISPLAY ' TFID-I-IDENT-END    ==> ' TFID-I-IDENT-END
"     D    DISPLAY ' TFID-A-APPL         ==> ' TFID-A-APPL
"     D    DISPLAY ' TFID-I-REF-FIC-INIT ==> ' TFID-I-REF-FIC-INIT
"     D    DISPLAY ' TFID-D-REF-FIC-INIT ==> ' TFID-D-REF-FIC-INIT
"
"     * APPEL MFUSTEND
"          PERFORM SQ-ACCES-TFID
"     * CONTROLE CODE RETOUR
"          EVALUATE ACCE-CODE-RETOUR
"     * ==>   SI OK
"             WHEN ZERO
"                CONTINUE
"             WHEN OTHER
"     * ==>   SI ERREUR APPEL MODULE MFUSTEND
"                MOVE 1016                      TO WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"          END-EVALUATE
"          .
"
"     *==================================================*
"     * SELECT LIGNE FICHIER DECLARATIF                  *
"     *==================================================*
"      1364-RECUP-REF-FID.
"     *
"     * APPEL MODULE MFUSTFID POUR UNE INSERTION
"     * DANS LA TABLE TBFUSFID
"     *
"          MOVE 'SE'                  TO ACCE-TYPE-REQUETE              03753799
"          MOVE 'SE-PREVIOUS'         TO ACCE-NOM-FONCTION              03753799
"
"     * APPEL MFUSTEND
"          PERFORM SQ-ACCES-TFID
"     * CONTROLE CODE RETOUR
"          EVALUATE ACCE-CODE-RETOUR
"     * ==>   SI OK
"             WHEN ZERO
"     D          DISPLAY 'TFID-I-IDENT-REF-REP PREV :'
"     D                   TFID-I-IDENT-REF-REP
"                MOVE TFID-I-IDENT-REF-REP TO WS-I-IDENT-REF-REP
"             WHEN OTHER
"     * ==>   SI ERREUR APPEL MODULE MFUSTEND
"                MOVE 2600                      TO WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"          END-EVALUATE
150978     .

"     *==================================================*
"     * SELECT LIGNE FICHIER DECLARATIF INITIAL          *
"     *==================================================*
MCHA+- 1365-RECUP-REF-FID.
"     *
"     * APPEL MODULE MFUSTFID POUR UNE SELECT
"     * DANS LA TABLE TBFUSFID
"     *
"          MOVE TEND-I-IDENT-END      TO TFID-I-IDENT-END               03753799
"          MOVE WS-A-APPL             TO TFID-A-APPL                    03753799
"          MOVE 'OP'                  TO ACCE-TYPE-REQUETE              03753799
"          MOVE '02'                  TO ACCE-NOM-FONCTION              03753799
"
"     * APPEL MFUSTEND
"          PERFORM SQ-ACCES-TFID
"     * CONTROLE CODE RETOUR
"          EVALUATE ACCE-CODE-RETOUR
"     * ==>   SI OK
"             WHEN ZERO
"     D          DISPLAY 'TFID-I-IDENT-REF-REP PREV :'
"     D                   TFID-I-IDENT-REF-REP
"                MOVE TFID-I-REF-FIC      TO WS-I-REF-FIC-INIT
"                MOVE TFID-D-REF-FIC-INIT TO WS-D-REF-FIC-INIT
"                CONTINUE
"             WHEN OTHER
"     * ==>   SI ERREUR APPEL MODULE MFUSTFID
"                MOVE 1016                      TO WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"          END-EVALUATE
MCHA+-     .

      * =============================================================  *
      * FERMETURE CURSEUR CLEC_02
      * =============================================================  *
MCHA+- 1365-FERM-FID.
"     *
"     * APPEL MODULE MFUSTFID POUR FERMETURE
"     * DANS LA TABLE TBFUSFID
"          MOVE 'CL'                  TO ACCE-TYPE-REQUETE              03753799
"          MOVE '02'                  TO ACCE-NOM-FONCTION              03753799
"
"     * APPEL MFUSTEND
"          PERFORM SQ-ACCES-TFID
"     * CONTROLE CODE RETOUR
"          EVALUATE ACCE-CODE-RETOUR
"     * ==>   SI OK
"             WHEN ZERO
"     D          DISPLAY 'TFID-I-IDENT-REF-REP PREV :'
"     D                   TFID-I-IDENT-REF-REP
"                CONTINUE
"             WHEN OTHER
"     * ==>   SI ERREUR APPEL MODULE MFUSTEND
"                MOVE 1016                      TO WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"          END-EVALUATE
150978     .

      * =============================================================  *
      * ECRITURE DANS LE FICHIER DFUSLS11 (GSAM FORMAT FIXE)
      * EN CAS D'ERREUR ABANDON DU PROGRAMME
      * =============================================================  *
       1360-ECRIRE-OUT.
      *
      *    ECRITURE LIGNE IDENTIFIANT ENTITé DéCLARENTE
      *
           MOVE SPACE                  TO   STAT-FUSEL10
150978     MOVE SPACE                  TO   STAT-FUSEL20
"          MOVE SPACE                  TO   STAT-FUSEL30
"          MOVE SPACE                  TO   STAT-FUSEL40
"          MOVE SPACE                  TO   STAT-FUSEL50
"          MOVE SPACE                  TO   STAT-FUSEL60
"          MOVE SPACE                  TO   STAT-FUSEL70
"          MOVE SPACE                  TO   STAT-FUSEL80
"          MOVE SPACE                  TO   STAT-FUSEL90
"
"
"     * AIGUILLAGE DE L ECRITURE
"          EVALUATE TRUE
"     * ==>   Entité 1G159I.00000.BR.492
"             WHEN TOP-DECL-01
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS11
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL10     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT1-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2410                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00000.LE.250
"             WHEN TOP-DECL-02
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS12
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL20     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT2-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2420                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00352.ME.876
"             WHEN TOP-DECL-03
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS13
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL30     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT3-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2430                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00353.BR.663
"             WHEN TOP-DECL-04
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS14
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL40     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT4-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2440                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00353.ME.250
"             WHEN TOP-DECL-05
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS11
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL50     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT5-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2450                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00356.ME.250
"             WHEN TOP-DECL-06
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS16
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL60     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT6-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2460                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00357.ME.250
"             WHEN TOP-DECL-07
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS17
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL70     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT7-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2470                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00359.ME.540
"             WHEN TOP-DECL-08
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS18
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL80     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT8-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2480                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
"     * ==>   Entité 1G159I.00360.ME.250
"             WHEN TOP-DECL-09
"               CALL  'CBLTDLI'           USING  ISRT
"                                                 PCB-DFUSLS19
"                                                 EL10-CFUSEL10
"
"               IF STAT-FUSEL90     = SPACE
"     *            ECRITURE REUSSIE
"                  ADD  1                   TO   WS-EDT9-ECRIT
"                                                WS-CPT-ECRIT
"               ELSE
"                  MOVE 2490                TO   WS-CODE-ABEND
"                  PERFORM 4200-ABEND-ERR
"               END-IF
150978        WHEN OTHER
      * ==>   SI ERREUR ECRITURE
                 MOVE 2500                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
      *
           .

      *                         *****                               *  *
      * =============================================================  *
      * ALIMENTATION DES INFORMATIONS SELON LE FORMAT DE LA LIGNE NUMéRO
      * ZéRO
      * =============================================================  *
       1361-ALIM-INF-END.
      *
      *    ALIMENTATION DES INFORMATIONS LIGNE ZéRO
      *
      *    Initialisation
           MOVE SPACE                  TO   EL10-G-DATA-TET-END

      *    Code enregistrement
           MOVE '10'                   TO   EL10-C-ENR
      *    Identifiant de lentité déclarante
           MOVE TEND-I-IDENT-END       TO   EL10-I-IDENT
      *    N° Ligne /Identifiant
           MOVE 1                      TO   EL10-N-LIG-IDENT
      *    Entité concernée
           MOVE 'END'                  TO   EL10-C-ENTIT
      *    Référence fiscalité étrangère GIN de lentité
           MOVE TEND-C-REF-GIIN        TO   EL10-C-REF-GIIN
                                       OF   EL10-G-DATA-TET-END
      *    Année fiscale
           MOVE TEND-A-APPL            TO   EL10-A-APPL
                                       OF   EL10-G-DATA-TET-END
      *    Code du pays émetteur du GIN
           IF TEND-C-CODE-EXPD = 'US'
              MOVE SPACES                 TO   EL10-C-PAYS-EMET-GIN
           ELSE
              MOVE 'FR'                   TO   EL10-C-PAYS-EMET-GIN
           END-IF
      *    Raison sociale de lentité
           MOVE TEND-L-RAISON-SOCIALE  TO   EL10-L-RAISON-SOCIALE
      *    Code Pays émetteur (ex : "FR")
           MOVE TEND-C-PAYS-EMET       TO   EL10-C-PAYS-EMET
      *    Code Pays destinataire final (ex : "US")
           MOVE TEND-C-PAYS-DEST       TO   EL10-C-PAYS-DEST
      *    Code Expédition directe ("US") ou indirecte ("FR")
           MOVE TEND-C-CODE-EXPD       TO   EL10-C-CODE-EXPD
      *    Contact 1 (Fiscal) ou réserve
           MOVE TEND-L-CONTACT1        TO   EL10-L-CONTACT1
      *    Nom et prénom du contact 1
           MOVE TEND-L-NOM-CONTACT1    TO   EL10-L-NOM-CONTACT1
      *    Téléphone 1er Contact (Fiscal) ou réserve
           MOVE TEND-N-TEL-CONTACT1    TO   EL10-N-TEL-CONTACT1
      *    Mail 1er Contact (Fiscal) ou réserve
           MOVE TEND-L-MAIL-CONTACT1   TO   EL10-L-MAIL-CONTACT1
MCHA??     MOVE W-DATE-TIMESTAMP
150978                                 TO   EL10-D-REF-FIC-INIT
150978                                 OF   EL10-G-DATA-TET-END
150978*    INITIALISATION DES RéFéRENCES BLOC
150978     MOVE SPACES TO WS-I-REF-FIC
150978                    WS-I-REF-BLOC-CPT
150978                    WS-I-REF-BLOC-CLR
150978
150978     IF SYSIN-TYPE-FATCA NOT = SPACES OR LOW-VALUE
150978        IF SYSIN-TYPE-FATCA = 'FATCA1 ' OR 'FATCA2 ' OR 'FATCA3 '
150978                           OR 'FATCA4 '
150978           MOVE SYSIN-TYPE-FATCA TO
150978                              WS-TYPE-FATCA OF WS-TYPE-REP
150978                              WS-TYPE-FATCA OF WS-BLOC-CPT
150978                              WS-TYPE-FATCA OF WS-BLOC-CLR
150978           MOVE '1'                          TO EL10-C-MOD-REPORT
150978                                                WS-C-MOD-REPORT
150978        END-IF
150978        IF SYSIN-TYPE-FATCA = 'FATCA11' OR 'FATCA12' OR 'FATCA13'
150978                           OR 'FATCA14'
150978           MOVE SYSIN-TYPE-FATCA TO
150978                          WS-TYPE-FATCA-TST OF WS-TYPE-REP-TST
150978                          WS-TYPE-FATCA-TST OF WS-BLOC-CPT-TST
150978                          WS-TYPE-FATCA-TST OF WS-BLOC-CLR-TST
150978           MOVE '2'                          TO EL10-C-MOD-REPORT
150978                                                WS-C-MOD-REPORT
150978        END-IF
150978     END-IF
150978
150978     IF WS-TYPE-FATCA-TST OF WS-TYPE-REP-TST = 'FATCA11' OR
150978        'FATCA12' OR 'FATCA13' OR 'FATCA14'
150978        MOVE W-DATE-TIMESTAMP             TO WS-DATE-TIMESTAMP-TST
150978                                          OF WS-TYPE-REP-TST
150978        MOVE LST-ENT-DECL                 TO WS-GIIN-TST
150978                                          OF WS-TYPE-REP-TST
150978        MOVE '-'                          TO WS-FILLER-01-TST
150978                                          OF WS-TYPE-REP-TST
150978                                             WS-FILLER-02-TST
150978                                          OF WS-TYPE-REP-TST
150978        MOVE '2'                          TO EL10-C-MOD-REPORT
150978                                             WS-C-MOD-REPORT
150978     ELSE
150978        MOVE W-DATE-TIMESTAMP             TO WS-DATE-TIMESTAMP
150978                                          OF WS-TYPE-REP
150978        MOVE LST-ENT-DECL                 TO WS-GIIN
150978                                          OF WS-TYPE-REP
150978        MOVE '-'                          TO WS-FILLER-01
150978                                          OF WS-TYPE-REP
150978                                             WS-FILLER-02
150978                                          OF WS-TYPE-REP
150978        MOVE '1'                          TO EL10-C-MOD-REPORT
150978                                             WS-C-MOD-REPORT
150978     END-IF
150978     MOVE WS-I-REF-FIC
150978                                 TO   EL10-I-REF-FIC
150978                                 OF   EL10-G-DATA-TET-END
           IF EL10-C-MOD-REPORT = '1'
              MOVE SYSIN-TYPE-FATCA(1:6) TO   EL10-C-TYPE-FATCA
           ELSE
              MOVE SYSIN-TYPE-FATCA(1:7) TO   EL10-C-TYPE-FATCA
           END-IF
           .

MCHA+-*                         *****                               *  *
"     * =============================================================  *
"     * ALIMENTATION DES INFORMATIONS FATCA 2-3-7
"     * =============================================================  *
"      1361-TRT-FATCA234.
"     *
"     *    RECUPERATION REFERENCE INITIAL
"     *
"          PERFORM 1365-RECUP-REF-FID
"     *    REFERENCE INITIAL
"          MOVE WS-I-REF-FIC-INIT    TO EL10-I-REF-FIC-INIT
"                                    OF EL10-G-DATA-TET-END
"     *    DATE INITIALE
"          MOVE WS-D-REF-FIC-INIT    TO EL10-D-REF-FIC-INIT
"                                    OF EL10-G-DATA-TET-END
"     *    FERMETURE CURSEUR FID
"          PERFORM 1365-FERM-FID
"          .
      *                   ***                            *
      *==================================================*
      * ACCèS à LA BASE FUS (TBADRFUS)                   *
      *==================================================*
       1370-SEL-INF-ADR.
      *
      * APPEL MODULE MFUSTADR POUR UNE SELECTION
      * DANS LA TABLE TBFUSADR
      *
           MOVE 'SE'                    TO ACCE-TYPE-REQUETE
           MOVE '01-SEL'                TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE 'ED'                    TO TADR-C-TYPE-ADR
           MOVE TEND-I-IDENT-END        TO TADR-I-IDENT

      * APPEL MFUSTEND
           PERFORM SQ-ACCES-TADR
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI ABSENT
              WHEN NON-TROUVE
MCHA++           MOVE 1025                      TO WS-CODE-ABEND
MCHA++           PERFORM 4200-ABEND-ERR
              WHEN OTHER
      * ==>   SI ERREUR APPEL MODULE MFUSTEND
                 MOVE 1018                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .

      *                         *****                               *  *
      * =============================================================  *
      * ALIMENTATION DES INFORMATIONS SELON LE FORMAT DE LA LIGNE
      * ADRESSE
      * =============================================================  *
       1381-ALIM-INF-ADR.
      *
      *    ALIMENTATION DES INFORMATIONS ADRESSE
      *
      *    Initialisation
           MOVE SPACE                  TO   EL10-G-DATA-TET-END

      *    Code enregistrement
           MOVE '10'                   TO   EL10-C-ENR
      *    Identifiant de lentité déclarante
           MOVE TADR-I-IDENT           TO   EL10-I-IDENT
      *    N° Ligne /Identifiant
           ADD  1                      TO   EL10-N-LIG-IDENT
      *    Entité concernée
           MOVE 'ADR'                  TO   EL10-C-ENTIT
      *    Type adresse
           MOVE TADR-C-TYPE-ADR        TO   EL10-C-TYPE-ADR
                                       OF   EL10-G-DATA-TET-ADR
      *    Nom commune adresse
           MOVE TADR-L-COMM-ADR        TO   EL10-L-COMM-ADR
                                          OF   EL10-G-DATA-TET-ADR
      *    Code postale adresse
           MOVE TADR-C-CPOST           TO   EL10-C-CPOST
                                       OF   EL10-G-DATA-TET-ADR
      *    Code pays ISO de l'adresse
           MOVE TADR-C-PAYS-ADR        TO   EL10-C-PAYS-ADR
                                           OF   EL10-G-DATA-TET-ADR
      *    Intitulé courrier ligne 1 adresse
           MOVE TADR-L-INTIT-COURR-1   TO   EL10-L-INTIT-COURR-1
                                         OF   EL10-G-DATA-TET-ADR
      *    Intitulé courrier ligne 2 adresse
           MOVE TADR-L-INTIT-COURR-2   TO   EL10-L-INTIT-COURR-2
                                           OF   EL10-G-DATA-TET-ADR
      *    Adresse 1
           MOVE TADR-L-ADR-LIGNE-1     TO   EL10-L-ADR-LIGNE-1
                                       OF   EL10-G-DATA-TET-ADR
      *    Adresse 2
           MOVE TADR-L-ADR-LIGNE-2     TO   EL10-L-ADR-LIGNE-2
                                           OF   EL10-G-DATA-TET-ADR
      *    Adresse 3
           MOVE TADR-L-ADR-LIGNE-3     TO   EL10-L-ADR-LIGNE-3
                                           OF   EL10-G-DATA-TET-ADR
      *    Libellé pays de l'adresse
           MOVE TADR-L-PAYS-ADR        TO   EL10-L-PAYS-ADR
                                           OF   EL10-G-DATA-TET-ADR
           .

      *                         *****                               *  *
      *                         *****                               *  *

      *==============================================*
      * AFFICHAGE DU DEBUT DE LA SYSOUT DU PROGRAMME *
      *==============================================*
       1400-AFFICHAGE-DEBUT.
      *
      *    RECUPERATION DATE ET HEURE DEBUT TRAITEMENT
      *    AFFICHAGE COMPTEUR RENDU DEBUT D'EXECUTION
      *
           PERFORM 9920-RECUP-DATES-HEURES

      *
      * LIGNES DECORATION
           MOVE  13             TO      BILA-Q-LIST-DISP
           MOVE  WS-LIGNE-DECO1 TO      BILA-L-DISP (1)
           MOVE  WS-LIGNE-DECO2 TO      BILA-L-DISP (2)
           MOVE  WS-LIGNE-DECO1 TO      BILA-L-DISP (3)
           MOVE  WS-LIGNE-DECO3 TO      BILA-L-DISP (4)
           MOVE  WS-LIGNE-DECO1 TO      BILA-L-DISP (5)
           MOVE  '* PROGRAMME ............... : '
                                TO      WS-LIGNE-LIB
           MOVE  'BFUSEL10                     *'
                                TO      WS-LIGNE-VAL
           MOVE  WS-LIGNE       TO      BILA-L-DISP (6)

      * DATE DE COMPILATION

           MOVE WS-DATE-COMPIL  TO      WS-LIGNE-D-COMP
           MOVE '* DATE DE COMPILATION ..... : '
                                TO      WS-LIGNE-LIB
           MOVE WS-LIGNE-DECO4  TO      WS-LIGNE-VAL
           MOVE WS-LIGNE        TO      BILA-L-DISP (7)

      * HEURE DE COMPILATION
           MOVE W-H-COMPIL      TO      WS-LIGNE-H-COMP
           MOVE '* HEURE DE COMPILATION .... : '
                                TO      WS-LIGNE-LIB
           MOVE WS-LIGNE-DECO5  TO      WS-LIGNE-VAL
           MOVE WS-LIGNE        TO      BILA-L-DISP (8)

      * DATE DEBUT TRAITEMENT
           MOVE W-D-SYSTEME-ISO TO      WS-LIGNE-D-TRT
           MOVE '* DATE DEBUT TRAITEMENT ... : '
                                TO      WS-LIGNE-LIB
           MOVE WS-LIGNE-DECO6  TO      WS-LIGNE-VAL
           MOVE WS-LIGNE        TO      BILA-L-DISP (9)

      * HEURE DEBUT TRAITEMENT
           MOVE W-H-SYSTEME-ISO TO      WS-LIGNE-H-TRT
           MOVE '* HEURE DE DEBUT TRAITEMENT : '
                                TO      WS-LIGNE-LIB
           MOVE WS-LIGNE-DECO7  TO      WS-LIGNE-VAL
           MOVE WS-LIGNE        TO      BILA-L-DISP (10)

      * FREQUENCE DE CHECKPOINT
           MOVE W999-FREQCHKP   TO      WS-LIGNE-FQ-CHKP
           MOVE '* FREQUENCE PRISE CHECKPOINT: '
                                TO      WS-LIGNE-LIB
           MOVE WS-LIGNE-DECO8  TO      WS-LIGNE-VAL
           MOVE WS-LIGNE        TO      BILA-L-DISP (11)

      * VERIFICATION DU MODE DE LANCEMENT
           MOVE  '* MODE DE LANCEMENT ....... : '
                                TO      WS-LIGNE-LIB
           IF W999-GCXT-INDICE = 'R'
      *       LANCEMENT MODE REPRISE
              MOVE  'REPRISE                      *'
                                TO      WS-LIGNE-VAL
           ELSE
      *       LANCEMENT MODE NORMAL
              MOVE  'NORMAL                       *'
                                TO      WS-LIGNE-VAL
           END-IF
           MOVE  WS-LIGNE       TO      BILA-L-DISP (12)

           MOVE WS-LIGNE-DECO1  TO      BILA-L-DISP (13)

      * APPEL MCCDBILA
           CALL 'MCCDBILA'      USING   BILA-PARAM
           .
      *
      *                         *****                               *  *
      * =============================================================  *
      * OUVERTURE CURESEUR CNT
      *                         *****                               *  *
150978* =============================================================  *
"      1500-LECTURE-FICHIER-AIG.
"     *
"     *    LECTURE FICHIER EN ENTRéE
"     *
"          MOVE SPACE                  TO   STAT-FUSEL01
"          MOVE  AI11-I-IDENT-END      TO   WS-I-IDENT-END-SV
"
"          CALL  'CBLTDLI'           USING  GN
"                                           PCB-DFUSLE11
"                                           AI11-CFUSAI00
"
"          IF STAT-FUSEL01     = SPACE
"     *       ECRITURE REUSSIE
"             ADD  1                   TO   WS-CPT-AIG-LUS
"             MOVE  AI11-I-IDENT-END   TO   WS-I-IDENT-END-LU
"
"             IF AI11-C-TYP-ENR = '10'
"                  ADD  1              TO   WS-CPT-AIG-LUS-10
"             END-IF
"
"             IF AI11-C-TYP-ENR = '20'
"                  ADD  1              TO   WS-CPT-AIG-LUS-20
"             END-IF
"          ELSE
"             IF STAT-FUSEL01    = 'GB'
"                IF WS-CPT-AIG-LUS = 0
"                   MOVE 2100          TO   WS-CODE-ABEND
"                   PERFORM 4200-ABEND-ERR
"                ELSE
"                   MOVE 'O'           TO IND-FIN-FIC
"                END-IF
"             ELSE
"                MOVE 2300             TO   WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"             END-IF
"          END-IF
"     *
150978     .

150978* =============================================================  *
"      1500-LECTURE-FICHIER-AIG-REP.
"     *
"     *    LECTURE FICHIER EN ENTRéE
"     *
"          MOVE SPACE                  TO   STAT-FUSEL01
"     *    MOVE W999-I-IDENT-END       TO   WS-I-IDENT-END-SV
"
"          CALL  'CBLTDLI'           USING  GN
"                                           PCB-DFUSLE11
"                                           AI11-CFUSAI00
"
"          IF STAT-FUSEL01     = SPACE
"     *       ECRITURE REUSSIE
"             ADD  1                   TO   WS-CPT-AIG-LUS
"             MOVE  AI11-I-IDENT-END   TO   WS-I-IDENT-END-LU
"
"             IF AI11-C-TYP-ENR = '10'
"                  ADD  1              TO   WS-CPT-AIG-LUS-10
"             END-IF
"
"             IF AI11-C-TYP-ENR = '20'
"                  ADD  1              TO   WS-CPT-AIG-LUS-20
"             END-IF
"          ELSE
"             IF STAT-FUSEL01    = 'GB'
"                IF WS-CPT-AIG-LUS = 0
"                   MOVE 2100          TO   WS-CODE-ABEND
"                   PERFORM 4200-ABEND-ERR
"                ELSE
"                   MOVE 'O'           TO IND-FIN-FIC
"                END-IF
"             ELSE
"                MOVE 2300             TO   WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"             END-IF
"          END-IF
"     *
150978     .
      *                         *****                               *  *
      *                         *****                               *  *
      * =============================================================  *
      * LECTURE CURESEUR CNT
      * =============================================================  *
       2210-LECTURE-CNT.
      *
      *    LECTURE DU CURSEUR DE LA TABLE DES COMPTES EN MODE REPRISE
      *
150978     MOVE AI11-I-IDENT-CPT        TO TCNT-I-IDENT-CPT
"     *
"          MOVE 'SE'                    TO ACCE-TYPE-REQUETE
150978     MOVE '06'                    TO ACCE-NOM-FONCTION

      * APPEL MFUSTCNT
           PERFORM SQ-ACCES-TCNT
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
MCHA? *          ADD  1  TO WS-CPT-CNT-LUS
MCHA?            CONTINUE
      * ==>   SI KO
              WHEN NON-TROUVE
                 MOVE 'O'               TO IND-FIN-TRT
              WHEN OTHER
      * ==>   SI ERREUR APPEL MODULE MFUSTCNT
                 MOVE 1015                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .

      *                         *****                               *  *
      *---------------------------------------------------------*
      *                   PARAGRAPHES 2XXX
      *---------------------------------------------------------*
      *
      *===================================*
      * TRAITEMENT PRINCIPAL DU PROGRAMME *
      *===================================*
       2000-TRT-PGM.
      *
150978     PERFORM UNTIL IND-FIN-FIC  = 'O'
      *
      * TRAITEMENT PRINCIPALE
      * Restitution et écriture des informations du compte
150978D       DISPLAY ' WS-I-IDENT-END LU 'WS-I-IDENT-END-LU
      D       DISPLAY ' WS-I-IDENT-END SV 'WS-I-IDENT-END-SV
              IF WS-I-IDENT-END-LU NOT = WS-I-IDENT-END-SV
150978D          DISPLAY 'WS-CPT-ECRIT :' WS-CPT-ECRIT
150978           IF WS-CPT-ECRIT NOT = ZERO AND LOW-VALUE
150978              PERFORM 4100-RESTIT-ENQUEUE
150978           END-IF
150978           PERFORM 1330-RECH-INF-END
              END-IF
      *
150978        IF AI11-C-TYP-ENR = '10'
                 PERFORM 2200-RESTIT-CNT

150978           ADD  1              TO WS-Q-PERS-DECL
150978                                  WS-Q-CONTRAT-DECL
150978        END-IF
      *
150978        IF AI11-C-TYP-ENR = '20'
                 PERFORM 3000-RESTIT-CLR

150978           ADD  1              TO WS-Q-RECAL-DECL
150978        END-IF
      * CHECK POINT
              PERFORM 9960-PRISE-CHECKPOINT

RDMR  *       IF WS-CPT-ECRIT > 10
RDMR  *          DISPLAY 'TEST RDMR'
RDMR  *          MOVE 1003                   TO WS-CODE-ABEND
RDMR  *          PERFORM 4200-ABEND-ERR
RDMR  *       END-IF

      * Lecture suivante
150978        PERFORM 1500-LECTURE-FICHIER-AIG
      *
           END-PERFORM
           .
      *
      *                     ***                           *     *      *
      *===================================================*
      * RESTITUTION ET éCRITURE DES INFORMATIONS DU COMPTE.
      *===================================================*
       2200-RESTIT-CNT.
      *
      * RESTITUTION DE SOLDE DU COMPTE
      *
151197* SI LE TRAITEMENT TRAITE UN FICHIER FATCA3
151197*    IF SYSIN-TYPE-FATCA = 'FATCA3 ' OR 'FATCA13'
MC+        IF SYSIN-TYPE-FATCA13 = 'FATCA31A' OR 'FATCA31A1'
151197*    ACCèS à LA TABLE ENTêTE HISTORIQUE
151197        PERFORM RECH-DONNEE-FATCA3
151197     END-IF

           PERFORM 2210-LECTURE-CNT
           MOVE 'N'               TO IND-FIN-TRT-RUB
150978     MOVE ZERO              TO WS-Q-NBR-DEC
           MOVE ZERO              TO WS-M-MNT-ASS

      * APPEL à L'ACCESSEUR MFUSTRUB POUR OUVRIR LE CURSEUR DE LA TABLE
      * DES MONTANTS.
           PERFORM 2111-OUV-CUR-RUB-01

      * Boucle...
           PERFORM UNTIL IND-FIN-TRT-RUB = 'O'
      *       Traitement de l'enregistrement lu
151197*       IF SYSIN-TYPE-FATCA = 'FATCA3 ' OR 'FATCA13'
MC+           IF SYSIN-TYPE-FATCA13 = 'FATCA31A' OR 'FATCA31A1'
151197           PERFORM 2112-TRT-CUMULE-HIS
151197        ELSE
                 PERFORM 2112-TRT-CUMULE
151197        END-IF
      *       Lecture suivante
              PERFORM 2113-FET-CUR-RUB-01
      * Fin Boucle.
           END-PERFORM
      *  ==> RéCUPéRER LES INFORMATIONS DU CLIENT
           PERFORM 2114-TRAITEMENT-CLI-SEL

      *    ALIMENTATION DES COMPTES
151197     IF SYSIN-TYPE-FATCA = 'FATCA3 ' OR 'FATCA13'
151197        PERFORM REMPLIR-DONNEE-CONTRAT
151197        PERFORM 2115-ALIM-INF-CNT-HIS
151197     ELSE
              PERFORM 2115-ALIM-INF-CNT
151197     END-IF

      *    ECRITURE COMPTE
           PERFORM 1360-ECRIRE-OUT

MC+   * SI LE TRAITEMENT TRAITE UN FICHIER FATCA3+1
"     D    DISPLAY ' SYSIN-TYPE-FATCA13' SYSIN-TYPE-FATCA13
"          IF SYSIN-TYPE-FATCA13 = 'FATCA31A' OR 'FATCA31A1'
"     *    ACCèS à LA TABLE ENTêTE HISTORIQUE
"             PERFORM RECH-DONNEE-CLT-FATCA3
MC+        END-IF
      *    ECRITURE DONNEES CLIENT
           PERFORM 2116-TRAITEMENT-CLI

150978*    ALIMENTATION DONNEES BLOC COMPTES
"          PERFORM 2117-ALIM-INF-REP
"
"     *    ALIMENTATION DONNEES BLOC COMPTES
150978     PERFORM 1360-ECRIRE-OUT

150978*    RESTITUER DONNEES LCC
150978     PERFORM 2118-TRAITEMENT-LCC
150978
150978*    ALIMENTATION DONNEES BLOC COMPTES
150978     PERFORM 2119-ALIM-INF-LCC
150978
150978*    ALIMENTATION DONNEES BLOC COMPTES
150978     PERFORM 1360-ECRIRE-OUT
           .

151197*
151197*                     ***                                        *
151197*==============================================================*
151197* RECHERCHE DES DONNéES DE FATCA3 à PARTIR DE LA TABLE TBHISFUS
151197*==============================================================*
151197 RECH-DONNEE-FATCA3.
151197*
151197*==> INITIALISATION DES DONNéES DE WORKING
151197     MOVE SPACES            TO W-N-OPE
151197     MOVE SPACES            TO W-I-UNIQ-KAC
MC         MOVE SPACES            TO W-I-RIB-INVAR
151197     MOVE SPACES            TO W-C-IBAN
151197     MOVE SPACES            TO W-C-DEV
151197     MOVE ZEROES            TO W-M-MNT-ASS
151197     MOVE ZEROES            TO W-M-MNT-ASS-06
MCHA+!     SET ENR-HIS-NN-TRV     TO TRUE
151197
151197*==> ACCèS à LA TABLE ENTêTE HISTORIQUE
151197*    OUVERTURE DU CURSEUR D'ACCèS à LA TABLE TBHISFUS
MC+        MOVE AI11-I-IDENT-CPT   TO WS-I-IDENT-1
151197     PERFORM OUV-CUR-HIS-01
151197
151197*    BALAYAGE DE LA TABLE JUSQU'à ENTêTE DéTECTé
151197     PERFORM UNTIL IND-FIN-TRT-HIS = 'O' OR ENR-HIS-TRV
151197        IF THIS-C-ACTION = 'M' AND THIS-C-TYPE-IDENT-1 = '03' AND
151197           THIS-D-TIMSP > AI11-D-REF-FIC-INIT OF AI11-ENR-PERS-CPT
151197           SET ENR-HIS-TRV     TO TRUE
151197           MOVE THIS-N-OPE     TO W-N-OPE
151197        END-IF
151197*       LECTURE SUIVANTE DU CURSEUR D'ACCèS à LA TABLE TBHISFUS
151197        PERFORM LEC-CUR-HIS-01
151197     END-PERFORM
151197
151197*    FERMETURE DU CURSEUR D'ACCèS à LA TABLE TBHISFUS
151197     IF IND-FIN-TRT-HIS NOT = 'O' AND ENR-HIS-TRV
151197        PERFORM FER-CUR-HIS-01
151197     END-IF
151197
151197*==> ACCèS à LA TABLE DETAIL HISTORIQUE
151197     IF ENR-HIS-TRV
151197*       OUVERTURE DU CURSEUR D'ACCèS à LA TABLE TBHIDFUS
151197        PERFORM OUV-CUR-HID-01
151197
151197*       BALAYAGE DES DéTAILS HISTORIQUES
151197        PERFORM UNTIL IND-FIN-TRT-HID = 'O'
151197           EVALUATE THID-L-DONNEE-MODIF
151197              WHEN 'I-UNIQ-KAC'
151197                 MOVE THID-L-DONNEE-AVANT  TO W-I-UNIQ-KAC
151197              WHEN 'C-IBAN'
151197                 MOVE THID-L-DONNEE-AVANT  TO W-C-IBAN
MC                  WHEN 'I-RIB-INVAR'
MC                     MOVE THID-L-DONNEE-AVANT  TO W-I-RIB-INVAR
151197              WHEN 'C-DEV'
151197                 MOVE THID-L-DONNEE-AVANT  TO W-C-DEV
151197              WHEN 'M-MNT-ASS'
151197                 MOVE THID-L-DONNEE-AVANT(1:18) TO TEMP-M-MNT-ASS
151197                 MOVE TEMP-M-MNT-ASS            TO T1-M-MNT-ASS
151197                 MOVE T1-M-MNT-ASS              TO W-M-MNT-ASS
151197              WHEN 'M-MNT-ASS-06'
151197                 MOVE THID-L-DONNEE-AVANT(1:18)
151197                                           TO TEMP-M-MNT-ASS-06
151197                 MOVE TEMP-M-MNT-ASS-06    TO T1-M-MNT-ASS-06
151197                 MOVE T1-M-MNT-ASS-06      TO W-M-MNT-ASS-06
151197              WHEN OTHER
151197                 CONTINUE
151197           END-EVALUATE
151197*         LECTURE SUIVANTE DU CURSEUR D'ACCèS à LA TABLE TBHIDFUS
151197           PERFORM LEC-CUR-HID-01
151197        END-PERFORM
151197     END-IF
151197     .

MC+   *
"     *                     ***                                        *
"     *==============================================================*
"     * RECHERCHE DES DONNéES CLIENT FATCA 3
"     *==============================================================*
"      RECH-DONNEE-CLT-FATCA3.
"     *
"     *==> INITIALISATION DES DONNéES DE WORKING
"          MOVE LOW-VALUE         TO W-ENR-CLT-ADR
"          MOVE SPACES            TO W-N-OPE
"          MOVE SPACES            TO W-I-UNIQ-KAC
"          MOVE SPACES            TO W-I-RIB-INVAR
"          MOVE SPACES            TO W-C-IBAN
"          MOVE SPACES            TO W-C-DEV
"          MOVE ZEROES            TO W-M-MNT-ASS
"          MOVE ZEROES            TO W-M-MNT-ASS-06
"          SET ENR-HIS-NN-TRV     TO TRUE
"
"     *==> ACCèS à LA TABLE ENTêTE HISTORIQUE
"     *    OUVERTURE DU CURSEUR D'ACCèS à LA TABLE TBHISFUS
MC+        MOVE AI11-I-IDENT-CLT   TO WS-I-IDENT-1
"          PERFORM OUV-CUR-HIS-01
"
"     *    BALAYAGE DE LA TABLE JUSQU'à ENTêTE DéTECTé
"          PERFORM UNTIL IND-FIN-TRT-HIS = 'O' OR ENR-HIS-TRV
"             IF THIS-C-ACTION = 'M' AND THIS-C-TYPE-IDENT-1 = '02' AND
"                THIS-D-TIMSP > AI11-D-REF-FIC-INIT OF AI11-ENR-PERS-CPT
"                SET ENR-HIS-TRV     TO TRUE
"                MOVE THIS-N-OPE     TO W-N-OPE
"             END-IF
"     *       LECTURE SUIVANTE DU CURSEUR D'ACCèS à LA TABLE TBHISFUS
"             PERFORM LEC-CUR-HIS-01
"          END-PERFORM
"
"     *    FERMETURE DU CURSEUR D'ACCèS à LA TABLE TBHISFUS
"          IF IND-FIN-TRT-HIS NOT = 'O' AND ENR-HIS-TRV
"             PERFORM FER-CUR-HIS-01
"          END-IF
"
"     *==> ACCèS à LA TABLE DETAIL HISTORIQUE
"          IF ENR-HIS-TRV
"     *       OUVERTURE DU CURSEUR D'ACCèS à LA TABLE TBHIDFUS
"             PERFORM OUV-CUR-HID-01
"
"     *       BALAYAGE DES DéTAILS HISTORIQUES
"             PERFORM UNTIL IND-FIN-TRT-HID = 'O'
"                EVALUATE THID-L-DONNEE-MODIF
"     *             Historiques client
"                   WHEN 'I-UNIQ-KPI'
"                      MOVE THID-L-DONNEE-AVANT  TO W-I-UNIQ-KPI
"     D                DISPLAY 'I-UNIQ-KPI' W-I-UNIQ-KPI
"                   WHEN 'C-NTUR-PERS'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-NTUR-PERS
"     D                DISPLAY 'W-C-NTUR-PERS' W-C-NTUR-PERS
"                   WHEN 'C-REF-GIIN'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-REF-GIIN
"     D                DISPLAY 'C-REF-GIIN' W-C-REF-GIIN
"                   WHEN 'L-NOM-NAISS'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-NOM-NAISS
"     D                DISPLAY 'L-NOM-NAISS' W-L-NOM-NAISS
"                   WHEN 'L-PRNOM'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-PRNOM
"     D                DISPLAY 'L-PRNOM' W-L-PRNOM
"                   WHEN 'D-NAISS'
                       IF THID-L-DONNEE-AVANT (1:2) IS NUMERIC
"                         MOVE THID-L-DONNEE-AVANT  TO W-D-NAISS
"     D                   DISPLAY 'D-NAISS' W-D-NAISS
"                      ELSE
"                         MOVE THID-L-DONNEE-AVANT  TO W-C-TITRE-CVLTE
"     D                   DISPLAY 'C-TITRE ' W-C-TITRE-CVLTE
"                      END-IF
"                   WHEN 'L-NOM-MRTL'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-NOM-MRTL
"     D                DISPLAY 'L-NOM-MRTL' W-L-NOM-MRTL
"                   WHEN 'C-PAYS-NLITE'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-PAYS-NLITE
"     D                DISPLAY 'C-PAYS-NLI' W-C-PAYS-NLITE
"                   WHEN 'C-AUTRE-PAYS-NLITE'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-AUTRE-PAYS-NLITE
"     D                DISPLAY 'C-A-PAYS' W-C-AUTRE-PAYS-NLITE
"                   WHEN 'C-PAYS-NAISS'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-PAYS-NAISS
"     D                DISPLAY 'C-PAYS-NAIS' W-C-PAYS-NAISS
"                   WHEN 'C-DEPT-NAISS'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-DEPT-NAISS
"     D                DISPLAY 'C-DEPT-NAIS' W-C-DEPT-NAISS
"     *             WHEN 'L-VILLE-NAISS'
"     *                MOVE THID-L-DONNEE-AVANT  TO W-L-VILLE-NAISS
"                   WHEN 'L-RAISON-SOCIALE'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-RAISON-SOCIALE
"     D                DISPLAY 'L-RAISON-SO' W-L-RAISON-SOCIALE
"     *             Historiques adresse
"                   WHEN 'L-COMM-ADR-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-COMM-ADR
"     D                DISPLAY 'L-COMM-ADR' W-L-COMM-ADR
"                   WHEN 'C-CPOST-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-CPOST
"     D                DISPLAY 'W-C-CPOSTI' W-C-CPOST
"                   WHEN 'C-PAYS-ADR-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-C-PAYS-ADR
"     D                DISPLAY 'C-PAYS-ADR' W-C-PAYS-ADR
"                   WHEN 'L-INTIT-COURR-1-AF'
"                      MOVE THID-L-DONNEE-AVANT TO W-L-INTIT-COURR-1
"     D                DISPLAY 'COURR-1   ' W-L-INTIT-COURR-1
"                   WHEN 'L-INTIT-COURR-2-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-INTIT-COURR-2
"     D                DISPLAY 'T-COURR-2' W-L-INTIT-COURR-2
"                   WHEN 'L-ADR-LIGNE-1-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-ADR-LIGNE-1
"     D                DISPLAY 'ADR-LIGNE-1' W-L-ADR-LIGNE-1
"                   WHEN 'L-ADR-LIGNE-2-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-ADR-LIGNE-2
"     D                DISPLAY 'ADR-LIGNE-2' W-L-ADR-LIGNE-2
"                   WHEN 'L-ADR-LIGNE-3-AF'
"                      MOVE THID-L-DONNEE-AVANT  TO W-L-ADR-LIGNE-3
"     D                DISPLAY 'ADR-LIGNE-3' W-L-ADR-LIGNE-3
"                   WHEN OTHER
"                      CONTINUE
"                END-EVALUATE
"     *         LECTURE SUIVANTE DU CURSEUR D'ACCèS à LA TABLE TBHIDFUS
"                PERFORM LEC-CUR-HID-01
"             END-PERFORM
"          END-IF
MC+        .

151197*                     ***                           *     *      *
151197*===================================================*
151197* OUVERTURE CURSEUR DE LA TABLE DES HISTORIQUES.
151197*===================================================*
151197 OUV-CUR-HIS-01.
151197*
151197* OUVERTURE CURSEUR
151197*
151197     MOVE 'OP'                    TO ACCE-TYPE-REQUETE
151197     MOVE '01'                    TO ACCE-NOM-FONCTION
151197* ALIMENTATION DE CLAUSE WHERE
151197*    MOVE AI11-I-IDENT-CPT        TO THIS-I-IDENT-1
MC+        MOVE WS-I-IDENT-1            TO THIS-I-IDENT-1
151197
151197* APPEL MFUSTHIS
151197     PERFORM SQ-ACCES-THIS
151197* CONTROLE CODE RETOUR
151197     EVALUATE TRUE
151197* ==>   SI OK
151197        WHEN TROUVE
151197           MOVE 'N'               TO IND-FIN-TRT-HIS
151197           CONTINUE
151197* ==>   SI KO
151197        WHEN NON-TROUVE
151197           MOVE 'O'               TO IND-FIN-TRT-HIS
151197           SET ENR-HIS-NN-TRV     TO TRUE
151197        WHEN OTHER
151197* ==>   SI ERREUR APPEL MODULE MFUSTHIS
151197           MOVE 1020                      TO WS-CODE-ABEND
151197           PERFORM 4200-ABEND-ERR
151197     END-EVALUATE
151197     .

151197*                     ***                           *     *      *
151197*===================================================*
151197* LECTURER CURSEUR DE LA TABLE DES HISTORIQUES.
151197*===================================================*
151197 LEC-CUR-HIS-01.
151197*
151197* LECTURE CURSEUR
151197*
151197     MOVE 'FE'                    TO ACCE-TYPE-REQUETE
151197     MOVE '01'                    TO ACCE-NOM-FONCTION
151197* ALIMENTATION DE CLAUSE WHERE
151197     MOVE AI11-I-IDENT-CPT        TO THIS-I-IDENT-1
151197
151197* APPEL MFUSTHIS
151197     PERFORM SQ-ACCES-THIS
151197* CONTROLE CODE RETOUR
151197     EVALUATE TRUE
151197* ==>   SI OK
151197        WHEN TROUVE
151197           CONTINUE
151197* ==>   SI KO
151197        WHEN NON-TROUVE
151197           MOVE 'O'               TO IND-FIN-TRT-HIS
151197        WHEN OTHER
151197* ==>   SI ERREUR APPEL MODULE MFUSTHIS
151197           MOVE 1021                      TO WS-CODE-ABEND
151197           PERFORM 4200-ABEND-ERR
151197     END-EVALUATE
151197     .

151197*                     ***                           *     *      *
151197*===================================================*
151197* FERMETURE CURSEUR DE LA TABLE DES HISTORIQUES.
151197*===================================================*
151197 FER-CUR-HIS-01.
151197*
151197* FERMETURE CURSEUR
151197*
151197     MOVE 'CL'                    TO ACCE-TYPE-REQUETE
151197     MOVE '01'                    TO ACCE-NOM-FONCTION
151197* ALIMENTATION DE CLAUSE WHERE
151197*    MOVE AI11-I-IDENT-CPT        TO THIS-I-IDENT-1
MC+        MOVE WS-I-IDENT-1            TO THIS-I-IDENT-1
151197
151197* APPEL MFUSTHIS
151197     PERFORM SQ-ACCES-THIS
151197* CONTROLE CODE RETOUR
151197     EVALUATE ACCE-CODE-RETOUR
151197* ==>   SI OK
151197        WHEN 0
151197           CONTINUE
151197        WHEN OTHER
151197* ==>   SI ERREUR APPEL MODULE MFUSTHIS
151197           MOVE 1022                      TO WS-CODE-ABEND
151197           PERFORM 4200-ABEND-ERR
151197     END-EVALUATE
151197     .

151197*                     ***                           *     *      *
151197*===================================================*
151197* OUVERTURE CURSEUR DE LA TABLE DETAIL HISTORIQUES.
151197*===================================================*
151197 OUV-CUR-HID-01.
151197*
151197* OUVERTURE CURSEUR
151197*
151197     MOVE 'OP'                    TO ACCE-TYPE-REQUETE
151197     MOVE '01'                    TO ACCE-NOM-FONCTION
151197* ALIMENTATION DE CLAUSE WHERE
151197     MOVE W-N-OPE                 TO THID-N-OPE
151197
151197* APPEL MFUSTHID
151197     PERFORM SQ-ACCES-THID
151197* CONTROLE CODE RETOUR
151197     EVALUATE ACCE-CODE-RETOUR
151197* ==>   SI OK
151197        WHEN 0
151197           MOVE 'N'               TO IND-FIN-TRT-HID
151197* ==>   SI KO
151197        WHEN 4
151197           MOVE 'O'               TO IND-FIN-TRT-HID
151197        WHEN OTHER
151197* ==>   SI ERREUR APPEL MODULE MFUSTHID
151197           MOVE 1023                      TO WS-CODE-ABEND
151197           PERFORM 4200-ABEND-ERR
151197     END-EVALUATE
151197     .

151197*                     ***                           *     *      *
151197*===================================================*
151197* LECTURER CURSEUR DE LA TABLE DETAIL HISTORIQUES.
151197*===================================================*
151197 LEC-CUR-HID-01.
151197*
151197* LECTURE CURSEUR
151197*
151197     MOVE 'FE'                    TO ACCE-TYPE-REQUETE
151197     MOVE '01'                    TO ACCE-NOM-FONCTION
151197* ALIMENTATION DE CLAUSE WHERE
151197     MOVE W-N-OPE                 TO THID-N-OPE
151197
151197* APPEL MFUSTHID
151197     PERFORM SQ-ACCES-THID
151197* CONTROLE CODE RETOUR
151197     EVALUATE ACCE-CODE-RETOUR
151197* ==>   SI OK
151197        WHEN 0
151197           CONTINUE
151197* ==>   SI KO
151197        WHEN 4
151197           MOVE 'O'               TO IND-FIN-TRT-HID
151197        WHEN OTHER
151197* ==>   SI ERREUR APPEL MODULE MFUSTHID
151197           MOVE 1024                      TO WS-CODE-ABEND
151197           PERFORM 4200-ABEND-ERR
151197     END-EVALUATE
           .

      *                     ***                           *     *      *
      *===================================================*
      * OUVERTURE CURSEUR DE LA TABLE DES MONTANTS.
      *===================================================*
       2111-OUV-CUR-RUB-01.
      *
      * OUVERTURE CURSEUR
      *
           MOVE 'OP'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE TCNT-I-IDENT-CPT        TO TRUB-I-IDENT-CPT

      * APPEL MFUSTRUB
           PERFORM SQ-ACCES-TRUB
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
150978           MOVE 1                 TO WS-I
      * ==>   SI KO
              WHEN NON-TROUVE
                 MOVE 'O'               TO IND-FIN-TRT-RUB
              WHEN OTHER
      * ==>   SI ERREUR APPEL MODULE MFUSTRUB
                 MOVE 1003                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *

      *                     ***                           *     *      *
      *===================================================*
      * TRAITEMENT DE CUMUL DES MONTANTS
      *===================================================*
       2112-TRT-CUMULE.
      *
      * TRAITEMENT CUMULE MONTANT SELON LE TYPE MONTANT
      *
      * Tester le type de montant
           EVALUATE TRUE
      * ==>   SI 01
150978        WHEN TRUB-C-TYPE-MNT = 1
      *   **     Cumul
150978D          DISPLAY ' WS-M-MNT-ASS01 ' WS-M-MNT-ASS
MCHA- *          ADD WS-M-MNT-ASS , TRUB-M-MNT-ASS TO WS-M-MNT-ASS
MCHA+            ADD TRUB-M-MNT-ASS                TO WS-M-MNT-ASS
150978                                                WS-M-TOT-SOL-DECL
150978                                                WS-M-TOT-M-DECL
150978           MOVE TRUB-Q-NBR-DEC               TO WS-Q-NBR-DEC
150978D          DISPLAY ' WS-M-MNT-ASS1  ' WS-M-MNT-ASS
150978D          DISPLAY ' TR-M-MNT-ASS1  ' TRUB-M-MNT-ASS
150978           MOVE TRUB-Q-NBR-DEC               TO WS-Q-NBR-DEC
150978* ==>   SI 06
150978        WHEN TRUB-C-TYPE-MNT = 6
150978*   **     Cumul
150978D          DISPLAY ' WS-M-MNT-ASS06 ' WS-M-MNT-ASS
MCHA- *          ADD WS-M-MNT-ASS , TRUB-M-MNT-ASS TO WS-M-MNT-ASS
MCHA+            ADD TRUB-M-MNT-ASS                TO WS-M-MNT-ASS
150978                                                WS-M-TOT-SOL-DECL
150978                                                WS-M-TOT-M-DECL
150978           MOVE TRUB-Q-NBR-DEC               TO WS-Q-NBR-DEC
150978D          DISPLAY ' WS-M-MNT-ASS6  ' WS-M-MNT-ASS
150978D          DISPLAY ' TR-M-MNT-ASS6  ' TRUB-M-MNT-ASS
150978* ==>   Autres
150978        WHEN OTHER
150978           MOVE TRUB-C-TYPE-MNT TO EL10-C-TYP-MNT (WS-I)
150978           MOVE TRUB-Q-NBR-DEC  TO EL10-Q-NBR-DEC
150978                                OF EL10-G-DATA-DET-CNT (WS-I)
150978           MOVE TRUB-M-MNT-ASS  TO EL10-M-MNT-ASS (WS-I)
150978           ADD   1              TO WS-I EL10-Q-LIST-MNT
150978
150978           ADD TRUB-M-MNT-ASS     TO WS-M-TOT-M-DECL
150978
150978           IF TRUB-C-TYPE-MNT = 2
150978              ADD  TRUB-M-MNT-ASS TO WS-M-TOT-INT-DECL
150978           END-IF
150978
150978           IF TRUB-C-TYPE-MNT = 5
150978              ADD  TRUB-M-MNT-ASS TO WS-M-TOT-CES-DECL
150978           END-IF
150978     END-EVALUATE
           .

151197*                     ***                           *     *      *
151197*===================================================*
151197* TRAITEMENT DE CUMUL DES MONTANTS HISTORIQUE
151197*===================================================*
151197 2112-TRT-CUMULE-HIS.
151197*
151197* TRAITEMENT CUMULE MONTANT SELON LE TYPE MONTANT
151197*
151197* TESTER LE TYPE DE MONTANT
151197     EVALUATE TRUE
151197* ==>   SI 01
151197        WHEN TRUB-C-TYPE-MNT = 1
151197*   **     CUMUL
151197           IF W-M-MNT-ASS = ZEROES
151197              ADD WS-M-MNT-ASS , TRUB-M-MNT-ASS TO WS-M-MNT-ASS
151197                                                WS-M-TOT-SOL-DECL
151197                                                WS-M-TOT-M-DECL
151197           ELSE
151197              ADD WS-M-MNT-ASS , W-M-MNT-ASS TO WS-M-MNT-ASS
151197                                                WS-M-TOT-SOL-DECL
151197                                                WS-M-TOT-M-DECL
151197           END-IF
151197           MOVE TRUB-Q-NBR-DEC               TO WS-Q-NBR-DEC
151197* ==>   SI 06
151197        WHEN TRUB-C-TYPE-MNT = 6
151197*   **     CUMUL
151197           IF W-M-MNT-ASS-06 = ZEROES
151197              ADD WS-M-MNT-ASS , TRUB-M-MNT-ASS TO WS-M-MNT-ASS
151197                                                WS-M-TOT-SOL-DECL
151197                                                WS-M-TOT-M-DECL
151197           ELSE
151197              ADD WS-M-MNT-ASS , W-M-MNT-ASS-06 TO WS-M-MNT-ASS
151197                                                WS-M-TOT-SOL-DECL
151197                                                WS-M-TOT-M-DECL
151197           END-IF
151197           MOVE TRUB-Q-NBR-DEC               TO WS-Q-NBR-DEC
151197* ==>   AUTRES
151197        WHEN OTHER
151197           MOVE TRUB-C-TYPE-MNT TO EL10-C-TYP-MNT (WS-I)
151197           MOVE TRUB-Q-NBR-DEC  TO EL10-Q-NBR-DEC
151197                                OF EL10-G-DATA-DET-CNT (WS-I)
151197           MOVE TRUB-M-MNT-ASS  TO EL10-M-MNT-ASS (WS-I)
151197           ADD   1              TO WS-I EL10-Q-LIST-MNT
151197
151197           ADD TRUB-M-MNT-ASS     TO WS-M-TOT-M-DECL
151197
151197           IF TRUB-C-TYPE-MNT = 2
151197              ADD  TRUB-M-MNT-ASS TO WS-M-TOT-INT-DECL
151197           END-IF
151197
151197           IF TRUB-C-TYPE-MNT = 5
151197              ADD  TRUB-M-MNT-ASS TO WS-M-TOT-CES-DECL
151197           END-IF
151197     END-EVALUATE
151197     .
      *===================================================*
      * LECTURE CURSEUR DE LA TABLE DES MONTANTS.
      *===================================================*
       2113-FET-CUR-RUB-01.
      *
      * LECTURE CURSEUR
      *
           MOVE 'FE'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION

      * APPEL MFUSTRUB
           PERFORM SQ-ACCES-TRUB
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI KO
              WHEN NON-TROUVE
                 MOVE 'O'               TO IND-FIN-TRT-RUB
              WHEN OTHER
      * ==>   SI ERREUR APPEL MODULE MFUSTRUB
                 MOVE 1004                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *

      *                         *****                               *  *
      * =============================================================  *
       2115-ALIM-INF-CNT.
      *
      *    CALCUL REFERENCE BLOC
150978*    MOVE "FATCA1"               TO   WS-TYPE-FATCA
"     *                                OF   WS-I-REF-BLOC-CPT
150978     IF WS-TYPE-FATCA-TST OF WS-BLOC-CPT-TST = 'FATCA11' OR
150978        'FATCA12' OR 'FATCA13' OR 'FATCA14'
150978        MOVE W-DATE-TIMESTAMP       TO   WS-DATE-TIMESTAMP-TST
150978                                    OF   WS-BLOC-CPT-TST
150978        MOVE LST-ENT-DECL           TO   WS-GIIN-TST
150978                                    OF   WS-BLOC-CPT-TST
150978        MOVE TCNT-I-UNIQ-KAC        TO   WS-I-UNIQ-KAC-TST
150978                                    OF   WS-BLOC-CPT-TST
150978        MOVE TCLI-I-UNIQ-KPI        TO   WS-I-UNIQ-KPI-TST
150978                                    OF   WS-BLOC-CPT-TST
150978        MOVE '-'                    TO   WS-FILLER-01-TST
150978                                    OF   WS-BLOC-CPT-TST
150978                                         WS-FILLER-02-TST
150978                                    OF   WS-BLOC-CPT-TST
150978                                         WS-FILLER-03-TST
150978                                    OF   WS-BLOC-CPT-TST
150978                                         WS-FILLER-04-TST
150978                                    OF   WS-BLOC-CPT-TST
150978     ELSE
150978        MOVE W-DATE-TIMESTAMP       TO   WS-DATE-TIMESTAMP
150978                                    OF   WS-BLOC-CPT
150978        MOVE LST-ENT-DECL           TO   WS-GIIN
150978                                    OF   WS-BLOC-CPT
150978        MOVE TCNT-I-UNIQ-KAC        TO   WS-I-UNIQ-KAC
150978                                    OF   WS-BLOC-CPT
150978        MOVE TCLI-I-UNIQ-KPI        TO   WS-I-UNIQ-KPI
150978                                    OF   WS-BLOC-CPT
150978        MOVE '-'                    TO   WS-FILLER-01
150978                                    OF   WS-BLOC-CPT
150978                                         WS-FILLER-02
150978                                    OF   WS-BLOC-CPT
150978                                         WS-FILLER-03
150978                                    OF   WS-BLOC-CPT
150978                                         WS-FILLER-04
150978                                    OF   WS-BLOC-CPT
150978     END-IF
      *
      *    ALIMENTATION DES INFORMATIONS COMPTE
      *
      *    Initialisation
150978     MOVE SPACE                  TO   EL10-G-DATA-DET-CNT

      *    Code enregistrement
           MOVE '20'                   TO   EL10-C-ENR
      *    Identifiant de lentité déclarante
           MOVE TCNT-I-IDENT-CPT       TO   EL10-I-IDENT
      *    N° Ligne /Identifiant
           MOVE 1                      TO   EL10-N-LIG-IDENT
      *    Entité concernée
           MOVE 'CNT'                  TO   EL10-C-ENTIT
      *    Le numéro du contrat
           MOVE TCNT-I-UNIQ-KAC        TO   EL10-I-UNIQ-KAC
                                       OF EL10-G-DATA-DET-CNT
      *
           IF TCNT-C-IBAN  NOT = LOW-VALUE AND SPACES
               MOVE TCNT-C-IBAN        TO   EL10-I-KAC-IBAN
           ELSE
               IF TCNT-I-RIB-INVAR NOT = LOW-VALUE AND SPACES
                  MOVE TCNT-I-RIB-INVAR   TO   EL10-I-KAC-IBAN
               ELSE
                  IF TCNT-I-UNIQ-KAC NOT = LOW-VALUE AND SPACES
                     MOVE TCNT-I-UNIQ-KAC    TO   EL10-I-KAC-IBAN
                  ELSE
                     IF TCLI-I-UNIQ-KPI NOT = LOW-VALUE AND SPACES
                        MOVE TCLI-I-UNIQ-KPI    TO   EL10-I-KAC-IBAN
                     ELSE
                        MOVE 'NANUM'            TO   EL10-I-KAC-IBAN
                     END-IF
                  END-IF
               END-IF
           END-IF
      *
      *    Le solde et montant
150978     MOVE WS-M-MNT-ASS           TO   EL10-M-MNT-ASS-SLD
"     *    Le nombre de décimaux du montant associé au compte
"          MOVE WS-Q-NBR-DEC           TO   EL10-Q-NBR-DEC-SLD
"     *    Le code devise
"          MOVE TCNT-C-DEV             TO   EL10-C-DEV
"                                      OF   EL10-G-DATA-DET-CNT
"     *    Reference du bloc
"          MOVE WS-I-REF-BLOC-CPT      TO   EL10-I-REF-BLOC
"                                      OF   EL10-G-DATA-DET-CNT
"     *    Reference du bloc initial
MCHA+-*    MOVE AI11-I-REF-FIC-INIT    OF   AI11-ENR-PERS-CPT
MCHA+-*                                TO   EL10-I-REF-BLOC-INIT
MCHA++     MOVE AI11-I-REF-BLOC        OF   AI11-ENR-PERS-CPT
"                                      TO   EL10-I-REF-BLOC-INIT
"                                      OF   EL10-G-DATA-DET-CNT
MC    *    REFERENCE DU BLOC INITIAL
"          MOVE AI11-I-REF-FIC-INIT    OF   AI11-ENR-PERS-CPT
"                                      TO   EL10-I-REF-FID-INIT
"                                      OF   EL10-G-DATA-DET-CNT
MC         .
150978
           .

151197*                         *****                               *  *
151197* =============================================================  *
151197 2115-ALIM-INF-CNT-HIS.
151197*
151197*    CALCUL REFERENCE BLOC
151197*    MOVE "FATCA1"               TO   WS-TYPE-FATCA
151197*                                OF   WS-I-REF-BLOC-CPT
151197     IF WS-TYPE-FATCA-TST OF WS-BLOC-CPT-TST = 'FATCA11' OR
151197        'FATCA12' OR 'FATCA13' OR 'FATCA14'
151197        MOVE W-DATE-TIMESTAMP       TO   WS-DATE-TIMESTAMP-TST
151197                                    OF   WS-BLOC-CPT-TST
151197        MOVE LST-ENT-DECL           TO   WS-GIIN-TST
151197                                    OF   WS-BLOC-CPT-TST
151197        MOVE TEMP-I-UNIQ-KAC        TO   WS-I-UNIQ-KAC-TST
151197                                    OF   WS-BLOC-CPT-TST
151197        MOVE TCLI-I-UNIQ-KPI        TO   WS-I-UNIQ-KPI-TST
151197                                    OF   WS-BLOC-CPT-TST
151197        MOVE '-'                    TO   WS-FILLER-01-TST
151197                                    OF   WS-BLOC-CPT-TST
151197                                         WS-FILLER-02-TST
151197                                    OF   WS-BLOC-CPT-TST
151197                                         WS-FILLER-03-TST
151197                                    OF   WS-BLOC-CPT-TST
151197                                         WS-FILLER-04-TST
151197                                    OF   WS-BLOC-CPT-TST
151197     ELSE
151197        MOVE W-DATE-TIMESTAMP       TO   WS-DATE-TIMESTAMP
151197                                    OF   WS-BLOC-CPT
151197        MOVE LST-ENT-DECL           TO   WS-GIIN
151197                                    OF   WS-BLOC-CPT
151197        MOVE TEMP-I-UNIQ-KAC        TO   WS-I-UNIQ-KAC
151197                                    OF   WS-BLOC-CPT
151197        MOVE TCLI-I-UNIQ-KPI        TO   WS-I-UNIQ-KPI
151197                                    OF   WS-BLOC-CPT
151197        MOVE '-'                    TO   WS-FILLER-01
151197                                    OF   WS-BLOC-CPT
151197                                         WS-FILLER-02
151197                                    OF   WS-BLOC-CPT
151197                                         WS-FILLER-03
151197                                    OF   WS-BLOC-CPT
151197                                         WS-FILLER-04
151197                                    OF   WS-BLOC-CPT
151197     END-IF
151197*
151197*    ALIMENTATION DES INFORMATIONS COMPTE
151197*
151197*    INITIALISATION
151197     MOVE SPACE                  TO   EL10-G-DATA-DET-CNT
151197
151197*    CODE ENREGISTREMENT
151197     MOVE '20'                   TO   EL10-C-ENR
151197*    IDENTIFIANT DE LENTITé DéCLARANTE
151197     MOVE TCNT-I-IDENT-CPT       TO   EL10-I-IDENT
151197*    N° LIGNE /IDENTIFIANT
151197     MOVE 1                      TO   EL10-N-LIG-IDENT
151197*    ENTITé CONCERNéE
151197     MOVE 'CNT'                  TO   EL10-C-ENTIT
151197*    LE NUMéRO DU CONTRAT
151197     MOVE TEMP-I-UNIQ-KAC        TO   EL10-I-UNIQ-KAC
151197                                 OF EL10-G-DATA-DET-CNT
151197*
151197     IF TEMP-C-IBAN  NOT = LOW-VALUE AND SPACES
151197         MOVE TEMP-C-IBAN        TO   EL10-I-KAC-IBAN
151197     ELSE
               IF TEMP-I-RIB-INVAR NOT = LOW-VALUE AND SPACES
                  MOVE TEMP-I-RIB-INVAR   TO   EL10-I-KAC-IBAN
               ELSE
151197            IF TEMP-I-UNIQ-KAC NOT = LOW-VALUE AND SPACES
151197               MOVE TEMP-I-UNIQ-KAC    TO   EL10-I-KAC-IBAN
151197            ELSE
151197               IF TCLI-I-UNIQ-KPI NOT = LOW-VALUE AND SPACES
151197                  MOVE TCLI-I-UNIQ-KPI    TO   EL10-I-KAC-IBAN
151197               ELSE
151197                  MOVE 'NANUM'            TO   EL10-I-KAC-IBAN
151197               END-IF
                  END-IF
151197         END-IF
151197     END-IF
151197D    DISPLAY ' EL10-I-KAC-IBAN :' EL10-I-KAC-IBAN
151197
151197*    LE SOLDE ET MONTANT
151197     MOVE WS-M-MNT-ASS           TO   EL10-M-MNT-ASS-SLD
151197*    LE NOMBRE DE DéCIMAUX DU MONTANT ASSOCIé AU COMPTE
151197     MOVE WS-Q-NBR-DEC           TO   EL10-Q-NBR-DEC-SLD
151197*    LE CODE DEVISE
151197     MOVE TEMP-C-DEV             TO   EL10-C-DEV
151197                                 OF   EL10-G-DATA-DET-CNT
151197*    REFERENCE DU BLOC
151197     MOVE WS-I-REF-BLOC-CPT      TO   EL10-I-REF-BLOC
151197                                 OF   EL10-G-DATA-DET-CNT
151197*    REFERENCE DU BLOC INITIAL
151197     MOVE AI11-I-REF-BLOC        OF   AI11-ENR-PERS-CPT
151197                                 TO   EL10-I-REF-BLOC-INIT
151197                                 OF   EL10-G-DATA-DET-CNT
MC    *    REFERENCE DU BLOC INITIAL
"          MOVE AI11-I-REF-FIC-INIT    OF   AI11-ENR-PERS-CPT
"                                      TO   EL10-I-REF-FID-INIT
"                                      OF   EL10-G-DATA-DET-CNT
MC         .

151197*                     ***                           *     *      *
151197*=============================================================*
151197* REMPLISSAGE DES DONNEES CONTRAT DEPUIS LA TABLE THID OU TCNT
151197*=============================================================*
151197 REMPLIR-DONNEE-CONTRAT.
151197*
151197*  ==> VERIFIER SI LES DONNéES HISTORIQUES SONT RENSEIGNéES
151197     IF W-I-UNIQ-KAC = SPACES
151197        MOVE TCNT-I-UNIQ-KAC   TO TEMP-I-UNIQ-KAC
151197     ELSE
151197        MOVE W-I-UNIQ-KAC      TO TEMP-I-UNIQ-KAC
151197     END-IF
151197
151197     IF W-C-IBAN = SPACES
151197        MOVE TCNT-C-IBAN       TO TEMP-C-IBAN
151197     ELSE
151197        MOVE W-C-IBAN          TO TEMP-C-IBAN
151197     END-IF
151197
MC         IF W-I-RIB-INVAR = SPACES
"             MOVE TCNT-I-RIB-INVAR  TO TEMP-I-RIB-INVAR
"          ELSE
"             MOVE W-I-RIB-INVAR     TO TEMP-I-RIB-INVAR
MC         END-IF
151197
151197     IF W-C-DEV = SPACES
151197        MOVE TCNT-C-DEV        TO TEMP-C-DEV
151197     ELSE
151197        MOVE W-C-DEV           TO TEMP-C-DEV
151197     END-IF
151197     .
      *                         *****                               *  *
MC+   *=============================================================*
"     * REMPLISSAGE DES DONNEES CLIENT  DEPUIS LA TABLE THID OU TCNT
"     *=============================================================*
"      REMPLIR-DONNEE-CLIENT.
"     *
"     *  ==> VERIFIER SI LES DONNéES HISTORIQUES SONT RENSEIGNéES
"          IF W-I-UNIQ-KPI = SPACES OR LOW-VALUE
"             MOVE TCLI-I-UNIQ-KPI   TO TEMP-I-UNIQ-KPI
"          ELSE
"             MOVE W-I-UNIQ-KPI      TO TEMP-I-UNIQ-KPI
"          END-IF
"
"          IF W-C-NTUR-PERS = SPACES OR LOW-VALUE
"             MOVE TCLI-C-NTUR-PERS  TO TEMP-C-NTUR-PERS
"          ELSE
"             MOVE W-C-NTUR-PERS     TO TEMP-C-NTUR-PERS
"          END-IF
"
"          IF W-C-REF-GIIN = SPACES OR LOW-VALUE
"             MOVE TCLP-C-REF-GIIN  TO TEMP-C-REF-GIIN
"          ELSE
"             MOVE W-C-REF-GIIN     TO TEMP-C-REF-GIIN
"          END-IF
"
"          IF W-L-NOM-NAISS = SPACES OR LOW-VALUE
"             MOVE TCLP-L-NOM-NAISS        TO TEMP-L-NOM-NAISS
"          ELSE
"             MOVE W-L-NOM-NAISS           TO TEMP-L-NOM-NAISS
"          END-IF
"
"          IF W-C-TITRE-CVLTE = SPACES OR LOW-VALUE
"             MOVE TCLP-C-TITRE-CVLTE      TO TEMP-C-TITRE-CVLTE
"          ELSE
"             MOVE W-C-TITRE-CVLTE         TO TEMP-C-TITRE-CVLTE
"          END-IF
"
"          IF W-L-PRNOM = SPACES  OR LOW-VALUE
"             MOVE TCLP-L-PRNOM            TO TEMP-L-PRNOM
"          ELSE
"             MOVE W-L-PRNOM               TO TEMP-L-PRNOM
"          END-IF
"
"          IF W-D-NAISS = SPACES OR LOW-VALUE
"             MOVE TCLP-D-NAISS            TO TEMP-D-NAISS
"          ELSE
"             MOVE W-D-NAISS               TO TEMP-D-NAISS
"          END-IF
"
"          IF W-L-NOM-MRTL = SPACES OR LOW-VALUE
"             MOVE TCLP-L-NOM-MRTL         TO TEMP-L-NOM-MRTL
"          ELSE
"             MOVE W-L-NOM-MRTL            TO TEMP-L-NOM-MRTL
"          END-IF
"
"          IF W-C-PAYS-NLITE = SPACES OR LOW-VALUE
"             MOVE TCLP-C-PAYS-NLITE       TO TEMP-C-PAYS-NLITE
"          ELSE
"             MOVE W-C-PAYS-NLITE          TO TEMP-C-PAYS-NLITE
"          END-IF
"
"          IF W-C-PAYS-NAISS = SPACES OR LOW-VALUE
"             MOVE TCLP-C-PAYS-NAISS       TO TEMP-C-PAYS-NAISS
"          ELSE
"             MOVE W-C-PAYS-NAISS          TO TEMP-C-PAYS-NAISS
"          END-IF
"
"          IF W-C-AUTRE-PAYS-NLITE = SPACES OR LOW-VALUE
"             MOVE TCLP-C-AUTRE-PAYS-NLITE TO TEMP-C-AUTRE-PAYS-NLITE
"          ELSE
"             MOVE W-C-AUTRE-PAYS-NLITE    TO TEMP-C-AUTRE-PAYS-NLITE
"          END-IF
"
"          IF W-C-DEPT-NAISS = SPACES OR LOW-VALUE
"             MOVE TCLP-C-DEPT-NAISS       TO TEMP-C-DEPT-NAISS
"          ELSE
"             MOVE W-C-DEPT-NAISS          TO TEMP-C-DEPT-NAISS
"          END-IF
"
"     *    IF W-L-VILLE-NAISS = SPACES OR LOW-VALUE
"     *       MOVE TCLP-L-VILLE-NAISS      TO TEMP-L-VILLE-NAISS
"     *    ELSE
"     *       MOVE W-L-VILLE-NAISS         TO TEMP-L-VILLE-NAISS
"     *    END-IF
"
"          IF W-L-RAISON-SOCIALE = SPACES OR LOW-VALUE
"             MOVE TCLM-L-RAISON-SOCIALE   TO TEMP-L-RAISON-SOCIALE
"          ELSE
"             MOVE W-L-RAISON-SOCIALE      TO TEMP-L-RAISON-SOCIALE
"          END-IF
MC+        .
      *                         *****                               *  *
      *                     ***                           *     *      *
      *===================================================*
      * TRAITEMENT CLIENT
      *===================================================*
       2116-TRAITEMENT-CLI.
      *
      *  ==> SELON LA NATURE RéCUPERER LES INFORMATIONS CLIENTS
           IF TCLI-C-NTUR-PERS = '01'
              PERFORM 2116-TRAITEMENT-CLI-PP
           ELSE
              PERFORM 2116-TRAITEMENT-CLI-PM
           END-IF
      *  ==> Alimentation de la ligne numéro zéro
"     D    DISPLAY ' SYSIN-TYPE-FATCA13' SYSIN-TYPE-FATCA13
MC+        IF SYSIN-TYPE-FATCA13 = 'FATCA31A' OR 'FATCA31A1'
"             PERFORM REMPLIR-DONNEE-CLIENT
"             PERFORM 2116-ECRIRE-ID-CLI-ALIM-HIS
"          ELSE
"             PERFORM 2116-ECRIRE-ID-CLI-ALIM
"          END-IF
MC+   *    PERFORM 2116-ECRIRE-ID-CLI-ALIM

      *  ==> Ecriture ligne identifiant client
           PERFORM 1360-ECRIRE-OUT

      *  ==> Ecriture ligne identifiant client
           PERFORM 2116-RECUP-ADR
      *  ==> POUR PM
150978     IF TCLI-C-NTUR-PERS = '02' AND TCLM-C-CLSFN-REEL = '05'

      *  ==> Chercher les liens PERSONNE/PERSONNE
              PERFORM 2117-RECUP-LPP
           END-IF
      *  ==> Ecrire un enregistrement pour la mise à jour du statut ECV
           PERFORM 2116-MAJ-CLI

      *  ==> Ecriture ligne identifiant client
           PERFORM 1360-ECRIRE-OUT
           .

      *                      ***                 * *         * *   ** *
      *                     ***                           *     *      *
150978*===================================================*
"     * ALIMENTATION DES DONNEES REP
"     *===================================================*
"      2117-ALIM-INF-REP.
"     *
"     *  ==> SELON LA NATURE RéCUPERER LES INFORMATIONS CLIENTS
"
151197     MOVE SPACES                  TO EL10-G-DATA-DET-REP
"          MOVE '40'                    TO EL10-C-ENR
"
"          MOVE 1                       TO EL10-N-LIG-IDENT
"
"          MOVE 'REP'                   TO EL10-C-ENTIT
"
"     *  ==> CODE OPERATION
"          MOVE 'CRE'                   TO EL10-C-OPE
"                                       OF EL10-G-DATA-DET-REP
"     *  ==> LE NOM DE LA TABLE A METTRE A JOUR
"          MOVE 'TBREPFUS'              TO EL10-NOM-TABLE
"                                       OF EL10-G-DATA-DET-REP
"     *  ==> ANNEE FISCALE
"          MOVE WS-A-APPL               TO EL10-A-APPL
"                                       OF EL10-G-DATA-DET-REP
"     *  ==> IDENTIFIANT INTERNE REFERENCE FICHIER
"          MOVE WS-I-IDENT-REF-REP      TO EL10-I-IDENT-REF-REP
"                                       OF EL10-G-DATA-DET-REP
"     *  ==> IDENTIFIANT COMPTE
"          MOVE AI11-I-IDENT-CPT        TO EL10-I-IDENT-CPT
"                                       OF EL10-G-DATA-DET-REP
"     *  ==> IDENTIFIANT CLIENT
"          MOVE AI11-I-IDENT-CLT        TO EL10-I-IDENT-CLT
"                                       OF EL10-G-DATA-DET-REP
"     *  ==> REFERENCE BLOC COMPTE
"          MOVE WS-I-REF-BLOC-CPT       TO EL10-I-REF-BLOC
"                                       OF EL10-G-DATA-DET-REP
150978     .
      *                      ***                 * *         * *   ** *
151197*===================================================*
151197* ALIMENTATION DES DONNEES LCC
151197*===================================================*
151197 2119-ALIM-INF-LCC.
151197*
151197*  ==> SELON LA NATURE RéCUPERER LES INFORMATIONS CLIENTS
151197
151197     MOVE SPACES                  TO EL10-G-DATA-DET-LCC
151197     MOVE '40'                    TO EL10-C-ENR
151197
151197     MOVE 1                       TO EL10-N-LIG-IDENT
151197
151197     MOVE 'LCC'                   TO EL10-C-ENTIT
151197
151197*  ==> CODE OPERATION
151197     MOVE 'MAJ'                   TO EL10-C-OPE
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> LE NOM DE LA TABLE A METTRE A JOUR
151197     MOVE 'TBLCCFUS'              TO EL10-NOM-TABLE
151197                                  OF EL10-G-DATA-DET-LCC
151197*  ==> REPORTABILITé DU CONTRAT
151197     MOVE TLCC-C-PRTB-CNTRT       TO EL10-C-PRTB-CNTRT
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> TIMESTAMP DE CRéATION OU DE MAJ
151197     MOVE TLCC-D-CRE-MAJ          TO EL10-D-CRE-MAJ
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> IDENTIFIANT TECHNIQUE TABLE CLIENT
151197     MOVE TLCC-I-IDENT-CLT        TO EL10-I-IDENT-CLT
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> IDENTIFIANT TECHNIQUE TABLE COMPTE
151197     MOVE TLCC-I-IDENT-CPT        TO EL10-I-IDENT-CPT
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> ETAT CYCLE DE VIE LIEN
151197     MOVE '05'                    TO EL10-C-ECV-LIEN
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> CODE ACTION
151197     MOVE TLCC-C-ACTION           TO EL10-C-ACTION
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197*  ==> CODE MOTIF
151197     MOVE TLCC-C-MOTIF            TO EL10-C-MOTIF
151197                                  OF EL10-G-DATA-DET-LCC
151197
151197     .
      *                      ***                 * *         * *   ** *
      *===================================================*
      * SELECTION DE LA TABLE TBCLIFUS
      *===================================================*
       2114-TRAITEMENT-CLI-SEL.
      *
      * SELECTION
      *
           MOVE 'SE'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE AI11-I-IDENT-CLT        TO TCLI-I-IDENT-CLT
           MOVE WS-A-APPL               TO TCLI-A-APPL
           MOVE TCNT-C-SYS-ORIGINE      TO TCLI-C-SYS-ORIGINE
      * APPEL MFUSTCLI
           PERFORM SQ-ACCES-TCLI
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI ERREUR APPEL MODULE MFUSTCLI OU KO
              WHEN OTHER
                 MOVE 1006                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *                      ***                 * *         * *   ** *
      *===================================================*
      * SELECTION DE LA TABLE TBCLPFUS POUR PP
      *===================================================*
       2116-TRAITEMENT-CLI-PP.
      *
      * RéCUPéRER LES INFORMATIONS DU CLIENT PHYSIQUE
      *
           MOVE 'SE'                    TO ACCE-TYPE-REQUETE
           MOVE '01-SEL'                TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE TCLI-I-IDENT-CLT        TO TCLP-I-IDENT-CLT
      D    DISPLAY 'TCLI-I-IDENT-CLT = ' TCLI-I-IDENT-CLT
      * APPEL MFUSTCLP
           PERFORM SQ-ACCES-TCLP
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI ERREUR APPEL MODULE MFUSTCLP OU KO
              WHEN OTHER
                 MOVE 1007                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *===================================================*
      * SELECTION DE LA TABLE TBCLMFUS POUR PM
      *===================================================*
       2116-TRAITEMENT-CLI-PM.
      *
      * RéCUPéRER LES INFORMATIONS DU CLIENT MORAL
      *
           MOVE 'SE'                    TO ACCE-TYPE-REQUETE
           MOVE '01-SEL'                TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE TCLI-I-IDENT-CLT        TO TCLM-I-IDENT-CLT

      * APPEL MFUSTCLM
           PERFORM SQ-ACCES-TCLM
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI ERREUR APPEL MODULE MFUSTCLM OU KO
              WHEN OTHER
                 MOVE 1008                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
151197*                      ***                 * *         * *   ** *
151197*===================================================*
151197* SELECTION DE LA TABLE TBLCCFUS POUR COUPLE CNT/CLI
151197*===================================================*
151197 2118-TRAITEMENT-LCC.
151197*
151197* RéCUPéRER LES INFORMATIONS DU CLIENT MORAL
151197*
151197     MOVE 'SE'                    TO ACCE-TYPE-REQUETE
151197     MOVE '03'                    TO ACCE-NOM-FONCTION
151197* Alimentation de clause Where
151197     MOVE TCLI-I-IDENT-CLT        TO TLCC-I-IDENT-CLT
151197     MOVE TCNT-I-IDENT-CPT        TO TLCC-I-IDENT-CPT
151197D    DISPLAY 'TLCC-I-IDENT-CLT  :' TLCC-I-IDENT-CLT
151197D    DISPLAY 'TLCC-I-IDENT-CPT  :' TLCC-I-IDENT-CPT
151197D    DISPLAY 'ACCE-TYPE-REQUETE :' ACCE-TYPE-REQUETE
151197D    DISPLAY 'ACCE-NOM-FONCTION :' ACCE-NOM-FONCTION
151197
151197* APPEL MFUSTLCC
151197     PERFORM SQ-ACCES-TLCC
151197* CONTROLE CODE RETOUR
151197     EVALUATE TRUE
151197* ==>   SI OK
151197        WHEN TROUVE
151197           CONTINUE
151197* ==>   SI ERREUR APPEL MODULE MFUSTLCC OU KO
151197        WHEN OTHER
151197           MOVE 2700                      TO WS-CODE-ABEND
151197           PERFORM 4200-ABEND-ERR
151197     END-EVALUATE
151197     .
      *                         *****                               *  *
      * =============================================================  *
      *            ALIMENTATION DES INFORMATIONS CLIENT
      * =============================================================  *
       2116-ECRIRE-ID-CLI-ALIM.
      *
      *    ALIMENTATION DES INFORMATIONS
      *
      *    Initialisation
           MOVE SPACES                 TO   EL10-G-DATA-TET-END
150978                                      EL10-G-DATA-DET-CLT

      *    Code enregistrement
           MOVE '20'                   TO   EL10-C-ENR
      *    Identifiant de lentité déclarante
           MOVE TCNT-I-IDENT-CPT       TO   EL10-I-IDENT
      *    N° Ligne /Identifiant
           IF WS-I-IDENT NOT = TCNT-I-IDENT-CPT
              MOVE 1                   TO   EL10-N-LIG-IDENT
           ELSE
              ADD 1                    TO   EL10-N-LIG-IDENT
           END-IF
           MOVE TCNT-I-IDENT-CPT       TO   WS-I-IDENT
      *    Entité concernée
           MOVE 'CLI'                  TO   EL10-C-ENTIT
      *    Identifiant client
           MOVE TCLI-I-UNIQ-KPI        TO   EL10-I-UNIQ-KPI
                                       OF   EL10-G-DATA-DET-CLT
      *    Identifiant client
           MOVE TCLI-C-NTUR-PERS       TO   EL10-C-NTUR-PERS
                                       OF   EL10-G-DATA-DET-CLT
           IF TCLI-C-NTUR-PERS = '01'
      *    TIN ou NIF du titulaire du compte (PP)
              MOVE TCLP-C-REF-GIIN     TO   EL10-C-REF-GIIN
                                       OF   EL10-G-DATA-DET-CLT
      *    Le nom patronymique du titulaire (PP)
              MOVE TCLP-L-NOM-NAISS    TO   EL10-L-NOM-NAISS
                                       OF   EL10-G-DATA-DET-CLT
      *    Titre civilité (PP)
              MOVE TCLP-C-TITRE-CVLTE  TO   EL10-C-TITRE-CVLTE
                                       OF   EL10-G-DATA-DET-CLT
      *    Calcul prénom du client
MCHA- *       UNSTRING TCLP-L-PRNOM DELIMITED " "
MCHA- *                              INTO EL10-L-PRENOM
MCHA- *                                   EL10-L-PRENOM-2
MCHA+ *    Le prénom du titulaire (PP)
MCHA+         MOVE TCLP-L-PRNOM        TO   EL10-L-PRENOM
MCHA+                                  OF   EL10-G-DATA-DET-CLT
      *    La date de naissance du titulaire du compte (PP)
              MOVE TCLP-D-NAISS        TO   EL10-D-NAISS
                                       OF   EL10-G-DATA-DET-CLT
      *    Nom marital (PP)
              MOVE TCLP-L-NOM-MRTL     TO   EL10-L-NOM-MRLT
                                       OF   EL10-G-DATA-DET-CLT
      *    Le code pays nationalité (PP)
              MOVE TCLP-C-PAYS-NLITE   TO   EL10-C-PAYS-NLITE
                                       OF   EL10-G-DATA-DET-CLT
      *    Code paye autre nationalité
              MOVE TCLP-C-AUTRE-PAYS-NLITE TO EL10-C-AUTRE-PAYS-NLITE
      *    Le code pays naissance  (PP)
              MOVE TCLP-C-PAYS-NAISS   TO   EL10-C-PAYS-NAISS
      *    le code département naissance (PP)
              MOVE TCLP-C-DEPT-NAISS   TO   EL10-C-DEPT-NAISS
      *    Ville naissance (PP)
              MOVE TCLP-L-VILLE-NAISS  TO   EL10-L-VILL-NAISS
           END-IF
           IF TCLI-C-NTUR-PERS = '02'
      *    Le numéro d'identifiant fiscal EIN du titulaire du compte PM
              MOVE TCLM-C-REF-GIIN     TO   EL10-C-REF-GIIN
                                       OF   EL10-G-DATA-DET-CLT
      *    La raison sociale du titulaire du compte PM
              MOVE TCLM-L-RAISON-SOCIALE  TO   EL10-L-RAIS-SOCIALE
                                          OF   EL10-G-DATA-DET-CLT
      *    La classification PM
              IF TCLM-C-CLSFN-REEL = '01'
                 MOVE 'FATCA104'             TO   EL10-C-TYPE-CLASS
                                             OF   EL10-G-DATA-DET-CLT
              END-IF
              IF TCLM-C-CLSFN-REEL = '05'
                 MOVE 'FATCA102'             TO   EL10-C-TYPE-CLASS
                                             OF   EL10-G-DATA-DET-CLT
              END-IF
           END-IF
           .
      *
MC+   * =============================================================  *
"     *            ALIMENTATION DES INFORMATIONS CLIENT EN CAS HISTOR
"     * =============================================================  *
"      2116-ECRIRE-ID-CLI-ALIM-HIS.
"     *
"     *    ALIMENTATION DES INFORMATIONS
"     *
"     *    Initialisation
"          MOVE SPACES                 TO   EL10-G-DATA-TET-END
"                                           EL10-G-DATA-DET-CLT
"
"     *    Code enregistrement
"          MOVE '20'                   TO   EL10-C-ENR
"     *    Identifiant de lentité déclarante
"          MOVE TCNT-I-IDENT-CPT       TO   EL10-I-IDENT
"     *    N° Ligne /Identifiant
"          IF WS-I-IDENT NOT = TCNT-I-IDENT-CPT
"             MOVE 1                   TO   EL10-N-LIG-IDENT
"          ELSE
"             ADD 1                    TO   EL10-N-LIG-IDENT
"          END-IF
"          MOVE TCNT-I-IDENT-CPT       TO   WS-I-IDENT
"     *    Entité concernée
"          MOVE 'CLI'                  TO   EL10-C-ENTIT
"     *    Identifiant client
"          MOVE TEMP-I-UNIQ-KPI        TO   EL10-I-UNIQ-KPI
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Identifiant client
"          MOVE TEMP-C-NTUR-PERS       TO   EL10-C-NTUR-PERS
"                                      OF   EL10-G-DATA-DET-CLT
"          IF TCLI-C-NTUR-PERS = '01'
"     *    TIN ou NIF du titulaire du compte (PP)
"             MOVE TEMP-C-REF-GIIN     TO   EL10-C-REF-GIIN
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Le nom patronymique du titulaire (PP)
"             MOVE TEMP-L-NOM-NAISS    TO   EL10-L-NOM-NAISS
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Titre civilité (PP)
"             MOVE TEMP-C-TITRE-CVLTE  TO   EL10-C-TITRE-CVLTE
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Calcul prénom du client
"     *       UNSTRING TCLP-L-PRNOM DELIMITED " "
"     *                              INTO EL10-L-PRENOM
"     *                                   EL10-L-PRENOM-2
"     *    Le prénom du titulaire (PP)
"             MOVE TEMP-L-PRNOM        TO   EL10-L-PRENOM
"                                      OF   EL10-G-DATA-DET-CLT
"     *    La date de naissance du titulaire du compte (PP)
"             MOVE TEMP-D-NAISS        TO   EL10-D-NAISS
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Nom marital (PP)
"             MOVE TEMP-L-NOM-MRTL     TO   EL10-L-NOM-MRLT
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Le code pays nationalité (PP)
"             MOVE TEMP-C-PAYS-NLITE   TO   EL10-C-PAYS-NLITE
"                                      OF   EL10-G-DATA-DET-CLT
"     *    Code paye autre nationalité
"             MOVE TEMP-C-AUTRE-PAYS-NLITE TO EL10-C-AUTRE-PAYS-NLITE
"     *    Le code pays naissance  (PP)
"             MOVE TEMP-C-PAYS-NAISS   TO   EL10-C-PAYS-NAISS
"     *    le code département naissance (PP)
"             MOVE TEMP-C-DEPT-NAISS   TO   EL10-C-DEPT-NAISS
"     *    Ville naissance (PP)
"             MOVE TCLP-L-VILLE-NAISS  TO   EL10-L-VILL-NAISS
"          END-IF
"          IF TCLI-C-NTUR-PERS = '02'
"     *    Le numéro d'identifiant fiscal EIN du titulaire du compte PM
"             MOVE TCLM-C-REF-GIIN     TO   EL10-C-REF-GIIN
"                                      OF   EL10-G-DATA-DET-CLT
"     *    La raison sociale du titulaire du compte PM
"             MOVE TEMP-L-RAISON-SOCIALE  TO   EL10-L-RAIS-SOCIALE
"                                         OF   EL10-G-DATA-DET-CLT
"     *    La classification PM
"             IF TCLM-C-CLSFN-REEL = '01'
"                MOVE 'FATCA104'             TO   EL10-C-TYPE-CLASS
"                                            OF   EL10-G-DATA-DET-CLT
"             END-IF
"             IF TCLM-C-CLSFN-REEL = '05'
"                MOVE 'FATCA102'             TO   EL10-C-TYPE-CLASS
"                                            OF   EL10-G-DATA-DET-CLT
"             END-IF
"          END-IF
MC+        .
      *                         *****                               *  *
      *                         *****                               *  *
      * =============================================================  *
      *            ECRIRE UN ENREGISTREMENT MAJ ECV DU CLIENT
      * =============================================================  *
151197 2116-MAJ-CLI.
"
"          MOVE SPACES                TO EL10-G-DATA-MAJ-CLI
"          MOVE 'CLI'                 TO EL10-C-ENTIT
"          MOVE '40'                  TO EL10-C-ENR
"
"          MOVE 1                     TO EL10-N-LIG-IDENT
"
"          MOVE 'MAJ'                 TO EL10-C-OPE                     05720499
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE 'TBCLIFUS'            TO EL10-NOM-TABLE                 05720499
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-A-APPL           TO EL10-A-APPL
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-I-IDENT-CLT      TO EL10-I-IDENT-CLT
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-SYS-ORIGINE    TO EL10-C-SYS-ORIGINE
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-I-UNIQ-KPI       TO EL10-I-UNIQ-KPI
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-NTUR-PERS      TO EL10-C-NTUR-PERS
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-PAYS-FISCL-ETR TO EL10-C-PAYS-FISCL-ETR
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-REF-FISC-ETR   TO EL10-C-REF-FISC-ETR
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-STUT-FISC-ETR  TO EL10-C-STUT-FISC-ETR
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-D-STUT-FISC-ETR  TO EL10-D-STUT-FISC-ETR
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-D-MAJ-STUT       TO EL10-D-MAJ-STUT
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-D-CLSFN-SEUIL    TO EL10-D-CLSFN-SEUIL
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-CLSN-SEUIL     TO EL10-C-CLSN-SEUIL
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
MCHA+      MOVE TCLI-C-STUT-DECL      TO EL10-C-STUT-DECL
MCHA+                                    OF EL10-G-DATA-MAJ-CLI
"          MOVE '05'                  TO EL10-C-ECV-RPCLI
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-TYPE-CLT       TO EL10-C-TYPE-CLT
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-NTURE-JUR      TO EL10-C-NTURE-JUR
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-ECV-RELVE      TO EL10-C-ECV-RELVE
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-ACTION         TO EL10-C-ACTION
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-C-MOTIF          TO EL10-C-MOTIF
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-N-AGENT-CRE      TO EL10-N-AGENT-CRE
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-N-AGENT-MAJ      TO EL10-N-AGENT-MAJ
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-D-CRE            TO EL10-D-CRE
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
"          MOVE TCLI-D-MAJ            TO EL10-D-MAJ
"                                        OF EL10-G-DATA-MAJ-CLI         05720499
151197     .
      *                         *****                               *  *
      * =============================================================  *
      *            ECRIRE UN ENREGISTREMENT MAJ ECV DU RECALCITRANT
      * =============================================================  *
151345 3111-MAJ-CLR.
"
"          MOVE SPACES                TO EL10-G-DATA-MAJ-CLR
"          MOVE '40'                  TO EL10-C-ENR
"          MOVE 'CLR'                 TO EL10-C-ENTIT
"
"          MOVE 1                     TO EL10-N-LIG-IDENT
"
"          MOVE 'MAJ'                 TO EL10-C-OPE                     05720499
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE 'TBCLRFUS'            TO EL10-NOM-TABLE                 05720499
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-A-APPL           TO EL10-A-APPL
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-I-IDENT-TYP-REC  TO EL10-I-IDENT-TYP-REC
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-I-IDENT-END      TO EL10-I-IDENT-END
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-C-TYPE-RECAL     TO EL10-C-TYPE-RECAL
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-Q-NBR-DEC        TO EL10-Q-NBR-DEC
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-C-SIGNE-MNT      TO EL10-C-SIGNE-MNT
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-M-MNT            TO EL10-M-MNT
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-C-DEV            TO EL10-C-DEV
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-Q-NBR-CPT        TO EL10-Q-NBR-CPT
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE '05'                  TO EL10-C-ECV-TYP-REC
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-C-ACTION-USER    TO EL10-C-ACTION-USER
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          MOVE TCLR-C-MOTIF          TO EL10-C-MOTIF
"                                        OF EL10-G-DATA-MAJ-CLR         05720499
"          .
151345*                         *****                               *  *
      *                         *****                               *  *
      *                         *****                               *  *
      * =============================================================  *
      *            RéCUPéRER LES ADRESSES DU CLIENT
      * =============================================================  *
       2116-RECUP-ADR.
      *
      *    RéCUPéRER LES ADRESSES DU CLIENT
      *
      *    OUVERTURE CURSEUR DE RECHERCHE
           PERFORM 2116-RECUP-ADR-OP-CUR

      *    Boucle ...
           PERFORM UNTIL IND-TRT-ADR = 'O'
      *    Alimentation de la ligne adresse
150978        IF TADR-C-TYPE-ADR = 'AF'
                 PERFORM 2116-ECRIRE-ADR-ALIM
      *    Ecriture adresse
                 PERFORM 1360-ECRIRE-OUT
150978        END-IF
      *    Lecture adresse
              PERFORM 2116-LECTURE-CUR-ADR
      *    Fin boucle
           END-PERFORM
           .
      *                         *****                               *  *
      *                      ***                 * *         * *   ** *
      *===================================================*
      * OUVERTURE CURSEUR TBADRFUS
      *===================================================*
       2116-RECUP-ADR-OP-CUR.
      *
      * Ouverture curseur
      *
      * Initialisation
           MOVE 'N'                     TO IND-TRT-ADR
      * Paramètre d'appel
           MOVE 'OP'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE TCLI-I-IDENT-CLT        TO TADR-I-IDENT

      * APPEL MFUSTADR
           PERFORM SQ-ACCES-TADR
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
150978           CONTINUE
150978* ==>   SI FIN DE CURSEUR
150978        WHEN NON-TROUVE
150978           MOVE 'O'               TO IND-TRT-ADR
      * ==>   SI ERREUR APPEL MODULE MFUSTADR OU KO
              WHEN OTHER
                 MOVE 1009                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *                         *****                               *  *
      * =============================================================  *
      *            ALIMENTATION DES INFORMATIONS ADRESSE
      * =============================================================  *
       2116-ECRIRE-ADR-ALIM.
      *
      *    ALIMENTATION DES INFORMATIONS
      *
      *    Initialisation
           MOVE SPACE                  TO   EL10-G-DATA-TET-END

      *    Code enregistrement
           MOVE '20'                   TO   EL10-C-ENR
      *    Identifiant de lentité déclarante
           MOVE TCNT-I-IDENT-CPT       TO   EL10-I-IDENT
      *    N° Ligne /Identifiant
           IF WS-I-IDENT NOT = TCNT-I-IDENT-CPT
              MOVE 1                   TO   EL10-N-LIG-IDENT
           ELSE
              ADD 1                    TO   EL10-N-LIG-IDENT
           END-IF
           MOVE TCNT-I-IDENT-CPT       TO   WS-I-IDENT
      *    Entité concernée
           MOVE 'ADR'                  TO   EL10-C-ENTIT
      *    Type adresse
           MOVE TADR-C-TYPE-ADR        TO   EL10-C-TYPE-ADR
                                       OF   EL10-G-DATA-DET-ADR
      *    Nom commune adresse
MC+        IF  W-L-COMM-ADR = LOW-VALUE
              MOVE TADR-L-COMM-ADR        TO   EL10-L-COMM-ADR
                                          OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"             MOVE W-L-COMM-ADR           TO   EL10-L-COMM-ADR
"                                         OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Code postale adresse
MC+        IF  W-C-CPOST = LOW-VALUE
              MOVE TADR-C-CPOST           TO   EL10-C-CPOST
                                       OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"             MOVE W-C-CPOST           TO   EL10-C-CPOST
"                                      OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Code pays ISO de l'adresse
MC+        IF  W-C-PAYS-ADR = LOW-VALUE
               MOVE TADR-C-PAYS-ADR        TO   EL10-C-PAYS-ADR
                                           OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"              MOVE W-C-PAYS-ADR        TO   EL10-C-PAYS-ADR
"                                       OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Intitulé courrier ligne 1 adresse
MC+        IF  W-L-INTIT-COURR-1 =  LOW-VALUE
               MOVE TADR-L-INTIT-COURR-1   TO   EL10-L-INTIT-COURR-1
                                            OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"              MOVE W-L-INTIT-COURR-1   TO   EL10-L-INTIT-COURR-1
"                                           OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Intitulé courrier ligne 2 adresse
MC+        IF  W-L-INTIT-COURR-2 = LOW-VALUE
               MOVE TADR-L-INTIT-COURR-2   TO   EL10-L-INTIT-COURR-2
                                           OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"              MOVE W-L-INTIT-COURR-2   TO   EL10-L-INTIT-COURR-2
"                                           OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Adresse 1
MC+        IF  W-L-ADR-LIGNE-1   = LOW-VALUE
              MOVE TADR-L-ADR-LIGNE-1     TO   EL10-L-ADR-LIGNE-1
                                       OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"             MOVE W-L-ADR-LIGNE-1     TO   EL10-L-ADR-LIGNE-1
"                                      OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Adresse 2
MC+        IF  W-L-ADR-LIGNE-2   =  LOW-VALUE
               MOVE TADR-L-ADR-LIGNE-2     TO   EL10-L-ADR-LIGNE-2
                                           OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"              MOVE W-L-ADR-LIGNE-2        TO   EL10-L-ADR-LIGNE-2
"                                          OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Adresse 3
MC+        IF  W-L-ADR-LIGNE-3   =  LOW-VALUE
               MOVE TADR-L-ADR-LIGNE-3     TO   EL10-L-ADR-LIGNE-3
                                           OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"              MOVE W-L-ADR-LIGNE-3        TO   EL10-L-ADR-LIGNE-3
"                                          OF   EL10-G-DATA-DET-ADR
MC+        END-IF
      *    Libellé pays de l'adresse
MC+        IF  W-L-PAYS-ADR  =  LOW-VALUE
               MOVE TADR-L-PAYS-ADR        TO   EL10-L-PAYS-ADR
                                           OF   EL10-G-DATA-DET-ADR
MC+        ELSE
"              MOVE W-L-PAYS-ADR           TO   EL10-L-PAYS-ADR
"                                          OF   EL10-G-DATA-DET-ADR
MC+        END-IF
           .
      *                         *****                               *  *
      *                      ***                 * *         * *   ** *
      *===================================================*
      * LECTURE CURSEUR TBADRFUS
      *===================================================*
       2116-LECTURE-CUR-ADR.
      *
      * LECTURE CURSEUR
      *
      * Paramètre d'appel
           MOVE 'FE'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE TCLI-I-IDENT-CLT        TO TADR-I-IDENT

      * APPEL MFUSTADR
           PERFORM SQ-ACCES-TADR
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
150978           CONTINUE
150978* ==>   SI FIN DE CURSEUR
150978        WHEN NON-TROUVE
150978           MOVE 'O'               TO IND-TRT-ADR
      * ==>   SI ERREUR APPEL MODULE MFUSTADR OU KO
              WHEN OTHER
                 MOVE 1017                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *                      ***                 * *         * *   ** *
      * =============================================================  *
      *            RéCUPéRER LES LPP
      * =============================================================  *
       2117-RECUP-LPP.
      *
      *    RéCUPéRER LES LIENS P/P
      *
           MOVE 'N'                     TO IND-TRT-LPP
      *    OUVERTURE CURSEUR DE RECHERCHE
           PERFORM 2117-RECUP-LPP-OP-CUR

      *    Boucle ...
           PERFORM UNTIL IND-TRT-LPP = 'O'
      *    Alimentation de la ligne adresse
MCHA+         IF TLPP-C-SENS-FAM NOT = 'A'   AND
MCHA+            TCLI-C-STUT-FISC-ETR = '02'
                 PERFORM 2117-SELECT-CLP-INF
      *    Ecriture adresse
                 PERFORM 2117-SELECT-ADR-INF
      *    Alimentation de la zone en sortie pour écriture
                 PERFORM 2117-RECUP-LPP-ALIM
      *    Ecriture Liens personne/personne
                 PERFORM 1360-ECRIRE-OUT
              END-IF
      *    Lecture suivante de curseur
              PERFORM 2117-RECUP-LPP-FE-CUR
      *    Fin boucle
           END-PERFORM
           .
      *                         *****                               *  *
      *                      ***                 * *         * *   ** *
      *===================================================*
      * OUVERTURE CURSEUR TBLPPFUS
      *===================================================*
       2117-RECUP-LPP-OP-CUR.
      *
      * Ouverture curseur
      *
      * Initialisation
           MOVE 'N'                     TO IND-TRT-LPP
      * Paramètre d'appel
           MOVE 'OP'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
           MOVE TCLI-I-IDENT-CLT        TO TLPP-I-IDENT-CLT-PERE

      * APPEL MFUSTLPP
           PERFORM SQ-ACCES-TLPP
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
150978           CONTINUE
150978        WHEN NON-TROUVE
150978           MOVE 'O'               TO IND-TRT-LPP
      * ==>   SI ERREUR APPEL MODULE MFUSTLPP OU KO
              WHEN OTHER
                 MOVE 1010                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *===================================================*
      * RECHERCHE INFORMATIONS LORS DE LA PRéSENCE
      *===================================================*
       2117-SELECT-CLP-INF.
      *
      * RéCUPéRER LES INFORMATIONS DU CLIENT PHYSIQUE
      *
      * Paramètre d'appel
           MOVE 'SE'                    TO ACCE-TYPE-REQUETE
           MOVE '01-SEL'                TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
150978     MOVE TLPP-I-IDENT-CLT        TO TCLP-I-IDENT-CLT

      * APPEL MFUSTLPP
           PERFORM SQ-ACCES-TCLP
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI ERREUR APPEL MODULE MFUSTCLP OU KO
              WHEN OTHER
                 MOVE 1012                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *                      ***                 * *         * *   ** *
      *===================================================*
      * RECHERCHE INFORMATIONS LORS DE LA PRéSENCE
      *===================================================*
       2117-SELECT-ADR-INF.
      *
      * RéCUPéRER LES INFORMATIONS DU CLIENT PHYSIQUE
      *
      * Paramètre d'appel
           MOVE 'SE'                    TO ACCE-TYPE-REQUETE
           MOVE '01-SEL'                TO ACCE-NOM-FONCTION
      * Alimentation de clause Where
150978     MOVE TLPP-I-IDENT-CLT        TO TADR-I-IDENT
           MOVE 'AF'                    TO TADR-C-TYPE-ADR

      * APPEL MFUSTADR
           PERFORM SQ-ACCES-TADR
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
                 CONTINUE
      * ==>   SI ERREUR APPEL MODULE MFUSTADR OU KO
              WHEN OTHER
                 MOVE 1013                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *                      ***                 * *         * *   ** *
      *===================================================*
      * LECTURE CURSEUR TBLPPFUS
      *===================================================*
       2117-RECUP-LPP-FE-CUR.
      *
      * LECTURE CURSEUR
      *
      * Paramètre d'appel
           MOVE 'FE'                    TO ACCE-TYPE-REQUETE
           MOVE '01'                    TO ACCE-NOM-FONCTION

      * APPEL MFUSTLPP
           PERFORM SQ-ACCES-TLPP
      * CONTROLE CODE RETOUR
           EVALUATE TRUE
      * ==>   SI OK
              WHEN TROUVE
150978           CONTINUE
150978        WHEN NON-TROUVE
150978           MOVE 'O'               TO IND-TRT-LPP
      * ==>   SI ERREUR APPEL MODULE MFUSTLPP OU KO
              WHEN OTHER
                 MOVE 1011                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
           END-EVALUATE
           .
      *                      ***                 * *         * *   ** *
      *                         *****                               *  *
      * =============================================================  *
      *            ALIMENTATION DES INFORMATIONS LPP
      * =============================================================  *
       2117-RECUP-LPP-ALIM.
      *
      *    Ecritrure ligne identifiant Bénéficiaire
      *
      *    Initialisation
           MOVE SPACE                  TO   EL10-G-DATA-TET-END

      *    Code enregistrement
           MOVE '20'                   TO   EL10-C-ENR
      *    Identifiant de lentité déclarante
           MOVE TCNT-I-IDENT-CPT       TO   EL10-I-IDENT
      *    N° Ligne /Identifiant
           IF WS-I-IDENT NOT = TCNT-I-IDENT-CPT
              MOVE 1                   TO   EL10-N-LIG-IDENT
           ELSE
              ADD 1                    TO   EL10-N-LIG-IDENT
           END-IF
           MOVE TCNT-I-IDENT-CPT       TO   WS-I-IDENT
      *    Entité concernée
           MOVE 'BNF'                  TO   EL10-C-ENTIT
      *    Unique KPI
           MOVE TCLI-I-UNIQ-KPI        TO   EL10-I-UNIQ-KPI
                                       OF   EL10-G-DATA-DET-BNF
      *    L'identifiant TIN ou NIF du bénéficiaire (co-titulaire)
           MOVE TCLP-C-REF-GIIN        TO   EL10-C-REF-GIIN
                                       OF   EL10-G-DATA-DET-BNF
MCHA+ *    Le titre civilité du bénéficiaire (co-titulaire)
MCHA+      MOVE TCLP-C-TITRE-CVLTE     TO   EL10-C-TITRE-CVLTE-F
                                       OF   EL10-G-DATA-DET-BNF
      *    Le rem patronymique du bénéficiaire (co-titulaire)
           MOVE TCLP-L-NOM-NAISS       TO   EL10-L-NOM-NAISS
                                       OF   EL10-G-DATA-DET-BNF
      *    Le prénom patronymique du bénéficiaire (co-titulaire)
           MOVE TCLP-L-PRNOM           TO   EL10-L-PRNOM
                                       OF   EL10-G-DATA-DET-BNF
      *    La date de naissance du bénéficiaire (co-titulaire)
           MOVE TCLP-D-NAISS           TO   EL10-D-NAISS
                                       OF   EL10-G-DATA-DET-BNF
      *    Code pays ISO de l'adresse
           MOVE TADR-C-PAYS-ADR        TO   EL10-C-PAYS-ADR
                                       OF   EL10-G-DATA-DET-BNF
      *    La commune de l'adresse du bénéficiaire (co-titulaire
           MOVE TADR-L-COMM-ADR        TO   EL10-L-COMM-ADR
                                       OF   EL10-G-DATA-DET-BNF
           .
      *                         *****                               *  *
150978*========================================================*
"     * RESTITUTION ET éCRITURE DES INFORMATIONS DU RECALCITANT
"     *========================================================*
"      3000-RESTIT-CLR.
"     *
"     * RESTITUTION DE DONNEE RECALCITRANT
"     *
"          PERFORM 3100-LECTURE-CLR
"
"     *    ALIMENTATION DE CLIENTS RECALCITRANTS
"          PERFORM 3110-ALIM-INF-CLR
"
"     *    ECRITURE BLOC RECALCITRANT
"          PERFORM 1360-ECRIRE-OUT
"
"     *    ALIMENTATION DE REFERENCE CLIENTS RECALCITRANTS
"          PERFORM 3120-ALIM-INF-RBR
"
"     *    ECRITURE COMPTE
"          PERFORM 1360-ECRIRE-OUT
MCHA+1*    AJOUT LA MAJ DE ECV DE RECALCITRANT
MCHA+1*    ALIMENTATION DE CLIENTS RECALCITRANTS
"          PERFORM 3111-MAJ-CLR
"
"     *    ECRITURE CLIENT RECALICTRANT
"          PERFORM 1360-ECRIRE-OUT
"          .
"
"     * LECTURE DE LA TABLE RéCALCITRANT
"     * =============================================================  *
"      3100-LECTURE-CLR.
"     *
"     *    LECTURE DU CURSEUR DE LA TABLE DES COMPTES EN MODE REPRISE
"     *
"          MOVE AI11-I-IDENT-TYPE-REC   TO TCLR-I-IDENT-TYP-REC
"     *
"          MOVE 'SE'                    TO ACCE-TYPE-REQUETE
"          MOVE '03'                    TO ACCE-NOM-FONCTION
"     D    DISPLAY 'TCLR-A-APPL          :' TCLR-A-APPL
"     D    DISPLAY 'TCLR-I-IDENT-END     :' TCLR-I-IDENT-END
"     D    DISPLAY 'TCLR-I-IDENT-TYP-REC :' TCLR-I-IDENT-TYP-REC
"     D    DISPLAY 'ACCE-TYPE-REQUETE    :' ACCE-TYPE-REQUETE
"     D    DISPLAY 'ACCE-NOM-FONCTION    :' ACCE-NOM-FONCTION
"
"     * APPEL MFUSTCLR
"          PERFORM SQ-ACCES-TCLR
"     D    DISPLAY 'ACCE-SQLCODE    :' ACCE-SQLCODE
"     D    DISPLAY 'ACCE-code-retour:' ACCE-code-retour
"     * CONTROLE CODE RETOUR
"          EVALUATE TRUE
"     * ==>   SI OK
"             WHEN TROUVE
MCHA? *          ADD  1  TO WS-CPT-CNT-LUS
150978           CONTINUE
"     * ==>   SI KO
"             WHEN NON-TROUVE
"                MOVE 'O'               TO IND-FIN-TRT
"             WHEN OTHER
"     * ==>   SI ERREUR APPEL MODULE MFUSTCNT
"                MOVE 1015                      TO WS-CODE-ABEND
"                PERFORM 4200-ABEND-ERR
"          END-EVALUATE
"          .
"     *
"     * =============================================================  *
"     *  ALIMENTATION DES RECALCITRANT
"     * =============================================================  *
"      3110-ALIM-INF-CLR.
"     *
"     *    CALCUL REFERENCE BLOC RECALCITRANT
"     *    MOVE "FATCA1"               TO   WS-TYPE-FATCA
"     *                                OF   WS-I-REF-BLOC-CLR
"          IF WS-TYPE-FATCA-TST OF WS-BLOC-CLR-TST = 'FATCA11' OR
"             'FATCA12' OR 'FATCA13' OR 'FATCA14'
"             MOVE W-DATE-TIMESTAMP       TO   WS-DATE-TIMESTAMP-TST
"                                         OF   WS-BLOC-CLR-TST
"             MOVE LST-ENT-DECL           TO   WS-I-REF-GIIN-TST
"                                         OF   WS-BLOC-CLR-TST
"             MOVE TCLR-C-TYPE-RECAL      TO   WS-C-TYPE-RECAL-TST
"                                         OF   WS-BLOC-CLR-TST
"             MOVE TCLR-C-DEV             TO   WS-C-DEV-TST
"                                         OF   WS-BLOC-CLR-TST
"             MOVE '-'                    TO   WS-FILLER-01-TST
"                                         OF   WS-BLOC-CLR-TST
"                                              WS-FILLER-02-TST
"                                         OF   WS-BLOC-CLR-TST
"                                              WS-FILLER-03-TST
"                                         OF   WS-BLOC-CLR-TST
"                                              WS-FILLER-04-TST
"                                         OF   WS-BLOC-CLR-TST
"          ELSE
"             MOVE W-DATE-TIMESTAMP       TO   WS-DATE-TIMESTAMP
"                                         OF   WS-BLOC-CLR
"             MOVE LST-ENT-DECL           TO   WS-I-REF-GIIN
"                                         OF   WS-BLOC-CLR
"             MOVE TCLR-C-TYPE-RECAL      TO   WS-C-TYPE-RECAL
"                                         OF   WS-BLOC-CLR
"             MOVE TCLR-C-DEV             TO   WS-C-DEV
"                                         OF   WS-BLOC-CLR
"             MOVE '-'                    TO   WS-FILLER-01
"                                         OF   WS-BLOC-CLR
"                                              WS-FILLER-02
"                                         OF   WS-BLOC-CLR
"                                              WS-FILLER-03
"                                         OF   WS-BLOC-CLR
"                                              WS-FILLER-04
"                                         OF   WS-BLOC-CLR
"          END-IF
"     *
"     *    ALIMENTATION DES INFORMATIONS RECALCITRANT
"     *
"     *    Initialisation
"          MOVE SPACE                  TO   EL10-G-DATA-INF-CLR
"
"     *    Code enregistrement
"          MOVE '30'                   TO   EL10-C-ENR
"     *    Identifiant de lentité déclarante
150978*    MOVE TCLR-I-IDENT-END       TO   EL10-I-IDENT
150978     MOVE TCLR-I-IDENT-TYP-REC   TO   EL10-I-IDENT
"     *    N° Ligne /Identifiant
"          MOVE 1                      TO   EL10-N-LIG-IDENT
"     *
"          MOVE 'CLR'                  TO   EL10-C-ENTIT
"     *
"          MOVE WS-I-REF-BLOC-CLR      TO   EL10-C-REF-BLOC-RECAL
"                                      OF   EL10-G-DATA-INF-CLR
MCHA+-     MOVE AI11-C-REF-BLOC-RECAL  TO   EL10-C-REF-BLOC-RLC-INIT
"                                      OF   EL10-G-DATA-INF-CLR
"     *    Type de récalcitrant
"          MOVE TCLR-C-TYPE-RECAL      TO   EL10-C-TYPE-RECAL
"                                      OF   EL10-G-DATA-INF-CLR
"     *    ANNEE FISCALE
"          MOVE TCLR-A-APPL            TO   EL10-A-APPL
"                                      OF   EL10-G-DATA-INF-CLR
"     *    Le code devise
"          MOVE TCLR-C-DEV             TO   EL10-C-DEV
"                                      OF   EL10-G-DATA-INF-CLR
"     *    Nombre des compte
"          MOVE TCLR-Q-NBR-CPT         TO   EL10-Q-NBR-CPT
"                                      OF   EL10-G-DATA-INF-CLR
"     *    Montant
"          MOVE TCLR-M-MNT             TO   EL10-M-MNT
"                                      OF   EL10-G-DATA-INF-CLR
"     *    Nombre de décimales
"          MOVE TCLR-Q-NBR-DEC         TO   EL10-Q-NBR-DEC
"                                      OF   EL10-G-DATA-INF-CLR
"          .
"
"     *===================================================*
"     * ALIMENTATION DES DONNEES RBR
"     *===================================================*
"      3120-ALIM-INF-RBR.
"     *
"     *  ==> ALIMENTATION DES DONNEES DES BLOCS RéCALCITRANTS
"
151197     MOVE SPACES                  TO EL10-G-DATA-DET-RBR
"
"          MOVE '40'                    TO EL10-C-ENR
"
"          MOVE TCLR-I-IDENT-TYP-REC    TO EL10-I-IDENT
"
"          MOVE 1                       TO EL10-N-LIG-IDENT
"
"          MOVE 'RBR'                   TO EL10-C-ENTIT
"
"     *  ==> CODE OPERATION
"          MOVE 'CRE'                   TO EL10-C-OPE
"                                       OF EL10-G-DATA-DET-RBR
"     *  ==> LE NOM DE LA TABLE A METTRE A JOUR
"          MOVE 'TBRBRFUS'              TO EL10-NOM-TABLE
"                                       OF EL10-G-DATA-DET-RBR
"     *  ==> IDENTIFIANT INTERNE DU FICHIER DE REPORTING
"          MOVE WS-I-IDENT-REF-REP      TO EL10-I-IDENT-REF-REP
"                                       OF EL10-G-DATA-DET-RBR
"     *  ==> IDENTIFIANT INTERNE DU RéCALCITRANT
"          MOVE AI11-I-IDENT-TYPE-REC   TO EL10-I-IDENT-TYP-REC
"                                       OF EL10-G-DATA-DET-RBR
"     *  ==> RéFéRENCE DU BLOC RéCALCITRANT
"          MOVE WS-I-REF-BLOC-CLR       TO EL10-C-REF-BLOC-RECAL
"                                       OF EL10-G-DATA-DET-RBR
"          .
"     *                         *****                               *  *
"     *==================*
"     * FIN DU PROGRAMME *
"     *==================*
"      4000-FIN-PGM.
"     *
"     * TRAITEMENT ENQUEUE DU FICHIER
"          PERFORM 4100-RESTIT-ENQUEUE
"          .
"     * TRAITEMENT FIN PROGRAMME
"          PERFORM FIN-PROGRAMME
"          .
"
"     *========================================================*
"     * RESTITUTION ET éCRITURE DES INFORMATIONS DU ENQUEUE
"     *========================================================*
"      4100-RESTIT-ENQUEUE.
"     *
"     * ALIMENTATION DES DONNEES FID
"          PERFORM 4110-ALIM-INF-FID
"
"     *    ECRITURE BLOC FID
"          PERFORM 1360-ECRIRE-OUT
"
"     *    ALIMENTATION DE DONNEES ENQUEUE
"          PERFORM 4120-ALIM-INF-ENQ
"
"     *    ECRITURE COMPTE
"          PERFORM 1360-ECRIRE-OUT
"
"     *      Initialisation des compteurs et montants
"          MOVE ZERO                    TO WS-Q-PERS-DECL
"                                          WS-Q-CONTRAT-DECL
"                                          WS-Q-RECAL-DECL
"                                          WS-M-TOT-SOL-DECL
"                                          WS-M-TOT-INT-DECL
"                                          WS-M-TOT-CES-DECL
"                                          WS-M-TOT-M-DECL
"          .
"
"     *===================================================*
"     * ALIMENTATION DES DONNEES FID
"     *===================================================*
"      4110-ALIM-INF-FID.
"     *
"     *    Alimentation de la référence du fichier
"     *    MOVE "FATCA1"               TO   WS-TYPE-FATCA
"     *                                OF   WS-I-REF-FIC
"     *  ==> ALIMENTATION DES DONNEES DES BLOCS RéCALCITRANTS
"
151197     MOVE SPACES                  TO EL10-G-DATA-DET-FID
151197                                     EL10-G-IDENT-ENREG
"
"          MOVE '40'                    TO EL10-C-ENR
"
151197*    Identifiant de lentité déclarante
151197     MOVE WS-I-IDENT-END-SV       TO EL10-I-IDENT
"
"          MOVE 1                       TO EL10-N-LIG-IDENT
"
"          MOVE 'FID'                   TO EL10-C-ENTIT
"
"     *  ==> CODE OPERATION
"          MOVE 'MAJ'                   TO EL10-C-OPE
"                                       OF EL10-G-DATA-DET-FID
"
"     *  ==> LE NOM DE LA TABLE A METTRE A JOUR
"          MOVE 'TBFIDFUS'              TO EL10-NOM-TABLE
"                                       OF EL10-G-DATA-DET-FID
"     *      Entité déclarante
150978     MOVE  WS-I-IDENT-END-SV      TO EL10-I-IDENT-END
"                                       OF EL10-G-DATA-DET-FID
"     *      Année fiscale
"          MOVE  WS-A-APPL              TO EL10-A-APPL
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Identifiant interne du fichier de reporting
"          MOVE  WS-I-IDENT-REF-REP     TO EL10-I-IDENT-REF-REP
"                                       OF EL10-G-DATA-DET-FID
MCHA+-     IF SYSIN-TYPE-FATCA = 'FATCA1 ' OR 'FATCA11'
"     *      Référence initial du fichier déclaratif
"             MOVE  WS-I-REF-FIC           TO EL10-I-REF-FIC-INIT
"                                         OF EL10-G-DATA-DET-FID

"     *      Date du fichier déclaratif initial
"             MOVE W-DATE-TIMESTAMP        TO EL10-D-REF-FIC-INIT
"                                         OF EL10-G-DATA-DET-FID
"          ELSE
"     *      Référence initial du fichier déclaratif
"             MOVE  WS-I-REF-FIC-INIT      TO EL10-I-REF-FIC-INIT
"                                         OF EL10-G-DATA-DET-FID
"
"     *      Date du fichier déclaratif initial
"             MOVE  WS-D-REF-FIC-INIT      TO EL10-D-REF-FIC-INIT
"                                         OF EL10-G-DATA-DET-FID
MCHA+-     END-IF
"     *      Référence du fichier déclaratif
"          MOVE  WS-I-REF-FIC           TO EL10-I-REF-FIC
"                                       OF EL10-G-DATA-DET-FID
"
"     *      N° Agent
MCHA?      MOVE  99999                  TO EL10-N-AGENT
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Nom agent émetteur
"          MOVE  'XXXXXXX'              TO EL10-NOM-EMET
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Référence attribuée par l'administration fiscale
MCHA?      MOVE  'XXXXXXXX'             TO EL10-I-REF-ADM
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Clé de hachage
MCHA?      MOVE  'XXXXXXX'              TO EL10-C-CLE-HACH
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Date accusée réception
MCHA?      MOVE  W-D-SYSTEME-ISO        TO EL10-D-ACCUSE
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Nombre de personnes déclarées
"          MOVE  WS-Q-PERS-DECL         TO EL10-Q-PERS-DECL
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Nombre de contrats déclarés
"          MOVE  WS-Q-CONTRAT-DECL      TO EL10-Q-CONTRAT-DECL
"                                       OF EL10-G-DATA-DET-FID
"     *    Type de déclaration
"
MCHA!!     IF WS-C-MOD-REPORT = '1'
"            MOVE  '01'                   TO EL10-C-TYPE-DECL
"                                         OF EL10-G-DATA-DET-FID
"            IF SYSIN-TYPE-FATCA = 'FATCA1 '
"               MOVE  '01'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
"            IF SYSIN-TYPE-FATCA = 'FATCA2 '
"               MOVE  '03'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
"            IF SYSIN-TYPE-FATCA = 'FATCA3 '
"               MOVE  '04'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
"            IF SYSIN-TYPE-FATCA = 'FATCA4 '
"               MOVE  '05'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
MCHA!!       IF SYSIN-TYPE-FATCA13 = 'FATCA31A ' OR 'FATCA31A1'
"               MOVE  '02'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
MCHA+      ELSE
"            MOVE  '05'                   TO EL10-C-TYPE-DECL
"                                       OF EL10-G-DATA-DET-FID
"            IF SYSIN-TYPE-FATCA = 'FATCA11'
"               MOVE  '06'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
"            IF SYSIN-TYPE-FATCA = 'FATCA12'
"               MOVE  '08'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
"            IF SYSIN-TYPE-FATCA = 'FATCA13'
"               MOVE  '09'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
"            IF SYSIN-TYPE-FATCA = 'FATCA14'
"               MOVE  '10'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
MCHA!!       IF SYSIN-TYPE-FATCA13 = 'FATCA31I ' OR 'FATCA31I1'
"               MOVE  '05'                   TO EL10-C-TYPE-DECL
"                                            OF EL10-G-DATA-DET-FID
"            END-IF
MCHA+      END-IF
"
"     *      Montant total de solde déclaré
"          MOVE  WS-M-TOT-SOL-DECL      TO EL10-M-TOT-SOL-DECL
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Montant total d'intérêt déclaré
"          MOVE  WS-M-TOT-INT-DECL      TO EL10-M-TOT-INT-DECL
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Montant total de solde déclaré
"          MOVE  WS-M-TOT-CES-DECL      TO EL10-M-TOT-CES-DECL
"                                       OF EL10-G-DATA-DET-FID
"
"     *      Montant total autre montant déclaré
"          MOVE  WS-M-TOT-M-DECL        TO EL10-M-TOT-M-DECL
"                                       OF EL10-G-DATA-DET-FID
"
"          .
"     *===================================================*
"     * ALIMENTATION DES DONNEES ENQUEUE
"     *===================================================*
"      4120-ALIM-INF-ENQ.
"     *
"     *  ==> TYPE ENREGISTREMENT
151197     MOVE SPACES                  TO EL10-G-DATA-ENQ-END
151197                                     EL10-G-IDENT-ENREG
"
"          MOVE '90'                    TO EL10-C-ENR
"
151197*    Identifiant de lentité déclarante
151197     MOVE WS-I-IDENT-END-SV       TO EL10-I-IDENT
"
"          MOVE 1                       TO EL10-N-LIG-IDENT
"
"          MOVE 'END'                   TO EL10-C-ENTIT
"
"     *    COMPTEUR DE NOMBRE DE COMPTES TRAITéS                        00027000
"          MOVE WS-Q-CONTRAT-DECL       TO EL10-N-CPT
"                                       OF EL10-G-DATA-ENQ-END
"     *    COMPTEUR DE NOMBRE DE CLIENTS TRAITéS                        00027000
"          MOVE WS-Q-PERS-DECL          TO EL10-N-CLT
"                                       OF EL10-G-DATA-ENQ-END
"     *    COMPTEUR DE NOMBRE DE RECALCITRANTS TRAITES                  00027000
MCHA?      MOVE WS-Q-RECAL-DECL         TO EL10-N-CLR
"                                       OF EL10-G-DATA-ENQ-END
150978     .
      *===============================================================*
      *                    GESTION DES ABENDS                         *
      *===============================================================*
       4200-ABEND-ERR.
      *--------------*
      *
      *
      *    CE PARAGRAPHE GERE L'AFFICHAGE DU CODE ABEND ET LA NATURE
      *    DE L'ANOMALIE

      *    LE CODE ABEND EST PASSE A CE PARAGRAPHE PAR WS-CODE-ABEND
      *
      *
           MOVE  WS-CODE-ABEND           TO   INAB-C-ABEND

      *    INITIALISATION DES LIGNES DE DESCRIPTIONS D'ABEND

           MOVE ZERO                     TO   INAB-Q-LIST-DISP
           MOVE SPACE                    TO   INAB-L-DISP (1)
                                              INAB-L-DISP (2)
                                              INAB-L-DISP (3)
                                              INAB-L-DISP (4)
                                              INAB-L-DISP (5)
                                              INAB-L-DISP (6)
                                              INAB-L-DISP (7)
                                              INAB-L-DISP (8)
                                              INAB-L-DISP (9)
                                              INAB-L-DISP (10)
                                              INAB-L-DISP (11)
                                              INAB-L-DISP (12)
                                              INAB-L-DISP (13)
                                              INAB-L-DISP (14)
                                              INAB-L-DISP (15)
                                              INAB-L-DISP (16)
                                              INAB-L-DISP (17)
      *
      *    ALIMENTATION DU MESSAGE D'ERREUR SELON LE CODE ABEND
      *
           EVALUATE    INAB-C-ABEND
      *
      *       1000 : PROBLEME APPEL MODULE MGDATR03 DEBUT TRAITEMENT
      *
              WHEN       1000
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB RECUPERATION DATE DU JOUR VIA MGDATR03'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
                 MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1001 : PROBLEME APPEL MODULE MGDATR03 FIN TRAITMENT
      *
              WHEN       1001
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB RECUPERATION DATE DU JOUR VIA MGDATR03'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
                 MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1002 : PROBLEME APPEL MODULE MFUSTCNT POUR OP CURSEUR
      *
              WHEN       1002
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTCNT POUR OP CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1003 : PROBLEME APPEL MODULE MFUSTRUB POUR OP CURSEUR
      *
              WHEN       1003
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTRUB POUR OP CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1004 : PROBLEME APPEL MODULE MFUSTRUB POUR FE CURSEUR
      *
              WHEN       1004
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTRUB POUR FE CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1005 : PROBLEME APPEL MODULE MFUSTLCC POUR FE CURSEUR
      *
              WHEN       1005
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTLCC POUR FE CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1006 : PROBLEME APPEL MODULE MFUSTCLI POUR SELECTION
      *
              WHEN       1006
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTCLI POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1007 : PROBLEME APPEL MODULE MFUSTCLP POUR SELECTION
      *
              WHEN       1007
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTCLP POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1008 : PROBLEME APPEL MODULE MFUSTCLM POUR SELECTION
      *
              WHEN       1008
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTCLM POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1009 : PROBLEME APPEL MODULE MFUSTADR POUR FE CURSEUR
      *
              WHEN       1009
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTADR POUR OP CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1010 : PROBLEME APPEL MODULE MFUSTLPP POUR OP CURSEUR
      *
              WHEN       1010
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTLPP POUR OP CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1011 : PROBLEME APPEL MODULE MFUSTLPP POUR FE CURSEUR
      *
              WHEN       1011
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTLPP POUR FE CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1012 : PROBLEME APPEL MODULE MFUSTLPP POUR FE CURSEUR
      *
              WHEN       1012
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTCLP POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1013 : PROBLEME APPEL MODULE MFUSTADR POUR SELECTION
      *
              WHEN       1013
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTADR POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1014 : PROBLEME APPEL MODULE MFUSTLCC POUR FE CURSEUR
      *
              WHEN       1014
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTLCC POUR FE CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1015 : PROBLEME APPEL MODULE MFUSTCNT POUR FE CURSEUR
      *
              WHEN       1015
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTCNT POUR FE CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1016 : PROBLEME APPEL MODULE MFUSTEND POUR SELECTION
      *
              WHEN       1016
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTEND POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1017 : PROBLEME APPEL MODULE MFUSTADR POUR FE CURSEUR
      *
              WHEN       1017
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTADR POUR FE CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1018 : PROBLEME APPEL MODULE MFUSTADR POUR SELECTION
      *
              WHEN       1018
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTADR POUR SELECTION'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
      *       1019 : PROBLEME APPEL MODULE MFUSTLCC POUR SELECTION
      *
              WHEN       1019
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB APPEL MODULE MFUSTLCC POUR CL CURSEUR'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> RETURN CODE = '
                                         TO      WS-LIGNE-ANO10
      *          MOVE RETURN-CODE        TO      WS-LIGNE-ANO11
MCHA             MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE '*=> OP06-C-RET  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE OP06-C-RET         TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (6)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (7)
      *
151197*
151197*       1020 : PROBLEME APPEL MODULE MFUSTHIS POUR OUVERTURE
      *
MCHA++        WHEN       1020
151197           MOVE    7               TO      INAB-Q-LIST-DISP
151197           MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
151197           MOVE '*=> CODE ABEND  = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
151197           MOVE '*=> PB APPEL MODULE MFUSTHIS POUR OUVERTURE'
151197                                   TO      WS-LIGNE-ANO21
151197           MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
151197           MOVE '*=> CODE RETOUR = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
151197*
151197*       1021 : PROBLEME APPEL MODULE MFUSTHIS POUR LECTURE
151197*
151197        WHEN       1021
151197           MOVE    7               TO      INAB-Q-LIST-DISP
151197           MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
151197           MOVE '*=> CODE ABEND  = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
151197           MOVE '*=> PB APPEL MODULE MFUSTHIS POUR LECTURE'
151197                                   TO      WS-LIGNE-ANO21
151197           MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
151197           MOVE '*=> CODE RETOUR = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
151197*
151197*       1022 : PROBLEME APPEL MODULE MFUSTHIS POUR FERMETURE
151197*
151197        WHEN       1022
151197           MOVE    7               TO      INAB-Q-LIST-DISP
151197           MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
151197           MOVE '*=> CODE ABEND  = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
151197           MOVE '*=> PB APPEL MODULE MFUSTHIS POUR FERMETURE'
151197                                   TO      WS-LIGNE-ANO21
151197           MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
151197           MOVE '*=> CODE RETOUR = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
151197*
151197*       1023 : PROBLEME APPEL MODULE MFUSTHID POUR OUVERTURE
151197*
151197        WHEN       1023
151197           MOVE    7               TO      INAB-Q-LIST-DISP
151197           MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
151197           MOVE '*=> CODE ABEND  = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
151197           MOVE '*=> PB APPEL MODULE MFUSTHID POUR OUVERTURE'
151197                                   TO      WS-LIGNE-ANO21
151197           MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
151197           MOVE '*=> CODE RETOUR = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
151197*
151197*       1024 : PROBLEME APPEL MODULE MFUSTHID POUR LECTURE
151197*
151197        WHEN       1024
151197           MOVE    7               TO      INAB-Q-LIST-DISP
151197           MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
151197           MOVE '*=> CODE ABEND  = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
151197           MOVE '*=> PB APPEL MODULE MFUSTHID POUR LECTURE'
151197                                   TO      WS-LIGNE-ANO21
151197           MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
151197           MOVE '*=> CODE RETOUR = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
151197           MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
MCHA++*
"     *       1025 : PROBLEME APPEL MODULE MFUSTEND POUR LECTURE
"     *
"             WHEN       1025
"                MOVE    5               TO      INAB-Q-LIST-DISP
"                MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
"                MOVE '*=> CODE ABEND  = '
"                                        TO      WS-LIGNE-ANO10
"                MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
"                MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
"                MOVE '*=> PB ADRESSE ABSENTE DE LE ENTITE DECLARANTE'
"                                        TO      WS-LIGNE-ANO21
"                MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
"                MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
"                MOVE '*=> REF GIIN    = '
"                                        TO      WS-LIGNE-ANO10
"                MOVE SPACES             TO      WS-LIGNE-ANO11
"                MOVE TEND-C-REF-GIIN    TO      WS-LIGNE-ANO12
MCHA++           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
      *       2410 : PROBLEME ECRITURE DANS FICHIER EN SORTIE
      *
              WHEN       2410
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB ECRITURE DANS FICHIER EN SORTIE'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> CODE RETOUR = '
                                         TO      WS-LIGNE-ANO10
                 MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
      *
      *       2100 : FICHIER EN ENTRéE VIDE
      *
              WHEN       2100
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> FICHIER EN ENTRéE VIDE'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> CODE RETOUR = '
                                         TO      WS-LIGNE-ANO10
                 MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
      *
      *       2200 : PB TCHNIQUE DE FICHIER EN ENTRéE
      *
              WHEN       2200
                 MOVE    7               TO      INAB-Q-LIST-DISP
                 MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
                 MOVE '*=> CODE ABEND  = '
                                         TO      WS-LIGNE-ANO10
                 MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
                 MOVE '*=> PB TCHNIQUE DE FICHIER EN ENTRéE'
                                         TO      WS-LIGNE-ANO21
                 MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
                 MOVE '*=> CODE RETOUR = '
                                         TO      WS-LIGNE-ANO10
                 MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
                 MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
                 MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
      *
      *  VALEUR D'ABEND INCONNUE DANS LE PROGRAMME
      *
150978*       2400 : PB TCHNIQUE DE FICHIER EN ENTRéE
"     *
"             WHEN       2400
"                MOVE    7               TO      INAB-Q-LIST-DISP
"                MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
"                MOVE '*=> CODE ABEND  = '
"                                        TO      WS-LIGNE-ANO10
"                MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
"                MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
"                MOVE '*=> PB TCHNIQUE DE FICHIER EN ENTRéE'
"                                        TO      WS-LIGNE-ANO21
"                MOVE WS-LIGNE-ANO2      TO      INAB-L-DISP (3)
"                MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (4)
"                MOVE '*=> CODE RETOUR = '
"                                        TO      WS-LIGNE-ANO10
"                MOVE ACCE-CODE-RETOUR   TO      WS-LIGNE-ANO11
"                MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (5)
"                MOVE WS-LIGNE-DECO1     TO      INAB-L-DISP (6)
"     *
"     *       2500 : ENTITE INEXISTENTE
"     *
"             WHEN       2500
"                MOVE    7               TO      INAB-Q-LIST-DISP
"                MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
"                MOVE '*=> CODE ABEND  = '
"                                        TO      WS-LIGNE-ANO10
"                MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
"                MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
"                MOVE '*=> ENTITE INEXISTENTE              '
"                                        TO      WS-LIGNE-ANO21
"     *
"     *
"     *       2600 : Problème de récupération de séquence
"     *
"             WHEN       2600
"                MOVE    7               TO      INAB-Q-LIST-DISP
"                MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
"                MOVE '*=> CODE ABEND  = '
"                                        TO      WS-LIGNE-ANO10
"                MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
"                MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
"                MOVE '*=> Problème de récupération de séquence'
"                                        TO      WS-LIGNE-ANO21
150978*
151197*       2700 : Problème de récupération de séquence
151197*
151197        WHEN       2700
151197           MOVE    7               TO      INAB-Q-LIST-DISP
151197           MOVE WS-LIGNE-ANO0      TO      INAB-L-DISP (1)
151197           MOVE '*=> CODE ABEND  = '
151197                                   TO      WS-LIGNE-ANO10
151197           MOVE WS-CODE-ABEND      TO      WS-LIGNE-ANO11
151197           MOVE WS-LIGNE-ANO1      TO      INAB-L-DISP (2)
151197           MOVE '*=> Problème d''appel au MFUSTLCC'
151197                                   TO      WS-LIGNE-ANO21
151197*
      *  VALEUR D'ABEND INCONNUE DANS LE PROGRAMME
      *
              WHEN OTHER
                 MOVE    3               TO      INAB-Q-LIST-DISP
                 MOVE '********************************'
                                         TO      INAB-L-DISP (1)
                 MOVE 'ERREUR INCONNU'
                                         TO      INAB-L-DISP (2)
                 MOVE '********************************'
                                         TO      INAB-L-DISP (3)
           END-EVALUATE
      *
      *    ARRET TRAITEMENT
      *
           CALL 'MCCDINAB' USING INAB-PARAM
           .
      *
      *============================================*
      * AFFICHAGE DE FIN DE LA SYSOUT DU PROGRAMME *
      *============================================*
       4300-AFFICHAGE-FIN.
      *
      *    RECUPERATION DATE ET HEURE DE FIN DU TRAITEMENT
      *    AFFICHAGE DES COMPTEURS DE PROGRAMME
      *

      * BILAN D'EXECUTION
           MOVE  21             TO      BILA-Q-LIST-DISP
           MOVE  WS-LIGNE-DECO1 TO      BILA-L-DISP (1)
           MOVE  WS-LIGNE-DECO0 TO      BILA-L-DISP (2)
           MOVE  WS-LIGNE-DECO1 TO      BILA-L-DISP (3)

           MOVE W-D-SYSTEME-ISO TO      WS-FIN-PART1
           MOVE '*  DATE  DE FIN TRAITEMENT   :    '
                                TO      WS-LN-LIB-FIN
           MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
           MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (4)

           MOVE W-H-SYSTEME-ISO TO      WS-FIN-PART1
           MOVE '*  HEURE DE FIN TRAITEMENT   :    '
                                TO      WS-LN-LIB-FIN
           MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
           MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (05)

           MOVE WS-LIGNE-DECO1  TO      BILA-L-DISP (06)

      * NOMBRE D'ENREGISTREMENTS LUS EN ENTRéE
           MOVE WS-CPT-AIG-LUS
                                TO      WS-FIN-PART1
           MOVE '*  NOMBRE D''ENREG. LUS TOTAL          : '
                                TO      WS-LN-LIB-FIN
           MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
           MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (07)

150978* NOMBRE D'ENREGISTREMENTS LUS EN ENTRéE TYPE 10
150978     MOVE WS-CPT-AIG-LUS-10
150978                          TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE D''ENREG. LUS DE TYPE CPT/CLT: '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (08)
150978
150978* NOMBRE D'ENREGISTREMENTS LUS EN ENTRéE TYPE 20
150978     MOVE WS-CPT-AIG-LUS-20
150978                          TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE D''ENREG. LUS DE TYPE RECAL  : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (09)
      * NOMBRE D'ENREGISTREMENTS INSéRéES VIA LE MFUSTANO
           MOVE WS-CPT-ECRIT    TO      WS-FIN-PART1
           MOVE '*  NOMBRE ECRITURE EN SORTIE          : '
                                TO      WS-LN-LIB-FIN
           MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
           MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (10)

150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT1-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS11 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (11)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT2-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS12 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (12)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT3-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS13 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (13)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT4-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS14 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (14)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT5-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS15 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (15)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT6-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS16 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (16)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT7-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS17 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (17)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT8-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS18 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (18)
150978
150978* NOMBRE D'ENREGISTREMENTS INSéRéES
150978     MOVE WS-EDT9-ECRIT   TO      WS-FIN-PART1
150978     MOVE '*  NOMBRE ECRITURE EN SORTIE DFUSLS19 : '
150978                          TO      WS-LN-LIB-FIN
150978     MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
150978     MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (19)


      * NOMBRE DE CHECKPOINT EFFECTUES
           MOVE WS-CPT-NBRE-CHECK
                                TO      WS-FIN-PART1
           MOVE '*  WS-CPT-NBRE-CHECK                  : '
                                TO      WS-LN-LIB-FIN
           MOVE WS-LIGNE-FIN0   TO      WS-LN-VAL-FIN
           MOVE WS-LIGNE-FIN    TO      BILA-L-DISP (20)

           MOVE WS-LIGNE-DECO1  TO      BILA-L-DISP (21)

      * APPEL MCCDBILA
           CALL 'MCCDBILA'      USING   BILA-PARAM
           .

      *---------------------------------------------------------*
      *                   PARAGRAPHES 9XXX
      *---------------------------------------------------------*

      *=========================================================*
      * RECUPERATION DES DATES ET HEURES SYSTEME ET DE DERNIERE *
      * COMPILATION                                             *
      *=========================================================*
       9920-RECUP-DATES-HEURES.
      *

      *--> RECUPERATION DE LA DATE ET DE L'HEURE SYSTEME
      *--> INTERROGER UNE DATE
           MOVE 'ACD-D-HR'            TO NOM-PRIMITIVE
           PERFORM 9925-APPEL-MGDATR03
      *
      *    RETOUR TECHNIQUE
           IF RETURN-CODE NOT = ZERO
              MOVE    1000        TO         WS-CODE-ABEND
              PERFORM 4200-ABEND-ERR
           ELSE
      *       RETOUR FONCTIONNEL
              IF OP06-C-RET NOT = 0
                 MOVE 1001                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
              END-IF
           END-IF
      *
150978     MOVE OP06-T-REF            TO W-DATE-TIMESTAMP
           MOVE D-ISO OF OP06-T-REF   TO W-D-SYSTEME-ISO
           MOVE H-ISO OF OP06-T-REF   TO W-H-SYSTEME-ISO
150978     MOVE D-ISO-AAAA OF D-ISO OF OP06-T-REF
"                                     TO WS-A-ANNEE-FISC
"          MOVE '0001-01-01'          TO WS-A-APPL
"          ADD    -1                  TO WS-A-ANNEE-FISC
"          MOVE  WS-A-ANNEE-FISC      TO WS-A-APPL(1:4)
150978D    DISPLAY 'WS-A-APPL : ' WS-A-APPL
           MOVE ':'                   TO W-H-SYSTEME-ISO(3:1)
           MOVE ':'                   TO W-H-SYSTEME-ISO(6:1)

      *--> RECUPERATION DE LA DATE ET DE L'HEURE DE COMPILATION
           MOVE WHEN-COMPILED         TO W-D-H-COMPIL
           MOVE W-D-COMPIL(4:2)       TO W-D-COMPIL-JJ
           MOVE W-D-COMPIL(1:2)       TO W-D-COMPIL-MM
           MOVE W-D-COMPIL(7:2)       TO W-D-COMPIL-AA
           MOVE ':'                   TO W-H-COMPIL(3:1)
           MOVE ':'                   TO W-H-COMPIL(6:1)
           .

      *=========================================================*
      * RECUPERATION DES DATES ET HEURES SYSTEME                *
      *=========================================================*
       9922-RECUP-DATES-HEURES.
      *

      *--> RECUPERATION DE LA DATE ET DE L'HEURE SYSTEME
      *--> INTERROGER UNE DATE
           MOVE 'ACD-D-HR'            TO NOM-PRIMITIVE
           PERFORM 9925-APPEL-MGDATR03
      *
      *    RETOUR TECHNIQUE
           IF RETURN-CODE NOT = ZERO
              MOVE    1000        TO         WS-CODE-ABEND
              PERFORM 4200-ABEND-ERR
           ELSE
      *       RETOUR FONCTIONNEL
              IF OP06-C-RET NOT = 0
                 MOVE 1001                      TO WS-CODE-ABEND
                 PERFORM 4200-ABEND-ERR
              END-IF
           END-IF
      *
           MOVE D-ISO OF OP06-T-REF   TO W-D-SYSTEME-ISO
           MOVE H-ISO OF OP06-T-REF   TO W-H-SYSTEME-ISO
           MOVE ':'                   TO W-H-SYSTEME-ISO(3:1)
           MOVE ':'                   TO W-H-SYSTEME-ISO(6:1)
           .
      *
      *===============================================*
      * APPEL AU MODULE MGDATR03 DE GESTION DES DATES *
      *===============================================*
       9925-APPEL-MGDATR03.
      *
      *
      *  APPEL MODULE
           CALL 'MGDATR03' USING NOM-PRIMITIVE
                                 MGDATR03-PARAM
           .

      *
      *==================================================*
      * PRISE DE CHECKPOINT POUR UN REDEMARRAGE EVENTUEL *
      *==================================================*
       9960-PRISE-CHECKPOINT.
      *
      * PRISE CHECKPOINT ET SAUVEGARDE COMPTEURS
           ADD     1                    TO WS-CPT-CHKPT
150978D    DISPLAY 'WS-CPT-CHKPT :' WS-CPT-CHKPT
           IF      WS-CPT-CHKPT    NOT <   W999-FREQCHKP

              ADD   1                  TO   WS-CPT-NBRE-CHECK
              MOVE  ZERO               TO   WS-CPT-CHKPT
              MOVE  WS-CPT-ECRIT       TO   W999-CPT-ECRIT
150978        MOVE  WS-EDT1-ECRIT      TO   W999-EDT1-ECRIT
150978        MOVE  WS-EDT2-ECRIT      TO   W999-EDT2-ECRIT
150978        MOVE  WS-EDT3-ECRIT      TO   W999-EDT3-ECRIT
150978        MOVE  WS-EDT4-ECRIT      TO   W999-EDT4-ECRIT
150978        MOVE  WS-EDT5-ECRIT      TO   W999-EDT5-ECRIT
150978        MOVE  WS-EDT6-ECRIT      TO   W999-EDT6-ECRIT
150978        MOVE  WS-EDT7-ECRIT      TO   W999-EDT7-ECRIT
150978        MOVE  WS-EDT8-ECRIT      TO   W999-EDT8-ECRIT
150978        MOVE  WS-EDT9-ECRIT      TO   W999-EDT9-ECRIT
              MOVE  WS-A-APPL          TO   W999-A-APPL
150978*       MOVE  TEND-I-IDENT-END   TO   W999-I-IDENT-END
150978        MOVE  WS-I-IDENT-END-SV  TO   W999-I-IDENT-END
150978        MOVE  WS-I-IDENT-REF-REP TO   W999-I-IDENT-REF-REP
              MOVE  WS-CPT-NBRE-CHECK  TO   W999-NB-CHECKPOINT
150978        MOVE  WS-Q-PERS-DECL     TO   W999-Q-PERS-DECL
150978        MOVE  WS-Q-CONTRAT-DECL  TO   W999-Q-CONTRAT-DECL
150978        MOVE  WS-Q-RECAL-DECL    TO   W999-Q-RECAL-DECL
150978        MOVE  WS-M-TOT-SOL-DECL  TO   W999-M-TOT-SOL-DECL
150978        MOVE  WS-M-TOT-INT-DECL  TO   W999-M-TOT-INT-DECL
150978        MOVE  WS-M-TOT-CES-DECL  TO   W999-M-TOT-CES-DECL
150978        MOVE  WS-M-TOT-M-DECL    TO   W999-M-TOT-M-DECL
150978        MOVE  LST-ENT-DECL       TO   W999-ENT-DECL
150978        MOVE  WS-I-REF-FIC       TO   W999-I-REF-FIC
150978        MOVE  WS-I-REF-BLOC-CLR  TO   W999-I-REF-BLOC-CLR
150978        MOVE  WS-I-REF-BLOC-CPT  TO   W999-I-REF-BLOC-CPT
150978        MOVE  WS-CPT-AIG-LUS     TO   W999-CPT-AIG-LUS
150978        MOVE  WS-CPT-AIG-LUS-10  TO   W999-CPT-AIG-LUS-10
150978        MOVE  WS-CPT-AIG-LUS-20  TO   W999-CPT-AIG-LUS-20

              PERFORM SQ-COMMIT
      D       DISPLAY 'BFUSEL10 > PRISE DE CHECKPOINT'
150978D       DISPLAY 'W999-I-IDENT-END     :'W999-I-IDENT-END
150978D       DISPLAY 'W999-I-IDENT-REF-REP :'W999-I-IDENT-REF-REP
150978D       DISPLAY 'W999-CPT-AIG-LUS     :'W999-CPT-AIG-LUS
150978D       DISPLAY 'W999-CPT-AIG-LUS-10  :'W999-CPT-AIG-LUS-10
150978D       DISPLAY 'W999-CPT-AIG-LUS-20  :'W999-CPT-AIG-LUS-20
150978D       DISPLAY 'W999-CPT-ECRIT       :'W999-CPT-ECRIT
150978D       DISPLAY 'W999-EDT1-ECRIT      :'W999-EDT1-ECRIT
150978D       DISPLAY 'W999-EDT2-ECRIT      :'W999-EDT2-ECRIT
150978D       DISPLAY 'W999-EDT3-ECRIT      :'W999-EDT3-ECRIT
150978D       DISPLAY 'W999-EDT4-ECRIT      :'W999-EDT4-ECRIT
150978D       DISPLAY 'W999-EDT5-ECRIT      :'W999-EDT5-ECRIT
150978D       DISPLAY 'W999-EDT6-ECRIT      :'W999-EDT6-ECRIT
150978D       DISPLAY 'W999-EDT7-ECRIT      :'W999-EDT7-ECRIT
150978D       DISPLAY 'W999-EDT8-ECRIT      :'W999-EDT8-ECRIT
150978D       DISPLAY 'W999-EDT9-ECRIT      :'W999-EDT9-ECRIT
150978D       DISPLAY 'W999-A-APPL          :'W999-A-APPL
150978D       DISPLAY 'W999-NB-CHECKPOINT   :'W999-NB-CHECKPOINT
150978D       DISPLAY 'W999-Q-PERS-DECL     :'W999-Q-PERS-DECL
150978D       DISPLAY 'W999-Q-CONTRAT-DECL  :'W999-Q-CONTRAT-DECL
150978D       DISPLAY 'W999-Q-RECAL-DECL    :'W999-Q-RECAL-DECL
150978D       DISPLAY 'W999-M-TOT-SOL-DECL  :'W999-M-TOT-SOL-DECL
150978D       DISPLAY 'W999-M-TOT-INT-DECL  :'W999-M-TOT-INT-DECL
150978D       DISPLAY 'W999-M-TOT-CES-DECL  :'W999-M-TOT-CES-DECL
150978D       DISPLAY 'W999-M-TOT-M-DECL    :'W999-M-TOT-M-DECL
150978D       DISPLAY 'W999-ENT-DECL        :'W999-ENT-DECL
150978D       DISPLAY 'W999-I-REF-FIC       :'W999-I-REF-FIC
150978D       DISPLAY 'W999-I-REF-BLOC-CLR  :'W999-I-REF-BLOC-CLR
150978D       DISPLAY 'W999-I-REF-BLOC-CPT  :'W999-I-REF-BLOC-CPT
           END-IF
           .

      *==================================================*
      * TRAITEMENT FIN DU PROGRAMME
      *==================================================*
       FIN-PROGRAMME.
      *
      * AFFICHAGE COMPTE RENDU D'EXECUTION
           PERFORM 9922-RECUP-DATES-HEURES
           PERFORM 4300-AFFICHAGE-FIN

      * ARRET TRAITEMENT
           PERFORM SQ-FIN-PGM-BATCH
           GOBACK
           .
      *
      * CLAUSE COPY POUR LES COMPOSANTS FONCTIONNELS
       COPY CANAAFCT.

      * CLAUSE COPY POUR LES PROGRAMMES BATCHS
       COPY CANAABAT.
       TRAIT-ERREUR-TECH.
           MOVE ACCE-SQLCODE      TO WS-SQLCODE-EDIT.
      D    DISPLAY 'ERREUR TECHNIQUE INTERCEPTEE'
      D    DISPLAY 'TYPE ERREUR ' W999-TYPE-ERREUR
      D    DISPLAY 'SQLCA       ' ACCE-SQLCA
      D    DISPLAY 'SQLCODE     ' WS-SQLCODE-EDIT
      D    DISPLAY 'CODE ERREUR ' W999-CODE-ERREUR
      D    DISPLAY 'LIB  ERREUR ' W999-LIB-ERREUR
      D    DISPLAY 'MODU ERREUR ' W999-PGM-ERREUR.
      *--- COPY TECHNIQUE IDEV D'APPEL AUX ACCESSEURS
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTEND',
                                 ==(PREF)== BY  ==TEND==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTADR',
                                 ==(PREF)== BY  ==TADR==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTCNT',
                                 ==(PREF)== BY  ==TCNT==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTLCC',
                                 ==(PREF)== BY  ==TLCC==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTCLI',
                                 ==(PREF)== BY  ==TCLI==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTCLP',
                                 ==(PREF)== BY  ==TCLP==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTCLM',
                                 ==(PREF)== BY  ==TCLM==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTRUB',
                                 ==(PREF)== BY  ==TRUB==,
                                 'MODE'     BY  'RIEN'.
      *---
       COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTLPP',
                                 ==(PREF)== BY  ==TLPP==,
                                 'MODE'     BY  'RIEN'.
150978*---
"      COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTFID',
"                                ==(PREF)== BY  ==TFID==,
"                                'MODE'     BY  'RIEN'.
"     *---
"      COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTCLR',
"                                ==(PREF)== BY  ==TCLR==,
150978                           'MODE'     BY  'RIEN'.
      *---
151197 COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTHIS',
151197                           ==(PREF)== BY  ==THIS==,
151197                           'MODE'     BY  'RIEN'.
151197*---
151197 COPY CANAAACP REPLACING   'MODULE'   BY  'MFUSTHID',
151197                           ==(PREF)== BY  ==THID==,
151197                           'MODE'     BY  'RIEN'.
      *---
      * CLAUSE COPY POUR LES BMP REDEMARRABLES
       COPY CANAARED.

       END PROGRAM BFUSEL10.
