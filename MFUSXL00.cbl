       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFUSXL00.
      ******************************************************************
      *      ____________________________________________________      *
      *    _(                                                    )_    *
      *  _(       F O R M A T A G E   D U   R E P O R T I N G      )_  *
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
      *               APPLICATION   : FUS                              *
      *               DATE          : 27/10/2014                       *
      *               COMPOSANT     : MFUSXL00                         *
      *               TYPE          : MODULE                           *
      *               CHAINE        : FUSEL                            *
      *               AUTEUR        : EBAI - ESSADIQ BAICH - CGI       *
      *               REFERENCE     : GAMA - 20141525                  *
      *                                                                *
      *             __________________________________________         *
      *                                                                *
      *                                                                *
      *       MODULE D'AIDE A LA CREATION DES BALISES XML              *00030039
      *                                                                *
      *     LE MODULE FOURNIT PLUSIEURS FONCTIONS POUR ENRICHIR,       *
      *     TRANSFORMER ET FORMATER UN STRING XML. EN PARTICULIER      *
      *     POUR CONTOURNER LES LIMITES DU XML GENERATE DE LA          *
      *     VERSION 4.2 DU COMPILATEUR COBOL.                          *
      *                                                                *
      *________________________________________________________________*
      *                             !                                  *
      *  CODE RETOUR                ! DESCRIPTION                      *
      *_____________________________!__________________________________*
      *                             !                                  *
      *  ZCOM-CODE-RETOUR = 0       ! PAS D'ERREUR SYSTEME             *
      *  ZCOM-CODE-RETOUR = 4       ! ERREUR NON BLOQUANTE             *
      *  ZCOM-CODE-RETOUR = 8       ! ERREUR BLOQUANTE FONCTIONNEL     *
      *  ZCOM-CODE-RETOUR = 12      ! ERREUR BLOQUANTE TECHNIQUE       *
      *_____________________________!__________________________________*
      *                                                                *
      *                                                                *
      * Historique des modifications :                                 *
      *                                                                *
      *________________________________________________________________*
      *              |          |        |                             *
      *    AUTEUR    |   DATE   |  REF.  | DESCRIPTION                 *
      *______________|__________|________|_____________________________*
      *              |          |        |                             *
      * Essadiq      | 27/10/14 | 141525 | CREATION                    *
      *    BAICH     |          |        |                             *
      *______________|__________|________|_____________________________*
      *                                                                *
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
      *OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

                                                                        00630099
      *****************************************************************
      *                                                               *
      *                   D A T A   D I V I S I O N                   *
      *                                                               *
      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      **********************************************
      *                                            *
      *   Variables du travail pour les fonctions  *
      *                                            *
      **********************************************
       01     WS-FCT.

      * ---- IN


        05    WS-Q-XML                  PIC  9(08) BINARY VALUE ZERO.
size  * 05    WS-L-XML                  PIC  X(200000) VALUE SPACE.
size    05    WS-L-XML                  PIC  X(22000) VALUE SPACE.
      *       Indentation
        05    WS-Q-INDT-INIT            PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-INDT-UNIT            PIC  9(04) BINARY VALUE ZERO.
      *       Declaration
        05    WS-Q-DECL-ENCODING        PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-DECL-ENCODING        PIC  X(80) VALUE SPACE.
      *       Namespace
        05    WS-Q-NMSP                 PIC  9(04)       BINARY.
size  * 05    WS-G-NMSP OCCURS 10.
size    05    WS-G-NMSP OCCURS 5.
         15   WS-Q-NMSP-PRFX            PIC  9(04)       BINARY.
         15   WS-L-NMSP-PRFX            PIC  X(20).
         15   WS-Q-NMSP-URN             PIC  9(04)       BINARY.
         15   WS-L-NMSP-URN             PIC  X(80).
      *       Schema
        05    WS-Q-XSD-URN              PIC  9(04)       BINARY.
        05    WS-L-XSD-URN              PIC  X(80).
      *       Indicatif des noms résérvés à enlever
        05    WS-Q-RESV-INDICATIF       PIC  9(04)       BINARY.
        05    WS-L-RESV-INDICATIF       PIC  X(80).

      *       Préfixe par défaut
        05    WS-Q-PRFX-DFLT            PIC  9(04)       BINARY.
        05    WS-L-PRFX-DFLT            PIC  X(20).
      *       Préfixe paramètre
        05    WS-Q-PRFX-PARM            PIC  9(04)       BINARY.
        05    WS-L-PRFX-PARM            PIC  X(20).
      *       Liste des noms balises
        05    WS-Q-PRFX                 PIC  9(04)       BINARY.
size  * 05    WS-G-PRFX OCCURS 100.
size    05    WS-G-PRFX OCCURS 30.
         10   WS-Q-PRFX-TAG             PIC  9(04)       BINARY.
         10   WS-L-PRFX-TAG             PIC  X(80).



      * ---- OUT

      * --  INDT

      * --
      * -- Le code famille Q signifie longueur
      * -- Le code famille N signifie position
      * -- Le code famille B signifie booléen (existence)
      * --

      *
      *       Nombre de lignes XML
        05    WS-Q-XML-TAB              PIC  9(04) BINARY VALUE ZERO.
      *       Données ligne XML
        05    WS-G-XML-TAB              OCCURS 1000.
size  * 05    WS-G-XML-TAB              OCCURS 120.
      *       Taille totale de la ligne XML
         10   WS-Q-XML-LINE             PIC  9(04) BINARY VALUE ZERO.
      *       Indentation de la ligne XML
         10   WS-Q-XML-INDT             PIC  9(04) BINARY VALUE ZERO.
      *       Contenu de la ligne XML
         10   WS-L-XML-LINE             PIC  X(1000) VALUE SPACE.
size  *  10   WS-L-XML-LINE             PIC  X(250) VALUE SPACE.
      *
      *       Est elle une décalration XML ?
         10   WS-B-XML-TAG-DECL         PIC  X(01) VALUE SPACE.
      *       Niveau imbrication
         10   WS-Q-XML-TAG-IMBR         PIC S9(04) BINARY VALUE ZERO.
      *       Taille nom de la balise
         10   WS-Q-XML-TAG-NAME         PIC  9(04) BINARY VALUE ZERO.
      *       Offset nom de la balise
         10   WS-N-XML-TAG-NAME         PIC  9(04) BINARY VALUE ZERO.
      *       Donnée nom de la balise
         10   WS-L-XML-TAG-NAME         PIC  X(80) VALUE SPACE.
      *       Offset attributs de la balise
         10   WS-N-XML-TAG-ATTR         PIC  9(04) BINARY VALUE ZERO.
      *       Taille attributs de la balise
         10   WS-Q-XML-TAG-ATTR         PIC  9(04) BINARY VALUE ZERO.
      *       Donnée attributs de la balise
         10   WS-L-XML-TAG-ATTR         PIC  X(80) VALUE SPACE.
      *       Taille Text de la balise
         10   WS-Q-XML-TAG-TEXT         PIC  9(04) BINARY VALUE ZERO.
      *       Offset Text de la balise
         10   WS-N-XML-TAG-TEXT         PIC  9(04) BINARY VALUE ZERO.
      *       Donnée Text de la balise
         10   WS-L-XML-TAG-TEXT         PIC  X(255) VALUE SPACE.
      *       Offset fin balise   > (nom balise + attributs)
         10   WS-N-XML-TAG-FIN          PIC  9(04) BINARY VALUE ZERO.
      *       Offset debut balise 2 (si balise simple)
         10   WS-N-XML-TAG-DEB2         PIC  9(04) BINARY VALUE ZERO.
      *       Est elle une balise ouvrante
         10   WS-B-XML-TAG-OUVR         PIC  X(01) VALUE SPACE.
      *       Est elle une balise fermante
         10   WS-B-XML-TAG-FERM         PIC  X(01) VALUE SPACE.
      *       Est elle une balise complexe
         10   WS-B-XML-TAG-CMPLX        PIC  X(01) VALUE SPACE.
      *       Est elle une balise simple
         10   WS-B-XML-TAG-SMPLE        PIC  X(01) VALUE SPACE.
      *       Numéro de ligne de la balise mère
         10   WS-N-XML-TAG-MERE         PIC  9(04) BINARY VALUE ZERO.
      *

      * --  DCOP

        05    WS-N-XML-DCOP             PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-XML-DCOP-INDT        PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-DCOP-TAG             PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-DCOP-TAG             PIC  X(80) VALUE SPACE.

      * ---- INTERNES

        05    WS-Q-XML-BUFFER-IMBR      PIC S9(04) BINARY VALUE ZERO.
        05    WS-Q-XML-BUFFER           PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-BUFFER           PIC  X(1000) VALUE SPACE.
        05    WS-Q-XML-BUFFER-ATTR      PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-BUFFER-ATTR      PIC  X(1000) VALUE SPACE.
        05    WS-Q-XML-BUFFER-PRFX      PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-BUFFER-PRFX      PIC  X(20) VALUE SPACE.
        05    WS-L-XML-BLANC            PIC  X(100) VALUE SPACES.
        05    I                         PIC  9(08) BINARY VALUE ZERO.
        05    J                         PIC  9(08) BINARY VALUE ZERO.
        05    K                         PIC  9(08) BINARY VALUE ZERO.
        05    L                         PIC  9(08) BINARY VALUE ZERO.
        05    IND                       PIC  9(04) BINARY VALUE ZERO.
        05    IND2                      PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-LENGTH               PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-OFFSET               PIC  9(04) BINARY VALUE ZERO.

        05    WS-C-TAG-COMPLEXE         PIC 9(01)   VALUE 0.
         88   TAG-CMPLX                             VALUE 1.
         88   TAG-SMPLE                             VALUE 2.

        05    WS-C-TAG-FERMANTE         PIC 9(01)   VALUE 0.
         88   TAG-FERM                              VALUE 1.
         88   TAG-OUVR                              VALUE 2.

        05    WS-C-TAG-DEB-FIN          PIC 9(01)   VALUE 0.
         88   TAG-DEB                               VALUE 1.
         88   TAG-FIN                               VALUE 2.

        05    WS-C-TAG-NAME-ATTR        PIC 9(01)   VALUE 0.
         88   TAG-NAME                              VALUE 1.
         88   TAG-ATTR                              VALUE 2.

        05    WS-N-XML-TAG-NAME-BUFFER PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-XML-TAG-NAME-BUFFER PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-TAG-NAME-BUFFER PIC  X(80) VALUE SPACE.
        05    WS-N-XML-TAG-ATTR-BUFFER PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-XML-TAG-ATTR-BUFFER PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-TAG-ATTR-BUFFER PIC  X(80) VALUE SPACE.
        05    WS-N-XML-TAG-FIN-BUFFER  PIC  9(04) BINARY VALUE ZERO.
        05    WS-N-XML-TAG-NAME-CREER  PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-XML-TAG-NAME-CREER  PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-TAG-NAME-CREER  PIC  X(80) VALUE SPACE.
        05    WS-N-XML-TAG-ATTR-CREER  PIC  9(04) BINARY VALUE ZERO.
        05    WS-Q-XML-TAG-ATTR-CREER  PIC  9(04) BINARY VALUE ZERO.
        05    WS-L-XML-TAG-ATTR-CREER  PIC  X(80) VALUE SPACE.
        05    WS-N-XML-TAG-FIN-CREER   PIC  9(04) BINARY VALUE ZERO.

        05    WS-Q-NIVEAU-IMBR         PIC  9(04) BINARY VALUE ZERO.
        05    WS-G-NIVEAU-IMBR OCCURS 500.
         10   WS-N-NIVEAU-IMBR         PIC  9(04) BINARY VALUE ZERO.

      * --  DCOP

      * WORKING COMMUNE IDEV DES COMPOSANTS FONCTIONNELS
       COPY CWNAAFCT.

      * WORKING COMMUNE IDEV DES MODULES FONCTIONNELS
       COPY CWNAAMOD.

      * COPY TECHNIQUE DU MODULE FONCTIONNEL
       COPY TFUSXL00.
      *

      *****************************************************************
      *                                                               *
      *                 L I N K A G E  S E C T I O N                  *
      *                                                               *
      *****************************************************************

       LINKAGE SECTION.
      *---------------*

      * ZONE DE COMMUNICATION DU MODULE FONCTIONNEL
       COPY CFUSXL00 REPLACING ==(PREF)== BY ==ZCOM==.


      *****************************************************************
      *                                                               *
      *            P R O C E D U R E   D I V I S I O N                *
      *                                                               *
      *****************************************************************

       PROCEDURE DIVISION USING ZCOM-BIND,
                                ZCOM-NO-VERSION,
                                ZCOM-TECH-OUT,
                                ZCOM-FONC-IN,
                                ZCOM-FONC-IN-OUT,
                                ZCOM-FONC-OUT.


      * ----------->  DECLARATIVES


      DDECLARATIVES.
      DTRACING      SECTION.
      D    USE FOR DEBUGGING ON ALL PROCEDURES.
      DETIQ.
      D    DISPLAY 'MFUSXL00 ' DEBUG-NAME.
      DEND DECLARATIVES.



      * ----------->  Cinématique générale



           PERFORM INITIALISATION
           PERFORM TRAITEMENTS
           PERFORM FIN-PROGRAMME
           .


      *--------------*
       INITIALISATION.
      *--------------*

      *    Affichage des paramètres en entrée
      D    PERFORM AFFICH-ZCOM-FONC-IN

      *    Initialisations IDEV
           PERFORM SQ-INIT-MODULE

      *    Initialisation  des données en sortie
           PERFORM INIT-ZCOM-FONC-OUT

      *    Initialisation  des données du travail
           PERFORM INIT-DONNEES-TRAVAIL

      *    Contrôle des données en entrée
           PERFORM CTRL-ZCOM-FONC-IN

           .

      *-----------------*
       CTRL-ZCOM-FONC-IN.
      *-----------------*

      *    Contrôle des données d'entrée.

      D    DISPLAY 'ZCOM-B-INDT     :'        ZCOM-B-INDT
      D    DISPLAY 'ZCOM-B-NMSP     :'        ZCOM-B-NMSP
      D    DISPLAY 'ZCOM-B-XSD      :'        ZCOM-B-XSD
      D    DISPLAY 'ZCOM-B-PRFX     :'        ZCOM-B-PRFX
      D    DISPLAY 'ZCOM-B-ATTR     :'        ZCOM-B-ATTR
      D    DISPLAY 'ZCOM-B-TEXT     :'        ZCOM-B-TEXT
      D    DISPLAY 'ZCOM-B-VIDE     :'        ZCOM-B-VIDE
      D    DISPLAY 'ZCOM-B-RESV     :'        ZCOM-B-RESV
      D    DISPLAY 'ZCOM-B-DECL     :'        ZCOM-B-DECL
      D    DISPLAY 'ZCOM-B-DCOP     :'        ZCOM-B-DCOP

           IF ZCOM-L-XML-BRUTE = SPACES OR LOW-VALUE
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  01                    TO   ZCOM-C-RET
              MOVE 'Donnée <L-XML-BRUTE> non renseignée'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF ZCOM-Q-XML-BRUTE     = ZEROS
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  02                    TO   ZCOM-C-RET
              MOVE 'Donnée <Q-XML-BRUTE> non renseignée'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-INDT NOT = '1' AND ZCOM-B-INDT NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-NMSP NOT = '1' AND ZCOM-B-NMSP NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-XSD  NOT = '1' AND ZCOM-B-XSD  NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-PRFX NOT = '1' AND ZCOM-B-PRFX NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-ATTR NOT = '1' AND ZCOM-B-ATTR NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-TEXT NOT = '1' AND ZCOM-B-TEXT NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-VIDE NOT = '1' AND ZCOM-B-VIDE NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-RESV NOT = '1' AND ZCOM-B-RESV NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-DECL NOT = '1' AND ZCOM-B-DECL NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF (ZCOM-B-DCOP NOT = '1' AND ZCOM-B-DCOP NOT = '0')
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  03                    TO   ZCOM-C-RET
              MOVE 'Boolèens des fonctions mal renseignées'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF

           IF ZCOM-B-INDT = '1'
              IF ZCOM-Q-INDT-UNIT = ZERO
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  04                    TO   ZCOM-C-RET
                 MOVE 'Indentation demandée mais paramètres manquants'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF

           IF ZCOM-B-DCOP = '1'
              IF ZCOM-Q-DCOP-TAG NOT > 0
              OR ZCOM-L-DCOP-TAG     = SPACE
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  06                    TO   ZCOM-C-RET
                 MOVE 'Decoupage demandé mais paramètres manquants'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF

           IF ZCOM-B-DECL = '1'
              IF ZCOM-Q-DECL-ENCODING NOT > 0
              OR ZCOM-L-DECL-ENCODING     = SPACE
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  09                    TO   ZCOM-C-RET
                 MOVE 'XML Declaration demandée mais paramètre manquant'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF

           IF ZCOM-B-NMSP = '1'
              IF ZCOM-Q-NMSP          NOT > 0
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  10                    TO   ZCOM-C-RET
                 MOVE 'XML Name space demandé mais paramètre manquant'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF

           IF ZCOM-B-XSD  = '1'
              IF ZCOM-Q-XSD-URN       NOT > 0
              OR ZCOM-L-XSD-URN           = SPACE
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  11                    TO   ZCOM-C-RET
                 MOVE 'XML Schema demandée mais paramètre manquant'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF

           IF ZCOM-B-RESV = '1'
              IF ZCOM-Q-RESV-INDICATIF  NOT > 0
              OR ZCOM-L-RESV-INDICATIF  = SPACE
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  12                    TO   ZCOM-C-RET
                 MOVE 'mots réservés demandé mais parmetre manquant'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF

           IF ZCOM-B-PRFX = '1'
              IF ZCOM-Q-PRFX-DFLT       NOT > 0
              OR ZCOM-L-PRFX-DFLT       = SPACE
              OR ZCOM-Q-PRFX-PARM       NOT > 0
              OR ZCOM-L-PRFX-DFLT       = SPACE
              OR ZCOM-Q-PRFX            NOT > 0
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  13                    TO   ZCOM-C-RET
                 MOVE 'gérer préfixes demandé mais parmetre manquant'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
              END-IF
           END-IF
           .

      *-----------*
       TRAITEMENTS.
      *-----------*

           MOVE ZCOM-Q-XML-BRUTE          TO  WS-Q-XML
           MOVE ZCOM-L-XML-BRUTE          TO  WS-L-XML

      ******************************
      *                            *
      *        PARSING XML         *
      *                            *
      ******************************

      *    Traitement initial du parsing XML
      *    Permet de fournir des informations du base pour les fonctions
      *    transforme le bloc XMl en lignes et calcule les indentations

           PERFORM PARSE-XML


      *    Attention l'ordre d'execution des fonctions est important !!

      ******************************
      *                            *
      *    TRAITEMENT VIDES        *
      *                            *
      ******************************

           IF ZCOM-B-VIDE = '1'

              PERFORM FCT-VIDE

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT RESERVES     *
      *                            *
      ******************************

           IF ZCOM-B-RESV = '1'

              MOVE ZCOM-Q-RESV-INDICATIF TO WS-Q-RESV-INDICATIF
              MOVE ZCOM-L-RESV-INDICATIF TO WS-L-RESV-INDICATIF

              PERFORM FCT-RESV

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT NAMESPACE    *
      *                            *
      ******************************

           IF ZCOM-B-NMSP = '1'

              MOVE ZCOM-Q-NMSP           TO WS-Q-NMSP

              PERFORM VARYING I FROM 1 BY 1
              UNTIL I > ZCOM-Q-NMSP

                 MOVE ZCOM-G-NMSP(I)     TO WS-G-NMSP(I)

              END-PERFORM


              PERFORM FCT-NMSP

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT SCHEMA       *
      *                            *
      ******************************

           IF ZCOM-B-XSD  = '1'

              MOVE ZCOM-Q-XSD-URN        TO WS-Q-XSD-URN
              MOVE ZCOM-L-XSD-URN        TO WS-L-XSD-URN

              PERFORM FCT-XSD

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT ATTRIBUTS    *
      *                            *
      ******************************

           IF ZCOM-B-ATTR = '1'

              PERFORM FCT-ATTR

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT TEXT         *
      *                            *
      ******************************

           IF ZCOM-B-TEXT = '1'

              PERFORM FCT-TEXT

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT DECOUPAGE    *
      *                            *
      ******************************

           IF ZCOM-B-DCOP = '1'

              MOVE ZCOM-Q-DCOP-TAG       TO WS-Q-DCOP-TAG
              MOVE ZCOM-L-DCOP-TAG       TO WS-L-DCOP-TAG

              PERFORM FCT-DCOP

           END-IF

      ******************************
      *                            *
      *    TRAITEMENT PREFIXES     *
      *                            *
      ******************************

           IF ZCOM-B-PRFX = '1'

              MOVE ZCOM-Q-PRFX-DFLT      TO WS-Q-PRFX-DFLT
              MOVE ZCOM-L-PRFX-DFLT      TO WS-L-PRFX-DFLT
              MOVE ZCOM-Q-PRFX-PARM      TO WS-Q-PRFX-PARM
              MOVE ZCOM-L-PRFX-PARM      TO WS-L-PRFX-PARM
              MOVE ZCOM-Q-PRFX           TO WS-Q-PRFX

              PERFORM VARYING I FROM 1 BY 1
              UNTIL I > ZCOM-Q-PRFX

                 MOVE ZCOM-G-PRFX(I)     TO WS-G-PRFX(I)

              END-PERFORM

              PERFORM FCT-PRFX

           END-IF

      ******************************
      *                            *
      *   TRAITEMENT INDENTATION   *
      *                            *
      ******************************

           IF ZCOM-B-INDT = '1'

              MOVE ZCOM-Q-INDT-INIT       TO  WS-Q-INDT-INIT
              MOVE ZCOM-Q-INDT-UNIT       TO  WS-Q-INDT-UNIT

              PERFORM FCT-INDT

           END-IF

      ******************************
      *                            *
      *   TRAITEMENT DECLARATION   *
      *                            *
      ******************************

           IF ZCOM-B-DECL = '1'

              MOVE ZCOM-Q-DECL-ENCODING   TO  WS-Q-DECL-ENCODING
              MOVE ZCOM-L-DECL-ENCODING   TO  WS-L-DECL-ENCODING

              PERFORM FCT-DECL

           END-IF

      ******************************
      *                            *
      * ALIMENTATION ZCOM-FONC-OUT *
      *                            *
      ******************************

           MOVE WS-N-XML-DCOP             TO  ZCOM-N-XML-DCOP
           MOVE WS-Q-XML-DCOP-INDT        TO  ZCOM-Q-XML-DCOP-INDT

size       IF WS-Q-XML-TAB > 120
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  17                    TO   ZCOM-C-RET
              MOVE 'Nombre de lignes dépasse taille ZCOM'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR

           END-IF

           MOVE WS-Q-XML-TAB              TO  ZCOM-Q-XML-TAB

           PERFORM VARYING I FROM 1 BY 1
           UNTIL   I > WS-Q-XML-TAB
      D       DISPLAY WS-L-XML-LINE(I)(1:WS-Q-XML-LINE(I))

size          IF WS-Q-XML-LINE(I) > 250
mcha+ *       IF WS-Q-XML-LINE(I) > 600
mcha+ *          DISPLAY ' WS-Q-XML-LINE(I) ' WS-Q-XML-LINE(I)
                 MOVE  08                    TO   ZCOM-CODE-RETOUR
                 MOVE  18                    TO   ZCOM-C-RET
                 MOVE 'taille ligne dépasse taille ZCOM'
                                             TO   ZCOM-L-C-RET
                 PERFORM TRT-ERREUR

              END-IF

              MOVE WS-Q-XML-LINE(I)       TO ZCOM-Q-XML-LINE(I)
              MOVE WS-Q-XML-INDT(I)       TO ZCOM-Q-XML-INDT(I)
              MOVE WS-L-XML-LINE(I)       TO ZCOM-L-XML-LINE(I)
           END-PERFORM

           .


      *--------------------*
       INIT-DONNEES-TRAVAIL.
      *--------------------*

      D    DISPLAY '=> MFUSXL00 : INIT-DONNEES-TRAVAIL'

           MOVE ZEROS                 TO     WS-Q-XML
           MOVE SPACE                 TO     WS-L-XML
           MOVE ZEROS                 TO     WS-Q-INDT-INIT
           MOVE ZEROS                 TO     WS-Q-INDT-UNIT
           MOVE ZEROS                 TO     WS-Q-XML-TAB

           MOVE ZEROS                 TO     WS-N-XML-DCOP
           MOVE ZEROS                 TO     WS-Q-XML-DCOP-INDT

           MOVE ZEROS                 TO     WS-Q-XML-BUFFER-IMBR
           MOVE ZEROS                 TO     WS-Q-XML-BUFFER
           MOVE SPACE                 TO     WS-L-XML-BUFFER

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > 1000
              MOVE ZERO               TO      WS-Q-XML-LINE     (I)
              MOVE ZERO               TO      WS-Q-XML-INDT     (I)
              MOVE SPACE              TO      WS-L-XML-LINE     (I)
              MOVE SPACE              TO      WS-B-XML-TAG-DECL (I)
              MOVE ZERO               TO      WS-Q-XML-TAG-IMBR (I)
              MOVE ZERO               TO      WS-Q-XML-TAG-NAME (I)
              MOVE ZERO               TO      WS-N-XML-TAG-NAME (I)
              MOVE SPACE              TO      WS-L-XML-TAG-NAME (I)
              MOVE ZERO               TO      WS-N-XML-TAG-ATTR (I)
              MOVE ZERO               TO      WS-Q-XML-TAG-ATTR (I)
              MOVE SPACE              TO      WS-L-XML-TAG-ATTR (I)
              MOVE SPACE              TO      WS-B-XML-TAG-OUVR (I)
              MOVE SPACE              TO      WS-B-XML-TAG-FERM (I)
              MOVE SPACE              TO      WS-B-XML-TAG-CMPLX(I)
              MOVE SPACE              TO      WS-B-XML-TAG-SMPLE(I)
           END-PERFORM

           MOVE ZERO                  TO      WS-Q-NIVEAU-IMBR

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > 500
              MOVE ZERO               TO      WS-N-NIVEAU-IMBR  (I)
           END-PERFORM
           .

      *---------*
       PARSE-XML.
      *---------*

      *    La fonction permet aussi de valoriser les caractèristques
      *    de la ligne/balise

      D    DISPLAY SPACE
      D    DISPLAY 'WS-Q-XML             : ' WS-Q-XML
      D    DISPLAY SPACE

      *    Initialiser buffer
           MOVE ZEROS                     TO WS-Q-XML-BUFFER
           MOVE SPACE                     TO WS-L-XML-BUFFER
           MOVE ZEROS                     TO WS-Q-XML-BUFFER-IMBR

      *    Initialiser booléens
           SET TAG-OUVR                   TO TRUE
           SET TAG-SMPLE                  TO TRUE
           SET TAG-DEB                    TO TRUE
           SET TAG-NAME                   TO TRUE

           MOVE ZERO                      TO WS-N-XML-TAG-NAME-BUFFER
           MOVE ZERO                      TO WS-Q-XML-TAG-NAME-BUFFER
           MOVE ZERO                      TO WS-N-XML-TAG-ATTR-BUFFER
           MOVE ZERO                      TO WS-Q-XML-TAG-ATTR-BUFFER
           MOVE ZERO                      TO WS-N-XML-TAG-FIN-BUFFER

           MOVE ZERO                      TO WS-N-XML-TAG-NAME-CREER
           MOVE ZERO                      TO WS-Q-XML-TAG-NAME-CREER
           MOVE ZERO                      TO WS-N-XML-TAG-ATTR-CREER
           MOVE ZERO                      TO WS-Q-XML-TAG-ATTR-CREER
           MOVE ZERO                      TO WS-N-XML-TAG-FIN-CREER

      *    Les noms des elements commencent à la position 2
           MOVE 2                         TO WS-N-XML-TAG-NAME-BUFFER


      *    Parcourir tous les caractères du XML généré
           PERFORM VARYING I FROM 1 BY 1
           UNTIL           I > WS-Q-XML

      *       Alimenter taille buffer
              ADD  1                   TO WS-Q-XML-BUFFER
      *       Vérifier dépassement buffer
              PERFORM CTRL-Q-XML-BUFFER
      *       Alimenter buffer
              MOVE WS-L-XML(I:1)       TO WS-L-XML-BUFFER
                                         (WS-Q-XML-BUFFER:1)

      *       Evaluer le caractère en cours
              EVALUATE  WS-L-XML(I:1)

                 WHEN '<'

                    MOVE ZERO          TO WS-Q-XML-TAG-NAME-BUFFER
                    MOVE ZERO          TO WS-Q-XML-TAG-ATTR-BUFFER
                    SET  TAG-DEB       TO TRUE
                    SET  TAG-NAME      TO TRUE

      *          Si '/' Alors verifier si il sert à fermer une balise
                 WHEN '/'
      *                     </
                    IF WS-L-XML(I - 1:1) = '<'
      *                     />
                    OR WS-L-XML(I + 1:1) = '>'
                       SET TAG-FERM       TO TRUE
                    END-IF

                 WHEN '?'
      *                     <?
                    IF WS-L-XML(I - 1:1) = '<'
      *                     ?>
                    OR WS-L-XML(I + 1:1) = '>'
                       MOVE  08                  TO   ZCOM-CODE-RETOUR
                       MOVE  07                  TO   ZCOM-C-RET
                       MOVE 'XML contient déjà une balise déclaration'
                                                 TO   ZCOM-L-C-RET
                       PERFORM TRT-ERREUR
                    END-IF

                 WHEN '>'

                    SET  TAG-FIN       TO TRUE

      *             DISPLAY ' TAGE FERM' WS-C-TAG-FERMANTE
      *             Si Balise fermante ...
                    IF TAG-FERM

      *                     ><./.>
      *                ...et est complexe
                       IF TAG-CMPLX
      *                   décrémenter niveau imbrication
                          SUBTRACT 1 FROM WS-Q-XML-BUFFER-IMBR
      *
      *                   Alimenter tableau XML et vider buffer
                          PERFORM XML-CREER-LINE

      *                   ...<./.>
                       ELSE

      *                   Garder même indentation
      *                   Alimenter tableau XML et vider buffer
                          PERFORM XML-CREER-LINE

                          SET TAG-CMPLX    TO TRUE

                       END-IF

                       SET TAG-OUVR       TO TRUE

                    ELSE

                       MOVE WS-N-XML-TAG-NAME-BUFFER
                         TO WS-N-XML-TAG-NAME-CREER

                       MOVE WS-Q-XML-TAG-NAME-BUFFER
                         TO WS-Q-XML-TAG-NAME-CREER

                       MOVE WS-L-XML-BUFFER(WS-N-XML-TAG-NAME-BUFFER:
                                            WS-Q-XML-TAG-NAME-BUFFER)
                         TO WS-L-XML-TAG-NAME-CREER

                       MOVE WS-N-XML-TAG-ATTR-BUFFER
                         TO WS-N-XML-TAG-ATTR-CREER

                       MOVE WS-Q-XML-TAG-ATTR-BUFFER
                         TO WS-Q-XML-TAG-ATTR-CREER

                       COMPUTE WS-N-XML-TAG-FIN-BUFFER
                             = WS-N-XML-TAG-NAME-BUFFER +
                               WS-Q-XML-TAG-NAME-BUFFER +
                               WS-Q-XML-TAG-ATTR-BUFFER

                       MOVE WS-N-XML-TAG-FIN-BUFFER
                         TO WS-N-XML-TAG-FIN-CREER

                       MOVE 2      TO WS-N-XML-TAG-NAME-BUFFER
                       MOVE 0      TO WS-Q-XML-TAG-NAME-BUFFER
                       MOVE space  TO WS-L-XML-TAG-NAME-BUFFER

                       MOVE 0      TO WS-N-XML-TAG-ATTR-BUFFER
                       MOVE 0      TO WS-Q-XML-TAG-ATTR-BUFFER
                       MOVE space  TO WS-L-XML-TAG-ATTR-BUFFER

                       MOVE ZERO   TO WS-N-XML-TAG-FIN-BUFFER

      *                Vérifier est ce que c'est une balise complexe
      *                <...><
                       IF WS-L-XML(I + 1:1) = '<'
      *                   Marquer comme tag complexe
                          SET TAG-CMPLX    TO TRUE

      *                   Alimenter tableau XML et vider buffer
                          PERFORM XML-CREER-LINE

      *                   Incrementer indentation
                          ADD      1 TO   WS-Q-XML-BUFFER-IMBR

                       ELSE

      *                   Marquer comme tag simple
                          SET TAG-SMPLE  TO TRUE

                       END-IF
                    END-IF

                 WHEN OTHER

                    IF WS-L-XML(I:1) = ' ' AND TAG-DEB

      *                Si on est sur le premier espace après nom balise
                       IF TAG-NAME

                          COMPUTE WS-N-XML-TAG-ATTR-BUFFER =
                                  WS-N-XML-TAG-NAME-BUFFER +
                                  WS-Q-XML-TAG-NAME-BUFFER

                       END-IF

                       SET TAG-ATTR          TO TRUE

                    END-IF

                    IF TAG-DEB

                       IF TAG-NAME
      *                   Incrémenter la longueur NAME
                          ADD   1            TO WS-Q-XML-TAG-NAME-BUFFER
                       ELSE
      *                   Incrémenter la longueur ATTR
                          ADD   1            TO WS-Q-XML-TAG-ATTR-BUFFER
                       END-IF

                    END-IF

              END-EVALUATE

           END-PERFORM

      *    Alimenter tag name pour les balises fermantes complexes
           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              IF   WS-B-XML-TAG-CMPLX(I) = '1'
              AND  WS-B-XML-TAG-FERM (I) = '1'

                 COMPUTE WS-Q-XML-TAG-NAME(I)
                       = WS-Q-XML-LINE(I) - 3

                 MOVE 3
                   TO WS-N-XML-TAG-NAME(I)

                 MOVE WS-L-XML-LINE(I)(WS-N-XML-TAG-NAME(I):
                                       WS-Q-XML-TAG-NAME(I))
                   TO WS-L-XML-TAG-NAME(I)

              END-IF

           END-PERFORM

      *    Alimenter text pour les balises simples
           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              SET TAG-DEB            TO TRUE

              IF WS-B-XML-TAG-SMPLE(I) = '1'

      D          DISPLAY 'SMPLE : ' WS-L-XML-LINE(I)(1:WS-Q-XML-LINE(I))

                 COMPUTE WS-Q-XML-TAG-TEXT(I)
                       = WS-Q-XML-LINE    (I)
                       -(WS-Q-XML-TAG-NAME(I) * 2 )
                       - WS-Q-XML-TAG-ATTR(I)
                       - 5

      D          DISPLAY 'TAILLE TEXT : ' WS-Q-XML-TAG-TEXT(I)

                 COMPUTE WS-N-XML-TAG-TEXT(I)
                       = WS-N-XML-TAG-FIN (I)
                       + 1

      D          DISPLAY 'OFFSET TEXT : ' WS-N-XML-TAG-TEXT(I)

                 MOVE WS-L-XML-LINE(I)(WS-N-XML-TAG-TEXT(I):
                                       WS-Q-XML-TAG-TEXT(I))
                                    TO WS-L-XML-TAG-TEXT(I)

      D          DISPLAY 'TEXT : ' WS-L-XML-TAG-TEXT(I)(1:
      D                            WS-Q-XML-TAG-TEXT(I))

      *          <...>    </...>
      *                     ^
                 COMPUTE WS-N-XML-TAG-DEB2(I)
                       = WS-N-XML-TAG-TEXT(I)
                       + WS-Q-XML-TAG-TEXT(I)
                       + 2

      D          DISPLAY 'DEB2 : ' WS-N-XML-TAG-DEB2(I)


              END-IF

           END-PERFORM

           PERFORM ALIM-BALISES-MERES

      D    display 'PARSE-XML OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *------------------*
       ALIM-BALISES-MERES.
      *------------------*

      *    Alimenter balise mère pour chaque balises

      *    Premièrement alimenter les niveaux imbrications
      *    Commencer à partir de 2 pour sauter racine dont niveau est 0
           PERFORM VARYING L FROM 2 BY 1
           UNTIL L > WS-Q-XML-TAB

      *       indice utiliser pour contourner limitation COBOL
      *       on peut pas utiliser un indice qui element dui tableau
              MOVE WS-Q-XML-TAG-IMBR(L) TO IND

      D       DISPLAY 'niveau imbrc de la ligne lue : ' IND

              IF WS-B-XML-TAG-CMPLX(L) = '1'

                 IF WS-B-XML-TAG-FERM (L) = '1'

                    MOVE ZERO                 TO WS-N-NIVEAU-IMBR(IND)

                 END-IF

                 IF WS-B-XML-TAG-OUVR (L) = '1'

                    MOVE L                    TO WS-N-NIVEAU-IMBR(IND)

                 END-IF

              END-IF


      *       Initialiser numéro balise mère
              MOVE ZERO                  TO WS-N-XML-TAG-MERE (L)

      *       Si premier niveau imbr => element racine
              IF   WS-Q-XML-TAG-IMBR(L)  =  1

                 MOVE 1
                   TO WS-N-XML-TAG-MERE (L)

              END-IF

      *       Si deuxieme ou plus
              IF   WS-Q-XML-TAG-IMBR(L)  >  1

                 MOVE WS-N-NIVEAU-IMBR(IND - 1)
                   TO WS-N-XML-TAG-MERE (L)

              END-IF

              MOVE WS-N-XML-TAG-MERE (L) TO IND2

      D       DISPLAY 'Tag           : ' WS-L-XML-TAG-NAME   (L)
      D                                  (1:WS-Q-XML-TAG-NAME(L)) ' '
      D       DISPLAY 'Ligne         : ' L                        ' '
      D       DISPLAY 'Ouvr          : ' WS-B-XML-TAG-OUVR   (L)  ' '
      D       DISPLAY 'Ferm          : ' WS-B-XML-TAG-FERM   (L)  ' '
      D       DISPLAY 'Niveau imbr   : ' IND
      D       DISPLAY 'Ligne tag mere: ' IND2
      D       DISPLAY 'Tag mere      : ' WS-L-XML-TAG-NAME
      D                                  (IND2)
      D                                  (1:WS-Q-XML-TAG-NAME(IND2))
      D
      D       DISPLAY SPACE
      D       DISPLAY SPACE

           END-PERFORM

           .

      *--------*
       FCT-VIDE.
      *--------*

      *    Fonction pour enlever les attributs et elements vides

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

MCHA  D       DISPLAY 'WS-L-XML-TAG-NAME (MCH):'
MCHA  D                WS-L-XML-TAG-NAME(I)
MCHA          IF (WS-L-XML-TAG-NAME(I) NOT = 'R-Title' AND 'FirstName')
MCHA  D       DISPLAY 'WS-L-XML-TAG-NAME (MCH):'
MCHA  D                WS-L-XML-TAG-NAME(I)
              IF  WS-B-XML-TAG-SMPLE(I) = '1'

                 IF WS-L-XML-TAG-TEXT(I)(1:WS-Q-XML-TAG-TEXT(I))
                                           = SPACE
      D              DISPLAY 'FCT-VIDE SMPLE: ' WS-L-XML-LINE(I)(1:
      D                                   WS-Q-XML-LINE(I))

      *             stocker balise mère pour y brancher
                    MOVE WS-N-XML-TAG-MERE(I) TO J


      *             Enlever ligne vide
                    PERFORM VARYING K FROM I BY 1
                    UNTIL K = WS-Q-XML-TAB

                       MOVE WS-G-XML-TAB(K + 1) tO WS-G-XML-TAB(K)

                    END-PERFORM

      *             Decrementer taille totale tableau
                    SUBTRACT 1 FROM WS-Q-XML-TAB

      *             Re-calculer les numéros des balises mères
                    PERFORM ALIM-BALISES-MERES

      *             re-traiter l'itération I qui a de nouvelles valeurs
      *             SUBTRACT 1 FROM I
                    MOVE J TO I
                    SUBTRACT 1 FROM I

                 END-IF

      *       balise complexe
              ELSE

      *          balise ouvrant
                 IF  WS-B-XML-TAG-OUVR(I)  = '1'
      *          la ligne n'est pas sujet de découpage
                 AND NOT ((ZCOM-B-DCOP = '1')
                     AND (WS-L-XML-TAG-NAME(I)(1:WS-Q-XML-TAG-NAME(I))
                          = ZCOM-L-DCOP-TAG(1:ZCOM-Q-DCOP-TAG)))

      *            si même niveau d'imbrication que son suivant
                    IF WS-Q-XML-TAG-IMBR(I) = WS-Q-XML-TAG-IMBR(I + 1)

      D                DISPLAY 'FCT-VIDE CMPLX: ' WS-L-XML-LINE(I)(1:
      D                                           WS-Q-XML-LINE(I))
      D                DISPLAY '2/          : ' WS-L-XML-LINE(I + 1)(1:
      D                                         WS-Q-XML-LINE(I + 1))

      *                stocker balise mère pour y brancher
                       MOVE WS-N-XML-TAG-MERE(I) TO J

      *                Enlever ligne ouvrante
                       PERFORM VARYING K FROM I BY 1
                       UNTIL K = WS-Q-XML-TAB

                          MOVE WS-G-XML-TAB(K + 1) tO WS-G-XML-TAB(K)

                       END-PERFORM

      *                Decrementer taille totale tableau
                       SUBTRACT 1 FROM WS-Q-XML-TAB

      *                Enlever ligne fermante
                       PERFORM VARYING K FROM I BY 1
                       UNTIL K = WS-Q-XML-TAB

                          MOVE WS-G-XML-TAB(K + 1) tO WS-G-XML-TAB(K)

                       END-PERFORM

      *                Decrementer taille totale tableau
                       SUBTRACT 1 FROM WS-Q-XML-TAB

      *                Re-calculer les numéros des balises mères
                       PERFORM ALIM-BALISES-MERES

      *                re-traiter I qui a de nouvelles valeurs
                       MOVE J TO I
                       SUBTRACT 1 FROM I

                    END-IF

                 END-IF

              END-IF
SBOU          END-IF

           END-PERFORM

      D    display 'VIDE OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-INDT.
      *--------*

      *    Fonction d'indentation XML

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

      *       Utilisation buffer car on peut pas string sur lui même
              MOVE SPACE TO WS-L-XML-BUFFER

      *       Calcul indetation = indetation initiale +
      *       niveau imbrication * unité d'indentation
      *
              COMPUTE WS-Q-XML-INDT(I)
                    = WS-Q-INDT-INIT       +
                     (WS-Q-XML-TAG-IMBR(I) * WS-Q-INDT-UNIT)

      *       Ajouter indentation seulement si elle est > 0
              IF WS-Q-XML-INDT(I) > 0 AND WS-Q-XML-TAG-IMBR(I) NOT < 0

                 STRING  WS-L-XML-BLANC
                         (1:WS-Q-XML-INDT(I))
                         WS-L-XML-LINE(I)
                         (1:WS-Q-XML-LINE(I))
                 DELIMITED BY SIZE
                 INTO    WS-L-XML-BUFFER

              ELSE

                 STRING  WS-L-XML-LINE(I)
                         (1:WS-Q-XML-LINE(I))
                 DELIMITED BY SIZE
                 INTO    WS-L-XML-BUFFER

              END-IF

      *       Calculer taille totale ligne
              ADD     WS-Q-XML-INDT(I)
              TO      WS-Q-XML-LINE(I)

              MOVE WS-L-XML-BUFFER(1:WS-Q-XML-LINE(I))
              TO   WS-L-XML-LINE(I)

           END-PERFORM

      D    display 'INDT OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-DECL.
      *--------*

      *    Fonction de création déclaration xml

      *    Vérifier dépassement tableau des lignes XML
           ADD 1                   TO WS-Q-XML-TAB
           PERFORM  CTRL-Q-XML-TAB
           SUBTRACT 1            FROM WS-Q-XML-TAB

           MOVE SPACE TO WS-L-XML-BUFFER

           STRING '<?xml version="1.0" encoding="'
                  WS-L-DECL-ENCODING(1:WS-Q-DECL-ENCODING)
                  '"?>'
           DELIMITED BY SIZE
           INTO    WS-L-XML-BUFFER

           COMPUTE WS-Q-XML-BUFFER
                 = 30 + WS-Q-DECL-ENCODING + 3

           PERFORM VARYING I FROM WS-Q-XML-TAB BY -1
           UNTIL I < 1

              MOVE WS-G-XML-TAB(I) TO WS-G-XML-TAB(I + 1)

           END-PERFORM

      *    Ajouter la ligne declaration en tête du tableau
           MOVE '1'                  TO WS-B-XML-TAG-DECL (1)
           MOVE '0'                  TO WS-B-XML-TAG-OUVR (1)
                                        WS-B-XML-TAG-FERM (1)
                                        WS-B-XML-TAG-SMPLE(1)
                                        WS-B-XML-TAG-CMPLX(1)

           MOVE -1                   TO WS-Q-XML-TAG-IMBR (1)

           MOVE 0                    TO WS-Q-XML-TAG-NAME (1)
           MOVE 0                    TO WS-N-XML-TAG-NAME (1)
           MOVE SPACE                TO WS-L-XML-TAG-NAME (1)

           MOVE 0                    TO WS-N-XML-TAG-ATTR (1)
           MOVE 0                    TO WS-Q-XML-TAG-ATTR (1)
           MOVE SPACE                TO WS-L-XML-TAG-ATTR (1)


           MOVE WS-L-XML-BUFFER(1:WS-Q-XML-BUFFER)
                                     TO WS-L-XML-LINE(1)

      *    Calculer taille totale ligne
           MOVE    WS-Q-XML-BUFFER   TO WS-Q-XML-LINE(1)


      *    Incrementer toutes paramètres sortie de numérotation ligne
           ADD 1 TO WS-N-XML-DCOP

      *    Incrementer taille totale du tableau
           ADD 1 TO WS-Q-XML-TAB

      D    display 'DECL OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-NMSP.
      *--------*

      *    Fonction d'ajout des namespace à la racine XML
      *    la racine est à priori dans la première ligne

           MOVE WS-Q-XML-LINE(1) TO WS-Q-XML-BUFFER
           MOVE WS-L-XML-LINE(1) TO WS-L-XML-BUFFER

           MOVE 1                TO WS-Q-XML-BUFFER-ATTR
           MOVE SPACE            TO WS-L-XML-BUFFER-ATTR

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-NMSP


              STRING  ' xmlns:'
                      WS-L-NMSP-PRFX(I)(1:WS-Q-NMSP-PRFX(I))
                      '="'
                      WS-L-NMSP-URN (I)(1:WS-Q-NMSP-URN (I))
                      '"'
              DELIMITED BY SIZE
              INTO    WS-L-XML-BUFFER-ATTR
              POINTER WS-Q-XML-BUFFER-ATTR


      D       DISPLAY 'WS-L-XML-BUFFER-ATTR : '
      D                WS-L-XML-BUFFER-ATTR(1:WS-Q-XML-BUFFER-ATTR)

           END-PERFORM

           MOVE SPACE            TO WS-L-XML-LINE(1)
           MOVE 0                TO WS-Q-XML-LINE(1)

      D    DISPLAY '1/ : ' WS-L-XML-BUFFER(1:WS-N-XML-TAG-FIN(1) - 1)
      D    DISPLAY '2/ : '
      D                WS-L-XML-BUFFER-ATTR(1:WS-Q-XML-BUFFER-ATTR - 1)
      D    DISPLAY '3/ : ' WS-L-XML-BUFFER(WS-N-XML-TAG-FIN(1):1)

           STRING WS-L-XML-BUFFER(1:WS-N-XML-TAG-FIN(1) - 1)
                  WS-L-XML-BUFFER-ATTR(1:WS-Q-XML-BUFFER-ATTR - 1)
                  WS-L-XML-BUFFER(WS-N-XML-TAG-FIN(1):1)
           DELIMITED BY SIZE
           INTO    WS-L-XML-LINE(1)

           COMPUTE WS-Q-XML-LINE(1)
                 = WS-Q-XML-BUFFER
                 + WS-Q-XML-BUFFER-ATTR
                 - 1

           COMPUTE WS-N-XML-TAG-FIN(1)
                 = WS-N-XML-TAG-FIN(1)
                 + WS-Q-XML-BUFFER-ATTR
                 - 1

      D    display 'NMSP OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-XSD.
      *--------*

      *    Fonction d'ajout de schemaLocation
      *    la racine est à priori dans la première ligne

           MOVE WS-Q-XML-LINE(1) TO WS-Q-XML-BUFFER
           MOVE WS-L-XML-LINE(1) TO WS-L-XML-BUFFER

           MOVE 1                TO WS-Q-XML-BUFFER-ATTR
           MOVE SPACE            TO WS-L-XML-BUFFER-ATTR

           STRING  ' xsi:schemaLocation="'
                   WS-L-XSD-URN (1:WS-Q-XSD-URN)
                   '"'
           DELIMITED BY SIZE
           INTO    WS-L-XML-BUFFER-ATTR
           POINTER WS-Q-XML-BUFFER-ATTR


           MOVE SPACE            TO WS-L-XML-LINE(1)
           MOVE 0                TO WS-Q-XML-LINE(1)

           STRING WS-L-XML-BUFFER(1:WS-N-XML-TAG-FIN(1) - 1)
                  WS-L-XML-BUFFER-ATTR(1:WS-Q-XML-BUFFER-ATTR - 1)
                  WS-L-XML-BUFFER(WS-N-XML-TAG-FIN(1):1)
           DELIMITED BY SIZE
           INTO    WS-L-XML-LINE(1)

           COMPUTE WS-Q-XML-LINE(1)
                 = WS-Q-XML-BUFFER
                 + WS-Q-XML-BUFFER-ATTR
                 - 1

           COMPUTE WS-N-XML-TAG-FIN(1)
                 = WS-N-XML-TAG-FIN(1)
                 + WS-Q-XML-BUFFER-ATTR
                 - 1

      D    display 'XSD  OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-RESV.
      *--------*

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

      D       DISPLAY 'FCT-RESV : TAG NAME : ' WS-L-XML-TAG-NAME(I)

              IF WS-L-XML-TAG-NAME(I)(1:WS-Q-RESV-INDICATIF)
               = WS-L-RESV-INDICATIF(1:WS-Q-RESV-INDICATIF)

                 MOVE WS-L-XML-LINE(I) TO WS-L-XML-BUFFER
                 MOVE WS-Q-XML-LINE(I) TO WS-Q-XML-BUFFER

                 MOVE SPACE            TO WS-L-XML-LINE(I)
                 MOVE ZEROS            TO WS-Q-XML-LINE(I)

                 COMPUTE  WS-Q-LENGTH
                       =  WS-Q-XML-BUFFER
                       - (WS-Q-RESV-INDICATIF + WS-N-XML-TAG-NAME(I))
                       +  1


                 STRING WS-L-XML-BUFFER(1:WS-N-XML-TAG-NAME(I) - 1)
                        WS-L-XML-BUFFER(WS-N-XML-TAG-NAME(I) +
                                        WS-Q-RESV-INDICATIF:
                                        WS-Q-LENGTH)
                 DELIMITED BY SIZE
                 INTO WS-L-XML-LINE(I)

                 COMPUTE WS-N-XML-TAG-FIN(I)
                       = WS-N-XML-TAG-FIN(I) - WS-Q-RESV-INDICATIF

                 COMPUTE WS-Q-XML-LINE(I)
                       = WS-Q-XML-BUFFER - WS-Q-RESV-INDICATIF

                 IF WS-B-XML-TAG-SMPLE(I) = '1'

                    COMPUTE WS-N-XML-TAG-DEB2(I)
                          = WS-N-XML-TAG-DEB2(I) - WS-Q-RESV-INDICATIF

                    MOVE WS-L-XML-LINE(I) TO WS-L-XML-BUFFER
                    MOVE WS-Q-XML-LINE(I) TO WS-Q-XML-BUFFER
                    MOVE SPACE            TO WS-L-XML-LINE(I)

                    COMPUTE WS-Q-OFFSET
                          = WS-Q-XML-BUFFER
      *                   - 1
                          - WS-Q-XML-TAG-NAME(I)

                    COMPUTE WS-Q-LENGTH
                          = WS-Q-XML-TAG-NAME(I)
                          + 1
                          - WS-Q-RESV-INDICATIF

                    STRING WS-L-XML-BUFFER(1:WS-Q-OFFSET - 1)
                           WS-L-XML-BUFFER(WS-Q-OFFSET
                                           +WS-Q-RESV-INDICATIF
                                           :WS-Q-LENGTH)
                    DELIMITED BY SIZE
                    INTO WS-L-XML-LINE(I)


                 END-IF

              END-IF

           END-PERFORM
      D    display 'RESV OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-ATTR.
      *--------*

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              IF  WS-L-XML-TAG-NAME(I)(1:5) = 'tech-'
              AND WS-L-XML-TAG-NAME(I)(6:5) = 'attr-'

      D          DISPLAY 'FCT-ATTR : ' WS-L-XML-LINE(I)(1:
      D                                WS-Q-XML-LINE(I))
      D
      D          DISPLAY 'USE      : ' WS-L-XML-TAG-NAME(I)(11:4)

                 MOVE ZERO TO J

                 MOVE 0                TO WS-Q-XML-BUFFER-ATTR
                 MOVE SPACE            TO WS-L-XML-BUFFER-ATTR

                 COMPUTE WS-Q-XML-BUFFER-ATTR
                       = WS-Q-XML-TAG-NAME(I)- 14

                 MOVE WS-L-XML-TAG-NAME(I)(15:WS-Q-XML-BUFFER-ATTR)
                   TO WS-L-XML-BUFFER-ATTR

      D          DISPLAY 'ATTR     : ' WS-L-XML-BUFFER-ATTR(1:
      D                                WS-Q-XML-BUFFER-ATTR)
      D          DISPLAY 'TEXT     : ' WS-L-XML-TAG-TEXT(I)(1:
      D                                WS-Q-XML-TAG-TEXT(I))

                 MOVE WS-N-XML-TAG-MERE(I) TO J

      D          DISPLAY 'MERE     : ' WS-L-XML-LINE(J)(1:
      D                                WS-Q-XML-LINE(J))

      D          DISPLAY 'TAG-FIN  : ' WS-N-XML-TAG-FIN(J)

                 MOVE WS-Q-XML-LINE(J) TO WS-Q-XML-BUFFER
                 MOVE WS-L-XML-LINE(J) TO WS-L-XML-BUFFER

                 MOVE ZERO             TO WS-Q-XML-LINE(J)
                 MOVE SPACE            TO WS-L-XML-LINE(J)

                 COMPUTE WS-Q-XML-LINE(J)
                 = WS-Q-XML-BUFFER
                 + WS-Q-XML-BUFFER-ATTR
                 + 4
                 + WS-Q-XML-TAG-TEXT(I)

      D          DISPLAY ' taille av  : ' WS-Q-XML-BUFFER
      D          DISPLAY ' taille ap  : ' WS-Q-XML-LINE(J)

                 STRING WS-L-XML-BUFFER(1:WS-N-XML-TAG-FIN(J) - 1)
                  ' '
                  WS-L-XML-BUFFER-ATTR(1:WS-Q-XML-BUFFER-ATTR)
                  '="'
                  WS-L-XML-TAG-TEXT(I)(1:WS-Q-XML-TAG-TEXT(I))
                  '"'
                  WS-L-XML-BUFFER(WS-N-XML-TAG-FIN(J):)
      *                           WS-Q-XML-BUFFER - WS-N-XML-TAG-FIN(J)
      *                           +1)

                 DELIMITED BY SIZE
                 INTO    WS-L-XML-LINE(J)


                 COMPUTE WS-N-XML-TAG-FIN(J)
                 = WS-N-XML-TAG-FIN(J)
                 + WS-Q-XML-BUFFER-ATTR
                 + 4
                 + WS-Q-XML-TAG-TEXT(I)

      D          DISPLAY 'MERE+ATTR: ' WS-L-XML-LINE(J)(1:
      D                                WS-Q-XML-LINE(J))

      *          Enlever ligne balise technique
                 PERFORM VARYING K FROM I BY 1
                 UNTIL K = WS-Q-XML-TAB

                    MOVE WS-G-XML-TAB(K + 1) tO WS-G-XML-TAB(K)

                 END-PERFORM

      *          Decrementer taille totale tableau
                 SUBTRACT 1 FROM WS-Q-XML-TAB

      *          Re-calculer les numéros des balises mères
                 PERFORM ALIM-BALISES-MERES

      *          re-traiter l'itération I qui a de nouvelles valeurs
                 SUBTRACT 1 FROM I

              END-IF

           END-PERFORM

      D    display 'ATTR OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab

      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))

      D    end-perform
           .


      *--------*
       FCT-TEXT.
      *--------*

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              IF  WS-L-XML-TAG-NAME(I)(1:WS-Q-XML-TAG-NAME(I))
                                            = 'tech-text'

      D          DISPLAY 'FCT-TEXT : ' WS-L-XML-LINE(I)(1:
      D                                WS-Q-XML-LINE(I))

                 MOVE ZERO TO J

      D          DISPLAY 'TEXT     : ' WS-L-XML-TAG-TEXT(I)(1:
      D                                WS-Q-XML-TAG-TEXT(I))

                 MOVE WS-N-XML-TAG-MERE(I) TO J

      D          DISPLAY 'MERE     : ' WS-L-XML-LINE(J)(1:
      D                                WS-Q-XML-LINE(J))

      D          DISPLAY 'TAG-FIN  : ' WS-N-XML-TAG-FIN(J)

                 MOVE WS-Q-XML-LINE(J) TO WS-Q-XML-BUFFER
                 MOVE WS-L-XML-LINE(J) TO WS-L-XML-BUFFER

                 MOVE ZERO             TO WS-Q-XML-LINE(J)
                 MOVE SPACE            TO WS-L-XML-LINE(J)

                 COMPUTE WS-Q-XML-LINE(J)
                 = WS-Q-XML-BUFFER
                 + WS-Q-XML-TAG-TEXT(I)
                 + WS-Q-XML-LINE(I + 1)

      D          DISPLAY ' taille av  : ' WS-Q-XML-BUFFER
      D          DISPLAY ' taille ap  : ' WS-Q-XML-LINE(J)

                 STRING WS-L-XML-BUFFER(1:WS-Q-XML-BUFFER)
                        WS-L-XML-TAG-TEXT(I)(1:WS-Q-XML-TAG-TEXT(I))
                        WS-L-XML-LINE(I + 1)(1:WS-Q-XML-LINE(I + 1))

                 DELIMITED BY SIZE
                 INTO    WS-L-XML-LINE(J)

      D          DISPLAY 'fusion   : ' WS-L-XML-LINE(J)(1:
      D                                WS-Q-XML-LINE(J))


                 COMPUTE WS-N-XML-TAG-TEXT(J)
                       = WS-N-XML-TAG-FIN (J)
                       + 1

                 COMPUTE WS-Q-XML-TAG-TEXT(J)
                       = WS-Q-XML-TAG-TEXT(I)

                 COMPUTE WS-N-XML-TAG-DEB2(J)
                       = WS-N-XML-TAG-TEXT(J)
                       + WS-Q-XML-TAG-TEXT(J)
                       + 2


                 MOVE '0'            TO WS-B-XML-TAG-CMPLX(J)
                 MOVE '1'            TO WS-B-XML-TAG-SMPLE(J)
                 MOVE '1'            TO WS-B-XML-TAG-OUVR(J)
                 MOVE '1'            TO WS-B-XML-TAG-FERM(J)

      D          DISPLAY 'DEB2     : ' WS-N-XML-TAG-DEB2(J)

      *          Enlever ligne balise technique
                 PERFORM VARYING K FROM I BY 1
                 UNTIL K = WS-Q-XML-TAB

                    MOVE WS-G-XML-TAB(K + 1) tO WS-G-XML-TAB(K)

                 END-PERFORM

      *          Decrementer taille totale tableau
                 SUBTRACT 1 FROM WS-Q-XML-TAB

      *          Enlever fermeture complexe
                 PERFORM VARYING K FROM I BY 1
                 UNTIL K = WS-Q-XML-TAB

                    MOVE WS-G-XML-TAB(K + 1) tO WS-G-XML-TAB(K)

                 END-PERFORM

      *          Decrementer taille totale tableau
                 SUBTRACT 1 FROM WS-Q-XML-TAB

      *          Re-calculer les numéros des balises mères
                 PERFORM ALIM-BALISES-MERES

      *          re-traiter l'itération I qui a de nouvelles valeurs
                 SUBTRACT 1 FROM I

              END-IF

           END-PERFORM

      D    display 'TEXT OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .

      *--------*
       FCT-PRFX.
      *--------*

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              MOVE WS-L-PRFX-DFLT            TO WS-L-XML-BUFFER-PRFX
              MOVE WS-Q-PRFX-DFLT            TO WS-Q-XML-BUFFER-PRFX

              PERFORM VARYING J FROM 1 BY 1
              UNTIL J > WS-Q-PRFX

                 IF WS-L-XML-TAG-NAME(I)(1:WS-Q-XML-TAG-NAME(I))
                  = WS-L-PRFX-TAG    (J)(1:WS-Q-PRFX-TAG(J))

                    MOVE WS-L-PRFX-PARM      TO WS-L-XML-BUFFER-PRFX
                    MOVE WS-Q-PRFX-PARM      TO WS-Q-XML-BUFFER-PRFX

                 END-IF

              END-PERFORM

      D       DISPLAY 'PREFIX : ' WS-L-XML-BUFFER-PRFX(1:
      D                           WS-Q-XML-BUFFER-PRFX)

      D       DISPLAY 'LINE   : ' WS-L-XML-LINE(I)(1:
      D                           WS-Q-XML-LINE(I))

              MOVE WS-Q-XML-LINE(I) TO WS-Q-XML-BUFFER
              MOVE WS-L-XML-LINE(I) TO WS-L-XML-BUFFER

              MOVE ZERO             TO WS-Q-XML-LINE(I)
              MOVE SPACE            TO WS-L-XML-LINE(I)

              COMPUTE WS-Q-XML-LINE(I)
                    = WS-Q-XML-BUFFER-PRFX
                    + 1
                    + WS-Q-XML-BUFFER

      D       DISPLAY 'DEB1  ?: ' WS-N-XML-TAG-NAME(I)
      D       DISPLAY 'TAILLE1 AV: ' WS-Q-XML-BUFFER
      D       DISPLAY 'TAILLE1 AP: ' WS-Q-XML-LINE(I)
      D       DISPLAY '1/' WS-L-XML-BUFFER(1:WS-N-XML-TAG-NAME(I) - 1)
      D       DISPLAY '2/' WS-L-XML-BUFFER(WS-N-XML-TAG-NAME(I):
      D                                    WS-Q-XML-BUFFER -
      D                                    WS-N-XML-TAG-NAME(I))
              STRING  WS-L-XML-BUFFER(1:WS-N-XML-TAG-NAME(I) - 1)
                      WS-L-XML-BUFFER-PRFX(1:WS-Q-XML-BUFFER-PRFX)
                      ':'
                      WS-L-XML-BUFFER(WS-N-XML-TAG-NAME(I):)
              DELIMITED BY SIZE
              INTO WS-L-XML-LINE(I)

              COMPUTE WS-N-XML-TAG-DEB2(I)
                    = WS-N-XML-TAG-DEB2(I)
                    + 1
                    + WS-Q-XML-BUFFER-PRFX

              COMPUTE WS-N-XML-TAG-NAME(I)
                    = WS-N-XML-TAG-NAME(I)
                    + WS-Q-XML-BUFFER-PRFX

      D       DISPLAY 'SMPLE ?: ' WS-B-XML-TAG-SMPLE(I)
              IF WS-B-XML-TAG-SMPLE(I) = '1'

      D       DISPLAY 'LINE2  : ' WS-L-XML-LINE(I)(1:
      D                           WS-Q-XML-LINE(I))

      D          DISPLAY 'DEB2  ?: ' WS-N-XML-TAG-DEB2(I)

                 MOVE WS-Q-XML-LINE(I) TO WS-Q-XML-BUFFER
                 MOVE WS-L-XML-LINE(I) TO WS-L-XML-BUFFER

                 MOVE ZERO             TO WS-Q-XML-LINE(I)
                 MOVE SPACE            TO WS-L-XML-LINE(I)

                 COMPUTE WS-Q-XML-LINE(I)
                       = WS-Q-XML-BUFFER-PRFX
                       + 1
                       + WS-Q-XML-BUFFER

      D          DISPLAY 'TAILLE2 AV: ' WS-Q-XML-BUFFER
      D          DISPLAY 'TAILLE2 AP: ' WS-Q-XML-LINE(I)
      D          DISPLAY '1/' WS-L-XML-BUFFER(1:WS-N-XML-TAG-DEB2(I)- 1)
      D          DISPLAY '2/' WS-L-XML-BUFFER(WS-N-XML-TAG-DEB2(I):
      D                                    WS-Q-XML-BUFFER -
      D                                    WS-N-XML-TAG-DEB2(I))

                 STRING  WS-L-XML-BUFFER(1:WS-N-XML-TAG-DEB2(I) - 1)
                         WS-L-XML-BUFFER-PRFX(1:WS-Q-XML-BUFFER-PRFX)
                         ':'
                         WS-L-XML-BUFFER(WS-N-XML-TAG-DEB2(I):)
                 DELIMITED BY SIZE
                 INTO WS-L-XML-LINE(I)

                 COMPUTE WS-N-XML-TAG-DEB2(I)
                       = WS-N-XML-TAG-DEB2(I)
                       + 1
                       + WS-Q-XML-BUFFER-PRFX

              END-IF


           END-PERFORM

      D    display 'PRFX OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .


      *--------*
       FCT-DCOP.
      *--------*

      *    Fonction de decoupage XML pour permettre
      *    l'imbrication dynamique des balises.
      *    Chercher les mots de la liste dans l'ordre

      *    Initialisation
           MOVE ZERO                        TO WS-N-XML-DCOP
           MOVE ZERO                        TO WS-Q-XML-DCOP-INDT

      D    DISPLAY 'WS-L-DCOP-TAG : ' WS-L-DCOP-TAG
      D    DISPLAY 'WS-Q-DCOP-TAG : ' WS-Q-DCOP-TAG

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > WS-Q-XML-TAB

              IF   WS-B-XML-TAG-CMPLX(I) = '1'
              AND  WS-B-XML-TAG-FERM (I) = '1'

      D          DISPLAY 'TAG CURR : '
      D                  WS-L-XML-TAG-NAME(I)(1:WS-Q-XML-TAG-NAME(I))

      D          DISPLAY 'TAG RECH : '
      D                  WS-L-DCOP-TAG(1:WS-Q-DCOP-TAG)

                 IF WS-L-XML-TAG-NAME(I)(1:WS-Q-XML-TAG-NAME(I))
                  = WS-L-DCOP-TAG(1:WS-Q-DCOP-TAG)

                    COMPUTE WS-N-XML-DCOP = I - 1

                    IF ZCOM-B-INDT = '1'
                       COMPUTE WS-Q-XML-DCOP-INDT
                         = ZCOM-Q-INDT-INIT + ZCOM-Q-INDT-UNIT +
                          (ZCOM-Q-INDT-UNIT * WS-Q-XML-TAG-IMBR(I))

                    END-IF

                 END-IF

              END-IF

           END-PERFORM

           IF WS-N-XML-DCOP = 0
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  19                    TO   ZCOM-C-RET
              MOVE 'Balise de découpage introuvable'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR

           END-IF

      D    display 'DCOP OUT'
      D    perform varying i from 1 by 1
      D    until i > ws-q-xml-tab
      D
      D       display  ws-l-xml-line(i)(1:ws-q-xml-line(i))
      D
      D    end-perform
           .


      *---------------*
       XML-CREER-LINE.
      *---------------*


      *    Incrementer ligne tableau
           ADD 1 TO WS-Q-XML-TAB

      *    Vérifier dépassement tableau des lignes XML
           PERFORM  CTRL-Q-XML-TAB

           MOVE '0'       TO WS-B-XML-TAG-DECL (WS-Q-XML-TAB)
                             WS-B-XML-TAG-OUVR (WS-Q-XML-TAB)
                             WS-B-XML-TAG-FERM (WS-Q-XML-TAB)
                             WS-B-XML-TAG-SMPLE(WS-Q-XML-TAB)
                             WS-B-XML-TAG-CMPLX(WS-Q-XML-TAB)

           IF TAG-OUVR
              MOVE '1'    TO WS-B-XML-TAG-OUVR(WS-Q-XML-TAB)
           END-IF
           IF TAG-FERM
              MOVE '1'    TO WS-B-XML-TAG-FERM(WS-Q-XML-TAB)
           END-IF
      *    Si balise simple la ligne contient TAG-OUVR et FERM
           IF TAG-SMPLE
              MOVE '1'    TO WS-B-XML-TAG-SMPLE(WS-Q-XML-TAB)
              MOVE '1'    TO WS-B-XML-TAG-OUVR(WS-Q-XML-TAB)
              MOVE '1'    TO WS-B-XML-TAG-FERM(WS-Q-XML-TAB)
           END-IF
           IF TAG-CMPLX
              MOVE '1'    TO WS-B-XML-TAG-CMPLX(WS-Q-XML-TAB)
           END-IF

           MOVE WS-Q-XML-BUFFER-IMBR TO
                                        WS-Q-XML-TAG-IMBR(WS-Q-XML-TAB)
           MOVE WS-Q-XML-TAG-NAME-CREER
                                     TO WS-Q-XML-TAG-NAME(WS-Q-XML-TAB)
           MOVE WS-N-XML-TAG-NAME-CREER
                                     TO WS-N-XML-TAG-NAME(WS-Q-XML-TAB)
           MOVE WS-L-XML-TAG-NAME-CREER
                                     TO WS-L-XML-TAG-NAME(WS-Q-XML-TAB)
           MOVE WS-N-XML-TAG-ATTR-CREER
                                     TO WS-N-XML-TAG-ATTR(WS-Q-XML-TAB)
           MOVE WS-Q-XML-TAG-ATTR-CREER
                                     TO WS-Q-XML-TAG-ATTR(WS-Q-XML-TAB)
           MOVE WS-L-XML-TAG-ATTR-CREER
                                     TO WS-L-XML-TAG-ATTR(WS-Q-XML-TAB)
           MOVE WS-N-XML-TAG-FIN-CREER
                                     TO WS-N-XML-TAG-FIN (WS-Q-XML-TAB)

           STRING    WS-L-XML-BUFFER
                  (1:WS-Q-XML-BUFFER)
           DELIMITED BY SIZE
           INTO      WS-L-XML-LINE(WS-Q-XML-TAB)

      *    Calculer taille totale ligne
           MOVE    WS-Q-XML-BUFFER   TO WS-Q-XML-LINE(WS-Q-XML-TAB)

      D    DISPLAY SPACE
      D    DISPLAY 'IMBR -' WS-Q-XML-TAG-IMBR (WS-Q-XML-TAB) ' - '
      D            'DECL -' WS-B-XML-TAG-DECL (WS-Q-XML-TAB) ' - '
      D            'OUVR -' WS-B-XML-TAG-OUVR (WS-Q-XML-TAB) ' - '
      D            'FERM -' WS-B-XML-TAG-FERM (WS-Q-XML-TAB) ' - '
      D            'SMPLE-' WS-B-XML-TAG-SMPLE(WS-Q-XML-TAB) ' - '
      D            'CMPLX-' WS-B-XML-TAG-CMPLX(WS-Q-XML-TAB) ' - '
      D            'Q    -' WS-Q-XML-BUFFER ' - '
      D    DISPLAY '----------------------------------------------'
      D    DISPLAY 'Q-NAME-' WS-Q-XML-TAG-NAME(WS-Q-XML-TAB)  ' - '
      D            'N-NAME-' WS-N-XML-TAG-NAME(WS-Q-XML-TAB)  ' - '
      D            'L-NAME-' WS-L-XML-TAG-NAME(WS-Q-XML-TAB)  ' - '
      D            'N-ATTR-' WS-N-XML-TAG-ATTR(WS-Q-XML-TAB)  ' - '
      D            'Q-ATTR-' WS-Q-XML-TAG-ATTR(WS-Q-XML-TAB)  ' - '
      *            'L-ATTR-' WS-L-XML-TAG-ATTR(WS-Q-XML-TAB)  ' - '
      D            'N-FIN -' WS-N-XML-TAG-FIN (WS-Q-XML-TAB)  ' - '
      D    DISPLAY '----------------------------------------------'
      D    DISPLAY 'L    - ' WS-L-XML-BUFFER(1:WS-Q-XML-BUFFER)
      D    DISPLAY SPACE



      *    Vider Buffer
           MOVE ZERO  TO WS-Q-XML-BUFFER
           MOVE SPACE TO WS-L-XML-BUFFER

           MOVE ZERO  TO WS-N-XML-TAG-NAME-CREER
           MOVE ZERO  TO WS-Q-XML-TAG-NAME-CREER
           MOVE SPACE TO WS-L-XML-TAG-NAME-CREER
           MOVE ZERO  TO WS-N-XML-TAG-ATTR-CREER
           MOVE ZERO  TO WS-Q-XML-TAG-ATTR-CREER
           MOVE SPACE TO WS-L-XML-TAG-ATTR-CREER
           MOVE ZERO  TO WS-N-XML-TAG-FIN-CREER

           .

      *----------------------*
       CTRL-Q-XML-BUFFER.
      *----------------------*

           IF WS-Q-XML-BUFFER      > 1000
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  04                    TO   ZCOM-C-RET
              MOVE 'Taille du buffer des lignes xml dépassée'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF
           .

      *--------------*
       CTRL-Q-XML-TAB.
      *--------------*

           IF WS-Q-XML-TAB  > 1000
              MOVE  08                    TO   ZCOM-CODE-RETOUR
              MOVE  05                    TO   ZCOM-C-RET
              MOVE 'Taille du tableau des lignes indentées dépassée'
                                          TO   ZCOM-L-C-RET
              PERFORM TRT-ERREUR
           END-IF
           .

      *--------------------*
       AFFICH-ZCOM-FONC-IN.
      *--------------------*

      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!           ZONE ENTREE MFUSXL00           !'
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-B-INDT          : ' ZCOM-B-INDT
      D    DISPLAY '! ZCOM-B-NMSP          : ' ZCOM-B-NMSP
      D    DISPLAY '! ZCOM-B-XSD           : ' ZCOM-B-XSD
      D    DISPLAY '! ZCOM-B-PRFX          : ' ZCOM-B-PRFX
      D    DISPLAY '! ZCOM-B-ATTR          : ' ZCOM-B-ATTR
      D    DISPLAY '! ZCOM-B-TEXT          : ' ZCOM-B-TEXT
      D    DISPLAY '! ZCOM-B-VIDE          : ' ZCOM-B-VIDE
      D    DISPLAY '! ZCOM-B-RESV          : ' ZCOM-B-RESV
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-INDT-INIT     : ' ZCOM-Q-INDT-INIT
      D    DISPLAY '! ZCOM-Q-INDT-UNIT     : ' ZCOM-Q-INDT-UNIT
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-NMSP          : ' ZCOM-Q-NMSP
      D    DISPLAY '! ZCOM-Q-NMSP-PRFX     : ' ZCOM-Q-NMSP-PRFX    (1)
      D    DISPLAY '! ZCOM-L-NMSP-PRFX     : ' ZCOM-L-NMSP-PRFX    (1)
      D    DISPLAY '! ZCOM-Q-NMSP-URN      : ' ZCOM-Q-NMSP-URN     (1)
      D    DISPLAY '! ZCOM-L-NMSP-URN      : ' ZCOM-L-NMSP-URN     (1)
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-XSD-URN       : ' ZCOM-Q-XSD-URN
      D    DISPLAY '! ZCOM-L-XSD-URN       : ' ZCOM-L-XSD-URN
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-PRFX-DFLT     : ' ZCOM-Q-PRFX-DFLT
      D    DISPLAY '! ZCOM-L-PRFX-DFLT     : ' ZCOM-L-PRFX-DFLT
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-C-ATTR-TYPE     : ' ZCOM-C-ATTR-TYPE
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-C-VIDE-TYPE     : ' ZCOM-C-VIDE-TYPE
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-RESV-INDICATIF: ' ZCOM-Q-RESV-INDICATIF
      D    DISPLAY '! ZCOM-L-RESV-INDICATIF: ' ZCOM-L-RESV-INDICATIF
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-XML-BRUTE     : ' ZCOM-Q-XML-BRUTE
      D    DISPLAY '! ZCOM-L-XML-BRUTE 1-80: ' ZCOM-L-XML-BRUTE(1:80)
      D    DISPLAY '+------------------------------------------+'
           .

      *--------------------*
       AFFICH-ZCOM-FONC-OUT.
      *--------------------*

      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '!           ZONE SORTIE MFUSXL00           !'
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-CODE-RETOUR     : ' ZCOM-CODE-RETOUR
      D    DISPLAY '! ZCOM-C-RET           : ' ZCOM-C-RET
      D    DISPLAY '! ZCOM-L-C-RET         : ' ZCOM-L-C-RET
      D    DISPLAY '+------------------------------------------+'
      D    DISPLAY '! ZCOM-Q-XML-TAB       : ' ZCOM-Q-XML-TAB
      D    DISPLAY '+--------+'
      D    DISPLAY '! ZCOM-Q-XML-LINE      : ' ZCOM-Q-XML-LINE(1)
      D    DISPLAY '! ZCOM-Q-XML-INDT      : ' ZCOM-Q-XML-INDT(1)
      D    DISPLAY '! ZCOM-L-XML-LINE      : ' ZCOM-L-XML-LINE(1)
      D    DISPLAY '+--------+'
      D    DISPLAY '! ZCOM-Q-XML-INDT-LINE : ' ZCOM-Q-XML-LINE(2)
      D    DISPLAY '! ZCOM-Q-XML-INDT-INDT : ' ZCOM-Q-XML-INDT(2)
      D    DISPLAY '! ZCOM-L-XML-INDT-LINE : ' ZCOM-L-XML-LINE(2)
      D    DISPLAY '+--------+'
      D    DISPLAY '! ZCOM-Q-XML-INDT-LINE : ' ZCOM-Q-XML-LINE(3)
      D    DISPLAY '! ZCOM-Q-XML-INDT-INDT : ' ZCOM-Q-XML-INDT(3)
      D    DISPLAY '! ZCOM-L-XML-INDT-LINE : ' ZCOM-L-XML-LINE(3)
      D    DISPLAY '+------------------------------------------+'
           .

      *=====================
       INIT-ZCOM-FONC-OUT.
      *=====================
      *    Initialisation des données
      *    en sortie de module ZCOM-FONC-OUT

           MOVE ZERO                  TO      ZCOM-CODE-RETOUR
           MOVE ZERO                  TO      ZCOM-C-RET
           MOVE SPACE                 TO      ZCOM-L-C-RET

           MOVE ZERO                  TO      ZCOM-Q-XML-TAB

           PERFORM VARYING I FROM 1 BY 1
size  *    UNTIL I > 1000
size       UNTIL I > 120
              MOVE ZERO               TO      ZCOM-Q-XML-LINE(I)
              MOVE ZERO               TO      ZCOM-Q-XML-INDT(I)
              MOVE SPACE              TO      ZCOM-L-XML-LINE(I)
           END-PERFORM
           .

      *=====================
       TRT-ERREUR.
      *=====================

      D    DISPLAY '=> MFUSXL00 : FIN ANORMALE'
           PERFORM FIN-PROGRAMME
           .


      *================
       FIN-PROGRAMME.
      *================

      D    PERFORM AFFICH-ZCOM-FONC-OUT

           PERFORM SQ-FIN-MODULE
           GOBACK
           .

      *
      * TRAITEMENTS COMMUNS DES MODULES FONCTIONNELS
       COPY CANAAMOD.
      *
      * TRAITEMENTS COMMUNS DES COMPOSANTS FONCTIONNELS
       COPY CANAAFCT.
      *

       END PROGRAM MFUSXL00.
