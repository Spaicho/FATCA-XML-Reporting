      **********************************
      *                                *
      *   Structure de données FATCA   *
      *                                *
      **********************************

      *   *******************************
      *   * Indicateurs de multiplicité *
      *   *                             *
      *   *******************************



       01 COUNTERS--C.
        03 MessageSpec-COUNTERS.
          05 SendingCompanyIN--C PIC 9(9) COMP-5.
          05 Warning--C PIC 9(9) COMP-5.
          05 Contact--C PIC 9(9) COMP-5.
          05 CorrMessageRefId--C PIC 9(9) COMP-5.
      * 03 FATCA--C PIC 9(9) COMP-5.
      * 03 FATCA-COUNTERS.
        03 ReportingFI-COUNTERS.
          05 ResCountryCode--C PIC 9(9) COMP-5.
          05 TIN--C PIC 9(9) COMP-5.
          05 Name--C PIC 9(9) COMP-5.
          05 R-Address--C PIC 9(9) COMP-5.
          05 R-Address-COUNTERS.
            07 AddressFree2--C PIC 9(9) COMP-5.
            07 AddressFix--C PIC 9(9) COMP-5.
            07 Street--C PIC 9(9) COMP-5.
            07 BuildingIdentifier--C PIC 9(9) COMP-5.
            07 SuiteIdentifier--C PIC 9(9) COMP-5.
            07 FloorIdentifier--C PIC 9(9) COMP-5.
            07 DistrictName--C PIC 9(9) COMP-5.
            07 POB--C PIC 9(9) COMP-5.
            07 PostCode--C PIC 9(9) COMP-5.
            07 CountrySubentity--C PIC 9(9) COMP-5.
            07 AddressFree--C PIC 9(9) COMP-5.
          05 CorrMessageRefId--C PIC 9(9) COMP-5.
          05 CorrDocRefId--C PIC 9(9) COMP-5.
      * 03 ReportingGroup--C PIC 9(9) COMP-5.
        03 ReportingGroup-COUNTERS.
          05 Sponsor--C      PIC 9(9) COMP-5.
          05 Sponsor-COUNTERS.
            07 ResCountryCode--C PIC 9(9) COMP-5.
            07 TIN--C PIC 9(9) COMP-5.
            07 Name--C PIC 9(9) COMP-5.
            07 R-Address--C PIC 9(9) COMP-5.
            07 R-Address-COUNTERS.
              09 AddressFree2--C PIC 9(9) COMP-5.
              09 AddressFix--C PIC 9(9) COMP-5.
              09 Street--C PIC 9(9) COMP-5.
              09 BuildingIdentifier--C PIC 9(9) COMP-5.
              09 SuiteIdentifier--C PIC 9(9) COMP-5.
              09 FloorIdentifier--C PIC 9(9) COMP-5.
              09 DistrictName--C PIC 9(9) COMP-5.
              09 POB--C PIC 9(9) COMP-5.
              09 PostCode--C PIC 9(9) COMP-5.
              09 CountrySubentity--C PIC 9(9) COMP-5.
              09 AddressFree--C PIC 9(9) COMP-5.
            07 CorrMessageRefId--C PIC 9(9) COMP-5.
            07 CorrDocRefId--C PIC 9(9) COMP-5.
          05 Intermediary--C      PIC 9(9) COMP-5.
          05 Intermediary-COUNTERS.
            07 ResCountryCode--C PIC 9(9) COMP-5.
            07 TIN--C PIC 9(9) COMP-5.
            07 Name--C PIC 9(9) COMP-5.
            07 R-Address--C PIC 9(9) COMP-5.
            07 R-Address-COUNTERS.
              09 AddressFree2--C PIC 9(9) COMP-5.
              09 AddressFix--C PIC 9(9) COMP-5.
              09 Street--C PIC 9(9) COMP-5.
              09 BuildingIdentifier--C PIC 9(9) COMP-5.
              09 SuiteIdentifier--C PIC 9(9) COMP-5.
              09 FloorIdentifier--C PIC 9(9) COMP-5.
              09 DistrictName--C PIC 9(9) COMP-5.
              09 POB--C PIC 9(9) COMP-5.
              09 PostCode--C PIC 9(9) COMP-5.
              09 CountrySubentity--C PIC 9(9) COMP-5.
              09 AddressFree--C PIC 9(9) COMP-5.
            07 CorrMessageRefId--C PIC 9(9) COMP-5.
            07 CorrDocRefId--C PIC 9(9) COMP-5.
          05 AccountReport--C PIC 9(9) COMP-5.
          05 AccountReport-COUNTERS.
            07 CorrMessageRefId--C PIC 9(9) COMP-5.
            07 CorrDocRefId--C PIC 9(9) COMP-5.
            07 Individual--C   PIC 9(9) COMP-5.
            07 Individual-COUNTERS.
              09 ResCountryCode--C PIC 9(9) COMP-5.
              09 TIN--C PIC 9(9) COMP-5.
              09 Name--C PIC 9(9) COMP-5.
              09 Name-COUNTERS.
                11 PrecedingTitle--C PIC 9(9) COMP-5.
                11 R-Title--C PIC 9(9) COMP-5.
                11 MiddleName--C PIC 9(9) COMP-5.
                11 NamePrefix--C PIC 9(9) COMP-5.
                11 GenerationIdentifier--C PIC 9(9) COMP-5.
                11 Suffix--C PIC 9(9) COMP-5.
                11 GeneralSuffix--C PIC 9(9) COMP-5.
              09 R-Address--C PIC 9(9) COMP-5.
              09 R-Address-COUNTERS.
                11 AddressFree2--C PIC 9(9) COMP-5.
                11 AddressFix--C PIC 9(9) COMP-5.
                11 Street--C PIC 9(9) COMP-5.
                11 BuildingIdentifier--C PIC 9(9) COMP-5.
                11 SuiteIdentifier--C PIC 9(9) COMP-5.
                11 FloorIdentifier--C PIC 9(9) COMP-5.
                11 DistrictName--C PIC 9(9) COMP-5.
                11 POB--C PIC 9(9) COMP-5.
                11 PostCode--C PIC 9(9) COMP-5.
                11 CountrySubentity--C PIC 9(9) COMP-5.
                11 AddressFree--C PIC 9(9) COMP-5.
              09 Nationality--C PIC 9(9) COMP-5.
              09 BirthInfo--C PIC 9(9) COMP-5.
              09 BirthInfo-COUNTERS.
                11 BirthDate--C PIC 9(9) COMP-5.
                11 City--C PIC 9(9) COMP-5.
                11 CitySubentity--C PIC 9(9) COMP-5.
                11 CountryInfo--C PIC 9(9) COMP-5.
                11 CountryCode--C PIC 9(9) COMP-5.
                11 FormerCountryName--C PIC 9(9) COMP-5.
            07 Organisation--C   PIC 9(9) COMP-5.
            07 Organisation-COUNTERS.
              09 ResCountryCode--C PIC 9(9) COMP-5.
              09 TIN--C PIC 9(9) COMP-5.
              09 Name--C PIC 9(9) COMP-5.
              09 R-Address--C PIC 9(9) COMP-5.
              09 R-Address-COUNTERS.
                11 AddressFree2--C PIC 9(9) COMP-5.
                11 AddressFix--C PIC 9(9) COMP-5.
                11 Street--C PIC 9(9) COMP-5.
                11 BuildingIdentifier--C PIC 9(9) COMP-5.
                11 SuiteIdentifier--C PIC 9(9) COMP-5.
                11 FloorIdentifier--C PIC 9(9) COMP-5.
                11 DistrictName--C PIC 9(9) COMP-5.
                11 POB--C PIC 9(9) COMP-5.
                11 PostCode--C PIC 9(9) COMP-5.
                11 CountrySubentity--C PIC 9(9) COMP-5.
                11 AddressFree--C PIC 9(9) COMP-5.
            07 AcctHolderType--C PIC 9(9) COMP-5.
            07 SubstantialOwner--C   PIC 9(9) COMP-5.
            07 SubstantialOwner-COUNTERS.
              09 ResCountryCode--C PIC 9(9) COMP-5.
              09 TIN--C PIC 9(9) COMP-5.
              09 Name--C PIC 9(9) COMP-5.
              09 Name-COUNTERS.
                11 PrecedingTitle--C PIC 9(9) COMP-5.
                11 R-Title--C PIC 9(9) COMP-5.
                11 MiddleName--C PIC 9(9) COMP-5.
                11 NamePrefix--C PIC 9(9) COMP-5.
                11 GenerationIdentifier--C PIC 9(9) COMP-5.
                11 Suffix--C PIC 9(9) COMP-5.
                11 GeneralSuffix--C PIC 9(9) COMP-5.
              09 R-Address--C PIC 9(9) COMP-5.
              09 R-Address-COUNTERS.
                11 AddressFree2--C PIC 9(9) COMP-5.
                11 AddressFix--C PIC 9(9) COMP-5.
                11 Street--C PIC 9(9) COMP-5.
                11 BuildingIdentifier--C PIC 9(9) COMP-5.
                11 SuiteIdentifier--C PIC 9(9) COMP-5.
                11 FloorIdentifier--C PIC 9(9) COMP-5.
                11 DistrictName--C PIC 9(9) COMP-5.
                11 POB--C PIC 9(9) COMP-5.
                11 PostCode--C PIC 9(9) COMP-5.
                11 CountrySubentity--C PIC 9(9) COMP-5.
                11 AddressFree--C PIC 9(9) COMP-5.
              09 Nationality--C PIC 9(9) COMP-5.
              09 BirthInfo--C PIC 9(9) COMP-5.
              09 BirthInfo-COUNTERS.
                11 BirthDate--C PIC 9(9) COMP-5.
                11 City--C PIC 9(9) COMP-5.
                11 CitySubentity--C PIC 9(9) COMP-5.
                11 CountryInfo--C PIC 9(9) COMP-5.
                11 CountryCode--C PIC 9(9) COMP-5.
                11 FormerCountryName--C PIC 9(9) COMP-5.
            07 Payment--C  PIC 9(9) COMP-5.
            07 Payment-Amnt.
MCHA++        09 PaymentAmnt--C PIC 9(9) COMP-5.
          05 PoolReport--C PIC 9(9) COMP-5.
          05 PoolReport-COUNTERS.
            07 CorrMessageRefId--C PIC 9(9) COMP-5.
            07 CorrDocRefId--C PIC 9(9) COMP-5.



      *   **************
      *   * FATCA_OECD *
      *   *            *
      *   **************
      *   Element racine du fichier XML
       01 FATCA_OECD.
      *      xmlns:ftc="urn:oecd:ties:fatca:v1"
      *      xmlns:sfa="urn:oecd:ties:stffatcatypes:v1"
      *      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      *      attribut : version="X.Y"
         03  tech-attr-req-version PIC X(03).
      *      xsi:schemaLocation=
      *                      "urn:oecd:ties:fatca:v1 FatcaXML_v1.1.xsd"

      *     ***************
      *     * MessageSpec *
      *     *             *
      *     ***************
         03  MessageSpec.
      *      GIIN IF remettante    = 98Q96B.00000.LE.250
          05 SendingCompanyIN OCCURS 0 TO 1 DEPENDING ON
             SendingCompanyIN--C        PIC X(19).
      *      Seule valeur possible = FR
          05 TransmittingCountry PIC X(02).
      *      Seule valeur possible = US
          05 ReceivingCountry PIC X(02).
      *      Seule valeur possible = FATCA
          05 MessageType PIC X(05).
      *      Avertissement specifiques à l'utilisation du fichier
      *      A blanc par défaut.
          05 Warning OCCURS 0 TO 1 DEPENDING ON Warning--C PIC
             X(255).
      *      Balise à ne pas utiliser pour le cahier de charge actuel
          05 Contact OCCURS 0 TO 1 DEPENDING ON Contact--C PIC
             X(32).
      *      Id unique message, concatener DocTypeIndic et date du jour
      *      exemple valeur        = FATCA1-2014-11-13
SBOU  *   05 MessageRefId PIC X(17).
SBOU      05 MessageRefId PIC X(80).
      *      Id unique messages initiaux à corriger
          05 CorrMessageRefId  OCCURS 0 TO 3  DEPENDING ON
MCHA+-*      CorrMessageRefId--C OF MessageSpec-COUNTERS PIC X(17).
MCHA+-       CorrMessageRefId--C OF MessageSpec-COUNTERS PIC X(70).
      *      Année civile à laquelle se rapporte le message
      *      Année  : année fiscale du fichier en entrée
      *      Mois   : 12
      *      jour   : 31
      *      exemple valeur        = 2014-12-31
          05 ReportingPeriod PIC X(10).
      *      Date et heure de création du fichier
      *      exemple valeur        = 2015-03-15T09:45:30
          05 Timestamp PIC X(19).

      *     *********
      *     * FATCA *
      *     *       *
      *     *********
      *      Balise FATCA, multiplicité toujours à 1
      *  03  FATCA OCCURS 1 TO 3  DEPENDING ON FATCA--C.
         03  FATCA.

      *       ***************
      *       * ReportingFI *
      *       *             *
      *       ***************
          05  ReportingFI.
      *        code du pays de résidence fiscale l'entité déclarante
      *        exemple valeur  = FR
           07  ResCountryCode OCCURS 0 TO 3  DEPENDING ON
               ResCountryCode--C OF ReportingFI-COUNTERS PIC X(02).
      *        Identification de l'entité déclarante
      *        doit contenir minimum un caractère
      *        par défaut c'est le même que SendingCompanyIN
      *         exemple valeur  = 98Q96B.00000.LE.250
           07  TIN OCCURS 0 TO 3  DEPENDING ON TIN--C
                                            OF ReportingFI-COUNTERS.
      *         Attribut à mettre à blanc pour indiquer US
            09  tech-attr-opt-issuedBy PIC X(02).
            09  tech-text PIC X(19).
      *         Name de l'entité déclarante (Raison sociale)
      *         exemple valeur  = Assureur A
           07  Name OCCURS 1 TO 3  DEPENDING ON Name--C
                                            OF ReportingFI-COUNTERS.
      *         Attr à ne pas utiliser pour le cahier de charge actuel
      *         exemple valeur  = OECD202
            09  tech-attr-opt-nameType PIC X(07).
            09  tech-text PIC X(64).
           07  R-Address OCCURS 1 TO 3  DEPENDING ON R-Address--C
                                            OF ReportingFI-COUNTERS.
      *         Attr à ne pas utiliser pour le cahier de charge actuel
      *         exemple valeur  = OECD301
            09  tech-attr-opt-legalAddressType PIC X(07).
      *         Code du pays associé à l'adresse
      *         exemple valeur  = FR
            09  CountryCode PIC X(02).
      *         Choisir :
      *         Soit AddressFree
      *         Soit AddressFix puis AddressFree

      *         Addresse format libre
            09  AddressFree2 OCCURS 0 TO 1 DEPENDING ON
                AddressFree2--C OF ReportingFI-COUNTERS PIC X(255).
      *         Addresse format structuré (prioritaire)
            09  AddressFix OCCURS 0 TO 1 DEPENDING ON
                AddressFix--C OF ReportingFI-COUNTERS.
      *           rue
              11  Street OCCURS 0 TO 1 DEPENDING ON Street--C
                 OF ReportingFI-COUNTERS PIC X(80).
      *           Numéro de rue (à défaut bâtiment)
              11  BuildingIdentifier OCCURS 0 TO 1 DEPENDING ON
                  BuildingIdentifier--C OF ReportingFI-COUNTERS
                  PIC X(80).
      *           Complément d'adresse : n appartement, résidence, etc
              11  SuiteIdentifier OCCURS 0 TO 1 DEPENDING ON
                  SuiteIdentifier--C OF ReportingFI-COUNTERS PIC X(80).
      *           Numéro d'étage
              11  FloorIdentifier OCCURS 0 TO 1 DEPENDING ON
                  FloorIdentifier--C OF ReportingFI-COUNTERS PIC X(80).
      *           Département
              11  DistrictName OCCURS 0 TO 1 DEPENDING ON
                  DistrictName--C OF ReportingFI-COUNTERS PIC X(80).
      *           Boîte postale
              11  POB OCCURS 0 TO 1 DEPENDING ON POB--C
                  OF ReportingFI-COUNTERS PIC X(80).
      *           Code postal
              11  PostCode OCCURS 0 TO 1 DEPENDING ON PostCode--C
                  OF ReportingFI-COUNTERS PIC X(80).
      *           Commune
              11  City PIC X(80).
      *           Région ou État fédéré
              11  CountrySubentity OCCURS 0 TO 1 DEPENDING ON
                  CountrySubentity--C OF ReportingFI-COUNTERS PIC X(80).
            09  AddressFree OCCURS 0 TO 1 DEPENDING ON
                AddressFree--C OF ReportingFI-COUNTERS PIC X(255).
           07  DocSpec.
      *         Type de déclaration communiquée
      *         valeur possible :
      *         FATCA1  = Données Nouvelles
      *         FATCA2  = Données Corrigées
      *         FATCA3  = Données Annulées
      *         FATCA4  = Données Modifiées
      *         FATCA11 = Nouvelles Données Test
      *         FATCA12 = Données Test Corrigées
      *         FATCA13 = Données Test Annulées
      *         FATCA14 = Données Test Modifiées
            09  DocTypeIndic PIC X(07).
      *         Identifiant du bloc de données
      *         Concatener :
      *         MessageRefId " " Name " reporting FI"
      *         exemple = fatca1-2014-17-01 Assureur A reporting FI
            09  DocRefId PIC X(80).
      *         Identifiant du message à corriger
            09  CorrMessageRefId OCCURS 0 TO 1 DEPENDING ON
MCHA+-*         CorrMessageRefId--C OF ReportingFI-COUNTERS PIC X(17).
MCHA+-          CorrMessageRefId--C OF ReportingFI-COUNTERS PIC X(80).
      *         Identifiant du bloc de données à corriger
            09  CorrDocRefId OCCURS 0 TO 1 DEPENDING ON
                CorrDocRefId--C OF ReportingFI-COUNTERS PIC X(80).

      *       ******************
      *       * ReportingGroup *
      *       *                *
      *       ******************
      *       Balise ReportingGroup, multiplicité toujours à 1
      *   05  ReportingGroup OCCURS 1 TO 3  DEPENDING ON
      *       ReportingGroup--C.
          05  ReportingGroup.
      *        ***********
      *        * Sponsor *
      *        *         *
      *        ***********
           07  Sponsor OCCURS 0 TO 1 DEPENDING ON Sponsor--C.
      *        code du pays de résidence fiscale l'entité déclarante
      *        exemple valeur  = FR
            09  ResCountryCode OCCURS 0 TO 3  DEPENDING ON
                ResCountryCode--C OF Sponsor-COUNTERS PIC X(02).
      *         Identification de l'entité déclarante
      *         doit contenir minimum un caractère
      *         par défaut c'est le même que SendingCompanyIN
      *          exemple valeur  = 98Q96B.00000.LE.250
            09  TIN OCCURS 0 TO 3  DEPENDING ON TIN--C
                                             OF Sponsor-COUNTERS.
      *          Attribut à mettre à blanc pour indiquer US
             11  tech-attr-opt-issuedBy PIC X(02).
             11  tech-text PIC X(19).
      *          Name de l'entité déclarante (Raison sociale)
      *          exemple valeur  = Assureur A
            09  Name OCCURS 1 TO 3  DEPENDING ON Name--C
                                             OF Sponsor-COUNTERS.
      *          Attr à ne pas utiliser pour le cahier de charge actuel
      *          exemple valeur  = OECD202
             11  tech-attr-opt-nameType PIC X(07).
             11  tech-text PIC X(64).
            09  R-Address OCCURS 1 TO 3  DEPENDING ON R-Address--C
                                             OF Sponsor-COUNTERS.
      *          Attr à ne pas utiliser pour le cahier de charge actuel
      *          exemple valeur  = OECD301
             11  tech-attr-opt-legalAddressType PIC X(07).
      *          Code du pays associé à l'adresse
      *          exemple valeur  = FR
             11  CountryCode PIC X(02).
      *          Choisir :
      *          Soit AddressFree
      *          Soit AddressFix puis AddressFree

      *          Addresse format libre
             11  AddressFree2 OCCURS 0 TO 1 DEPENDING ON
                 AddressFree2--C OF Sponsor-COUNTERS PIC X(255).
      *          Addresse format structuré (prioritaire)
             11  AddressFix OCCURS 0 TO 1 DEPENDING ON
                 AddressFix--C OF Sponsor-COUNTERS.
      *            rue
               13  Street OCCURS 0 TO 1 DEPENDING ON Street--C
                   OF Sponsor-COUNTERS PIC X(80).
      *            Numéro de rue (à défaut bâtiment)
               13  BuildingIdentifier OCCURS 0 TO 1 DEPENDING ON
                   BuildingIdentifier--C OF Sponsor-COUNTERS PIC X(80).
      *            Complément d'adresse : n appartement, résidence, etc
               13  SuiteIdentifier OCCURS 0 TO 1 DEPENDING ON
                   SuiteIdentifier--C OF Sponsor-COUNTERS PIC X(80).
      *            Numéro d'étage
               13  FloorIdentifier OCCURS 0 TO 1 DEPENDING ON
                   FloorIdentifier--C OF Sponsor-COUNTERS PIC X(80).
      *            Département
               13  DistrictName OCCURS 0 TO 1 DEPENDING ON
                   DistrictName--C OF Sponsor-COUNTERS PIC X(80).
      *            Boîte postale
               13  POB OCCURS 0 TO 1 DEPENDING ON POB--C
                   OF Sponsor-COUNTERS PIC X(80).
      *            Code postal
               13  PostCode OCCURS 0 TO 1 DEPENDING ON PostCode--C
                   OF Sponsor-COUNTERS PIC X(80).
      *            Commune
               13  City PIC X(80).
      *            Région ou État fédéré
               13  CountrySubentity OCCURS 0 TO 1 DEPENDING ON
                   CountrySubentity--C OF Sponsor-COUNTERS PIC X(80).
             11  AddressFree OCCURS 0 TO 1 DEPENDING ON
                 AddressFree--C OF Sponsor-COUNTERS PIC X(255).
            09  DocSpec.
      *          Type de déclaration communiquée
      *          valeur possible :
      *          FATCA1  = Données Nouvelles
      *          FATCA2  = Données Corrigées
      *          FATCA3  = Données Annulées
      *          FATCA4  = Données Modifiées
      *          FATCA11 = Nouvelles Données Test
      *          FATCA12 = Données Test Corrigées
      *          FATCA13 = Données Test Annulées
      *          FATCA14 = Données Test Modifiées
             11  DocTypeIndic PIC X(07).
      *          Identifiant du bloc de données
      *          Concatener :
      *          MessageRefId " " Name " reporting FI"
      *          exemple = fatca1-2014-17-01 Assureur A reporting FI
150978*      11  DocRefId PIC X(80).
150978       11  DocRefId PIC X(90).
      *          Identifiant du message à corriger
             11  CorrMessageRefId OCCURS 0 TO 1 DEPENDING ON
MCHA+-*          CorrMessageRefId--C OF Sponsor-COUNTERS PIC X(17).
MCHA+-           CorrMessageRefId--C OF Sponsor-COUNTERS PIC X(80).
      *          Identifiant du bloc de données à corriger
             11  CorrDocRefId OCCURS 0 TO 1 DEPENDING ON
                 CorrDocRefId--C OF Sponsor-COUNTERS PIC X(80).

      *        ****************
      *        * Intermediary *
      *        *              *
      *        ****************
           07  Intermediary OCCURS 0 TO 1 DEPENDING ON
               Intermediary--C.
      *        code du pays de résidence fiscale l'entité déclarante
      *        exemple valeur  = FR
            09  ResCountryCode OCCURS 0 TO 3  DEPENDING ON
                ResCountryCode--C  OF Intermediary-COUNTERS PIC X(02).
      *         Identification de l'entité déclarante
      *         doit contenir minimum un caractère
      *         par défaut c'est le même que SendingCompanyIN
      *          exemple valeur  = 98Q96B.00000.LE.250
            09  TIN OCCURS 0 TO 3  DEPENDING ON TIN--C
                                   OF Intermediary-COUNTERS.
      *          Attribut à mettre à blanc pour indiquer US
             11  tech-attr-opt-issuedBy PIC X(02).
             11  tech-text PIC X(19).
      *          Name de l'entité déclarante (Raison sociale)
      *          exemple valeur  = Assureur A
            09  Name OCCURS 1 TO 3  DEPENDING ON Name--C
                                           OF Intermediary-COUNTERS.
      *          Attr à ne pas utiliser pour le cahier de charge actuel
      *          exemple valeur  = OECD202
             11  tech-attr-opt-nameType PIC X(07).
             11  tech-text PIC X(64).
            09  R-Address OCCURS 1 TO 3  DEPENDING ON R-Address--C
                                           OF Intermediary-COUNTERS.
      *          Attr à ne pas utiliser pour le cahier de charge actuel
      *          exemple valeur  = OECD301
             11  tech-attr-opt-legalAddressType PIC X(07).
      *          Code du pays associé à l'adresse
      *          exemple valeur  = FR
             11  CountryCode PIC X(02).
      *          Choisir :
      *          Soit AddressFree
      *          Soit AddressFix puis AddressFree

      *          Addresse format libre
             11  AddressFree2 OCCURS 0 TO 1 DEPENDING ON
                 AddressFree2--C OF Intermediary-COUNTERS PIC X(255).
      *          Addresse format structuré (prioritaire)
             11  AddressFix OCCURS 0 TO 1 DEPENDING ON
                 AddressFix--C OF Intermediary-COUNTERS.
      *            rue
               13  Street OCCURS 0 TO 1 DEPENDING ON Street--C
                   OF Intermediary-COUNTERS PIC X(80).
      *            Numéro de rue (à défaut bâtiment)
               13  BuildingIdentifier OCCURS 0 TO 1 DEPENDING ON
                   BuildingIdentifier--C
                                     OF Intermediary-COUNTERS PIC X(80).
      *            Complément d'adresse : n appartement, résidence, etc
               13  SuiteIdentifier OCCURS 0 TO 1 DEPENDING ON
                   SuiteIdentifier--C
                                     OF Intermediary-COUNTERS PIC X(80).
      *            Numéro d'étage
               13  FloorIdentifier OCCURS 0 TO 1 DEPENDING ON
                   FloorIdentifier--C
                                     OF Intermediary-COUNTERS PIC X(80).
      *            Département
               13  DistrictName OCCURS 0 TO 1 DEPENDING ON
                   DistrictName--C
                                   OF Intermediary-COUNTERS PIC X(80).
      *            Boîte postale
               13  POB OCCURS 0 TO 1 DEPENDING ON POB--C
                                   OF Intermediary-COUNTERS PIC X(80).
      *            Code postal
               13  PostCode OCCURS 0 TO 1 DEPENDING ON PostCode--C
                                   OF Intermediary-COUNTERS PIC X(80).
      *            Commune
               13  City PIC X(80).
      *            Région ou État fédéré
               13  CountrySubentity OCCURS 0 TO 1 DEPENDING ON
                   CountrySubentity--C
                                   OF Intermediary-COUNTERS PIC X(80).
             11  AddressFree OCCURS 0 TO 1 DEPENDING ON
                 AddressFree--C
                                   OF Intermediary-COUNTERS PIC X(255).
            09  DocSpec.
      *          Type de déclaration communiquée
      *          valeur possible :
      *          FATCA1  = Données Nouvelles
      *          FATCA2  = Données Corrigées
      *          FATCA3  = Données Annulées
      *          FATCA4  = Données Modifiées
      *          FATCA11 = Nouvelles Données Test
      *          FATCA12 = Données Test Corrigées
      *          FATCA13 = Données Test Annulées
      *          FATCA14 = Données Test Modifiées
             11  DocTypeIndic PIC X(07).
      *          Identifiant du bloc de données
      *          Concatener :
      *          MessageRefId " " Name " reporting FI"
      *          exemple = fatca1-2014-17-01 Assureur A reporting FI
150978*      11  DocRefId PIC X(80).
150978       11  DocRefId PIC X(90).
      *          Identifiant du message à corriger
             11  CorrMessageRefId OCCURS 0 TO 1 DEPENDING ON
MCHA+-*          CorrMessageRefId--C OF Intermediary-COUNTERS PIC X(17).
MCHA+-           CorrMessageRefId--C OF Intermediary-COUNTERS PIC X(80).
      *          Identifiant du bloc de données à corriger
             11  CorrDocRefId OCCURS 0 TO 1 DEPENDING ON
                 CorrDocRefId--C OF Intermediary-COUNTERS PIC X(80).
      *        *****************
      *        * AccountReport *
      *        *               *
      *        *****************
      *        Pratiquement la balise a une mutliplicité n
      *        la multiplicité est géré au niveau du traitement
           07  AccountReport OCCURS 0 TO 1  DEPENDING ON
               AccountReport--C.
            09  DocSpec.
      *          Type de déclaration communiquée
      *          valeur possible :
      *          FATCA1  = Données Nouvelles
      *          FATCA2  = Données Corrigées
      *          FATCA3  = Données Annulées
      *          FATCA4  = Données Modifiées
      *          FATCA11 = Nouvelles Données Test
      *          FATCA12 = Données Test Corrigées
      *          FATCA13 = Données Test Annulées
      *          FATCA14 = Données Test Modifiées
             11  DocTypeIndic PIC X(07).
      *          Identifiant du bloc de données
      *          Concatener :
      *          MessageRefId " " Name " reporting FI"
      *          exemple = fatca1-2014-17-01 Assureur A reporting FI
150978*      11  DocRefId PIC X(80).
150978       11  DocRefId PIC X(90).
      *          Identifiant du message à corriger
             11  CorrMessageRefId OCCURS 0 TO 1 DEPENDING ON
                 CorrMessageRefId--C OF AccountReport-COUNTERS
MCHA+-*          PIC X(17).
MCHA+-           PIC X(90).
      *          Identifiant du bloc de données à corriger
             11  CorrDocRefId OCCURS 0 TO 1 DEPENDING ON
                 CorrDocRefId--C OF AccountReport-COUNTERS PIC X(90).
      *         Identifiant Unique de contrat I-KAC
MCHA- *     09  AccountNumber PIC X(17).
MCHA+       09  AccountNumber PIC X(30).
      *         Titulaire du compte
            09  AccountHolder.
      *          Choix :
      *          Soit Individual
      *          Soit Organisation ET AccHolderType

      *          **************
      *          * Individual *
      *          *            *
      *          **************
             11  Individual OCCURS 0 TO 1 DEPENDING ON
                 Individual--C.
      *           code du pays de résidence du titulaire du compte
      *           exemple valeur  = FR
              13  ResCountryCode OCCURS 0 TO  3 DEPENDING ON
                  ResCountryCode--C OF Individual-COUNTERS PIC X(02).
      *           Référence fiscalité étrangère (GIIN)
      *           doit contenir minimum un caractère
              13  TIN OCCURS 0 TO  3 DEPENDING ON TIN--C
                                    OF Individual-COUNTERS.
               15  tech-attr-opt-issuedBy PIC X(02).
               15  tech-text PIC X(17).
      *           Nom
              13  Name OCCURS 1 TO 3  DEPENDING ON Name--C
                                    OF Individual-COUNTERS.
      *            Champs non utilisé
               15  PrecedingTitle OCCURS 0 TO 1 DEPENDING ON
                   PrecedingTitle--C OF Individual-COUNTERS PIC X(32).
      *            Titre civilité
               15  R-Title OCCURS 0 TO 3  DEPENDING ON
                   R-Title--C OF Individual-COUNTERS PIC X(32).
      *            Prénom
               15  FirstName.
      *             Attr non utilisé
                17  tech-attr-non-xnlNameType PIC X(32).
                17  tech-text PIC X(32).
      *            Champs non utilisé
               15  MiddleName OCCURS 0 TO 3  DEPENDING ON
                   MiddleName--C OF Individual-COUNTERS.
      *             Attr non utilisé
                17  tech-attr-non-xnlNameType PIC X(32).
                17  tech-text PIC X(32).
      *            Champs non utilisé
               15  NamePrefix OCCURS 0 TO 1 DEPENDING ON
                   NamePrefix--C OF Individual-COUNTERS.
      *             Attr non utilisé
                17  tech-attr-non-xnlNameType PIC X(32).
                17  tech-text PIC X(32).
      *            Nom du famille
               15  LastName.
      *             Attr non utilisé
                17  tech-attr-non-xnlNameType PIC X(32).
                17  tech-text PIC X(32).
      *            Champs non utilisé
               15  GenerationIdentifier OCCURS 0 TO 3
                   DEPENDING ON GenerationIdentifier--C
                                 OF Individual-COUNTERS PIC X(32).
      *            Champs non utilisé
               15  Suffix OCCURS 0 TO 3  DEPENDING ON Suffix--C
                   OF Individual-COUNTERS PIC X(32).
      *            Champs non utilisé
               15  GeneralSuffix OCCURS 0 TO 1 DEPENDING ON
                   GeneralSuffix--C OF Individual-COUNTERS PIC X(32).
              13  R-Address OCCURS 1 TO 3  DEPENDING ON
                  R-Address--C OF Individual-COUNTERS.
      *           Attr à ne pas utiliser pour cahier de charge actuel
      *           exemple valeur  = OECD301
               15  tech-attr-opt-legalAddressType PIC X(32).
      *            Code du pays associé à l'adresse
      *            exemple valeur  = FR
               15  CountryCode PIC X(02).
      *            Choisir :
      *            Soit AddressFree
      *            Soit AddressFix puis AddressFree

      *            Addresse format libre
               15  AddressFree2 OCCURS 0 TO 1 DEPENDING ON
                   AddressFree2--C OF Individual-COUNTERS PIC X(255).
      *            Addresse format structuré (prioritaire)
               15  AddressFix OCCURS 0 TO 1 DEPENDING ON
                   AddressFix--C OF Individual-COUNTERS.
      *             Rue
                17  Street OCCURS 0 TO 1 DEPENDING ON
                    Street--C OF Individual-COUNTERS PIC X(80).
      *             Numéro de rue (à défaut bâtiment)
                17  BuildingIdentifier OCCURS 0 TO 1
                    DEPENDING ON BuildingIdentifier--C
                         OF Individual-COUNTERS   PIC X(80).
      *             Complément d'adresse : n appartement, résidence, etc
                17  SuiteIdentifier OCCURS 0 TO 1 DEPENDING
                    ON SuiteIdentifier--C OF Individual-COUNTERS
                                PIC X(80).
      *             Numéro d'étage
                17  FloorIdentifier OCCURS 0 TO 1 DEPENDING
                    ON FloorIdentifier--C OF Individual-COUNTERS
                                         PIC X(80).
      *             Département
                17  DistrictName OCCURS 0 TO 1 DEPENDING ON
                    DistrictName--C OF Individual-COUNTERS PIC X(80).
      *             Boîte postale
                17  POB OCCURS 0 TO 1 DEPENDING ON POB--C
                    OF Individual-COUNTERS PIC X(80).
      *             Code postal
                17  PostCode OCCURS 0 TO 1 DEPENDING ON
                    PostCode--C OF Individual-COUNTERS PIC X(80).
      *             Commune
                17  City PIC X(80).
      *             Région ou État fédéré
                17  CountrySubentity OCCURS 0 TO 1 DEPENDING
                    ON CountrySubentity--C
                      OF Individual-COUNTERS PIC X(80).
               15  AddressFree OCCURS 0 TO 1 DEPENDING ON
                   AddressFree--C OF Individual-COUNTERS PIC X(255).
      *           Code pays nationlité
              13  Nationality OCCURS 0 TO 3  DEPENDING ON
                  Nationality--C OF Individual-COUNTERS PIC X(02).
      *           Information naissance
              13  BirthInfo OCCURS 0 TO 1 DEPENDING ON
                  BirthInfo--C OF Individual-COUNTERS.
      *           date naissance
               15 BirthDate OCCURS 0 TO 1 DEPENDING ON
                  BirthDate--C OF Individual-COUNTERS PIC X(10).
      *           champs à ne pas utilisé, Ville naissance
               15 City OCCURS 0 TO 1 DEPENDING ON City--C
                  OF Individual-COUNTERS PIC X(80).
      *           champs à ne pas utilisé, Région de naissance
               15 CitySubentity OCCURS 0 TO 1 DEPENDING ON
                  CitySubentity--C OF Individual-COUNTERS PIC X(80).
               15 CountryInfo OCCURS 0 TO 1 DEPENDING ON
                  CountryInfo--C OF Individual-COUNTERS.
      *             champs à ne pas utilisé, Code pays naissance
                17  CountryCode OCCURS 0 TO 1 DEPENDING ON
                    CountryCode--C OF Individual-COUNTERS PIC X(02).
      *             champs à ne pas utilisé, Code pays naissance
                17  FormerCountryName OCCURS 0 TO 1
                    DEPENDING ON FormerCountryName--C
                              OF Individual-COUNTERS  PIC X(02).
      *          ****************
      *          * Organisation *
      *          *              *
      *          ****************
             11  Organisation OCCURS 0 TO 1 DEPENDING ON
                 Organisation--C.
      *           code du pays de résidence du titulaire du compte
      *           exemple valeur  = FR
              13  ResCountryCode OCCURS 0 TO 3  DEPENDING ON
                  ResCountryCode--C OF Organisation-COUNTERS PIC X(02).
      *           Référence fiscalité étrangère (GIIN)
      *           doit contenir minimum un caractère
              13  TIN OCCURS 0 TO 3  DEPENDING ON TIN--C
                                 OF Organisation-COUNTERS.
               15  tech-attr-opt-issuedBy PIC X(02).
               15  tech-text PIC X(17).
      *           Raison sociale
              13  Name OCCURS 1 TO 10 DEPENDING ON Name--C
                                 OF Organisation-COUNTERS.
      *            Attr à ne pas utiliser pour le cahier charge actuel
      *            exemple valeur  = OECD202
               15  tech-attr-opt-nameType PIC X(07).
               15  tech-text PIC X(64).
              13  R-Address OCCURS 1 TO 3  DEPENDING ON
                  R-Address--C OF Organisation-COUNTERS.
      *           Attr à ne pas utiliser pour cahier de charge actuel
      *           exemple valeur  = OECD301
               15  tech-attr-opt-legalAddressType PIC X(32).
      *            Code du pays associé à l'adresse
      *            exemple valeur  = FR
               15  CountryCode PIC X(02).
      *            Choisir :
      *            Soit AddressFree
      *            Soit AddressFix puis AddressFree

      *            Addresse format libre
               15  AddressFree2 OCCURS 0 TO 1 DEPENDING ON
                   AddressFree2--C OF Organisation-COUNTERS PIC X(255).
      *            Addresse format structuré (prioritaire)
               15  AddressFix OCCURS 0 TO 1 DEPENDING ON
                   AddressFix--C OF Organisation-COUNTERS.
      *             Rue
                17  Street OCCURS 0 TO 1 DEPENDING ON
                    Street--C OF Organisation-COUNTERS PIC X(80).
      *             Numéro de rue (à défaut bâtiment)
                17  BuildingIdentifier OCCURS 0 TO 1
                    DEPENDING ON BuildingIdentifier--C
                         OF Organisation-COUNTERS  PIC X(80).
      *             Complément d'adresse : n appartement, résidence, etc
                17  SuiteIdentifier OCCURS 0 TO 1 DEPENDING
                    ON SuiteIdentifier--C OF Organisation-COUNTERS
                                PIC X(80).
      *             Numéro d'étage
                17  FloorIdentifier OCCURS 0 TO 1 DEPENDING
                    ON FloorIdentifier--C OF Organisation-COUNTERS
                                         PIC X(80).
      *             Département
                17  DistrictName OCCURS 0 TO 1 DEPENDING ON
                    DistrictName--C OF Organisation-COUNTERS PIC X(80).
      *             Boîte postale
                17  POB OCCURS 0 TO 1 DEPENDING ON POB--C
                    OF Organisation-COUNTERS PIC X(80).
      *             Code postal
                17  PostCode OCCURS 0 TO 1 DEPENDING ON
                    PostCode--C OF Organisation-COUNTERS PIC X(80).
      *             Commune
                17  City PIC X(80).
      *             Région ou État fédéré
                17  CountrySubentity OCCURS 0 TO 1 DEPENDING
                    ON CountrySubentity--C
                      OF Organisation-COUNTERS PIC X(80).
               15  AddressFree OCCURS 0 TO 1 DEPENDING ON
                   AddressFree--C OF Organisation-COUNTERS PIC X(255).
      *          ******************
      *          * AcctHolderType *
      *          *                *
      *          ******************
      *          Type organisation détentrice du compte
      *          Valeur possible =
      *            FATCA101 : Owner-Documented FI with specified
      *                       US owner(s)
      *            FATCA102 : Passive Non-Financial Entity with
      *                       substantial US Owners
      *            FATCA103 : Non-Participating FI
      *            FATCA104 : Specified US Person
      *            FATCA105 : Direct Reporting NFFE
             11  AcctHolderType OCCURS 0 TO 1 DEPENDING ON
                   AcctHolderType--C PIC X(08).
      *         ********************
      *         * SubstantialOwner *
      *         *                  *
      *         ********************
            09  SubstantialOwner OCCURS 0 TO 30 DEPENDING ON
                SubstantialOwner--C.
      *          code du pays de résidence du titulaire du compte
      *          exemple valeur  = FR
             11  ResCountryCode OCCURS 0 TO  3 DEPENDING ON
                 ResCountryCode--C
                               OF SubstantialOwner-COUNTERS PIC X(02).
      *          Référence fiscalité étrangère (GIIN)
      *          doit contenir minimum un caractère
             11  TIN OCCURS 0 TO  3 DEPENDING ON TIN--C
                                   OF SubstantialOwner-COUNTERS.
              13  tech-attr-opt-issuedBy PIC X(02).
              13  tech-text PIC X(17).
      *          Nom
             11  Name OCCURS 1 TO 3  DEPENDING ON Name--C
                                   OF SubstantialOwner-COUNTERS.
      *           Champs non utilisé
              13  PrecedingTitle OCCURS 0 TO 1 DEPENDING ON
                  PrecedingTitle--C
                               OF SubstantialOwner-COUNTERS PIC X(32).
      *           Titre civilité
              13  R-Title OCCURS 0 TO 3  DEPENDING ON
                  R-Title--C OF SubstantialOwner-COUNTERS PIC X(32).
      *           Prénom
              13  FirstName.
      *            Attr non utilisé
               15  tech-attr-non-xnlNameType PIC X(32).
               15  tech-text PIC X(32).
      *           Champs non utilisé
              13  MiddleName OCCURS 0 TO 3  DEPENDING ON
                  MiddleName--C OF SubstantialOwner-COUNTERS.
      *            Attr non utilisé
               15  tech-attr-non-xnlNameType PIC X(32).
               15  tech-text PIC X(32).
      *           Champs non utilisé
              13  NamePrefix OCCURS 0 TO 1 DEPENDING ON
                  NamePrefix--C OF SubstantialOwner-COUNTERS.
      *            Attr non utilisé
               15  tech-attr-non-xnlNameType PIC X(32).
               15  tech-text PIC X(32).
      *           Nom du famille
              13  LastName.
      *            Attr non utilisé
               15  tech-attr-non-xnlNameType PIC X(32).
               15  tech-text PIC X(32).
      *           Champs non utilisé
              13  GenerationIdentifier OCCURS 0 TO 3
                  DEPENDING ON GenerationIdentifier--C
                            OF SubstantialOwner-COUNTERS PIC X(32).
      *           Champs non utilisé
              13  Suffix OCCURS 0 TO 3  DEPENDING ON Suffix--C
                  OF SubstantialOwner-COUNTERS PIC X(32).
      *           Champs non utilisé
              13  GeneralSuffix OCCURS 0 TO 1 DEPENDING ON
                  GeneralSuffix--C
                            OF SubstantialOwner-COUNTERS PIC X(32).
             11  R-Address OCCURS 1 TO 3  DEPENDING ON
                 R-Address--C OF SubstantialOwner-COUNTERS.
      *          Attr à ne pas utiliser pour cahier de charge actuel
      *          exemple valeur  = OECD301
              13  tech-attr-opt-legalAddressType PIC X(32).
      *           Code du pays associé à l'adresse
      *           exemple valeur  = FR
              13  CountryCode PIC X(02).
      *           Choisir :
      *           Soit AddressFree
      *           Soit AddressFix puis AddressFree

      *           Addresse format libre
              13  AddressFree2 OCCURS 0 TO 1 DEPENDING ON
                  AddressFree2--C
                              OF SubstantialOwner-COUNTERS PIC X(255).
      *           Addresse format structuré (prioritaire)
              13  AddressFix OCCURS 0 TO 1 DEPENDING ON
                  AddressFix--C OF SubstantialOwner-COUNTERS.
      *            Rue
               15  Street OCCURS 0 TO 1 DEPENDING ON
                   Street--C OF SubstantialOwner-COUNTERS PIC X(80).
      *            Numéro de rue (à défaut bâtiment)
               15  BuildingIdentifier OCCURS 0 TO 1
                   DEPENDING ON BuildingIdentifier--C
                        OF SubstantialOwner-COUNTERS PIC X(80).
      *            Complément d'adresse : n appartement, résidence, etc
               15  SuiteIdentifier OCCURS 0 TO 1 DEPENDING
                   ON SuiteIdentifier--C
                        OF SubstantialOwner-COUNTERS PIC X(80).
      *            Numéro d'étage
               15  FloorIdentifier OCCURS 0 TO 1 DEPENDING
                   ON FloorIdentifier--C
                        OF SubstantialOwner-COUNTERS PIC X(80).
      *            Département
               15  DistrictName OCCURS 0 TO 1 DEPENDING ON
                   DistrictName--C
                        OF SubstantialOwner-COUNTERS PIC X(80).
      *            Boîte postale
               15  POB OCCURS 0 TO 1 DEPENDING ON POB--C
                   OF SubstantialOwner-COUNTERS PIC X(80).
      *            Code postal
               15  PostCode OCCURS 0 TO 1 DEPENDING ON
                   PostCode--C OF SubstantialOwner-COUNTERS PIC X(80).
      *            Commune
               15  City PIC X(80).
      *            Région ou État fédéré
               15  CountrySubentity OCCURS 0 TO 1 DEPENDING
                   ON CountrySubentity--C
                     OF SubstantialOwner-COUNTERS PIC X(80).
              13  AddressFree OCCURS 0 TO 1 DEPENDING ON
                  AddressFree--C
                     OF SubstantialOwner-COUNTERS PIC X(255).
      *          Code pays nationlité
             11  Nationality OCCURS 0 TO 3  DEPENDING ON
                 Nationality--C
                     OF SubstantialOwner-COUNTERS PIC X(02).
      *          Information naissance
             11  BirthInfo OCCURS 0 TO 1 DEPENDING ON
                 BirthInfo--C OF SubstantialOwner-COUNTERS.
      *          date naissance
              13 BirthDate OCCURS 0 TO 1 DEPENDING ON
                 BirthDate--C
                              OF SubstantialOwner-COUNTERS PIC X(10).
      *          champs à ne pas utilisé, Ville naissance
              13 City OCCURS 0 TO 1 DEPENDING ON City--C
                 OF SubstantialOwner-COUNTERS PIC X(80).
      *          champs à ne pas utilisé, Région de naissance
              13 CitySubentity OCCURS 0 TO 1 DEPENDING ON
                 CitySubentity--C
                 OF SubstantialOwner-COUNTERS PIC X(80).
              13 CountryInfo OCCURS 0 TO 1 DEPENDING ON
                 CountryInfo--C OF SubstantialOwner-COUNTERS.
      *            champs à ne pas utilisé, Code pays naissance
               15  CountryCode OCCURS 0 TO 1 DEPENDING ON
                   CountryCode--C
                                OF SubstantialOwner-COUNTERS PIC X(02).
      *            champs à ne pas utilisé, Code pays naissance
               15  FormerCountryName OCCURS 0 TO 1
                   DEPENDING ON FormerCountryName--C
                             OF SubstantialOwner-COUNTERS PIC X(02).
      *         ******************
      *         * AccountBalance *
      *         *                *
      *         ******************
            09  AccountBalance.
      *          Devise de solde du compte
             11  tech-attr-req-currCode PIC X(03).
      *          Montant de solde du compte
             11  tech-text PIC S9(16)V9(2)
                   COMP-3.
      *         ***********
      *         * Payment *
      *         *         *
      *         ***********
            09  Payment OCCURS 0 TO 3  DEPENDING ON Payment--C.
      *          Type du payment
      *          Valeur possible =
      *            FATCA501 : Dividendes
      *            FATCA502 : Intérêt
      *            FATCA503 : Produits Brut/Rachats sur contrats
      *                       d'assurance-vie et contrats ou bons
      *                       de capitalisation
      *            FATCA504 : Autres revenus, y compris les rentes
      *                       et arrérages.
             11  R-Type PIC X(08).
MCHA++       11  PaymentAmnt OCCURS 0 TO 1 DEPENDING ON PaymentAmnt--C.
      *           Devise du payment
              13  tech-attr-req-currCode PIC X(03).
      *           Montant de payment
              13  tech-text PIC S9(16)V9(2)
                     COMP-3.
      *        **************
      *        * PoolReport *
      *        *            *
      *        **************
      *        Balise PoolReport non utilisée pour la France
           07  PoolReport OCCURS 0 TO 3  DEPENDING ON PoolReport--C.
            09  DocSpec.
      *          Type de déclaration communiquée
      *          valeur possible :
      *          FATCA1  = Données Nouvelles
      *          FATCA2  = Données Corrigées
      *          FATCA3  = Données Annulées
      *          FATCA4  = Données Modifiées
      *          FATCA11 = Nouvelles Données Test
      *          FATCA12 = Données Test Corrigées
      *          FATCA13 = Données Test Annulées
      *          FATCA14 = Données Test Modifiées
             11  DocTypeIndic PIC X(07).
      *          Identifiant du bloc de données
      *          Concatener :
      *          MessageRefId " " Name " reporting FI"
      *          exemple = fatca1-2014-17-01 Assureur A reporting FI
             11  DocRefId PIC X(80).
      *          Identifiant du message à corriger
             11  CorrMessageRefId OCCURS 0 TO 1 DEPENDING ON
MCHA+-*          CorrMessageRefId--C OF PoolReport-COUNTERS PIC X(17).
MCHA+-           CorrMessageRefId--C OF PoolReport-COUNTERS PIC X(80).
      *          Identifiant du bloc de données à corriger
             11  CorrDocRefId OCCURS 0 TO 1 DEPENDING ON
                 CorrDocRefId--C OF PoolReport-COUNTERS PIC X(80).
            09  AccountCount PIC S9(9) COMP-5.
      *         FATCA201 = Recalcitrant account holders
      *                    with US Indicia
      *         FATCA202 = Recalcitrant account holders
      *                    without US Indicia
      *         FATCA203 = Dormant Accounts
      *         FATCA204 = Non-participating foreign
      *                    financial institutions
      *         FATCA205 = Recalcitrant account holders
      *                    that are US persons
      *         FATCA206 = Recalcitrant account holders
      *                    that are passive NFFEs
            09  AccountPoolReportType PIC X(08).
            09  PoolBalance.
      *          Devise du solde
             11  tech-attr-req-currCode PIC X(03).
      *          Montant du solde
             11  tech-text PIC S9(16)V9(2)
                   COMP-3.
