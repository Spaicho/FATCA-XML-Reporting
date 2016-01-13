# FATCA-XML-Reporting

FATCA is a new US law aimed to prevent tax evasion by US citizens and US residents through use of offshore accounts.
Foreign financial institutions must identify US accounts, account holders and entities and send XML reports to the IRS.

While most of financial institutions still uses COBOL on mainframe, it lacks advanced XML features support.
So this solution brings features lacking in IBM COBOL ENTREPRISE 4.2.

## FATCA.cbl

Because XML schema to COBOL mapping is not supported by IBM COBOL IBM COBOL ENTREPRISE 4.2
I use the tool legstar-xsd2cob (see link below) to generate a raw mapping between FATCA Xml schemas and Cobol data description.
Then I enhanced it by adding several informations (see FATCA.CBL).
This copy contain Cobol data description compliant with the XML schemas of FATCA Reporting.
It raw version was generated using

## Features

#### XML tags indenting
IN : 
<...><...><...>...</...><...>...</...></...><...>
OUT:
<...>
  <...>
    <...>...</...>
    <...>...</...>
  <...>
<...>

#### Namespaces
IN:
<racine>         + ftc + urn:oecd:ties:fatca:v1
OUT :
<racine xmlns:ftc="urn:oecd:ties:fatca:v1">

#### Schema (xsi)
IN:
<racine>         + urn:oecd:ties:fatca:v1 FatcaXML_v1.1.xsd
OUT :
<racine xsi:schemaLocation="urn:oecd:ties:fatca:v1 FatcaXML_v1.1.xsd">

####
IN : Prefixes
  <MessageSpec>
    <SendingCompanyIN>98Q96B.00000.LE.250</SendingCompanyIN>
    <TransmittingCountry>FR</TransmittingCountry>
OUT :
  <ftc:MessageSpec>
    <sfa:SendingCompanyIN>98Q96B.00000.LE.250</sfa:SendingCompanyIN>
    <sfa:TransmittingCountry>FR</sfa:TransmittingCountry>


