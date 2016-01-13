# FATCA-XML-Reporting

FATCA is a new US law aimed to prevent tax evasion by US citizens and US residents through use of offshore accounts.
Foreign financial institutions must identify US accounts, account holders and entities and send XML reports to the IRS.

While most of financial institutions still uses COBOL on mainframe, it lacks advanced XML features support.
So this solution brings features lacking in IBM COBOL ENTREPRISE 4.2.

## FATCA.cbl

Because XML schema to COBOL mapping is not supported by IBM COBOL IBM COBOL ENTREPRISE 4.2,
I use the tool legstar-xsd2cob (see link below) to generate a raw mapping between FATCA Xml schemas and Cobol data description.

Then I enhanced it by adding several informations (data starting with the prefix "tech"). 
those information will be used by the module in order to add features (see below)

https://code.google.com/p/legstar-xsd2cob/

## MFUSXL00.cbl

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

#### Namespace

IN:

<racine>         + ftc + urn:oecd:ties:fatca:v1

OUT:

<racine xmlns:ftc="urn:oecd:ties:fatca:v1">

#### Schema

IN:

<racine>         + urn:oecd:ties:fatca:v1 FatcaXML_v1.1.xsd
OUT :

<racine xsi:schemaLocation="urn:oecd:ties:fatca:v1 FatcaXML_v1.1.xsd">

#### Prefixes

IN :

  <MessageSpec>
    <SendingCompanyIN>98Q96B.00000.LE.250</SendingCompanyIN>
    <TransmittingCountry>FR</TransmittingCountry>
    
OUT :

  <ftc:MessageSpec>
    <sfa:SendingCompanyIN>98Q96B.00000.LE.250</sfa:SendingCompanyIN>
    <sfa:TransmittingCountry>FR</sfa:TransmittingCountry>


#### Attributes

IN :

<AccountBalance>
  <tech-attr-req-currCode>EUR </tech-attr-req-currCode>
</AccountBalance>

OUT :

<AccountBalance currCode="EUR">
</AccountBalance>

#### Text

IN :

<AccountBalance>
  <tech-text>9000000000000.00</tech-text>
</AccountBalance>

OUT :

<AccountBalance>9000000000000000.00</AccountBalance>


#### Cobol Reserved Words

IN :
<R-Address></R-Address>
<R-Type></R-Type>
<R-Title></R-Title>
OUT :
<Address></Address>
<Type></Type>
<Title></Title>

#### XML Declaration

OUT :
<?xml version="1.0" encoding="UTF-8"?>

