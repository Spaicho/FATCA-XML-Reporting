Private Function ConnectionCheck(ConnectionString As String) As Boolean

    On Error GoTo Err
    ConnectionCheck = True
    
    With Worksheets("Contrats").QueryTables.Add(Connection:=Replace(Replace(ConnectionString, "&1", "mvcihm"), "&2", "Pass_MVCIHM#"), Destination:=Worksheets("Contrats").Range("A1"))
        .Sql = "select count(*) from user_tables"
        .RefreshStyle = xlOverwriteCells
        .FieldNames = False
        .PreserveFormatting = False
        .Refresh BackgroundQuery:=False
    End With
    Worksheets("Contrats").QueryTables.Item(1).Delete
    Exit Function
Err:
    ConnectionCheck = False

End Function

Public Sub RefreshData()
    Dim ConnectionString As String
    Dim ConnectionString10 As String
    Dim ConnectionStringSSL As String
    Dim iStep, iCurState, iStepPct As Double
    Dim iMaxStep As Integer
    
    iMaxStep = 6
    iStep = ActiveSheet.LblProgressBar.Width / iMaxStep
    iCurState = 0
    ActiveSheet.Lblprogress.Width = 0
    ActiveSheet.LblProgressBar.Width = 0
    ActiveSheet.LblProgressBar.Caption = "Calcul en cours"
    ActiveSheet.LblProgressBar.Width = 199.5
    DoEvents
    ActiveSheet.Calculate
    
    'ConnectionString10 = "ODBC;Driver={Oracle dans OraClient10g};Server=(DESCRIPTION =(ADDRESS_LIST =(ADDRESS =(COMMUNITY = tcp.world)(PROTOCOL = TCP)(Host = pmvcp01b.dns20.socgen)(Port = 12200)))(CONNECT_DATA = (SID = PMVCPMVC)));DBQ=pmvcpmvc;uid=&1;Pwd=&2;"
    ConnectionString = "OLEDB;provider=OraOLEDB.Oracle;Data Source=(DESCRIPTION=(CID=GTU_APP)(ADDRESS_LIST =(ADDRESS =(PROTOCOL = TCP)(Host = pmvcp01b.dns20.socgen)(Port = 12200)))(CONNECT_DATA = (SID = PMVCPMVC)(SERVER=DEDICATED)));User Id=&1;Password=&2;"
    ConnectionStringSSL = "OLEDB;provider=OraOLEDB.Oracle;data Source=MVC_PROD_SSL;User Id=&1;Password=&2;"
    If Not ConnectionCheck(ConnectionStringSSL) Then
        If Not ConnectionCheck(ConnectionString) Then
           MsgBox ("une erreur de connexion aux données s'est produite. Merci de vous rapprocher du service informatique")
           End
        Else
            ConnectionString = ConnectionString
        End If
    Else
        ConnectionString = ConnectionStringSSL
    End If
    
    ' refresh Nb totals d'abonnés
    With ActiveSheet.QueryTables.Add(Connection:=Replace(Replace(ConnectionString, "&1", "mvcihm"), "&2", "Pass_MVCIHM#"), Destination:=Range("B6"))
        .Sql = "SELECT COUNT(1) FROM MVC.ETR_REFERENCE_ABONNE WHERE (STCOET = '00' OR (STCOET != 'OO' and to_char(DAREET,'YYYYMM') > '" & Range("B2") & "'))"
        .RefreshStyle = xlOverwriteCells
        .FieldNames = False
        .PreserveFormatting = True
        .Refresh BackgroundQuery:=False
    End With
    ActiveSheet.QueryTables.Item(1).Delete
    iCurState = iCurState + iStep
    ActiveSheet.Lblprogress.Width = iCurState
    
    DoEvents
    ActiveSheet.Calculate
    
        ' Nb souscriptions dans le mois
    With ActiveSheet.QueryTables.Add(Connection:=Replace(Replace(ConnectionString, "&1", "mvcihm"), "&2", "Pass_MVCIHM#"), Destination:=Range("B7"))
        .Sql = "SELECT COUNT(1) FROM MVC.ETR_REFERENCE_ABONNE WHERE (to_char(DAOUET,'YYYYMM') = '" & Range("B2") & "')"
        .RefreshStyle = xlOverwriteCells
        .FieldNames = False
        .PreserveFormatting = True
        .Refresh BackgroundQuery:=False
    End With
    ActiveSheet.QueryTables.Item(1).Delete
    iCurState = iCurState + iStep
    ActiveSheet.Lblprogress.Width = iCurState
    
    DoEvents
    ActiveSheet.Calculate
    
        ' Nb de résiliations dans le mois
    With ActiveSheet.QueryTables.Add(Connection:=Replace(Replace(ConnectionString, "&1", "mvcihm"), "&2", "Pass_MVCIHM#"), Destination:=Range("B8"))
        .Sql = "SELECT COUNT(1) FROM MVC.ETR_REFERENCE_ABONNE WHERE STCOET = '99' AND to_char(DAREET,'YYYYMM') = '" & Range("B2") & "'"
        .RefreshStyle = xlOverwriteCells
        .FieldNames = False
        .PreserveFormatting = True
        .Refresh BackgroundQuery:=False
    End With
    ActiveSheet.QueryTables.Item(1).Delete
    iCurState = iCurState + iStep
    ActiveSheet.Lblprogress.Width = iCurState
    
    DoEvents
    ActiveSheet.Calculate
    
    ' Nb total de contrats avec virement permanent mis en place
    With ActiveSheet.QueryTables.Add(Connection:=Replace(Replace(ConnectionString, "&1", "mvcihm"), "&2", "Pass_MVCIHM#"), Destination:=Range("B9"))
        .Sql = "SELECT COUNT(1) FROM (SELECT DISTINCT IDPRET FROM MVC.ETR_VIREMENTS_PERMANENTS)"
        .RefreshStyle = xlOverwriteCells
        .FieldNames = False
        .PreserveFormatting = True
        .Refresh BackgroundQuery:=False
    End With
    ActiveSheet.QueryTables.Item(1).Delete
    iCurState = iCurState + iStep
    ActiveSheet.Lblprogress.Width = iCurState
    
    DoEvents
    ActiveSheet.Calculate
    
        ' Nb virements permanents mis en place dans le mois
    With ActiveSheet.QueryTables.Add(Connection:=Replace(Replace(ConnectionString, "&1", "mvcihm"), "&2", "Pass_MVCIHM#"), Destination:=Range("B10"))
        .Sql = "SELECT COUNT(1) FROM MVC.ETR_VIREMENTS_PERMANENTS WHERE to_char(DACRVIPE,'YYYYMM') = '" & Range("B2") & "'"
        .RefreshStyle = xlOverwriteCells
        .FieldNames = False
        .PreserveFormatting = True
        .Refresh BackgroundQuery:=False
    End With
    ActiveSheet.QueryTables.Item(1).Delete
    iCurState = iCurState + iStep
    ActiveSheet.Lblprogress.Width = iCurState
    
    DoEvents
    ActiveSheet.Calculate
        
    ActiveSheet.Columns("B:B").ColumnWidth = 10
    ActiveSheet.Columns("G:M").ColumnWidth = 10
    ActiveSheet.LblProgressBar.Caption = "Calcul terminé"
        
End Sub



