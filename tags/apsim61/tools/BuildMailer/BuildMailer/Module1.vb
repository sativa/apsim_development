Imports System.IO
Imports System.Net
Imports Microsoft.Office.Interop.Outlook
Imports Redemption

Module Module1

    Sub Main()
        Dim myClient As WebClient = New WebClient()
        Dim reader As New StreamReader(myClient.OpenRead("http://apsru-silo/~Reds/BuildSummary.html"))
        Dim body As String = "<html>" + reader.ReadToEnd() + "</html>"
        reader.Close()

        Dim OutlookApplication As Microsoft.Office.Interop.Outlook.Application
        OutlookApplication = New Microsoft.Office.Interop.Outlook.Application()

        Dim OutlookNamespace As Microsoft.Office.Interop.Outlook.NameSpace
        OutlookNamespace = OutlookApplication.GetNamespace("MAPI")
        'OutlookNamespace.Logon("Outlook", "YieldProphet!", Nothing, Nothing)
        OutlookNamespace.Logon("Outlook", "PassW0rd", Nothing, Nothing)

        Dim oItem = OutlookApplication.CreateItem(0) 'Create a new message
        Dim SafeItem = CreateObject("Redemption.SafeMailItem") 'Create an instance of Redemption.SafeMailItem 
        SafeItem.Item = oItem 'set Item property
        SafeItem.Recipients.Add("dean.holzworth@csiro.au")
        SafeItem.Recipients.Add("john.hargreaves@csiro.au")
        SafeItem.Recipients.Add("neil.huth@csiro.au")
        SafeItem.Recipients.Add("peter.devoil@dpi.qld.gov.au")
        SafeItem.Recipients.ResolveAll()
        SafeItem.Subject = "APSIM Build Summary"
        SafeItem.HTMLBody = body
        SafeItem.Send()

        'SafeItem.Close()
        'Outlook.Close()
    End Sub

End Module
