<%@ Page language="c#" Codebehind="wfSessionTimeOut.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfSessionTimeOut" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfSessionTimeOut</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Label id="lblWarning" style="Z-INDEX: 101; LEFT: 200px; POSITION: absolute; TOP: 96px"
				runat="server" Width="224px" Height="40px" Font-Size="X-Large">Session Time Out</asp:Label>
			<asp:Label id="lblDetail" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 200px"
				runat="server" Width="624px">Your session has expired.  To continue using Yield Prophet please login again. Sorry for any inconvience</asp:Label>
			<asp:HyperLink id="hylLogin" style="Z-INDEX: 103; LEFT: 288px; POSITION: absolute; TOP: 264px"
				runat="server" NavigateUrl="wfLogin.aspx" Target="_parent" tabIndex="1">Login</asp:HyperLink>
		</form>
	</body>
</HTML>
