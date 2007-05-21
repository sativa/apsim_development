<%@ Page language="c#" Codebehind="wfSessionTimeOut.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfSessionTimeOut" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Session Time Out</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 800px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 504px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 101; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Large" Height="20px" Width="785px" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black">Session Time Out</asp:label></DIV>
							<asp:Label id="lblDetail" style="Z-INDEX: 102; LEFT: 40px; POSITION: absolute; TOP: 232px"
								runat="server" Width="720px" Font-Names="Arial">Your session has expired.  To continue using Yield Prophet please login again. Sorry for any inconvience</asp:Label>
							<asp:HyperLink id="hylLogin" style="Z-INDEX: 104; LEFT: 368px; POSITION: absolute; TOP: 272px"
								runat="server" NavigateUrl="wfLogin.aspx" Target="_parent" tabIndex="1" Font-Names="Arial">Login</asp:HyperLink>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 488px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
