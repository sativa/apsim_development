<%@ Page language="c#" Codebehind="wfViewReports.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfViewReports" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfViewReports</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 504px; POSITION: relative; HEIGHT: 40px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnDeleteReport" style="Z-INDEX: 107; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" EnableViewState="False" Font-Size="Smaller">Delete</asp:LinkButton>
					<asp:Label id="lblYear" style="Z-INDEX: 108; LEFT: 280px; POSITION: absolute; TOP: 16px" runat="server"
						Width="32px" Font-Size="Smaller" ForeColor="Blue"> Year:</asp:Label>
					<asp:DropDownList id="cboYear" style="Z-INDEX: 101; LEFT: 312px; POSITION: absolute; TOP: 16px" runat="server"
						Width="64px" AutoPostBack="True">
						<asp:ListItem Value="2003">2003</asp:ListItem>
						<asp:ListItem Value="2004">2004</asp:ListItem>
						<asp:ListItem Value="2005">2005</asp:ListItem>
						<asp:ListItem Value="2006">2006</asp:ListItem>
						<asp:ListItem Value="2007">2007</asp:ListItem>
						<asp:ListItem Value="2008">2008</asp:ListItem>
					</asp:DropDownList>
					<asp:ImageButton id="btnDeleteReportImg" style="Z-INDEX: 106; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnShowReport" style="Z-INDEX: 105; LEFT: 32px; POSITION: absolute; TOP: 16px"
						tabIndex="3" runat="server" EnableViewState="False" Font-Size="Smaller">Show</asp:LinkButton>
					<asp:ImageButton id="btnShowReportImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px"
						tabIndex="2" runat="server" ImageUrl="Images\view.gif"></asp:ImageButton>
					<asp:LinkButton id="btnRenameReport" style="Z-INDEX: 100; LEFT: 184px; POSITION: absolute; TOP: 16px"
						tabIndex="7" runat="server" EnableViewState="False" Font-Size="Smaller">Rename</asp:LinkButton>
					<asp:ImageButton id="btnRenameImg" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" ImageUrl="Images\rename.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:ListBox id="lstReports" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="480px" Height="428px" tabIndex="1"></asp:ListBox>
		</form>
	</body>
</HTML>
