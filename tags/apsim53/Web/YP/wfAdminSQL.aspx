<%@ Page language="c#" Codebehind="wfAdminSQL.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminSQL" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>SQL Entry</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 507px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Width="800px"
								Height="40px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="88px"
										CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" runat="server"
										Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="168px" CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Large" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="785px"
									Height="20px">SQL Entry</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 488px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:Button id="btnExecute" style="Z-INDEX: 105; LEFT: 232px; POSITION: absolute; TOP: 440px"
								runat="server" Width="120px" Height="32px" Text="Execute SQL" tabIndex="2"></asp:Button>
							<asp:Button id="btnClear" style="Z-INDEX: 106; LEFT: 384px; POSITION: absolute; TOP: 440px"
								runat="server" Width="120px" Height="32px" Text="Clear SQL" tabIndex="3"></asp:Button>
							<asp:TextBox id="edtSQL" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 224px" tabIndex="1"
								runat="server" Font-Names="Arial" Width="744px" Height="24px"></asp:TextBox>
							<asp:Label id="lblSQL" style="Z-INDEX: 108; LEFT: 24px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial">Enter SQL statement (be carefull with this option!):</asp:Label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
