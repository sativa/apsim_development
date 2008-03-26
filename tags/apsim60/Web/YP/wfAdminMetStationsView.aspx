<%@ Page language="c#" Codebehind="wfAdminMetStationsView.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminMetStationsView" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>View Met Stations</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 736px" height="736" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 736px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px" Height="8px"
										CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" runat="server"
										Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="168px" Height="8px" CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Large" Height="20px"
									Width="785px">Manage Met Stations</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 712px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:Label id="lblMultipleSelect" style="Z-INDEX: 105; LEFT: 272px; POSITION: absolute; TOP: 560px"
								runat="server" Font-Names="Arial" Font-Size="Smaller" Height="16px" Width="232px">Hold Ctrl to select multiple met stations</asp:Label>
							<asp:label id="lblRegions" style="Z-INDEX: 114; LEFT: 104px; POSITION: absolute; TOP: 176px"
								runat="server" Font-Names="Arial" Height="16px" Width="56px">Regions:</asp:label>
							<asp:dropdownlist id="cboRegions" style="Z-INDEX: 115; LEFT: 176px; POSITION: absolute; TOP: 176px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="416px" AutoPostBack="True"></asp:dropdownlist>
							<asp:listbox id="lstMetStations" style="Z-INDEX: 116; LEFT: 176px; POSITION: absolute; TOP: 248px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="312px" Width="416px" SelectionMode="Multiple"></asp:listbox>
							<asp:label id="lblMetStations" style="Z-INDEX: 117; LEFT: 72px; POSITION: absolute; TOP: 248px"
								runat="server" Font-Names="Arial" Height="16px" Width="88px">Met stations:</asp:label>
							<asp:Button id="btnEdit" style="Z-INDEX: 118; LEFT: 32px; POSITION: absolute; TOP: 616px" runat="server"
								Height="32px" Width="129px" Text="Edit met station" tabIndex="3"></asp:Button>
							<asp:Button id="btnImport" style="Z-INDEX: 119; LEFT: 192px; POSITION: absolute; TOP: 616px"
								runat="server" Height="32px" Width="120px" Text="Import->" tabIndex="5"></asp:Button><INPUT class="stdInput" id="flImport" style="Z-INDEX: 120; LEFT: 312px; FONT-FAMILY: Arial; POSITION: absolute; TOP: 616px; HEIGHT: 32px"
								tabIndex="4" type="file" size="25" name="flImport" runat="server">
							<asp:Button id="btnDelete" style="Z-INDEX: 121; LEFT: 608px; POSITION: absolute; TOP: 616px"
								runat="server" Height="32px" Width="136px" Text="Delete met station(s)" tabIndex="6"></asp:Button></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
