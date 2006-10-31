<%@ Page language="c#" Codebehind="wfAdminCrops.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminCrops" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Manage Crops</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 739px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black"> Yield Prophet<sup>
									�</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Width="800px"
								Height="40px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										tabIndex="7" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px"
										Width="88px" CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" tabIndex="6"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="168px"
										CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Large" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="785px"
									Height="20px">Manage Crops</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 720px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:Label id="lblCrops" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 200px"
								runat="server" Font-Names="Arial" Width="40px" Height="16px">Crops:</asp:Label>
							<asp:ListBox id="lstCultivars" style="Z-INDEX: 105; LEFT: 232px; POSITION: absolute; TOP: 264px"
								tabIndex="2" runat="server" Font-Names="Arial" Width="312px" Height="360px"></asp:ListBox>
							<asp:Label id="lblCultivars" style="Z-INDEX: 106; LEFT: 152px; POSITION: absolute; TOP: 264px"
								runat="server" Font-Names="Arial" Width="64px" Height="16px">Cultivars:</asp:Label>
							<asp:DropDownList id="cboCrops" style="Z-INDEX: 107; LEFT: 232px; POSITION: absolute; TOP: 200px"
								tabIndex="1" runat="server" Font-Names="Arial" Width="312px" AutoPostBack="True"></asp:DropDownList>
							<asp:Button id="btnImport" style="Z-INDEX: 108; LEFT: 120px; POSITION: absolute; TOP: 656px"
								runat="server" Width="120px" Height="32px" Text="Import->" tabIndex="4"></asp:Button><INPUT class="stdInput" id="flImport" style="Z-INDEX: 111; LEFT: 240px; FONT-FAMILY: Arial; POSITION: absolute; TOP: 656px; HEIGHT: 32px"
								tabIndex="3" type="file" size="25" name="flImport" runat="server">
							<asp:Button id="btnDeleteCultivar" style="Z-INDEX: 112; LEFT: 560px; POSITION: absolute; TOP: 656px"
								runat="server" Width="120px" Height="32px" Text="Delete cultivar" tabIndex="5"></asp:Button></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
