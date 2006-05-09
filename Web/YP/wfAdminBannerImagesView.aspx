<%@ Page language="c#" Codebehind="wfAdminBannerImagesView.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminBannerImagesView" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Manage Banner Images</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 627px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Width="800px"
								Height="40px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										tabIndex="5" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px"
										Width="88px" CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" tabIndex="4"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="168px"
										CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Large" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="785px"
									Height="20px">Manage Banner Images</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 608px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:listbox id="lstBannerImages" style="Z-INDEX: 116; LEFT: 200px; POSITION: absolute; TOP: 184px"
								tabIndex="1" runat="server" Font-Names="Arial" Width="416px" Height="312px" SelectionMode="Multiple"></asp:listbox>
							<asp:Button id="btnImport" style="Z-INDEX: 122; LEFT: 216px; POSITION: absolute; TOP: 520px"
								runat="server" Width="120px" Height="32px" Text="Import->" tabIndex="3"></asp:Button><INPUT class="stdInput" id="flImport" style="Z-INDEX: 123; LEFT: 336px; FONT-FAMILY: Arial; POSITION: absolute; TOP: 520px; HEIGHT: 32px"
								tabIndex="2" type="file" size="25" name="flImport" runat="server">
							<asp:label id="lblBannerImages" style="Z-INDEX: 125; LEFT: 72px; POSITION: absolute; TOP: 184px"
								runat="server" Font-Names="Arial" Width="113px" Height="16px">Banner Images:</asp:label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
