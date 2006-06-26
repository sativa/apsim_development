<%@ Page language="c#" Codebehind="wfAdminMenu.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminMenu" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Administration Menu</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 792px" height="792" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 779px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Bold="True" Font-Size="X-Large" Font-Names="Arial Black" ForeColor="MediumBlue"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Height="40px"
								Width="800px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Names="Arial" Font-Bold="True" Width="88px" Height="8px"
										CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" runat="server"
										ForeColor="White" Font-Names="Arial" Font-Bold="True" Width="168px" Height="8px" CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Bold="True" Font-Size="Large" Font-Names="Arial Black"
									ForeColor="DarkGray">Administration Menu</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 760px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:LinkButton id="btnClimateForcast" style="Z-INDEX: 104; LEFT: 48px; POSITION: absolute; TOP: 264px"
								runat="server" Height="8px" Width="128px" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue"
								CommandName="wfAdminClimateForecast.aspx" tabIndex="1">Climate Forcast</asp:LinkButton>
							<asp:LinkButton id="btnSQL" style="Z-INDEX: 105; LEFT: 48px; POSITION: absolute; TOP: 624px" runat="server"
								Height="8px" Width="88px" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" CommandName="wfAdminSQL.aspx"
								tabIndex="4">SQL Entry</asp:LinkButton>
							<asp:LinkButton id="btnSoils" style="Z-INDEX: 106; LEFT: 48px; POSITION: absolute; TOP: 552px" runat="server"
								Height="8px" Width="136px" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" CommandName="wfAdminSoilsView.aspx"
								tabIndex="6">Soil Management</asp:LinkButton>
							<asp:LinkButton id="btnMetStations" style="Z-INDEX: 107; LEFT: 48px; POSITION: absolute; TOP: 408px"
								runat="server" Height="8px" Width="192px" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue"
								CommandName="wfAdminMetStationsView.aspx" tabIndex="3">Met Station Management</asp:LinkButton>
							<asp:LinkButton id="btnCrops" style="Z-INDEX: 108; LEFT: 48px; POSITION: absolute; TOP: 336px" runat="server"
								Height="8px" Width="144px" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" CommandName="wfAdminCrops.aspx"
								tabIndex="5">Crop Management</asp:LinkButton>
							<asp:LinkButton id="btnReportTemplates" style="Z-INDEX: 109; LEFT: 48px; POSITION: absolute; TOP: 480px"
								runat="server" Height="8px" Width="232px" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue"
								CommandName="wfAdminReportTemplateMap.aspx" tabIndex="2">Report Template Management</asp:LinkButton>
							<asp:LinkButton id="btnBannerImages" style="Z-INDEX: 110; LEFT: 48px; POSITION: absolute; TOP: 192px"
								runat="server" ForeColor="MediumBlue" Font-Names="Arial" Font-Bold="True" Width="216px" Height="8px"
								CommandName="wfAdminBannerImagesView.aspx" tabIndex="7">Banner Image Management</asp:LinkButton>
							<asp:Label id="lblBannerImages" style="Z-INDEX: 111; LEFT: 368px; POSITION: absolute; TOP: 192px"
								runat="server" Font-Names="Arial" Width="336px">Allows you to import image files to be used as banner images into the system</asp:Label>
							<asp:LinkButton id="btnDataManagement" style="Z-INDEX: 112; LEFT: 48px; POSITION: absolute; TOP: 696px"
								tabIndex="5" runat="server" ForeColor="MediumBlue" Font-Names="Arial" Font-Bold="True" Width="248px"
								Height="8px" CommandName="wfAdminDataManagement.aspx">User and Paddock Management</asp:LinkButton>
							<asp:Label id="lblClimateForcast" style="Z-INDEX: 113; LEFT: 368px; POSITION: absolute; TOP: 264px"
								runat="server" Font-Names="Arial" Width="336px">Allows you to view and update the climate information used to generate the climate report</asp:Label>
							<asp:Label id="lblCrops" style="Z-INDEX: 114; LEFT: 368px; POSITION: absolute; TOP: 336px"
								runat="server" Font-Names="Arial" Width="336px">Allows you to add to or delete from the list of cultivars that are available to the user</asp:Label>
							<asp:Label id="lblMetStations" style="Z-INDEX: 116; LEFT: 368px; POSITION: absolute; TOP: 408px"
								runat="server" Font-Names="Arial" Width="336px">Allows you to add to or delete from the list of met stations that are available to the user</asp:Label>
							<asp:Label id="lblReportTemplates" style="Z-INDEX: 117; LEFT: 368px; POSITION: absolute; TOP: 480px"
								runat="server" Font-Names="Arial" Width="336px">Allows you to add/delete/modify/assign the templates used in the report generation process</asp:Label>
							<asp:Label id="Label2" style="Z-INDEX: 118; LEFT: 368px; POSITION: absolute; TOP: 552px" runat="server"
								Font-Names="Arial" Width="336px">Allows you to add to or delete from the list of soils  that are available to the user</asp:Label>
							<asp:Label id="Label3" style="Z-INDEX: 119; LEFT: 368px; POSITION: absolute; TOP: 624px" runat="server"
								Font-Names="Arial" Width="336px">Allows you to execute an SQL statement on the database</asp:Label>
							<asp:Label id="Label4" style="Z-INDEX: 120; LEFT: 368px; POSITION: absolute; TOP: 696px" runat="server"
								Font-Names="Arial" Width="336px">Allows you to copy/paste/export user and paddock  information</asp:Label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
