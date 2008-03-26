<%@ Page language="c#" Codebehind="wfMainMenu.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfMainMenu" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Yield Prophet Main Menu</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 569px" height="569" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 643px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" ForeColor="MediumBlue" Font-Names="Arial Black" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Label id="lblPDSubscribers" style="Z-INDEX: 103; LEFT: 216px; POSITION: absolute; TOP: 208px"
								runat="server" Font-Names="Arial" Width="320px">Allows you to change your display name, email address and password.</asp:Label>
							<asp:Label id="lblSubscribers" style="Z-INDEX: 102; LEFT: 224px; POSITION: absolute; TOP: 176px"
								runat="server" Font-Names="Arial" Font-Bold="True">Subscribers</asp:Label>
							<asp:LinkButton id="btnPersonalDetailsMain" style="Z-INDEX: 101; LEFT: 16px; POSITION: absolute; TOP: 208px"
								runat="server" ForeColor="MediumBlue" Font-Names="Arial" Font-Bold="True" Width="152px" Height="8px"
								CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
							<asp:Label id="lblVisitors" style="Z-INDEX: 104; LEFT: 600px; POSITION: absolute; TOP: 176px"
								runat="server" Font-Names="Arial" Font-Bold="True">Visitors</asp:Label>
							<asp:Label id="lblPDVisitors" style="Z-INDEX: 105; LEFT: 600px; POSITION: absolute; TOP: 208px"
								runat="server" Font-Names="Arial" Width="176px">Visitors are unable to change personal details.</asp:Label>
							<asp:Panel id="pnlConsultantMain" style="Z-INDEX: 106; LEFT: 0px; POSITION: absolute; TOP: 288px"
								runat="server" Width="800px" BackColor="Transparent" BorderStyle="None" BorderColor="White"
								Height="168px">
								<DIV id="divConsultantMain" style="WIDTH: 800px; POSITION: relative; HEIGHT: 152px" ms_positioning="GridLayout">
									<asp:LinkButton id="btnManageGrowersMain" style="Z-INDEX: 100; LEFT: 16px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" Width="96px" CommandName="wfManageGrowers.aspx"
										Height="8px">My Growers</asp:LinkButton>
									<asp:Label id="lblGrowersSubscribers" style="Z-INDEX: 102; LEFT: 216px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="320px">Allows you to view and edit your growers' paddocks.  And generate and view reports for a single grower</asp:Label>
									<asp:LinkButton id="btnManageReportsMain" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 104px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" Width="96px" CommandName="wfReportsMenuConsultant.aspx"
										Height="8px">My Reports</asp:LinkButton>
									<asp:Label id="lblReportsSubscribersCon" style="Z-INDEX: 105; LEFT: 216px; POSITION: absolute; TOP: 104px"
										runat="server" Font-Names="Arial" Width="256px">Allows you to generate and view reports for multiple growers at once</asp:Label></DIV>
							</asp:Panel>
							<asp:Panel id="pnlGrowerMain" style="Z-INDEX: 107; LEFT: 0px; POSITION: absolute; TOP: 288px"
								runat="server" Width="800px" BackColor="Transparent" BorderStyle="None" BorderColor="White"
								Height="234px">
								<DIV id="divGrowersMain" style="WIDTH: 800px; POSITION: relative; HEIGHT: 242px" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksMain" style="Z-INDEX: 100; LEFT: 16px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" Width="104px" CommandName="wfPaddocksMenu.aspx"
										Height="8px">My Paddocks</asp:LinkButton>
									<asp:Label id="lblPaddocksSubscribers" style="Z-INDEX: 101; LEFT: 216px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="320px">Use this menu to enter in-crop management events (sowing date, crop and variety and date and rate of nitrogen fertiliser application, as well as rainfall) or try entering ‘what if’ events to see how they will affect simulation results.</asp:Label>
									<asp:LinkButton id="btnReportsMain" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 136px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" Width="96px" CommandName="wfReportsMenu.aspx"
										Height="8px">My Reports</asp:LinkButton>
									<asp:Label id="lblReportsGrowerSubscriber" style="Z-INDEX: 103; LEFT: 216px; POSITION: absolute; TOP: 144px"
										runat="server" Font-Names="Arial" Width="328px">Choose from six different reports in the drop-down menu: Agronomic Report, Climate Report, Nitrogen Comparison Report, Nitrogen Profit Report, Sowing Time X Variety Report and Fallow Report.</asp:Label>
									<asp:Label id="lblPaddockVisitors" style="Z-INDEX: 104; LEFT: 592px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="176px">View the setup data from a BCG trial paddock only.</asp:Label>
									<asp:Label id="lblReportsGrowerVisitor" style="Z-INDEX: 105; LEFT: 592px; POSITION: absolute; TOP: 144px"
										runat="server" Font-Names="Arial" Width="176px">View previously generated reports. Visitors are unable to save changes or request a new report.</asp:Label></DIV>
							</asp:Panel>
							<asp:Panel id="pnlAdministratorMain" style="Z-INDEX: 108; LEFT: 0px; POSITION: absolute; TOP: 496px"
								runat="server" Width="800px" BackColor="Transparent" BorderStyle="None" BorderColor="White"
								Height="50px">
								<DIV id="divAdministratorMain" style="WIDTH: 800px; POSITION: relative; HEIGHT: 58px"
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 100; LEFT: 16px; POSITION: absolute; TOP: 16px" runat="server"
										Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" Width="160px" CommandName="wfAdminMenu.aspx"
										Height="8px">Administration Tools</asp:LinkButton>
									<asp:Label id="lblAdministratorSubscriber" style="Z-INDEX: 102; LEFT: 216px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="320px">Allows you to make changes to the settings that run Yield Propet</asp:Label></DIV>
							</asp:Panel>
							<asp:Panel id="pnlNavigationMenu" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" Width="800px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White"
								Height="40px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="160px" CommandName="wfPersonalDetails.aspx"
										Height="8px">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="88px" CommandName="wfReportsMenuConsultant.aspx"
										Height="8px">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="104px" CommandName="wfManageGrowers.aspx"
										Height="8px">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="112px" CommandName="wfMainMenu.aspx"
										Height="8px">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 111; LEFT: 0px; POSITION: absolute; TOP: 624px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:LinkButton id="btnLogout" style="Z-INDEX: 112; LEFT: 368px; POSITION: absolute; TOP: 592px"
								runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="MediumBlue" Width="56px" Height="8px">Logout</asp:LinkButton>
							<asp:Image id="imgBanner" style="Z-INDEX: 113; LEFT: 72px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<DIV style="Z-INDEX: 110; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 40px"
								align="center" ms_positioning="text2D">
								<asp:Label id="lblHeading" runat="server" Font-Bold="True" Font-Size="Large" Font-Names="Arial Black"
									ForeColor="DarkGray" Width="799px" Height="20px">UserPlaceHolder's Main Menu</asp:Label></DIV>
							<asp:ImageButton id="btnHelpMainMenuPage" style="Z-INDEX: 114; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
						</DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
