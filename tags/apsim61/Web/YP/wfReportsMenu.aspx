<%@ Page language="c#" Codebehind="wfReportsMenu.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfReportsMenu" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Reports Menu</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 560px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlConsultant" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Height="64px"
								Width="800px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="160px" Height="8px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px" Height="8px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px" Height="8px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="382px" Height="8px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="368px" Height="8px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenuConsultant" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Large"
									Font-Bold="True">GrowerPlaceHolder's  Report Menu</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" BorderStyle="None" BorderColor="White" Height="48px"
								Width="800px">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										tabIndex="4" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px"
										Height="8px" CommandName="wfReportsView.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										tabIndex="5" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px"
										Height="8px" CommandName="wfReportsGenerate.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="152px"
										Height="8px" CommandName="wfReportsFavourites.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:LinkButton id="btnReportsViewMain" style="Z-INDEX: 105; LEFT: 32px; POSITION: absolute; TOP: 264px"
								runat="server" Height="8px" Width="112px" CommandName="wfReportsView.aspx" Font-Names="Arial"
								ForeColor="MediumBlue" Font-Bold="True" tabIndex="1">View Reports</asp:LinkButton>
							<asp:LinkButton id="btnNewReportsMain" style="Z-INDEX: 106; LEFT: 32px; POSITION: absolute; TOP: 336px"
								runat="server" Height="8px" Width="104px" CommandName="wfReportsGenerate.aspx" Font-Names="Arial"
								ForeColor="MediumBlue" Font-Bold="True" tabIndex="2">New Reports</asp:LinkButton>
							<asp:LinkButton id="btnFavouriteReportsMain" style="Z-INDEX: 107; LEFT: 32px; POSITION: absolute; TOP: 400px"
								runat="server" Height="8px" Width="152px" CommandName="wfReportsFavourites.aspx" Font-Names="Arial"
								ForeColor="MediumBlue" Font-Bold="True" tabIndex="3">Manage Favourites</asp:LinkButton>
							<asp:Label id="lblView" style="Z-INDEX: 108; LEFT: 208px; POSITION: absolute; TOP: 264px" runat="server"
								Width="320px" Font-Names="Arial">View previously generated reports for all of your paddocks.</asp:Label>
							<asp:Label id="lblNew" style="Z-INDEX: 109; LEFT: 208px; POSITION: absolute; TOP: 336px" runat="server"
								Width="320px" Font-Names="Arial">Generate multiple reports in one go for any number of paddocks</asp:Label>
							<asp:Label id="lblFavourites" style="Z-INDEX: 110; LEFT: 208px; POSITION: absolute; TOP: 400px"
								runat="server" Width="320px" Font-Names="Arial">A favourite report is a report that you have saved after the first run so you can run it again with out having to re-enter data into all the fields again. Generate, edit or delete your favourite reports here.</asp:Label>
							<asp:Label id="lblSubscribers" style="Z-INDEX: 111; LEFT: 304px; POSITION: absolute; TOP: 232px"
								runat="server" Font-Names="Arial" Font-Bold="True">Subscribers</asp:Label>
							<asp:Label id="lblNewVisitors" style="Z-INDEX: 112; LEFT: 552px; POSITION: absolute; TOP: 336px"
								runat="server" Width="224px" Font-Names="Arial">Visitors are unable to save changes or request a new report</asp:Label>
							<asp:Label id="lblFavouritesVisitors" style="Z-INDEX: 113; LEFT: 552px; POSITION: absolute; TOP: 400px"
								runat="server" Width="216px" Font-Names="Arial">This function is for subscribers only.</asp:Label>
							<asp:Label id="lblVisitors" style="Z-INDEX: 115; LEFT: 608px; POSITION: absolute; TOP: 232px"
								runat="server" Font-Names="Arial" Font-Bold="True">Visitors</asp:Label>
							<asp:Label id="lblViewVisitors" style="Z-INDEX: 116; LEFT: 552px; POSITION: absolute; TOP: 264px"
								runat="server" Height="40px" Width="216px" Font-Names="Arial">Visitors are able to view previously generated reports from a BCG trial paddock only.</asp:Label>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 117; LEFT: 0px; POSITION: absolute; TOP: 536px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 118; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpReportMenuPage" style="Z-INDEX: 134; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
