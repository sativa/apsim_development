<%@ Page language="c#" Codebehind="wfReportsMenuConsultant.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfReportsMenuConsultant" %>
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
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 804px; POSITION: relative; HEIGHT: 544px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Bold="True" Font-Size="X-Large" ForeColor="MediumBlue"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Height="48px"
								Width="800px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="160px" Height="8px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="88px" Height="8px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageGrowers" style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="104px" Height="8px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnMainMenuConsultant" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" Font-Bold="True" Font-Size="Large"
									ForeColor="DarkGray">ConsultantPlaceHolder's Reports</asp:Label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 160px"
								runat="server" BackColor="DarkGray" BorderStyle="None" BorderColor="White" Height="48px"
								Width="800px">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										tabIndex="4" runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="104px"
										Height="8px" CommandName="wfReportsViewConsultant.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										tabIndex="5" runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="104px"
										Height="8px" CommandName="wfReportsGenerateConsultant.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Width="152px"
										Height="8px" CommandName="wfReportsFavouritesConsultant.aspx">Manage Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:LinkButton id="btnReportsViewMain" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 240px"
								runat="server" Height="8px" Width="112px" CommandName="wfReportsViewConsultant.aspx" Font-Names="Arial"
								Font-Bold="True" ForeColor="MediumBlue" tabIndex="1">View Reports</asp:LinkButton>
							<asp:Label id="lblView" style="Z-INDEX: 105; LEFT: 184px; POSITION: absolute; TOP: 240px" runat="server"
								Width="320px" Font-Names="Arial">View previously generated reports for all of your growers and their paddocks.</asp:Label>
							<asp:LinkButton id="btnNewReportsMain" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 320px"
								runat="server" Height="8px" Width="104px" CommandName="wfReportsGenerateConsultant.aspx" Font-Names="Arial"
								Font-Bold="True" ForeColor="MediumBlue" tabIndex="2">New Reports</asp:LinkButton>
							<asp:Label id="lblNew" style="Z-INDEX: 107; LEFT: 192px; POSITION: absolute; TOP: 320px" runat="server"
								Width="320px" Font-Names="Arial">Generate multiple reports in one go for any number of growers and their paddocks</asp:Label>
							<asp:LinkButton id="btnFavouriteReportsMain" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 392px"
								runat="server" Height="8px" Width="152px" CommandName="wfReportsFavouritesConsultant.aspx" Font-Names="Arial"
								Font-Bold="True" ForeColor="MediumBlue" tabIndex="3">Manage Favourites</asp:LinkButton>
							<asp:Label id="lblFavourites" style="Z-INDEX: 109; LEFT: 192px; POSITION: absolute; TOP: 392px"
								runat="server" Width="320px" Font-Names="Arial">A favourite report is a report that you have saved after the first run so you can run it again with out having to re-enter data into all the fields again. Generate, edit or delete your favourite reports here.</asp:Label>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 111; LEFT: 0px; POSITION: absolute; TOP: 520px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 112; LEFT: 64px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpConsultantReportMenuPage" style="Z-INDEX: 134; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
