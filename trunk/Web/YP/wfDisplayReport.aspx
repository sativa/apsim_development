<%@ Page language="c#" Codebehind="wfDisplayReport.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfDisplayReport" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Display Report(s)</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 805px; HEIGHT: 546px" height="546" cellSpacing="0"
				cellPadding="0" width="805" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 804px; POSITION: relative; HEIGHT: 280px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Bold="True" Font-Size="X-Large" ForeColor="MediumBlue"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="64px" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="160px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="88px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="382px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="368px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Width="785px" Height="20px" Font-Names="Arial Black" Font-Bold="True" Font-Size="Medium"
									ForeColor="DarkGray">Display Reports</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" Width="800px" Height="48px" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfReportsView.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfReportsGenerate.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="152px"
										CommandName="wfReportsFavourites.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Button id="btnNextTop" style="Z-INDEX: 106; LEFT: 416px; POSITION: absolute; TOP: 240px"
								runat="server" Width="72px" Height="32px" Text="Next >" tabIndex="2"></asp:Button>
							<asp:Button id="btnPreviousTop" style="Z-INDEX: 107; LEFT: 304px; POSITION: absolute; TOP: 240px"
								runat="server" Width="72px" Height="32px" Text="< Previous" tabIndex="1"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 111; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpDisplayReportPage" style="Z-INDEX: 112; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
						<TABLE id="Table1" style="WIDTH: 800px; HEIGHT: 160px" cellSpacing="1" cellPadding="1"
							width="800" border="0">
							<TR>
								<TD colSpan="3">
									<asp:Table id="tblReports" runat="server" Height="128px" Width="784px" BorderStyle="None"></asp:Table></TD>
							</TR>
							<TR>
								<TD colSpan="3">
									<DIV style="WIDTH: 795px; POSITION: relative; HEIGHT: 88px" ms_positioning="GridLayout">
										<asp:Button id="btnPreviousBottom" style="Z-INDEX: 101; LEFT: 296px; POSITION: absolute; TOP: 8px"
											runat="server" Width="72px" Height="32px" Text="< Previous" tabIndex="3"></asp:Button>
										<asp:Button id="btnNextBottom" style="Z-INDEX: 102; LEFT: 416px; POSITION: absolute; TOP: 8px"
											runat="server" Width="72px" Height="32px" Text="Next >" tabIndex="4"></asp:Button>
										<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 56px"
											runat="server" Height="16px" Width="792px" BackColor="MediumBlue"></asp:Panel></DIV>
								</TD>
							</TR>
						</TABLE>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
