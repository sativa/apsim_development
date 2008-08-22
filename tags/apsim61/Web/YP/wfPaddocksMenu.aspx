<%@ Page language="c#" Codebehind="wfPaddocksMenu.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPaddocksMenu" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock Menu</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 800px; HEIGHT: 712px" height="712" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 795px" align="left" ms_positioning="GridLayout"><asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>®</sup></asp:label><asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Height="64px" Width="800px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
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
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout"><asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Large" Font-Bold="True">GrowerPlaceHolder's Paddocks</asp:label></DIV>
							<asp:dropdownlist id="cboPaddocks" style="Z-INDEX: 103; LEFT: 400px; POSITION: absolute; TOP: 248px"
								runat="server" Height="24px" Width="272px" Font-Names="Arial" tabIndex="1"></asp:dropdownlist><asp:label id="lblSelectPaddock" style="Z-INDEX: 104; LEFT: 72px; POSITION: absolute; TOP: 248px"
								runat="server" Font-Names="Arial">Use the drop down menu to select a paddock:</asp:label><asp:panel id="pnlPaddock" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" BorderStyle="None" BorderColor="White" Height="40px" Width="800px">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksRainfall" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="7" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="144px"
										Height="8px" CommandName="wfPaddocksRainfall.aspx">Paddock's Rainfall</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksApplictions" style="Z-INDEX: 101; LEFT: 288px; POSITION: absolute; TOP: 8px"
										tabIndex="9" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="184px"
										Height="8px" CommandName="wfPaddocksApplications.aspx">Paddock's Applications</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksSoil" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="8" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="120px"
										Height="8px" CommandName="wfPaddocksSoil.aspx">Paddock's Soil</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksCrop" style="Z-INDEX: 103; LEFT: 480px; POSITION: absolute; TOP: 8px"
										tabIndex="10" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="128px"
										Height="8px" CommandName="wfPaddocksCrop.aspx">Paddock's Crop</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksInformation" style="Z-INDEX: 104; LEFT: 616px; POSITION: absolute; TOP: 8px"
										tabIndex="11" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="176px"
										Height="8px" CommandName="wfPaddocksInformation.aspx">Paddock's Information</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Label id="lblSubscribers" style="Z-INDEX: 106; LEFT: 312px; POSITION: absolute; TOP: 368px"
								runat="server" Font-Bold="True" Font-Names="Arial">Subscribers</asp:Label>
							<asp:Label id="lblRainfallSub" style="Z-INDEX: 107; LEFT: 200px; POSITION: absolute; TOP: 400px"
								runat="server" Font-Names="Arial" Width="320px">Allows you to edit and view your slected paddock's rainfall history</asp:Label>
							<asp:Label id="lblVisitors" style="Z-INDEX: 108; LEFT: 608px; POSITION: absolute; TOP: 368px"
								runat="server" Font-Bold="True" Font-Names="Arial">Visitors</asp:Label>
							<asp:Label id="lblRainfallVis" style="Z-INDEX: 109; LEFT: 544px; POSITION: absolute; TOP: 400px"
								runat="server" Font-Names="Arial" Width="248px">Visitors can view a BCG trial paddock's rainfall history</asp:Label>
							<asp:LinkButton id="btnPaddocksInformationMain" style="Z-INDEX: 110; LEFT: 8px; POSITION: absolute; TOP: 688px"
								runat="server" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial" Width="168px" Height="8px"
								CommandName="wfPaddocksInformation.aspx" tabIndex="6">Paddocks Information</asp:LinkButton>
							<asp:LinkButton id="btnPaddocksSoilMain" style="Z-INDEX: 111; LEFT: 8px; POSITION: absolute; TOP: 472px"
								runat="server" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial" Width="112px" Height="8px"
								CommandName="wfPaddocksSoil.aspx" tabIndex="3">Paddocks Soil</asp:LinkButton>
							<asp:LinkButton id="btnPaddocksRainfallMain" style="Z-INDEX: 113; LEFT: 8px; POSITION: absolute; TOP: 400px"
								runat="server" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial" Width="144px" Height="8px"
								CommandName="wfPaddocksRainfall.aspx" tabIndex="2">Paddocks Rainfall</asp:LinkButton>
							<asp:LinkButton id="btnPaddocksApplicationsMain" style="Z-INDEX: 114; LEFT: 8px; POSITION: absolute; TOP: 544px"
								runat="server" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial" Width="176px" Height="8px"
								CommandName="wfPaddocksApplications.aspx" tabIndex="4">Paddocks Applications</asp:LinkButton>
							<asp:LinkButton id="btnPaddocksCropMain" style="Z-INDEX: 115; LEFT: 8px; POSITION: absolute; TOP: 616px"
								runat="server" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial" Width="120px" Height="8px"
								CommandName="wfPaddocksCrop.aspx" tabIndex="5">Paddocks Crop</asp:LinkButton>
							<asp:Label id="lblAppsVis" style="Z-INDEX: 116; LEFT: 544px; POSITION: absolute; TOP: 544px"
								runat="server" Font-Names="Arial" Width="248px">Visitors can view a BCG trial paddock's irrigation and nitrogen applications</asp:Label>
							<asp:Label id="lblSoilVis" style="Z-INDEX: 117; LEFT: 544px; POSITION: absolute; TOP: 472px"
								runat="server" Font-Names="Arial" Width="248px">Visitors can view a BCG trial paddock's soil details</asp:Label>
							<asp:Label id="lblInfoVis" style="Z-INDEX: 118; LEFT: 544px; POSITION: absolute; TOP: 616px"
								runat="server" Font-Names="Arial" Width="248px">Visitors can view a BCG trial paddock's current crop</asp:Label>
							<asp:Label id="lblCropVis" style="Z-INDEX: 119; LEFT: 544px; POSITION: absolute; TOP: 688px"
								runat="server" Font-Names="Arial" Width="248px">Visitors can view a BCG trial paddock's basic information</asp:Label>
							<asp:Label id="lblAppsSub" style="Z-INDEX: 120; LEFT: 200px; POSITION: absolute; TOP: 544px"
								runat="server" Font-Names="Arial" Width="320px">Allows you to edit and view your selected paddock's nitrogen and irrigation applications</asp:Label>
							<asp:Label id="lblInfoSub" style="Z-INDEX: 121; LEFT: 200px; POSITION: absolute; TOP: 688px"
								runat="server" Font-Names="Arial" Width="320px">Allows you to edit and view your selected paddock's basic information, such as its  name, region and weather station</asp:Label>
							<asp:Label id="lblSoilSub" style="Z-INDEX: 122; LEFT: 200px; POSITION: absolute; TOP: 472px"
								runat="server" Font-Names="Arial" Width="320px">Allows you to edit and view your selected paddock's soil details</asp:Label>
							<asp:Label id="lblCropSub" style="Z-INDEX: 123; LEFT: 200px; POSITION: absolute; TOP: 616px"
								runat="server" Font-Names="Arial" Width="320px">Allows you to edit and view your selected paddock's current crop</asp:Label>
							<asp:Panel id="pnlAdminOptions" style="Z-INDEX: 124; LEFT: 0px; POSITION: absolute; TOP: 288px"
								runat="server" Width="800px" Height="55px">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 50px" ms_positioning="GridLayout">
									<asp:Button id="btnAddPaddock" style="Z-INDEX: 101; LEFT: 184px; POSITION: absolute; TOP: 8px"
										tabIndex="12" runat="server" Width="170px" Height="32px" Text="Add new paddock"></asp:Button>
									<asp:Button id="btnDeletePaddock" style="Z-INDEX: 103; LEFT: 448px; POSITION: absolute; TOP: 8px"
										tabIndex="13" runat="server" Width="170px" Height="32px" Text="Delete current paddock"></asp:Button></DIV>
							</asp:Panel>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 125; LEFT: 0px; POSITION: absolute; TOP: 776px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 126; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpPaddockMenuPage" style="Z-INDEX: 127; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
