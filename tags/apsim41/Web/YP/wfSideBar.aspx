<%@ Page language="c#" Codebehind="wfSideBar.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfSideBar" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfSideBar</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
		<BASE target="main">
	</HEAD>
	<body MS_POSITIONING="GridLayout" bgColor="palegoldenrod">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlSideBar" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="536px" Width="160px" Wrap="False"
				tabIndex="-1">
				<P></P>
				<DIV id="SidePanelGrid" style="WIDTH: 161px; POSITION: relative; HEIGHT: 504px" tabIndex="-1"
					ms_positioning="GridLayout">
					<asp:Panel id="pnlGrower" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 8px" tabIndex="-1"
						runat="server" Width="160px" Height="172px" HorizontalAlign="Left">
						<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod"></P>
						<DIV style="WIDTH: 160px; POSITION: relative; HEIGHT: 164px" tabIndex="-1" ms_positioning="GridLayout">
							<P>
								<asp:HyperLink id="hylUserDetails" style="Z-INDEX: 101; LEFT: 40px; POSITION: absolute; TOP: 40px"
									tabIndex="2" runat="server" Font-Size="Smaller" NavigateUrl="wfEditUserDetails.aspx">User Details</asp:HyperLink></P>
							<P>
								<asp:HyperLink id="hylUserDetailsImg" style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 32px"
									tabIndex="1" runat="server" Font-Size="Smaller" NavigateUrl="wfUserDetails.aspx" ImageUrl="Images\users_details.gif"></asp:HyperLink>
								<asp:Label id="lblGrower" style="Z-INDEX: 103; LEFT: 56px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Bold="True" ForeColor="Blue">Grower</asp:Label></P>
							<asp:DropDownList id="cboPaddocks" style="Z-INDEX: 104; LEFT: 40px; POSITION: absolute; TOP: 104px"
								tabIndex="6" runat="server" Width="112px" Font-Size="X-Small"></asp:DropDownList>
							<asp:Button id="btnView" style="Z-INDEX: 105; LEFT: 56px; POSITION: absolute; TOP: 136px" tabIndex="7"
								runat="server" Width="48px" Height="24px" Font-Size="X-Small" Text="View"></asp:Button>
							<asp:Button id="btnViewReports" style="Z-INDEX: 106; LEFT: 40px; POSITION: absolute; TOP: 72px"
								tabIndex="4" runat="server" Width="80px" Height="16px" BackColor="Transparent" Font-Size="Smaller"
								ForeColor="Blue" Text="View Reports" BorderStyle="None" Font-Underline="True" Font-Names="Times New Roman"
								BorderWidth="0px"></asp:Button>
							<asp:ImageButton id="btnViewReportsImg" style="Z-INDEX: 107; LEFT: 8px; POSITION: absolute; TOP: 64px"
								tabIndex="3" runat="server" ImageUrl="Images\reports.gif"></asp:ImageButton>
							<asp:ImageButton id="btnViewPaddocksImg" style="Z-INDEX: 108; LEFT: 8px; POSITION: absolute; TOP: 104px"
								runat="server" ImageUrl="Images\paddock.gif"></asp:ImageButton></DIV>
					</asp:Panel>
					<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod">
						<asp:Panel id="Panel1" style="Z-INDEX: 105; LEFT: -32000px; POSITION: absolute; TOP: -32000px"
							runat="server" Height="342px" HorizontalAlign="Left"></P>
					<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 307px" ms_positioning="GridLayout">
						<asp:HyperLink id="hylSQLImg" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 240px" runat="server"
							Font-Size="Smaller" NavigateUrl="wfSQL.aspx" ImageUrl="file:///C:\Inetpub\wwwroot\YieldProphet\Images\sql.gif"></asp:HyperLink>
						<asp:HyperLink id="hylSQL" style="Z-INDEX: 102; LEFT: 40px; POSITION: absolute; TOP: 248px" runat="server"
							Font-Size="Smaller" NavigateUrl="wfSQL.aspx">SQL</asp:HyperLink>
						<asp:HyperLink id="hylDropDownsImg" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 200px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfViewDropDowns.aspx" ImageUrl="file:///C:\Inetpub\wwwroot\YieldProphet\Images\drop_downs.gif"></asp:HyperLink>
						<asp:HyperLink id="hylDropDowns" style="Z-INDEX: 104; LEFT: 40px; POSITION: absolute; TOP: 208px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfViewDropDowns.aspx">Drop Downs</asp:HyperLink>
						<asp:HyperLink id="hylAddUserImg" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 40px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfAddUser.aspx" ImageUrl="Images\growers.gif"></asp:HyperLink>
						<asp:HyperLink id="hylAddUser" style="Z-INDEX: 106; LEFT: 40px; POSITION: absolute; TOP: 48px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfAddUser.aspx">Add User</asp:HyperLink>
						<asp:Label id="lblAdministration" style="Z-INDEX: 107; LEFT: 32px; POSITION: absolute; TOP: 8px"
							runat="server" ForeColor="Blue">Administration</asp:Label>
						<asp:HyperLink id="hylReportTemplatesImg" style="Z-INDEX: 110; LEFT: 8px; POSITION: absolute; TOP: 80px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfReportTemplate.aspx" ImageUrl="Images\apsim_report_gen.gif"></asp:HyperLink>
						<asp:HyperLink id="hylReportTemplates" style="Z-INDEX: 111; LEFT: 40px; POSITION: absolute; TOP: 88px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfReportTemplate.aspx">Report Templates</asp:HyperLink>
						<asp:HyperLink id="hylCropsImg" style="Z-INDEX: 112; LEFT: 8px; POSITION: absolute; TOP: 120px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfViewCrops.aspx" ImageUrl="file:///C:\Inetpub\wwwroot\YieldProphet\Images\soils.gif"></asp:HyperLink>
						<asp:HyperLink id="hylCrops" style="Z-INDEX: 113; LEFT: 40px; POSITION: absolute; TOP: 128px" runat="server"
							Font-Size="Smaller" NavigateUrl="wfViewCrops.aspx">Crops</asp:HyperLink>
						<asp:HyperLink id="hylRegionsImg" style="Z-INDEX: 114; LEFT: 8px; POSITION: absolute; TOP: 160px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfViewRegions.aspx" ImageUrl="file:///C:\Inetpub\wwwroot\YieldProphet\Images\metstations.gif"></asp:HyperLink>
						<asp:HyperLink id="hylRegions" style="Z-INDEX: 115; LEFT: 40px; POSITION: absolute; TOP: 168px"
							runat="server" Font-Size="Smaller" NavigateUrl="wfViewRegions.aspx">Regions</asp:HyperLink></DIV>
					<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod">
			</asp:Panel></P>
			<asp:Panel id="pnlConsultant" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 192px"
				runat="server" Height="84px" HorizontalAlign="Left" Width="160px" tabIndex="-1">
				<DIV style="WIDTH: 160px; POSITION: relative; HEIGHT: 92px" tabIndex="-1" ms_positioning="GridLayout">
					<asp:HyperLink id="hylGrowersImg" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 32px"
						tabIndex="8" runat="server" Font-Size="Smaller" NavigateUrl="wfManageUsers.aspx" ImageUrl="Images\growers.gif"></asp:HyperLink>
					<P>
						<asp:HyperLink id="hylGrowers" style="Z-INDEX: 102; LEFT: 40px; POSITION: absolute; TOP: 40px"
							tabIndex="9" runat="server" Font-Size="Smaller" NavigateUrl="wfManageUsers.aspx">Growers</asp:HyperLink>
						<asp:HyperLink id="hylClimateForecast" style="Z-INDEX: 103; LEFT: 40px; POSITION: absolute; TOP: 72px"
							tabIndex="11" runat="server" Font-Size="Smaller" NavigateUrl="wfEditClimateForecast.aspx">Climate Forecast</asp:HyperLink></P>
					<P>
						<asp:HyperLink id="hylClimateForecastImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 64px"
							tabIndex="10" runat="server" Font-Size="Smaller" NavigateUrl="wfClimateForecast.aspx" ImageUrl="Images\climate_forecast.gif"></asp:HyperLink></P>
					<asp:Label id="lblConsultant" style="Z-INDEX: 105; LEFT: 48px; POSITION: absolute; TOP: 8px"
						runat="server" Font-Bold="True" ForeColor="Blue">Consultant</asp:Label></DIV>
			</asp:Panel>
			<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod">
				<asp:Panel id="pnlAdministration" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 306px"
					runat="server" Width="160px" Height="208px" tabIndex="-1"></P>
			<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod"></P>
			<DIV style="WIDTH: 160px; POSITION: relative; HEIGHT: 192px" tabIndex="-1" ms_positioning="GridLayout">
				<asp:HyperLink id="Hyperlink1" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 160px"
					tabIndex="22" runat="server" NavigateUrl="wfSQL.aspx" Font-Size="Smaller" ImageUrl="Images\sql.gif"></asp:HyperLink>
				<asp:HyperLink id="Hyperlink2" style="Z-INDEX: 102; LEFT: 40px; POSITION: absolute; TOP: 168px"
					tabIndex="23" runat="server" NavigateUrl="wfSQL.aspx" Font-Size="Smaller">SQL</asp:HyperLink>
				<asp:HyperLink id="Hyperlink3" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 128px"
					tabIndex="20" runat="server" NavigateUrl="wfViewDropDowns.aspx" Font-Size="Smaller" ImageUrl="Images\drop_downs.gif"></asp:HyperLink>
				<asp:HyperLink id="Hyperlink4" style="Z-INDEX: 104; LEFT: 40px; POSITION: absolute; TOP: 136px"
					tabIndex="21" runat="server" NavigateUrl="wfViewDropDowns.aspx" Font-Size="Smaller">Drop Downs</asp:HyperLink>
				<asp:Label id="Label1" style="Z-INDEX: 107; LEFT: 32px; POSITION: absolute; TOP: 8px" runat="server"
					ForeColor="Blue" Font-Bold="True">Administration</asp:Label>
				<asp:HyperLink id="Hyperlink7" style="Z-INDEX: 110; LEFT: 8px; POSITION: absolute; TOP: 32px" tabIndex="14"
					runat="server" NavigateUrl="wfReportTemplate.aspx" Font-Size="Smaller" ImageUrl="Images\apsim_report_gen.gif"></asp:HyperLink>
				<asp:HyperLink id="Hyperlink8" style="Z-INDEX: 111; LEFT: 40px; POSITION: absolute; TOP: 40px"
					tabIndex="15" runat="server" NavigateUrl="wfEditReportTemplate.aspx" Font-Size="Smaller">Report Templates</asp:HyperLink>
				<asp:HyperLink id="Hyperlink9" style="Z-INDEX: 112; LEFT: 8px; POSITION: absolute; TOP: 64px" tabIndex="16"
					runat="server" NavigateUrl="wfViewCrops.aspx" Font-Size="Smaller" ImageUrl="Images\soils.gif"></asp:HyperLink>
				<asp:HyperLink id="Hyperlink10" style="Z-INDEX: 113; LEFT: 40px; POSITION: absolute; TOP: 72px"
					tabIndex="17" runat="server" NavigateUrl="wfViewCrops.aspx" Font-Size="Smaller">Crops</asp:HyperLink>
				<asp:HyperLink id="Hyperlink11" style="Z-INDEX: 114; LEFT: 8px; POSITION: absolute; TOP: 96px"
					tabIndex="18" runat="server" NavigateUrl="wfViewRegions.aspx" Font-Size="Smaller" ImageUrl="Images\metstations.gif"></asp:HyperLink>
				<asp:HyperLink id="Hyperlink12" style="Z-INDEX: 115; LEFT: 40px; POSITION: absolute; TOP: 104px"
					tabIndex="19" runat="server" NavigateUrl="wfViewRegions.aspx" Font-Size="Smaller">Regions</asp:HyperLink></DIV>
			<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod"></asp:Panel></P>
			</DIV>
			<P style="BACKGROUND-IMAGE: none; BACKGROUND-COLOR: palegoldenrod"></asp:Panel></P>
			&nbsp;
		</form>
	</body>
</HTML>
