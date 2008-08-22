<%@ Page language="c#" Codebehind="wfLogin.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfLogin" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Welcome to Yield Prophet</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			&nbsp;
			<TABLE id="Table1" style="WIDTH: 800px; HEIGHT: 600px" height="648" cellSpacing="0" cellPadding="0"
				width="67" align="center" border="0">
				<TR>
					<TD style="WIDTH: 788px; HEIGHT: 86px" vAlign="middle" align="center" colSpan="2">
						<asp:Label id="Label1" runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large"
							Font-Bold="True"> Yield Prophet<sup>®</sup></asp:Label></TD>
				</TR>
				<TR>
					<TD style="WIDTH: 788px; HEIGHT: 44px" vAlign="top" bgColor="mediumblue" colSpan="2">
						<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 35px" ms_positioning="GridLayout">
							<asp:HyperLink id="HyperLink2" style="Z-INDEX: 121; LEFT: 32px; POSITION: absolute; TOP: 8px" runat="server"
								Font-Names="Arial" ForeColor="White" Font-Size="Medium" Font-Bold="True" NavigateUrl="http://www.bcg.org.au/cb_pages/yield_prophet.php"
								Target="_blank" tabIndex="5">Further Info</asp:HyperLink>
							<asp:LinkButton id="btnJoin" style="Z-INDEX: 122; LEFT: 176px; POSITION: absolute; TOP: 8px" runat="server"
								Font-Names="Arial" ForeColor="White" Font-Size="Medium" Font-Bold="True" tabIndex="6" CommandName="wfRegistrationMenu.aspx">Subscribe for 2006</asp:LinkButton>
							<asp:LinkButton id="btnFeedback" style="Z-INDEX: 123; LEFT: 392px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial" ForeColor="White" Font-Size="Medium" Font-Bold="True" tabIndex="7"
								CommandName="wfSurvey.aspx">2005 Subscriber Survey</asp:LinkButton>
							<asp:HyperLink id="HyperLink1" style="Z-INDEX: 124; LEFT: 640px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial" ForeColor="White" Font-Size="Medium" Font-Bold="True" NavigateUrl="mailto:james.hunt@aanet.com.au"
								tabIndex="8">Contact Us</asp:HyperLink></DIV>
					</TD>
				</TR>
				<TR>
					<TD style="WIDTH: 518px; HEIGHT: 489px" vAlign="top">
						<DIV style="WIDTH: 505px; POSITION: relative; HEIGHT: 472px" ms_positioning="GridLayout">
							<asp:Label id="Label3" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 176px" runat="server"
								Font-Names="Arial" ForeColor="MediumBlue" Font-Size="Medium" Width="64px" Height="8px"
								Font-Bold="True">Visitors</asp:Label>
							<asp:Label id="lblVisitorText" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 200px"
								runat="server" Font-Names="Arial" Font-Size="Small" Width="464px"> Yield Prophet<sup>
									®</sup> is a subscriber only service. However, visitors may browse with restricted access by responding to Username and Password with <B>Visitor</B> (please note the login boxes are case-sensitive).</asp:Label>
							<asp:Label id="Label5" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 312px" runat="server"
								Font-Names="Arial" ForeColor="Black" Font-Size="Small" Width="472px" Height="72px"> Yield Prophet<sup>
									®</sup> does not generate recommendations or advice. Yield Prophet<sup>®</sup> uses the computer simulation model APSIM together with paddock specific soil, crop and climate data to generate information about the likely outcomes of farming decisions. APSIM does not take into account weed competition, pest/disease pressure, pesticide damage, farmer error, or extreme events (such as extreme weather, flood and fire). For more information about APSIM please look at <a href="http://www.apsim.info">www.apsim.info</a></asp:Label>
							<asp:Label id="Label6" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 40px" runat="server"
								Font-Names="Arial" Font-Size="Small" Width="504px"> Yield Prophet<sup>®</sup> is an on-line crop production model designed to provide grain growers with real-time information about the crop during growth.</asp:Label>
							<asp:Label id="Label7" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 8px" runat="server"
								Font-Names="Arial" ForeColor="MediumBlue" Font-Size="Medium" Width="248px" Height="8px"
								Font-Bold="True">Welcome to Yield Prophet<sup>®</sup></asp:Label>
							<asp:Label id="Label4" style="Z-INDEX: 106; LEFT: 0px; POSITION: absolute; TOP: 288px" runat="server"
								Font-Names="Arial" ForeColor="MediumBlue" Font-Size="Medium" Width="232px" Height="16px"
								Font-Bold="True">Important notice to users:</asp:Label>
							<asp:Label id="lblWelcomeText2" style="Z-INDEX: 107; LEFT: 0px; POSITION: absolute; TOP: 88px"
								runat="server" Font-Size="Small" Font-Names="Arial" Width="472px">To assist in management decisions, growers enter inputs at any time during the season to generate reports of projected yield outcomes showing the impact of crop type and variety, sowing time, nitrogen fertiliser and irrigation.</asp:Label></DIV>
					</TD>
					<TD style="WIDTH: 278px; HEIGHT: 489px" vAlign="top">
						<DIV style="WIDTH: 275px; POSITION: relative; HEIGHT: 472px" ms_positioning="GridLayout">
							<asp:Label id="lblPassword" style="Z-INDEX: 101; LEFT: 16px; POSITION: absolute; TOP: 120px"
								runat="server" Font-Size="Small" ForeColor="Black" Font-Names="Arial" Height="16px" Width="72px"
								BackColor="Transparent">Password:</asp:Label>
							<asp:Label id="lblUserName" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 80px"
								runat="server" Font-Size="Small" ForeColor="Black" Font-Names="Arial" Height="16px" Width="74px"
								BackColor="Transparent">Username:</asp:Label>
							<asp:TextBox id="edtUserName" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 80px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="152px"></asp:TextBox>
							<asp:TextBox id="edtPassword" style="Z-INDEX: 104; LEFT: 104px; POSITION: absolute; TOP: 120px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="24px" Width="152px" TextMode="Password"></asp:TextBox>
							<asp:Button id="btnLogin" style="Z-INDEX: 105; LEFT: 168px; POSITION: absolute; TOP: 184px"
								tabIndex="4" runat="server" Font-Size="Smaller" Height="32px" Width="81px" Text="Login"></asp:Button>
							<asp:Label id="lblSubscribers" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 40px"
								runat="server" Font-Bold="True" Font-Size="Medium" ForeColor="MediumBlue" Font-Names="Arial"
								Height="24px" Width="152px" BackColor="Transparent">Subscriber login</asp:Label>
							<asp:CheckBox id="chkRemember" style="Z-INDEX: 107; LEFT: 112px; POSITION: absolute; TOP: 152px"
								runat="server" Font-Names="Arial" Text="Remember me?" TextAlign="Left" tabIndex="3"></asp:CheckBox>
							<asp:HyperLink id="hylCsiro" style="Z-INDEX: 108; LEFT: 160px; POSITION: absolute; TOP: 232px"
								runat="server" Height="120px" Width="100px" ImageUrl="Images\csiro_logo.JPG" Target="_blank"
								NavigateUrl="http://www.csiro.au/"></asp:HyperLink>
							<asp:HyperLink id="hylBCG" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 224px" runat="server"
								Height="144px" Width="104px" ImageUrl="Images\bcg_logo.JPG" Target="_blank" NavigateUrl="http://www.bcg.org.au/"></asp:HyperLink>
							<asp:HyperLink id="hylAPSRU" style="Z-INDEX: 110; LEFT: 8px; POSITION: absolute; TOP: 376px" runat="server"
								Target="_blank" NavigateUrl="http://www.apsru.gov.au/apsru/Default.htm" Height="80px" Width="123px"
								ImageUrl="Images\logo_apsru.jpg"></asp:HyperLink>
							<asp:HyperLink id="hlyDCITA" style="Z-INDEX: 111; LEFT: 136px; POSITION: absolute; TOP: 376px"
								runat="server" Target="_blank" NavigateUrl="http://www.dcita.gov.au/" Height="82px" Width="138px"
								ImageUrl="Images\logo_dcita.jpg"></asp:HyperLink></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
