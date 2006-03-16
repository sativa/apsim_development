<%@ Page language="c#" Codebehind="wfLogin.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfLogin" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Yield Prophet</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			&nbsp;
			<TABLE id="MainTable" cellSpacing="0" cellPadding="0" width="800" align="center" border="0"
				height="600">
				<TR>
					<TD style="WIDTH: 120px" vAlign="top"><IMG id="IMGSideBar" style="WIDTH: 98px; HEIGHT: 600px" height="600" alt="" src="Images\wheat.jpg"
							width="98"></TD>
					<TD vAlign="top">
						<TABLE id="CenterTable" cellSpacing="0" cellPadding="0" width="680" border="0" style="WIDTH: 680px"
							height="600" align="left">
							<TR>
								<TD style="HEIGHT: 67px"><IMG id="imgHeader" style="WIDTH: 696px; HEIGHT: 65px" height="65" alt="" src="Images\yieldprophet.jpg"
										width="696"></TD>
							</TR>
							<TR>
								<TD vAlign="top" align="left">
									<DIV style="WIDTH: 720px; POSITION: relative; HEIGHT: 536px" ms_positioning="GridLayout">
										<asp:Label id="lblUserName" style="Z-INDEX: 101; LEFT: 24px; POSITION: absolute; TOP: 176px"
											runat="server" Height="16px" Width="80px" Font-Size="Small" ForeColor="Black" Font-Names="Arial">Username:</asp:Label>
										<asp:TextBox id="edtUserName" style="Z-INDEX: 102; LEFT: 112px; POSITION: absolute; TOP: 176px"
											tabIndex="1" runat="server" Height="24px" Width="144px"></asp:TextBox>
										<asp:TextBox id="edtPassword" style="Z-INDEX: 103; LEFT: 112px; POSITION: absolute; TOP: 208px"
											tabIndex="2" runat="server" TextMode="Password" Height="24px" Width="144px"></asp:TextBox>
										<asp:Button id="btnLogin" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 240px"
											tabIndex="3" runat="server" Height="32px" Width="81px" Font-Size="Smaller" Text="Login"></asp:Button>
										<asp:Label id="lblPassword" style="Z-INDEX: 105; LEFT: 24px; POSITION: absolute; TOP: 216px"
											runat="server" Height="16px" Width="72px" Font-Size="Small" ForeColor="Black" Font-Names="Arial">Password:</asp:Label>
										<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 160px; POSITION: absolute; TOP: 16px"
											runat="server" Font-Size="X-Large" ForeColor="Maroon" Font-Bold="True" Font-Names="Arial">Welcome to Yield Prophet®</asp:Label>
										<asp:Label id="lblDescription" style="Z-INDEX: 107; LEFT: 32px; POSITION: absolute; TOP: 96px"
											runat="server" Font-Size="Small" Font-Names="Arial">Yield Prophet® is an on-line crop production model designed to provide grain growers with real-time information about the crop during growth.</asp:Label>
										<asp:LinkButton id="btnFeedback" style="Z-INDEX: 108; LEFT: 384px; POSITION: absolute; TOP: 64px"
											runat="server" ForeColor="Maroon" Font-Bold="True" Font-Names="Arial">Feedback</asp:LinkButton>
										<asp:LinkButton id="btnJoin" style="Z-INDEX: 109; LEFT: 280px; POSITION: absolute; TOP: 64px" runat="server"
											ForeColor="Maroon" Font-Bold="True" Font-Names="Arial">Join</asp:LinkButton>
										<asp:HyperLink id="hylContactUs" style="Z-INDEX: 110; LEFT: 520px; POSITION: absolute; TOP: 64px"
											runat="server" Font-Size="Small" ForeColor="Maroon" Font-Bold="True" NavigateUrl="mailto:james.hunt@aanet.com.au"
											Font-Names="Arial">Contact Us</asp:HyperLink>
										<asp:Label id="lblVisitorDescription" style="Z-INDEX: 111; LEFT: 288px; POSITION: absolute; TOP: 176px"
											runat="server" Width="400px" Font-Size="Small" Font-Names="Arial">Yield Prophet® is a subscriber only service. However, visitors are invited to navigate around the BCG trial paddock by responding to Username and Password with ‘Visitor’. You will be able to view the setup data and previously generated reports. You will not be able to save changes or request a new report.</asp:Label>
										<asp:Label id="lblNoticeOne" style="Z-INDEX: 112; LEFT: 32px; POSITION: absolute; TOP: 392px"
											runat="server" Height="16px" Width="200px" Font-Size="Small" ForeColor="Black" Font-Bold="True"
											Font-Names="Arial">Important notice to users:</asp:Label>
										<asp:Label id="lblSubscribers" style="Z-INDEX: 113; LEFT: 24px; POSITION: absolute; TOP: 152px"
											runat="server" Height="24px" Width="96px" Font-Size="Small" ForeColor="Maroon" Font-Bold="True"
											Font-Names="Arial">Subscribers</asp:Label>
										<asp:Label id="lblVisitors" style="Z-INDEX: 114; LEFT: 280px; POSITION: absolute; TOP: 152px"
											runat="server" Height="24px" Width="96px" Font-Size="Small" ForeColor="Maroon" Font-Bold="True"
											Font-Names="Arial">Visitors</asp:Label>
										<asp:Label id="lblNoticeTwo" style="Z-INDEX: 115; LEFT: 32px; POSITION: absolute; TOP: 416px"
											runat="server" Height="72px" Width="632px" Font-Size="Small" ForeColor="Black" Font-Names="Arial">Yield Prophet® does not generate recommendations or advice. Yield Prophet® uses the computer simulation model APSIM together with paddock specific soil, crop and climate data to generate information about the likely outcomes of farming decisions. APSIM does not take into account weed competition, pest/disease pressure, pesticide damage, farmer error, or extreme events (such as extreme weather, flood and fire). For more information about APSIM please look at</asp:Label>
										<asp:HyperLink id="hylApsim" style="Z-INDEX: 116; LEFT: 232px; POSITION: absolute; TOP: 504px"
											runat="server" Font-Size="Small" ForeColor="Blue" Target="_blank" NavigateUrl="http://www.apsim.info"
											Font-Names="Arial">www.apsim.info</asp:HyperLink>
										<asp:Image id="imgBCG" style="Z-INDEX: 117; LEFT: 288px; POSITION: absolute; TOP: 296px" runat="server"
											Height="96px" Width="88px" ImageUrl="Images\bcg.JPG"></asp:Image>
										<asp:Image id="imgASPRU" style="Z-INDEX: 118; LEFT: 392px; POSITION: absolute; TOP: 304px"
											runat="server" Height="80px" Width="128px" ImageUrl="Images\apsru.jpg"></asp:Image>
										<asp:Image id="imgDCITA" style="Z-INDEX: 119; LEFT: 544px; POSITION: absolute; TOP: 304px"
											runat="server" Height="80px" Width="128px" ImageUrl="Images\dcita.jpg"></asp:Image>
										<asp:Image id="imgTents" style="Z-INDEX: 120; LEFT: 32px; POSITION: absolute; TOP: 280px" runat="server"
											Height="104px" Width="224px" ImageUrl="Images\tents.JPG"></asp:Image>
										<asp:HyperLink id="hylFurtherInfo" style="Z-INDEX: 121; LEFT: 136px; POSITION: absolute; TOP: 64px"
											runat="server" Font-Size="Small" ForeColor="Maroon" Font-Bold="True" Target="_blank" NavigateUrl="http://www.bcg.org.au/cb_pages/yield_prophet.php"
											Font-Names="Arial">Futher Info</asp:HyperLink></DIV>
								</TD>
							</TR>
						</TABLE>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
