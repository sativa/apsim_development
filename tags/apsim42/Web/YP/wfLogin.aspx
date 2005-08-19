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
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:TextBox id="edtPassword" style="Z-INDEX: 102; LEFT: 256px; POSITION: absolute; TOP: 104px"
				runat="server" Width="144px" Height="24px" tabIndex="2" TextMode="Password"></asp:TextBox>
			<asp:Label id="lblRegInfo" style="Z-INDEX: 115; LEFT: 168px; POSITION: absolute; TOP: 368px"
				runat="server" Height="24px" Width="184px" ForeColor="Blue" Font-Size="Small">Want registration information?</asp:Label>
			<asp:Label id="lblFurtherDetails" style="Z-INDEX: 113; LEFT: 168px; POSITION: absolute; TOP: 304px"
				runat="server" Height="16px" Width="160px" ForeColor="Blue" Font-Size="Small">For further deatils contact:</asp:Label>
			<asp:Label id="lblPassword" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 104px"
				runat="server" Width="72px" Height="16px" ForeColor="Green" Font-Size="Small">Password:</asp:Label>&nbsp;
			<asp:TextBox id="edtUserName" style="Z-INDEX: 101; LEFT: 256px; POSITION: absolute; TOP: 64px"
				runat="server" Width="144px" Height="24px" tabIndex="1"></asp:TextBox>
			<asp:Label id="lblUserName" style="Z-INDEX: 103; LEFT: 168px; POSITION: absolute; TOP: 64px"
				runat="server" Width="80px" Height="16px" ForeColor="Green" Font-Size="Small">User Name:</asp:Label>
			<asp:Button id="btnLogin" style="Z-INDEX: 105; LEFT: 320px; POSITION: absolute; TOP: 144px"
				runat="server" Width="81px" Height="32px" Text="Login" tabIndex="3" Font-Size="Smaller"></asp:Button>
			<asp:Label id="lblWelcome" style="Z-INDEX: 106; LEFT: 168px; POSITION: absolute; TOP: 16px"
				runat="server" Height="24px" Width="216px" ForeColor="Green" Font-Size="Small">Welcome to Yield Prophet</asp:Label>
			<asp:Label id="lblVisiting" style="Z-INDEX: 107; LEFT: 168px; POSITION: absolute; TOP: 208px"
				runat="server" Height="32px" Width="112px" ForeColor="Blue" Font-Size="Small">Just Visiting?</asp:Label>
			<asp:Label id="lblVisitingDetail" style="Z-INDEX: 108; LEFT: 168px; POSITION: absolute; TOP: 240px"
				runat="server" Height="32px" Width="731px" ForeColor="Blue" Font-Size="Small">Yield Prophet is a subscriber only service. Visitors are invited to navigate around the BCG trial paddock. Respond to User name and Password with ‘Visitor’. You will be able to view the setup data and previously generated reports. You will not be able to save changes or request a new report.</asp:Label>
			<asp:Label id="lblWarningTwo" style="Z-INDEX: 109; LEFT: 168px; POSITION: absolute; TOP: 496px"
				runat="server" Height="24px" Width="732px" BackColor="Transparent" Font-Size="Small">Yield Prophet does not generate recommendations or advice. Yield Prophet uses the computer simulation model APSIM together with paddock specific soil, crop and climate data to generate information about the likely outcomes of farming decisions.  APSIM does not take into account weed competition, pest/disease pressure, pesticide damage, farmer error, or extreme events (such as extreme weather, flood and fire). For more information about APSIM please look at:</asp:Label>
			<asp:Label id="lblWarningOne" style="Z-INDEX: 110; LEFT: 168px; POSITION: absolute; TOP: 464px"
				runat="server" Height="32px" Width="248px" Font-Bold="True" Font-Size="Small">Important notice to users:</asp:Label>
			<asp:HyperLink id="hylWarning" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 576px"
				runat="server" Height="24px" Width="120px" NavigateUrl="http://www.apsim.info" Target="_blank"
				BackColor="Transparent" Font-Size="Small">www.apsim.info</asp:HyperLink>
			<asp:Image id="imgSide" style="Z-INDEX: 112; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				ImageUrl="Images\wheat.jpg" Width="120px"></asp:Image>
			<asp:HyperLink id="hylEmail" style="Z-INDEX: 114; LEFT: 336px; POSITION: absolute; TOP: 304px"
				runat="server" NavigateUrl="mailto:james.hunt@aanet.com.au" Font-Size="Small">James Hunt</asp:HyperLink>
			<asp:Button id="RegistrationButton" style="Z-INDEX: 116; LEFT: 352px; POSITION: absolute; TOP: 368px"
				runat="server" Height="24px" Width="192px" Text="Click here for registration info" Font-Size="Smaller"></asp:Button>
			<asp:Label id="lblSurvey" style="Z-INDEX: 117; LEFT: 168px; POSITION: absolute; TOP: 400px"
				runat="server" Height="24px" Width="448px" ForeColor="Blue" Font-Size="Small">Users of Yield Prophet in 2004 are invited to take part in an online suvey</asp:Label>
			<asp:Button id="btnSurvey" style="Z-INDEX: 118; LEFT: 616px; POSITION: absolute; TOP: 400px"
				runat="server" Height="24px" Width="184px" Text="Click here for online survey" Font-Size="Smaller"></asp:Button>
		</form>
	</body>
</HTML>
