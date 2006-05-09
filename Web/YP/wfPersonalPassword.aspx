<%@ Page language="c#" Codebehind="wfPersonalPassword.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPersonalPassword" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Personal Password</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 507px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Bold="True" ForeColor="MediumBlue" Font-Size="X-Large"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" Width="800px" Height="40px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="160px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="88px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnMainMenuConsultant" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Width="785px" Height="20px" Font-Names="Arial Black" Font-Bold="True" ForeColor="DarkGray"
									Font-Size="Large">UserPlaceHolder's Password</asp:Label></DIV>
							<asp:Label id="lblPasswordOne" style="Z-INDEX: 103; LEFT: 120px; POSITION: absolute; TOP: 200px"
								tabIndex="-1" runat="server" Width="160px" Height="16px" Font-Names="Arial" ForeColor="Black">Enter a new password:</asp:Label>
							<asp:TextBox id="edtPasswordOne" style="Z-INDEX: 104; LEFT: 304px; POSITION: absolute; TOP: 200px"
								tabIndex="1" runat="server" Width="248px" Height="24px" Font-Names="Arial" EnableViewState="False"
								TextMode="Password"></asp:TextBox>
							<asp:Label id="lblPasswordTwo" style="Z-INDEX: 105; LEFT: 120px; POSITION: absolute; TOP: 248px"
								tabIndex="-1" runat="server" Width="168px" Height="16px" Font-Names="Arial" ForeColor="Black">Re-enter new password:</asp:Label>
							<asp:TextBox id="edtPasswordTwo" style="Z-INDEX: 106; LEFT: 304px; POSITION: absolute; TOP: 240px"
								tabIndex="2" runat="server" Width="248px" Height="24px" Font-Names="Arial" EnableViewState="False"
								TextMode="Password"></asp:TextBox>
							<asp:Button id="btnSave" style="Z-INDEX: 107; LEFT: 304px; POSITION: absolute; TOP: 280px" runat="server"
								Width="120px" Height="32px" Font-Names="Arial" Text="Save" tabIndex="3"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 108; LEFT: 440px; POSITION: absolute; TOP: 280px"
								runat="server" Width="120px" Height="32px" Font-Names="Arial" Text="Cancel" tabIndex="4"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 488px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 110; LEFT: 72px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="imgHelpPassword" style="Z-INDEX: 111; LEFT: 568px; POSITION: absolute; TOP: 200px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpPasswordReenter" style="Z-INDEX: 113; LEFT: 568px; POSITION: absolute; TOP: 240px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpPasswordPage" style="Z-INDEX: 114; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
