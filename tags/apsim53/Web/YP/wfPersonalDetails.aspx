<%@ Page language="c#" Codebehind="wfPersonalDetails.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPersonalDetails" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Personal Details</title>
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
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlNavigatoinMenu" style="Z-INDEX: 106; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" Height="40px" Width="800px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV id="divNavigatoinMenu" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="160px" Height="8px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px" Height="8px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px" Height="8px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<asp:Label id="lblName" style="Z-INDEX: 102; LEFT: 224px; POSITION: absolute; TOP: 224px" runat="server"
								Font-Names="Arial" ForeColor="Black" Height="16px" Width="40px">Name:</asp:Label>
							<asp:TextBox id="edtName" style="Z-INDEX: 101; LEFT: 280px; POSITION: absolute; TOP: 216px" tabIndex="1"
								runat="server" Font-Names="Arial" Height="24px" Width="248px" EnableViewState="False"></asp:TextBox>
							<asp:Label id="lblEmail" style="Z-INDEX: 103; LEFT: 224px; POSITION: absolute; TOP: 264px"
								runat="server" Font-Names="Arial" ForeColor="Black" Height="16px" Width="40px">Email:</asp:Label>
							<asp:TextBox id="edtEmail" style="Z-INDEX: 104; LEFT: 280px; POSITION: absolute; TOP: 256px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="24px" Width="248px" EnableViewState="False"></asp:TextBox>
							<asp:Button id="btnSave" style="Z-INDEX: 108; LEFT: 208px; POSITION: absolute; TOP: 408px" runat="server"
								Font-Names="Arial" Height="32px" Width="120px" Text="Save" tabIndex="4"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 107; LEFT: 344px; POSITION: absolute; TOP: 408px"
								runat="server" Font-Names="Arial" Height="32px" Width="120px" Text="Cancel" tabIndex="5"></asp:Button>
							<asp:Button id="btnPassword" style="Z-INDEX: 105; LEFT: 480px; POSITION: absolute; TOP: 408px"
								tabIndex="6" runat="server" Font-Names="Arial" Height="32px" Width="120px" Text="Change Password"></asp:Button>
							<DIV id="divPage" style="Z-INDEX: 109; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 40px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 113; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
									Font-Size="Large" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="799px"
									Height="20px">UserPlaceHolder's Personal Details</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 110; LEFT: 0px; POSITION: absolute; TOP: 488px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:DropDownList id="cboStartPages" style="Z-INDEX: 111; LEFT: 280px; POSITION: absolute; TOP: 304px"
								runat="server" Font-Names="Arial" Width="248px" Height="24px" tabIndex="3"></asp:DropDownList>
							<asp:Label id="lblStartPage" style="Z-INDEX: 112; LEFT: 192px; POSITION: absolute; TOP: 304px"
								runat="server" Font-Names="Arial">Start Page:</asp:Label>
							<asp:ImageButton id="imgHelpEmail" style="Z-INDEX: 113; LEFT: 544px; POSITION: absolute; TOP: 256px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpStartPage" style="Z-INDEX: 114; LEFT: 544px; POSITION: absolute; TOP: 304px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:Image id="imgBanner" style="Z-INDEX: 115; LEFT: 72px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="imgHelpName" style="Z-INDEX: 116; LEFT: 544px; POSITION: absolute; TOP: 216px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpPersonalDetailsPage" style="Z-INDEX: 117; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
						</DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
