<%@ Page language="c#" Codebehind="wfAdminUserEditPassword.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminUserEditPassword" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
  <HEAD>
		<title>Edit Grower's Password</title>
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
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Bold="True" ForeColor="MediumBlue" Font-Size="X-Large"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Width="800px" Height="40px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
      <DIV id=divConsultant 
      style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: " 
      ms_positioning="GridLayout">
<asp:LinkButton id=btnPersonalDetailsConsultant style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px" tabIndex=6 runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="160px" CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
<asp:LinkButton id=btnManageReports style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px" tabIndex=7 runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="88px" CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
<asp:LinkButton id=btnManageGrowers style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px" tabIndex=5 runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="104px" CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
<asp:LinkButton id=btnMainMenuConsultant style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px" runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="112px" CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 104; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" Font-Bold="True" ForeColor="DarkGray" Font-Size="Large" Width="785px"
									Height="20px">Edit GrowerPlaceHolder's Password</asp:Label></DIV>
							<asp:Label id="lblPasswordOne" style="Z-INDEX: 105; LEFT: 120px; POSITION: absolute; TOP: 200px"
								tabIndex="-1" runat="server" Font-Names="Arial" ForeColor="Black" Width="160px" Height="16px">Enter a new password:</asp:Label>
							<asp:TextBox id="edtPasswordOne" style="Z-INDEX: 106; LEFT: 304px; POSITION: absolute; TOP: 192px"
								tabIndex="1" runat="server" Font-Names="Arial" Width="248px" Height="24px" EnableViewState="False"
								TextMode="Password"></asp:TextBox>
							<asp:Label id="lblPasswordTwo" style="Z-INDEX: 107; LEFT: 120px; POSITION: absolute; TOP: 248px"
								tabIndex="-1" runat="server" Font-Names="Arial" ForeColor="Black" Width="168px" Height="16px">Re-enter new password:</asp:Label>
							<asp:TextBox id="edtPasswordTwo" style="Z-INDEX: 108; LEFT: 304px; POSITION: absolute; TOP: 240px"
								tabIndex="2" runat="server" Font-Names="Arial" Width="248px" Height="24px" EnableViewState="False"
								TextMode="Password"></asp:TextBox>
							<asp:Button id="btnSave" style="Z-INDEX: 111; LEFT: 304px; POSITION: absolute; TOP: 280px" runat="server"
								Font-Names="Arial" Width="120px" Height="32px" Text="Save" tabIndex="3"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 114; LEFT: 440px; POSITION: absolute; TOP: 280px"
								runat="server" Font-Names="Arial" Width="120px" Height="32px" Text="Cancel" tabIndex="4"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 115; LEFT: 0px; POSITION: absolute; TOP: 488px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
