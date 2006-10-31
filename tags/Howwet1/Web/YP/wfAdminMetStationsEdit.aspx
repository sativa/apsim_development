<%@ Page language="c#" Codebehind="wfAdminMetStationsEdit.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminMetStationsEdit" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Edit Met Station</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 577px" height="577" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 563px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px" Height="8px"
										CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" runat="server"
										Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="168px" Height="8px" CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Large" Height="20px"
									Width="785px">Edit Met Station</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 544px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:Button id="btnSave" style="Z-INDEX: 106; LEFT: 256px; POSITION: absolute; TOP: 472px" runat="server"
								Height="32px" Width="120px" Text="Save changes" tabIndex="3"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 108; LEFT: 416px; POSITION: absolute; TOP: 472px"
								runat="server" Height="32px" Width="120px" Text="Cancel changes" tabIndex="4"></asp:Button>
							<asp:label id="lblStationName" style="Z-INDEX: 109; LEFT: 192px; POSITION: absolute; TOP: 288px"
								runat="server" Font-Names="Arial" Height="16px" Width="104px">Station Name:</asp:label>
							<asp:Label id="lblStationNumber" style="Z-INDEX: 110; LEFT: 184px; POSITION: absolute; TOP: 336px"
								runat="server" Font-Names="Arial" Height="16px">Station Number:</asp:Label>
							<asp:TextBox id="edtStationNumber" style="Z-INDEX: 111; LEFT: 312px; POSITION: absolute; TOP: 336px"
								runat="server" Font-Names="Arial" Width="320px" tabIndex="2"></asp:TextBox>
							<asp:textbox id="edtStationName" style="Z-INDEX: 112; LEFT: 312px; POSITION: absolute; TOP: 280px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="320px"></asp:textbox></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
