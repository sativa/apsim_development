<%@ Page language="c#" Codebehind="wfAdminSoilsEdit.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminSoilsEdit" validateRequest="false"%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Edit Soil</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 736px" height="736" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 731px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px"
										Height="8px" CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" tabIndex="5"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="168px" Height="8px"
										CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Large" Height="20px"
									Width="785px">Edit Soils</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 712px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:Button id="btnSave" style="Z-INDEX: 105; LEFT: 256px; POSITION: absolute; TOP: 656px" runat="server"
								Height="32px" Width="120px" Text="Save changes" tabIndex="3"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 106; LEFT: 416px; POSITION: absolute; TOP: 656px"
								runat="server" Height="32px" Width="120px" Text="Cancel changes" tabIndex="4"></asp:Button>
							<asp:label id="lblSoilName" style="Z-INDEX: 107; LEFT: 96px; POSITION: absolute; TOP: 192px"
								runat="server" Font-Names="Arial" Height="16px" Width="80px">Soil Name:</asp:label>
							<asp:textbox id="edtSoilName" style="Z-INDEX: 108; LEFT: 192px; POSITION: absolute; TOP: 184px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="416px"></asp:textbox>
							<asp:textbox id="edtSoilData" style="Z-INDEX: 109; LEFT: 192px; POSITION: absolute; TOP: 232px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="390px" Width="416px" TextMode="MultiLine"></asp:textbox>
							<asp:label id="lblSoilData" style="Z-INDEX: 110; LEFT: 96px; POSITION: absolute; TOP: 240px"
								runat="server" Font-Names="Arial" Width="78px">Soil Data:</asp:label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
