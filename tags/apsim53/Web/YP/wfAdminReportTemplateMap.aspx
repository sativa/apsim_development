<%@ Page language="c#" Codebehind="wfAdminReportTemplateMap.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminReportTemplateMap" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Report Template Map</title>
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
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Width="800px" Height="40px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="88px"
										CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" runat="server"
										ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="168px" CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" Font-Bold="True" ForeColor="DarkGray" Font-Size="Large" Width="785px"
									Height="20px">Report Template Map</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 488px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Label id="lblAPSIMReportTemplate" style="Z-INDEX: 105; LEFT: 136px; POSITION: absolute; TOP: 304px"
								runat="server" Font-Names="Arial">APSIM report template:</asp:Label>
							<asp:Label id="lblConParTemplate" style="Z-INDEX: 106; LEFT: 168px; POSITION: absolute; TOP: 256px"
								runat="server" Font-Names="Arial">Con/Par template:</asp:Label>
							<asp:DropDownList id="cboConParTemplates" style="Z-INDEX: 107; LEFT: 320px; POSITION: absolute; TOP: 256px"
								runat="server" Font-Names="Arial" Width="264px" tabIndex="3"></asp:DropDownList>
							<asp:DropDownList id="cboReportTypes" style="Z-INDEX: 108; LEFT: 464px; POSITION: absolute; TOP: 192px"
								runat="server" Font-Names="Arial" Width="248px" AutoPostBack="True" tabIndex="2"></asp:DropDownList>
							<asp:Label id="lblReportTypes" style="Z-INDEX: 109; LEFT: 368px; POSITION: absolute; TOP: 192px"
								runat="server" Font-Names="Arial">Report type:</asp:Label>
							<asp:DropDownList id="cboCropTypes" style="Z-INDEX: 110; LEFT: 184px; POSITION: absolute; TOP: 192px"
								runat="server" Font-Names="Arial" Width="160px" AutoPostBack="True" tabIndex="1"></asp:DropDownList>
							<asp:Label id="lblCropType" style="Z-INDEX: 111; LEFT: 88px; POSITION: absolute; TOP: 192px"
								runat="server" Font-Names="Arial" Width="80px" Height="16px">Crop type:</asp:Label>
							<asp:DropDownList id="cboAPSIMTemplates" style="Z-INDEX: 112; LEFT: 320px; POSITION: absolute; TOP: 304px"
								runat="server" Font-Names="Arial" Width="264px" tabIndex="4"></asp:DropDownList>
							<asp:Button id="btnSave" style="Z-INDEX: 113; LEFT: 176px; POSITION: absolute; TOP: 400px" runat="server"
								Width="120px" Height="32px" Text="Save changes" tabIndex="5"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 114; LEFT: 320px; POSITION: absolute; TOP: 400px"
								runat="server" Width="120px" Height="32px" Text="Cancel changes" tabIndex="6"></asp:Button>
							<asp:Button id="btnEditTemplate" style="Z-INDEX: 115; LEFT: 464px; POSITION: absolute; TOP: 400px"
								runat="server" Width="120px" Height="32px" Text="Edit Template" tabIndex="7"></asp:Button></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
