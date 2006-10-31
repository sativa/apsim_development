<%@ Page language="c#" Codebehind="wfAdminReportTemplateEdit.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminReportTemplateEdit" validateRequest="false"%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Edit Report Template</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 728px" height="728" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 779px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BorderColor="White" BorderStyle="None" BackColor="MediumBlue" Height="40px"
								Width="800px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										tabIndex="10" runat="server" Font-Names="Arial" Font-Bold="True" ForeColor="White" Width="88px"
										Height="8px" CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" tabIndex="9"
										runat="server" Font-Names="Arial" Font-Bold="True" ForeColor="White" Width="168px" Height="8px"
										CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Large" ForeColor="DarkGray" Font-Bold="True" Font-Names="Arial Black" Height="20px"
									Width="785px">Edit Report Template </asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 760px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:TextBox id="edtDisplayTemplate" style="Z-INDEX: 105; LEFT: 56px; POSITION: absolute; TOP: 208px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="390px" Width="656px" TextMode="MultiLine"
								Wrap="False"></asp:TextBox>
							<asp:DropDownList id="cboReportTemplates" style="Z-INDEX: 106; LEFT: 200px; POSITION: absolute; TOP: 168px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="376px" AutoPostBack="True"></asp:DropDownList>
							<asp:Label id="lbReportlTemplate" style="Z-INDEX: 107; LEFT: 64px; POSITION: absolute; TOP: 168px"
								runat="server" Font-Names="Arial" Width="120px">Report Template:</asp:Label><INPUT class="stdInput" id="flImport" style="Z-INDEX: 108; LEFT: 328px; FONT-FAMILY: Arial; POSITION: absolute; TOP: 680px; HEIGHT: 32px"
								tabIndex="6" type="file" size="25" name="flImport" runat="server">
							<asp:Button id="btnSave" style="Z-INDEX: 110; LEFT: 232px; POSITION: absolute; TOP: 616px" runat="server"
								Height="32px" Width="120px" Text="Save changes" tabIndex="3"></asp:Button>
							<asp:Button id="btnAddReportTemplate" style="Z-INDEX: 109; LEFT: 32px; POSITION: absolute; TOP: 680px"
								runat="server" Height="32px" Width="120px" Text="Add template" tabIndex="5"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 111; LEFT: 376px; POSITION: absolute; TOP: 616px"
								runat="server" Height="32px" Width="120px" Text="Cancel changes" tabIndex="4"></asp:Button>
							<asp:Button id="btnImport" style="Z-INDEX: 112; LEFT: 208px; POSITION: absolute; TOP: 680px"
								runat="server" Height="32px" Width="120px" Text="Import->" tabIndex="7"></asp:Button>
							<asp:Button id="btnDeleteReportTemplate" style="Z-INDEX: 113; LEFT: 648px; POSITION: absolute; TOP: 680px"
								runat="server" Height="32px" Width="120px" Text="Delete template" tabIndex="8"></asp:Button></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
