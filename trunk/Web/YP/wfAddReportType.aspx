<%@ Page language="c#" Codebehind="wfAddReportType.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfAddReportType" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfAddReportType</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:TextBox id="edtReportType" style="Z-INDEX: 105; LEFT: 24px; POSITION: absolute; TOP: 168px"
				runat="server" Width="304px" tabIndex="2" Height="24px"></asp:TextBox>
			<asp:Label id="lblReportType" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 144px"
				runat="server">Report type:</asp:Label>
			<asp:Label id="lblTemplateTypes" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server">Template type:</asp:Label>
			<asp:DropDownList id="cboTemplateTypes" style="Z-INDEX: 108; LEFT: 24px; POSITION: absolute; TOP: 104px"
				runat="server" Width="304px" AutoPostBack="True" tabIndex="1" Height="24px"></asp:DropDownList>
			<asp:Panel id="pnlTop" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" EnableViewState="False" Font-Size="X-Small">Cancel</asp:LinkButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 100; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="4"
						runat="server" EnableViewState="False" Font-Size="X-Small">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 104; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="3"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
		</form>
	</body>
</HTML>
