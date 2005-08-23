<%@ Page language="c#" Codebehind="wfEditReportTemplateMap.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditReportTemplateMap" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfEditReportTemplateMap</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				BackColor="PaleGoldenrod" HorizontalAlign="Left" Width="100%" Height="48px">
				<DIV style="WIDTH: 296px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" EnableViewState="False" Font-Size="X-Small">Cancel</asp:LinkButton>
					<asp:LinkButton id="btnEditTemplate" style="Z-INDEX: 106; LEFT: 184px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" EnableViewState="False" Font-Size="X-Small">Edit Templates</asp:LinkButton>
					<asp:ImageButton id="btnEditTemplateImg" style="Z-INDEX: 105; LEFT: 160px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" ImageUrl="Images\rename.gif"></asp:ImageButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 100; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="4"
						runat="server" EnableViewState="False" Font-Size="X-Small">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 103; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="3"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:DropDownList id="cboCropTypes" style="Z-INDEX: 102; LEFT: 96px; POSITION: absolute; TOP: 80px"
				runat="server" Width="160px" AutoPostBack="True"></asp:DropDownList>
			<asp:DropDownList id="cboReportTypes" style="Z-INDEX: 103; LEFT: 360px; POSITION: absolute; TOP: 80px"
				runat="server" Width="160px" AutoPostBack="True"></asp:DropDownList>
			<asp:DropDownList id="cboConParTemplates" style="Z-INDEX: 104; LEFT: 208px; POSITION: absolute; TOP: 152px"
				runat="server" Width="264px"></asp:DropDownList>
			<asp:DropDownList id="cboAPSIMTemplates" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 192px"
				runat="server" Width="264px"></asp:DropDownList>
			<asp:Label id="lblCropType" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="70px" Height="16px">Crop type:</asp:Label>
			<asp:Label id="lblConParTemplate" style="Z-INDEX: 107; LEFT: 56px; POSITION: absolute; TOP: 152px"
				runat="server">Con/Par template:</asp:Label>
			<asp:Label id="lblAPSIMReportTemplate" style="Z-INDEX: 108; LEFT: 56px; POSITION: absolute; TOP: 192px"
				runat="server">APSIM report template:</asp:Label>
			<asp:Label id="lblReportTypes" style="Z-INDEX: 109; LEFT: 280px; POSITION: absolute; TOP: 80px"
				runat="server">Report type:</asp:Label>
		</form>
	</body>
</HTML>
