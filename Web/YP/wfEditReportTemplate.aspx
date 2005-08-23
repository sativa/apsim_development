<%@ Page language="c#" Codebehind="wfEditReportTemplate.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditReportTemplate" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfReportTemplate</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left" tabIndex="-1">
				<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 41px" tabIndex="-1" ms_positioning="GridLayout">
					<asp:LinkButton id="btnDeleteReportTemplate" style="Z-INDEX: 104; LEFT: 632px; POSITION: absolute; TOP: 16px"
						tabIndex="12" runat="server" EnableViewState="False" Font-Size="X-Small">Delete Report Template</asp:LinkButton>
					<asp:LinkButton id="btnAddReportTemplate" style="Z-INDEX: 109; LEFT: 472px; POSITION: absolute; TOP: 16px"
						tabIndex="10" runat="server" EnableViewState="False" Font-Size="X-Small">Add Report Template</asp:LinkButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 108; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="5"
						runat="server" EnableViewState="False" Font-Size="X-Small">Save</asp:LinkButton>
					<asp:ImageButton id="btnAddReportTemplateImg" style="Z-INDEX: 107; LEFT: 448px; POSITION: absolute; TOP: 16px"
						tabIndex="9" runat="server" ImageUrl="Images\add.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="4"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:ImageButton id="btnDeleteReportTemplateImg" style="Z-INDEX: 103; LEFT: 608px; POSITION: absolute; TOP: 16px"
						tabIndex="11" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnImport" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="7" runat="server" EnableViewState="False" Font-Size="X-Small">Import</asp:LinkButton>
					<asp:ImageButton id="btnImportImg" style="Z-INDEX: 100; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" ImageUrl="Images\import.gif"></asp:ImageButton><INPUT class="stdInput" id="flImport" style="Z-INDEX: 105; LEFT: 152px; WIDTH: 280px; POSITION: absolute; TOP: 16px; HEIGHT: 25px"
						tabIndex="8" type="file" size="27" name="flImport" runat="server">
				</DIV>
			</asp:Panel>
			<asp:TextBox id="edtDisplayTemplate" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 144px"
				runat="server" Width="656px" Height="390px" TextMode="MultiLine" Wrap="False" tabIndex="3"></asp:TextBox>
			<asp:Label id="lbReportlTemplate" style="Z-INDEX: 110; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="120px">Report Template:</asp:Label>
			<asp:DropDownList id="cboReportTemplates" style="Z-INDEX: 110; LEFT: 24px; POSITION: absolute; TOP: 104px"
				tabIndex="2" runat="server" Height="24px" Width="210px" AutoPostBack="True"></asp:DropDownList>
		</form>
	</body>
</HTML>
