<%@ Page language="c#" Codebehind="wfEditReport.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditReport" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfEditReport</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: -8px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 192px; POSITION: relative; HEIGHT: 40px" ms_positioning="GridLayout">
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 16px"
						tabIndex="2" runat="server" ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 101; LEFT: 40px; POSITION: absolute; TOP: 16px" tabIndex="3"
						runat="server" Font-Size="X-Small" EnableViewState="False">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 103; LEFT: 88px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 102; LEFT: 112px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" Font-Size="X-Small" EnableViewState="False">Cancel</asp:LinkButton></DIV>
			</asp:Panel>
			<asp:Label id="lblReportName" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="88px" Height="16px">Report Name:</asp:Label>
			<asp:TextBox id="edtReportName" style="Z-INDEX: 103; LEFT: 128px; POSITION: absolute; TOP: 80px"
				runat="server" Width="280px" tabIndex="1" Height="24px"></asp:TextBox>
		</form>
	</body>
</HTML>
