<%@ Page language="c#" Codebehind="wfAddDropDownType.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfAddDropDownType" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfAddDropDownType</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
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
			<asp:Label id="lblDropDownType" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="192px">Drop Down Type:</asp:Label>
			<asp:TextBox id="edtDropDownType" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 104px"
				runat="server" Width="304px" tabIndex="1"></asp:TextBox>
			<asp:Label id="lblTableName" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 152px"
				runat="server" Width="192px">Table Name:</asp:Label>
			<asp:TextBox id="edtTableName" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 176px"
				runat="server" Width="304px" tabIndex="2"></asp:TextBox>
		</form>
	</body>
</HTML>
