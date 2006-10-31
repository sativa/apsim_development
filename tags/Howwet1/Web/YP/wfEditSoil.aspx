<%@ Page language="c#" Codebehind="wfEditSoil.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditSoil" validateRequest="false" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfEditSoil</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 192px; POSITION: relative; HEIGHT: 40px" ms_positioning="GridLayout">
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 16px"
						tabIndex="2" runat="server" ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 100; LEFT: 40px; POSITION: absolute; TOP: 16px" tabIndex="3"
						runat="server" EnableViewState="False" Font-Size="Smaller">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 104; LEFT: 88px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 112px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" EnableViewState="False" Font-Size="Smaller">Cancel</asp:LinkButton></DIV>
			</asp:panel><asp:label id="lblSoilName" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="72px">Soil Name:</asp:label><asp:textbox id="edtSoilName" style="Z-INDEX: 103; LEFT: 96px; POSITION: absolute; TOP: 80px"
				tabIndex="1" runat="server" Height="24px" Width="416px"></asp:textbox><asp:textbox id="edtSoilData" style="Z-INDEX: 104; LEFT: 96px; POSITION: absolute; TOP: 128px"
				tabIndex="3" runat="server" Height="390px" Width="416px" TextMode="MultiLine"></asp:textbox><asp:label id="lblSoilData" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 128px"
				runat="server">Soil Data:</asp:label></form>
	</body>
</HTML>
