<%@ Page language="c#" Codebehind="wfEditMetStation.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditMetStation" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfEditMetStation</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 192px; POSITION: relative; HEIGHT: 40px" ms_positioning="GridLayout">
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 16px"
						tabIndex="2" runat="server" ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 100; LEFT: 40px; POSITION: absolute; TOP: 16px" tabIndex="3"
						runat="server" Font-Size="Smaller" EnableViewState="False">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 104; LEFT: 88px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 112px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton></DIV>
			</asp:panel>
			<asp:label id="lblStationName" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="88px">Station Name:</asp:label>
			<asp:textbox id="edtStationName" style="Z-INDEX: 103; LEFT: 128px; POSITION: absolute; TOP: 80px"
				tabIndex="1" runat="server" Height="24px" Width="224px"></asp:textbox>
			<asp:Label id="lblStationNumber" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 120px"
				runat="server" Height="16px">Station Number:</asp:Label>
			<asp:TextBox id="edtStationNumber" style="Z-INDEX: 105; LEFT: 128px; POSITION: absolute; TOP: 120px"
				runat="server" Width="224px"></asp:TextBox>
		</form>
	</body>
</HTML>
