<%@ Page language="c#" Codebehind="wfViewRegions.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfViewRegions" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfViewRegions</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 624px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnDelete" style="Z-INDEX: 103; LEFT: 400px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" Font-Size="Smaller" EnableViewState="False">Delete</asp:LinkButton>
					<asp:LinkButton id="LinkButton1" style="Z-INDEX: 108; LEFT: 520px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" Font-Size="Smaller" EnableViewState="False">Check all soils</asp:LinkButton>
					<asp:LinkButton id="btnEdit" style="Z-INDEX: 107; LEFT: 480px; POSITION: absolute; TOP: 16px" tabIndex="8"
						runat="server" Font-Size="Smaller" EnableViewState="False">Edit</asp:LinkButton>
					<asp:ImageButton id="btnEditImg" style="Z-INDEX: 105; LEFT: 456px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\preferences.gif"></asp:ImageButton>
					<asp:ImageButton id="btnDeleteImg" style="Z-INDEX: 102; LEFT: 376px; POSITION: absolute; TOP: 16px"
						tabIndex="7" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnImport" style="Z-INDEX: 101; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="5"
						runat="server" Font-Size="Smaller" EnableViewState="False">Import</asp:LinkButton>
					<asp:ImageButton id="btnImportImg" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\import.gif"></asp:ImageButton><INPUT class="stdInput" id="flImport" style="Z-INDEX: 104; LEFT: 72px; WIDTH: 280px; POSITION: absolute; TOP: 16px; HEIGHT: 25px"
						tabIndex="6" type="file" size="27" runat="server">
				</DIV>
			</asp:panel><asp:label id="lblRegions" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 88px"
				runat="server" Height="16px" Width="56px">Regions:</asp:label><asp:dropdownlist id="cboRegions" style="Z-INDEX: 101; LEFT: 24px; POSITION: absolute; TOP: 112px"
				tabIndex="1" runat="server" Height="24px" Width="416px" AutoPostBack="True"></asp:dropdownlist><asp:label id="lblValues" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 216px"
				runat="server" Height="16px" Width="160px">Value:</asp:label><asp:listbox id="lstValues" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 240px"
				tabIndex="3" runat="server" Height="322px" Width="416px" SelectionMode="Multiple"></asp:listbox><asp:dropdownlist id="cboTypes" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 176px" tabIndex="2"
				runat="server" Height="24px" Width="416px" AutoPostBack="True">
				<asp:ListItem Value="Met Stations">Met Stations</asp:ListItem>
				<asp:ListItem Value="Soils">Soils</asp:ListItem>
			</asp:dropdownlist><asp:label id="lblType" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 152px" runat="server"
				Height="16px" Width="24px">Type:</asp:label>
			<asp:Label id="lblMultipleSelect" style="Z-INDEX: 108; LEFT: 128px; POSITION: absolute; TOP: 560px"
				runat="server" Width="184px" Height="16px" Font-Size="Smaller">Hold Ctrl to select multiple values</asp:Label></form>
	</body>
</HTML>
