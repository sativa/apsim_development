<%@ Page language="c#" Codebehind="wfViewRegions.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfViewRegions" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfViewRegions</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 440px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnDelete" style="Z-INDEX: 104; LEFT: 400px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" EnableViewState="False" Font-Size="X-Small">Delete</asp:LinkButton>
					<asp:ImageButton id="btnDeleteImg" style="Z-INDEX: 103; LEFT: 376px; POSITION: absolute; TOP: 16px"
						tabIndex="7" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnImport" style="Z-INDEX: 102; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="5"
						runat="server" EnableViewState="False" Font-Size="X-Small">Import</asp:LinkButton>
					<asp:ImageButton id="btnImportImg" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\import.gif"></asp:ImageButton><INPUT class="stdInput" id="flImport" style="Z-INDEX: 105; LEFT: 72px; WIDTH: 280px; POSITION: absolute; TOP: 16px; HEIGHT: 25px"
						tabIndex="6" type="file" size="27" runat="server">
				</DIV>
			</asp:Panel>
			<asp:Label id="lblRegions" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 88px"
				runat="server" Height="16px" Width="56px">Regions:</asp:Label>
			<asp:DropDownList id="cboRegions" style="Z-INDEX: 101; LEFT: 24px; POSITION: absolute; TOP: 112px"
				runat="server" Width="312px" AutoPostBack="True" tabIndex="1" Height="24px"></asp:DropDownList>
			<asp:Label id="lblValues" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 216px"
				runat="server" Height="16px" Width="160px">Value:</asp:Label>
			<asp:ListBox id="lstValues" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 240px"
				runat="server" Width="312px" Height="322px" tabIndex="3"></asp:ListBox>
			<asp:DropDownList id="cboTypes" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 176px" runat="server"
				Width="312px" AutoPostBack="True" tabIndex="2" Height="24px">
				<asp:ListItem Value="Met Stations">Met Stations</asp:ListItem>
				<asp:ListItem Value="Soils">Soils</asp:ListItem>
			</asp:DropDownList>
			<asp:Label id="lblType" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 152px" runat="server"
				Height="16px" Width="24px">Type:</asp:Label>
		</form>
	</body>
</HTML>
