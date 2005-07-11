<%@ Page language="c#" Codebehind="wfViewCrops.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfViewCrops" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfViewCrops</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Label id="lblCrops" style="Z-INDEX: 101; LEFT: 24px; POSITION: absolute; TOP: 80px" runat="server"
				Width="40px" Height="16px">Crops:</asp:Label>
			<asp:Panel id="pnlTop" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 488px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnDelete" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 16px"
						tabIndex="7" runat="server" EnableViewState="False" Font-Size="Smaller">Delete</asp:LinkButton>
					<asp:ImageButton id="btnDeleteImg" style="Z-INDEX: 103; LEFT: 384px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnImport" style="Z-INDEX: 102; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="4"
						runat="server" EnableViewState="False" Font-Size="Smaller">Import</asp:LinkButton>
					<asp:ImageButton id="btnImportImg" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 16px"
						tabIndex="3" runat="server" ImageUrl="Images\import.gif"></asp:ImageButton><INPUT class="stdInput" id="flImport" style="Z-INDEX: 105; LEFT: 88px; WIDTH: 280px; POSITION: absolute; TOP: 16px; HEIGHT: 25px"
						tabIndex="5" type="file" size="27" name="flImport" runat="server">
				</DIV>
			</asp:Panel>
			<asp:DropDownList id="cboCrops" style="Z-INDEX: 100; LEFT: 24px; POSITION: absolute; TOP: 104px" runat="server"
				Width="312px" AutoPostBack="True" tabIndex="1"></asp:DropDownList>
			<asp:ListBox id="lstCultivars" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 168px"
				runat="server" Width="312px" Height="360px" tabIndex="2"></asp:ListBox>
			<asp:Label id="lblCultivars" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 144px"
				runat="server" Width="56px" Height="16px">Cultivars:</asp:Label>
		</form>
	</body>
</HTML>
