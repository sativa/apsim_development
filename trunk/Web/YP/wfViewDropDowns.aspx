<%@ Page language="c#" Codebehind="wfViewDropDowns.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfViewDropDowns" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfViewDropDowns</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 504px; POSITION: relative; HEIGHT: 40px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnDeleteValue" style="Z-INDEX: 101; LEFT: 360px; POSITION: absolute; TOP: 16px"
						tabIndex="9" runat="server" EnableViewState="False" Font-Size="X-Small">Delete value</asp:LinkButton>
					<asp:LinkButton id="btnDeleteType" style="Z-INDEX: 108; LEFT: 128px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" EnableViewState="False" Font-Size="X-Small">Delete type</asp:LinkButton>
					<asp:ImageButton id="btnDeleteTypeImg" style="Z-INDEX: 107; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:LinkButton id="btnAddType" style="Z-INDEX: 106; LEFT: 32px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" EnableViewState="False" Font-Size="X-Small">Add type</asp:LinkButton>
					<asp:ImageButton id="btnAddTypeImg" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 16px"
						tabIndex="3" runat="server" ImageUrl="Images\add.gif"></asp:ImageButton>
					<asp:LinkButton id="btnAddValue" style="Z-INDEX: 100; LEFT: 256px; POSITION: absolute; TOP: 16px"
						runat="server" EnableViewState="False" Font-Size="X-Small">Add value</asp:LinkButton>
					<asp:ImageButton id="btnDeleteValueImg" style="Z-INDEX: 103; LEFT: 336px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnAddValueImg" style="Z-INDEX: 104; LEFT: 232px; POSITION: absolute; TOP: 16px"
						tabIndex="7" runat="server" ImageUrl="Images\add.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:DropDownList id="cboDropDownTypes" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 104px"
				runat="server" Width="312px" AutoPostBack="True" tabIndex="1" Height="24px"></asp:DropDownList>
			<asp:Label id="lblDropDownTypes" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="112px">Drop down type:</asp:Label>
			<asp:Label id="lblDropDownValue" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 144px"
				runat="server" Height="16px" Width="112px">Drop down value:</asp:Label>
			<asp:ListBox id="lstDropDownValues" style="Z-INDEX: 105; LEFT: 24px; POSITION: absolute; TOP: 168px"
				runat="server" Width="312px" Height="360px" tabIndex="2"></asp:ListBox>
		</form>
	</body>
</HTML>
