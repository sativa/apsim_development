<%@ Page language="c#" Codebehind="wfAddUser.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfAddUser" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfAddConsultant</title>
		<meta name="vs_showGrid" content="True">
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Label id="lblName" style="Z-INDEX: 108; LEFT: 24px; POSITION: absolute; TOP: 88px" runat="server"
				ForeColor="Black" Width="56px" Height="16px">Name:</asp:Label>
			<asp:Panel id="pnlTop" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="10" runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 100; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="8"
						runat="server" Font-Size="Smaller" EnableViewState="False">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 103; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="9" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="7"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:TextBox id="edtName" style="Z-INDEX: 101; LEFT: 120px; POSITION: absolute; TOP: 80px" tabIndex="1"
				runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:TextBox id="edtEmail" style="Z-INDEX: 102; LEFT: 120px; POSITION: absolute; TOP: 120px"
				tabIndex="2" runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:Label id="lblEmail" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 128px" runat="server"
				ForeColor="Black" Width="48px" Height="16px">Email:</asp:Label>
			<asp:Label id="lblUserName" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 168px"
				runat="server" ForeColor="Black" Width="72px" Height="16px">User name:</asp:Label>
			<asp:TextBox id="edtUserName" style="Z-INDEX: 105; LEFT: 120px; POSITION: absolute; TOP: 160px"
				tabIndex="3" runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:TextBox id="edtPassword" style="Z-INDEX: 106; LEFT: 120px; POSITION: absolute; TOP: 200px"
				tabIndex="4" runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:Label id="lblPassword" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 208px"
				runat="server" ForeColor="Black" Width="64px" Height="16px">Password:</asp:Label>
			<asp:DropDownList id="cboAccessType" style="Z-INDEX: 110; LEFT: 120px; POSITION: absolute; TOP: 240px"
				runat="server" Width="249px" AutoPostBack="True" tabIndex="5" Height="24px"></asp:DropDownList>
			<asp:Label id="lblAccess" style="Z-INDEX: 111; LEFT: 24px; POSITION: absolute; TOP: 240px"
				runat="server" Height="16px" Width="88px">Access Type:</asp:Label>
			<asp:Label id="lblConsultant" style="Z-INDEX: 112; LEFT: 24px; POSITION: absolute; TOP: 272px"
				runat="server" Height="16px">Consultant:</asp:Label>
			<asp:DropDownList id="cboConsultant" style="Z-INDEX: 113; LEFT: 120px; POSITION: absolute; TOP: 272px"
				runat="server" Width="248px" tabIndex="6" Height="24px"></asp:DropDownList></form>
	</body>
</HTML>
