<%@ Page language="c#" Codebehind="wfEditPassword.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditPassword" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfPassword</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 107; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left" tabIndex="-1">
				<DIV style="WIDTH: 272px; POSITION: relative; HEIGHT: 40px" tabIndex="-1" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="6" runat="server" EnableViewState="False" Font-Size="Smaller">Cancel</asp:LinkButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 102; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="4"
						runat="server" EnableViewState="False" Font-Size="Smaller">Save</asp:LinkButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="3"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 104; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:Label id="lblPasswordOne" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 88px"
				runat="server" Width="144px" Height="16px" ForeColor="Black" tabIndex="-1">Enter a new password:</asp:Label>
			<asp:TextBox id="edtPasswordOne" style="Z-INDEX: 103; LEFT: 184px; POSITION: absolute; TOP: 80px"
				tabIndex="1" runat="server" Width="248px" Height="24px" EnableViewState="False" TextMode="Password"></asp:TextBox>
			<asp:TextBox id="edtPasswordTwo" style="Z-INDEX: 104; LEFT: 184px; POSITION: absolute; TOP: 120px"
				tabIndex="2" runat="server" Width="248px" Height="24px" EnableViewState="False" TextMode="Password"></asp:TextBox>
			<asp:Label id="lblPasswordTwo" style="Z-INDEX: 105; LEFT: 24px; POSITION: absolute; TOP: 128px"
				runat="server" Width="152px" Height="16px" ForeColor="Black" tabIndex="-1">Re-enter new password:</asp:Label>
		</form>
	</body>
</HTML>
