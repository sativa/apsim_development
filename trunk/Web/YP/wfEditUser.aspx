<%@ Page language="c#" Codebehind="wfEditUser.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditUser" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
  <HEAD>
		<title>wfEditGrower</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
  </HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 40px" 
ms_positioning="GridLayout">
<asp:LinkButton id=btnCancel style="Z-INDEX: 101; LEFT: 112px; POSITION: absolute; TOP: 16px" tabIndex=8 runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton>
<asp:LinkButton id=btnSave style="Z-INDEX: 100; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex=6 runat="server" Font-Size="Smaller" EnableViewState="False">Save</asp:LinkButton>
<asp:ImageButton id=btnSaveImg style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 16px" runat="server" ImageUrl="Images\save.gif"></asp:ImageButton>
<asp:ImageButton id=btnCancelImg style="Z-INDEX: 103; LEFT: 88px; POSITION: absolute; TOP: 16px" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:Label id="lblName" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 80px" runat="server"
				Width="48px" Height="16px" ForeColor="Black">Name:</asp:Label>
			<asp:TextBox id="edtName" style="Z-INDEX: 100; LEFT: 104px; POSITION: absolute; TOP: 80px" tabIndex="1"
				runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:TextBox id="edtEmail" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 120px"
				tabIndex="2" runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:Label id="lblEmail" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 120px" runat="server"
				Width="32px" Height="16px" ForeColor="Black">Email:</asp:Label>
			<asp:ListBox id="lstConsultants" style="Z-INDEX: 115; LEFT: 104px; POSITION: absolute; TOP: 296px"
				runat="server" Height="168px" Width="248px" SelectionMode="Multiple"></asp:ListBox>
			<asp:DropDownList id="cboAccessType" style="Z-INDEX: 114; LEFT: 104px; POSITION: absolute; TOP: 264px"
				tabIndex="5" runat="server" Height="24px" Width="249px" AutoPostBack="True"></asp:DropDownList>
			<asp:Label id="lblAccess" style="Z-INDEX: 115; LEFT: 16px; POSITION: absolute; TOP: 264px"
				runat="server" Height="16px" Width="82px">Access Type:</asp:Label>
			<asp:Label id="lblConsultant" style="Z-INDEX: 115; LEFT: 16px; POSITION: absolute; TOP: 296px"
				runat="server" Height="16px">Consultant:</asp:Label>
			<asp:Label id="lblConsultantTwo" style="Z-INDEX: 114; LEFT: 128px; POSITION: absolute; TOP: 464px"
				runat="server" Height="16px" Width="216px" Font-Size="Smaller">Hold Ctrl to select multiple Consultants</asp:Label>
			<asp:Button id="btnPassword" style="Z-INDEX: 115; LEFT: 152px; POSITION: absolute; TOP: 496px"
				runat="server" Text="Change Password"></asp:Button>
			<asp:ListBox id="lstUsersCrops" style="Z-INDEX: 107; LEFT: 104px; POSITION: absolute; TOP: 160px"
				runat="server" Height="72px" Width="248px" SelectionMode="Multiple"></asp:ListBox>
			<asp:Label id="lblUsersCropsTwo" style="Z-INDEX: 107; LEFT: 128px; POSITION: absolute; TOP: 232px"
				runat="server" Height="16px" Width="184px" Font-Size="Smaller">Hold Ctrl to select multiple Crops</asp:Label>
			<asp:Label id="lblUsersCrops" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 160px"
				runat="server" Height="16px">Crops:</asp:Label>
		</form>
	</body>
</HTML>
