<%@ Page language="c#" Codebehind="wfAddPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfAddPaddock" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfAddPaddock</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 54px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 102; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 101; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="6"
						runat="server" EnableViewState="False">Save</asp:LinkButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 16px" runat="server"
						ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 104; LEFT: 80px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:TextBox id="edtName" style="Z-INDEX: 102; LEFT: 216px; POSITION: absolute; TOP: 136px" tabIndex="1"
				runat="server" Width="248px" Height="24px" EnableViewState="False"></asp:TextBox>
			<asp:Label id="lblPaddockName" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 144px"
				runat="server" Width="104px" Height="18px" ForeColor="Black">Paddock Name:</asp:Label>
			<asp:Label id="lblCropManagement" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 96px"
				runat="server" Height="18px">In crop management for user: </asp:Label>
			<asp:Label id="lblName" style="Z-INDEX: 105; LEFT: 216px; POSITION: absolute; TOP: 96px" runat="server"
				Height="18px">Name</asp:Label>
			<asp:CheckBox id="chkSown" style="Z-INDEX: 106; LEFT: 64px; POSITION: absolute; TOP: 184px" runat="server"
				TextAlign="Left" Text="Have you sown yet?" AutoPostBack="True"></asp:CheckBox>
			<asp:Calendar id="cldSowDate" style="Z-INDEX: 107; LEFT: 216px; POSITION: absolute; TOP: 184px"
				runat="server" BackColor="White" Height="180px" Width="248px" ForeColor="Black" DayNameFormat="FirstLetter"
				Font-Size="8pt" Font-Names="Verdana" BorderColor="#999999" CellPadding="4" tabIndex="2">
				<TodayDayStyle ForeColor="Black" BackColor="White"></TodayDayStyle>
				<SelectorStyle BackColor="PaleGoldenrod"></SelectorStyle>
				<NextPrevStyle VerticalAlign="Bottom"></NextPrevStyle>
				<DayHeaderStyle Font-Size="7pt" Font-Bold="True" BackColor="#CCCCCC"></DayHeaderStyle>
				<SelectedDayStyle Font-Bold="True" ForeColor="White" BackColor="PaleGoldenrod"></SelectedDayStyle>
				<TitleStyle Font-Bold="True" BorderColor="Black" BackColor="PaleGoldenrod"></TitleStyle>
				<WeekendDayStyle BackColor="White"></WeekendDayStyle>
				<OtherMonthDayStyle ForeColor="Gray"></OtherMonthDayStyle>
			</asp:Calendar>
			<asp:DropDownList id="cboCrops" style="Z-INDEX: 108; LEFT: 216px; POSITION: absolute; TOP: 384px"
				runat="server" Width="248px" AutoPostBack="True" tabIndex="3" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboCultivars" style="Z-INDEX: 109; LEFT: 216px; POSITION: absolute; TOP: 424px"
				runat="server" Width="248px" tabIndex="4" Height="24px"></asp:DropDownList>
			<asp:Label id="lblCrop" style="Z-INDEX: 110; LEFT: 136px; POSITION: absolute; TOP: 384px" runat="server"
				Height="22px">Crop Type:</asp:Label>
			<asp:Label id="lblCultivar" style="Z-INDEX: 111; LEFT: 120px; POSITION: absolute; TOP: 424px"
				runat="server">Cultivar Type:</asp:Label>
		</form>
	</body>
</HTML>
