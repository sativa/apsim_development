<%@ Page language="c#" Codebehind="wfEditClimateForecast.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditClimateForecast" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfClimateForecast</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="13" runat="server" EnableViewState="False" Font-Size="X-Small">Cancel</asp:LinkButton>
					<asp:LinkButton id="btnSave" style="Z-INDEX: 100; LEFT: 32px; POSITION: absolute; TOP: 16px" tabIndex="11"
						runat="server" EnableViewState="False" Font-Size="X-Small">Save</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 103; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="12" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="10"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:DropDownList id="cboSOIMonth" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 104px"
				runat="server" Width="216px" tabIndex="1" Height="24px">
				<asp:ListItem Value="1">January</asp:ListItem>
				<asp:ListItem Value="2">February</asp:ListItem>
				<asp:ListItem Value="3">March</asp:ListItem>
				<asp:ListItem Value="4">April</asp:ListItem>
				<asp:ListItem Value="5">May</asp:ListItem>
				<asp:ListItem Value="6">June</asp:ListItem>
				<asp:ListItem Value="7">July</asp:ListItem>
				<asp:ListItem Value="8">August</asp:ListItem>
				<asp:ListItem Value="9">September</asp:ListItem>
				<asp:ListItem Value="10">October</asp:ListItem>
				<asp:ListItem Value="11">November</asp:ListItem>
				<asp:ListItem Value="12">December</asp:ListItem>
			</asp:DropDownList>
			<asp:Label id="lblSOIMonth" style="Z-INDEX: 103; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server">SOI month:</asp:Label>
			<asp:Label id="lblSOIPhase" style="Z-INDEX: 104; LEFT: 304px; POSITION: absolute; TOP: 80px"
				runat="server">SOI phase:</asp:Label>
			<asp:DropDownList id="cboSOIPhase" style="Z-INDEX: 105; LEFT: 304px; POSITION: absolute; TOP: 104px"
				runat="server" Width="216px" tabIndex="2" Height="24px"></asp:DropDownList>
			<asp:Label id="lblSOIDescription" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 144px"
				runat="server">SOI description:</asp:Label>
			<asp:TextBox id="edtSOIDescription" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 168px"
				runat="server" Width="496px" Height="136px" TextMode="MultiLine" tabIndex="3"></asp:TextBox>
			<asp:Label id="lblAnalogueYears" style="Z-INDEX: 108; LEFT: 24px; POSITION: absolute; TOP: 328px"
				runat="server">David Stephens' analogue years:</asp:Label>
			<asp:TextBox id="edtAnalogueYearOne" style="Z-INDEX: 109; LEFT: 24px; POSITION: absolute; TOP: 352px"
				runat="server" Width="80px" tabIndex="4" Height="26px" MaxLength="4"></asp:TextBox>
			<asp:TextBox id="edtAnalogueYearTwo" style="Z-INDEX: 109; LEFT: 128px; POSITION: absolute; TOP: 352px"
				runat="server" Width="80px" tabIndex="5" Height="24px" MaxLength="4"></asp:TextBox>
			<asp:TextBox id="edtAnalogueYearThree" style="Z-INDEX: 109; LEFT: 232px; POSITION: absolute; TOP: 352px"
				runat="server" Width="80px" tabIndex="6" Height="24px" MaxLength="4"></asp:TextBox>
			<asp:TextBox id="edtAnalogueYearFour" style="Z-INDEX: 109; LEFT: 336px; POSITION: absolute; TOP: 352px"
				runat="server" Width="80px" tabIndex="7" Height="24px" MaxLength="4"></asp:TextBox>
			<asp:TextBox id="edtAnalogueYearFive" style="Z-INDEX: 109; LEFT: 440px; POSITION: absolute; TOP: 352px"
				runat="server" Width="80px" tabIndex="8" Height="24px" MaxLength="4"></asp:TextBox>
			<asp:Label id="lblDavidsDiscription" style="Z-INDEX: 108; LEFT: 24px; POSITION: absolute; TOP: 400px"
				runat="server">David Stephens' description:</asp:Label>
			<asp:TextBox id="edtDavidsDescription" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 424px"
				runat="server" Width="496px" Height="136px" TextMode="MultiLine" tabIndex="9"></asp:TextBox>
		</form>
	</body>
</HTML>
