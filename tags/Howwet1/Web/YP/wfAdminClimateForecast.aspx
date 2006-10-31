<%@ Page language="c#" Codebehind="wfAdminClimateForecast.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminClimateForecast" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Climate Forecast</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 720px" height="720" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 755px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" BorderStyle="None" BorderColor="White" Width="800px"
								Height="40px">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										tabIndex="13" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px"
										Width="88px" CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" tabIndex="12"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="168px"
										CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Large" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="785px"
									Height="20px">Change Climate Forecast</asp:Label></DIV>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 736px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:Label id="lblSOIMonth" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 176px"
								runat="server" Font-Names="Arial">SOI month:</asp:Label>
							<asp:DropDownList id="cboSOIMonth" style="Z-INDEX: 105; LEFT: 280px; POSITION: absolute; TOP: 176px"
								tabIndex="1" runat="server" Font-Names="Arial" Width="480px" Height="24px">
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
							<asp:Label id="lblSOIPhase" style="Z-INDEX: 106; LEFT: 168px; POSITION: absolute; TOP: 224px"
								runat="server" Font-Names="Arial">SOI phase:</asp:Label>
							<asp:DropDownList id="cboSOIPhase" style="Z-INDEX: 107; LEFT: 280px; POSITION: absolute; TOP: 224px"
								tabIndex="2" runat="server" Font-Names="Arial" Width="480px" Height="24px"></asp:DropDownList>
							<asp:TextBox id="edtSOIDescription" style="Z-INDEX: 108; LEFT: 280px; POSITION: absolute; TOP: 272px"
								tabIndex="3" runat="server" Font-Names="Arial" Width="481px" Height="136px" TextMode="MultiLine"></asp:TextBox>
							<asp:Label id="lblSOIDescription" style="Z-INDEX: 109; LEFT: 136px; POSITION: absolute; TOP: 272px"
								runat="server" Font-Names="Arial">SOI description:</asp:Label>
							<asp:Label id="lblAnalogueYears" style="Z-INDEX: 110; LEFT: 16px; POSITION: absolute; TOP: 440px"
								runat="server" Font-Names="Arial">David Stephens' analogue years:</asp:Label>
							<asp:TextBox id="edtAnalogueYearOne" style="Z-INDEX: 111; LEFT: 280px; POSITION: absolute; TOP: 440px"
								tabIndex="4" runat="server" Font-Names="Arial" Width="80px" Height="26px" MaxLength="4"></asp:TextBox>
							<asp:TextBox id="edtAnalogueYearTwo" style="Z-INDEX: 112; LEFT: 384px; POSITION: absolute; TOP: 440px"
								tabIndex="5" runat="server" Font-Names="Arial" Width="80px" Height="24px" MaxLength="4"></asp:TextBox>
							<asp:TextBox id="edtAnalogueYearThree" style="Z-INDEX: 113; LEFT: 488px; POSITION: absolute; TOP: 440px"
								tabIndex="6" runat="server" Font-Names="Arial" Width="80px" Height="24px" MaxLength="4"></asp:TextBox>
							<asp:TextBox id="edtAnalogueYearFour" style="Z-INDEX: 114; LEFT: 584px; POSITION: absolute; TOP: 440px"
								tabIndex="7" runat="server" Font-Names="Arial" Width="80px" Height="24px" MaxLength="4"></asp:TextBox>
							<asp:TextBox id="edtAnalogueYearFive" style="Z-INDEX: 115; LEFT: 680px; POSITION: absolute; TOP: 440px"
								tabIndex="8" runat="server" Font-Names="Arial" Width="80px" Height="24px" MaxLength="4"></asp:TextBox>
							<asp:TextBox id="edtDavidsDescription" style="Z-INDEX: 116; LEFT: 280px; POSITION: absolute; TOP: 496px"
								tabIndex="9" runat="server" Font-Names="Arial" Width="481px" Height="136px" TextMode="MultiLine"></asp:TextBox>
							<asp:Label id="lblDavidsDiscription" style="Z-INDEX: 117; LEFT: 48px; POSITION: absolute; TOP: 496px"
								runat="server" Font-Names="Arial">David Stephens' description:</asp:Label>
							<asp:Button id="btnSave" style="Z-INDEX: 118; LEFT: 256px; POSITION: absolute; TOP: 680px" runat="server"
								Width="120px" Height="32px" Text="Save changes" tabIndex="10"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 119; LEFT: 416px; POSITION: absolute; TOP: 680px"
								runat="server" Width="120px" Height="32px" Text="Cancel changes" tabIndex="11"></asp:Button></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
