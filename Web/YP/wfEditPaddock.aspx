<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfEditPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditPaddock" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfEditPaddock</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 730px; POSITION: relative; HEIGHT: 44px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 102; LEFT: 104px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 103; LEFT: 80px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px" runat="server"
						ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSettingUpImg" style="Z-INDEX: 105; LEFT: 160px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\preferences.gif"></asp:ImageButton>
					<asp:ImageButton id="btnRainfallImg" style="Z-INDEX: 106; LEFT: 264px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\rainfall.gif"></asp:ImageButton>
					<asp:ImageButton id="btnReportsImg" style="Z-INDEX: 107; LEFT: 504px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\reports.gif"></asp:ImageButton>
					<asp:LinkButton id="btnSettingUp" style="Z-INDEX: 108; LEFT: 184px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller">Setting Up</asp:LinkButton>
					<asp:LinkButton id="btnRainfall" style="Z-INDEX: 109; LEFT: 288px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller">Rainfall</asp:LinkButton>
					<asp:CheckBox id="chkEmail" style="Z-INDEX: 110; LEFT: 624px; POSITION: absolute; TOP: 16px" runat="server"
						Height="16px" Font-Size="Smaller" ForeColor="Purple" Text="Email con/par"></asp:CheckBox>
					<asp:LinkButton id="btnReport" style="Z-INDEX: 111; LEFT: 528px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller">Create Report</asp:LinkButton>
					<asp:DropDownList id="cboReport" style="Z-INDEX: 112; LEFT: 352px; POSITION: absolute; TOP: 16px"
						runat="server" Width="145px" Font-Size="Smaller"></asp:DropDownList>
					<asp:Button id="btnSave" style="Z-INDEX: 112; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						BackColor="Transparent" Height="16px" Width="32px" Font-Size="Smaller" ForeColor="Purple"
						Text="Save" BorderColor="Transparent" BorderStyle="None" Font-Underline="True"></asp:Button></DIV>
			</asp:panel><asp:label id="lblCropManagement" style="Z-INDEX: 101; LEFT: 16px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="176px">In crop management for user: </asp:label><asp:label id="lblName" style="Z-INDEX: 102; LEFT: 208px; POSITION: absolute; TOP: 80px" runat="server"
				Height="16px">Name</asp:label><asp:checkbox id="chkSown" style="Z-INDEX: 103; LEFT: 48px; POSITION: absolute; TOP: 120px" runat="server"
				Text="Have you sown yet?" TextAlign="Left" AutoPostBack="True" tabIndex="1" Height="16px" Width="144px"></asp:checkbox><asp:calendar id="cldSowDate" style="Z-INDEX: 104; LEFT: 208px; POSITION: absolute; TOP: 120px"
				runat="server" Width="248px" Height="180px" BackColor="White" Font-Size="8pt" ForeColor="Black" DayNameFormat="FirstLetter" Font-Names="Verdana" BorderColor="#999999" CellPadding="4" tabIndex="2">
				<TodayDayStyle ForeColor="Black" BackColor="White"></TodayDayStyle>
				<SelectorStyle BackColor="PaleGoldenrod"></SelectorStyle>
				<NextPrevStyle VerticalAlign="Bottom"></NextPrevStyle>
				<DayHeaderStyle Font-Size="7pt" Font-Bold="True" BackColor="#CCCCCC"></DayHeaderStyle>
				<SelectedDayStyle Font-Bold="True" ForeColor="White" BackColor="PaleGoldenrod"></SelectedDayStyle>
				<TitleStyle Font-Bold="True" BorderColor="Black" BackColor="PaleGoldenrod"></TitleStyle>
				<WeekendDayStyle BackColor="White"></WeekendDayStyle>
				<OtherMonthDayStyle ForeColor="Gray"></OtherMonthDayStyle>
			</asp:calendar><asp:dropdownlist id="cboCrops" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 320px"
				runat="server" Width="248px" AutoPostBack="True" tabIndex="3" Height="24px"></asp:dropdownlist><asp:dropdownlist id="cboCultivars" style="Z-INDEX: 106; LEFT: 208px; POSITION: absolute; TOP: 360px"
				runat="server" Width="248px" tabIndex="4" Height="24px"></asp:dropdownlist><asp:label id="lblCultivar" style="Z-INDEX: 108; LEFT: 104px; POSITION: absolute; TOP: 360px"
				runat="server" Height="16px" Width="88px">Cultivar Type:</asp:label><asp:label id="lblCrop" style="Z-INDEX: 107; LEFT: 120px; POSITION: absolute; TOP: 320px" runat="server"
				Height="16px" Width="72px">Crop Type:</asp:label><asp:label id="lblNitrogen" style="Z-INDEX: 110; LEFT: 8px; POSITION: absolute; TOP: 400px"
				runat="server"> Nitrogen fertiliser applications:</asp:label>
			<jwg:gridEX id="grdNitrogen" style="Z-INDEX: 111; LEFT: 208px; POSITION: absolute; TOP: 400px"
				runat="server" Height="120px" Width="302px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch">
				<RootTable DataMember="Nitrogen" Key="Nitrogen">
					<Columns>
						<jwg:GridEXColumn UseType="System.Single" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="ID" Width="0px" Visible="False">
							<CellStyle Width="0px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
							DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="Application Date" Width="140px">
							<CellStyle Width="140px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Rate" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Application Rate (kg/ha)" Width="160px">
							<CellStyle Width="160px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"
					Padding="0"></SelectedFormatStyle>
				<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
				<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
				<PageNavigatorSettings>
					<BottomPageNavigatorPanels>
						<jwg:GridEXPageNavigatorItemCountPanel></jwg:GridEXPageNavigatorItemCountPanel>
						<jwg:GridEXPageNavigatorEmptyPanel Width="100%"></jwg:GridEXPageNavigatorEmptyPanel>
						<jwg:GridEXPageNavigatorPreviousBlockPanel Align="right"></jwg:GridEXPageNavigatorPreviousBlockPanel>
						<jwg:GridEXPageNavigatorPreviousPagePanel Align="right"></jwg:GridEXPageNavigatorPreviousPagePanel>
						<jwg:GridEXPageNavigatorPageSelectorDropDownPanel Align="right"></jwg:GridEXPageNavigatorPageSelectorDropDownPanel>
						<jwg:GridEXPageNavigatorNextPagePanel Align="right"></jwg:GridEXPageNavigatorNextPagePanel>
						<jwg:GridEXPageNavigatorNextBlockPanel Align="right"></jwg:GridEXPageNavigatorNextBlockPanel>
					</BottomPageNavigatorPanels>
					<TopPageNavigatorPanels>
						<jwg:GridEXPageNavigatorItemCountPanel></jwg:GridEXPageNavigatorItemCountPanel>
						<jwg:GridEXPageNavigatorEmptyPanel Width="100%"></jwg:GridEXPageNavigatorEmptyPanel>
						<jwg:GridEXPageNavigatorPreviousBlockPanel Align="right"></jwg:GridEXPageNavigatorPreviousBlockPanel>
						<jwg:GridEXPageNavigatorPreviousPagePanel Align="right"></jwg:GridEXPageNavigatorPreviousPagePanel>
						<jwg:GridEXPageNavigatorPageSelectorDropDownPanel Align="right"></jwg:GridEXPageNavigatorPageSelectorDropDownPanel>
						<jwg:GridEXPageNavigatorNextPagePanel Align="right"></jwg:GridEXPageNavigatorNextPagePanel>
						<jwg:GridEXPageNavigatorNextBlockPanel Align="right"></jwg:GridEXPageNavigatorNextBlockPanel>
					</TopPageNavigatorPanels>
				</PageNavigatorSettings>
				<AlternatingRowFormatStyle BorderStyle="Solid" BackColor="Control" Height="20px" BorderWidth="1px"></AlternatingRowFormatStyle>
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
					VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="20px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
		</form>
	</body>
</HTML>
