<%@ Page language="c#" Codebehind="wfEditSetupPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditSetupPaddock" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock Setup</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="9" runat="server" Font-Size="X-Small" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 112; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						BackColor="Transparent" Height="16px" Width="32px" Font-Size="Smaller" Text="Save" Font-Underline="True"
						BorderStyle="None" ForeColor="Purple" BorderColor="Transparent"></asp:Button>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 103; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="6"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:Label id="lblPaddockSetup" style="Z-INDEX: 102; LEFT: 56px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="160px">In paddock setup for user: </asp:Label>
			<asp:Label id="lblName" style="Z-INDEX: 103; LEFT: 232px; POSITION: absolute; TOP: 80px" runat="server">Name</asp:Label>
			<asp:Calendar id="cldInitialConditions" style="Z-INDEX: 101; LEFT: 232px; POSITION: absolute; TOP: 328px"
				runat="server" Width="248px" Height="180px" BackColor="White" Font-Size="8pt" ForeColor="Black"
				DayNameFormat="FirstLetter" Font-Names="Verdana" BorderColor="#999999" CellPadding="4" tabIndex="5">
				<TodayDayStyle ForeColor="Black" BackColor="White"></TodayDayStyle>
				<SelectorStyle BackColor="PaleGoldenrod"></SelectorStyle>
				<NextPrevStyle VerticalAlign="Bottom"></NextPrevStyle>
				<DayHeaderStyle Font-Size="7pt" Font-Bold="True" BackColor="#CCCCCC"></DayHeaderStyle>
				<SelectedDayStyle Font-Bold="True" ForeColor="White" BackColor="PaleGoldenrod"></SelectedDayStyle>
				<TitleStyle Font-Bold="True" BorderColor="Black" BackColor="PaleGoldenrod"></TitleStyle>
				<WeekendDayStyle BackColor="White"></WeekendDayStyle>
				<OtherMonthDayStyle ForeColor="Gray"></OtherMonthDayStyle>
			</asp:Calendar>
			<asp:DropDownList id="cboSubSoil" style="Z-INDEX: 105; LEFT: 232px; POSITION: absolute; TOP: 240px"
				runat="server" Width="248px" tabIndex="4" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboSoilType" style="Z-INDEX: 106; LEFT: 232px; POSITION: absolute; TOP: 200px"
				runat="server" Width="248px" tabIndex="3" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboWeatherStation" style="Z-INDEX: 107; LEFT: 232px; POSITION: absolute; TOP: 160px"
				runat="server" Width="248px" tabIndex="2" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboRegion" style="Z-INDEX: 108; LEFT: 232px; POSITION: absolute; TOP: 120px"
				runat="server" Width="248px" AutoPostBack="True" tabIndex="1" Height="24px"></asp:DropDownList>
			<asp:Label id="lbRegion" style="Z-INDEX: 109; LEFT: 168px; POSITION: absolute; TOP: 120px"
				runat="server" Width="48px" Height="16px">Region:</asp:Label>
			<asp:Label id="lblWeatherStation" style="Z-INDEX: 113; LEFT: 72px; POSITION: absolute; TOP: 160px"
				runat="server" Width="144px" Height="16px">Closest weather station:</asp:Label>
			<asp:Label id="lblSoilType" style="Z-INDEX: 110; LEFT: 152px; POSITION: absolute; TOP: 200px"
				runat="server" Width="64px" Height="16px">Soil Type:</asp:Label>
			<asp:Label id="lblSubSoil" style="Z-INDEX: 111; LEFT: 96px; POSITION: absolute; TOP: 240px"
				runat="server" Width="120px" Height="16px">Sub soil constraints:</asp:Label>
			<asp:Label id="lblInitialConditions" style="Z-INDEX: 112; LEFT: 112px; POSITION: absolute; TOP: 320px"
				runat="server" Width="104px" Height="16px">Initial Conditions:</asp:Label>
			<jwg:gridEX id=grdSoilSampleOne style="Z-INDEX: 114; LEFT: 24px; POSITION: absolute; TOP: 568px" runat="server" Height="180px" Width="502px" DataSource="<%# dsSoilSampleOne %>" DataMember="SoilSampleOne" GridLineColor="ScrollBar" GroupByBoxVisible="False" AllowEdit="True" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" ImagesFolderPath="/gridex/images" UpdateMode="RowUpdateBatch">
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="20px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<AlternatingRowFormatStyle BorderStyle="Solid" BackColor="Control" Height="20px" BorderWidth="1px"></AlternatingRowFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
					VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
				<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
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
				<RootTable DataMember="SoilSampleOne" Key="SoilSample">
					<Columns>
						<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Water" DataMember="Water" DefaultGroupPrefix="Water (% Vol):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Water (% Vol)" Width="133px">
							<CellStyle Width="133px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="NO3" DataMember="NO3" DefaultGroupPrefix="NO3 (ppm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="NO3 (ppm)" Width="133px">
							<CellStyle Width="133px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="NH4" DataMember="NH4" DefaultGroupPrefix="NH4 (ppm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="NH4 (ppm)" Width="133px">
							<CellStyle Width="133px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
			</jwg:gridEX>
			<asp:Label id="lblDepthOne" style="Z-INDEX: 115; LEFT: 24px; POSITION: absolute; TOP: 520px"
				runat="server">Depths should be entered as 0-10, 10-20, etc.</asp:Label>
			<asp:Label id="lblDepthTwo" style="Z-INDEX: 116; LEFT: 24px; POSITION: absolute; TOP: 544px"
				runat="server">Starting water is in % (volumetric)</asp:Label>
			<asp:CheckBox id="chkLinkedRainfall" style="Z-INDEX: 117; LEFT: 8px; POSITION: absolute; TOP: 280px"
				runat="server" Text="Link rainfall to existing paddock" AutoPostBack="True" TextAlign="Left"></asp:CheckBox>
			<asp:DropDownList id="cboLinkedRainfall" style="Z-INDEX: 113; LEFT: 232px; POSITION: absolute; TOP: 280px"
				tabIndex="4" runat="server" Height="24px" Width="248px"></asp:DropDownList>
			<jwg:gridEX id=grdSoilSampleTwo style="Z-INDEX: 114; LEFT: 24px; POSITION: absolute; TOP: 768px" runat="server" Height="180px" Width="502px" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="SoilSampleTwo" DataSource="<%# dsSoilSampleTwo %>">
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="20px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<AlternatingRowFormatStyle BorderStyle="Solid" BackColor="Control" Height="20px" BorderWidth="1px"></AlternatingRowFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
					VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
				<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
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
				<RootTable DataMember="SoilSampleTwo" Key="SoilSample">
					<Columns>
						<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="OC" DataMember="OC" DefaultGroupPrefix="OC (%C):" InvalidValueAction="DiscardChanges"
							NullText="" Caption="OC (%C)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="EC" DataMember="EC" DefaultGroupPrefix="EC (dS/m):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="EC (dS/m)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="PH" DataMember="PH" DefaultGroupPrefix="PH (CaCl2):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="PH (CaCl2)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="ESP" DataMember="ESP" DefaultGroupPrefix="ESP (%):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="ESP (%)"></jwg:GridEXColumn>
					</Columns>
				</RootTable>
			</jwg:gridEX>
		</form>
	</body>
</HTML>
