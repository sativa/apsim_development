<%@ Page language="c#" Codebehind="wfEditSetupPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditSetupPaddock" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="xceedchart" Namespace="Xceed.Chart.Server" Assembly="Xceed.Chart.Server, Version=3.0.100.0, Culture=neutral, PublicKeyToken=ba83ff368b7563c6" %>
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
			<asp:Panel id="pnlTop" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 100; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="9" runat="server" EnableViewState="False" Font-Size="Smaller">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 104; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						BackColor="Transparent" Height="16px" Width="40px" Font-Size="Smaller" Font-Names="Times New Roman"
						BorderColor="Transparent" ForeColor="Blue" BorderStyle="None" Font-Underline="True" Text="Save"></asp:Button>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 102; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="6"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<asp:Label id="lblPaddockSetup" style="Z-INDEX: 101; LEFT: 56px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="160px">In paddock setup for user: </asp:Label>
			<asp:Label id="lblName" style="Z-INDEX: 102; LEFT: 232px; POSITION: absolute; TOP: 80px" runat="server">Name</asp:Label>
			<asp:DropDownList id="cboSubSoil" style="Z-INDEX: 104; LEFT: 232px; POSITION: absolute; TOP: 240px"
				runat="server" Width="248px" tabIndex="4" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboSoilType" style="Z-INDEX: 105; LEFT: 232px; POSITION: absolute; TOP: 200px"
				runat="server" Width="248px" tabIndex="3" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboWeatherStation" style="Z-INDEX: 107; LEFT: 232px; POSITION: absolute; TOP: 160px"
				runat="server" Width="248px" tabIndex="2" Height="24px"></asp:DropDownList>
			<asp:DropDownList id="cboRegion" style="Z-INDEX: 108; LEFT: 232px; POSITION: absolute; TOP: 120px"
				runat="server" Width="248px" AutoPostBack="True" tabIndex="1" Height="24px"></asp:DropDownList>
			<asp:Label id="lbRegion" style="Z-INDEX: 109; LEFT: 168px; POSITION: absolute; TOP: 120px"
				runat="server" Width="48px" Height="16px">Region:</asp:Label>
			<asp:Label id="lblWeatherStation" style="Z-INDEX: 116; LEFT: 72px; POSITION: absolute; TOP: 160px"
				runat="server" Width="144px" Height="16px">Closest weather station:</asp:Label>
			<asp:Label id="lblSoilType" style="Z-INDEX: 110; LEFT: 152px; POSITION: absolute; TOP: 200px"
				runat="server" Width="64px" Height="16px">Soil Type:</asp:Label>
			<asp:Label id="lblSubSoil" style="Z-INDEX: 111; LEFT: 96px; POSITION: absolute; TOP: 240px"
				runat="server" Width="120px" Height="16px">Sub soil constraints:</asp:Label>
			<asp:Label id="lblInitialConditions" style="Z-INDEX: 112; LEFT: 112px; POSITION: absolute; TOP: 360px"
				runat="server" Width="104px" Height="16px">Initial Conditions:</asp:Label>
			<jwg:gridEX id=grdSoilSampleOne style="Z-INDEX: 117; LEFT: 24px; POSITION: absolute; TOP: 448px" runat="server" Height="184px" Width="502px" DataSource="<%# dsSoilSampleOne %>" DataMember="SoilSampleOne" GridLineColor="ScrollBar" GroupByBoxVisible="False" AllowEdit="True" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" ImagesFolderPath="/gridex/images" UpdateMode="RowUpdateBatch">
				<RootTable DataMember="SoilSampleOne" Key="SoilSample">
					<Columns>
						<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Water" DataMember="Water" DefaultGroupPrefix="Water (% Vol):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Water (% Vol)" Width="133px">
							<CellStyle Width="133px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="NO3" DataMember="NO3" DefaultGroupPrefix="NO3 (ppm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="NO&lt;sub&gt;3&lt;/sub&gt; (ppm)" Width="133px">
							<CellStyle Width="133px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="NH4" DataMember="NH4" DefaultGroupPrefix="NH4 (ppm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="NH&lt;sub&gt;4&lt;/sub&gt; (ppm)" Width="133px">
							<CellStyle Width="133px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
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
					VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="24px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
			<asp:Label id="lblDepthOne" style="Z-INDEX: 119; LEFT: 24px; POSITION: absolute; TOP: 400px"
				runat="server">Depths should be entered as 0-10, 10-20, etc.</asp:Label>
			<asp:Label id="lblDepthTwo" style="Z-INDEX: 120; LEFT: 24px; POSITION: absolute; TOP: 424px"
				runat="server">Starting water is in % (volumetric)</asp:Label>
			<asp:CheckBox id="chkLinkedRainfall" style="Z-INDEX: 121; LEFT: 8px; POSITION: absolute; TOP: 280px"
				runat="server" Text="Link rainfall to existing paddock" AutoPostBack="True" TextAlign="Left"></asp:CheckBox>
			<asp:DropDownList id="cboLinkedRainfall" style="Z-INDEX: 113; LEFT: 232px; POSITION: absolute; TOP: 280px"
				tabIndex="4" runat="server" Height="24px" Width="248px"></asp:DropDownList>
			<jwg:gridEX id=grdSoilSampleTwo style="Z-INDEX: 118; LEFT: 24px; POSITION: absolute; TOP: 648px" runat="server" Height="184px" Width="502px" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="SoilSampleTwo" DataSource="<%# dsSoilSampleTwo %>">
				<RootTable DataMember="SoilSampleTwo" Key="SoilSample">
					<Columns>
						<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="OC" DataMember="OC" DefaultGroupPrefix="OC (%C):" InvalidValueAction="DiscardChanges"
							NullText="" Caption="OC (%C)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="EC" DataMember="EC" DefaultGroupPrefix="EC (dS/m):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="EC (dS/m)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="PH" DataMember="PH" DefaultGroupPrefix="PH (CaCl2):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="pH (CaCl&lt;sub&gt;2&lt;/sub&gt;)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="ESP" DataMember="ESP" DefaultGroupPrefix="ESP (%):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="ESP (%)"></jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
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
					VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="24px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
			<jwg:gridEX id=grdSampleDate style="Z-INDEX: 114; LEFT: 232px; POSITION: absolute; TOP: 360px" runat="server" Height="20px" Width="268px" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="InitialDate" DataSource="<%# dsInitialDate %>" ColumnHeaders="False">
				<RootTable DataMember="InitialDate" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="InitialDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="InitialDate" DefaultGroupPrefix="InitialDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="InitialDate" Width="248px">
							<CellStyle Width="248px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
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
					VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
					BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
			<jwg:gridEX id=grdStartOfGrowingSeason style="Z-INDEX: 115; LEFT: 232px; POSITION: absolute; TOP: 320px" runat="server" Height="20px" Width="268px" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="StartOfGrowingSeason" DataSource="<%# dsStartOfGrowingSeason %>" ColumnHeaders="False">
				<RootTable DataMember="StartOfGrowingSeason" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="GrowingSeasonDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="GrowingSeasonDate" DefaultGroupPrefix="GrowingSeasonDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="GrowingSeasonDate" Width="248px">
							<CellStyle Width="248px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
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
					VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
					BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
			<asp:Label id="lblStartGrowingSeason" style="Z-INDEX: 122; LEFT: 64px; POSITION: absolute; TOP: 320px"
				runat="server">Start of Growing Season:</asp:Label>
			<xceedchart:chartservercontrol id="cscSoilChart" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 864px"
				runat="server" Height="422px" Width="616px" Visible="False">
				<Xceed.Chart.Server.StringHolderCount Count="124"></Xceed.Chart.Server.StringHolderCount>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED7D077C1CC5D5F8ED4977D2A958B26CCB452E72016C5C902C17D960B02CB92297582E9812B1D2ADA4C357C4DD49968C31BD432018D3EC506CC24787D07B270642087F12AA091F10124280502D5B526CFFE7CDEEECBEDD9DB93D87838FFC72A7DF3DEDCEBC29AFCEEC9BD93997E472B9F6930FFC874F9E9B8095C7342A8A7F52&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;758B1C8D4FAA8E449509A5AB94682C1009CFAA985436A9BCAC6C52D984D2EAB660BC2DAACC0A2B6DF1A81C9C50BAACAD2118683C5AE95C1159AB846735C895154D4D15D32A1BA64F9D56D1382D2F8354BD06575D1797C37E39EA4F49F51E2061A4B5E7EA6575241C8F468273E4989245B0B243F53439E60BD5D72ACD4AD81F23&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;49B57283128CE585EA57CB71251A92A36BE1668EDCB8B6391A690BFB7342F5754A3C1E0837C70687EA17860992DC180FB407E29D4B5BC9759C1010CB543FA5A27E04834A2320BAB99D553B63C61AC7E3987A4B7B6C60037FC772B1758ACCC823B8C806C58033D8D649C604E8DC346E0D02DE981B87E2BE4C001E005E00201C5F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;36001F01391EC87664650E41EA13AA0F5741830ACDF486EA5774B62A59A1FAD81239A40C59AE340448D71A824A55342A77D60662F1F12A4A093F8B161B00087E6B89625B2A452E3452EA037125142B4009B1C07AA508DDB7ABEAEECA90E08FD09F3DA2AE33464A4D5A19262CAB53A2013918584FD9B62012F42BD1EF9B9F9DED&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D62CDC07FCCA31C15C02254D241E4F529A09F2F90FE1B2CEE41F87C7BE3CCA530CF3317FBD0764D3693EF3F80CFEC3D787F216C30202210BDC8627EB40BC619ACD42361752D662D8D7657C3CD9498E2384ADF3A2846432A4CE0B0483739B9A08E70B2CE3281939C738D4452BC910DA1042D45B01EC51CE035586A63DBE2200FD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;00F4070A7D094741A03FC3DFDA90119B1CC9504E8A679C148F91AF3F4391E58CC658D4130D36445A19BBA45C293B579224CD199F7E94CBAD5D90C44CF0C64E52A1FA5D23C7651FC500BDCAAB8AC5945043B013544A7249D9DE0104A9983F6E826D788B7FB0495E218C25A026BE8104E41748D403429B830818281857E834003A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5598CF8A0F518BF761C54B847A66B84D4ADBD01F74965958C03A380CC0700023A0ABF95221EB6A2901C3137B1E4A30F4B4B0AF565F7E61DE28F26F8526FE9AA8BC8EA897D1E772D2E7A9654976BAACA2696AD3F4A6F272FFD432B942F68046276558A309625F32975A18929B159A020AD6174F8389D38C44F3346B66370D0BE4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;585D8BEC8FAC233761BD603EB959DA4668AE8B7706E9DDC27018DDCD51DA95E0EA803FDE52C8EE6A88AF93C38D4A7FD226BE672DD10A6A03CD2D717243EBA637B95A4E0D616EAE9601D7C4EDD092647E1FA60D91597C78991C27D9611F7495587F243A8CF678593442E6AAF180121B6FEA372FD7A083976BD0552ACA65748DB1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;23D8E91E6A4732F8C0C934F852222809BC29111484BC81F63CDA93411C7A28A14338191A9F0773B254BE17D706C29C4606DA92D53606DBD3B52606D973D416C888E282AF4B7D2E03E769BA21DFD1FCA1C16401E075FB9B4D7312EDEAA84419D2D8646C8ED52F18454119A8A2258B939D7DE03DFD3132B2B3B3FF0B484C94075D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F7FC79FFFEFDC9A81C3C3064B5CBC136A5BEDE458BD2F9D67BA438B70D3AE9A5F8396BC39175619AE8214E24AE6486499DA08FBEACAC5168F27613792AC99176921AA15673AAE75D9294B4FE0A3AFB0EA9C3499EFCA2D2DBA4E83B5AAF58704A7ACBD2552FEDEA9B96540F4D7DC392EAA3A97FB2A466D1D43F72EB7DDD923A1A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;525D3460A6F649FA7FB893AFF17AFC076ECDAF727BFC7B6E8F5FE1F6F877DC7A5F76ECF14B4E18799504ACC6329B5F9BB2D9A1A7DF014DDC614648C6EBC5306B22F360F2B4B042E9800617CBADC4D3F731FC3FE8204C21E647657F4009C7A96615A1845532A9211C273831D5CC024105A6CBD9DA9053CE2E260F0BD557F9DB61&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E8F5B3C2354AAC311A68854938419B138887E45632B1A0AD0E30BA3BDEE8EA1053AAA9D7834D59988012738E899661FC3C8D2C73493385FD4C792AB19CB4C9634D690958602EAC72A3C8DC3DA0860EB152A6FE512F87D9358BFC636CABB43EA9E9182616565A27D81A1AE265A53572C86AC24C153667E66EE5BF35169471E948&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C057686680A53695BBA384B31483ED6C169166F00FCC603A88EF202E3431A7E1B9CE2D97B9E572B73CD92D57B8FD656E7FB9DB3FD9EDAF70C7CADCB172776CB23B56E156CADC4AB95B99EC56487A8BCBFAC9B57E2A59E4E080BEF803F75F55F96602212F10421C246D1EA02BD990F53C29E9A80082B2CF91B249E805BFB4F748&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3AB23EC39DB63CCD1BDF72723C4F09675D66090B66234F92E23B68B52962BCF438A9ED05365B788C5C3CCF6E1E2517CF69373EA0557A984BEA437C52A50749DA535ABAA77F72B12FAF1AFF8CAD5C5E4B06DCD88A4824180FB40E2197D56DB1782444DC42555491ABE2F168A0813C23C6E0E9BDBA2D1A8B44816B64A46D58DAAA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;90A7E225CABAD581B03FB28E0CD6E195E1C0C96DCA92B650831285F8963082A357040C97D4B8E39126E079403895344AF385E72A83F203C521B13C754910826D4A8C70A1AA438965C13260902E07D2E7E8C511BF1224481535AB02CA3AC2A386558158A021A8878B495E137D48F5918B050A9481A41AA535DE021311F23CDF1A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;943B9786D5885771A87E5954F12B4DE4B9D54F7B41ED457D2EA5B3C7CC919C9823F4D0BC3C38C28655D511B0E08CE1D30D0426A84C67B0C1800CABD33707E154E650191247957D90AD591EC96EB616580B60318025009602580675797F0602A4255CAE8BE7B85C2F92EF0DB3A97EDF45D422B976CCCAC156C33C431284254131&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7289EC9647D6559399290499E0B18A5EF723D70B22D1C0FA48382E0717CBD1E640182267AB2002D1C8526096089C2B3261570503CDE142844C13ACC170A23E0B141902F1A1FA799108C9C963CBD510FFCD55837244A9C20AD517ED439E33B34B04044157DC8231D9D23DD1F86EEA7265F29175519760DEE1FE1EF9C00AB730E0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AA7388AD38E7685F997C212AEAB99328502276D9D506523C770846300B17ED23185D03BC5D30769A986B2F0B29BE550056033806C01A00C742A5C3928890C3E26E3F705076CD6DB2696E0C789FAB3EBE406C2D66D550D2A3CCCCDCDCE1E246A1E8012EBFB85C0BA8797B8F031151FF59DA1E2BA5A1F3D2B1078DF31D0F149F00&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EC80E8FB305AE44152E48839DE9FC303ECB2AAD5D5A5B34A8F39A63414F2D503F289803CC284BC6C8E1754209720EBB80D80DB08AC9C99DC3419422E85464AADDC19698BE71FA3A5D4115E2AF96BF01D3CB1327CA25B507E28A719BDAE4A3A01A4B2F76C253A9310993F5B623311690CA9A396BBB66F1D5672D5450035794524&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;48644466D4038822D0EBB8E2D7B2C890FB1FB2689A99A92F9B960AE8D709E57B1B8364B7EBC7DA4AE0D962F513BCFE0A06B66BB98ECD20835FCCD74C97783164CF24D4EF2D4E62CA01FE154637485DE8A7A3A5124D6FC9E1CB986E790A50466378929605CB8D9E25C9CEE1D2DB1884BB72D652B66218C42C5E9AC4EC1790DD4A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D81D6C373DD80763AA7D644A30EBCD3D545C4B5DA42D4A57F4D036383A5B767DF355EB91EC0BDEDA17824E2D4B66BA2D691B1A891CA272660977D452332BD9068730B409F388FEF6072DE932E2371E608F95D7120179563B4CCF32D5ED1DBCE90279226C0A9211435FE3D5EED5072478224C62F2A0499076F1646DD10EBE124C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8056EB593194E559E33065847D06F00C3BB7A35556277A30B927539FB575445F134DFA350AAA95A0BAC2DBDF9CA65296AF0DF44B9B9A628A892984510BC144C803A0310983D5CB18CD26B5B1799891388C2ECC06C93DAF48899E6B2F99A9A926FD8876F5193C70ABEB54E4938458C625602F90687EB41C2D300BBC6C9BF17F88&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;47C7DB8B89DE3B73893F72D26DA8DAFF4CF4F5B5016807B00E4007804E00EBA1C9639D9F5D72D565101671306DB7009B52AF54553BC69AB006E69C73FDCDCA3285F89E306CB96AB2186593C92861A7047D6036EDD4220FB06AAD46525F6D0F88C14797A16D9244942837777462CA1C167DCD5BB892C1E16F72B174D370BB1EBA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F6A6FA417D2DEE1C61A8D0DC6F7EB4C9B701C0A90036426DC7BB1C7D5C3F75D49E47ECDA245B90DCD228C443E9704654A04913B271B906E467550053C21AE2521AA82F22236F506E555787FA62F7C3C20FA6C74F1C5F20BD6B5AAEF821A0353FAA28F070D83487100E0955C1D616394F6F94A0F5D16F2872BE7E0B458C4C5A30&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D3D01926807E96103DF085EEA6C886E1554A65EC2289A0010BBBD381F33497FD0371A8D992743AD19A3B5808F73472713B0BE19E692B42E3C2A6AF74826034DE8047E318998148F0444B1F81CF1177663DEE4C27EECCF9C974E6444167DA7167DAA0330DAC3317893B13C79D89E1CEFC2299CE340A3AD38A3BB30E3AD3CC36E0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FD32C1131CBC03626C376CD177DF4901567A132C13711F718C7227B15D809B015C09E02A005703B806C0B500B640D53E692DAB7A2B0143C41379A3FE20ABFF3A00D703B801C08D00B601D80EE0265ABF07268B0738EDFC77A6ECF050A4C6E67FAD3D0ED247C2B095DB966967811A456D5A1E51FD18F88AB941A59DDE80273936&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1209D1001499CD2A518656A8A51017B02A126C0BC186C1A65AA529AEC6A7C08F2CA753737A0B95AE88B4AA3730C6CD89C4E9BA05DCF757A776E46A5E205E178710467327046BC9987092CA1378B021C51A56C694450158FA22CD42A08C8C9AB15685BE5952156E0E2A10DD3F8606A5E06A0DBDD2B557F561FA4ADD186EB4C6D2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8D4AEB1C45E79EB973955A54BDD2642F1009BFFF5997ABB85ABDBF7F8EFAFFBD399E63896D24D503C11ADD1AEB3028E8183F4C49BEAD73F45DCF273337718BD84DACC66E62157613B727E32662AC893BC44DD4E12696E326EE4EA68936D6C43DE226966A4D501E2CC14DDC9744139EF6039A57A71FF7858FFBF7D3477C0C1FD0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;775511E55E97E49381F6689BC416E24C877D8174C2B988B7642BE1FD59F074009D93163AA27632D4058EA8EB19EA7C2754CF862466DAB9FF01FBB2D27B8E7EDC2D31D2BC546E91780C6AACC15B24AAF1168939B62D12B3B95B248E126C9138126D9190E0590DB45E9AA553607C5E7A71C191F6FB2428781E6A3C1C5330135330&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C346C1742E05D304144CC59B3C3626FBE4DB578B7AFD27BC93F163BC1B60791320BD3DDD254DC1BB982B785B9A2773B71E9773B7349771B7341FC6DDD23C895BEF44C7C16D82E398769A20AA40E50EA34D26047D3DEA0E1C985848442F7207714A50146057892593F8D6D69640630C4A433E7CBD6F40E35530C890B96195E750&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D24961A5E66934DBA5EF1907AB7809DAB217A3528498C3B19C10D7D3B3E12B8D25B59EC3BCCF3B00DE05B0134A40580118E67D0FF57E6895349AE01FCA746114B980AED1C6CE4FDCD848DCD807003E04F01194B88835F617736343716325B8B15F246E6C086EEC13007F07F0296C0CF80714C5E6BB321E0806C027A46673FB66&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EBF65B73D0A923105B017DD15746FBC256B3400C85F872D514AA1028E2ABEF2DF3AA332D536416663E912899F992EAE46874B11C6E9383EA5916F0404B5C5473B3128562311A1A54EFD5D8200D16AB09B5E0C16245341A4C7CF68A40E3DA5A25DC4C37AF3551D76D4E5A1C084750527FCDD7D314CDDFC262865112251A6559E2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;00D4F7C5F2496AAE399595012AE88D5E1812A08C9ED04F2BA7A5D18D7C5A1A2B48D3B261D12A100FC27A0FFD0F5C2AD06400FB3520DADF1FDDD704424A18F4A408252E690B91896463314AAA8D3493695CBC251468EC878B9329E78A008D64AC02939D1789866488A3372D8BC402A000AA580AF0E640925544FB0ED5303CDB56&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1C32E7AE5620619502431B29A1F735A62669BAB5BC2DA844F3999E9169A002FC5317F0C331BA8C14D3A853B77EAAAAD40712C6EBAA5BACDD5AF4B7102553252ED0129826E76BF7AA3AE76A77C0F6BEDAB5A1D80C57D56ED6A245C5FBB306B09E0F60AD98949DA59A357E20C3B5A8BD9E6ED17D3DDD620083B574BB15B01CBB29&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B01CBB3D0CB1526B18853D4BB78C01B83EC3B658AAC94606E16AB0A1E00C93B53079A826C344ADDB4D7F2C7B663C83AD89BA560EB4E6686654624D47B634C8569B6650455A06B22AA62F16D362BD34D9D74083649391F5D7658A2CAD9F9688CD8D5560B5396C0FD4F08A4C0642AD6F004A324C10B307DBA1FEFA2A5ED8452FDA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1829C963669772A3DDC8AEDDDCB8B56EE574576432DB009359C24B76C1760C77249F640E59FD83F60C168EFEAFD69593EE66D2883FFD1E26A109C3F9DAC4FC866DC7801945B733FE315A569F02580789B19087B16D8EB6B4ABF91B5B850637900B82F341801DB987706B34B9A00033B3B136DC25D827993755FC3B7BAFF5B090&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EAA22A93C8E79B3E756809584BDD9BB9BF63053B0789D703BD747A6B01902DE723D11D2B69FF95F65F69FF95F65FFFA1FE4BDFB23D80BBDDDBE2CB04DBBDFB93B2095C1CBF14497DFA28DF171095F827802F017CA5E6B5CE769BDE34751DE5FB1AB2BF01F02D80EF00EC02D0056037803D00BAF59D1ABDD0B54CEB3BA6767BE1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BF65E3DB0B601F80FD5AF0D0E593B440A467DFBEFDFB93D54CC166F70CA83093004FEF3ECB6E798B1AD1D85387D44997D2F465F841CF12063EE57295CE967A4805500970D4E57A750EE56C16D49F0DC0072047EBBAB48B200E60E1ADEFC84D7F23C8FAC30B44FA98B498A92D5227E4B1F43EC1DCB74F0BA7E9DC9276EAC4220E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BC9B240724D82EB399ED137803B3E24F36564C845293001C06A04C12B1A21CB22703A8003005C05400D3004C0750096086C4587138F4E579CC8A59340FC051006603A862AC785263055D5EAE86AC1AA8E0319DEABC392A2BEE795A7A94C78AF950660180850016491A071EC01CB8FF47E7C04D4973E03A3E07B6F038706D921C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F05CC53DE27509DAD1978E9BA6E3A6E9B8693A6E9A8E9BA6E3A6E9B8693AEE908E3BA4E30EE9B8433A6E9AF65F69FF95F65FFFE57153EA82366B11141A4EB9C216410941B0014280BE0880566104E564C88E02880180CD75BE3600ED00E04D7B5F07804E3D82720A0440CEC6119453216F2380D3009C0EE00C1641D9A84550E8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4B546741D6D950C12946B0E429976BC5B3349CB65E4FBC7C8E1A4521A49C0765CE077001800B258D036D9803F11F9D03FEA439F0733E078EE371E0D8243920C15B7757B1AD8F2B312B562056B868F93BA1D45D00EE06708F9015BF81EC7B01DC07E07E000F007810C043001E06F088CE8AC7A02F351A2B68C4F409C87B12C053&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;009E06F00C63C5911A2BE8CF1F3D0759CF4305879B58016721DDF3B43493C78A1D50E645002F017859D23830157360CA8FCE8131497360389F03253C0E0C4992039E6BB82F7F2EA16FEAA56389E958623A96988E25A66389E958623A96987E164F3F8BA79FC5D3CFE2E95862DA7FA5FD57DA7FFDB7C712E9999203B4E009F547&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FD6DC1932F20CEF04F005F02F84A183CF91AB2BF01F02D80EF00EC02D0056037803D00BAF5E0492FC43E3271F0642FE4ED0300B11C1F74D2C77E3A54DAB7570D9ED01FCECE80AC4C02A4DEBD7837D60ABA3D4FEAD98B7763C1D755E5CB8232B017D2E70390E3D638B06B2FE2C0777B7F6C0E7CBC37590EBCCFE7C04E1E07DE4D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;92039E6BB9BABA5C265601AA73B67AF449D3312425D01683CB35EA259C16A53E1FD42871B9B1857897B81AF0A886639622D14E3577007950AE6A8B47CCA979ECF7E7C1B9A5C353E9F0D47F5E78AA18BFE96E84A9865B922DFA3C88934D957BA0258369FA004BBAAAF6FD2CA920A6C19634C320AC75A8D661EDA9C554865A3B84&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED6698B5572623B2E69A2D6A84B5ACC5BC6CF9165BB3E55B0C6FA425DF6E85560CBB495A31ECF6394AC43DC358C528BAE50EE3B5A3B761CB35D97429AF7A6CE03C0493B55BF542357DAB8AEA7E60284F7799531829CAD4AD6E8408437317A345F9C877940A5BD11CC9100B02F22A567DB7B8182B75267F33C2CE4A93F3196AD3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;39E4894A2C99D82D592BB6FA289EBFA00E6B08D79150EF358C9365B8329E18D261F774D83D1D76FF8985DDE1F924F38708BE278F693DD53561CC28999F364887CDD261B374D82C1D364B87FDD3FE2BEDBFD2FE2BEDBFD2FE2BEDBFD2FE2BEDBFD2FEEB008E8E513F37CCF68DA787334C70AB47E34A7FDC8BCE96787DAFF5BD80&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C360D9AB0C403980C96ED15A5E05644F013015C03400D30154029801602680C3DD6C2D6F162CC53DBB17BD177014E441D5BE2A00730054B3B5BCC7F7A28346E642D63CA8E0117DD9CEF8480F73127D0BA1CC22004703A8651CB80F73E0DE1F9D03DB92E6C0563E07AEE171E0EA03E1C026CC81CB7F740E9C99340736F039D0C9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E34047B21CD8C235E26591A0BA9E7BAE703D177E6E688ED21C08D3DFFD81DF17A2177571A5D5B458CB5BF72D08D5ABD61A53DD613F5600B9C8F4226F7A9137BDC89B5EE44D2FF2A61779D38BBCE945DEF4226F7A9137BDC84BD75F5DD2FFFD326F3231BDDCE40321E940633AD0980E34A6038DE98592B4FF4AFBAFB4FF4AFBAF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B4FF4AFBAFB4FF4AFBAFB4FF3AF0855EBA4257E5BBC4ADAEFA7A7F412EB28FF02BCD514539D277A91B7E3C97388556BCF217B1ADFC5D0E8B649B005C0160B370E5EF4AC8BE0AC0D500AE01702D802D00B602F81580EBF495BF1B60E1AE1EAFFC6D83BCED006E02F06B0037B395BF3578E5EF16C8BA152A58C55BF95BC95BF9BB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;03CADC09E02E0077330E2CC31C58FAA373E0A8A4393083CF81693C0E4C3D100E94610E1CF6A3736044D21C18CCE740318F030392E480E73A9231D86663B54A531C427E45F0CBC3A17A70EEBC5556FEF2AA75C54D5F94821AC7ABB50DB4A4D996B5B474B59DC19654CE129696A3F660A825D5D49D424A27EA4B7F9CC03A528413&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D55E14E324A30B264CB5FDC138C9D4B81E5FD3A17D0CD40B43E7D459552AA74C490E17E97E1D58BFA0614FE15EDE0F39993A28FC9DA9D9B37D5F83597E03E05BB0EC3C521BD42844C87640F05CCFB5EE3972E3DAB475A7AD3BDDAF03B26E295333374937B7BD6069FB00C0612092CB09E15FFF4A8CE0B9817FB846A0B9253D1E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A72D36DDAF03B4D83D9ABD69C32CB1375F069C63032037034EEF7142F8CA01C17323D762E74523E1B4C5A62D36DDAF03B4D8CF357BCBD0ED6D00985A31808160907F7742F8D801C1B38D7BFE39BD9C178C44A279A930D99F8C8DA4B5F0DFD0C20F341DCAD4E76923417D4601180D4AF69E038267BBF894342031AD63FFF53AF6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B6A6421EDD4F8D03ED3914C078D0B13F3920786E12BFB991D6B1B48E91865FD354C8ABAB503968CF640015A063AF382014FE9AA41642F0B892DCE51748B790EBD340B16680065645037290205449BF25151DCA02E72F909B71FBD52158BA9D8063D51F24D7BEFDB5B620742E3D4F70CF61A759CE82768F047014F4EE0ED6D86C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;73634FE1C69EC48DDD9DB8B127706335D0CE5C00F3A0B17B5863F3CD8D3D8C1B7B0837765FE2C61EC48D1D0DEDD402580C8DDD0F6F5541634B322C0BE25AFCBD5909FB6BE4B8BC30AE846090F11583201ED0A4915F283D46FECD845EDC439AD9AA1F40AA2E484863E097BB19C6DD228C77C8ED06C0B88B60ECA0186C4123D92F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FEC0FD5755BE3A2A3A52E30B8C6BB7939BE7D9CD6DE4E639C6165807916E2129EF1947A8BA6E22436A8EF43F96D4D1909A9323DD4CD29F6295BDCB28F8B54E81F179E9C50547DAEF93A0E078A0603BA6601BA6E0461B05D77329B84E40C1AF30053B09D8485F6F24A9EFB0D42DE8465FD4B9D6529F97B6728D25D54353AFB6A4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FA68EA5596D42C9A7A25B7DECD5AEA7E3DC74D55D605B7EA0F8939627CC0C4B329950A1606F1FC128BE7322C9E4B6DE2B9842B9E8B05E2B9088BE74346C185A954B07540C1F99882F33005E7DA28389B4BC159020ACEC4147CC414EC0CAC60A7F314EC34AE226CE42AD8A95C05DBC055B053B8F5AE77549F4E478C4F98783A52&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A960178078DAB178DAB078E236F144B9E23959209E562C9EBF330A22A954B0CB818210A6208829586BA320C0A5A045404133A6E053A6604D58C1149E82F9B98AD0C855B006AE82C95C053B915B6FBDA3FAFCDC11E30B028E87D6B767D83698B866C357928E27B5DCC1E83E8EDCDCCE587BB3AD90BA59007FA57F320D5863D261&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5C8277EFA001B78006ACC61AB00A6BC04A9B06D4713560B940037E8635E04B02D6D1DD243C6C09F1D3F355B25BC83CC6D34B56A83EB6440E29857AE6F8FA009917C50A50422CB05E2942F7ED4A14F6866548A4A211759D31823F6965984C8EEB1498DA05D6D33D8D0B2241F204929D0D3BB47CBF212CF3DE4B4091FA5E55296C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;352A555FBBF1DD8744297DCD08AE752258FA86A11EED88FA2D435DE488FA1D01C04ADF43D0E787519FE9BB5CA5F46D30DF23B8CFBB4C451EC345E02D2FADC8E3B84817EB50B5A5438B6C1DDACD50E75850A7D850F7B08E3C45DACAF13D9D817EA8B4DB9CF72CEE4D2F33C5E7C4A6780436C5C3B129FE360953F4EC4D72C76116&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EC380CD58797B4851AE0ED4298A92F8B04C2F1D81C25BE4E51C2EA8BBE893054B52A25CFD03581586B50EEE4D791AF1D86AFA2F72535EA6FFB5547DAC2F1817AE7C69B304B70BAA58C467F76B624654B740B82C4BE92B6958058EABEA476559610AC3CD2F2CA98B2341A680E847DA17ABF7A45BAEE57DF5C8323210AB4F70FA1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;17B0B9B3503DF95FCF07DA68FF8DB71973588ADC91636402DE7242867F4584D62577585302E13CBD69928D6E02E102E086DCA1F3C289BFA53AC1744FE7785B27070B10E48EC1C2A29C4ACD14392004C2253604835E715E203CDC9667E2C6507E97355DE5944D852EB2D70DAD1FC9EBB56F2A35E910DAD99BFC4670F878BD44F1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D1956E04E8F3D5519E19C48124D705FBAE1EBA9FEFC50CAD412B6D39AC3D875C6A89FB93DEB93C8460720CCDAFA3CC91638AD92EE1D736E676B446C24A386E6875DA18FF1B8D516088E4332EB1155836CF7BBD92D8CC3CD3218696747D7CCB523F636633534D8195C1BF2476FD8389E503E722E1784B4C3BECC83027DBB06635&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C7B465FD770E73A2512E3B2FE9A1EBE0C44683DF4B7118E59E0E1CFA128C4F2F43A87B2AB1C724ABE61BE3F7B73D4048EE559A0CD5FCAADB82F1B6A8B230DC14C90ED5ABD970A698AA01EA7D46A654AA3DF8CD0F461AF467BE49A8F048C74681F1F4E9F015E05505E1957319339BFEA13DDCD0C76C4F867400AFF7F8EC0FC143&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;962B0D81B05F6E082AC6D3AE8A52C2CFA2C50698D5492B516C4BFD1E0FDAF097C4C3F6F7CDCFA67AE7FB037D40C4F0350233D8AFD4654AEA6A8CFAA6850EA44944800FB0C7D345E439CE9325257CB709445042EC993CE0449980EAE2046F99126D2473A6C196BCB961BF96934BBCC3C230614F5CF113E7BC4C26CF44CD604DE4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;264CEE14EA1908961F6EEA085BFB826247A3A42CA4006A3F828A921686FD4A473FEAE9F4347A049F4A76267DA8B3BFC8490931DAA7FCE3CCAD299AA5032A2EDBA6EEA7D63A91F0D0B111FE3E778FC90B8C99ED99C09D62F37AC2AFD0FAF1644BC9BE8B96362EA171BD410D0AC33733F417155C1E9F74006FF1A5D92C64F35B94&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B518BE8DD90C61B3A4DE7F4CB358C8E277285B317C17B1589A286931C59DE298E2181C531C8D638AEF2713DE9F2469E1FD91A90CEF7F0091851138BC3F1C87F787D9C2FB25DCF0FE1041787F300EEF1F2669B1DD418E71E932498BDF422CDD07B174DF6738865BCEAAEAEF58D56486DACF11B582A11639A24E915014DC078173&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DFD7B883534DF91025F77D87F3A7B1A6721D43E2D3196A8E6348BC523285BDF7E0166798F37A70DEE14C7D7BC5EA9B89D53703ABEFBE64D4771669622F3F442C1D49FEEDE345CE2489B403D1331A058379C9F77E46908E22FFA0DFD2FEDEFDFBA76B75A73E0E20CDD628353D28B98184BDA4E1A95AC3DFBF9D2A4965A60F9E0C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A55E52373C5CE80F0A5235C9838705CE4C373B13CD746B0433DDAE5E34D3AD25335D693EC1CC92D04C4EDA45706036C79B9A49DF91CC09FF32AFE0EA9AB780D403532DCE54A14F2672B00B0147E28E7585186F118160131C875D84F1424CE3FB650A35FEF35EA4F19FF5228D2FCE4C42E3C3CC617FDA9B42873D08A4FC492F72&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D87FEB450EFBAFBD5687FD975E9EC3FEA897EFB03FEC450E3BC23CCF07BD4EFEB0D5EEB04723264927B3AADE73AC2ACA50773AA2C618EABB8EA871BBC31E873BD86677D813707E3B6BEA8FBD4E0E7B1D437DBDD7C96177989D72592672909DE6BCC9B837A730F5AD10ABEF2B587D7F87D5775A32EA7BAAD561A3E6370A1DF64B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BDAAC3A6FEA73291C37625747078371173D83B7E58877D3ACF61CF04125E48A9C33E8339EC23A0EEE76C0EFB2CB1C39E8D1DF6D90287FD2476D84BC0619F6773D84F2472D88F2772D8E78B1DF63CEC602F103BEC0518EF42B1C35E84F1EE641A7FB458E3EFC71A7F1FD6F825C968FC5DCC61FF26950E7B1948F96EECB0EFC20E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FB4E9BC3BE9DEBB06F1338EC5BB1C3BE9B799E5B1CFDE13D76877D1CF630BF6155DDE458D5BD0C75BB23EA7D0C759B23EAFD76872DE30E3E6077D87E9CFF206B6A8BA3C37E88A15EEBE8B01F363BE516DCE223E6BC9370DE634C7DD78AD5F70AACBE9BB0FA869351DF2712CCB09F143AEC5FF6A219766B4A66D84F31877DE90F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EBB09FE639EC289070494A1DF633CC61C3C2A67491CD613F2776D81DD8613F2F70D8E76287BD0C1CF60E9BC33E2791C33E3B91C37E51ECB0376207FB92D8619F8EF15E163BEC3331DE174CE3CF126BFC06ACF1A7608D3F37198DFF2773D89DA974D8E78394D76187DD8E1D769BCD61C7B80E3B2A70D8276387FD25F33CAD8EFE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F02BBBC3DE843DCCD7ACAAA06355DF30D4B58EA8DF32D4931C51BFB33BEC6B700777D91DF6569CDFC59A6A7474D8BB196A83A3C3DE6376CA37E016BBCD79DB705E2F53DFED62F53D1EABEF71587D6F4E467DF72670D8FB840E7B0D76D8B7A4C461EF670E7BF50FEBB05D6E8EC3BE0D48589952872DB935877D07D45D6773D819&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6EA1C3BE475DFE527FD02BD3CD77D84BB0C35E0E0E3BCB6D75D88B1339ECDA440E1B02CE0287FD2076B03EB7D0613F8CF172DC4287FD28C6835FB4D31D8CF771923594BE675C5A2DC795E648B4B3545D15673B999FC0E6023F8447CDE549B1B9546173998DCDE59964CCE530D6C4B3E22666E1268EC04DBC904C13F0EB6C7440&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9999CA0165076861251E50A6E301659A6D4099C21D502A0403CA643CA0C04FCB51CF58EEE8AF27BB6D03CAEB58A415ACAA898E554D61A8131C51A732D4F18EA8D3DCB601E56DDCC1E96EDB80B213E757B2A6C6380E283318EA68C70165A6DB3468FC2F6EF17073DE87386F1653DF8FC4EA3B0CABEF50ACBE7F4D467DE147FF44&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;030A1C34C91F5086E001E593940C28F09B83744019F4C30E28737803CAA74042714A07946A36A07C0675F7B70D2873C503CA97F809609E604029C003CA0A185016DA06943E890694FC4403CA22F180B20B0F00478B0794DD18AF563CA07463BC4B98C6F78835DE83353E136BFCDE6434FE5226987D546F6D82B99C7541F208BB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B0AF0775616F0FEA42A627892E6C6263466F4F0AC70CAF07E6A73D68CCD8D383C68CDD3DD63163570F6FCCF8AE873F667CDB83C68C2B98F3FBA6C7C9256FB68F197D1193A42B5955FF74ACEA2A86FA8523EAD50CF57347D46BEC63C640DCC16BED63C6109CBF8535F5D71EA731632B43FDB8C769CCF895795C188E5BBCCE9C57&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8AF36E60EA3B52ACBEEF63F5FD3356DF31C9A8EFB60463C676E198B1B3078D19077B52E1636F6263C63B3D3FE898F16BDE9831164878AB279563C6CDCC351D0A75BFD163754DB788C78C491E3466DC2A18335EEB4163C62A1833EEB08D197FE8493066BCDA9360CCB8533C664CF1201F7F9778CC9886F1EE168F199518EF6BE6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4D77F424FB22B64DC1EDDE742688E005EC4D9FC7DEF4399B377D86EB4D9F1678D3A7B037FD86B985271DDDC2B702D13E81455B0FA2DDCBD8F2782AD93217D8F22866CB23982D0FDBD8F220972D0F08D8723F66CB3EC696FB1CD9B25FC0967B315B64600B9CA1A8AED8A4922DCB802D7763B6DC85D972A78D2DB773D9729B802D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B762B6C00190EA8A8D235BE098481E5BFE07B3A511D83280B1E5E654B2E53860CB4D982DDB315BB6D9D87203972DD70BD8721D664B3163CBAF1CD93250C096AD982D0AB0652463CB9654B2A509D8720D66CBD5982D57D9D8B299CB962B046CD984D9328AB1E57247B68C16B0E597982DCDC096718C2D97A5922D27035B7E81D9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;720966CBC536B65CC865CB0502B69C8FD9722863CB798E6C192F60CBB9982D01604B3963CB39A964CB0660CB59982D6762B69C6163CB695CB66C14B0E554CC96C98C2D1B786C719B763E0AD8720A66CB5A78CD01CEEF2A151C31D6DC12AF8BB4451B95C1F0AA4AACD1DD28BB83B2FB64D91D53DC72995B2E77CB93DD7285DB5F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E6F697BBFD93DDFE0A77ACCC1D2B77C726BB631519ADB132F22D27DFC9E45B91E18F96916F39F94E26DF0AE16B85B9A28FBA33714AB5551E679C7EFA91EC9B9C2C5F9CE37215937AAEACB6E743748769CBFA9E141E20731E684B07D69675585BDA6DDA12E76A4B4CA02D51AC2D47320A4EEE49E10132970105114C41185310B2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;51B0964BC149020A02980238EB8D1E20D3D2830E9069C6374C5C4D3DBC835E941EDE0132FE1EDE01328D3DBC03641AB8F5CA8E8FAD273A62D430F1D4A752C1B681784EC0E2391E8BE7389B78D670C5738C403CABB178E6320A56A552C16E070A56600AEA3005CB6D142CE352B05440C1124CC13CA6608BB14ED5F214EC68AE22&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2CE22AD842AE822DE02AD87C6EBDF31CD567AE23C6D14C3C35A954B027403C73B078AAB07866DBC47324573CB304E239028BA7965170782A15ECB740C10C4C4125A660BA8D82A95C0AA60828A8C0142C660A3619EB54394FC1CAB88A701857C12671156C2257C12670EB1DEFA83E873A62C0798EF41CC9713D8273248F671863&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;45186186718808631DC33858847101C33848847139C31823C28053BCE8219F7FF6980EF91CD5830EF91CD9830EF98423BC121CF259DA830EF9FCD003CB4400FE02CA770BEBCE505177201A4B8F01FD2BC1CFD7DE6882F714C3CD1043F2FE8D242F09C51A23D160A06142E92AF51DAA59E593CA264D2D2B2B9B5436A1547B9979&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5658698B47E5E084D2656D0DC140E3D14AE78AC85A253CAB61FA74796AE3D469E5332AA6286595330AE1202BED0CD18758F39FA0E66B22A402859E390ACD173EA2E3173E6E147D8A15FDD4E37806AE717EE9D35A05BECF80499F43AB7D0A9F356A7D8E89E70BB3788AB078FA62F1FC36B1780AB178BE8646BF01F02D88070E67&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;310EC4DAE58179A2BA50BE301A559ADB8272B4941EE1E0EBF2A038EBCBACD86E92EADD838AC11BF2A575AD72582BD68D8A65C2EBE38EAFA3E7A82FF285E5101CD9ECD7CE7FED1BAA6F8B29F092F1D276251A0DF8155FA356A6262F541F882D5764FFD270B033B731126A95B5D7E2E34A471C2EB2C26D21F89FE7D75EE2A7B970&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4C014CFFA95FCA800F9C1020E8A051EB302EC20AADA983B8B9EA1158EA9BF1807508178B1D3160E01DCCC59B1F853D0D01395CAD5190339E6EAAC8A30B5620DC5E00FF02B017C03E90C21F98BAEEE7AAABE9F57B435D5F63EA2A79E11D20001904E4E74B6FB0FA32BDBC1F13305E37362A7BD350F3B75869AF57F423A8FA8BB4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;46056F1B15BCC32AC8F68A5E5EA79B3D8CC2EF1A85773223CBF19A8C6C573732B2EFBA9191BD9FD8C8BEED4646D607B85400A0D00B6FA0301FF86537DF07167E66F897AF8DCBEF8CCB3DC6658F41452FA3A2C84CC517988ACF3115FB1253F119A6A21808180860105001117D63F5095C85AFC48B7C823B13F9041FF804DF709C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0FAF788103E0EBEA4868679457D3D54278BB8B6ADD18483A08C0C154EB0AFB64EA9C28342E8BF44B095E8AA24C39C4CC94B73153DEC24C8137A21230E54DCC94F1D0950900260271F0461315EDEB22D18E363A39CEB89C605C9631520F03020B0A271BA4543052CACCA4FC0193F22A26655A62527E8F499902544C05300D48A9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B4CB770696DF4CBB7C8FC0F9472494EF51D0CE6C5DBEB319D17320A91A408D2ADF7906671618978B0CA61CCD9832D7CC944731531EC14C599298290F63A62C84AE2C02703410B78CC9F77E917C8F333A291B977EE3B2C5B83CC9A0622DA362B1998A7B3115BFC154841353710FA6E26740C07200754045AB5DB4ABB0E8A276D1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AEC1F9F184A23D1EDA3941176D07136D3D249D08405645BBD1E0C4E9C6E5990653CE624C693033E55798295B3153CE4DCC942D98294DD09566002D40DCF94CB4578944BBC9E8E435C6E556E3F206E3729B41C57646C549662A36632AAEC054DC9C988A4D988A30101001D00A54DC62176D0C8BEE36BB68DB71FE1D0945DB09ED&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ACD7457B0F13ED06483A15C04600A7817CF30A1F34D8F1B071F9A871F984C1A42719934E3733E934CCA48D9849CF2466D2A998496743AFCE01702E10FB2C6BEC3C73631DB8B175B8B1171237D68E1BBB08DAB918C025D0D80EA65751915EBD6E70E46DE372A771F9BFC6E58706CB3E62545C6AA6A2155311C154FC353115614C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C52620E00A009B818A4FEC7A7535D69B4FED7AB505E77F9650AFAE8376AED7F5EA4BA6573742D23600DB5597B1CBE0C46EE3B2DB604A0F63CA4D66A61C8799722C66CADEC44C598399720B74E55600B70171FB12127527E0DDC5889260D716EDD93DE69E2DC53D5B827B065BB612F46C31EED9FDD0CE03001E849EC1962BAA74&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0B454A075B9D34F60D342E871897C38DCB52FD521AC9A878D84CC57C4CC53C4CC598C454CCC5543C0E043C01E049A00236E35894EE19AC54633D36A57B1EE7C36E15B17C76403B2FEA4A374923D2F73224FD0EC02BAAD24D313831CDB8AC3498329371BB42C06D692EC3982CC258C630CA4518C7318C32114613C3384C847132&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C39824C2D8C030268A30CE631813441897318CF1228C6D0CE35011C6ED0C639C08E30986315684F15B8671880803A25334CC7A70770A03C53B4109C774A330EBE86E14661DD56D0DB39676F3C2AC23BAF961D6E1DDF8A72C1805C3BA531828FE182828C1140CC1140CB65130904B41B18082019802880BD24071FF6E141BEED7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CD09141775F302BA7DBB7981E2C26E5EA0B8A09B1728EEC3AD37BFDB290C9CE78801413DFDE7C83FD76ED4DD614C70B9A954BD2E109C0F0B2E1B0B2ECB26380F57709902C16560C17DC32870A752F5F6530F8E29D8BF0751B06F8F95827FEDE151D0BB874F41CF1E44C1B74CF5BAF7206DDBB387A37ABBF7F054A46B0F4FF576&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EDE1A9DE777B78AAF72DB7DE6FF63829D6D74E18855DDA00959D0BBCCD2F906094A411B2C22C97AB408B24427C11E2B34670BB9B95CB635B4AF30B3261D8748C84D20DA1EB02E18AC9B5D50B6BB2B4802C1CC008632F7C3361E44D1C308549436EA83E5C238CF4E687EAD546E470F3C21AE8A994AD8FF09910E54C2EE83A02B8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;14A689F3A391B656386E3256D4488F586CEC3492FAB6AA87571A2979AD342EDAAED4059AC37961A5596637C56A7D354A63202407EB14C21A391E89F6C7CDB0C46273532C79104BB656D28765D475861A22C122391C0B549B927C6139AC5E15B31E124203F438383599F5D59C3C50A3D0DAE20013E52C355F4B55CBF621778B03&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C1A0A2DEA2187D3F132F6A02CD81786C8085383575204B5DA6F57A991C8F2BD1B09EBE44EBB6963E40ADD9925AACF5CB52094BB660F737D3AC7624C75838B0EBDD8076A24B7EA23FCBE4684CA98AA9FA34D092CA2492959525993F266F978D3E90E92BCA82139C00C0957700ECDB1AEF2D867F13BD03E1DF24EF20F837C13B58&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FD37444D2C817F6372BC43C9FF8C25F212EF307291CD04EC1D0E554E64B73EA8C907F5784742B983BCA3A0D847A7DF0316C47E73C4653F27474D271DCD841584A4D72B6888426CCAFD60A9A646699289AB608B173EB6F85353083FA1C192E97A8D1CAA5162C4CC400DF35AD14D3EACE4E81A5A108C849BA1279AAC0B632D9168&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1C250C6C56C24A9468BA25BD584BAF3597CF8F8397D46B0FC139E7357227D3243968AB28D68F245A6A89E92B4C7D9A02D1589CD4B0B469B5A2AC2D64E97003E7BCF66B6A0B061937B5D294264833D184133A15394A8F60D712FAC90D0D51A53D4050FCA42D389E3296EDD72E06A04C5A88A6E684F44B9D2AD4844E154E23164F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5846CBC049EFB4DAB95AC2403D21DC1C0CC45A587A2EF512CAEA48D41FEB1B6905ED91834CD231D3025E1111093567A3974541456E5DC3A8552B6CA23A372F2837C75475353E19F44E727BC91FFD6491FB0CFA4B2F49AEA7818D8E4D42E569F37428529F36738C8F1A04A0E3F3FEFDE4DE3B060C7131F99476924F8EEFA02C78&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;81E0E02C56CA774816AD21F36D32D227DDB8F94460682B131A4E924EED07A6E29DAD70EEB2EA7CE34466F41CEF82F8BA08F59294F372C710566ABC0D75909E652943F7308E4FAE33B04C1E03F798F916E1C08114323381311D7F3C92D771CD13FA4A34B1AE33DC08C90BFDD9446929797D42F50D90DC128D84C97400BCB917DE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D377990E8F94DC34126378543535434B35BD1228C112219D4BBFB927854F03E3C1C5FF690F9A4BFF11CFA55FB7CDA55FE3CEA5FF20984BBF8AE7D2058C82DFEF49E1D3C014A0E07798829731052FD928D8C1A5E0B7020A5EC014C0E22C7D1A781E3F003CC77B1A78963B6B7F86FB34F034F769E029EED3C093DC7A9F707C1A78&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DC11A39889E7B1542AD83C10CF23583C0F63F13C6413CF035CF1DC2F10CF7D583C031905F7A652C1960205F7600AEEC614DC65A3E00E2E05B70B28B80D53308829D8AD58A76EE129D8FF7015E166AE82FD9AAB603771156C3BB7DE6D8EEA73A3E3E3262CFEE3C7CDC2E15EFB73A404EBFA741F8A79BAEB83E9AE0F26BBC604D5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;07D35C1F4C727D25746C8409AE0F26B7BEE1664498C9FA601E9BEC1C56829D05308F4D6EACF68DA1ED8B4668E942C21C18A5E1238DD1FCBB792C38883B161CCC1D0BC63345BF2095A6DA414F4FC48A7E2E56F4736C8A7E1657D1CF1428FA1958D127300A4E4FA5A99E01146CC4149C8A29D860A3603D97824E01051D988289CC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;54D761EB6CE7996A1BD7A4E25C538D714D35CA35D593B9F5B63A9A6AC411037696401C92BE933B85C92A9C4A6DDB04B20A6259ADC5B23AC926AB16AEAC9A05B26AC2B29ACA285052A96D5B8182464C4103A640B65150CFA5E0E7020A4EC0144C63DA763C56B0E378DA762C572BD670B5ED18AEB6ADE66ADB2A6EBD2B1D756985&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E3C030C33A301CC11B188EFA690C0CB3533A30CCC203C31CCDD99B5FB6AAD6524DD121A9863B302C648A7E442A4DF50550F49958D1676045AFB429FA34AEA24F1528FA14ACE88B180515A934D557E93B8E9882324CC161360A2672299820A0603CA6E06866AA8762EB1CC733D5B15C933A846BAA07734DF520AEA98EE1D63BDA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D154473962FC8C8967642A15EC0310CF082C9EE1583CC36CE229E18A6788403C83B17896330A06A552C13E050A8A3105033005FD6D14147129E82BA0A0105350C714AC00EB541F9E82E57315218FAB60B95C05CBE12A988F5B6FB6A3FA64398E05ABAC63C11ADE5870FC4F632C3821A563C157BBD15850CF1D0B4EE48E053277&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2C68628AFEE5EE149A6A51361C62B41B29FAE7BB91A27FB6DBAAE89FEEE629FADF77F315FD93DD48D19B19057FDB9D42532D010A3EC614FC0553F0918D820FB814FCAF8082F731052DCC54FFBC1B59E77BBB39A6BA7337CFA4DEDDCD33D57776F34CF5EDDD3C537D8B5BEF9BBB9D4CF50D478C3013CF9F52A96013413CAF63F1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FC3F2C9ED76CE279952B9EDF0BC4F30A164F8451F0BB542AD834A0E0254CC18B98821D360A5EE052F0BC8082E73005AD4CC19EC53AF50C4FC19EE62AC2535C057B92AB604F7015EC716EBD8F39AACFA34E188531EB58D0CE1B0B3A7F1A63C1FA948E05DBF158B0813B169CCA1D0B3672834BA771834B6733F5DF964A03AE07F5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BF01ABFFF558FDAFB3A9FF56AEFA6F11A8FFB558FDCF61145C934A036E010AAEC2145C8929D86CA360139782CB0514FC1253702E33E0CBB0CD5ECA33E05F700DED12AE015FCC35E08BB8067C21B7DE0B1C0DF87C478C8B9878CE4BA582AD07F19C83C5733616CF5936F19CC115CFE902F19C86C57331A360632A15EC2CA06003&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A6E0144CC17A1B051D5C0AD6092868C7145CC214AC0DEB549CA76031AE2244B90A763257C15AB90A16E1D61B76549F9023C626269E602A156C3388E7242C9E00164F8B4D3C4D5CF12802F1F8B178AE601434A652C1AE030A644CC18998827A1B05277029385E40C1719882CD4CC18EC53AB586A760C77015613557C15671156C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2557C15670EBAD73549FE58E5390ABAD53902DBC29C8753F8D29C8F5299D821C8EA7203772A715DBB8D38AEDDCC7D15B98A2CF4CA5A9EE0045AFC48A3E1D2BFA349BA24FE12A7A8540D1276345BF9551509E4A537D0D28380C5330095330D146C1782E05870A28188729B88D99EA586C9D87F04CF560AE491DC435D5315C531D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CD35D551DC7A473A9A6AA923C69D3F0D23BC2BA546E8C346783F53C0EC549A503728A0172BA0072B60A64D01DD5C0594040AE8C20AF800A3607F570A4DC8ED83B7F4BA1005FFEA4214F4765929E8EEE251B0A78B4FC1EE2E44C183CC84BABA90D5ECEAE298D0775D3C55FFB68B6742DF74F14CE8EB2E9E097DC5ADF7CB2E2703&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F9A723C6E34C3C5F74A550C1FA83783EC3E2F90716CFA736F17CC215CFDF04E2F92B16CF138C828F53A960C380828F30051F620A3EB051F03E97823F0B28780F53F02453B09D58A7DEE529D83B5C45789BAB606F7115EC4DAE82BDC1ADF74F8EEAF347278CC267ACD3A9E779D3A91D3F0D4FFE624A3DF9E35DC893BFCC9D4EFD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8E3B9D7A853B9D823708E99B8B8F7509DE5CFC98613C2AC2E862188F8830F6338C870518052018E042369052D00FDFF5055E00F5DE85C47EBC756D61BFDCE95D04D78B23F4FA68729DB5A24D89C14D2DB9F1AD56FC61F57631B9CD5ED1D216A5774BA0D4BC6800AE97424E9D1C6F8B92BBBEC0EA7C68641954B6480EB7C9D14E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EFCF00679ED210A577CBC99D67B11C6D6CF1D6C165556B3410F4AE2097198B49852BC945E6A2B6B0E25DA55E053BBDABA1C1AAB6E6B658DC7B0CF4AC4E698D2BF03A87770D34B4B4311E819B63A1A125917635EB38B8AB511AE91D750505207FB74B3D5F5EFD4930D8734B99BA5DC4F6290C639B08631EC3B85184B19461DC20&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C2E86018D78B30CE6018D7893036318C5F8930B6328CAD228C1718C61611C6AB0CE35A11C6070CE31A11C6A70CE36A11062C6DA9E77B88304A18C695228C890C63B308631AC3B8428451CF303689305A18C6E5228CF50CE397228CB318C665228CCD0CE35211C6750CE317228C1D0CE31211C66B0CE362114637C3B848840113&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;408A71A108A33FC3B84084318C619C2FC0C8D5BD3D1B2DF2C09CD7F08E87364E1DAC985436A93CC94307E5CA8AA6A68A69950DD3A74EAB689CE68176A6714F9F5E188E2B51B9311E680FC43B97B6926B781F21561D09069546B8CC55DFD380971DB242F5317803E6D0F9B58EC5C6AB45C627874AAB1DB25C6908905E350495AA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6854EEAC0DC4E25A3525FC2C5A6C0020F8AD258A6DA914B9D048A90F102F1C2B4009B1C07AA508DDB7AB9CCF90D43F324A8FD0DE0C5949DF6754E0480AEDF5900591A05F89FED0F9D9D9F4BD5718137338100FEC9990E4541FFDB5147865CE473180677955B118197482F4D52D893C21C2F91EAEF15879E6D7D2375844A2A547&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;67C222D36A7BA194A8331CBEA69D8791FBFF01&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
			</xceedchart:chartservercontrol>
			<asp:Button id="btnUpdateGraph" style="Z-INDEX: 123; LEFT: 264px; POSITION: absolute; TOP: 1288px"
				runat="server" Text="Update Chart"></asp:Button>
		</form>
	</body>
</HTML>
