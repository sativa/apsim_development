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
							DataMember="InitialDate" DefaultGroupPrefix="InitialDate:" InvalidValueAction="DiscardChanges" NullText=""
							Caption="InitialDate" Width="248px">
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
							DataMember="GrowingSeasonDate" DefaultGroupPrefix="GrowingSeasonDate:" InvalidValueAction="DiscardChanges"
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
			<xceedchart:chartservercontrol id="cscSoilChart" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 864px"
				runat="server" Height="374px" Width="497px" Visible="False">
				<Xceed.Chart.Server.StringHolderCount Count="120"></Xceed.Chart.Server.StringHolderCount>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED3D097C14D5F9994D76C9E6202127108E20A8089606A2015B0F42C21D02124EB5C6497692ACEC11773740F002413C10F1AAD656EB7DF4B2B65A6F05694510E550BCB83CAA806D3DAA552088FEDFF766DECE3733EFED2CEDCADFFE3A9BDF7E9979EF7BC7F7BDEF78F3BD376FD3A4B4B4B46FC907FEC327C745C0CC39CD8AE21B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;56D3264762C36AC211E5C4F2594A24EA0F874EAB1C56316C7845C5B08A13CB6B3A02B18E88725A48E98845E4C089E5D33A9A02FEE6C94AE78CF03C25745A933CAAB2A5A5B26A54D3C893AB2A9BAB72D249D57371D50D3139E49323BE9454EF061206987BAE5ED68443B1483830468E2ADD085666B0912647BDC1C63AA55509F9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A224A94E6E5202D19C60E36C39A6448272641EDC8C919BE7B546C21D215F56B0B14189C5FCA1D668AF60E3C41041929B63FEF9FE58E7D476721D23044433D44FB9A81F8180D20C882E6E67D5CE18B14EE0714CBDA53DD6B181BF83B9D8718A8CC8FDB9C83AC580D3CBD249C604E85C15B706016F8C8D43716F060037000F0018&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1C6F26002F01596EC8B665651641EA1E6C0C5543830ACDF4041B6774B62BDD828DD17A39A8F49EAE34F949D79A024A75242277D6F9A3B1A12A4A193F8B162B06049FB94489259522E7EB298DFE98128CE6A184A87F915280EEE7ABE29E962EC11FA13FB37F436794941A36334458D6A044FC72C0BF88B26D4238E05322FF697E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;66A64BD3702FF02BCB00B30994B42171BB93924C189FFF122EC7997C7478ECCDA13CC53017F3D773443AEDF099C767B01FDEEE94B718E6613E773B126BE8B059C8E67CCA5A0C7BA4E91F7766927E84B0755C84904C5CEA387F2030B6A585703ECFE44789E71C645317AD245DA8430831DE0A601F63EFA8008D525C00A0104011&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;50E84DE80581FE745F7B537A7444385D393F967E7E2C4ABEBE744596D39BA3117724D0146E67EC92B2A5CC6C49923463BCF88C349776411233C01ADB8D0AF430A3568EC95E8A017295531D8D2AC1A6402788949426657A8A095209DF6F824BF5947C6793BC7CF0252026DE520272F3246A01A1CD9E04940AFC0A9D0640A7F273&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;59F1DE6AF1EEAC789950CE74B34969EBF39DCE32F3F35807FBAA1DCC671DEC4740BFC4F6869209FDCBEFA1D5929B9F534EFECDD006BD36222F2042A5F77438E9E9C9154976B5A2B2E5E496912DC387FB4EAE902B6537C87152EA349020F62033A88941B955A12920563DF0E49798CA702447D36176D334418E36B4C9BEF00272&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;138A17CC2537533B08CD0DB1CE00BD9B180AA1BB31CA7C2530DBEF8BB5E5B3BB5A62E1E450B35244DAC4F7AC255A419DBFB52D466E68DDF4265BCBA925CCCDD632E09A181B5A92CCEA43B42132770F4D9363243BE485AE129D0F47FAD21E4F8B84C90C35E657A2430DFDE6E5EA74F07275BACA45B98CAE4156042BDD7DAC483A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1F38993A5FCA0425813765828290576ACDA33DE9C9A18712DA9B93A1F1B917274BE57B499D3FC469A4D492ACB6D1CB9AAE35D1D39AA3B640FC481A7CD3D4A7313099861BF21DC87708060D005B5B6454CD61B4ABE58932A4C1C9E81CAB5FE03B4118A8A0258B939979E43D3D1A19999999FF032426CA83AEBBDFF9F6DB6F9311&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3998BE769B2F073A94C6C6345A94CEB27693E2DC364094DD143F6B5E28BC204413DDC488C4948C10A913E4D1DBAD5B399AB2DD4BE6C859D22E5223D46A4C75EF244949CBAFA0B33B481D76E3C92F2A6D27457768BD622129E96D53573DB4AB6F9952DD34F54D53AA97A6BE614AED46535FE7D6BBCD943A1052D368984CED93F4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1AEEE4ABBC1E6FE5D6BC85DBE3CDDC1E6FE2F6F8156EBD2FDBF678A31D464E1501B3F1988DAF4BD99CD05D7844D3759807127F3D05664D64F64B9E1166280BA1C129723BB1F4DD75FB0F32085388F111D9E75742312A59052861964C6A08C5084E5455337F40814972A6E67286B38B117D838DD5BEF9E07A7DAC70AD126D8EF8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DB61EA4DD0C6F86341B99D4C2C68ABC57A7787EA5DED6D4835F4BA97210B135066CC31D0D2979FA791652C69A4B0D090A712CB491B31D890968005C6C22A370A8CDD036AA88B9532E21FF5B2AF55B2C83FC6B62AF3F3591CC3C0C22AF3045B4343BCAC32C70B594D98A9C2E68CDCADFAB77C4105978E047C85668A4DB5A9DC2D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;17CE5274B6B35984C3E0EF98C1D4896F20263431A7E1B9CE2557B8E4E12E79844BAE74F92A5CBEE12EDF0897AFD215AD704587BBA2235CD14A9752E15286BB94112E85A4B7A5993FD9E64F158B171CD1177FE0FEB36AEF2820E4454288CD481B1D74157359EB48495B0110947D81944D422EF8A53DA752CFFA67EEB4652DCFBF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6565B99F17CEBA8C232C988DAC21C537D06A53C478E93952DB8B6CB6F02CB958C76E9E21172F68375EA0557A8A4BEA937C52A52748DAF35ABABB28B98897478D7A46674EAF230E373A231C0EC4FCEDBDC9654D4734160E12B3501D51E4EA582CE26F22CF8851787AAFE98844C311E01AF1B44D53DB15F2545CAF2C98ED0FF9C2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0B88B30ECD0CF92FE850EA3B824D4A04A25AC2B84DBC2260B8A4461B4F3500F7E3C2A9A45E9A3F78691550BE541C08CB51170221C4A6440917AA172AD16EB0F817A08B80F4397A4AD8A704085265ED2CBFB280F0A869963FEA6F0AC483C424AF853EA47AC9C50405CA4052ADD21E6B838908799E6F0FC89D53436A9CAB24D838&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2DA2F89416F2DCEAA3BDA0FAA23E97D2D963C6004EA4117A685C14EC6FC1AA5EE837E10CE2D30D0426A82CCE609D01E966A36F0CBDA9CCA163480C55E6B196667924BBD80AE024009301D4019802A01EEAF24C8501A425D2D2AE199396B69E7CEF1C4DE5FB612216C9B563140EB606E6EE9D20180982914DC66E7A78410D9999&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4290091EABE87521B99E108EF8178543313930458EB4FA4310399B051188669602B344E05C8101BB3AE06F0DE523649A600E8113F199A0C8107E0F368E0B87494E0E5BA486A86FB61A9423421552A8BC681FF29C9959262008BAE212F86453F744FEDDD0E5AAE4E3E9A22EC1BCC3F51FE4032B5CC2806B9C436C9D394BFB36B1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F1FF3D11A044ECB28A0DA4B81F1278301317AD1E8CAE48FD4EE03B0DCCB5968514EF0C003301CC02301BC01CA8B46F12717158D22D04036595DC168BE44681F7D9EAE30BC4D6A26609253DCAC8C8CEEE276E148A1EE1A24B5ADA04AADE9EB9E4B280DACFF2F9D1721A3A2F1F7CEC09DEB380E2B381E251C9CD66213292AFA7D4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C99DE18E58EE1C2DA58190ACE4CEC577F060C9F0890840F93E9C66E27555D1791A1D22F7BD64681322F327356CC2200D22754CE22EBC9BAD7FB61AAB5793678403849564E25B4CC68B5EC7149F96453CE37FC98A6646467C4DB35C407F9C50BE51D04976A51DAD757EF73D6675E6F557E07FEEE6DA1F9D0C7E316F235D7FC5F0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3C3657855A272731330033084E085227FAA8535322CE7E19FE18D3FD48326534864D5A16AC05BAEB929D6A397B0C845B669A295B31F461164F4962920AC82E25E40ACC373C7F07A2AA7E64483039CD1E22AEA521DC11A10B6F688F1A9DD4A67DFE59FBE9EC0BD6DA0B332F777D32B36249DB6D48C62122679471BD969A59C576&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1FB4409BE0EE8BACCF43D2CF88DD789C3DFDB5423766DACCA232D4BD173CAF4E1EDC5A02C463C49762B57BF539061EDC92F0F1DA08D22EFAB5B535F84A304F9919CF9A87B2DCB36D6676B009001E35C72E6C97D5F918CCC1C90C655E0391D74473738D821A25A02EC41619D354CA7235473FB5A525AA18984218351154843CA7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E9732558648CD26C521B9B2EE9897DE9FA6980DCF38A94C573AD253334D1A41FD1963B9D072E7539897C9218961312B01748343E010E14A8055E5D4DFF7FC4A3FEF62622F7F65CE27B4E0F7A08C8405F6F1040084018006CD4F15E0020024DCEB17FC4C856572B5860C0B02B02744ABD52456D8E39612ECC39C7FA5A95690AB1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3D21D80FD56252CA168352C28606FA5C6BD846459E33D55AF5A41EDA560D9D8F69BAB4491211A2ECEC818929B3599B35EEAF4A0687BF17C5D44DDDECBAE912996A07E34B662B85113D63BFF941216F0C400780F950DB5969B636AE50F5DAE3885E1BC616466E6A04C296D49D111168D10659BF9C0BE367160043C25C62529AA8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2D229E3720B7AB8B383DB0F9615102C353220E0390DEB54C577C10771A1F511478866B1943088784EA407B9B9C136F94A0758FDF50E4DCF82D14D13369C10C5D66D800149A22E9C017BAE92113DCAB94CA104312CFF62C3A4E1DE78234EB07C245A325E94A22350FB148EB15E4E2772CD2BAC85284866F0D5FE96C81375E86BD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7139F15A5223DB0D767182E714780D41DFF1765E7C2B9824B3D297C29A057722AF976B621BD19600B80CC05200CB005C0E6039802BA06AAFD4CCAABE9280DEE2E9AA5EBF8FD57F35801500AE01B012C0B5005601B88ED6EF8629D1114EAEFE9D89294CFDD540F1F5DA430F7DF0693173DB34B9CA53437A2DD3C3AAB682468C0D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;28F3E90DE8CB59E170904643C89C4D8930B47C2D8508FAAC70A02308BBD75AEA9496981A2C016D994E27A0F4162A9D116E576FC0928F09C768101DEE8BD4090CB91AE78F35C4E041BDB5132287C4F29DAFF204A6EFA458D3CCA832C90FEB30A45988DA10DF106D57E8CB0DD5A1D68002A1E6393442025773E9555C7A554D8D2F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1B0DE2C6244CDDA8327BE238F78C9DABD242BC55067D81B0ECA36BD3D24A6AF47BF87C36C61D22BA91540F040B4641B3B117748C1F3323DFF631F18DB73037A531A49BC466621E3613E76333714B3266621E6BE267E2265A71132DB8895F24D3449035719BB88966AD09CA8326DCC41D4934E10E1DD1ECD179A8153ED4DE491F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6431BC4BE37A35087738C9F9AFF60097C47ED60C9B4D6A745A752E6FFD50C29B85600E0C9D937E628B7A01433DC71635C250CFB64375C792984F66FF176C127236C01CDDFD19D259A95CAF7F006A9C83D7EB67E3F5FA5996F5FA19DCF5FA06C17AFD74B45E2FC1130948BD74669C02FDB361FD84D3ADF74950F030D438155350&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8F299862A16032978249020A26E21D07F3937DBEEBA1C576FE1B5E10381A1BD54DDBD29DBDD269D204BCA5763C6F7FED38EE3ED8B1DCFDB5B5DCFDB535DCFDB563B8F556DB3AB7D1B63E6D81E0D9998E3B789B0C086DBAD5ED2030B190885C64F7E494A028C0AE325326B1ADED6DFEE62894867CF87A9E87C6ABC1C990B961B5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FB74D24961A5C66934DB32EE3E0DD6AA12B4652D4647119EACE77002396B46C3573A95D4BA92599F1700AC03F0226C465E0FB35C2CE43363FE801F342735FB919798774C1A0310E4319B2EB7C757C97AC0EE207F14857BB2D514CA3614FD8B6F07F2A8F31143940EE607E108991F92EAE448648A1CEA9003EAA103F0D84714B9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B5558940B1280D13A9F76A9C88060ED5843AD0F368018D0C12CB36C3DF3CAF4E09B5D2FD462DD4C01993A6F843619454A459449AA25925086CEB2551A25E962516A3BE4F91CF57738DA9AC0C50416FE2852101CAC4130AB5725A1ADD7BA5A5B182342D131630FCB100C4FEE97FE0529E3606B0760F91DF22745FEB0F2A219093&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0294584F9EE023FEE6129454176E25939D585BD0DF5C888B9389D90C3F7DDE9F05823D2E1C09CA10536D99168EFA4100D461C9C3FBB9485601ED3B54C3F02CBB27C8CCB4468184590A38005222DED7A89AA4C9D6F48E8012C9657246264B0AF04F5DCC0D45E9924254A34EDDADA78A527748181A17DD12EDD624BFF928990A71&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9E96C0243957BB57C5395BBB03B6F7D0AE75C166B8AA74B3164D225EC41AC0725ECC5A31083B4B354A7C29C335897D3CDD24FBF1749302F4D2D2AD5AC072ACAAC072ACFAD0DB4CADAE14D6ACB86614E3FA74DD62A9061DE989ABC18A82330CDAC2C643551936D471BD29C263CF94A79739312E95A5E61C4D8DCACCE948977A5A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6AD314AA40CB405AC5E4C5A45AAC9706FD2AD549362859517C4C91A6156A8958DD5805669DC3FA4015AFC0A02054FB8A5192AE82983D580FE36F1CE2453EF46E849E923C66663937268CF4DAC58DEEC6B59C6E644B66E75632CB39C92EDE0DE27AF261C6C0CE7ADA335844F8FF5A634CBA9B49237EFF7B988424F4E34B13B31B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;96D563234A5CCFF8E71D996D0A601D2BC64216C6B29FD5D4AE666F2C15EADC4026080E720076641FCFADD16082FC4CCD065B70EBB14D322EB0FF3BDB65E3C113D544552591CF577D6AD012B0969A37637F070B769111AB077269B7D11C904D07D9D0DD0B8EFD72EC9763BF1CFBF55F6ABFE2DB774772B7FE9A6C9960EB6F1529&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9BC0C4F14B91D4356778374254E26500AF00D8A4E6B58F76195E0E4C3BC3BB19B2B700D80AE05500AF01D806E075006F007833BE9FE16DE8DA71E6D702ADFAC27F31C2BB03C04E00BB00EC06F00EA3B89C549BAC640A363EBF07E07DFABE8379E7B4498C68EC69A1D449E34FF1C5EA9E6B090357A7A5958F96FAC04B9C34F0B4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;86306BD318CAD90FA1FE3D00F602D8C7D85E4A1047B2E86309B9A8D24391DFFD80481ED2DA71DA526E421E4B708867390BA7C5B9251DFE86118B38F0F537C97140824D254BD86AFAFE6F102BBEFAC6CC8A6C098EBE03900BA0BB2462451E64E703E801A0004021802200C5004A00944A8C15BDC895B4E71BC48A32C8EB03A02F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;807E00FA6BD161E9DD6F5456D045D80190750C54B02B4E75CE1895150FAF9176F258712C94390EC0F100064B1A07DEC41C78E3A873605DD21C58C3E7C0B33C0E3C932407DC4BB96771D6A3DD5D4EDCD4899B3A7153276EEAC44D9DB8A9133775E20E4EDCC1893B387107276EEAD82FC77E39F6EB7F3C6E4A4DD0E35A04858653&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1EB344502643B0A10EC01400F5C208CA54C89E06E04C00D30134009801602680590066C72328732100723F8EA09C0D79E700F80980730134B208CA9D5A0485BE6A244356135470BB1E2C599D9636632D0DA7DD164FBC618C1A4521A440A0D4DB02A015409BA471E016CC819B8F3A079627CD81C57C0E5CCCE3C0454972408277&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D396B20D820B302BE62356A4D1F22BA0D435005602B856C88A55907D1D80EB01DC00E046003701F829809B01DC1267C5ADD097168D15EACB3790771B80DB01FC12C01D8C15E769ACA0BF53731764DD0D15FCC4C00A38BEE6E135D2393C56DC0765EE07F0008007258D03B33107661D750E8C4B9A03A3F91C388DC7815393E480&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FB72EE2B92F5F47D362796E8C4129D58A2134B7462894E2CD189253ACFE2CEB3B8F32CEE3C8B3BB144C77E39F6CBB15FFFEBB1447ABEE0482D7842ED51952578B211E20C2F037805C02661F06433646F01B015C0AB005E03B00DC0EB00DE00F0663C78F236C43E8EC3C1931D90B713C02E00BB01BCC38227E55AF084FEC2F17B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;90F53E54D0D7B01B6B06DD9E27F531ECC6826F5AB5F74328B307C05E00FB248D03A5980325479D039EA43990C6E7C0E1C31C0E7C7D38390EB897736575BA4CB4024467997A4048CB1C92E2EF88C2E55CF512CE54529F0F6A9598DCDC46AC4B4C0D78D4C06144E148A79A5B4C1E94AB3B6261636A0EFBA170306E4E78CA094FFD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F785A74AF09BEE7A98AA9F29D924CF3D39D954B84B4D194CD28B4DE9AAD8179A5261987A99D2748530D7A16A87B9A72655E963EE10D69BBEE65E1994C89C6BD4A8FEE6B226F5B2E49B74CD926F52BC01A67CAB169A31AC2A69C6B0EAE73122EEE9CA2A46896B6E5F5E3BF1362CB9069D2EE7558F159C8760D076B35CA8AA6F16&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D1B81DE8C3935D6614068832E35AD75F84A1998B81A27C643BCA85AD6886A4B709015915B3BC9B4C8C993A83BDE96F65A5C1F8F4B1C81CB24465A64C6C96CC159B6D14CF5E5083D59B6B48A8F5EACBC9D24D196F189CB0BB137677C2EEDFB3B03B3C9F647C17C1F7E431CD679F268C192573CCBD133673C2664ED8CC099B3961&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7FC77E39F6CBB15F8EFD72EC9763BF1CFBE5D82FC77E1DC1D131EAE7CED15E2F3D9C21CBA51E8D2B7D79189D2DF1AFC3E6F702725D70A204803C00F92ED15A5E0FC82E005008A0084031801200A5007A02E8E5626B7965E44AFAE0307A2FA02FE4F503D01F403980011A01D2EEC3E8A091819035082AD8115FB6D33FD2764EA2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F778283318C0090086300EBC8E39B0EDA873E02F4973E0393E079EE671E0A923E1C09F30071E3DEA1CB837690EFC92CF815FF038F0F3643970055789A78503EA7AEE72E17A2EFC28CF18A5D51FA2BF8E03BFC2432F1A624ABB61B196B7EE9B176C54B535AA9AC342560099486791D759E47516799D455E6791D759E47516799D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;455E6791D759E4751679E9FA6B9AF4FFBFCC9B4C4C2F3BF94088136874028D4EA0D109343A0B258EFD72EC9763BF1CFBE5D82FC77E39F6CBB15F8EFD3AF2855EBA4257ED9DE752577D3D01729179AA4F698D28CAE9DEA00B7E629618851BF0CADFF59695BF765824BB000090E88D0A57FE6290DD01603E8005001602E804B008&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C085002E8AAFFC5D020B774BF0CADF62C85B02E032004B012C632B7F17E295BFE590750554B090B7F2B780B7F27735945901E01A002B1907229803171C750EC84973E06C3E07E6F03830FB4838301D73E0CCA3CE81EAA439F0633E0746F1383032490EB8AF2619BD2C3A56A7B4C420E457003F651C6C04E3CE5B65E52FAF9A57&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DCE28B5250E350B5B652539A65594B4B57DBE9654AE52C6169396A0FFA98520DDDC9A774A2BE14E104D691029CA8F6A20427E95D3060AAEDF7C24986C6E3F1B538B4FAC07861E89C3AAB4AE594294977E1F4EBC8FA050DBB471CE6FD9093A183C2DF991A3DDABB19D4720B80ADA0D9C3486D50A31061880D827B0557BBC7C8CD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F31CED76B4DBE9D71169B7749CA66E525CDD7680A6ED04B00BF4F1183B847E3608EE6BF8876BF85BDB1C7FEC68ACD3AF23D4D8DE9ABE696E96E8DB5E50B57D003E02852CB14328B04170AFE46AECB84838E468ACA3B14EBF8E5063BB6BFA961ED7B72F40D5FE05E04B50C82C3B048F0D82FB5AEEF9E7F4725C201C8EE4A44265&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BF373AE248E1BF21852E4D8632E2F33470005EF8F500AF4480F4CDD78911DCABC4A7A401898E8CFDCFCB58972642EEB89DEA06D29309C00B32F6950D82FB3AF19B1B8E8C3932461AFE5C13214F5C84F2407AF201F40019FBC40621FF7A929A0FC1E31272979B27DD44AE1780609582045647FC728020544BFB4845A7B3C0F95E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;72739AF61B36D22D04CC517F905CFB16696D41E85CDA437057B2A87619B4DB07405FE8DDCF5863FD8C8DBD871B7B1737F68BC48DBD831B3B06DA1908601034761B6BEC586363DB71636FE3C6EE48DCD85BB8B113A09D210086426377C25B55D0D889E9A605712DFEDEAA847CB5724C9E1853821994FF30107769A3919B2F3D40&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FE8D825EBC4A9AB9371EEA571724A44124E36186B15584F102B98D01C61682B18162B0058D64BFF803F79F557B8703819B488D2F32AEBD426ED6B19B97C9CD0B8C2DB00E22BD445276A3C58A7B894BCD9236985207426A5696B49EA43FCF2A5BC72878314E81FED9B07EC2E9D6FB2428F81150F002A6E02F98823F5B28789E4B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C11A0105AB31052F12309FBEDE485277B0D467F10D8B113F63AACF435B79DA94EAA6A94F9952BD34F549536A379AFA04B7DEC779BD87BE4092FA4362B6181B09380B5A9F9C6E599E4D1B0D5F497A94D4F21023F51172F33BC6DA7A4B2175A90D7FA5979904FCC120C3B804EFDE4602A68104FC1E4BC04358027E679180DF7025&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E0D70209F8159680570808C3C58376FC746F4A7603865BF7FDDD828DD17A39A8E4C7338736FA895589E6A184A87F915280EEE72B11D859912E918AFA37744609FEB09921E25A1A14308CFE457447D0847080F8EFCCCCF5F477CE08CB3C671150A0BE95500E0BF5E5EAA675EFD96828A5CD8CE07B6D05680B43BDC716752B43BD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DB16F555028095DE46E8F379A8CFF44D8872FA2E8557C67D7ECD50A41917817724B4223E5C641BEBD02F4C1D9A64E9D0EB0CF5E726D4932CA86FB08EB492B6B2BC6DE9E867FEDE34E69D8F7BF33653C5796255BC09ABE28D58154349A8A27B4792FB75BAC17E9D6063A8BE23D804EFE6809F9B16F68762D1314A6C81A284D4D7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E41261A862554E66A0B5FE687B40EEE4D791AB1D25ADA2F72035C6DF95A909778462A5F1CE0D356096E17453198DFECC4C49CA94E8029EC4BE52DC5CBB7726B527A98C60917978D3CCA83235E26FF587BCC1469F7A45BAEE53DFFB8017AAF3B4B777A017B0352A5F3D373B9E0FB4D1FEEBEF0265B1147961969E0978D30919BE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;19615A97BCD09CE20FE5C49B26D9E8C61FCA036EC80BE3BCB0E36F799C60BA236AA8A593BD0408F2C25EC2A29C4A8D14D920F8436516049D5E719E3FD4CF9267E0461F7E973559E5944D852CB29775CC1FC9E3B16EC932C810DA1797FC364AF8783C44F0D1555C09D0E7B333DCD71303925C17AC6BE274374C7BBAD6A099B62C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D69E4D2ED5C45D49EFFBEB4D30398AE68BA38C91A38A512FE1ACFAB10BDBC3212514D3A5DA51C6FF4565142822F99C90580B4C5B4F3D1E49AC66EE5544AB92AF8FAF59EA67D068A6AA29D0B2DDC9ED990515CB05CE8543B1B6A8765488AE4E16B766564747B3FE37DD9CC8CB65E624EDBA8E4BAC347857B78D975BE31FB201FC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;530402452B893E2659355F19FF73DD7B27E98DE8E9AAFAD57404621D116562A8259C196C54B3E1441E5502D4FBF40CA95C7BF01B1F0837C59FF986A1C2036C1B05C6D3A7430871BA57105ED99731B269BDF670431FB3DDEF1DC9E678AFF521B8F774A5C91FF2C94D01457FDA5551CAF859B458B1519CB4122596D4FFE0411BFE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9278D8FE4FF333A9DC7917D007440C17A6AB3F8E4D57B4DED76299EA3EE538909693017C9C3D9EFE39870CC88789DF0C80212823FA4C1E70226C801A62046F9A12692673A65EA6BCB1219F96934DACC3C410614F4CF111E33C4D26CF44ADA04DE42644EE146A1908960F6E1A085B7B80604722A42CA4006A2141454913433E65&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6121B574F1347A80954A76067DA8B3BE064509D1DBA7FCE3CCAD299AA9032A2EDBE4E9A3DA7A39E1A16D23FC5DA26E83151834DABD8C3BC5E6F5845FA1F9E3DE93F49B1C8E720995EB62AA50185E921EDFE69BE6DE7B24EFC0386C16B2F952CA5A0C176336EF4BF6ED2187C542162FA16CC5F032C462295BD2628A4BC531C58B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;704CF1421C535C9E4C783F47D2C2FB9DA90CEF5F0991850538BC3F1F87F73B2CE1FD2837BC1F1184F72FC0E1FD5C498BEDB6DBC6A5BB4B5AFC1662E95E88A57B6FC431DC3C5655C0B6AA7C863ACF16B507433DDF16B5404251702F04CEBDB7E20E161AF2214AEEBD0DE717B1A69A6D43E2C50CB5C936245E2219C2DE77E2164B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8D7977E3BC5E4C7CEF118BEF39587CCFC6E27B7F32E25B469AD8C10F114B7DC8BF9DBCC8993497B403D1331A057B30155130A92FF9B78BBE1B47EA5DA5D59DFA3880D48FFCDB6D7E50FA3590301396C3B586FFF376FA937FF0DCE3FD2DD4DD40EA85878BF883823480E4BFC79FE93E8C67BAC748FC996E3D9EE9BE4066BAD2B1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;04F3433C9393A6101C98CDF1A666521DC958F6B571413A2E79C791BAF6F0A70A8F61037B3C817BF9BEEE098C3798C07D7C83FD14C69BCC24FE69B1C4D76289AFC112FF5C32125FC70C76752A0DF61A18E533B0C13E1D1BECD32C06FBC75C83FD2381C13E051BEC29CCF28CB2B587F55683BD115B98A9ACAA936CAB9AC6502B6D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;51CF64A8236C51A75B0DF656DCC106ABC1DE86F367B0A686DA1AEC990C7588ADC19E6534CA6FA5230339DB98B71DF7662E13DF1D62F11D84C5772016DFDDC988EFD966838D9A3F4768B00768069BDA9F771319ECB484060E35F61366B0FB7FB706FB5C9EC17E1F48E89B5283DDC80CF607507799C560CB6283BD0F1BEC2681C1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2EC106FB4530D88AC560172732D845890C768BD8607F820D6CABD8607F86F1DAC406FB738CB78249FC176289CFC1129F8D25FEAB6424FE1A66B0BDA934D8076094BB6183EDC106DB6D31D8E95C83ED12186C091BEC95CCF2A4D9DAC36BAD063B1D8596A555ACAAAF0FD955751D433D648B7A3D43EDB245BDC16AB0BDB883375A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0D760ECEBF8935F5C5213B83FD5386FAF9213B837DB3D128E7E3166F31E615E0BC5B99F8166608C5F71F8790F8FEFD1012DF928C24C4F7170966D8B7090DF64787D00CBB67462A0CDCEDCC60EF3DF49D1AEC5FF20C766F20E1C343A934D8773083DD07EAFEEB21B3C1BE4B6CB0CB3390C1BE5B60B0771F42067B0318ECFB2C06&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7BD7A104067BE7A10406FB7EB1C13E2E0319D807C4067B30C67B506CB08760BC8D4CE2878A257E1B96F8D7B0C40F4B46E25F66067BEBA1141AEC0A18E5CD8790C1DE740819EC570E990DF6C6433C83FDD221BEC1DE7008EF78649667BDAD3DDC6435D83FC6166633ABEA2FB6556D61A87FB645DDCA50D7DAA2BE6A35D8D5B883&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AF590D762DCEDFC69A7ADAD660BFCE509FB235D86F188DF204DCE29BC6BC4938EF6D26BE93C5E2FB2816DF47B0F8D62723BE3B1218EC9D4283FD076CB0A7A5C460EF6206FBF7DFADC1DECD33D8D38184DFA5D460BFC30CF60CA8FB371683FD9ED860CF5197BFD49FC3795F60B0EFC7067B2318EC0F2D06FBBE4406FBDE44067B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8FD8609F8B0DEC5EB1C13E0FE3ED131BEC268C07BF071537301E1FC9EA43DFD22BAF91634A6B38D259AEAE8AB39DCC0A5617F81929AA2E2D6275B915ABCBCFB0BAF89351975CD6C4F9E2267E8A9BB80937114CA609F86D23EA506E48A5430983145E871DCA2AEC50AEB538946BB80E6585C0A15C8D1D0AFC3013B58C57D9DAEB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7C97C5A12CC243DA835575B96D55050C75992D6A21435D6A8B5AE4B23894C5B883C52E8B43598AF34B585317D93A9452867AA1AD43E9E932388D2B708BBD8C7957E1BC3226BE578BC5B7038B6F0C8BEFCA64C4177E324BE450E097B4F80E25821DCAAA943814F8C52EEA50DABF5B87023F0A667128D70309A1943A14F8DD31EA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;506E84BA03168702BF40267028B7E02700F891329E4369C50EE5157028F0FB644687D292C8A128891C0AFCCC99C0A1DC8E1D00FC129AC0A1DC81F1E0C7D2040EE52E8C378F49FCDD6289FF0996F873B0C4DF978CC407D9C0DC0F0333D73230EDAC0BBF12776116EEC24CDC85DF26D3850B98CF6848A5CF7808E83913FB8C69D8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;674CB5F88C295C9F5127F01993B1CF8830E337C9D62447AD3EE3496CE462ACAA71B6557530D4B1B6A8F3196AAD2DEA02ABCF588D3BB8D0EA33D6E2FC4ED6D469B63E6311433DD5D6675C68F40BEB708B1719F3D6E3BC4B98F86E108B6F1516DF93B1F8BE9C8CF82E4EE03396087D4625F6199B52E2332E633E63F877EB3396F2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7CC61620E18729F519CB98697A15EAFE81C5342D17FB8C37B0CFB842E03306639FB1197CC6D5169F717C229F715C229FB142EC3376621B7F8DD867ECC6782BC53EE35D8CB79959D3FE87927D11DB22E0566BFA3E0C415F6C4DFB606B5A66B1A6BDB8D6B4A7C09A96626BBA859985125BB3B05530B4C57868DF82A1DDC1D85294&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4AB67C0C6C29C06CE981D9926F614B772E5B72056CC9C16CD9C9D8926DCB965D02B66461B66C07B6EC656CF1A6922D07802DDD305B3C982D6E0B5BD2B96C7109D82261B6EC636C49B365CB4702B67CDB85D8B213D8F20563CB375D29644BBA1B5683BA105B0E7521B6747599D972A08BC796FD5D7CB67CD585D8F22FC6962FBB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ECD8F2A5802DFFC26CD90D6CF996B1E58B54B2A53BB0E59F982D9F61B67C6A61CBC75CB6FC43C096BF63B68057A06CF99B2D5BE0EC251E5B3EC26C7917D80267E950B6EC4B255B7A015BF660B67C88D9F281852DEF73D9F29E802DEF62B66432B6BC63CB16AF802DBB315BDE07B6E431B6EC4A255B06015B7660B66CC76C79DB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C29637B96C7943C096D7315BF2195BB6F1D8E232EC7C14B0E535CC960FE0350738FDA65C70404F6B5BAC21DC1169567AC1AB2AD16657B3EC0AC8AE0B64575471C9152E79B84B1EE1922B5DBE0A976FB8CB37C2E5AB74452B5CD1E1AEE80857B432BD3D5A41BEC3C97704F956A6FB2215E43B9C7C47906FA5F0B5C26CD1475DF6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3FA9C63C1E4B162F3E9D7D931BCBF563D2D24A483D37D758F3610AC1A4E5D5AE141E20F34390962D585A366369D964919697B9D2B251202D2F6169E9C328D8D095C203644601052F620AD6610A5EB050F0672E056B05143C8F29809392E801326BBAD09931ABBB3807C83CD7C53BE8E5D92EDE0132CF74F10E9079BA8B7780CC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;53DC7A9FECB27B6C7DC216E318363C8FA752C026C1F0FC090FCFA378781EB10CCF1FB8C3F3B060787E8F876720A3E0A1540A580350F05B4CC16F3005BFB650F0209782070414DC8F2918C404EC3E2C53F7F204EC1EAE20DCCD15B0BBB802762757C0EEE0D6FB4B5BF1B9DD16E304363CB7A552C014189E9FE3E1B9150FCFCF2C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C3733377787E2A189E9BF0F00C6114DC984A010B0105D7630AAEC314ACB250B0924BC135020A56600A863201BB1ACBD4553C01BB922B085770056C3957C02EE70AD8326EBD4B6DC5E7325B0C380D8D9EC2B6A44B700ADB8F18C66211069C9E458FA65BE2361C4D7749173A9AEEE22E74341D1C9D95E068BA8BBAD0D17497BBE1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;651C0057C0A04F63DD5928EA0E4441E9E1755711FC5CED4D22783F30D40A410DCFD524B93E186D0E4702FEA613E1B7D9E1DDA5D3860FAB1876724545C5B08A13CBB597884F0B291DB1881C38B17C5A4753C0DF3C59E99C119EA7844E6B1A39523EB9F9E4AAE1A7549EA4548C3A251F0E90D24EBE6B64CDAF40CDD7864905746D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D60BCDE7CB71FC7C9F5EB495155DE9B63DB9513F75AF4DABC0BB0A98741DB4DA3DFF7CBDD6796C78AE370E4F080F4F100F4F28F1F004F0F0FC141ABD19C02D303C70288A7E10D5AD6E989FA90BD4132311A5B5232047CAE9D109DE9FBB517C33C28AFD82A47A6E43C5E0CDF4F2867639A415BB1D15CB80D7B66D5F03CF525FA0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0BC9413868D4A79D5AD823D8D81155E0E5DEA9F39548C4EF53BCCD5A99DA9C60A33F3A5D917D534381CEECE670B05DD65E478F290B6370D12DD41184FF393EEDE5799A0BC703C0B49BDA8374F8C09BF9820EEAB5F6E522CCD09A3A969BAB1E3DA5BE910E58C773B1D8ABFD3ADE715CBCF111D84BE09743351A055943E973199C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D1EAFD250CEE1D00EE04701780BB6114163071BD872BAE86D7DE75715DC8C4F53EA8E87E000F80CCE64A17B3FA1E74F38EC0D65FF3D52BBB4417F34B59E95FBB453FDD177F8155AF60B15EC11256C16FDDA297C6E9260BBDF0657AE1A54CC91E322AD99958C9A661255B9E58C9A66225FB2370E911008F02E3AF643670B2C006&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E6DFA8DB975BF5CBDBF4CB3BF5CBBB752AEE61543C66A46222A66202A6E2FEC4548CC7543C05043C0DE019A0E2416C2ABC602ABCABB14DF835B6095EB009DEB5381F5EAD0203C097D51700AC63B29AFF3093BAF590B401C04B54EAF21FD339F1847EF994CE94A71953361A997212664A2566CA738999320233653374650B80AD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;40DC1A36B4C34443BB51EFE456FD729B7EF91623F53520302F7FBB4ECA0E46CA362329433129433029BB1393720226E52DA0E26D00DB819477ADE3BB0B8FDFFBD6F17D17E77F90707CFF0AE083F8F8EE6344EF81A4BD00F6A9E3FB89CE99CFF4CBCF75A67CC198F2919129259829C598295F25664A1166CAC7D0954F007C0AC4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1D60E39B2F1A5FD8F0AF75D2AB5FE6E897F9FA6541FC52822DEC948A7F1AA9E88EA9C8C554C0FEF50454E4602ABE0402BE02B01FA8802DE9A6A1EDC243D73BC332B487713EECD9160F2DFCF0A157F2B0A12DD788F4A643520600B7870EED713A2706EB974374A60C654CF1780C4C39781031E5C041C494618999B2FF20624A16&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;74251B400E01126C51A643FBC541C1D0FE58EF64B57E59AB5F4ED02F27E9544C6654743752F14F4CC567988AFAC4547C8AA92800020A01140115D3AC435BEA414337DD3AB4BD71FE8C8443DB17DAE9171FDA396C68CB2169008063000C84F1CDC93F5767C779FA65937EA9E84C6A614C1A6464D20ECCA4ED9849FEC44C7A1B33&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6930F4EA04004380D8F35963438D8D6DC38DBD861B0B266EEC55DCD80FA19D0A00C3A1B13093AB4D22B95AA47364B17EB954BFBC42BFBC4A67D9D58C8A4A23152F632A36622A5626A6E2254CC5482060148053808A5556B93A15CBCDF556B93A03E7DF9850AEC6403B3571B9BA85C9D558481A0760BC6A326ED73971877E7997&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CE94BB1953261899F23866CA639829F72566CA9F3053EAA02B5300D40371F72724EA4CC09BCE88927EC57A36C3D8B3DFE09EFD1AF7ECB7897BF62BDCB339D0CE5C006741CF1E6242779F48E89ED4D9B75ABF5CAB5FAED32FD7EBFCDDC0A838C748C53D988ABB31152F27A6E22E4CC57940800CA009A8D864153A050BD516ABD0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B5E1FC57138ECF3C68271017BA3798D08520290CA05D15BA9D3A2776EB97EFEA4C799F717B9580DBD2C70CE35A11C60186B15284016BC514E31A11467786B14284D18B615C2DC218C430AE1261FC90615C29C218C530AE10614C6218CB45180D0CE3721186C230968930420C63A90803A25334BC79D9C11406682F05215C7C10&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;85372F3D88C29B971C3487372F3AC80B6F5E78901FDE5C7410853797330A3A0FA630407B2550B00053301F53D061A120CAA52022A0E0024C01C4056980B6FD208AC9860F7202B4A183BC406AF0202F401B38C80BD0CE3BC80BD09ECFADD77FD02EFCDA668B0141BDF88FE85EA7DDA82FC8B2816B4DA5E8FD1C064EC103E7C303&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D76C1938993B70E70906AE110FDCCD8C827353297AF70005E7600ACEC6149C65A1600E9782D9020A66610A6E61A237134BDB0C9EE8357045643A57F4CEE48ADE34AEE84DE5D65B6F2B5853EC30F221420B0E2A331B789B9B278197A411B247098BF3B44822C417213EAB07B76F67E572D856CEDCBC0C709BB69150F5F0477FA8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;72445DCDC4DA6E5A40160E3E04DF0BDF0CF0BC8903A63069C80E36866A8591DEDC60A3DA881C6A9D580B3D9532E31E3E03A29CC9055DFB03974234717C24DCD10EC73C460B9AE9D186CD9D7A528F76F5D0483D25A79DC645E72B0DFED6504E486995D94D895A5FADD2EC0FCA810685B0468E852345B8199658626C8A25F764C9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E64ABAB38C86CE605338502087A2FE1A4392372487D4AB12D64342A89F1EC3A626B3BE1A934B350ACD2D161B2867A9B95AAA5AB63BB99BE20F0414F516C5E80B0DBCA8F5B7FA63D16213716A6A294B9DA6F57A9A1C8B2991503CBD5EEBB6965EACD66C4A2DD1FA65AA84259BB08B8C34AB1DC9D2170EAC72573C9FC8928FC8CF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;34391255AAA3AA3C959A52D98874EBD64D327E0CD62E137D20D3FB18CC381F0700579E27C10C0EF53C05FF7EE0791AFE0DF33C03FF4EF43CABFE7B4E4D5C0DFF066579D690FFE9F572BDE7797291C906D8B316AAFC01BBF5424D5EA8C7F302943BD6B30E8ABDBFF861D020F65B1F69D6F369D474D2D10C5841487ABD824EA8C5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AA5C084B35B54A8B4C4C055BBCF0B2C59FDA7CF8E90A964CD76BE460AD12256A066298D38E6E726125272EA1798170A8157AA28D757EB42D1C89A184D25625A44488A49BD24BB4F43A63F9DC1858C978ED41385FBC56EE649224072C15450B49A2A996687C85A97B8B3F128D911AA6B6CC569479F92C1D6EE07CD5C2968E4080&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;71532B4D698234034D38A1539123F4E8732DA1506E6A8A28F3FD04C547DA826321A3993EEDA21865D24234352B18BF8C53859A885385D388C61396D13270C23AAD76AC96501A4F08B506FCD136969E4DAD84323B1CF1457B84DB417AE4001BE9A86101AF800C095567BD970501456E9FCBA8552B6CA132372E20B7465571D53F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E9F44E7279C81FFD7423F7E9F41756925C4F031D1D9C84C8D3E6A92B529F36B3F48F1A04A0FEF9DB6FC9BD673D8D25904F7927F96479377860E3FE4B1E56CABBD1436BC8388978FAA41B379EC40B6D6540C349D2A9FDB053ACB31DCE3B568D6F8C8C193D3F3B2FB6204CAD24E5BCBCB0372B35D482DA339E652A43F70E0E4DAE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;33B04C1E05F3985149387024858C4C604CC71FF77DF66B9ED05722890D9DA166489EE8CB24424BC9EB1E6C6C82E4B6483844A60360CD3D12D8C774E32921A40D68C7701EB2F480966A789148FA239B4B8F48E5D3C0661AD8C673E91FE2B9F430CB5CFA44EE5C7AA8602E3D04CFA51F61149C90CAA781B78082E33105C7610A8E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B55030904BC131020A06600A1E654F03E5F801A03FEF69A01F77D6DE97FB34D087FB3450C67D1AE8CDADB797EDD3404F5B8CA7D8F094A652C0FE06C3538C87A7080F4FA165787A7087275F303C7978789E6614744FA5807D0114E4600AB2310559160A32B914741350E0C1143CC304CC8D652A832760E95C417071054CE20A58&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1A57C0BE3DC0ABF79B0376E273D80E237FB5E971337F2DE7395282757DBA0FC538DDF5C274D70B935D7D82EA8569AE1726B9DED5D437C204D70B935BEF5A2322CC64BD308F4D760E2BC1CE0298C726E7ABBDEB69FB220F2DED21CC012F0D1F69BD66DF8DBE6003D717BCC4F5059B99A07F782085AADAA71B1CB4750009FAFB07&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;90A0BF77C02CE8EF1CE009FAEE037C41DF7500BFAFC728D8792085AA7A2C50B01D53F036A6E02D0B056F7029785D40C1364CC156A6AAAF1D40DAF9EA018EAA6EE5AAD496033C55DD7C80A7AA9B0EF054F5156EBD2FDBAAEA465B8CD7B438247D17F62D36562FA552DA46C258ADC763F5221EAB7596B1FA0B77ACFE2C18ABB578&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ACDE66143C9F4A691B0D14ACC6143C872978D642C1D35C0A9E1250F024A6603B93B627B0803DCE93B6C7B852F127AEB43DCA95B647B8D2F6476EBD7FB095A5876D1DC32EB3637897E718FEFAFD700C1FA4D431DC8A1DC31ECDD81B5F72DAABA51AA243D23EAE63F89809FACF52A9AA7E10F49BB1A0FF140BFA4D1641BF812BE8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D70B04FD3A2CE89F300A56A552552340C14A4CC135988215160AAEE25270A580822B30059F32555D8EB5F3729EAA2EE3AAD452AEAA5EC655D5255C555DCCADF7525B55BDC416E34B363C17A752C096C1F05C888767111E9E4ECBF02CE00ECF7CC1F074E0E1F98A51104BA580AD040A2298820B3005ED160A425C0A82020A0298&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;82FD4CC0E661993A9F27607EAE20B47105AC952B602D5C0153B8F5FA6CC5A7D9D61774997DC1619E2F801D84DF035F007B1853E70BEAB02F80BD90565F90E1E1F902B787E70B60F72215F4C9A954D5C740D02762419F80057DBC45D0C77205BD5620E83558D0B319056352A9AAAB8182D19882333005A75B2838954BC18F0514&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FC085300FB46A9AA9E82B573144F55477255AA8AABAA277355F524AEAA5672EB1D61ABAAC36D310AD8F054A452C0B6C2F00CC3C3F3033C3C275A86670877784E100CCF603C3C858C82E3532960DB8182633105833005032D140CE052502EA0A03FA6A08809583F2C537D7902D6872B08655C01EBCD15B05E5C01EBC9ADB7D456&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7C4A6C7D01EC4B36F882DE1E8E2FE8FBFDF005FD52EA0BBEDD8F7C4139D7170CE0FA82633CBCE0D2400F2FB8349889FF37FB53A8C0E9997062CD7E7C62CD7E7C62CD7ECB8935FBB927D6EC179C58B31F89FF098C822FF7A750817380822F30059F630AFE69A1E0532E059F0828F81853308429F03FF6239DFDFB7E8E02FF6D3F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4FD13EDACF53E07DFB790ABC773F4F81F770EBFD70BF9D027F608BF143363C7F4DA580F583E1790F0FCFBB7878DEB10CCF2EEEF0EC140CCF0E3C3C158C82EDA914B0E38182B730056F620ADEB050B08D4BC16B020A5EC5140C6702B615CBD4169E806DE60AC226AE80BDC215B097B902B6915BEF4BB6E2B3C11663241B9EF5A9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;14B0536078D6E1E179010FCF5F2CC3B3963B3CCF0B86670D1E9E518C82D5A914B03140C1B398826730054F5B2878924BC113020A1EC7149CC204EC312C537FE209D8A35C4178842B607FE40AD81FB802F630B7DEDFDB8ACF437618F9A79AA72067F0A62063BE1F53909A944E416EC15390B1DC69C538EEB4623CF771B48E09FA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CDA954D57920E8376141BF110BFA0D1641BF8E2BE8AB04827E2D16F4298C8295A954D51850B002537035A6E02A0B05577029582EA0E0724C413D53D565583B97F254F532AE4A2DE1AAEA62AEAA5ECA55D54BB8F55E6CABAA17D9629CF9FD50C2E9295542052BE11C2680BE54AAD0ED20804D5800652C80E75904F05CAE00FE44&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2080E760019CCB28383B952A743F5030175330075330DB42C14C2E0533041434600ACE622A341D6BCD993C159AC615F5A95C15AAE7AAD014AE0AD571EB9D6CAB20936C31CE63C333319502F6040CCF783C3CE3F0F08CB50C4F0D7778C60886A71A0F8FCC28189D4A017B1E28381D53701AA6E0540B053FE252708A8082519882&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;26266023B14C55F104EC64AE209CC415B04AAE808DE00AD8706EBD15F68F7BB6D329C53C9D6AE34DA7E67D3F2C7920A596BC27B6E421EE742ACC9D4EB573A753F006217D73B174BFE0CDC52B19468908E3E70CA35884710FC3281260E43DA6BD4E0BA7C1A6E53D8EEF7A002FBA81FC7C4CF4C7D3D011F2C99D9E4FE07A4A985E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7F4AAEBBCDE850A270F319B9F1CE567C21F5F69FE43673465B4784DE7D0EA5C645FC70FD05E434C8B18E08B9EB01ACCE8546FE05954D92431D72A4D3F325E08C539A22F4EE2B72E79E22479ADB3CFBE1B2BA3DE20F780E90CBF429A4C283E42263524748F174A957814ECF2168B0BAA3B5231AF37C0D3D6B50DA630ABCCEE139&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0C0D4D6D8E85E1E61B68A83E3C5FCDFA16EE6A95667A474D411E8CBF8BBE82A4C996B49931F5DBAF046C7F8B617C23C2F81BC3382CC2F882617C2DC280FD5E14E39008E35886D125C218C9300E8A3046338C03220C3FC3D82FC288308CAF4418CB18C697228C950CE35F228CC718C617228CD50CE37311C65686F14F11C67686&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F199080302A814E35311460EC3F84484D18F617C2CC2389E61FC4384710AC3F8BB08630CC3F89B08631EC3F84884116318FB4418B7338CBD228CFB19C61E11C6130CE34311C6F30CE303014676DCDA336F9103EA3C97772CB37EEA60E5B08A61C3933C74501E55D9D2525935AA69E4C95595CD556E68A78A7BEAF3C4504C89C8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CD31FF7C7FAC736A3BB986F711A235E140406986CB6CF53D0D78D9A15BB0310A6FC00C195F675B6CA85A646872A8B4DADED395263FE9555340A98E44E4CE3A7F34A65553C6CFA2C58A01C1672E516249A5C8F97A4AA39F58E1681E4A88FA172905E87EBECAF97449FD239EB3BFF666C84CFA3EA302475268AF874C08077C4AE4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BBCECFCCA4EFBD824FCCE240ECD83320C9AE3EFA2B25F0CA9C976200CF72AAA351E27402F4D52D294DCAF4C07AD1502C3CE3EBE81B2CA2A1A54767C25C65B6B5504AC499FE6AB37A1E46F6FF01&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
			</xceedchart:chartservercontrol>
			<asp:Button id="btnUpdateGraph" style="Z-INDEX: 123; LEFT: 240px; POSITION: absolute; TOP: 1248px"
				runat="server" Text="Update"></asp:Button>
		</form>
	</body>
</HTML>
