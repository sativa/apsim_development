<%@ Register TagPrefix="xceedchart" Namespace="Xceed.Chart.Server" Assembly="Xceed.Chart.Server, Version=3.0.100.0, Culture=neutral, PublicKeyToken=ba83ff368b7563c6" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfEditSetupPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditSetupPaddock" %>
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
						tabIndex="9" runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 104; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						BackColor="Transparent" Height="16px" Width="40px" Font-Size="Smaller" Text="Save" Font-Underline="True"
						BorderStyle="None" ForeColor="Blue" BorderColor="Transparent" Font-Names="Times New Roman"></asp:Button>
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
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED3D097C14D5F9994D76C9E6202127108E28A8089606A2015B0F02913304249C6A8D93EC2459D923EE6E8078A2201E8878556BABF53E7A595BADB782B4228872285E5C1E55C4B61ED52A1044FFEF7B336FE69B99F7769676F56F7F9DCD6FBFCCBCF7BDE3FBDE77BCF9DE9BB719524646C637E403FFE193E72160D6DC1645090C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1FD72EC712C3C74563CA3195B395583C188D9C583DBC6AF888AAAAE155C7548EEB0C253A63CA8911A533119343C7544EEF6C0E055BA6285D33A3F395C889CDF2E8EAD6D6EA9AD1CDA38EABA96EA9C9CB2455CFC3553726E448408E05D252BD174838CCDA73F5725C349288454363E5B8D2836065879B6872DC1F6EAA57DA9448&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;204E92EAE5662514CF0B37CD91134A2C2CC7E6C3CD58B9657E5B2CDA1909E4849B1A95442218698BF709374D8A1024B925115C104C744DEB20D70942403C4BFD548AFA110A292D80E8E17656ED8C19EB681EC7D45BDA63031BF83B848BAD5364461EC8453628069C3EB64E322640E76AB8350878636E1C8AFBB3007801F800C0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E0F8B301F809C8F142B6232B730852CF7053A4161A5468A62FDC34B3AB43E9116E8A37C861A5EF0CA53948BAD61C526A6331B9AB3E184F0C53512AF859B458292004AC25CA6CA914B9D048690A269470BC0025C483E72A45E87E812AEE199912FC11FAB3073676C549A9E1B32284658D4A2C288782E752B64D8C86024AEC3FCD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CFCEF6681AEE077EE598602E819236245E6F4A9209E3F35FC2659DC9DF0D8FFD7994A718E663FEFA0E49A75D3EF3F80CF6C3DF93F216C302CCE71E87620D5D360BD95C48598B61AF0CE3E3CD4ED18F10B68E8F1192894B1D1F0C854E696D259C2FB0F851E239073BD4452BC914EA1042D45B01ECC39D1D15A0518A8B00140328&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;010AFD49BD20D09F19E868CE8C8F8C662A672732CF4EC4C93790A9C872664B3CE68D859AA31D8C5D52AE949D2B4992668C179F9CE1D12E4862165863A751811E66D5C909D94F3140AEF26AE37125DC1CEA02919232A46C5F29412AE3FB4D70A9BEB26F6D925708BE04C4C45F4E407E81442D20B4D99B8072815FA1D300E85461&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3E2BDE572DDE9315AF10CA996136296DFDBED559666101EB607FB58385AC8303081890DCDE5032A17F85BDB45AF20BF32AC9BF99DAA0D7C5E48544A88C9E8E203D3DAE2AC5AE5655B71ED73AAA75C488C0715572B5EC05394E499D0611C45E640635292CB7293405C4AA179EFC1253198DE5693ACC6E9A27CAF1C67639105D48&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6E227AC17C7233AD93D0DC98E80AD1BB499108BA1BAB2C5042738281447B21BBAB23164E8EB42825A44D7CCF5AA215D407DBDA13E486D64D6F72B59C3AC2DC5C2D03AE89B1A125C9AC3E421B2273F7C8743941B2237EE82AD1F968AC3FEDF1F45894CC501341253ECCD46F5EAE41072FD7A0AB5294CBE81A6C47B0D3DDCF8E64&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F0819369F0A5425012785321280879E5F63CDA93DE1C7A28A17D39191A9FFB70B254BE97D507239C46CA6DC96A1B7DECE95A13BDED396A0BC48F64C037437D1A039369BA21DF417C8760D200B0B52566D51C4EBB5A992C431A928ACEB1FA05BE1384810A5AAA38D9D987DED3EF22233B3BFB7F80C46479D075EF5BDF7CF34D2A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2207D3D71E0BE450A7D2D494418BD259D62E529CDB0688B297E2E7CC8F44174668A29718918492152175823CFA7BF4A84453B6BBC91C3947DA496A845ACDA9DE1D242965F91574763BA9C3693CF945A56DA4E876AD572C2425BD69E9AA8F76F50D4BAA97A6BE6E49F5D3D4D72CA93D68EAABDC7AB75A5207416A060D93A97D92&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5EC19D7C99D7E32DDC9A37737BBC89DBE38DDC1EBFC4ADF745C71E6F70C2C8AB21600E1EB309F5699B137A8B0F69BA0EF340E2AFA7C2AC89CC7EC933C24C6511343855EE2096BEA761FF4106610A31212607824A244125AB0825CC96490D9104C189AB6A160C293049CED65CCE087631B27FB8A936B0005C6F8015AE53E22DB1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;60074CBD09DAD860222C779089056DB5D4E8EE30A3AB7D4DA9A65EF7316561022ACC39265AFAF3F334B2CC25CD14169BF254623969238798D292B0C05C58E54691B97B400D75B15296FE512FFBDB258BFC636CABB13E9FE9182616D65827D81A1AE2658D355EC86AC24C153667E66ECDBFE50BAAB87424E12B34536AA94DE56E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A5709662B09DCD225C067FCB0CA64E7C3D31A1C9390DCF751EB9CA238FF0C8233D72B52750E5098CF004467A02D59E7895273EC2131FE989577B942A8F32C2A38CF42824BD3DC3FAC9B57E6A58BCE090BEF803F79FD6FA470321CF13421C46DAECA06B98CB5A4B4A3A0A80A0EC73A46C0A72C12FED3B817AD63F73A72D6B78FE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2D27C7FBAC70D6651E61C16C643529BE9E569B26C64BCF90DA9E67B385A7C9C55A76F314B9784EBBF103ADD2135C521FE7932A3D46D29ED5D2BD25A945BC7C6AD4333E6B463D71B8F199D1682811ECE84B2EC775C613D130310BB53145AE4D2462C166F28C1887A7F7719DB17834065C239EB6795A87429E8A1B948573829140&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;742171D6915991E0399D4A4367B8598941544B18B7D12B02864B6AB4F10413F03E2A9C4A1AA5F983975105E5CBC581B03C752110426C4A9C70A1769112EF018B7F21BA08489FA3A746034A882055D7CD0E2A0B098F9A6707E3C1E6901E242679ADF421D54F2E262A500692EA948E443B4C44C8F37C4748EE9A1651E35C65E1A6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E93125A0B492E7D600ED05D517F5B994CE1EB30EE3441AA187E645C18136ACDA45410BCE603EDD406092CA74061B0CC8B41A7D73E84D650E1D4362A8B28FB035CB23D9C3560027039802A01EC054000D50976F1A0C202D919171D5D88C8C75E47BFB182ADF0F12B148AD1DB370B035306FDF24C148108C5C327633A20BC79199&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;290499E0B18A5E1793EB89D158F0DC68242187A6CAB1B660042267B32102D1C2526096089C2B3261D786826D9142844C13AC2170223E131519C2EFE1A6F1D128C9C9638BD410F5CD55837244A8220A9517ED439E33B32B040441573C029F6CE99EC8BF9BBA5C937A3C5DD425987778FE837C6085471870D539C4D69973B46F80&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8DFFEF89002563975D6C20C5FB80C08359B868F7607445EA7702DF6962AEBD2CA4F8670298056036803900E642A5FD53888BC3926E311828BBE4B6DA24370EBCCF551F5F20B616B74A28E95156566EEE0071A350F410175D32322652F5F6CD239745D47E562E8857D2D079E590238EF69F06149F0E148F4E6D360B91914223A5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5EEE8A7626F2E76A298D8464257F1EBE83074B864F4400CAF7E334A3D75543E7697488BC7793A14D8ACC9FD4B009833498D43199BBF06EB5FEB96AAC5E4D9E190D115692896F29192F7A9D50025A16F18CFF252B9A5959FA9A66A5807E9D50BE513048F6647C57EBFCDEBBACEACCEBAFC0FFDCC9B53F0619FC62FE26BAFE8AE1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;596CAE0AB54E4961660066109C10A44E0A50A7A6C4DCFD32FC31A6FB9164CA680C9BB52C580BF4D6A73AD572F71808B7CCB450B66218C02C9E9AC22415903D4AC4135A607AFE0EC555FDC89260729A3B545C4B63B4334617DED01E353AA9CDF8ECD38E93D817ACB51F665EDE865466C592B6DB908C434CCEAAE07A2D35B386ED&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3E688536C1DD97D89F87A49F11BBF1287BFADB06DD98E5308BCA52F75EF0BC3A79706B0D118FA12FC56AF7EA730C3CB8A5E0E3B511A45D0C6A6B6BF095609E324BCF9A8FB2BC731C6676B009001E354F59D421ABF33198839319CAFC4622AFC9E6E61A05E39490BA105B624E5329CBD71CFDB4D6D6B862620A61D4245011F29C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;66CC956091314EB3496D6CBA6424F6A7EBA72172CF2B52A1E7DA4B6669A2493FA22D77060F3CEA7212F9A4302C4727612F90687E021C24500BBCBA9AF9FF8847FDED0D44EE9DB9C4F79C748FA8F63F0B7DFD610011005100B051C77F0E80183439D7F91123575DAD608101D3AE08D029F54A15B5B9D6847930E73C25D0A64C57&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;88ED89C07EA8568B52B69A94123634D0E75AD3362AF29CA9D66A24F5D2B66A187CCC30A44D928810E5E60E4A4E99C3DAAC797F552A38FCBD28966E1A66D74B97C8543BA82F99AD1046F4CCFDE60785FC09009D0016406DA76538DAB862D56B8F277A6D1A5B18B96931085B52774644A0551B64E3721E8C9F55004C09F3884969&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A6B68878DE90DCA12EE2F4C2E68745094C4F89380C407AD73A430940DC69424C51E019AE752C211C126A431DED729EDE2841EBA9DF50E47CFD168A1899B4609621336C008A2D9174E00BDDF4900DEE554A67882185677B161DA78E736186FD03E1A231927439919A0758A4F53272F13B16693DD75684866F4D5FE97481375E8A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BD31088AD4C476835D90E439055E433076BC9DA56F05936456FA2258B3E04EE48D72CD6C23DAC5002E01B004C0520097025806E032A8DA2FB5B0AA2F27A0AF78BA6AD41F60F55F09603980AB00AC007035809500AEA1F57B614A748893AB7F67620A537F35507CADF6D0431F7C5AADDCB64CAE0AD4905EEB8CA8AAADA011A784&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9405F406F4E5B468344CA32164CEA6C4185AA19642047D7634D41986DD6BADF54A6B420D9680B6CCA013507A0B95CE8C76A83760C9C7461334880EF725EA04865C8D0F261A13F0A0DED605914362F9CE567902D37752AC79565C991C847518D22C446D886F887728F4E586DA485B488150F35C1A2181AB79F44A975E5553F565&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A3C1DC9884A51B35564FAC73CFDCB91A2DC45B63D21708CB3EBC2623A36C9C710F9F4FC77A23443752EA8160C1286C35F6828EF16366E4DB3156DF780B73531A43BA416C26E663337136361337A56226E6B3267E266EA20D37D18A9BF8452A4D845913B7889B68D19AA03C68C64DDC964213DEC821CD1EDD875AE143EDEDF441&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;16C33B34AED3E96C34C5F9AFF60097C27ED62C874D6A745A75266FFD50C29B85600E0C9D937EE2887A0E433DC31135C6504F7742F52652984FE6FE176C127237C07CB7FB33A4D3D2B95E7F1FD43817AFD7CFC1EBF5B36DEBF533B9EBF58D82F5FA1968BD5E822712907AE9549D02E3B37EDDC493ECF72950F020D4380D53D080&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;29986AA3600A9782C9020A26E11D070B527DBEEBA5C576FE1B5E10F82E36AA5BB6A5BB7BA533A489784BED04DEFEDAF1DC7DB0A770F7D7D671F7D78EE3EEAF1DCBADB7D6D1B98D71F4690B05CFCE74DCC1DB644168D3AB6E0781898544E422B737A704450176555832896DED680FB6C4A134E4C3D7F72C345E0B4E86CC0D6BBD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;27914E0A2B354FA3D99671EF89B05695A42D7B313A8AF0643D9713C8593D06BED209A4D615CCFA3C07602D80E76133F23A98E562219F95088682A039E9D98F7CB175C7A43900411EB3E972BBBE4AD60B760705E328DC93ABA650B6A1E89FBE1DC8A7CE474C513A981F4463647E48AA9363B1A972A4530EA9870EC0631F51E4B6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;362506C5E2344CA4DEAB71221A385413EA41CFE3453432482CDBCC60CBFC7A25D246F71BB55203674E9A1A8C445152896611698A669520B06D944489465996588AFA3E553E5BCD35A7B2324005BDD10B430294D1138AB5725A1ADD7BA5A5B182342D1B1630828910C4FEE97FE052813606B0760F91DF12745F170C2B11909322&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;94D8409EE063C1963294541F6D23939D447B38D8528C8B9389D9CC207DDE9F0D823D3E1A0BCB10536D9D1E8D074100D46129C0FBB9485611ED3B54C3F06CBB27C8CC749C0209B3157000A484DED7B89AA4C9D68CCE9012CB677246264B0AF04F5DCC8DC4E992425CA34EDDADA78A524F4818A68B6E99766B91DF42944C85B840&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4B60929CAFDDABE29CABDD01DB7B69D78660335C55BA598B16112F610D60392F65AD98849DA59A25BE9CE15AC45E4FB7C8BE9E6E51803E5ABA5D0B588E5D15588E5D1FFA5AA93594C29EA56B4629AECFD02D966AD291DEB81AAC2838C3A42D6C3C54956143ADEB4D091E7BA63C7DAC89BA54965B733435AAB0A6235DEA6DAB4D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;53A8222D03691593178B6AB15E9AF4ABDC20D9A46425FA98224D2BD612B1BAB10AAC3A87F5812A5E914941A8F695A2244305317BB01EEA6F1CE2453EF46E8491923A66762537268CF4DAC38DEEEA5A4E37B2A5B2732B95E59C5417EF06733DF9707360671DED192C22FC7FAD31A6DCCD9411BFFF3D4C411206F0A589D90DDBEA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B11945D733FE7947569B02584788B19085B1ED67B5B4ABD91B5B850637900982831C801DB947716B3499A02053B32136DC066C93CC0BECFFCE76593D78A29AA89A14F2F9AA4F0D5A12D652F366EEEF10C12E3262F5402E9D369A03B2E5201BBA7BC1B55FAEFD72ED976BBFFE4BED97BE7D771477EBAFC59609B6FED690B2494C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1CBF14495D7DB27F0344255E04F012808D6A5EC7188FE9E5C08C93FD9B207B33802D005E06F00A80AD005E05F01A80D7F5FD0C6F42D78EB4BE1668D717FE8B11FEED007600D809601780B718C595A4DA542553B0F1F91D00EFD2F71DAC3BA72D6244634F8BA42E1A7FD217AB7BAF210C5C9591513946EA072F71D2C0D36AC2AC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8D632967DF87FA7703F800C01EC6F67282388A451FCBC8458D118AFCF60744F291D68ED4967293F25882433C2B59384DE79674F06B462CE2C0575FA7C6010936955CCC56D3F77E8D58F1E5D75656E44A70F41D807C003D25112B0A20BB10402F0045008A01940028055006A05C62ACE843AEA4DD5F235654405E3F00FD010C00&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;30508B0E4B6F7FADB2822EC21E06598743053B75AAF3C6AAAC7870B5B483C78A23A0CC91008E023044D238F03AE6C06BDF3907D6A6CC81D57C0E3CCDE3C0532972C0BB847B166703DADDE5C64DDDB8A91B3775E3A66EDCD48D9BBA715337EEE0C61DDCB8831B7770E3A6AEFD72ED976BBFFEC7E3A6D4043DAA45506838E5115B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;04650A041BEA014C05D0208CA04C83ECE9004E05300340238099006601980D608E1E41990701907B7104E574C83B03C04F009C09A08945506ED72228F4552319B29AA1825B8D60C9AA8C8C996B6838ED163DF1BAB16A1485900281527F2B803600ED92C6819B30076EFCCE39B02C650E2CE673E0021E07CE4F910312BC9BB684&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6D105C8859B100B12283965F0EA5AE02B002C0D54256AC84EC6B005C0BE03A00D703B801C04F01DC08E0269D1537435F5A3556A82FDF40DE2D006E05F04B00B731569CA5B182FE4ECD1D90752754F013132BE0F89A07574B67F058710F94B917C07D00EE97340ECCC11C98FD9D73607CCA1C18C3E7C0893C0E9C902207BC9772&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5F916CA0EFB3B9B1443796E8C612DD58A21B4B7463896E2CD17D16779FC5DD6771F759DC8D25BAF6CBB55FAEFDFA5F8F25D2F3054769C1136A8F6A6CC1930D10677811C04B00360A83279B207B33802D005E06F00A80AD005E05F01A80D7F5E0C99B10FB3812074FB643DE0E003B01EC02F0160B9E546AC113FA0BC7EF40D6BB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;50417FD36EAC99747B9ED4CFB41B0BBE19B5FEF7A1CC6E001F00D823691C28C71C28FBCE39E04B9903197C0E1C3CC8E1C0570753E3807719575667C8442B407496AA0784B4CE2529C1CE385CCE532FE14C25F5F9A04E49C82DEDC4BA24D480C738388C281AEB52734BC983726D67226A4ECD633F140EC6CD0D4FB9E1A9FFBEF0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;54197ED3DD08530DB0245BE4B937279B0A77B92583497AA9255D15FB624B2A0C531F4B9AA110D63A54EDB0F6D4A22AFDAC1DC27AD3DFDA2B93125973CD1A35D05AD6A25EB67C8BAED9F22D8A779825DFAE85560CBB4A5A31ECFA79B8887B86B28A5174CDEDCF6B476FC3966BD2E94A5EF558C17908266DB7CA85AAFA5611D5ED&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;403F9EEC32A370982853D7BA81220CCD5C0C12E523DB51296C4533247D2D08C8AA58E5DD6262ACD499ECCD403B2B4DC6A79F4DE69025AAB06462B364ADD86AA378F6821AACBE5C4342AD577F4E9661CA78C3E086DDDDB0BB1B76FF9E85DDE1F924EBDB08BEA78E693DFB3469CC289563EEDDB0991B3673C3666ED8CC0DFBBBF6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CBB55FAEFD72ED976BBF5CFBE5DA2FD77EB9F6EB108E8E513FB78FF1FBE9E10C391EF5685CE98B83E86C897F1DB4BE1790EF81132500140028F488D6F27A41761180620025004A0194012807D01B401F0F5BCBAB2057D27B07D17B01FD216F008081002A011CA61120ED3A880E1A19045983A182EDFAB29DF191B67112FD4741&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9921008E06309471E055CC81ADDF3907FE9232079EE173E0491E079E38140EFC0973E0E1EF9C0377A7CC815FF239F00B1E077E9E2A072EE32AF1F468485DCF5D265CCF851FE519ABB40523F4D771E05778E8456342E9302DD6F2D67D0BC24DAAB6C6557358CC0A2013E92EF2BA8BBCEE22AFBBC8EB2EF2BA8BBCEE22AFBBC8EB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2EF2BA8BBCEE222F5D7FCD90FEFF97795389E9E5A61E0871038D6EA0D10D34BA814677A1C4B55FAEFD72ED976BBF5CFBE5DA2FD77EB9F6CBB55F87BED04B57E86AFDF33DEAAAAF2F442EB24F08286D314539C91FF6C04FCC12A3701D5EF9BBD6B6F2D7018B64E7000012FD71E1CA5F02B23B012C00B010C022005D00CE05701E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;80F3F595BF0B61E1EE62BCF2B718F22E06700980250096B295BFF3F0CADF32C8BA0C2A58C45BF95BC85BF9BB12CA2C07701580158C0331CC8173BE730EC82973E0743E07E6F23830E75038300373E0D4EF9C03B52973E0C77C0E8CE67160548A1CF05E4932FAD874AC5E694D40C8AF087ECA38DC04C69DB7CACA5F5EB5AEB8E9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8B5250E330B5B6724B9A6D594B4B57DBE96349E52C6169396A0FFA59524DDD29A474A2BE94E004D691229CA8F6A20C27195D3061AAEDF7C149A6C6F5F89A0EED3E502F0C9D536755E99C32A5E82EDC7E1D5ABFA061EFC883BC1F72327550F83B5363C6F837815A6E06B005347B38A90D6A14220C7540F02EE76AF758B965BEAB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DDAE76BBFD3A24ED968ED4D44DD2D56D3B68DA0E003B411F0F774218E080E0BD8A7FB846B0ADDDF5C7AEC6BAFD3A448DEDABE99BE66689BE7D00AAB607C087A090654E08450E08DE155C8D1D1F8B465C8D7535D6EDD7216A6C4F4DDF32757DFB1C54ED5F00BE0085CC7142F0392078AFE69E7F4E2FC787A2D1585E3A54F67BA3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;23AE14FE1B52E8D164284B9FA78103F0C3AF07F82502A4AFBF4A8EE05D293E250D487465EC7F5EC6BA3511F2EA76AA07484F36003FC8D8970E08DE6BC46F6EB832E6CA1869F8334D847CBA081580F41402E80532F6B10342E1B524B51082C765E42EBF40BA815C2F04C12A0709AC8D05E51041A895F6908A4E6281F30FC8CD89&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DA6FD84837113057FD4172ED5BA2B505A1736937C15DC1A2DA15D06E3F00FDA1773F638D0D3037F60E6EEC6DDCD82F9237F6166EEC7068671080C1D0D82DACB123CC8D6DC38DBD891BBB2D79636FE0C68E8676860218068DDD0E6F554163C7645A16C4B5F87B9B1209D4C909795242096751FEC340DCA18D467EA1741FF9371A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7AF13269E66E3DD4AF2E48488349C6830C638B08E339729B008CCD04633DC5600B1AA97EF107EE3FADF58F000237921A9F675C7B89DCAC65372F929BE7185B601D447A81A4EC428B157713979A23ADB7A40E82D49C1C691D497F9655B69651F0BC4E81F159BF6EE249F6FB1428F81150F01CA6E02F98823FDB2878964BC16A01&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;05AB3005CF13B080BEDE4852B7B3D4A7D18DBEA8F394A53E1F6DE5494BAA97A63E6149F5D3D4C72DA93D68EA63DC7A1FD552BFD1733C546433E056FD2131478C0D049C06AD4FC9B42DCF668C81AF243D4C6A7980D1FD10B9F91D636D83AD90BAD486BFD28B4C02FE6092615C8277EF2001D341027E8F25E0012C01BFB349C06F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B812F06B8104FC0A4BC04B0444E1E27E1EB684F8E9DD98EA060CAFE1FB7B849BE20D725829D43387350589558917A08478F05CA508DD2F5062B0B3225322150D6CEC8A13FCE1B322C4B5342A601883E7D21D4113A321E2BFB3B3D7D1DF39232CF39D464091FA5642252CD457AA9BD6FDA7A3A194363182EF762258DACC50EF72&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;44DDC250EF74447D990060A5BF09FA7C16EA337D13A292BE4BE197719F5F311569C145E01D09AD480017D9CA3AF40B4B8726DB3AF42A43FDB905F5581BEA6BAC236DA4AD1C7F7B26FA99BFD7CD7967E3DEBCC95471BE58156FC0AA783D56C5480AAAE8DD9EE27E9D1EB05F27DC1469E80C37C3BB39E0E7A6478391447CAC9258&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A82811F535B96418AA58559219685D30DE1192BBF875E46B4749ABE8BD488DFABB32E3A29D9144B9DEB96126CC0A9C6E29A3D19F9D2D49D9125DC093D857D216E288A6EE48694F5205C122F3F0E65971655A2CD8168CF8C34D01F58A743DA0BEF7012F5417686FEF402F606B54A17A6EB69E0FB4D1FE1BEF02E5B01479518E91&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;09783308198199515A97BCC89A128CE4E94D936C74138C140037E4453A2F9CF85BA9134C77440DB375B28F00415ED447589453A99922078460A4C28660D02BCE0B4606D8F24CDCE8C7EFB226AB9CB2E99045F6B28EF523F97CF62D59261942FBE252DF46091F9F8F083EBAD295007D3E3DD97B2D3120A975C1BE264E77C37464&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6A0D5A69CB61ED39E4524DDC99F2BEBFBE0493A368011D65AC1C57CC7A0967D59FB2A8231A51220943AA5D65FC5F5446812292CFD1C9B5C0B2F5D4E793C46AE65D49B42AF5FAF89AA57E068F61AA9A062DDB95DA9E5950B17CE05C3492688F6B478518EA64736B56757435EB7FD3CD89BC5C765ECAAEEBC8E44A8377753B78B9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D5C1A1EBC13FC52050B482E8638A55F395F13FD7BDB752DE889EA9AADFB8CE50A233A64C8AB446B3C34D6A369CC8A34A807A9F9925556A0F7E1342D166FD996F382A7C9863A3C078FA7408214EEF72C22BE7326636ADD31E6EE863B6F79D43D91CEFB73F04F79DA134072301B939A4184FBB2A4A053F8B162B358B9356A2CC96&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FA1F3C68C35F0A0FDBFF697E36953BFF42FA8088E1A24CF5C7B1E98AD6BB5A2C53DDA7AC03691919C047D9E3E99FF3C880BC9FFCCD0018820AA2CFE40127C606A83141F0A62BB1163267EA63C93B2512D072728975981421EC492801629CA7CBE499A80DB489DC44C89D422D03C10AC04D23616B2F10EC588C948514402D26A8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;28695224A02C2AA6964E4FA30758A96467D1873AFB6B509410A37DCA3FCEDC9AA2593AA0E2B24D9E01AAAD97121E3A36C2DF25EA355981C163BC4BB9536C5E4FF8155A3FDEDD29BFC9E12A9750B92EA00A85E18599FA36DF0CEF0787F20E8CCB66219B2FA2ACC5703166F39E54DF1E72592C64F1C594AD185E82582CE54A5A4C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;718938A6783E8E299E87638ACB5209EFE7495A78BF2B9DE1FDCB21B2B01087F717E0F07EA72DBC1FE786F76382F0FE3938BC9F2F69B1DD0EC7B8744F498BDF422CDD0FB174FFF538865BC0AA0A395655C850E73BA2F662A8673BA21649280AEE87C0B9FF66DCC162533E44C9FDB7E0FC12D6548B6348BC94A1363B86C4CB2453&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D8FB76DC62B939EF4E9CD78789EF5D62F13D038BEFE9587CEF4D457C2B4813DBF92162A91FF9B783173993E69176207A46A360F7A7230A26F527FF76D277E348BD2BB5BAD31F079006907FBBAC0F4ABF061266C172B8D6F07FDECE40F20F9E7BFCBF85BA1B49BDF070A13F28488791FC77F833DD07F14CF770893FD36DC033DD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E7C84C573A8260BE8F6772D2548203B339DED44CAA27194BBF32AFE0EA927724A96B377FAAF00836B04711F801DFD73D86F18610B8876FB09FC0785398C43F2996F83A2CF1E3B0C43F938AC4D733835D9B4E83BD1A46F9646CB04FC206FB449BC1FE31D760FF4860B08FC7067B2AB33CA31DED6183DD606FC016661AABEA58C7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AAA633D46A47D45319EA4847D4197683BD0577B0D16EB0B7E2FC99ACA9618E067B16431DEA68B0679B8DF21B99C840CE31E76DC3BD99C7C477BB587C0763F11D84C577572AE27BBAD560A3E6CF101AECC334834DEDCFDBC90C76465203871AFB0933D803BF5D837D26CF60BF0B24F44FABC16E6206FB3DA8BBC266B065B1C1DE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;830D76B3C060976183FD3C186CC566B04B9319EC926406BB556CB03FC606B64D6CB03FC578ED6283FD19C65BCE24FE73B1C4E76189CFC512FF652A127F1533D8FE741AEC7D30CA3DB0C1F66183EDB519EC4CAEC1F6080CB6840DF60A6679321CEDE1D576839D8942CBD24A56D557079CAABA86A11E7044BD96A1763BA25E6737&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D87EDCC1EBED063B0FE7DFC09AFAFC8093C1FE2943FDEC8093C1BED16C940B718B3799F38A70DECD4C7C8BB384E2FB8F03487CFF7E00896F59560AE2FB8B2433EC5B8406FBC3036886DD3B2B1D06EE5666B03F38F0AD1AEC5FF20C765F20E1FD03E934D8B73183DD0FEAFEEB01ABC1BE436CB02BB390C1BE5360B0771D40067B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3D18EC7B6C067BE78124067BC7812406FB5EB1C13E320B19D8FBC4067B08C6BB5F6CB08762BC0D4CE28789257E2B96F857B0C40F4F45E25F64067BCB81341AEC2A18E54D0790C1DE780019EC970E580DF686033C83FDC201BEC15E7F00EF786496679DA33DDC6837D83FC6166613ABEA2F8E556D66A87F7644DDC250D738A2BE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6C37D8B5B883AFD80D761DCEDFCA9A7AD2D160BFCA509F7034D8AF998DF244DCE2EBE6BCC938EF4D26BE53C4E2FB3016DF87B0F836A422BEDB9318EC1D4283FD076CB0A7A7C560EF6406FBF7DFAEC1DEC533D8338084DFA5D560BFC50CF64CA8FB373683FD8ED860CF5597BFD49FC3795760B0EFC5067B0318ECF76D06FB9E64&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;06FBEE64067BB7D8609F890DEC0762837D16C6DB2336D8CD180F7E0F4A3730BE00C9EA47DFD2AB1C272794B668ACAB525D15673B9915AC2EF03352545D5AC5EA723356979F617509A6A22EF9AC89B3C54DFC143771036E229C4A13F0DB46D4A15C974E87120529BC063B9495D8A15C6D732857711DCA728143B9123B14F86126&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6A19AF70B4D7851E9B4339170F692F56D5A58E551531D4A58EA8C50C7589236A89C7E65016E30E967A6C0E6509CE2F634D9DEFE850CA19EA798E0EA5B7C7E4342EC32DF631E75D81F32A98F85E2916DF4E2CBE092CBE2B52115FF8C92C9143815FD2E23B941876282BD3E250E017BBA843E9F8761D0AFC2898CDA15C0B2444D2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EA50E077C7A843B91EEA0ED91C0AFC0299C0A1DC849F00E047CA780EA50D3B9497C0A1C0EF93991D4A6B3287A2247328F033670287722B7600F04B680287721BC6831F4B1338943B30DE7C26F1778A25FE2758E2CFC0127F4F2A121F6603732F0CCC3CDBC074B02EFC4ADC85D9B80BB370177E9B4A17CE613EA3319D3EE301A0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E754EC33A6639F31CDE633A6727D46BDC0674CC13E23C68CDF6447931CB7FB8CC7B1914BB0AAC63B56D5C9504F71445DC050EB1C5117DA7DC62ADCC145769FB106E777B1A64E74F419E732D4131C7DC67966BFB016B778BE396F1DCEBB9089EF7AB1F8D660F13D0E8BEF8BA988EFE2243EE362A1CFA8C63E63635A7CC625CC67&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8CF8767DC6129ECFD80C24FC30AD3E6329334D2F43DD3FB099A665629FF11AF61997097CC610EC333681CFB8D2E6338E4AE6338E4CE633968B7DC60E6CE3AF12FB8C5D186F85D867BC8DF136316B3AF040AA2F62DB04DC6E4DDF8521E88FAD693F6C4D2B6CD6B40FD79AF61658D3726C4D3733B350E66816B60886B6140FED1B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;30B4DB195B4AD2C9968F802D45982DBD305B0A6D6CE9C9654BBE802D79982D3B185B721DD9B253C0961CCC966DC0960F185BFCE964CB3E604B0FCC161F668BD7C6964C2E5B3C02B648982D7B185B321CD9F2A1802DDF7423B6EC00B67CCED8F275771AD992E985D5A06EC49603DD882DDDDD56B6ECEBE6B1656F379F2D5F7623&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B6FC8BB1E58B6E27B67C2160CBBF305B76015BBE616CF93C9D6CE9096CF92766CBA7982D9FD8D8F211972DFF10B0E5EF982DE015285BFEE6C816387B89C7960F315BDE06B6C0593A942D7BD2C9963EC096DD982DEF63B6BC6763CBBB5CB6BC2360CBDB982DD98C2D6F39B2C52F60CB2ECC9677812D058C2D3BD3C996C1C096ED&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;982DDB305BDEB4B1E5752E5B5E13B0E555CC9642C696AD3CB6784C3B1F056C7905B3E53D78CD014EBFA9141CD0D3D69E688C76C65A943EF0AA4ABCC5D3227B42B2E71CD913573C7295471EE191477AE46A4FA0CA1318E1098CF404AA3DF12A4F7C84273ED213AFCEEC885791EF08F21D49BED599815815F98E20DF91E45B2D7C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AD3057F45197FD8F1D671D8F8B172F3E897D531BCB75633332CA483D378EB3E7C3148249CBCBDD693C40E687202D9BB1B46CC2D2B2D1262D2F72A56583405A5EC0D2D28F51B0BE3B8D07C88C060A9EC714ACC5143C67A3E0CF5C0AD60828781653002725D103645677A3036456E11B365CCF74F30E7A79BA9B7780CC53DDBC03&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;649EECE61D20F304B7DEC7BB9D1E5B1F73C4389C0DCFA3E914B0C9303C7FC2C3F3301E9E876CC3F307EEF03C28189EDFE3E119C42878209D02D60814FC1653F01B4CC1AF6D14DCCFA5E03E0105F7620A063301BB07CBD4DD3C01BB8B2B08777205EC0EAE80DDCE15B0DBB8F5FED2517C6E75C4389A0DCF2DE914300586E7E778&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;786EC6C3F333DBF0DCC81D9E9F0A86E7063C3C431905D7A753C02240C1B598826B30052B6D14ACE05270958082E59882614CC0AEC43275054FC02EE70AC2655C015BC615B04BB902B6945BEF1247F1B9C411034E43A3A7B05DDC2D3885ED470C63B108034ECFA247D35DEC351D4D7761373A9AEE826E74341D1C9D95E468BAF3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BBD1D174977AE1651C0097C1A04F67DD5924EA0E4441E9E1755710FC7CED4D22783F30D206410DDF9524B9211C6F89C642C1E663E0B7D9E1DDA513470CAF1A7E5C5555D5F0AA632AB597884F8C289D89981C3AA6727A677328D83245E99A199DAF444E6C1E354A3EAEE5B89A11C7571FAB548D3EBE100E90D24EBE6B62CD2F47&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CDD7454905746DD60FCD17CA3A7E61C028DAC68AAEF03A9EDC689CBAD7AE55E05F094CBA065AED5978B651EB7C363CD79A87278287278C8727927C784278787E0A8DDE08E026181E3814C53888EA662FCCCFD405EA49B198D2D619926395F4E804FFCFBD28BE1963C57E41527DB7A062F0667A6563871CD18ADD8A8A65C16BDB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8EAF81E7A82FD045E4301C341AD04E2DEC156EEA8C2BF072EFB4054A2C160C28FE16AD4C5D5EB829189FA1C881699150576E4B34DC216BAFA327944509B8E811E90CC3FFBC80F6F23CCD85E30160DA4DED41267CE0CD7C41078D5AFB7311666A4D1DC1CD558F9E52DF4807ACA3B858ECD57E03EF482EDE8418EC2508CA91711A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0539C3E873199CD1EAFF250CEE6D006E077007803B6114163271BD8B2BAEA6D7DE0D715DC4C4F51EA8E85E00F781CCE64B17B0FAEEF7F28EC0365EF3352ABBD010F38B58E95F7B453FDDA7BFC06A54B0D8A8E06256C16FBDA297C6E9260BA3F02546E1254CC91E302BD9A958C9A663255B965CC9A66125FB2370E921000F03E3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2F6736708AC006165E6FD8979B8DCB5B8CCBDB8DCB3B0D2AEE62543C62A66212A66222A6E2DEE4544CC0543C01043C09E029A0E27E6C2AFC602AFCABB04DF835B6097EB009FE35381F5EAD0203C097D5E700AC65B25AF82093BA7590B41EC00B54EA0A1F3138F19871F984C194271953369899722C664A3566CA33C999321233&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6513746533802D40DC6A36B4C34543BBC1E8E416E372AB71F90623F51520B0A0709B41CA7646CA563329C33029433129BB9293723426E50DA0E24D00DB8094B7EDE3BB138FDFBBF6F17D1BE7BF97747CFF0AE03D7D7CF730A27743D20700F6A8E3FBB1C1994F8DCBCF0CA67CCE98F2A19929659829A598295F26674A0966CA47&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D0958F017C02C4ED63E35B281A5FD8F0AF75D26F5CE6199785C665917E29C116764AC53FCD54F4C454E4632A60FF7A122AF230155F00015F02D80B54C09674CBD076E3A1EB9B651BDA83381FF66C8B87167EF8D02FF9D8D0566A44FA3321290B80D74787F6488313438CCBA106538631A6F87C26A6ECDF8F98B26F3F62CAF0E4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4CD9BB1F312507BA920B208F0009B628D3A1FD7CBF60687F6C74B2D6B8AC332E271A97930D2AA6302A7A9AA9F827A6E2534C4543722A3EC154140101C5004A808AE9F6A12DF7A1A19B611FDABE387F66D2A1ED0FED0CD087762E1BDA4A483A0CC0E10006C1F8E6159E69B0E32CE3B2D9B8540C26B532260D3633693B66D236CC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A4607226BD899934047A753480A140ECD9ACB161E6C6B6E2C65EC18D859337F6326EEC87D04E158011D05894C9D546915C9D6B7064B171B9C4B8BCCCB8BCC260D9958C8A6A33152F622A36602A5624A7E2054CC52820603480E3818A9576B93A01CBCDB576B93A19E75F9F54AEC6423BE374B9BA89C9D52990341EC004D564DC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6A70E236E3F20E83297732A64C3433E551CC94473053EE49CE943F61A6D44357A6026800E2EE4D4AD4A98037831125FD8AF56CA6B967BFC13DFB35EED96F93F7EC57B86773A19D79004E839E3DC084EE1E91D03D6EB06F9571B9C6B85C6B5CAE33F8BB9E517186998ABB301577622A5E4C4EC51D988AB3800019403350B1D12E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;740A16AACD76A16BC7F92F271D9FF9D04E4817BAD798D04520290AA04315BA1D06277619976F1B4C7997717BA580DBD2470CE36A11C63E86B14284016BC514E32A11464F86B15C84D187615C29C218CC30AE1061FC90615C2EC218CD302E13614C6618CB44188D0CE3521186C230968A30220C63890803A25334BC79C9FE3406&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;682F02215CBC1F85372FDA8FC29B17EEB78637CFDFCF0B6F9EB79F1FDE3C773F0A6F2E631474ED4F6380F672A06021A66001A6A0D346419C4B414C40C1399802880BD2006DC77E14938DEEE7046823FB7981D4F07E5E8036B49F17A09DBF9F17A03D9B5B6F70BF53F8B5DD1103827AFA8FE85EA3DDA82FC8B2816B4BA7E8FD1C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;064EC10317C003D7621B38993B70670906AE090FDC8D8C8233D3297A77010567600A4EC7149C66A3602E978239020A66630A6E62A2370B4BDB4C9EE8357245640657F44EE58ADE74AEE84DE3D6DBE02858539D300A21420B0E2A3B17789B5F208197A411B287098B0BB44822C417213E6B04B76F65E5F2D856CEFC822C709B8E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9150F5F0C760A47A64FDB849753DB4802C1C7C08BE17BE59E07993074C61D2901B6E8AD40923BDF9E126B51139D236A90E7A2A65EB1E3E0BA29CA9055D070297223471422CDAD901C73CC68B5AE8D1862D5D4652AF0EF5D0482325AF83C64517288DC1B6485E446993D94D995A5F9DD2120CCBA14685B0464E446325B8199658&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;666E8A25F766C9D64A7AB28CC6AE707334542447E2C171A6247F448EA85765AC8784D0203D864D4D667D3527976B145A5B2C3551CE52F3B554B56C4F723735180A29EA2D8AD1179B7851176C0B26E2A516E2D4D472963A5DEBF57439915062113DBD41EBB6965EAAD66C492DD3FA65A984255BB04BCC34AB1DC931160EEC7257&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BA80C85280C8CF743916576AE3AA3C955B52D988F4E8D143327F4CD62E1B7D20D3FF08CC381F050057BEC7C10C0EF33D01FF7EE07B12FE0DF73D05FF8EF13DADFE7B464D5C05FF06E7F85693FF990D7283EF597291CD06D8B706AAFC01BBF5434D7EA8C7F71C943BC2B7168ABDBBF841D020F65B1F19F6F369D474D2D12C5841&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4879BD824EA8C5AA5C0C4B35754AAB4C4C055BBCF0B3C59FBA42F8E90A964CD76BE4709D12276A066298D7816EF261254797D0825034D2063DD1C6BA30DE1E8D255042799B12516244D22DE9655A7ABDB97C7E02ACA45E7B18CE17AF93BB9824C9215B45F1629268A925AEAF30F56C0DC6E20952C3B4D6398A32BF90A5C30D9C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AF5ADCDA190A316E6AA5294D9066A209277429728C1E7DAE2514CBCDCD31654190A004485B702C643C3BA05D94A24C5A88A6E684F54B9D2AD4844E154E231A4F5846CBC009EBB4DA53B484723D21D2160AC6DB597A2EB512CA9C682C10EF15ED00E991436CA4E3A605BC223224549D8D5E168514B9631EA356ADB095CADCF890&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DC1657C5D5F864D23BC9E3237FF4D383DC67D25F5849713D0D7474480A224F9BA7AE487DDACC313E6A1080FAE76FBE21F7BE753496403E955DE493E35FEF838DFB2FF85829FF061FAD21EB58E2E9536EDC7C122FB495050DA748A7F6C34E89AE0E38EF5835BE093266F4FCEC82C4C228B59294F3F2A2BEACD4301B6A6F3DCB52&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;86EE1D1C965A6760993C0EE631AB9A70E0500A9999C0988E3FDE7B9CD73CA1AF44121BBB222D903C29904D849692D733DCD40CC9EDB168844C07C09AFB24B08F99E65342481BD08EE93C64E93E2DD5F42291F44736971E99CEA7814D34B08DE7D23FC473E9E1B6B9F431DCB9F430C15C7A289E4B3FC428383A9D4F036F000547&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;610A8EC4141C61A360109782C305141C862978983D0D54E2078081BCA78101DC597B7FEED3403FEED34005F769A02FB7DE3E8E4F03BD1D319E60C3539E4E01FB1B0C4F291E9E123C3CC5B6E1E9C51D9E42C1F014E0E1799251D0339D02F63950908729C8C514E4D828C8E652D04340810F53F01413302F96A92C9E80657205C1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C31530892B60195C01FB661FAFDEAFF73989CF41278CC25596C7CDC2359CE74809D6F5E93E14F374D70FD35D3F4C768D09AA1FA6B97E98E4FA5751DF08135C3F4C6EFD6BCC883093F5C33C36D539AC043B0B601E9B9AAFF6AFA3ED8B3CB4B49B3007BC347CA4759A7D37FB82F55C5FF002D7176C6282FEFEBE34AA6ABF1E70D0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D63E24E8EFEE4382FECE3EABA0BFB58F27E8BBF6F1057DE73EFCBE1EA360C7BE34AAEA1140C1364CC19B9882376C14BCC6A5E05501055B31055B98AABEB20F69E7CBFB38AABA85AB529BF7F15475D33E9EAA6EDCC753D597B8F5BEE8A8AA1B1C315ED1E290F45DD837D858BD904E691B0563B50E8FD5F378ACD6DAC6EA2FDCB1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FAB360ACD6E0B17A9351F06C3AA56D0C50B00A53F00CA6E0691B054F7229784240C1E398826D4CDA1EC302F6284FDA1EE14AC59FB8D2F63057DA1EE24ADB1FB9F5FEC151961E74740C3BAD8EE16D9E63F8EBF7C331BC9756C77033760CBB35636F7EC9E9032DD5141D92F6701DC3474CD07F964E550D82A0DF8805FDA758D06F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B009FA755C41BF5620E8D76041FF9851B0329DAA1A030A56600AAEC2142CB75170059782CB05145C8629F884A9EA32AC9D97F254752957A5967055F512AEAA5ECC55D5C5DC7A2F7254D50B1D31BE60C373413A056C290CCF797878CEC5C3D3651B9E85DCE15920189E4E3C3C5F320A12E914B01540410C53700EA6A0C3464184&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4B415840410853B09709D87C2C5367F3042CC8158476AE80B57105AC952B600AB7DE80A3F8B438FA826EAB2F38C8F305B083F07BE00B600F63FA7C413DF605B017D2EE0BB27C3C5FE0F5F17C01EC5EA4823E259DAAFA0808FA242CE813B1A04FB009FA295C41AF1308FA382CE8B98C82B1E954D55540C1184CC1C99882936C14&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9CC0A5E0C7020A7E8429807DA354558FC7DA399AA7AAA3B82A55C355D5E3B8AA7A2C5755ABB9F58E7454D5118E18456C78AAD229605B607886E3E1F9011E9E636CC333943B3C470B8667081E9E6246C151E914B06D40C1119882C1988241360A0EE3525029A06020A6A08409D8002C53FD7902D68F2B08155C01EBCB15B03E5C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;01EBCDADB7DC517CCA1C7D01EC4B36F982BE3E8E2FE8FFFDF00503D2EA0BBED98B7C4125D7171CC6F50587FB78C1A5413E5E70690813FFAFF7A6518133B3E1C49ABDF8C49ABDF8C49ABDB6136BF6724FACD92B38B1662F12FFA319055FEC4DA302E701059F630A3EC314FCD346C1275C0A3E1650F011A6602853E07FEC453AFB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F7BD1C05FEDB5E9EA27DB897A7C07BF6F214F883BD3C05DECDADF7FDBD4E0AFC9E23C60FD9F0FC359D02360086E71D3C3C6FE3E179CB363C3BB9C3B343303CDBF1F054310AB6A553C08E020ADEC014BC8E2978CD46C1562E05AF0828781953308209D8162C539B7902B6892B081BB902F61257C05EE40AD8066EBD2F388ACF7A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;478C516C78D6A553C08E87E1598B87E7393C3C7FB10DCF1AEEF03C2B189ED5787846330A56A553C0C602054F630A9EC2143C69A3E0712E058F0928781453703C13B047B04CFD8927600F7305E121AE80FD912B607FE00AD883DC7A7FEF283E0F3861149E609D829CCC9B828CFD7E4C41C6A5750A72139E829CC29D568CE74E2B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;26701F47EB99A0DF984E559D0F827E0316F4EBB1A05F6713F46BB882BE5220E85763419FCA2858914E554D0005CB310557620AAEB0517019978265020A2EC5143430555D8AB573094F552FE1AAD4C55C555DCC55D58BB8AA7A21B7DE0B1C55F57C478C53BF1F4A3823AD4AA860259CCB0430904E15BA1504B0190BA08C05F02C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9B009EC915C09F0804F00C2C80F31805A7A75385EE050AE6610AE6620AE6D82898C5A560A68082464CC1694C856660AD3995A742D3B9A23E8DAB420D5C159ACA55A17A6EBD531C1564B223C6596C7826A553C01E83E1998087673C1E9E536CC3338E3B3C6305C3538B874766148C49A7803D0B149C842938115370828D821F71&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;29385E40C1684C413313B05158A66A7802761C57108EE50A583557C04672056C04B7DE2AE7C73DC7E994629D4EB5F3A653F3BF1F963C94564BDE1B5BF208773A15E54EA73AB8D329788390BEB958BE57F0E6E2E50CA34C84F17386512AC2B88B619408300A1ED15EA785D360330A1EC577BD80173D407E3E22FAE36BEC8C04E4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2EDFC7703D354AAF3F21D73D66762A71B8F994DCF8E72881887AFB4F729B3DB3BD3346EF3E8352E36341B8FE1C721AE544678CDCF50256E74323FF82CA26CB914E39D6E5FB0270C62BCD317AF725B9F34E95632DEDBEBD7059DB110B867CFBC865E65452E17E729135B933A2F8BAD5AB5097EF003458DBD9D6194FF8BE829E35&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2A1D09055EE7F01D8486A6B524A270F33534D4105DA0667D0377754A0BBDA3A6A000C6DF435F41D2644BDAC498FACD9702B6BFC130BE1661FC8D611C14617CCE30BE1261C07E2F8A7140847104C3E816618C6218FB45186318C63E11469061EC1561C418C697228CA50CE30B11C60A86F12F11C6230CE37311C62A86F1990863&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0BC3F8A708631BC3F854840101548AF18908238F617C2CC218C0303E12611CC530FE21C2389E61FC5D84319661FC4D84319F617C28C248308C3D228C5B19C607228C7B19C66E11C6630CE37D11C6B30CE33D0146AE6EED99B7C803759EC73B96D93875B07A78D5F011291E3A288FAE6E6DADAE19DD3CEAB89AEA961A2FB453C3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3DF5795224A1C4E49644704130D135AD835CC3FB08F171D150486981CB5CF53D0D78D9A147B8290E6FC00C9D50EF586C985A64586AA8B4DABE3394E620E9557348A98DC5E4AEFA603CA15553C1CFA2C54A0121602D51664BA5C885464A539058E178014A8807CF558AD0FD0295F39992FA473CE740EDCD9059F47D46058EA4D0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5E0F99180D0594D8B79D9F9D4DDF7B059F98C381D8B1674192537DF4574AE095393FC5009EE5D5C6E3C4E984E8AB5B528694ED83F5A261587826D4D3375844434B8FCE84B9CA1C7BA1B48833FDD566F53C8CDCFF03&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
			</xceedchart:chartservercontrol>
			<asp:Button id="btnUpdateGraph" style="Z-INDEX: 123; LEFT: 192px; POSITION: absolute; TOP: 1248px"
				runat="server" Text="Update Chart"></asp:Button>
		</form>
	</body>
</HTML>
