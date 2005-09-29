<%@ Register TagPrefix="xceedchart" Namespace="Xceed.Chart.Server" Assembly="Xceed.Chart.Server, Version=3.0.100.0, Culture=neutral, PublicKeyToken=ba83ff368b7563c6" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfEditSetupPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditSetupPaddock" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock Setup</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 100; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="9" runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 104; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						Width="40px" Height="16px" BackColor="Transparent" Font-Size="Smaller" Text="Save" Font-Underline="True"
						BorderStyle="None" ForeColor="Blue" BorderColor="Transparent" Font-Names="Times New Roman"></asp:Button>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 102; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="8" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="6"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:panel><asp:label id="lblPaddockSetup" style="Z-INDEX: 101; LEFT: 56px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="160px">In paddock setup for user: </asp:label><asp:label id="lblName" style="Z-INDEX: 102; LEFT: 232px; POSITION: absolute; TOP: 80px" runat="server">Name</asp:label><asp:dropdownlist id="cboSoilType" style="Z-INDEX: 104; LEFT: 232px; POSITION: absolute; TOP: 200px"
				tabIndex="3" runat="server" Height="24px" Width="344px"></asp:dropdownlist><asp:dropdownlist id="cboWeatherStation" style="Z-INDEX: 106; LEFT: 232px; POSITION: absolute; TOP: 160px"
				tabIndex="2" runat="server" Height="24px" Width="344px"></asp:dropdownlist><asp:dropdownlist id="cboRegion" style="Z-INDEX: 107; LEFT: 232px; POSITION: absolute; TOP: 120px"
				tabIndex="1" runat="server" Height="24px" Width="344px" AutoPostBack="True"></asp:dropdownlist><asp:label id="lbRegion" style="Z-INDEX: 108; LEFT: 168px; POSITION: absolute; TOP: 120px"
				runat="server" Height="16px" Width="48px">Region:</asp:label><asp:label id="lblWeatherStation" style="Z-INDEX: 115; LEFT: 72px; POSITION: absolute; TOP: 160px"
				runat="server" Height="16px" Width="144px">Closest weather station:</asp:label><asp:label id="lblSoilType" style="Z-INDEX: 109; LEFT: 152px; POSITION: absolute; TOP: 200px"
				runat="server" Height="16px" Width="64px">Soil Type:</asp:label><asp:label id="lblRootingDepth" style="Z-INDEX: 110; LEFT: 88px; POSITION: absolute; TOP: 240px"
				runat="server" Height="16px" Width="129px">Max Rooting Depth:</asp:label><asp:label id="lblInitialConditions" style="Z-INDEX: 111; LEFT: 112px; POSITION: absolute; TOP: 424px"
				runat="server" Height="16px" Width="104px">Initial Conditions:</asp:label><jwg:gridex id=grdSoilSampleOne style="Z-INDEX: 116; LEFT: 24px; POSITION: absolute; TOP: 528px" runat="server" Height="184px" Width="516px" AutomaticSort="False" AllowColumnDrag="False" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="SoilSampleOne" DataSource="<%# dsSoilSampleOne %>">
				<RootTable DataMember="SoilSampleOne" Key="SoilSample">
					<Columns>
						<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Water" DataMember="Water" DefaultGroupPrefix="Water (% Grav):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Water (%)" Width="137px">
							<CellStyle Width="137px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="NO3" DataMember="NO3" DefaultGroupPrefix="NO3 (ppm or mg/kg):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="NO&lt;sub&gt;3&lt;/sub&gt; (ppm or mg/kg)"
							Width="137px">
							<CellStyle Width="137px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="NH4" DataMember="NH4" DefaultGroupPrefix="NH4 (ppm or mg/kg):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="NH&lt;sub&gt;4&lt;/sub&gt; (ppm or mg/kg)"
							Width="137px">
							<CellStyle Width="137px"></CellStyle>
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
			</jwg:gridex><asp:label id="lblDepthOne" style="Z-INDEX: 118; LEFT: 24px; POSITION: absolute; TOP: 456px"
				runat="server">Depths should be entered as 0-10, 10-20, etc.</asp:label><asp:label id="lblWaterType" style="Z-INDEX: 119; LEFT: 24px; POSITION: absolute; TOP: 496px"
				runat="server"> Starting water is in %</asp:label><asp:checkbox id="chkLinkedRainfall" style="Z-INDEX: 120; LEFT: 24px; POSITION: absolute; TOP: 304px"
				runat="server" Text="Link rainfall to existing paddock?" AutoPostBack="True" TextAlign="Left"></asp:checkbox><asp:dropdownlist id="cboLinkedRainfall" style="Z-INDEX: 112; LEFT: 248px; POSITION: absolute; TOP: 304px"
				tabIndex="4" runat="server" Height="24px" Width="328px"></asp:dropdownlist><jwg:gridex id=grdSoilSampleTwo style="Z-INDEX: 117; LEFT: 24px; POSITION: absolute; TOP: 728px" runat="server" Height="184px" Width="516px" AutomaticSort="False" AllowColumnDrag="False" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="SoilSampleTwo" DataSource="<%# dsSoilSampleTwo %>">
				<RootTable DataMember="SoilSampleTwo" Key="SoilSample">
					<Columns>
						<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="OC" DataMember="OC" DefaultGroupPrefix="OC (%C):" InvalidValueAction="DiscardChanges"
							NullText="" Caption="OC (%C)" Width="103px">
							<CellStyle Width="103px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="EC" DataMember="EC" DefaultGroupPrefix="EC (dS/m):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="EC (dS/m)" Width="103px">
							<CellStyle Width="103px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="PH" DataMember="PH" DefaultGroupPrefix="PH (CaCl2):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="pH (CaCl&lt;sub&gt;2&lt;/sub&gt;)" Width="103px">
							<CellStyle Width="103px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="ESP" DataMember="ESP" DefaultGroupPrefix="ESP (%):"
							InvalidValueAction="DiscardChanges" NullText="" Caption="ESP (%)" Width="103px">
							<CellStyle Width="103px"></CellStyle>
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
			</jwg:gridex><jwg:gridex id=grdSampleDate style="Z-INDEX: 113; LEFT: 232px; POSITION: absolute; TOP: 424px" runat="server" Height="20px" Width="168px" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="InitialDate" DataSource="<%# dsInitialDate %>" ColumnHeaders="False">
				<RootTable DataMember="InitialDate" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="InitialDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="InitialDate" DefaultGroupPrefix="InitialDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="InitialDate" Width="148px">
							<CellStyle Width="148px"></CellStyle>
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
			</jwg:gridex><jwg:gridex id=grdStartOfGrowingSeason style="Z-INDEX: 114; LEFT: 232px; POSITION: absolute; TOP: 384px" runat="server" Height="20px" Width="168px" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="StartOfGrowingSeason" DataSource="<%# dsStartOfGrowingSeason %>" ColumnHeaders="False">
				<RootTable DataMember="StartOfGrowingSeason" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="GrowingSeasonDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="GrowingSeasonDate" DefaultGroupPrefix="GrowingSeasonDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="GrowingSeasonDate" Width="148px">
							<CellStyle Width="148px"></CellStyle>
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
			</jwg:gridex><asp:label id="lblStartGrowingSeason" style="Z-INDEX: 121; LEFT: 64px; POSITION: absolute; TOP: 384px"
				runat="server">Start of Growing Season:</asp:label><xceedchart:chartservercontrol id="cscSoilChart" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 936px"
				runat="server" Height="552px" Width="592px">
				<Xceed.Chart.Server.StringHolderCount Count="121"></Xceed.Chart.Server.StringHolderCount>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED3D077C1455FA994D76C9A690D03B84AE821008100B288180705272846A8B93EC242C6E89BB9B40384F51B163EF0D51B1D753CF729EA77736ACD8DB8162011541383D4D13F8BFEFCDBC996F66DEDB59CED5BFF7BBD9FCF6CBCC7BDF2B5F7D6FBEF7E66D86949191B18F7CE03F7CF23C042C585CA32881515397CAB1C4A8A9D1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9832B268A1128B07A3914925A38A478D292E1E553CB2686A4328D1105326459486444C0E8D2CAA68A80E056B8E569AE6474F522293AAE5434A6A6B4B261C525D3A7E4249CD84BC4C52F5125C7565428E04E458202DD57B818481D69EAB9753A391442C1A9A22C7950E042B3B5C4593E3FE70D52CA54E8904E22469965CAD84E2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;79E1AA4572428985E5D849703345AE39A92E166D880472C255954A22118CD4C57B85AB664608925C93083606134D73EBC975821010CF523F45A27E84424A0D207AB89D553B63C63A90C731F596F6D8C006FE1EC0C5D62932230FE0221B14034E2F5B271913A07313B8350878636E1C8AFBB3007801F8008070FCD900FC04E478&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;21DB91953904A963B82A52060D2A34D317AE9ADF54AF740857C5E7C861A5F73CA53A48BA561D52CA6231B96956309E18A1A2F4E167D162DD0021602DD1DD964A910B8D94AA604209C70B50423CB852E98CEE1B5575CFC894E08FD09F3DA0B2294E4A8D5A10212CAB54624139145C49D936231A0A28B19F9B9F9DEDD12CDC0FFC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CA31C15C02254D245E6F4A9A09F2F92FE1B2CEE45F87C7FE3CCA530CF3317F7DFB65D32E9F797C06FFE1EF48798B6181C663C8F676D81F6FE8B259C8E642CA5A0C3B699E0454DA9B9DE23842D83A3D46482643EAF4602834ADB69670BEC0328E92917388435DB4924CA10D2144BD15C01EE43C50656A9AE3EF0CA00B80AE40A1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3FE92808F46706EAAB33E363A399CAB244E6B2449C7C03998A2C67D6C463DE58A83A5A9FC158962B65E74A92A439E355476678B40B929805DED8492AD0C3AC723921FB2906E8555E593CAE84AB434DA052528694EDEB4690BAF3C74D18527DDD7FB1495E218C25A026FE1E04E41748D403429B3D09E8211857E834003A5598CF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8AF7568B7764C5FB08F5CC709B94B6BEBFE82CB3B08075B01F80FED0CB8E5221EBE50002FA27773A9456E8646127ADAAFCC2BC81E4DF7C4DF2E5317939D12CA3BB634877C717A7D8DFE292DAF1B5A5B563C604C617CB25B2179439259B1A4C103B9169D4CCB05CA7D014D0AD4E78064CFC653496A71932BBA99E21C72B97CA81&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E8727213D10BE6939BB90D84E6CA445388DECD8C44D0DD14A551092D0A06124B0BD95D39717372A446E94ADAC4F7AC255AC1AC60DDD204B9A175D39B5C2DA79C303757CB806BE271684932B58FD086C8043E5221274876C40F5D25861F8DF5A33DAE8845C934351154E2234CFDE6E51A74F0720DBA8A44B98CAE2176043BDD7D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED48061F3899065FFA084A026FFA080A425E0F7B1EED494F0E3D94D0DE9C0C8DCFBD38592ADFBBCF0A46388DF4B025AB6DF4B2A76B4DF4B4E7A82D90C12403BE19EA2319F84DD30DF90EE68F0A260B0087DBD56C9AA368570726CB900E48C5E658FD82011494812A5AAA38D9D9FBDFD35F23233B3BFB7F80C46479744AFAF1BE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7DFB5251399858756894430D4A5555062D0A1FEF66529CDB06A8B297E2E79C14892E8FD0442F712209252B42EA047DF477E83030C3F8AC27B3B71C6913A9116A35A77AFF499252D65F41673F227538C9935F54FA9014FD48EB158B4B491F58BAEAA35D7DDF92EAA5A9EF5952FD34F55D4B6A079AFA0EB7DEB72DA983213583C6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CAD43E496FE14EBEC9EBF11BDC9A37727BFC3AB7C7AF717BFC2AB7DE571C7BFCB213465E29018BB0CC8E9A95B689A1B7CB7ECDD9613248C6EBD9306B225360F2A0305F59010DCE96EB89A7EF68F87FD04198421C159303412592A09AD519252C94490D9104C189AB66160C293053CED6869C31EC626CBF705559A01186DE002B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5CAEC46B62C17A987F13B429C14458AE27130BDA6A37A3BB238CAEF636A59A7ADDCB948509E863CE31D1D28F9FA791652E69A6B08B294F25969336F600535A1216980BABDCE86CEE1E50438758294BFFA897FDEC9A45FE31B6955A1FD2740C130B4BAD136C0D0DF1B2D41A34643561A60A9B3373B7F43F1A0B8AB97424E12B34&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D3CD529BCADD81C2598AC176368B7019FC0B33980EE21B880B4DCE6978AEF3C8C51E798C471EEB914B3C81624F608C2730D61328F1C48B3DF1319EF8584FBCC4A3147B94311E65AC4721E94B33AC9F5CEBA794050DF6EB8B3F70BFBBCC7F2810F20221C441D2E601BA940D59CF93928E0A2028FB1C299B825EF04BFB26D191F5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1FDC69CBDF79E35B4E8EF719E1ACCB2C61C16CE469527C03AD364D8C979E22B5BDC0660B7F2517CFB39B27C9C573DA8D1F68959EE092FA389F54E93192F68C96EEED9A5AD8CBA7863EE30BE6CD22036E7C7E341A4A04EB7B93CBA90DF144344CDC42594C91CB128958B09A3C23C6E1E97D6A432C1E8D01D7C8485B3DB75E214F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C57394E58B829140743919AC230B22C1931B94390DE16A2506A12D61F046AF08182EA921C74926E07D543895344AF38597510CE57B88A36179EA6A20C4D99438E142D90A25DE015600437425903E47CF8E069410412A295F185496131E552F0CC683D5213D524CF26AE943AA9F5CCC50A00C24952BF589A5301121CFF3F521B9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;696E440D76750F5755C49480524B9E5B03B417D45ED4E7523A7BCC1AC80937420FCD2B83036C58652B82169C217CBA81C02495E90C3618906975FAE6F89BCA1C2A43E2A8B287DA9AE591EC61CB80470398056036803900E6425DBE0A10202D9191B1664A46C67153F489F303442D526BC7AC1C6C21CCDB3B494412142397C86E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5E74F95432338520133C56D1EB2EE47A4634165C198D24E4D06C3956178C40E46C2144206A580ACC1281739D4DD865A1605DA41021D3046B1C9CA8CF0C4586187CB86A7A344A72F2D84A35847E73D5A01C51AA8842F545FB90E7CCEC3E0282A02B1EC1986CE99E687C3775B934F5A0BAA84B30EFF0FC8C7C6085471870D539C4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;169B73B46F35C4F341FEF713054AC62EBBDAD0158FFB042398858BF6118C2EFFDD2B183B4DCCB5978514FF02000B012C02B018C012A8B45F0AC17158D7ED020ECAAEB9B536CD8D03EF73D5C71788ADC5AD1A4A7A9495959BDB5FDC2814DDCF95978C8C478979CF98E23B069E462BCA164D2D9A54B478F1E270D87F2CD07A1C30&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0242EEFD74E4E3A6F88E075325C83AEE09805B057C3934B5392FC44F0A8D94597253B42191BF584BA9248C51F297E03B78FC64F84451A07C5F4E337A5DA574364705E95D47142029327FEAC3A615D21052C7D1DC357AEB1891AB46F4D5E4F9D1106138991E772352A5D70925A06591F1F3BF64F1332B4B5FFE2C12D0AF13CA77&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1D06C99E8C5F6B4B80F726ABD1F3FA2B18A5D672BD944106BF98BF862ED56218C0CBB6B352983F80B384A10A526706E8D0A7C4DCAD357C19D3AD4B0A653486B55A162C1B7A67A73A2173B7230877D7D451B662B814B3784E0A535940F628114FA8D1F4941E8AABF69125C11436F720712D95D186185D9E43DBD9E8D437E3BBDD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F547B02F786B7F103A353795B9B3A46D4C247288C9597DB8A3969A59CA362A2C83366152D0D5FED4245D41FCC6A3EC19F13B329FF52E74986B65A9DB3478633F79BCAB0D9111435FB0D5EED5A71D78BC4B6126A049907631ACADC0C15782D9CC423D2B8AB2BC8B1DE67FB05F001E48A7ADA897D5591BCCD4C93CE6A44AA2AFC9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;66F01A05539590BA5CDBD59CA65296AF0DF4736B6BE38A8929845133C144C8D39C31A382A5C838CD26B5B1499591D88FAEB286C83DAF481F3DD75E324B534DFA11EDCE3378E051179DC82705B11C9884BD40A2F93971B0C02CF01A6CE6FF231E1D6F2F217AEFCC25FEC8E9438F0A59E8EB3F19400C00380A7F02400380466872&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;89F38348AEBAA6C1C207A6BD136053EA95AA6A8BAD094B60CE392D50A75428C4F74460EB54ADC5286B4D4609DB1EE8D3AF69C715791A556B35923A691B3A0C3E6618DA2649448972730727A7CC6105D7BC152B151CFE8E154B370DB7EBA50B69AA1FD4E303E709E37EE67EF34347FE15009A00AC84DA8ECD70F4715DD4517B3A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B16B936C4172736310DCA4C31951815A4DC8C6E512909F55014C094B884BA9A6BE888CBC21B95E5DEAE984DD0F8B25989E2571B080F4AE769E1280E8D45131458127BDDA29847048280BD52F95F3F446095A47FD8622E7EBB750C4C8A405B30C9D6102E86289B7035FE8D6886C185EA5740622528800B0183A1D38FF9061FF4C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;86AF24AD265A731F8BC79E492EEE65F1D8536D456890D7F4958E138CC6ABF0683C9FF816099E52E953EDE9E2CE9C8A3BF347DC99D5A974A64AD09995B8330BA133356C03DB39491E9AE0F50963A75E40DFBD2629ACF479B0CCC27DAA30CAD5B20D7417005803E042001701B818C025002E85AAFD521DABFA32027A8BE7CE46FD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4B59FD5700B812C05500AE06700D806B015C47EBF7C2FC6C3F677AFFC92C199E43D4D8F6F5DA13187D0A5B66E5B665A657A046216BE74555D701E6392DA434D21B30DE63A2D1300DE09009A4126368855A0AB1BA85D150431836DCD5CE526A136A7C074C771E9D0DD35BA8747EB45EBD8161654A3441E3FE70DF559D4D91ABE9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C1446502A206754D10EC246E7899CA13789620C5AA17C495DF0561E988340B81263250C5EB15FA524659A42EA440747C310D8AC3D5127AA56BAFEA36F495AE21DC0089A51BA5D66981CE3D73E74AB5A874A9C95E366891E4EE53899329CBC85847EED711BBFBDB146F80D8464A3DE0C7E9BC35D69147D0317E7129E7FDC113F5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0DC361E626D68ADD848CDDC489D84DDC9C8A9B88B2266E1137713C6EE238DCC46DA93471326BE27671134BB426280F16E326EE4AA1096F6CBFA6B2EE13B6F009FB6EFA548DE13D1AD7E9FA773CC5C9B8F63499C216DC2C877D75748E379FB7E429E1FD4D302187CE49958EA80D0C759E236A2343FDBD13AA77450A93DBDCFF82&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7D4DEE9E9D5F774B895491CE2D067F821AE7E02D06B3F1168359B62D06BFE36E319829D86230036D3190E0F108B45E3A4AA7C0F8BCB461C611F6FB1428F80BD4380D53508E29986AA3A08C4BC164010547E24D122B537DD8ECA4059AFE1BDE69F835F6D65B76D2BBDBBB33A423F02EE049BC2DC113B95B770FE76E093E8CBB25&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F850EE96E043B8F5963A0E6E131CC7B43F081EE4A9DC61B4C98238AB57DDC102130B89E8456E4F4E098A02ECEA63C924BEB57E69B0260EA5211FBEBE57A0F1321864E081CB3B8E745258A9791ACD76B97B4B60E12C495BF662548AF098BF4493038E2A3D3319BED25852EB79CCFB6C04F0068037A1283CC903C37C6F997A2F1D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4CF0C7315D18492EA06BB4B1D5C91B1B811B7B0FC0FB003E80E5F10F0938065BD48244301404334DCF7EED0BAC3B4ACDA117F24C3F9F06F2D9FA6027D83D158CA34057AE9A426584E29EFA76299F3AF931C5276132128D91C928A94E8EC566CB910639A49ECC00CF98C46BD4D529312816A70132F55E8D90D190A99A300B9C4A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BC338D8912373A3F5873D22C255247F763D5526F6A4E9A1D8C44515257CDFDD214CD054248DF2889128DB22CB11BEAFB6C79999A6B4E6565800A7AA317860428A32774D1CA6969746F9A96C60AD2B46C58BA092642B0EA41FF03970A3419C0AE0588797745F7E5C1B012013DE98C12E73484C9DCAEA63B4A9A15AD2333ABC4D2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;70B0A60B2E4E6681F38334B8B010AC687A341696219A5C5B118D07410154B114E0FD6E24AB33ED3B54C3F06CBB4BC83478AA02090B15186D4809BDAF713549D3AD790D212596CFF48CCCCC14E09FBA8C1D89D3C594B8469DBA9B5155A58E90304257DDEEDAAD457F0B513255E2022D816972BE76AFAA73AE76076CEFA45D1B8A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CD7055ED662D5A54BC2B6B00EB7937D68A49D959AA59E37B305C8BDAEBE916DDD7D32D06D04B4BB75B01CBB19B02CBB1DB436F2BB58651D8B374CBE886EB336C8BA59A6CA427AE061B0ACE30590B93876A324CD4BADD74C5B267C6D3CB9AA86B650F6B8E66467DACE9C8967ADA6AD30CAAB39681AC8AE98BC5B4582F4DF6D5C3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;20D964645D7599224BEBA22562736315586D0EDB0335BCCE2603A1D6D70D25192688D983ED507F23132F6FA277478C94D431B38BB8016864D71E6E2859B772BAD12F959D6DA92C64A5BA6C39843B928F3247913EA43D83E593FFAFD5D594BB9932E26FBF872968427FBE3631BF615B3737A3E876C63F14CAEA53006BA8180B79&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;18DB7E5F4BBB9ABFB155687003B92038ED02D8913B9C5BA3C9050599991D60C39D837D92796BC17FB29D588FD4A82EAA34857CBEE953879684B5D4BD99FB7B8060FF1CF17AA0974E1BF101D972DA0FDDB7E1FA2FD77FB9FECBF55FFFA5FE4BDFB83C8CBBE9D9E2CB049B9E8792B2495C1CBF14497DE648FF26884A6C06F03180&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4FD4BCFAC91ED3CB931947FAB740F6A7003E03F039802F006C05B00DC09700BE8288CED7B06E4397AC8B0E08870FF46F875E76B3BE4169371DFE3B24FE1D007602F816C02E00BB19F105A4DA549554B0FBFB3B00DF436579D6EDE3168DA261A8155293B4525D7F5717C9F3FE619C58452AC8A331A8678E54DFBF224CFE11EA6F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;06D002A09521FB08E23016E9F2C20B5A4608F497928D1F64430522FDB077DFBE6EDA1272521E4BBB0866018BACE9DC9276EC65C4220E7CB337350E48B099E502B68ABF6D2F62C5D6BD062B3268F9BE129C2105A03F800192881545903D10C0200083010C013014C03000C3011C20B13D3E07912BE95DCC8A9190773080510046&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0328D6A2D2D2468D1574F1772C64954005AF1AAC60EFDCAD925EE1B1620294290570088043258D032F600E3CFFAB73E0CF2973E0013E07EEE571E09E1439E0BD907B76E91CB4C5CD0DA1BA21543784EA8650DD10AA1B427543A86E08C20D41B821083704E186505DFFE5FA2FD77FFD8F8750A90BBA5D8BA0D070CA6D7BADE1B4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E320D8703C801300540923282742B60CA01A400D800000884FFA6B01D401584A802F4840577A5C79D101438B1AE90B4484F49A03FDCB202A72250EAB84A05418004422FD5100F52CAC72B11656A1EF66C5202B0E15ACD12328F0D95446712FD013D74C51432B84BE4628032FC6FB570080D76D285BCEC66C39EBD7618B1FD8A2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;72A021650E44F91C08F13870528A1C90E045B90BD96EC55ACC0AC51663BB014ADD08602D809B84AC5807D93703B805C0AD00D603B80DC0ED00EE0070A71E63BB1BFAB24063050DA3DE0B79F701B81FC003001E64AC98ABB182FED8CF4390F5305430CBCE8A55D2D13C563C0A651E03F0388027248D03D33107A6FDEA1C189F32&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0746F3393092C781112972C07B31F77DCD3974A5C20D30BA014637C0E80618DD00A31B6074038CEE03BAFB80EE3EA0BB0FE86E80D1F55FAEFF72FDD7FF7A80919EBC384C0B9E507F34D4163CD9047186CD003E06F0893078B205B23F05F01980CF017C01602B806D00BE04F0951E3CD90EB18F6E3878B203F27602F816C02E00&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BB59F0A4400B9ED09F89FE0EB2BE870AF24C5BB436FD5DDDB168DAA2F50C3D64C0FF23946906D002A055D238E0C31CF0FEEA1CF8614FAA1CD8B587CB811D7B381CF8664F6A1CF05EC2D5D57932B10A509DD5EA6925B58B494AB0210E974BD44B38E0497D3E28571272CD52E25D126AC0632A9C8C148D35A9B9DDC88372594322&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6A4ECD63BFB60ECECD0D4FB9E1A9FFBEF05477FC26BC11A6EA6F49B6E8734F4E3655EE1E960CA6E9DD2CE9AADA77B1A482987A59D20C83B0D6A15A87B5A71653E96BED10B69B7ED65E998CC89A6BB6A801D6B216F3B2E55B6CCD966F31BC81967CBB155A31EC2669C5B0DBE72011F70C6315A3E896DB8FD78EDE862DD764D345&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BCEAB181F3104CD66ED50BD5F4AD2AAAFB81BE3CDD654E61A02853B7BA01220CCD5D0C16E523DF51246C457324BD2D08C8AB58F5DDE262ACD499FCCD003B2B4DCEA7AF4DE79027EA63C9C46EC95AB1D547F1FC057558BDB98E847AAF7E9C2CC395F1C4E086DDDDB0BB1B76FF8D85DDE1F924EB9708BEA78E693D883569CC2895&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1F0070C3666ED8CC0D9BB9613337ECEFFA2FD77FB9FECBF55FAEFF72FD97EBBF5CFFE5FAAFFD385A46FDAC9BECEF4D4F6CE8E3518FCE95BED8830E9CF87C8F752DAFBF078E9900500460A047B4963708B207031802602880610086033800C081000EF2B0B5BC91E44A7A7B0F7A2F6014E48D06500C600C80B11A01D26B7BD0E9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;23E3206B3C54F0F21EBC155CC37D8993E83F04CA1C0AE0300087330E3C8739F0ECAFCE818753E6C07D7C0EDCCDE3C05DFBC381F59803B7FEEA1CB83C650E5CC8E7C0F93C0E9C972A072EE51A714534A4AEE79E2D5CCF855F089AA2D40523F4A77AE02781E8456542A9372DD6F2D67D0BC255AAB5C65577D88515402ED25DE475&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1779DD455E7791D75DE4751779DD455E7791D75DE4751779DD455EBAFE9A21FDFF2FF3A612D3CB4D3D10E2061ADD40A31B6874038DEE4289EBBF5CFFE5FA2FD77FB9FECBF55FAEFF72FD97EBBFF67FA197AED095F94FF1A8ABBEBE3F928BEC8901A52EA62847F84FF5C0EFDDC2AFAFE295BF55B695BFD36191EC0C006702582D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5CF93B0BB2CF06700E8073019C07E07C0017005803E0427DE5EF6258B8ABC72B7F9742DE65002E077005802BD9CADF32BCF27735645D0315D4F156FE6A792B7FD743991B00DC08602DE3808C3970E2AFCE818A9439F03B3E078EE27160FAFE70A00C7360F2AFCE81E2943970109F03C3791C18962207BC57908C5E361B9BA5D4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2620E4D7197EC7265C05CE9DB7CACA5F5EB5AEB8E98B5250E308B5B61E9634DBB29696AEB6D3CB92CA59C2D272D41EF4B5A49ABA5348E9447DE98A1358473AE344B517DD7192D10513A6DA7E2F9C646A5C8FAFE9D03E06EA85A173EAAC2A9D53A614870BB75FFBD72F68D83B700FEFD79D4C1DE4FFFA12F94C9EECDF0266F929&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;80CFC0B2FB91DAA04621422F0704EF955CEB9E22D79CE45AB76BDD6EBFF6CBBAA56E9AB949BAB9ED004BDB09E05BB0C74E4E08F90E08DEABF8876B04EB96BAE3B16BB16EBFF6D362FD9ABD79747B6B01536B05D00606E97542901C10BC57732D767A2C1A712DD6B558B75FFB69B17B7E52ED2D53B7B72C72E9F702F01120B53A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;21FCE080E0BD867BFE39BD9C1E8A466379E930D9DF8C8DB85AF81F68E1BF341DCAD275A810D4A71380CEA0643B1D10BCD78A4F4903125D1DFB9FD7B1AF3515F2EA2AD413B4A71780DEA0635B1D10BCD789DFDC7075CCD531D2F0A79A0AF974152A02ED19086010E8D8660784C2EB496A21048F8793BBFC02692DB9FE0328D601&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A08165B1A01C220865D2FBA4A2712C8AFE1EB929D9A70EC1D2CD042C517FA55CFB76D5DA82D0B9F42EC13D8F45B54742BB07031805BDBB853536DADCD81BB8B18DB8B1DB9237F63A6EAC04DA1907603C34763B6B6C82B9B19770631B706377256FEC45DCD861D0CEE10026426377C35B55D0D8A44CCB82B8167FAF5322817239&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;21CF4C2861FA5340DD4110F768D2C82F94FE44FE1D0ABDF83B69669D1EEA571724A42124E32F0CE31911C64672BB02309E26181B28065BD048F58B3F70BFBBCC3F05087C8AD4F802E3DA5FC9CDF3ECE64972F31C630BAC83484F9094CD68B1627D869491233D6E491D0CA93939D26324FD1956D91B8C8247750A8CCF4B1B661C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;61BF4F8182A381824730050F630A1EB251F0209782070414DC8F2978938095F4F54692FA114BBD17DFB0459D7B2CF5F9682B775B52BD34F52E4BAA9FA6DE6949ED4053EFE0D67B3BAFF7D01748527F5DCC11E33D269EF5E954B0E3A96FC0E2B9198B679D4D3C6BB9E2B951209E1BB078DE67145C9F4E05AB050AAEC5145C8329&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B8DA46C1955C0AAE10507039A6E003A66097619DBA94A760977015E162AE825DC455B00BB90AB6865BEF058EEA73BE23C626028E85D69767DAD6FF3326C35792CE25B5DCC78E6B3E87DCDCCB58BBD256485DCBC55F6933D380B34C3A8C4BF0EE1D34E014D08033B1069C8135E0749B069CC6D58053051AF047AC011F1300EF0D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;48A738F1D3FB49AA3B7CBCC6E4B243B82A3E470E2B857AE688AA2019B6E20528211E5CA97446F78D4A0CB6EE644AA4A201954D71823F6A4184CC5D2A151879832BE996B319D11099206667C3061AFF598465BEB309E8ACBEF652043B418AD4B722FCE720514A5B18C12B1C15E85386BADC11F53386DAE888FA3901C04AFF05D0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E735A8CFF4559B22FAB28EFF42DCE72F4C452EC645E0251CADC825B8C856D6A1A8A543BFB375681B438D68A89B376FD6D03D16D42F59472E276DE5F8AFC8D4D2E1FB9539EF2ADC9BEDCC14AF86D46368E2EEBF9B4D71A9668AB4409D668AF44086EB523045EF8E14378475800D61E1AAC89C867035BCFC0513A98A683092884F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5112CB1525A2BE87990C4355AB22F288531E8CD787E4267E1DF9DA59E52A7A2752A3FE32D6D4684324D143EFDC0813661F9C6E29A3D19F9D2D49D9125D21D6F92FE9EEDABB33A54D6F7D081679D0AB5E1057E6C68275C1883F5C1550AF48D703EA8B45F0C67E81F67A18F402F6DE15AA07B3EBF9401BEDBFF1B2590E4B9157E4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;189980378F9011981FA575C92BAC29C1489EDE34C94637C1480170435EA1F3C289BF453AC174CBDD085B277B0910E415BD844539959A2972400846FAD8100C7AC579C1487F5B9E891B7DF95DD6749553361DBAC8DE06B37E249FCFBEE7CFA44368E365EAFB74E1E3F311C54757BA11A0CFEE23BD0A7120A975C1BEE9826EB7BA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;21536BD04A5B0E6BCF21975AE2B7296F2CED4D30398616D051A6C871C56C97F06308D356D447234A246168B56B8CFF8BC6283044F23930B91558F636FB7C92D8CCBC35C4AA52AF8F6F59EA67C86466AA69B0B25DA96DCA0613CB07CE452389A571ED2C1AC39C6CC39AD51C5DCBFADF1CE644A35C765ECA43D7B0E446835F1B70&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;18E59E091EF4128C4F6B211229137B4CB16ABE31FE7CDBDB9DF29B0E99AAF94D6D08251A62CACC486D343B5CA566C3914FAA06A8F799595291F6E07754285AAD3FF38D4285073A360A8CA74F87EB80575510CE762C6366D3875AB09C3E667BBFDB9FB72FFCF687E0DEF394EA602420578714E3695745E9C3CFA2C5BA99D5492B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D1DD96FA331EB4E12F8587ED9F9B9F4DF5CE7F2B7D40C4707DA6FAEBEBF427B4BED782E5EA46781D48C710013ECA9E644FCA2702F931F9AB2720823EC49EC9034E8C09A83241F02A94580D9933F5B2E44D8B04B49C5CE21D6646087B124A8038E70A993C13D58135919B08B953A867205801B8A9246CED048A1D8B91B29002A8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5D082A4A9A1909282BBA504FA7A7D113D254B2B3E8439DFD3D3B4A88D13EE51F676E4DD12C1D5071D92EE200FD41B22584878E8DF0B7217B4D5E60C864EF62EE149BD713E1BE66D3C7DB9CF2AB42AE71098DEB2E6A5018DEAD854F6810AF657F5EB272D92C64F33D94B518DE8BD9DC9AEAEB692E8B852CBE8FB215C3FB118BA5&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BE9216537C401CDEAFD0628AB4C05C1CDE7F2895F07E3F490BEFCF4E6778FF11882C1C8DC3FBBFC3E1FD99B6F0FE51DCF0FE7441787F1A0EEFF797B4D86EB9635C7A80A4C56F2196EE8758BAFF691CC32D62554D76AC6A20433DD2117510433DC21175B084A2E07E089CFB9FC71D1C62CA8728B97F03CE1FCA9A2A750C890F63&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A8132CA8E36CA8C32553D8FB55DCE201E6BCD771DE414C7D37426A473A99DE5D6656DF621C121F8D43E26FA5A2BE2349A53BF82162E960F26F272F72261D4CDA81E8198D82BD938E2898348AFC8348983482D45BF393FE8E439AE300D268F26F97F541E93D20E140D2A8AC35FCF3DB2926FFE0B9C7FF01D43D9CD40B0F17FA83&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;823496E47FC79FE96EC233DD12893FD31D8467BAE17CD04382F9239AC9111B2338309BE34DCDA42292B1F827F382B4AE79A5A4AE66FE54E133EC600F21B0853FD67D81F10E25B095EFB0B761BCE398C67F2976D83DB1C6F7C01ABF3D158D3F9E39EC6EE974D83B40CA5DB0C3EE8C1D76279BC32EE03AEC8E02879D8F1DF609CC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F3E439FAC32ABBC36EC61EE6445655B663553243EDE0885ACD507D8EA8357687BD0777306077D81928D42229ACA97DED4E0EBB96A1EE6D775AC3AC333B6508EEE80E72A939CF877BB38CA96F07733888AE636AEADBD28ED4B7B91DCD3772B25250DF90C5616310163AEC1FDA91C3CECBA27E3EA983CB70747011E6B0BF6FFF45&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1D7694E7B03B0209FF6A4FA7C3AE670EBB10EADED56E75D831B1C3EE9A851C765CE0B0B7B723871D0587DD6873D85FB72771D85FB52771D8CBC50EBB771672B02BC40EBB2FC66B123BECFE18EF06A6F103B2840E7B0BD6F84FDA91C31E948AC6DFC81CF6E6F6343AEC2120E57FB62387FD513B72D81FB65B1DF6FBED3C87FD5E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3BDF61BFDB8E1CF65AE679DE6977F28737D91DF668EC61D6B1AADE70ACEA6686BAD111F51686FABA23EAAD76873D1E7770BDDD611F82F36F634DBDE8E8B06F67A82FB43BCDB0EF303BE589B8C53BCD7947E0BCBB99FA1E2976D8CF60F57D1A3BEC29A9A8EFBD4966D8F7091DF653D8619767A5C3C1DDCF1CF693BFACC37E80E7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B0A703094FA4D5613FC81CF60CA8FB319BC37E48ECB0676187FDB0C0613F841DF6C9E0B01FB539EC3F2573D80F2673D88F891DF63CEC601F173BECF918EF09B1C35E88F136318D5F2476D877B6A390C81D9AC6534D3C26158DDFCC1CF66DE974D8C781946FC50EFB16ECB06FB639EC9BB80E7BADC061DF881DF6C7CCF3DCE0E8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0F3FB13BEC3AEC61B6B0AAAE71ACEA53867AB523EA670CF52A47D4CFED0E3B8C3BF885DD61D7E3FCADACA98B1D1DF636867A91A3C3FED2EC94F1DE04E92B735E23CEDBCED477B991BACE12123917ABEF39587D57A6A2BE3B9238EC9D42877D1676D8A7A4C5617FCB1CF699BFACC3DEC573D8A70209A7A7D561EF660E7B15D47D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9ACD617F2776D8ABD5E52FF5F796BE1738EC95D861C7C161FF6873D84DC91CF68A640EBB59ECB0CFC70EB645ECB0D760BC56B1C3BE08E3C10F8EE90EC67709C9EA4B5F032D9A2A2794BA68ACA9485D15673B992FC5E602BF5346CDE532B1B70FE3F94D08CF6FAE4CC55CFAB326AE1237116C475BE897E226AE4DA509F8F12C3A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A0D4A67340B91EB4308007941A3CA054DB069413B9034A95604039010F28F0CB5FD4331EEFE8AF077A6C03CAED58A48358554B1CAB1ACC50173BA20E61A88B1C51877A6C03CABDB883C33CB601E5019C3F9C3555E138A01CC050E73A0E28077A4C83C6C3B8C583CC797FC6792399FA3E9A25DC763E135BC80CFC00FB442AEA0B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BFC9261A50E0A7DAF803CA743CA03C999601057E128E0E28E5BFEC8002BF3A671B509E0212A6A47540811FB6A303CAD350F764DB80023F712718509EC54F00F02B78BC01E5703CA034C080023F80671E500E4B36A01C9A6C4081DFD1130C282FE101007E6A4F30A0BC82F1E0D7F80403CA6B18EF14A6F1AF8B1DF618ACF1C5D8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;61BF998AC69FCA04F31608E6609B604E675D7857DC8583F0987120EEC207A974E10C36660C4FE798F111D033148F1943F09831D836660CE48E1945823163001E33CE64CEAFBFA34B5E6D1F33B662277716ABAAB763556733D45E8EA8E730D49E8EA8E7DAC78C6F7007CFB38F19DFE2FCF359539D1CC78C0B186AA1E398B1C63C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2E7C875BBCD09CF76F9C773153DF1FC463460EB6203F1E335A5251DF4B938C199709C78C0E78CC684BCB9871391B33BCBFEC9871056FCCF80948C84CEB987125734D7B297B6CAEE96AF198E1F1A231E31AC198F1531B1A3396C39871BD6DCC686F4B3266B4B52519336E108F19D95EE4E36F148F1939186FAD78CCC8C3785B98&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;37FDBE2DD517B16D0A6EF7A61DBDB08AD386BCE9EE36E44D77B559BDE9CE369E37DDD1C6F7A6DFB4216FFA29730BDBDB9CDCC26702D17E8D45BB0A44BB83B1E5AB74B2A517B0651B66CB56CC962F6C6CF98CCB964F056CD982D9B293B1E51347B67C2B60CBC7982D67005B5A185B36A7932D43802DFFC46CF908B3E5431B5BDE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E7B2E53D015BDEC56C69656C79C7912D6D02B6BC8DD9B21AD802E78851B6BC954EB68C06B6BC81D9B211B3E5751B5B5EE5B2E515015B5EC66C8143D0285B5E72640B1C95C663CB06CC96B3812D858C2D2FA6932D87015B9EC76C790EB3E5591B5BFECE65CB3302B63C8DD9D289B1E56F8E6CE92C60CB53982DE7025B7A32B6FC&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;359D6C99066CF90B66CB13982D8FDBD8F228972D7F16B0E511CC965E8C2D0F3BB2A5B7802D0F61B69C0F6C29626CF9533AD952016C7900B3E57ECC96FB6C6CB987CB96BB056CB90BB3652063CB9D3CB6784C3B1F056CB903B3650DBCE600C72B15094E80AA5B9AA88C36C46A945EB0D5255EE3A9913D21D973B2EC892B1EB9D8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;238FF1C8633D72892750EC098CF104C67A02259E78B1273EC6131FEB899764D6C78BC9770CF98E25DF92CC40AC987CC790EF58F22D11BE56982BFAA83B7CC64DB5CAE3F455AB8E60DFD464B9614A46467752CF5553EDF910DD61DA727B5B1A0F903916B4653DD6965BB1B6DC62D396755C6DB949A02D6BB1B61CCC28B8B12D8D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;07C8048082EB3105D7610AAEB55170359782AB04145C892980A3B8E8013257B4A133632E6FE31C2073591BEFA0974BDB7807C85CD2C63B40E6E236DE01321771EBBDB0CDE9B1758D23460913CF05E954B04610CF79583CE762F19C6313CF595CF1AC1688E74C2C9E718C8233D2A960A70105AB3005A7610A4EB551700A97823F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;082858892918CF14AC09EBD40A9E822DE72A422357C11AB80A96E02A589C5B6FCC517D4E76C4388C89A73E9D0A7629882782C513C6E209D9C4B38C2B9EA0403C4BB1780E6714D4A553C1AE030A144C4100535063A340E65270A280822A4CC144A66027609D3A9EA760C77115E158AE821DC355B0255C055BCCAD7791A3FA2C74&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C480E3F6E8317F0BDA04C7FC1DCD30E68B308E671895228C5A86314F84012770D1F3131FF09ACE4FAC6843E727CE6D43E727C2F15B49CE4F9CD386CE4F7C9864FB1F01F067509C5358777E27EA0E4452E9098B8F11FC7CED6D2478C73042B777F81E27C973C2F19A682C14AC1E59B4507DFF69D29851C5A3C61717178F2A1E59&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A4BD883C29A234246272686451454375285873B4D2343F7A921299545D5A2A8FAF193F61CCA125E394E2430E2D8443A8B4E3192F60CD3F819A2F8F920A147A9C23345F78A18E5F788951F47256F449AFE3F1A2C6D190576815F89F82060B0AAF322ABC9A49E66F66C99461C94CC692B92EB9648EC492F90708E55900CF8164E0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4C15E31CAB17BC30BD53D7B767C6624A5D43488E15D19317FC2F7A5178742D2BB681A4FA5E42C5E0C5F6A2CA7A39A2157B1915CB82B7BE1DDF22CF51DFBF8BC861380837A09DAAD9295CD51057E0DDE0B98D4A2C160C28FE1AAD4C795EB82A189FA7C881B99150536E4D345C2F6B6FB327941509B8E8106908C3FFBC80F6EE3D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CD85D30560D64EDD49267CE0C57E41078D5AFB7111E66B4D0DE5E6AA2757A92FB403D6702E163B19C0C01BC6C53B2A065B11827264AA4681EF15D015257270D9821134020EA709FB5F0531BF06E075001B01BC01F2B895E9EC9B5C9D35BD3F6FE8EC7AA6B36F4345EF007817B4375FBA8BD5F79E977758BBF1BEB051D9DD86C2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DFC34A7FE015FDC8A4FE26AC51C1BD4605F7B10A3EF28ADE3EA7BB358CC2F71B851F60E6B6C96C6E83B1B90DC2E6F65072731B88CD6D0B70E953009F01E31F618EB09FC011163E6D3899E78DCB0DC6E5ABC6E5EB06151B19155F98A9E883A9E88DA9782B3915BD30155F01015F03D80E54BC839D861F9C867F27F60EEF61EFE0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;07EFE0DF8DF3E11D2D70057ED05AAEBEFE1BC00F4C5F0B3731CD6B86A41600AD54F30A3F33B8F18571B9CD60CC978C316D66C6783163B23063B627674C2666CC5EE80A1CF9ED875F27947630F1EE6B1588B7D9E8E41EE312D638B4CB2CEDD2EFF1D181C1A76749F0960B2525D36722E5A756444A7B2B22055E7149424A5B2B22&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A503A9D49F0DC00FA4C05B2B1619E7F9900C3B66D9645C80F3E1B58EE432EE026D75F531197765847787A41E007AFAA88C7B1BDCE96B5CF63718338031A6979931DB3063B662C60C4ACE982F3063FA4157FA03180004C29B0C54C65B44321E6D7472BC7179887139D1B83CC2A0E24846C54033151F632A36632AA624A76213A6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;622810300CC070A0A2DC2EDE83B0F8A6DBC57B30CE9FE128DE31D0D6585DBCB39878C741D278001354F1CE33B831DFB85C68306611634CA999311B30635EC48C392639635EC08C391CBA3211C02420F03826DE7F88C45B6774326C5CD61B9709E3B2D1A06239A3E2483315CF602A9EC654AC4C4EC5DF30155381807200D3808A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;53ECE29D81C577AA5DBC47E3FC558EE29D0B6D55E8E25DCDC43B0F922A01CC07B000649C5778BEC19235C6E545C6E5A506A32E638C5A6866D45D98517762465D999C517760461D03BD3A16C07140F055ACB1E3CD8DDD821BBB1937766DF2C6D6E1C66468A71A400D34763DD3AD1B44BA75BBC1917B8DCB078CCB878DCB3F1B2C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7B9451A198A9B80E53712DA6E289E4545C83A9080201CB009C04543C69D7AD08D69DA7ECBA7532CE7FDA51B71AA0AD465DB79E65BAB502929A00AC545DC74B06375E312E5F3318F33A63CC1FCC8C598D19732666CC9BC919730666CC69D09555004E0702DF7224EC2CC03D9B1126BDCB7A77AEB977CB71EF1A71EF3E48DEBB06&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DCBB35D0CE85002E82DE7DC494EF6491F26D3558F88D71F9AD71F99D71F96F83C73F302A2E315311C5544430152DC9A908632AAE0002AE04701550D16657BE6BB172FD6457BE1B70FE5E4719AD83B66ED6950F76A150E5BB1592D603B84D55BE6CAFCE8D1CE3324FBF9460BF05E5F831028E4BBD18C61211C61086B15884319A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;612C12611CC630168A30A6318C05228C0A86315F84712CC3A814610418C63C114623C3F8BD08E334865121C2B89461CC15615CC730E68830207CA59E32D39AC628F023A08847B7E253665AF12933ADB653665AB9A7CCB40A4E99694531D4471805E5AD698C023F05144CC11494610A26DB2838824BC12401051331051038A451&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E0C35B51E0F7B0564E14F8D0565EB4F690565E14B8B49517059ED0CA8B028FE7D63BAED57195CA1103A27EEC57EFA47F30598D4DA7B66D04591563598DC6B21A6593D548AEAC460864751096D5B38C8203D3A96D1F0005C33105C33005436D140CE652304840C1404CC1734CDB8AB0820DE0695B7FAE56F4E36A5B5FAEB6F5E1&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6A5B6F6EBDBD1C75A9A71346E18BDA98949D0BBCCD2F906070A401B3AF098B0BB41023041E21706B04BC5F66E5F2D816D1FC822C18291D43A47483E7F260A464ECACA933CB3B68915A385011865BF866C1609B3C920AC6911BAE8A940B43C0F9E12AB511395237B31C7A2A65EB837A16043D538BC60E002E4568E251B168433D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1C1F19EF5C438F4CAC6932923AD5AB87511A2979F5344CDAA85406EB227911A54E6637DDD5FACA959A60580E552A843572221AEB8A9B6189DDCD4DB1E49E2CD95A49479651D914AE8E863ACB917870AA29C91F9123EA5577D6434268901EEFA626B3BE9A937B68145A5BEC66A29CA5E66BA96AD98EE46E76301452D45B14BCEF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;62E24579B02E988877B310A7A6F660A9155AAF2BE444428945F4F4395AB7B5F46E6ACD96D4EE5ABF2C95B0640B765733CD6A47728C1505BBDE756B24BA1420FA5321C7E24A595CD5A71E965426910E1D3A48E68FC9DB65A30F64FABF8149E60E0070E5FB16DCE008DF2EF877B06F37FC1BE5FB17FC1BE9FB4EFDF7BD9AF86F1A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9FCAF1FD40FE67CE91E7F87E2417D94CC0BE66A8F26076EB879AFC508FAF0DCA0DF5B543B1CF563D3822433D62979D0A2A5936BF7BB4CDDF59B0A090F24206788424A6DC05D670CA955A99B80AB6AAE167AB42E585F093182C992EE4C8E172254ECC0CD430AF1EDDE4C3128FAEA105A168A40E7AA2C9BA30BE341A4BA0841E75&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4A4489114DB7A477D7D26799CBE727C04BEAB587E1DCF272B98969921CB25514EF42122DB5C4F5A5A78EB5C1583C416A985BBB48514E2A64E97003E7B676A96D08851837B5D294264833D184139A1439468F54D712BAC8D5D531A531485002A42D386E329E1DD02EBAA14C5A88A6E684F54B9D2AD4844E154E23164F5846CBC0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C9EDB4DA695A420F3D2152170AC697B2F45CEA259445D15820DE295A0FDA238798A4E3A695BDCE4424D49C8D5E760E2972FD1246AD5A612DD5B9E921B92EAEAAAB47F2689F4CD31DF97420F799F4975B525C68031B3D200595A7CDD3A1487DC0CCC9F1FD0456363A47FBD0A7CB7DDA87E4EE056B0C8C9E3D7B7413F9E4181F3F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FDB5C79C9CAC0C32CEA7DCB4F97C5F68230B1A4C914AEDE7A2124DF5708AB2EA7A134462F454EE82C4F228F59194EFF28ADEACD4081B6A4F3DCB5286EE481C915A6760E13C0ECE316B5FCBBE7DFB53C8CC04754A64FE78DF765E0085BE123DAC6C8AD440F2CC403651594A5EC7705535242F8D4523643200BEDC278177341D8C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;27C14A29B4633A65597A574B35BD9E246D6133E9BD2D697C16C8EC00A1911634936E6F4133E9B616EB4CBAA58537936E6EE1CFA47F6CC16F7E300A7E6849E3B3401E50F03DA6E03B4CC1BF6C14ECE252F0AD80829D9882CFD8B3C08E1634FDFFA685F32CB0BD853767FFBA85F72CF0550BEF59E0CB16DEB3C0366EBD5B5B9C9E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;05BE70C4F88A89E7F3742A587F10CFA7583C5BB0783EB1896733573C9B04E2F92716CFD78C828FD2A960C381820F3005EF630ADEB351F00E9782B70514BC8529D8CE14EC4DAC536FF0146C2357115EE72AD86B5C057B95AB60AF70EB7DD9517D5E72C228DC6979D82CDCCD798A9460819F6E4A314F76FD30D9F5C354D7989EFA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6192EB8729AE1F26B8397E98DEFA616AEB6F3623C23CD60FB3D85467B0126C31D8C822C16CA4F6C3482D18A7FD7B511E1A9DA58709636084868FD4ACF976F338D0C21D075AB9E3C05EA6E40FA5D34CE782923F8895FC01ACE4F7DB94FC5EAE92DF2350F2BBB192EF6314DC954E335D0C14DC8129B81D53709B8D825BB914DC22&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A0E0664C016CE9A066BA0E5BE64D3C335DCB35A71BB9667A03D74CAFE79AE975DC7AAF7534D36B1C31607B891E8184BD1F545657A753DB9681ACAEC4B2BA02CBEA729BAC2EE5CAEA1281AC2EC6B2CA66145C944E6D8B03056B300517600ACEB751702E97827304149C8D29F0336D3B0B2BD86A9EB69DC9D58A33B8DA763A57DB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5671B5ED346EBDA73AEAD21F1D0705D83A641A140A7C9C41017604FD060605D893949E41A10E0F0AB0AFE96DAFF595A91E5AAA292624F5F4F106857E4CC96BD369A6D7839207B092D76025AFB629F9895C25AF1228F90958C9FB330A8E4FA799AE070A8EC5141C83295862A36011978285020A16600A0630339D8F2DB39267A6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F3B8E6F47BAE995670CD742ED74CE770EB9DED68A6B31C318632F11C9D4E057B14C433138B670616CF5136F14CE38AA75C209EA9583CC3180553D2A9604F030593310547620A8EB05130914BC1E1020A0EC3140C670A7628D6A943780A56CA5584095C051BCF55B0715C052BE1D63BD6517DC6388E030759C7818379E3C098DF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C63830366DE3401F3C0E8CE38E03E3B9E3C004EE38703853F2DEE934D32F41C97B6225EF8195BCBB4DC9BB7295BC8B40C93B63259FC828E8944E33DD0D1414600A3A620AF26D14E47229C81150E0C7144C62669A8D2DB303CF4C7D5C73F272CD348B6BA6995C33F570EB951CCD34C311632A13CFBEE6342A98279BD4B8A71989&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E7A766249EF666AB785A9B79E26969E68BA7B91989A79C51F063731A152C1728F837A6E07B4CC177360A767329D825A0E05B4CC134A6603B9B914EED68E628D837CD3C45D8DECC53B0AF9B790AF655334FC1BEE4D6BBADD9497DB63A6114CEB08E0347F3C681B9BF8D71A0226DE3C0C666340ECCE38E0395DC7160BE8F17505A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E0E305948E61AAFF7A3A8DB70454FF55ACFAAF60D57FD9A6FA1BB8AAFFA240F55FC0AA7F2CA3E0F9741AEF44A0E0594CC13F30057FB751F0349782BF0928780A53701C33DEBF627B7D9267BC7FE11AD9135CE37D9C6BBC8F718DF7516EBD7F7634DE471C3164269E87D3A96073403C7FC2E279108BE7019B78EEE38AE75E8178&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EEC1E2A96614DC9D4E055B0414DC8929B8035370BB8D82F55C0A6E1550700BA6A08629D8CD58A7D6F114EC26AE22ACE52AD88D5C05BB81AB60D773EBBDCE517DAE75C40832F15C934E050B8278AEC2E2B9128BE70A9B782EE38AE75281782EC1E259C628B8389D0A16030A2EC414ACC1145C60A3E03C2E05E70A283807537012&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;53B0B3B14E9DC553B0D55C453893AB60677015EC74AE82ADE2D67B9AA3FA9CEA38FD8858A71F27F3A61F0DBF8DE94763DAA61F4BF1F46305774AD1C49D52ACE43E869EC694BC2E9D667A1D28B982953C8095BCC6A6E43257C94F1428791556F2558C8213D269A6B70205C7610A8EC5141C63A36031978245020A16620A4E6766&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BA005BE67C9E995672CD691ED74C7FCF35D30AAE99CEE5D63BC7D14C673B629CF5DB30C0B3D36680A5D800D730E59B904EF37905946F1C56BE12AC7C636DCA57CC55BED102E51B8595EF4246C1C1E9349F77808211988283300507DA2818CEA560988082A198828B98F90CC1163398673E83B86A3E906B3E455CF319C0359FFE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DC7AFB391A475F478C2B9878FAA453C1B681787A61F1F4C4E2E961134F37AE78BA0AC4D3058BE74A4641E7742AD82EA0A01053508029E868A3208F4B41AE80821C4CC1554CC1FC58A7B2790AD681AB083EAE8279B90A96C555B04C6EBD1E47F5911CA751D75AA75137F0A651EB7E1B5EFCE6B479F16F7E445EFC56EE346A3D77&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1A751B771A05AF05D2D711B7FF28781DF12986F1B5086323C3F84A84F101C3F8528051F08DF69E6C369052B0C37407A47BE8FB351A4B25D8524AEBDB266A318F616C1561F467185F883086338CCF45187319C667228CC50CE35311C63286B145841167189F8830AE67181F8B30D6338CCD228C4719C62611C6D30CE39F228C2F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;19C647228CDD0CE343110604EA29C607228C5C86F1BE08A38461BC27C298C830DE1561CC6118EF883016318CB745184186F1960823C630DE14615CC730DE1061DCCA30368A305E6118AF8B30DE6118AF8930B6318C574518BB18C62B028CDC0CB60D9E39C93C30E725BCB38C8D63F64A46158F1A93E2297BF22125B5B525130E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A92E1D3FA1A4668217DA99C03D2A796624A1C4E49A44B03198689A5B4FAE61BB7D7C6A3414526AE032577D0D01F6F2770857C5E1F58E838E9AE5586C845A64446AA8B4DADEF394EA20E9557548298BC5E4A659C17842ABA60F3F8B16EB0608016B89EEB6548A5C68A45405134A385E8012E2C1954A6774DFA8723E5352FFC8A0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
				<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;31407BF161017D594F816316B4B71F6644430125F64BE76767D3973A6138C8E1403CA6C1C9FB8EFDA53FED01EF83F92906F02CAF2C1E57C2D521FA5E92049BFA60796404569EA366D1173444A2A56745C230BDC85E282DEA0CC75A69E73BE4FE1F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
			</xceedchart:chartservercontrol><asp:button id="btnUpdateGraph" style="Z-INDEX: 122; LEFT: 224px; POSITION: absolute; TOP: 920px"
				runat="server" Text="Update Chart"></asp:button><asp:radiobuttonlist id="rdbSWUnit" style="Z-INDEX: 123; LEFT: 160px; POSITION: absolute; TOP: 496px"
				runat="server" Height="24px" Width="232px" RepeatDirection="Horizontal">
				<asp:ListItem Value="GravimetricPercent" Selected="True">Gravimetric</asp:ListItem>
				<asp:ListItem Value="VolumetricPercent">Volumetric</asp:ListItem>
			</asp:radiobuttonlist><asp:textbox id="edtRootingDepth" style="Z-INDEX: 124; LEFT: 232px; POSITION: absolute; TOP: 240px"
				runat="server" Width="312px"></asp:textbox>
			<asp:Label id="lblRootingDepthUnit" style="Z-INDEX: 125; LEFT: 552px; POSITION: absolute; TOP: 240px"
				runat="server">cm</asp:Label>
			<asp:CheckBox id="chkDefaultRainfall" style="Z-INDEX: 126; LEFT: 48px; POSITION: absolute; TOP: 272px"
				runat="server" Width="210px" Text="Use weather station's rainfall?" AutoPostBack="True" TextAlign="Left"></asp:CheckBox>
			<asp:Label id="InvalidSWLabel" style="Z-INDEX: 127; LEFT: 544px; POSITION: absolute; TOP: 568px"
				runat="server" Width="216px" Height="136px" ForeColor="Red" Font-Bold="True" Visible="False">The water values entered in the grid don't match the soil type you have choosen. Please contact James Hunt.</asp:Label>
			<asp:CheckBox id="chkUseEC" style="Z-INDEX: 128; LEFT: 16px; POSITION: absolute; TOP: 336px" runat="server"
				Width="352px" Text="Use EC to constrain crop growth?" AutoPostBack="True" Height="24px" TextAlign="Left"></asp:CheckBox></form>
	</body>
</HTML>
