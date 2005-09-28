<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfGenerateSowingXVarietyReport.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfGenerateSowingXVarietyReport" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfGenerateSowingXVarietyReport</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				BackColor="PaleGoldenrod" HorizontalAlign="Left" Width="100%" Height="48px">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 100; LEFT: 120px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" EnableViewState="False" Font-Size="Smaller">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 156; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						Height="16px" Width="64px" BackColor="Transparent" Font-Size="Smaller" Font-Names="Times New Roman"
						ForeColor="Blue" BorderColor="Transparent" BorderStyle="None" Font-Underline="True" Text="Do report"></asp:Button>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 101; LEFT: 96px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="2"
						runat="server" ImageUrl="Images\reports.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<jwg:gridEX id=grdNitrogen style="Z-INDEX: 137; LEFT: 216px; POSITION: absolute; TOP: 192px" runat="server" Width="302px" Height="120px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" AutomaticSort="False" AllowColumnDrag="False" SendDataKeyValuesToClient="True">
				<RootTable DataMember="Nitrogen" Key="Nitrogen">
					<Columns>
						<jwg:GridEXColumn UseType="System.Single" Key="ID" HasValueList="True" DataMember="ID" DefaultGroupPrefix="ID:"
							InvalidValueAction="DiscardChanges" Caption="ID" Width="0px" Visible="False">
							<CellStyle Width="0px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
							Caption="Application Date" Width="140px">
							<CellStyle Width="140px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Rate" HasValueList="True" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
							InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="160px">
							<CellStyle Width="160px"></CellStyle>
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
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="20px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
			<asp:Label id="lblReportName" style="Z-INDEX: 106; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="240px" Height="16px">Enter a descriptive name for the report:</asp:Label>
			<asp:label id="lblCrop" style="Z-INDEX: 108; LEFT: 136px; POSITION: absolute; TOP: 152px" runat="server"
				Width="72px" Height="16px">Crop Type:</asp:label>
			<asp:dropdownlist id="cboCrops" style="Z-INDEX: 114; LEFT: 216px; POSITION: absolute; TOP: 152px"
				tabIndex="3" runat="server" Width="304px" Height="24px" AutoPostBack="True"></asp:dropdownlist>
			<asp:label id="lblNitrogen" style="Z-INDEX: 116; LEFT: 24px; POSITION: absolute; TOP: 192px"
				runat="server"> Nitrogen fertiliser applications:</asp:label>
			<asp:TextBox id="edtReportName" style="Z-INDEX: 111; LEFT: 24px; POSITION: absolute; TOP: 112px"
				tabIndex="1" runat="server" Width="498px" Height="24px"></asp:TextBox>
			<asp:dropdownlist id="cboVarietyOne" style="Z-INDEX: 105; LEFT: 88px; POSITION: absolute; TOP: 400px"
				tabIndex="4" runat="server" Width="192px" Height="24px"></asp:dropdownlist>
			<asp:label id="lblVarietyOne" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 400px"
				runat="server" Width="64px" Height="16px"> Variety:</asp:label>
			<asp:dropdownlist id="cboVarietyTwo" style="Z-INDEX: 110; LEFT: 392px; POSITION: absolute; TOP: 400px"
				tabIndex="4" runat="server" Width="184px" Height="24px"></asp:dropdownlist>
			<asp:dropdownlist id="cboVarietyThree" style="Z-INDEX: 107; LEFT: 688px; POSITION: absolute; TOP: 400px"
				tabIndex="4" runat="server" Width="184px" Height="24px"></asp:dropdownlist>
			<asp:label id="lblVarietyTwo" style="Z-INDEX: 104; LEFT: 328px; POSITION: absolute; TOP: 400px"
				runat="server" Width="56px" Height="16px"> Variety:</asp:label>
			<asp:label id="lblVarietyThree" style="Z-INDEX: 103; LEFT: 624px; POSITION: absolute; TOP: 400px"
				runat="server" Width="64px" Height="16px"> Variety:</asp:label>
			<jwg:gridEX id=grdSowDateOne style="Z-INDEX: 120; LEFT: 176px; POSITION: absolute; TOP: 360px" runat="server" Width="120px" Height="20px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDate" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" ColumnHeaders="False" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True">
				<RootTable DataMember="SowDate" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="SowDate"></jwg:GridEXColumn>
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
			<asp:Label id="lblSowingDateOne" style="Z-INDEX: 141; LEFT: 24px; POSITION: absolute; TOP: 360px"
				runat="server" Width="136px" Height="16px"> Sowing date scenario:</asp:Label>
			<asp:Label id="lblSowingDateTwo" style="Z-INDEX: 143; LEFT: 328px; POSITION: absolute; TOP: 360px"
				runat="server" Width="136px" Height="16px"> Sowing date scenario:</asp:Label>
			<asp:Label id="lblSowingDateThree" style="Z-INDEX: 142; LEFT: 624px; POSITION: absolute; TOP: 360px"
				runat="server" Width="144px" Height="16px"> Sowing date scenario:</asp:Label>
			<jwg:gridEX id=grdSowDateTwo style="Z-INDEX: 123; LEFT: 472px; POSITION: absolute; TOP: 360px" runat="server" Width="120px" Height="20px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDate" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" ColumnHeaders="False" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True">
				<RootTable DataMember="SowDate" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="SowDate"></jwg:GridEXColumn>
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
			<jwg:gridEX id=grdSowDateThree style="Z-INDEX: 124; LEFT: 768px; POSITION: absolute; TOP: 360px" runat="server" Width="120px" Height="20px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDate" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" ColumnHeaders="False" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True">
				<RootTable DataMember="SowDate" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="SowDate"></jwg:GridEXColumn>
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
			<asp:label id="lblTillerTwo" style="Z-INDEX: 147; LEFT: 328px; POSITION: absolute; TOP: 560px"
				runat="server">NFT:</asp:label>
			<asp:Label id="lblRowSpacingTwo" style="Z-INDEX: 153; LEFT: 328px; POSITION: absolute; TOP: 520px"
				runat="server">Row Spacing:</asp:Label>
			<asp:label id="lblRowConfigurationTwo" style="Z-INDEX: 150; LEFT: 328px; POSITION: absolute; TOP: 480px"
				runat="server">Row Configuration:</asp:label>
			<asp:textbox id="edtTillerTwo" style="Z-INDEX: 154; LEFT: 368px; POSITION: absolute; TOP: 560px"
				runat="server" Width="80px"></asp:textbox>
			<asp:Button id="btnReCalculateTwo" style="Z-INDEX: 145; LEFT: 456px; POSITION: absolute; TOP: 560px"
				runat="server" Width="128px" Text="Re-Calculate NFT"></asp:Button>
			<asp:TextBox id="edtRowSpacingTwo" style="Z-INDEX: 149; LEFT: 416px; POSITION: absolute; TOP: 520px"
				runat="server" Width="144px"></asp:TextBox>
			<asp:Label id="lblRowSpacingUnitTwo" style="Z-INDEX: 146; LEFT: 568px; POSITION: absolute; TOP: 520px"
				runat="server">m</asp:Label>
			<asp:dropdownlist id="cboRowConfigurationTwo" style="Z-INDEX: 148; LEFT: 448px; POSITION: absolute; TOP: 480px"
				runat="server" Width="136px"></asp:dropdownlist>
			<asp:label id="lblPopulationUnitTwo" style="Z-INDEX: 151; LEFT: 520px; POSITION: absolute; TOP: 440px"
				runat="server" Width="56px">plants/ha</asp:label>
			<asp:textbox id="edtPopulationTwo" style="Z-INDEX: 152; LEFT: 400px; POSITION: absolute; TOP: 440px"
				runat="server" Width="112px"></asp:textbox>
			<asp:label id="lblPopulationTwo" style="Z-INDEX: 144; LEFT: 328px; POSITION: absolute; TOP: 440px"
				runat="server">Population: </asp:label>
			<asp:CheckBox id="chkAutoCalculateTwo" style="Z-INDEX: 130; LEFT: 328px; POSITION: absolute; TOP: 592px"
				runat="server" Height="16px" Width="264px" Text="Auto Calculate Number of Fertile Tillers" AutoPostBack="True"
				TextAlign="Left"></asp:CheckBox>
			<asp:CheckBox id="chkAutoCalculateThree" style="Z-INDEX: 155; LEFT: 624px; POSITION: absolute; TOP: 592px"
				runat="server" Height="16px" Width="264px" Text="Auto Calculate Number of Fertile Tillers" AutoPostBack="True"
				TextAlign="Left"></asp:CheckBox>
			<asp:Button id="btnReCalculateThree" style="Z-INDEX: 134; LEFT: 752px; POSITION: absolute; TOP: 560px"
				runat="server" Width="128px" Text="Re-Calculate NFT"></asp:Button>
			<asp:textbox id="edtTillerThree" style="Z-INDEX: 132; LEFT: 664px; POSITION: absolute; TOP: 560px"
				runat="server" Width="80px"></asp:textbox>
			<asp:label id="lblTillerThree" style="Z-INDEX: 136; LEFT: 624px; POSITION: absolute; TOP: 560px"
				runat="server">NFT:</asp:label>
			<asp:Label id="lblRowSpacingThree" style="Z-INDEX: 139; LEFT: 624px; POSITION: absolute; TOP: 520px"
				runat="server">Row Spacing:</asp:Label>
			<asp:TextBox id="edtRowSpacingThree" style="Z-INDEX: 129; LEFT: 712px; POSITION: absolute; TOP: 520px"
				runat="server" Width="144px"></asp:TextBox>
			<asp:Label id="lblRowSpacingUnitThree" style="Z-INDEX: 125; LEFT: 864px; POSITION: absolute; TOP: 520px"
				runat="server">m</asp:Label>
			<asp:dropdownlist id="cboRowConfigurationThree" style="Z-INDEX: 127; LEFT: 744px; POSITION: absolute; TOP: 480px"
				runat="server" Width="136px"></asp:dropdownlist>
			<asp:label id="lblRowConfigurationThree" style="Z-INDEX: 122; LEFT: 624px; POSITION: absolute; TOP: 480px"
				runat="server">Row Configuration:</asp:label>
			<asp:label id="lblPopulationThree" style="Z-INDEX: 118; LEFT: 624px; POSITION: absolute; TOP: 440px"
				runat="server">Population: </asp:label>
			<asp:textbox id="edtPopulationThree" style="Z-INDEX: 117; LEFT: 696px; POSITION: absolute; TOP: 440px"
				runat="server" Width="112px"></asp:textbox>
			<asp:label id="lblPopulationUnitThree" style="Z-INDEX: 115; LEFT: 816px; POSITION: absolute; TOP: 440px"
				runat="server" Width="56px">plants/ha</asp:label>
			<asp:CheckBox id="chkAutoCalculateOne" style="Z-INDEX: 140; LEFT: 24px; POSITION: absolute; TOP: 592px"
				runat="server" Height="16px" Width="264px" Text="Auto Calculate Number of Fertile Tillers" AutoPostBack="True"
				TextAlign="Left"></asp:CheckBox>
			<asp:label id="lblTillerOne" style="Z-INDEX: 131; LEFT: 24px; POSITION: absolute; TOP: 560px"
				runat="server">NFT:</asp:label>
			<asp:textbox id="edtTillerOne" style="Z-INDEX: 138; LEFT: 64px; POSITION: absolute; TOP: 560px"
				runat="server" Width="80px"></asp:textbox>
			<asp:Button id="btnReCalculateOne" style="Z-INDEX: 133; LEFT: 160px; POSITION: absolute; TOP: 560px"
				runat="server" Width="128px" Text="Re-Calculate NFT"></asp:Button>
			<asp:Label id="lblRowSpacingUnitOne" style="Z-INDEX: 135; LEFT: 264px; POSITION: absolute; TOP: 520px"
				runat="server">m</asp:Label>
			<asp:TextBox id="edtRowSpacingOne" style="Z-INDEX: 128; LEFT: 112px; POSITION: absolute; TOP: 520px"
				runat="server" Width="144px"></asp:TextBox>
			<asp:Label id="lblRowSpacingOne" style="Z-INDEX: 126; LEFT: 24px; POSITION: absolute; TOP: 520px"
				runat="server">Row Spacing:</asp:Label>
			<asp:label id="lblRowConfigurationOne" style="Z-INDEX: 121; LEFT: 24px; POSITION: absolute; TOP: 480px"
				runat="server">Row Configuration:</asp:label>
			<asp:dropdownlist id="cboRowConfigurationOne" style="Z-INDEX: 119; LEFT: 144px; POSITION: absolute; TOP: 480px"
				runat="server" Width="136px"></asp:dropdownlist>
			<asp:label id="lblPopulationUnitOne" style="Z-INDEX: 113; LEFT: 216px; POSITION: absolute; TOP: 440px"
				runat="server" Width="56px">plants/ha</asp:label>
			<asp:label id="lblPopulationOne" style="Z-INDEX: 112; LEFT: 24px; POSITION: absolute; TOP: 440px"
				runat="server">Population: </asp:label>
			<asp:textbox id="edtPopulationOne" style="Z-INDEX: 109; LEFT: 96px; POSITION: absolute; TOP: 440px"
				runat="server" Width="112px"></asp:textbox>
			<asp:Panel id="pnlDivideOne" style="Z-INDEX: 156; LEFT: 296px; POSITION: absolute; TOP: 360px"
				runat="server" Height="248px" Width="12px" BackColor="Black" ForeColor="Black" BorderColor="White"
				BorderStyle="Solid"></asp:Panel>
			<asp:Panel id="pnlDivideTwo" style="Z-INDEX: 157; LEFT: 600px; POSITION: absolute; TOP: 360px"
				runat="server" Height="248px" Width="12px" BackColor="Black" ForeColor="Black" BorderColor="White"
				BorderStyle="Solid"></asp:Panel>
			<asp:Label id="lblScenarioOne" style="Z-INDEX: 158; LEFT: 112px; POSITION: absolute; TOP: 328px"
				runat="server" Height="24px" Width="96px" Font-Bold="True">Scenario One</asp:Label>
			<asp:Label id="lblScenarioTwo" style="Z-INDEX: 158; LEFT: 408px; POSITION: absolute; TOP: 328px"
				runat="server" Height="24px" Width="96px" Font-Bold="True">Scenario Two</asp:Label>
			<asp:Label id="lblScenarioThree" style="Z-INDEX: 158; LEFT: 704px; POSITION: absolute; TOP: 328px"
				runat="server" Height="24px" Width="104px" Font-Bold="True">Scenario Three</asp:Label>
		</form>
	</body>
</HTML>
