<%@ Page language="c#" Codebehind="wfGenerateNitrogenComparisonReport.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfGenerateNitrogenComparisonReport" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfGenerateNitrogenComparisonReport</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:TextBox id="edtReportName" style="Z-INDEX: 101; LEFT: 24px; POSITION: absolute; TOP: 112px"
				tabIndex="1" runat="server" Width="480px" Height="24px"></asp:TextBox>
			<asp:Label id="lblReportName" style="Z-INDEX: 102; LEFT: 24px; POSITION: absolute; TOP: 80px"
				runat="server" Width="240px" Height="16px">Enter a descriptive name for the report:</asp:Label>
			<asp:Panel id="pnlTop" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Width="100%" Height="48px" BackColor="PaleGoldenrod" HorizontalAlign="Left">
				<DIV style="WIDTH: 184px; POSITION: relative; HEIGHT: 41px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 101; LEFT: 104px; POSITION: absolute; TOP: 16px"
						tabIndex="5" runat="server" EnableViewState="False" Font-Size="X-Small">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 112; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						Height="16px" Width="32px" BackColor="Transparent" Font-Size="Smaller" ForeColor="Purple"
						BorderColor="Transparent" BorderStyle="None" Font-Underline="True" Text="Save"></asp:Button>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 102; LEFT: 80px; POSITION: absolute; TOP: 16px"
						tabIndex="4" runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 16px" tabIndex="2"
						runat="server" ImageUrl="Images\save.gif"></asp:ImageButton></DIV>
			</asp:Panel>
			<jwg:gridEX id=grdScenarioOne style="Z-INDEX: 104; LEFT: 200px; POSITION: absolute; TOP: 232px" runat="server" Width="302px" Height="120px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch">
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="LightSteelBlue" ForeColor="ControlText" Height="20px"
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
				<RootTable DataMember="Nitrogen" Key="Nitrogen">
					<Columns>
						<jwg:GridEXColumn UseType="System.Single" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
							Caption="ID" Width="0px" Visible="False">
							<CellStyle Width="0px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
							DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
							Caption="Application Date" Width="140px">
							<CellStyle Width="140px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Rate" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
							InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="160px">
							<CellStyle Width="160px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
			</jwg:gridEX>
			<asp:TextBox id="edtScenarioOne" style="Z-INDEX: 111; LEFT: 200px; POSITION: absolute; TOP: 200px"
				runat="server" Width="304px" BackColor="LightSteelBlue"></asp:TextBox>
			<asp:TextBox id="edtScenarioTwo" style="Z-INDEX: 112; LEFT: 200px; POSITION: absolute; TOP: 384px"
				runat="server" Width="304px" BackColor="Gold"></asp:TextBox>
			<asp:TextBox id="edtScenarioThree" style="Z-INDEX: 113; LEFT: 200px; POSITION: absolute; TOP: 568px"
				runat="server" Width="302px" BackColor="Chocolate"></asp:TextBox>
			<jwg:gridEX id=grdScenarioTwo style="Z-INDEX: 105; LEFT: 200px; POSITION: absolute; TOP: 416px" runat="server" Width="302px" Height="120px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch">
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="Gold" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
					BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
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
				<RootTable DataMember="Nitrogen" Key="Nitrogen">
					<Columns>
						<jwg:GridEXColumn UseType="System.Single" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
							Caption="ID" Width="0px" Visible="False">
							<CellStyle Width="0px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
							DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
							Caption="Application Date" Width="140px">
							<CellStyle Width="140px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Rate" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
							InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="160px">
							<CellStyle Width="160px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
			</jwg:gridEX>
			<jwg:gridEX id=grdScenarioThree style="Z-INDEX: 106; LEFT: 200px; POSITION: absolute; TOP: 600px" runat="server" Width="302px" Height="120px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch">
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="Chocolate" ForeColor="ControlText" Height="20px"
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
				<RootTable DataMember="Nitrogen" Key="Nitrogen">
					<Columns>
						<jwg:GridEXColumn UseType="System.Single" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
							Caption="ID" Width="0px" Visible="False">
							<CellStyle Width="0px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
							DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
							Caption="Application Date" Width="140px">
							<CellStyle Width="140px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Rate" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
							InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="160px">
							<CellStyle Width="160px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
			</jwg:gridEX>
			<asp:Label id="lbScenarioOneDescription" style="Z-INDEX: 115; LEFT: 24px; POSITION: absolute; TOP: 200px"
				runat="server">Scenario One Description:</asp:Label>
			<asp:Label id="lblScenarioOneApplications" style="Z-INDEX: 114; LEFT: 24px; POSITION: absolute; TOP: 232px"
				runat="server">Scenario One Applications:</asp:Label>
			<asp:Label id="lblScenarioTwoApplications" style="Z-INDEX: 109; LEFT: 24px; POSITION: absolute; TOP: 416px"
				runat="server">Scenario Two Applications:</asp:Label>
			<asp:Label id="lblScenarioTwoDescription" style="Z-INDEX: 108; LEFT: 24px; POSITION: absolute; TOP: 384px"
				runat="server">Scenario Two Description:</asp:Label>
			<asp:Label id="lblScenarioThreeApplications" style="Z-INDEX: 110; LEFT: 24px; POSITION: absolute; TOP: 600px"
				runat="server">Scenario Three Applications:</asp:Label>
			<asp:Label id="lblScenarioThreeDescription" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 568px"
				runat="server">Scenario Three Description:</asp:Label>
			<asp:Label id="lblWarning" style="Z-INDEX: 116; LEFT: 32px; POSITION: absolute; TOP: 160px"
				runat="server" Width="464px" ForeColor="Red">Warning: If you do not enter a description the scenario will not be calculated</asp:Label>
		</form>
	</body>
</HTML>
