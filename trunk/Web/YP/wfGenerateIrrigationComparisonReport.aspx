<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfGenerateIrrigationComparisonReport.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfGenerateIrrigationComparisonReport" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Generate Irrigation Comparison Report(s)</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 800px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 1795px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" Width="800px" Height="64px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="160px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="88px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="382px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="368px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Width="785px" Height="20px" Font-Names="Arial Black" Font-Size="Large" ForeColor="DarkGray"
									Font-Bold="True">Generate Irrigation Comparision Report</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 176px"
								runat="server" Width="800px" Height="48px" BackColor="DarkGray" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfReportsView.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfReportsGenerate.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="152px"
										CommandName="wfReportsFavourites.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:label id="lblReportName" style="Z-INDEX: 105; LEFT: 144px; POSITION: absolute; TOP: 256px"
								runat="server" Font-Names="Arial" Height="16px" Width="280px">Enter a descriptive name for the report:</asp:label><asp:textbox id="edtReportName" style="Z-INDEX: 106; LEFT: 144px; POSITION: absolute; TOP: 280px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="480px"></asp:textbox><jwg:gridex id=grdIrrigationOne style="Z-INDEX: 107; LEFT: 144px; POSITION: absolute; TOP: 352px" runat="server" Font-Names="Arial" Height="220px" Width="482px" SendDataKeyValuesToClient="True" TableHeaders="True" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="IrrigationOne" DataSource="<%# dsIrrigation %>" GridLineColor="ScrollBar" GroupByBoxVisible="False" AllowEdit="True" tabIndex=2>
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
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
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
								<RootTable DataMember="IrrigationOne" Caption="Scenario One Irrigation" Key="IrrigationOne">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="Date" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="Date" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Application Date" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Amount" HasValueList="True" DataMember="Amount" DefaultGroupPrefix="Application Amount (mm/ha):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Amount (mm/ha)" Width="204px">
											<CellStyle Width="204px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Efficency" HasValueList="True" DataMember="Efficency"
											DefaultGroupPrefix="Efficency (%):" InvalidValueAction="DiscardChanges" NullText="" Caption="Efficency (%)"
											Width="135px">
											<CellStyle Width="135px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="LightSteelBlue" ForeColor="ControlText" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="middle"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex><asp:label id="lblScenarioOneApplications" style="Z-INDEX: 108; LEFT: 144px; POSITION: absolute; TOP: 328px"
								runat="server" Font-Names="Arial" Font-Size="Small" Font-Bold="True">Scenario One</asp:label>
							<jwg:gridEX id=grdNitrogenOne style="Z-INDEX: 109; LEFT: 144px; POSITION: absolute; TOP: 608px" runat="server" Font-Names="Arial" Height="127px" Width="322px" SendDataKeyValuesToClient="True" TableHeaders="True" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="NitrogenOne" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" tabIndex=3>
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
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
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
								<RootTable DataMember="NitrogenOne" Caption="Scenario One Nitrogen" Key="NitrogenThree">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="ID" HasValueList="True" DataMember="ID" DefaultGroupPrefix="ID:"
											InvalidValueAction="DiscardChanges" NullText="" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Application Date" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Rate" HasValueList="True" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="LightSteelBlue" ForeColor="ControlText" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="middle"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridEX><jwg:gridex id="grdIrrigationTwo" style="Z-INDEX: 110; LEFT: 144px; POSITION: absolute; TOP: 800px"
				runat="server" Font-Names="Arial" Height="220px" Width="482px" SendDataKeyValuesToClient="True" TableHeaders="True"
				AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html"
				ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="IrrigationTwo" GridLineColor="ScrollBar"
				GroupByBoxVisible="False" DataSource="<%# dsIrrigation %>" AllowEdit="True" tabIndex=4>
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
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
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
								<RootTable DataMember="IrrigationTwo" Caption="Scenario Two Irrigation" Key="IrrigationOne">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="Date" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="Date" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Application Date" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Amount" HasValueList="True" DataMember="Amount" DefaultGroupPrefix="Application Amount (mm/ha):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Amount (mm/ha)" Width="204px">
											<CellStyle Width="204px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Efficency" HasValueList="True" DataMember="Efficency"
											DefaultGroupPrefix="Efficency (%):" InvalidValueAction="DiscardChanges" NullText="" Caption="Efficency (%)"
											Width="135px">
											<CellStyle Width="135px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Gold" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="middle"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex><asp:label id="lblScenarioTwoApplications" style="Z-INDEX: 111; LEFT: 144px; POSITION: absolute; TOP: 776px"
								runat="server" Font-Names="Arial" Font-Size="Small" Font-Bold="True">Scenario Two</asp:label>
							<jwg:gridEX id=grdNitrogenTwo style="Z-INDEX: 112; LEFT: 144px; POSITION: absolute; TOP: 1056px" runat="server" Font-Names="Arial" Height="127px" Width="322px" SendDataKeyValuesToClient="True" TableHeaders="True" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="NitrogenTwo" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" tabIndex=5>
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
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
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
								<RootTable DataMember="NitrogenTwo" Caption="Scenario Two Nitrogen" Key="NitrogenThree">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="ID" HasValueList="True" DataMember="ID" DefaultGroupPrefix="ID:"
											InvalidValueAction="DiscardChanges" NullText="" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Application Date" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Rate" HasValueList="True" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Gold" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="middle"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridEX><asp:label id="lblScenarioThreeApplications" style="Z-INDEX: 114; LEFT: 144px; POSITION: absolute; TOP: 1224px"
								runat="server" Font-Names="Arial" Font-Bold="True">Scenario Three</asp:label><jwg:gridex id="grdIrrigationThree" style="Z-INDEX: 115; LEFT: 144px; POSITION: absolute; TOP: 1248px"
				runat="server" Font-Names="Arial" Height="220px" Width="482px" SendDataKeyValuesToClient="True" TableHeaders="True"
				AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html"
				ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" GridLineColor="ScrollBar" GroupByBoxVisible="False" DataMember="IrrigationThree" DataSource="<%# dsIrrigation %>" AllowEdit="True" tabIndex=6>
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
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
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
								<RootTable DataMember="IrrigationThree" Caption="ScenarioThree Irrigation" Key="IrrigationOne">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="Date" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="Date" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Application Date" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Amount" HasValueList="True" DataMember="Amount" DefaultGroupPrefix="Application Amount (mm/ha):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Amount (mm/ha)" Width="204px">
											<CellStyle Width="204px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Efficency" HasValueList="True" DataMember="Efficency"
											DefaultGroupPrefix="Efficency (%):" InvalidValueAction="DiscardChanges" NullText="" Caption="Efficency (%)"
											Width="135px">
											<CellStyle Width="135px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Chocolate" ForeColor="ControlText" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="middle"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<jwg:gridEX id=grdNitrogenThree style="Z-INDEX: 116; LEFT: 144px; POSITION: absolute; TOP: 1504px" runat="server" Font-Names="Arial" Height="127px" Width="322px" SendDataKeyValuesToClient="True" TableHeaders="True" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="NitrogenThree" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" tabIndex=7>
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
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
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
								<RootTable DataMember="NitrogenThree" Caption="Scenario Three Nitrogen" Key="NitrogenThree">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="ID" HasValueList="True" DataMember="ID" DefaultGroupPrefix="ID:"
											InvalidValueAction="DiscardChanges" NullText="" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="ApplicationDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="ApplicationDate" DefaultGroupPrefix="Application Date:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Application Date" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Rate" HasValueList="True" DataMember="Rate" DefaultGroupPrefix="Application Rate (kg/ha):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Chocolate" ForeColor="ControlText" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="middle"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridEX>
							<asp:Button id="btnGenerate" style="Z-INDEX: 117; LEFT: 248px; POSITION: absolute; TOP: 1688px"
								runat="server" Width="120px" Height="32px" Font-Names="Arial" Text="Generate" tabIndex="8"></asp:Button>
							<asp:CheckBox id="chkFavourite" style="Z-INDEX: 118; LEFT: 320px; POSITION: absolute; TOP: 1736px"
								runat="server" Font-Names="Arial" ForeColor="Black" Text="Save as favourite?" TextAlign="Left"
								tabIndex="10"></asp:CheckBox>
							<asp:Button id="btnCancel" style="Z-INDEX: 119; LEFT: 416px; POSITION: absolute; TOP: 1688px"
								runat="server" Width="120px" Height="32px" Font-Names="Arial" Text="Cancel" tabIndex="9"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 120; LEFT: 0px; POSITION: absolute; TOP: 1776px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 121; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
