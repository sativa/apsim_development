<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfGenerateNitrogenProfitReport.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfGenerateNitrogenProfitReport" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Generate Nitrogen Profit Report(s)</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 1156px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" Height="64px" Width="800px" BackColor="MediumBlue" BorderColor="White" BorderStyle="None">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="160px" Height="8px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px" Height="8px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px" Height="8px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="382px" Height="8px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="368px" Height="8px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Large"
									Font-Bold="True">Generate Nitrogen Profit Report</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 105; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" Height="48px" Width="800px" BackColor="DarkGray" BorderColor="White" BorderStyle="None">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px" Height="8px"
										CommandName="wfReportsView.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px" Height="8px"
										CommandName="wfReportsGenerate.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="152px" Height="8px"
										CommandName="wfReportsFavourites.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:label id="lblReportName" style="Z-INDEX: 106; LEFT: 128px; POSITION: absolute; TOP: 240px"
								runat="server" Height="16px" Width="296px" Font-Names="Arial">Enter a descriptive name for the report:</asp:label><asp:textbox id="edtReportName" style="Z-INDEX: 107; LEFT: 128px; POSITION: absolute; TOP: 272px"
								tabIndex="1" runat="server" Font-Names="Arial" Width="496px" Height="24px"></asp:textbox>
							<asp:label id="lblClassification" style="Z-INDEX: 108; LEFT: 128px; POSITION: absolute; TOP: 320px"
								runat="server" Height="16px" Width="112px" Font-Names="Arial">Classification:</asp:label><asp:dropdownlist id="cboClassification" style="Z-INDEX: 109; LEFT: 424px; POSITION: absolute; TOP: 320px"
								runat="server" Font-Names="Arial" Width="168px" tabIndex="2"></asp:dropdownlist>
							<asp:textbox id="edtPrice" style="Z-INDEX: 110; LEFT: 424px; POSITION: absolute; TOP: 360px"
								runat="server" Width="168px" Font-Names="Arial" tabIndex="3"></asp:textbox><asp:label id="lblPrice" style="Z-INDEX: 111; LEFT: 128px; POSITION: absolute; TOP: 360px"
								runat="server" Font-Names="Arial" Width="216px" Height="16px">Expected crop price on farm:</asp:label><asp:label id="lblProteinContent" style="Z-INDEX: 112; LEFT: 128px; POSITION: absolute; TOP: 400px"
								runat="server" Font-Names="Arial" Width="312px" Height="16px">Minimum protein content for classification:</asp:label><asp:label id="lblProteinIncrement" style="Z-INDEX: 113; LEFT: 128px; POSITION: absolute; TOP: 440px"
								runat="server" Font-Names="Arial" Width="200px" Height="16px">Protein increment payment:</asp:label>
							<asp:label id="lblProteinIncrementKey" style="Z-INDEX: 114; LEFT: 128px; POSITION: absolute; TOP: 464px"
								runat="server" Font-Names="Arial">(0.5% protein)</asp:label><asp:label id="lblFertiliserCost" style="Z-INDEX: 115; LEFT: 128px; POSITION: absolute; TOP: 504px"
								runat="server" Font-Names="Arial" Width="184px">Cost of nitrogen fertiliser:</asp:label><asp:label id="lblApplicationCost" style="Z-INDEX: 116; LEFT: 128px; POSITION: absolute; TOP: 544px"
								runat="server" Font-Names="Arial" Width="208px">Cost of nitrogen application:</asp:label>
							<asp:label id="lblScenarioOneApplications" style="Z-INDEX: 117; LEFT: 128px; POSITION: absolute; TOP: 592px"
								runat="server" Font-Names="Arial">Scenario one applications:</asp:label>
							<jwg:gridex id=grdScenarioOne style="Z-INDEX: 118; LEFT: 336px; POSITION: absolute; TOP: 592px" runat="server" Height="127px" Width="322px" Font-Names="Arial" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="NitrogenOne" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" SendDataKeyValuesToClient="True" tabIndex=8>
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
								<RootTable DataMember="NitrogenOne" Key="Nitrogen">
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
											InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="LightSteelBlue" ForeColor="ControlText" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:textbox id="edtApplicationCost" style="Z-INDEX: 119; LEFT: 424px; POSITION: absolute; TOP: 544px"
								runat="server" Width="168px" Font-Names="Arial" tabIndex="7"></asp:textbox>
							<asp:label id="lblApplicationCostUnit" style="Z-INDEX: 120; LEFT: 600px; POSITION: absolute; TOP: 544px"
								runat="server" Height="16px" Width="56px" Font-Names="Arial">$ / ha</asp:label>
							<asp:label id="lblFertiliserCostUnit" style="Z-INDEX: 121; LEFT: 600px; POSITION: absolute; TOP: 504px"
								runat="server" Height="16px" Width="72px" Font-Names="Arial">$ Kg / N</asp:label>
							<asp:textbox id="edtFertiliserCost" style="Z-INDEX: 122; LEFT: 424px; POSITION: absolute; TOP: 504px"
								runat="server" Width="168px" Font-Names="Arial" tabIndex="6"></asp:textbox><asp:textbox id="edtProteinIncrement" style="Z-INDEX: 123; LEFT: 424px; POSITION: absolute; TOP: 448px"
								runat="server" Font-Names="Arial" Width="168px" tabIndex="5"></asp:textbox>
							<asp:label id="lblProteinIncrementUnit" style="Z-INDEX: 124; LEFT: 600px; POSITION: absolute; TOP: 448px"
								runat="server" Height="16px" Width="48px" Font-Names="Arial">$ / t</asp:label>
							<asp:textbox id="edtProteinContent" style="Z-INDEX: 125; LEFT: 424px; POSITION: absolute; TOP: 400px"
								runat="server" Width="168px" Font-Names="Arial" tabIndex="4"></asp:textbox><asp:label id="lblProteinContentUnit" style="Z-INDEX: 126; LEFT: 600px; POSITION: absolute; TOP: 400px"
								runat="server" Font-Names="Arial" Width="24px" Height="16px">%</asp:label>
							<asp:label id="lblPriceUnit" style="Z-INDEX: 127; LEFT: 600px; POSITION: absolute; TOP: 360px"
								runat="server" Height="16px" Width="48px" Font-Names="Arial">$ / t</asp:label>
							<asp:label id="lblScenarioTwoApplications" style="Z-INDEX: 128; LEFT: 128px; POSITION: absolute; TOP: 736px"
								runat="server" Font-Names="Arial">Scenario two applications:</asp:label>
							<jwg:gridex id=grdScenarioTwo style="Z-INDEX: 129; LEFT: 336px; POSITION: absolute; TOP: 736px" runat="server" Height="124px" Width="322px" Font-Names="Arial" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="NitrogenTwo" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" SendDataKeyValuesToClient="True" tabIndex=9>
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
								<RootTable DataMember="NitrogenTwo" Key="Nitrogen">
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
											InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Gold" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:label id="lblScenarioThreeApplications" style="Z-INDEX: 130; LEFT: 128px; POSITION: absolute; TOP: 880px"
								runat="server" Font-Names="Arial">Scenario three applications:</asp:label>
							<jwg:gridex id=grdScenarioThree style="Z-INDEX: 131; LEFT: 336px; POSITION: absolute; TOP: 880px" runat="server" Height="126px" Width="322px" Font-Names="Arial" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="NitrogenThree" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" SendDataKeyValuesToClient="True" tabIndex=10>
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
								<RootTable DataMember="NitrogenThree" Key="Nitrogen">
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
											InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Chocolate" ForeColor="ControlText" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:Button id="btnGenerate" style="Z-INDEX: 132; LEFT: 272px; POSITION: absolute; TOP: 1040px"
								runat="server" Height="32px" Width="120px" Font-Names="Arial" Text="Generate" tabIndex="11"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 133; LEFT: 432px; POSITION: absolute; TOP: 1040px"
								runat="server" Height="32px" Width="120px" Font-Names="Arial" Text="Cancel" tabIndex="12"></asp:Button>
							<asp:CheckBox id="chkFavourite" style="Z-INDEX: 134; LEFT: 336px; POSITION: absolute; TOP: 1088px"
								runat="server" Font-Names="Arial" Text="Save as favourite?" ForeColor="Black" TextAlign="Left"
								tabIndex="13"></asp:CheckBox>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 135; LEFT: 0px; POSITION: absolute; TOP: 1136px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 136; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
