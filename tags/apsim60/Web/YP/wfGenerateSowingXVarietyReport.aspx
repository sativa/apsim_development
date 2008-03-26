<%@ Page language="c#" Codebehind="wfGenerateSowingXVarietyReport.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfGenerateSowingXVarietyReport" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Generate SowingX Variety Report(s)</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 1252px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" Width="800px" Height="64px" BackColor="MediumBlue" BorderColor="White" BorderStyle="None">
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
									Font-Bold="True">Generate Sowing Variety Report</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 176px"
								runat="server" Width="800px" Height="48px" BackColor="DarkGray" BorderColor="White" BorderStyle="None">
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
							</asp:panel><asp:label id="lblReportName" style="Z-INDEX: 105; LEFT: 32px; POSITION: absolute; TOP: 248px"
								runat="server" Font-Names="Arial" Height="16px" Width="272px">Enter a descriptive name for the report:</asp:label>
							<asp:textbox id="edtReportName" style="Z-INDEX: 106; LEFT: 32px; POSITION: absolute; TOP: 272px"
								tabIndex="1" runat="server" Width="528px" Height="24px" Font-Names="Arial"></asp:textbox><asp:label id="lblCrop" style="Z-INDEX: 107; LEFT: 160px; POSITION: absolute; TOP: 312px" runat="server"
								Font-Names="Arial" Height="16px" Width="80px">Crop type:</asp:label><asp:dropdownlist id="cboCrops" style="Z-INDEX: 108; LEFT: 256px; POSITION: absolute; TOP: 312px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="24px" Width="304px" AutoPostBack="True"></asp:dropdownlist><asp:label id="lblNitrogen" style="Z-INDEX: 109; LEFT: 32px; POSITION: absolute; TOP: 352px"
								runat="server" Font-Names="Arial"> Nitrogen fertiliser applications:</asp:label>
							<jwg:gridex id=grdNitrogen style="Z-INDEX: 110; LEFT: 248px; POSITION: absolute; TOP: 352px" runat="server" Width="322px" Height="128px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" AutomaticSort="False" AllowColumnDrag="False" SendDataKeyValuesToClient="True" Font-Names="Arial" tabIndex=3>
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
											InvalidValueAction="DiscardChanges" Caption="Application Rate (kg/ha)" Width="180px">
											<CellStyle Width="180px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="DarkGray" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
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
							<asp:label id="lblSowingDateOne" style="Z-INDEX: 111; LEFT: 40px; POSITION: absolute; TOP: 552px"
								runat="server" Width="160px" Height="16px" Font-Names="Arial"> Sowing date scenario:</asp:label>
							<jwg:gridex id=grdSowDateOne style="Z-INDEX: 112; LEFT: 200px; POSITION: absolute; TOP: 552px" runat="server" Width="120px" Height="20px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDateOne" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True" Font-Names="Arial" ColumnHeaders="False" tabIndex=4>
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
								<RootTable DataMember="SowDateOne" Key="SowDate">
									<Columns>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="SowDate"></jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:label id="lblVarietyOne" style="Z-INDEX: 114; LEFT: 40px; POSITION: absolute; TOP: 600px"
								runat="server" Width="56px" Height="16px" Font-Names="Arial"> Variety:</asp:label><asp:dropdownlist id="cboVarietyOne" style="Z-INDEX: 115; LEFT: 104px; POSITION: absolute; TOP: 600px"
								tabIndex="5" runat="server" Font-Names="Arial" Height="24px" Width="209px"></asp:dropdownlist>
							<asp:Panel id="pnlCanolaOne" style="Z-INDEX: 116; LEFT: 328px; POSITION: absolute; TOP: 512px"
								runat="server" Width="456px" Height="200px" Visible="False">
								<DIV title="Canola" style="WIDTH: 456px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:checkbox id="chkTriazineOne" style="Z-INDEX: 115; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Height="20px" Width="160px" Text="Triazine tolerant?" TextAlign="Left"></asp:checkbox></DIV>
							</asp:Panel>
							<asp:label id="lblScenarioTwo" style="Z-INDEX: 117; LEFT: 112px; POSITION: absolute; TOP: 720px"
								runat="server" Width="109px" Height="24px" Font-Names="Arial" Font-Bold="True">Scenario Two</asp:label>
							<asp:label id="lblSowingDateTwo" style="Z-INDEX: 118; LEFT: 32px; POSITION: absolute; TOP: 752px"
								runat="server" Width="160px" Height="16px" Font-Names="Arial"> Sowing date scenario:</asp:label>
							<jwg:gridex id=grdSowDateTwo style="Z-INDEX: 119; LEFT: 192px; POSITION: absolute; TOP: 752px" runat="server" Width="120px" Height="20px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDateTwo" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True" Font-Names="Arial" ColumnHeaders="False" tabIndex=6>
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
								<RootTable DataMember="SowDateTwo" Key="SowDate">
									<Columns>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="SowDate"></jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex><asp:dropdownlist id="cboVarietyTwo" style="Z-INDEX: 120; LEFT: 96px; POSITION: absolute; TOP: 800px"
								tabIndex="7" runat="server" Font-Names="Arial" Height="24px" Width="201px"></asp:dropdownlist>
							<asp:label id="lblVarietyTwo" style="Z-INDEX: 121; LEFT: 32px; POSITION: absolute; TOP: 800px"
								runat="server" Width="56px" Height="16px" Font-Names="Arial"> Variety:</asp:label>
							<asp:Panel id="pnlCanolaTwo" style="Z-INDEX: 122; LEFT: 328px; POSITION: absolute; TOP: 720px"
								runat="server" Width="456px" Height="200px" Visible="False">
								<DIV title="Canola" style="WIDTH: 456px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:checkbox id="chkTriazineTwo" style="Z-INDEX: 115; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Height="20px" Width="160px" Text="Triazine tolerant?" TextAlign="Left"></asp:checkbox></DIV>
							</asp:Panel>
							<asp:label id="lblScenarioThree" style="Z-INDEX: 123; LEFT: 112px; POSITION: absolute; TOP: 928px"
								runat="server" Width="128px" Height="24px" Font-Names="Arial" Font-Bold="True">Scenario Three</asp:label>
							<asp:label id="lblSowingDateThree" style="Z-INDEX: 124; LEFT: 32px; POSITION: absolute; TOP: 968px"
								runat="server" Width="160px" Height="16px" Font-Names="Arial"> Sowing date scenario:</asp:label>
							<jwg:gridex id=grdSowDateThree style="Z-INDEX: 125; LEFT: 200px; POSITION: absolute; TOP: 968px" runat="server" Width="120px" Height="20px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDateThree" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True" Font-Names="Arial" ColumnHeaders="False" tabIndex=8>
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
								<RootTable DataMember="SowDateThree" Key="SowDate">
									<Columns>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="SowDate"></jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex><asp:dropdownlist id="cboVarietyThree" style="Z-INDEX: 126; LEFT: 96px; POSITION: absolute; TOP: 1016px"
								tabIndex="9" runat="server" Font-Names="Arial" Height="24px" Width="210px"></asp:dropdownlist>
							<asp:label id="lblVarietyThree" style="Z-INDEX: 127; LEFT: 32px; POSITION: absolute; TOP: 1016px"
								runat="server" Width="56px" Height="16px" Font-Names="Arial"> Variety:</asp:label>
							<asp:Panel id="pnlSorgumThree" style="Z-INDEX: 128; LEFT: 328px; POSITION: absolute; TOP: 928px"
								runat="server" Width="456px" Height="200px" Visible="False">
								<DIV title="Sorgum" style="WIDTH: 458px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:label id="lblPopulationThree" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial">Population: </asp:label>
									<asp:textbox id="edtPopulationThree" style="Z-INDEX: 110; LEFT: 168px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="192px"></asp:textbox>
									<asp:label id="lblRowConfigurationThree" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="160px">Row Configuration:</asp:label>
									<asp:dropdownlist id="cboRowConfigurationThree" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="272px"></asp:dropdownlist>
									<asp:Label id="lblRowSpacingThree" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 88px"
										runat="server" Font-Names="Arial" Width="128px">Row Spacing:</asp:Label>
									<asp:TextBox id="edtRowSpacingThree" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial" Width="248px"></asp:TextBox>
									<asp:textbox id="edtTillerThree" style="Z-INDEX: 107; LEFT: 168px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="128px"></asp:textbox>
									<asp:label id="lblTillerThree" style="Z-INDEX: 108; LEFT: 8px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="64px">NFT:</asp:label>
									<asp:CheckBox id="chkAutoCalculateThree" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 160px"
										runat="server" Font-Names="Arial" Height="16px" Width="312px" AutoPostBack="True" Text="Auto Calculate Number of Fertile Tillers"
										TextAlign="Left"></asp:CheckBox>
									<asp:Button id="btnCalculateThree" style="Z-INDEX: 112; LEFT: 304px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="144px" Text="Re-Calculate NFT"></asp:Button>
									<asp:Label id="lblRowSpacingUnitThree" style="Z-INDEX: 120; LEFT: 424px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial">m</asp:Label>
									<asp:label id="lblPopulationUnitThree" style="Z-INDEX: 120; LEFT: 368px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="88px">plants/ha</asp:label></DIV>
							</asp:Panel><asp:label id="lblScenarioOne" style="Z-INDEX: 129; LEFT: 128px; POSITION: absolute; TOP: 512px"
								runat="server" Font-Names="Arial" Height="24px" Width="104px" Font-Bold="True">Scenario One</asp:label>
							<asp:panel id="pnlSorgumOne" style="Z-INDEX: 130; LEFT: 328px; POSITION: absolute; TOP: 512px"
								runat="server" Width="456px" Height="200px" Visible="False">
								<DIV title="Sorgum" style="WIDTH: 457px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:label id="lblPopulationOne" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial">Population: </asp:label>
									<asp:textbox id="edtPopulationOne" style="Z-INDEX: 110; LEFT: 168px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="192px"></asp:textbox>
									<asp:label id="lblRowConfigurationOne" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="160px">Row Configuration:</asp:label>
									<asp:dropdownlist id="cboRowConfigurationOne" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="272px"></asp:dropdownlist>
									<asp:Label id="lblRowSpacingOne" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 88px"
										runat="server" Font-Names="Arial" Width="128px">Row Spacing:</asp:Label>
									<asp:TextBox id="edtRowSpacingOne" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial" Width="248px"></asp:TextBox>
									<asp:textbox id="edtTillerOne" style="Z-INDEX: 107; LEFT: 168px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="128px"></asp:textbox>
									<asp:label id="lblTillerOne" style="Z-INDEX: 108; LEFT: 8px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="64px">NFT:</asp:label>
									<asp:CheckBox id="chkAutoCalculateOne" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 160px"
										runat="server" Font-Names="Arial" Height="16px" Width="312px" AutoPostBack="True" Text="Auto Calculate Number of Fertile Tillers"
										TextAlign="Left"></asp:CheckBox>
									<asp:Button id="btnCalculateOne" style="Z-INDEX: 112; LEFT: 304px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="144px" Text="Re-Calculate NFT"></asp:Button>
									<asp:Label id="lblRowSpacingUnitOne" style="Z-INDEX: 120; LEFT: 424px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial">m</asp:Label>
									<asp:label id="lblPopulationUnitOne" style="Z-INDEX: 120; LEFT: 368px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="88px">plants/ha</asp:label></DIV>
							</asp:panel>
							<asp:Panel id="pnlSorgumTwo" style="Z-INDEX: 131; LEFT: 328px; POSITION: absolute; TOP: 720px"
								runat="server" Width="456px" Height="200px" Visible="False">
								<DIV title="Sorgum" style="WIDTH: 457px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:label id="lblPopulationTwo" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial">Population: </asp:label>
									<asp:textbox id="edtPopulationTwo" style="Z-INDEX: 110; LEFT: 168px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="192px"></asp:textbox>
									<asp:label id="lblRowConfigurationTwo" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="136px">Row Configuration:</asp:label>
									<asp:dropdownlist id="cboRowConfigurationTwo" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="272px"></asp:dropdownlist>
									<asp:Label id="lblRowSpacingTwo" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 88px"
										runat="server" Font-Names="Arial" Width="128px">Row Spacing:</asp:Label>
									<asp:TextBox id="edtRowSpacingTwo" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial" Width="248px"></asp:TextBox>
									<asp:textbox id="edtTillerTwo" style="Z-INDEX: 107; LEFT: 168px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="128px"></asp:textbox>
									<asp:label id="lblTillerTwo" style="Z-INDEX: 108; LEFT: 8px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="64px">NFT:</asp:label>
									<asp:CheckBox id="chkAutoCalculateTwo" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 160px"
										runat="server" Font-Names="Arial" Height="16px" Width="312px" AutoPostBack="True" Text="Auto Calculate Number of Fertile Tillers"
										TextAlign="Left"></asp:CheckBox>
									<asp:Button id="btnCalculateTwo" style="Z-INDEX: 112; LEFT: 304px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="144px" Text="Re-Calculate NFT"></asp:Button>
									<asp:Label id="lblRowSpacingUnitTwo" style="Z-INDEX: 120; LEFT: 424px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial">m</asp:Label>
									<asp:label id="lblPopulationUnitTwo" style="Z-INDEX: 120; LEFT: 368px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="88px">plants/ha</asp:label></DIV>
							</asp:Panel>
							<asp:Panel id="pnlCanolaThree" style="Z-INDEX: 132; LEFT: 328px; POSITION: absolute; TOP: 928px"
								runat="server" Height="200px" Width="456px" Visible="False">
								<DIV title="Canola" style="WIDTH: 456px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:checkbox id="chkTriazineThree" style="Z-INDEX: 115; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Height="20px" Width="160px" Text="Triazine tolerant?" TextAlign="Left"></asp:checkbox></DIV>
							</asp:Panel>
							<asp:Button id="btnGenerate" style="Z-INDEX: 133; LEFT: 256px; POSITION: absolute; TOP: 1152px"
								runat="server" Width="120px" Height="32px" Font-Names="Arial" Text="Generate" tabIndex="10"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 134; LEFT: 416px; POSITION: absolute; TOP: 1152px"
								runat="server" Width="120px" Height="32px" Font-Names="Arial" Text="Cancel" tabIndex="11"></asp:Button>
							<asp:CheckBox id="chkFavourite" style="Z-INDEX: 135; LEFT: 320px; POSITION: absolute; TOP: 1192px"
								runat="server" Font-Names="Arial" Text="Save as favourite?" ForeColor="Black" TextAlign="Left"
								tabIndex="12"></asp:CheckBox>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 136; LEFT: 0px; POSITION: absolute; TOP: 1232px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 137; LEFT: 72px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
