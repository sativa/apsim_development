<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfGenerateFallowReport.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfGenerateFallowReport" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Generate Fallow Report(s)</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 800px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 931px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" Height="64px" Width="800px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
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
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 32px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 0px" runat="server"
									Font-Names="Arial Black" Height="20px" Width="785px" ForeColor="DarkGray" Font-Size="Large"
									Font-Bold="True">Generate Fallow Report</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" Height="48px" Width="800px" BackColor="DarkGray" BorderStyle="None" BorderColor="White">
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
							<asp:Label id="lblReportName" style="Z-INDEX: 105; LEFT: 96px; POSITION: absolute; TOP: 240px"
								runat="server" Width="272px" Height="16px" Font-Names="Arial">Enter a descriptive name for the report:</asp:Label>
							<asp:TextBox id="edtReportName" style="Z-INDEX: 106; LEFT: 96px; POSITION: absolute; TOP: 264px"
								tabIndex="1" runat="server" Width="488px" Height="24px" Font-Names="Arial"></asp:TextBox>
							<asp:Label id="lblSowingDate" style="Z-INDEX: 107; LEFT: 96px; POSITION: absolute; TOP: 312px"
								runat="server" Width="96px" Height="16px" Font-Names="Arial">Sowing date:</asp:Label>
							<jwg:gridEX id=grdSowDate style="Z-INDEX: 108; LEFT: 200px; POSITION: absolute; TOP: 312px" runat="server" Width="212px" Height="20px" ColumnHeaders="False" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsSowDate %>" DataMember="SowDate" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" SendDataKeyValuesToClient="True" Font-Names="Arial" tabIndex=2>
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
								<RootTable DataMember="SowDate" Key="SowDate">
									<Columns>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="SowDate" Width="192px">
											<CellStyle Width="192px"></CellStyle>
										</jwg:GridEXColumn>
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
							</jwg:gridEX>
							<asp:label id="lblCrop" style="Z-INDEX: 109; LEFT: 96px; POSITION: absolute; TOP: 352px" runat="server"
								Width="88px" Height="16px" Font-Names="Arial">Crop type:</asp:label>
							<asp:dropdownlist id="cboCrops" style="Z-INDEX: 110; LEFT: 200px; POSITION: absolute; TOP: 352px"
								tabIndex="3" runat="server" Width="192px" Height="24px" AutoPostBack="True" Font-Names="Arial"></asp:dropdownlist>
							<asp:dropdownlist id="cboVariety" style="Z-INDEX: 111; LEFT: 200px; POSITION: absolute; TOP: 392px"
								tabIndex="4" runat="server" Width="192px" Height="24px" Font-Names="Arial"></asp:dropdownlist>
							<asp:label id="lblVariety" style="Z-INDEX: 112; LEFT: 96px; POSITION: absolute; TOP: 392px"
								runat="server" Width="48px" Height="16px" Font-Names="Arial">Variety:</asp:label>
							<asp:label id="lblNitrogen" style="Z-INDEX: 114; LEFT: 96px; POSITION: absolute; TOP: 432px"
								runat="server" Font-Names="Arial"> Nitrogen fertiliser applications:</asp:label>
							<jwg:gridEX id=grdNitrogen style="Z-INDEX: 115; LEFT: 312px; POSITION: absolute; TOP: 432px" runat="server" Width="322px" Height="128px" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsNitrogen %>" DataMember="Nitrogen" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" AutomaticSort="False" AllowColumnDrag="False" SendDataKeyValuesToClient="True" Font-Names="Arial" tabIndex=5>
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
							</jwg:gridEX>
							<asp:Panel id="pnlSorgum" style="Z-INDEX: 116; LEFT: 96px; POSITION: absolute; TOP: 576px"
								runat="server" Height="200px" Width="600px" Visible="False">
								<DIV title="Sorgum" style="WIDTH: 600px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:label id="lblPopulation" style="Z-INDEX: 101; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial">Population: </asp:label>
									<asp:textbox id="edtPopulation" style="Z-INDEX: 110; LEFT: 168px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="192px"></asp:textbox>
									<asp:label id="lblRowConfiguration" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="160px">Row Configuration:</asp:label>
									<asp:dropdownlist id="cboRowConfiguration" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="272px"></asp:dropdownlist>
									<asp:Label id="lblRowSpacing" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 88px"
										runat="server" Font-Names="Arial" Width="128px">Row Spacing:</asp:Label>
									<asp:TextBox id="edtRowSpacing" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial" Width="248px"></asp:TextBox>
									<asp:textbox id="edtTiller" style="Z-INDEX: 107; LEFT: 168px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="128px"></asp:textbox>
									<asp:label id="lblTiller" style="Z-INDEX: 108; LEFT: 8px; POSITION: absolute; TOP: 120px" runat="server"
										Font-Names="Arial" Width="64px">NFT:</asp:label>
									<asp:CheckBox id="chkAutoCalculate" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 160px"
										runat="server" Font-Names="Arial" Width="312px" Height="16px" AutoPostBack="True" TextAlign="Left"
										Text="Auto Calculate Number of Fertile Tillers"></asp:CheckBox>
									<asp:Button id="btnCalculate" style="Z-INDEX: 104; LEFT: 304px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="144px" Text="Re-Calculate NFT"></asp:Button>
									<asp:Label id="lblRowSpacingUnit" style="Z-INDEX: 104; LEFT: 424px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial">m</asp:Label>
									<asp:label id="lblPopulationUnit" style="Z-INDEX: 104; LEFT: 368px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="88px">plants/ha</asp:label></DIV>
							</asp:Panel>
							<asp:CheckBox id="chkFavourite" style="Z-INDEX: 117; LEFT: 320px; POSITION: absolute; TOP: 848px"
								runat="server" Font-Names="Arial" Text="Save as favourite?" TextAlign="Left" ForeColor="Black"
								tabIndex="8"></asp:CheckBox>
							<asp:Panel id="pnlCanola" style="Z-INDEX: 118; LEFT: 96px; POSITION: absolute; TOP: 576px"
								runat="server" Height="200px" Width="600px" Visible="False">
								<DIV title="Canola" style="WIDTH: 598px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:checkbox id="chkTriazine" style="Z-INDEX: 103; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
										Font-Names="Arial" Width="160px" Height="20px" TextAlign="Left" Text="Triazine tolerant?"></asp:checkbox></DIV>
							</asp:Panel>
							<asp:Button id="btnGenerate" style="Z-INDEX: 119; LEFT: 248px; POSITION: absolute; TOP: 800px"
								runat="server" Font-Names="Arial" Height="32px" Width="120px" Text="Generate" tabIndex="6"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 120; LEFT: 424px; POSITION: absolute; TOP: 800px"
								runat="server" Font-Names="Arial" Height="32px" Width="120px" Text="Cancel" tabIndex="7"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 121; LEFT: 0px; POSITION: absolute; TOP: 912px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 122; LEFT: 72px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
