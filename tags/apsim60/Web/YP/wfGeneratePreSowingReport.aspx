<%@ Page language="c#" Codebehind="wfGeneratePreSowingReport.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfGeneratePreSowingReport" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Generate Pre-Sowing Report</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 776px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" Height="64px" Width="800px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="160px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="88px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="104px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="382px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="368px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Large" Font-Bold="True" Height="20px"
									Width="785px">Generate General Report</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" Height="48px" Width="800px" BackColor="DarkGray" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="104px"
										CommandName="wfReportsView.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="104px"
										CommandName="wfReportsGenerate.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px" Width="152px"
										CommandName="wfReportsFavourites.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Label id="lblReportName" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 240px"
								runat="server" Font-Names="Arial" Height="16px" Width="272px">Enter a descriptive name for the report:</asp:Label>
							<asp:TextBox id="edtReportName" style="Z-INDEX: 107; LEFT: 24px; POSITION: absolute; TOP: 272px"
								tabIndex="1" runat="server" Font-Names="Arial" Height="24px" Width="400px"></asp:TextBox>
							<asp:Button id="btnGenerate" style="Z-INDEX: 109; LEFT: 264px; POSITION: absolute; TOP: 680px"
								tabIndex="2" runat="server" Font-Names="Arial" Height="32px" Width="120px" Text="Generate"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 110; LEFT: 416px; POSITION: absolute; TOP: 680px"
								tabIndex="3" runat="server" Font-Names="Arial" Height="32px" Width="120px" Text="Cancel"></asp:Button>
							<asp:CheckBox id="chkFavourite" style="Z-INDEX: 112; LEFT: 320px; POSITION: absolute; TOP: 728px"
								tabIndex="4" runat="server" Font-Names="Arial" ForeColor="Black" Text="Save as favourite?"
								TextAlign="Left"></asp:CheckBox>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 115; LEFT: 0px; POSITION: absolute; TOP: 760px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 116; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<jwg:gridex id=grdSowDate style="Z-INDEX: 123; LEFT: 200px; POSITION: absolute; TOP: 320px" tabIndex=3 runat="server" Font-Names="Arial" Height="20px" Width="270px" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="SowDate" DataSource="<%# dsSowDate %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" ColumnHeaders="False">
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
											NullText="" Caption="SowDate" Width="248px">
											<CellStyle Width="248px"></CellStyle>
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
							</jwg:gridex>
							<asp:label id="lblCrop" style="Z-INDEX: 124; LEFT: 24px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Height="16px" Width="80px">Crop type:</asp:label>
							<asp:dropdownlist id="cboCrops" style="Z-INDEX: 125; LEFT: 200px; POSITION: absolute; TOP: 360px"
								tabIndex="4" runat="server" Font-Names="Arial" Height="24px" Width="184px" AutoPostBack="True"></asp:dropdownlist>
							<asp:dropdownlist id="cboCultivars" style="Z-INDEX: 126; LEFT: 200px; POSITION: absolute; TOP: 400px"
								tabIndex="5" runat="server" Font-Names="Arial" Height="24px" Width="184px"></asp:dropdownlist>
							<asp:label id="lblCultivar" style="Z-INDEX: 127; LEFT: 24px; POSITION: absolute; TOP: 400px"
								runat="server" Font-Names="Arial" Height="16px" Width="96px">Cultivar type:</asp:label>
							<asp:Panel id="pnlSorgum" style="Z-INDEX: 128; LEFT: 24px; POSITION: absolute; TOP: 448px"
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
										runat="server" Font-Names="Arial" Height="16px" Width="312px" Text="Auto Calculate Number of Fertile Tillers"
										TextAlign="Left" AutoPostBack="True"></asp:CheckBox>
									<asp:Button id="btnCalculate" style="Z-INDEX: 112; LEFT: 304px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="144px" Text="Re-Calculate NFT"></asp:Button>
									<asp:Label id="lblRowSpacingUnit" style="Z-INDEX: 120; LEFT: 424px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial">m</asp:Label>
									<asp:label id="lblPopulationUnit" style="Z-INDEX: 120; LEFT: 368px; POSITION: absolute; TOP: 16px"
										runat="server" Font-Names="Arial" Width="88px">plants/ha</asp:label></DIV>
							</asp:Panel>
							<asp:Panel id="pnlCanola" style="Z-INDEX: 129; LEFT: 24px; POSITION: absolute; TOP: 448px"
								runat="server" Height="200px" Width="600px" Visible="False">
								<DIV title="Canola" style="WIDTH: 598px; POSITION: relative; HEIGHT: 184px" ms_positioning="GridLayout">
									<asp:checkbox id="chkTriazine" style="Z-INDEX: 115; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
										Font-Names="Arial" Height="20px" Width="160px" Text="Triazine tolerant?" TextAlign="Left"></asp:checkbox></DIV>
							</asp:Panel>
							<asp:Label id="lblSowingDate" style="Z-INDEX: 130; LEFT: 24px; POSITION: absolute; TOP: 320px"
								runat="server" Font-Names="Arial" Height="16px" Width="96px">Sowing date:</asp:Label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
