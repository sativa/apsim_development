<%@ Page language="c#" Codebehind="wfPaddocksInformation.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPaddocksInformation" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock's Information</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 643px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" Height="64px" Width="800px" BorderColor="White" BorderStyle="None">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
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
									<asp:LinkButton id="btnMainMenuConsultant" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" Font-Size="Medium" ForeColor="DarkGray"
									Font-Bold="True">GrowerPlaceHolder's paddock PaddockPlaceHolder</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" Height="40px" Width="800px" BorderColor="White" BorderStyle="None">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksRainfall" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="10" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="144px"
										Height="8px" CommandName="wfPaddocksRainfall.aspx">Paddock's Rainfall</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksApplictions" style="Z-INDEX: 101; LEFT: 288px; POSITION: absolute; TOP: 8px"
										tabIndex="12" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="184px"
										Height="8px" CommandName="wfPaddocksApplications.aspx">Paddock's Applications</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksSoil" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="11" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="120px"
										Height="8px" CommandName="wfPaddocksSoil.aspx">Paddock's Soil</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksCrop" style="Z-INDEX: 103; LEFT: 480px; POSITION: absolute; TOP: 8px"
										tabIndex="13" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="128px"
										Height="8px" CommandName="wfPaddocksCrop.aspx">Paddock's Crop</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksInformation" style="Z-INDEX: 104; LEFT: 616px; POSITION: absolute; TOP: 8px"
										tabIndex="14" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="176px"
										Height="8px" CommandName="wfPaddocksInformation.aspx">Paddock's Information</asp:LinkButton></DIV>
							</asp:panel><asp:label id="lblPaddockName" style="Z-INDEX: 104; LEFT: 128px; POSITION: absolute; TOP: 304px"
								runat="server" Font-Names="Arial">Paddock name:</asp:label><asp:textbox id="edtPaddockName" style="Z-INDEX: 105; LEFT: 360px; POSITION: absolute; TOP: 304px"
								runat="server" Width="336px" Font-Names="Arial" tabIndex="1"></asp:textbox>
							<jwg:gridex id=grdStartOfGrowingSeason style="Z-INDEX: 106; LEFT: 360px; POSITION: absolute; TOP: 344px" runat="server" Height="20px" Width="168px" Font-Names="Arial" ColumnHeaders="False" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="StartOfGrowingSeason" DataSource="<%# dsStartOfGrowingSeason %>" tabIndex=2>
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
								<RootTable DataMember="StartOfGrowingSeason" Key="SowDate">
									<Columns>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="GrowingSeasonDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="GrowingSeasonDate" DefaultGroupPrefix="GrowingSeasonDate:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="GrowingSeasonDate" Width="148px">
											<CellStyle Width="148px"></CellStyle>
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
									VerticalAlign="top" BorderWidth="1px" Font-Names="Garamond"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:Button id="btnSave" style="Z-INDEX: 115; LEFT: 240px; POSITION: absolute; TOP: 568px" runat="server"
								Text="Save changes" tabIndex="8" Width="120px" Height="32px"></asp:Button>
							<asp:label id="lblStartGrowingSeason" style="Z-INDEX: 107; LEFT: 128px; POSITION: absolute; TOP: 344px"
								runat="server" Font-Names="Arial">Start of growing season:</asp:label><asp:label id="lbRegion" style="Z-INDEX: 108; LEFT: 128px; POSITION: absolute; TOP: 384px"
								runat="server" Width="48px" Height="16px" Font-Names="Arial">Region:</asp:label>
							<asp:dropdownlist id="cboRegion" style="Z-INDEX: 109; LEFT: 360px; POSITION: absolute; TOP: 384px"
								tabIndex="3" runat="server" Height="24px" Width="344px" Font-Names="Arial" AutoPostBack="True"></asp:dropdownlist>
							<asp:dropdownlist id="cboWeatherStation" style="Z-INDEX: 110; LEFT: 360px; POSITION: absolute; TOP: 424px"
								tabIndex="4" runat="server" Height="24px" Width="344px" Font-Names="Arial"></asp:dropdownlist><asp:label id="lblWeatherStation" style="Z-INDEX: 111; LEFT: 128px; POSITION: absolute; TOP: 424px"
								runat="server" Width="168px" Height="16px" Font-Names="Arial">Closest weather station:</asp:label>
							<asp:checkbox id="chkLinkedRainfall" style="Z-INDEX: 112; LEFT: 128px; POSITION: absolute; TOP: 464px"
								runat="server" Font-Names="Arial" AutoPostBack="True" Text="Link rainfall to existing paddock?"
								TextAlign="Left" tabIndex="5"></asp:checkbox>
							<asp:dropdownlist id="cboLinkedRainfall" style="Z-INDEX: 113; LEFT: 384px; POSITION: absolute; TOP: 464px"
								tabIndex="6" runat="server" Height="24px" Width="322px" Font-Names="Arial"></asp:dropdownlist><asp:checkbox id="chkDefaultRainfall" style="Z-INDEX: 114; LEFT: 128px; POSITION: absolute; TOP: 504px"
								runat="server" Width="232px" Font-Names="Arial" Text=" Use weather station's rainfall?    " AutoPostBack="True" TextAlign="Left" tabIndex="7"></asp:checkbox>
							<asp:Button id="btnCancel" style="Z-INDEX: 116; LEFT: 424px; POSITION: absolute; TOP: 568px"
								runat="server" Text="Cancel changes" tabIndex="9" Width="120px" Height="32px"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 117; LEFT: 0px; POSITION: absolute; TOP: 624px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Button id="btnSaveTwo" style="Z-INDEX: 118; LEFT: 240px; POSITION: absolute; TOP: 240px"
								tabIndex="9" runat="server" Width="120px" Height="32px" Text="Save changes"></asp:Button>
							<asp:Button id="btnCancelTwo" style="Z-INDEX: 119; LEFT: 424px; POSITION: absolute; TOP: 240px"
								tabIndex="11" runat="server" Width="120px" Height="32px" Text="Cancel changes"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 120; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="imgHelpPaddockName" style="Z-INDEX: 121; LEFT: 712px; POSITION: absolute; TOP: 304px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpStartOfGrowingSeason" style="Z-INDEX: 122; LEFT: 536px; POSITION: absolute; TOP: 344px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpRegion" style="Z-INDEX: 124; LEFT: 712px; POSITION: absolute; TOP: 384px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpWeatherStation" style="Z-INDEX: 125; LEFT: 712px; POSITION: absolute; TOP: 424px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpLinkRainfall" style="Z-INDEX: 126; LEFT: 712px; POSITION: absolute; TOP: 464px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpWeatherStationRainfall" style="Z-INDEX: 127; LEFT: 368px; POSITION: absolute; TOP: 504px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpPaddockInformationPage" style="Z-INDEX: 128; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
