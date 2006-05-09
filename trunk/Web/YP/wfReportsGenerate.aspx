<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="jwgecc" Namespace="Janus.Web.GridEX.EditControls.CalendarCombo" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfReportsGenerate.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfReportsGenerate" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Create New Report(s)</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 624px" height="624" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 803px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 611px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Bold="True" Font-Size="X-Large" ForeColor="MediumBlue" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlConsultant" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="64px" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
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
							<DIV id="divPage" style="Z-INDEX: 103; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Width="785px" Height="20px" Font-Bold="True" Font-Size="Large" ForeColor="DarkGray" Font-Names="Arial Black">Create New Report for GrowerPlaceHolder's</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" Width="800px" Height="48px" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										tabIndex="4" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px"
										Width="104px" CommandName="wfReportsView.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										tabIndex="5" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px"
										Width="104px" CommandName="wfReportsGenerate.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Height="8px"
										Width="152px" CommandName="wfReportsFavourites.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Label id="lblPaddock" style="Z-INDEX: 105; LEFT: 136px; POSITION: absolute; TOP: 240px"
								runat="server" Font-Names="Arial" Height="15pt" Width="128px">Select a Paddock:</asp:Label>
							<jwg:gridEX id="grdUsers" style="Z-INDEX: 106; LEFT: 272px; POSITION: absolute; TOP: 240px"
								runat="server" Width="288px" Height="206px" Font-Names="Arial" BorderStyle="Solid" SelectionMode="MultipleSelection"
								GroupByBoxVisible="False" AllowPaging="Never" Indent="20px" TableSpacing="10px" GroupIndent="20px"
								BorderWidth="1px" ChildLoadingMode="OnPageLoad" GridLineColor="ScrollBar" ScriptsFolderPath="/gridex/scripts"
								EditorsFrameUrl="/gridex/images/blank.html" ImagesFolderPath="/gridex/images" RecordExpandImage="/gridex/images/jsclosed.gif"
								RecordCollapseImage="/gridex/images/jsopen.gif" SelectOnExpand="False" tabIndex="1">
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<GroupIndentFormatStyle BackColor="Control" ForeColor="Black"></GroupIndentFormatStyle>
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
								<RootTable HierarchicalMode="SelfTable">
									<Columns>
										<jwg:GridEXColumn Key="PaddockName" DataMember="Name" DefaultGroupPrefix="Paddock Name:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Paddock Name" Width="263px">
											<CellStyle Width="263px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="CropType" DataMember="CropType" DefaultGroupPrefix="CropType:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="CropType" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
									<SelfReferencingSettings ParentMember="ParentID" ChildMember="ID" AutoSizeColumnOnExpand="True" UseExpandColumn="0"></SelfReferencingSettings>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="ActiveCaption" ForeColor="ControlText" Height="20px"
									VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="Window" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText" Font-Size="8pt"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridEX>
							<asp:DropDownList id="cboReportTypes" style="Z-INDEX: 107; LEFT: 272px; POSITION: absolute; TOP: 504px"
								runat="server" Width="265px" Font-Names="Arial" tabIndex="2"></asp:DropDownList>
							<asp:Label id="lblMultipleSelect" style="Z-INDEX: 108; LEFT: 296px; POSITION: absolute; TOP: 464px"
								runat="server" Width="216px" Height="16px" Font-Size="Smaller" Font-Names="Arial">Hold Ctrl to select multiple paddocks</asp:Label>
							<asp:Label id="lblReportType" style="Z-INDEX: 109; LEFT: 96px; POSITION: absolute; TOP: 504px"
								runat="server" Font-Names="Arial">Select a Report Type:</asp:Label>
							<asp:Button id="btnNext" style="Z-INDEX: 110; LEFT: 456px; POSITION: absolute; TOP: 544px" runat="server"
								Width="80px" Font-Names="Arial" Text="Next" Height="32px" tabIndex="3"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 111; LEFT: 0px; POSITION: absolute; TOP: 592px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:Image id="imgBanner" style="Z-INDEX: 112; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpGrid" style="Z-INDEX: 113; LEFT: 568px; POSITION: absolute; TOP: 240px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpReportType" style="Z-INDEX: 114; LEFT: 544px; POSITION: absolute; TOP: 504px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpReportGeneratePage" style="Z-INDEX: 115; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
