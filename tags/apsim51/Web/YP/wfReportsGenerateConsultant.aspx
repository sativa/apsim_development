<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfReportsGenerateConsultant.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfReportsGenerateConsultant" %>
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
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 763px" height="763" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 747px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="48px" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="160px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="88px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageGrowers" style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="104px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Width="785px" Height="20px" Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Large"
									Font-Bold="True">Create New Report for ConsultantPlaceHolder's</asp:Label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 160px"
								runat="server" BackColor="DarkGray" Width="800px" Height="48px" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnReportsView" style="Z-INDEX: 100; LEFT: 136px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="104px" CommandName="wfReportsViewConsultant.aspx">View Reports</asp:LinkButton>
									<asp:LinkButton id="btnNewReports" style="Z-INDEX: 101; LEFT: 352px; POSITION: absolute; TOP: 8px"
										tabIndex="7" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="104px" CommandName="wfReportsGenerateConsultant.aspx">New Reports</asp:LinkButton>
									<asp:LinkButton id="btnFavouriteReports" style="Z-INDEX: 104; LEFT: 536px; POSITION: absolute; TOP: 8px"
										tabIndex="8" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="152px" CommandName="wfReportsFavouritesConsultant.aspx">Reports Favourites</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Label id="lblMultipleSelect" style="Z-INDEX: 104; LEFT: 264px; POSITION: absolute; TOP: 608px"
								runat="server" Width="264px" Height="16px" Font-Size="Smaller" Font-Names="Arial">Hold Ctrl to select multiple growers/paddocks</asp:Label>
							<jwg:gridEX id="grdUsers" style="Z-INDEX: 105; LEFT: 144px; POSITION: absolute; TOP: 296px"
								runat="server" Width="500px" Height="291px" Font-Names="Arial" BorderStyle="Solid" SelectOnExpand="False"
								RecordCollapseImage="/gridex/images/jsopen.gif" RecordExpandImage="/gridex/images/jsclosed.gif"
								ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts"
								Hierarchical="True" GridLineColor="ScrollBar" ChildLoadingMode="OnPageLoad" BorderWidth="1px"
								GroupIndent="20px" TableSpacing="10px" Indent="20px" AllowPaging="Never" GroupByBoxVisible="False"
								SelectionMode="MultipleSelection" tabIndex="3">
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
										<jwg:GridEXColumn Key="Name" DataMember="Name" DefaultGroupPrefix="User's Name:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="User's Name" Width="240px">
											<CellStyle Width="240px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="UserName" DataMember="UserName" DefaultGroupPrefix="UserName:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="UserName" Width="1px" Visible="False">
											<CellStyle Width="1px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="PaddockName" DataMember="PaddockName" DefaultGroupPrefix="Paddock Name:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Paddock Name" Width="240px">
											<CellStyle Width="240px"></CellStyle>
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
							<asp:Label id="lblReportType" style="Z-INDEX: 106; LEFT: 152px; POSITION: absolute; TOP: 640px"
								runat="server" Font-Names="Arial">Select a Report Type:</asp:Label>
							<asp:DropDownList id="cboReportTypes" style="Z-INDEX: 107; LEFT: 312px; POSITION: absolute; TOP: 640px"
								runat="server" Width="330px" Font-Names="Arial" tabIndex="4"></asp:DropDownList>
							<asp:Button id="btnFind" style="Z-INDEX: 112; LEFT: 560px; POSITION: absolute; TOP: 232px" runat="server"
								Font-Names="Arial" Text="Find" tabIndex="2"></asp:Button>
							<asp:Button id="btnNext" style="Z-INDEX: 108; LEFT: 560px; POSITION: absolute; TOP: 680px" runat="server"
								Width="80px" Font-Names="Arial" Text="Next" Height="32px" tabIndex="5"></asp:Button>
							<asp:Label id="lblGrowers" style="Z-INDEX: 109; LEFT: 144px; POSITION: absolute; TOP: 272px"
								runat="server" Font-Names="Arial">Select growers/paddocks:</asp:Label>
							<asp:Label id="lblFind" style="Z-INDEX: 110; LEFT: 192px; POSITION: absolute; TOP: 232px" runat="server"
								Font-Names="Arial">User's name:</asp:Label>
							<asp:TextBox id="edtFind" style="Z-INDEX: 111; LEFT: 296px; POSITION: absolute; TOP: 232px" runat="server"
								Font-Names="Arial" Width="256px" tabIndex="1"></asp:TextBox>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 113; LEFT: 0px; POSITION: absolute; TOP: 728px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:CheckBox id="chkSelectAll" style="Z-INDEX: 114; LEFT: 296px; POSITION: absolute; TOP: 688px"
								runat="server" Font-Names="Arial" Text="Select all paddocks" TextAlign="Left" AutoPostBack="True"></asp:CheckBox>
							<asp:Image id="imgBanner" style="Z-INDEX: 115; LEFT: 64px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpFind" style="Z-INDEX: 116; LEFT: 608px; POSITION: absolute; TOP: 232px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpGrid" style="Z-INDEX: 117; LEFT: 648px; POSITION: absolute; TOP: 304px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpConsultantReportGeneratePage" style="Z-INDEX: 118; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpReportType" style="Z-INDEX: 119; LEFT: 648px; POSITION: absolute; TOP: 640px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpSelectAll" style="Z-INDEX: 134; LEFT: 456px; POSITION: absolute; TOP: 688px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
