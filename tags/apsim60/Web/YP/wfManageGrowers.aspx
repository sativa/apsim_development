<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfManageGrowers.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfManageGrowers" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Manage Growers</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 760px" height="760" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 880px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:Label>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" Width="800px" BackColor="MediumBlue" Height="40px" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										tabIndex="7" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="160px" CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										tabIndex="8" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="88px" CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageGrowers" style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="104px" CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnMainMenuConsultant" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" Width="785px" Height="20px" Font-Size="Large" ForeColor="DarkGray"
									Font-Bold="True">ConsultantPlaceHolder's Growers</asp:Label></DIV>
							<jwg:gridEX id="grdUsers" runat="server" Width="400px" Font-Names="Arial" SelectOnExpand="False"
								GroupByBoxVisible="False" ColumnHeaders="False" RecordCollapseImage="/gridex/images/jsopen.gif"
								RecordExpandImage="/gridex/images/jsclosed.gif" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html"
								ScriptsFolderPath="/gridex/scripts" Hierarchical="True" Height="400px" GridLineColor="ScrollBar"
								ChildLoadingMode="OnPageLoad" BorderStyle="Solid" BorderWidth="1px" GroupIndent="20px" TableSpacing="10px"
								Indent="20px" AllowPaging="Never" style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 288px"
								tabIndex="3">
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
										<jwg:GridEXColumn Key="Name" DataMember="Name" DefaultGroupPrefix=":" InvalidValueAction="DiscardChanges"
											NullText="" Width="380px">
											<CellStyle Width="380px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="UserName" DataMember="UserName" DefaultGroupPrefix="UserName:" InvalidValueAction="DiscardChanges"
											Caption="UserName" Width="1px" Visible="False">
											<CellStyle Width="1px"></CellStyle>
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
							<asp:Button id="btnFind" style="Z-INDEX: 108; LEFT: 568px; POSITION: absolute; TOP: 256px" runat="server"
								Font-Names="Arial" Text="Find" tabIndex="2"></asp:Button>
							<asp:Button id="btnEditPaddocks" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 720px"
								runat="server" Width="185px" Text="Edit grower's paddocks" Height="32px" tabIndex="4"></asp:Button>
							<asp:Button id="btnEditReports" style="Z-INDEX: 104; LEFT: 424px; POSITION: absolute; TOP: 720px"
								runat="server" Width="185px" Text="Edit grower's reports" Height="32px" tabIndex="5"></asp:Button>
							<asp:Label id="lblFind" style="Z-INDEX: 106; LEFT: 208px; POSITION: absolute; TOP: 256px" runat="server"
								Font-Names="Arial">User's name:</asp:Label>
							<asp:TextBox id="edtFind" style="Z-INDEX: 107; LEFT: 304px; POSITION: absolute; TOP: 256px" runat="server"
								Font-Names="Arial" Width="256px" tabIndex="1"></asp:TextBox>
							<asp:Panel id="pnlAdminOptions" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 784px"
								runat="server" Height="55px" Width="800px">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 50px" ms_positioning="GridLayout">
									<asp:Button id="btnAddUser" style="Z-INDEX: 101; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="9" runat="server" Height="32px" Width="150px" Text="Add new user"></asp:Button>
									<asp:Button id="btnEditUser" style="Z-INDEX: 102; LEFT: 336px; POSITION: absolute; TOP: 8px"
										tabIndex="10" runat="server" Height="32px" Width="150px" Text="Edit current user"></asp:Button>
									<asp:Button id="btnDeleteUser" style="Z-INDEX: 103; LEFT: 512px; POSITION: absolute; TOP: 8px"
										tabIndex="11" runat="server" Height="32px" Width="150px" Text="Delete current user"></asp:Button></DIV>
							</asp:Panel>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 110; LEFT: 0px; POSITION: absolute; TOP: 856px"
								runat="server" Height="16px" BackColor="MediumBlue" Width="800px"></asp:Panel>
							<asp:Button id="btnEditPaddocksTwo" style="Z-INDEX: 111; LEFT: 208px; POSITION: absolute; TOP: 192px"
								tabIndex="4" runat="server" Height="32px" Width="185px" Text="Edit grower's paddocks"></asp:Button>
							<asp:Button id="btnEditReportsTwo" style="Z-INDEX: 112; LEFT: 424px; POSITION: absolute; TOP: 192px"
								tabIndex="5" runat="server" Height="32px" Width="185px" Text="Edit grower's reports"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 113; LEFT: 64px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpFind" style="Z-INDEX: 114; LEFT: 616px; POSITION: absolute; TOP: 256px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpManageGrowerPage" style="Z-INDEX: 115; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
						</DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
