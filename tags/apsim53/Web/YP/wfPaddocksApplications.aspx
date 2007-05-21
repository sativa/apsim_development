<%@ Page language="c#" Codebehind="wfPaddocksApplications.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPaddocksApplications" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock's Applications</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 787px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
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
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Width="785px" Height="20px" Font-Names="Arial Black" Font-Size="Medium" ForeColor="DarkGray"
									Font-Bold="True">Applications for GrowerPlaceHolder's paddock PaddockPlaceHolder</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" Width="800px" Height="40px" BackColor="DarkGray" BorderColor="White" BorderStyle="None">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksRainfall" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="5" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="144px" CommandName="wfPaddocksRainfall.aspx">Paddock's Rainfall</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksApplictions" style="Z-INDEX: 101; LEFT: 288px; POSITION: absolute; TOP: 8px"
										tabIndex="7" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="184px" CommandName="wfPaddocksApplications.aspx">Paddock's Applications</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksSoil" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="6" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="120px" CommandName="wfPaddocksSoil.aspx">Paddock's Soil</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksCrop" style="Z-INDEX: 103; LEFT: 480px; POSITION: absolute; TOP: 8px"
										tabIndex="8" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="128px" CommandName="wfPaddocksCrop.aspx">Paddock's Crop</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksInformation" style="Z-INDEX: 104; LEFT: 616px; POSITION: absolute; TOP: 8px"
										tabIndex="9" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="176px" CommandName="wfPaddocksInformation.aspx">Paddock's Information</asp:LinkButton></DIV>
							</asp:panel>
							<jwg:gridex id=grdIrrigation style="Z-INDEX: 104; LEFT: 200px; POSITION: absolute; TOP: 464px" runat="server" Width="488px" Height="228px" Font-Names="Arial" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsIrrigation %>" DataMember="Irrigation" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" AutomaticSort="False" AllowColumnDrag="False" tabIndex=2>
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
								<RootTable DataMember="Irrigation" Key="Irrigation">
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
											InvalidValueAction="DiscardChanges" NullText="" Caption="Application Amount (mm/ha)" Width="205px">
											<CellStyle Width="205px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Efficency" HasValueList="True" DataMember="Efficency"
											DefaultGroupPrefix="Efficency (%):" InvalidValueAction="DiscardChanges" NullText="" Caption="Efficiency (%)"
											Width="135px">
											<CellStyle Width="135px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="DarkGray" ForeColor="White" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<jwg:gridex id=grdNitrogen style="Z-INDEX: 105; LEFT: 280px; POSITION: absolute; TOP: 312px" runat="server" Width="328px" Height="136px" Font-Names="Arial" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="Nitrogen" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" tabIndex=1>
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
								<HeaderFormatStyle BorderStyle="Solid" BackColor="DarkGray" ForeColor="White" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"
									Padding="0"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:label id="lblIrrigation" style="Z-INDEX: 106; LEFT: 40px; POSITION: absolute; TOP: 472px"
								runat="server" Font-Names="Arial">Irrigation applications:</asp:label>
							<asp:label id="lblNitrogen" style="Z-INDEX: 107; LEFT: 40px; POSITION: absolute; TOP: 312px"
								runat="server" Font-Names="Arial"> Nitrogen fertiliser applications:</asp:label>
							<asp:Button id="btnSave" style="Z-INDEX: 109; LEFT: 264px; POSITION: absolute; TOP: 712px" runat="server"
								Text="Save changes" Height="32px" Width="120px" tabIndex="3"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 108; LEFT: 416px; POSITION: absolute; TOP: 712px"
								runat="server" Text="Cancel changes" Height="32px" Width="120px" tabIndex="4"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 110; LEFT: 0px; POSITION: absolute; TOP: 768px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:Button id="btnSaveTwo" style="Z-INDEX: 111; LEFT: 264px; POSITION: absolute; TOP: 248px"
								tabIndex="9" runat="server" Height="32px" Width="120px" Text="Save changes"></asp:Button>
							<asp:Button id="btnCancelTwo" style="Z-INDEX: 112; LEFT: 416px; POSITION: absolute; TOP: 248px"
								tabIndex="11" runat="server" Height="32px" Width="120px" Text="Cancel changes"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 113; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="imgHelpNitrogen" style="Z-INDEX: 115; LEFT: 624px; POSITION: absolute; TOP: 312px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpIrrigation" style="Z-INDEX: 116; LEFT: 704px; POSITION: absolute; TOP: 464px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpPaddockApplicationsPage" style="Z-INDEX: 117; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
						</DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
