<%@ Page language="c#" Codebehind="wfPaddocksCrop.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPaddocksCrop" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock's Crop</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 763px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" Height="64px" Width="800px" BorderStyle="None" BorderColor="White">
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
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Medium"
									Font-Bold="True">Crop for GrowerPlaceHolder's paddock PaddockPlaceHolder</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" Height="40px" Width="800px" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksRainfall" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="14" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="144px"
										Height="8px" CommandName="wfPaddocksRainfall.aspx">Paddock's Rainfall</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksApplictions" style="Z-INDEX: 101; LEFT: 288px; POSITION: absolute; TOP: 8px"
										tabIndex="16" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="184px"
										Height="8px" CommandName="wfPaddocksApplications.aspx">Paddock's Applications</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksSoil" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="15" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="120px"
										Height="8px" CommandName="wfPaddocksSoil.aspx">Paddock's Soil</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksCrop" style="Z-INDEX: 103; LEFT: 480px; POSITION: absolute; TOP: 8px"
										tabIndex="17" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="128px"
										Height="8px" CommandName="wfPaddocksCrop.aspx">Paddock's Crop</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksInformation" style="Z-INDEX: 104; LEFT: 616px; POSITION: absolute; TOP: 8px"
										tabIndex="18" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="176px"
										Height="8px" CommandName="wfPaddocksInformation.aspx">Paddock's Information</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Panel id="pnlCanola" style="Z-INDEX: 104; LEFT: 104px; POSITION: absolute; TOP: 456px"
								runat="server" Height="200px" Width="600px" Visible="False">
								<DIV id="divCanola" title="Canola" style="WIDTH: 598px; POSITION: relative; HEIGHT: 184px"
									ms_positioning="GridLayout">
									<asp:checkbox id="chkTriazine" style="Z-INDEX: 115; LEFT: 8px; POSITION: absolute; TOP: 8px" tabIndex="11"
										runat="server" Font-Names="Arial" Width="144px" Height="20px" TextAlign="Left" Text="Triazine tolerant?"></asp:checkbox></DIV>
							</asp:Panel>
							<asp:Panel id="pnlSorgum" style="Z-INDEX: 105; LEFT: 104px; POSITION: absolute; TOP: 456px"
								runat="server" Height="200px" Width="600px" Visible="False">
								<DIV id="divSorghum" title="Sorgum" style="WIDTH: 600px; POSITION: relative; HEIGHT: 184px"
									ms_positioning="GridLayout">
									<asp:label id="lblPopulation" style="Z-INDEX: 101; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="104px">Population: </asp:label>
									<asp:textbox id="edtPopulation" style="Z-INDEX: 110; LEFT: 168px; POSITION: absolute; TOP: 8px"
										tabIndex="5" runat="server" Font-Names="Arial" Width="192px"></asp:textbox>
									<asp:label id="lblPopulationUnit" style="Z-INDEX: 109; LEFT: 376px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Names="Arial" Width="64px">plants/ha</asp:label>
									<asp:label id="lblRowConfiguration" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 48px"
										runat="server" Font-Names="Arial" Width="160px">Row configuration:</asp:label>
									<asp:dropdownlist id="cboRowConfiguration" style="Z-INDEX: 104; LEFT: 168px; POSITION: absolute; TOP: 48px"
										tabIndex="6" runat="server" Font-Names="Arial" Width="272px"></asp:dropdownlist>
									<asp:Label id="lblRowSpacing" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 88px"
										runat="server" Font-Names="Arial" Width="120px">Row spacing:</asp:Label>
									<asp:TextBox id="edtRowSpacing" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 80px"
										tabIndex="7" runat="server" Font-Names="Arial" Width="248px"></asp:TextBox>
									<asp:textbox id="edtTiller" style="Z-INDEX: 107; LEFT: 168px; POSITION: absolute; TOP: 120px"
										tabIndex="8" runat="server" Font-Names="Arial" Width="128px"></asp:textbox>
									<asp:label id="lblTiller" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 120px"
										runat="server" Font-Names="Arial" Width="56px">NFT:</asp:label>
									<asp:Button id="btnCalculate" style="Z-INDEX: 102; LEFT: 304px; POSITION: absolute; TOP: 120px"
										tabIndex="9" runat="server" Font-Names="Arial" Width="144px" Text="Re-Calculate NFT"></asp:Button>
									<asp:CheckBox id="chkAutoCalculate" style="Z-INDEX: 100; LEFT: 16px; POSITION: absolute; TOP: 160px"
										tabIndex="10" runat="server" Font-Names="Arial" Width="312px" Height="16px" TextAlign="Left"
										Text="Auto calculate number of fertile tillers" AutoPostBack="True"></asp:CheckBox>
									<asp:Label id="lblRowSpacingUnit" style="Z-INDEX: 112; LEFT: 424px; POSITION: absolute; TOP: 80px"
										runat="server" Font-Names="Arial">m</asp:Label></DIV>
							</asp:Panel>
							<asp:dropdownlist id="cboCultivars" style="Z-INDEX: 106; LEFT: 344px; POSITION: absolute; TOP: 400px"
								tabIndex="4" runat="server" Height="24px" Width="264px" Font-Names="Arial"></asp:dropdownlist>
							<asp:label id="lblCultivar" style="Z-INDEX: 107; LEFT: 152px; POSITION: absolute; TOP: 408px"
								runat="server" Height="16px" Width="104px" Font-Names="Arial">Cultivar type:</asp:label>
							<asp:label id="lblCrop" style="Z-INDEX: 108; LEFT: 152px; POSITION: absolute; TOP: 352px" runat="server"
								Height="16px" Width="80px" Font-Names="Arial">Crop type:</asp:label>
							<asp:dropdownlist id="cboCrops" style="Z-INDEX: 109; LEFT: 344px; POSITION: absolute; TOP: 352px"
								tabIndex="3" runat="server" Height="24px" Width="264px" Font-Names="Arial" AutoPostBack="True"></asp:dropdownlist>
							<asp:checkbox id="chkSown" style="Z-INDEX: 110; LEFT: 152px; POSITION: absolute; TOP: 304px" tabIndex="1"
								runat="server" Height="16px" Width="160px" Font-Names="Arial" Text="Have you sown yet?" AutoPostBack="True"
								TextAlign="Left"></asp:checkbox>
							<jwg:gridex id=grdSowDate style="Z-INDEX: 111; LEFT: 344px; POSITION: absolute; TOP: 304px" runat="server" Height="20px" Width="280px" Font-Names="Arial" ColumnHeaders="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="SowDate" DataSource="<%# dsSowDate %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" tabIndex=2>
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
							<asp:Button id="btnSave" style="Z-INDEX: 112; LEFT: 256px; POSITION: absolute; TOP: 688px" runat="server"
								Text="Save changes" Width="120px" Height="32px" tabIndex="12"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 113; LEFT: 424px; POSITION: absolute; TOP: 688px"
								runat="server" Text="Cancel changes" Width="120px" Height="32px" tabIndex="13"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 115; LEFT: 0px; POSITION: absolute; TOP: 744px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Button id="btnSaveTwo" style="Z-INDEX: 116; LEFT: 248px; POSITION: absolute; TOP: 240px"
								tabIndex="9" runat="server" Width="120px" Height="32px" Text="Save changes"></asp:Button>
							<asp:Button id="btnCancelTwo" style="Z-INDEX: 117; LEFT: 416px; POSITION: absolute; TOP: 240px"
								tabIndex="11" runat="server" Width="120px" Height="32px" Text="Cancel changes"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 118; LEFT: 88px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="imgHelpSowDate" style="Z-INDEX: 119; LEFT: 632px; POSITION: absolute; TOP: 304px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpCrop" style="Z-INDEX: 120; LEFT: 632px; POSITION: absolute; TOP: 352px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpCultivar" style="Z-INDEX: 121; LEFT: 632px; POSITION: absolute; TOP: 400px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpPaddockCropPage" style="Z-INDEX: 122; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
