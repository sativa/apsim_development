<%@ Page language="c#" Codebehind="wfPaddocksRainfall.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPaddocksRainfall" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock's Rainfall</title>
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
					<TD style="WIDTH: 987px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 1035px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" Width="800px" Height="64px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
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
									Font-Names="Arial Black" Width="785px" Height="20px" Font-Size="Medium" ForeColor="DarkGray"
									Font-Bold="True">GrowerPlaceHolder's paddock PaddockPlaceHolder</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" Width="800px" Height="40px" BackColor="DarkGray" BorderStyle="None" BorderColor="White">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 27px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksRainfall" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="7" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="144px" CommandName="wfPaddocksRainfall.aspx">Paddock's Rainfall</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksApplictions" style="Z-INDEX: 101; LEFT: 288px; POSITION: absolute; TOP: 8px"
										tabIndex="9" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="184px" CommandName="wfPaddocksApplications.aspx">Paddock's Applications</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksSoil" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="8" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="120px" CommandName="wfPaddocksSoil.aspx">Paddock's Soil</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksCrop" style="Z-INDEX: 103; LEFT: 480px; POSITION: absolute; TOP: 8px"
										tabIndex="10" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="128px" CommandName="wfPaddocksCrop.aspx">Paddock's Crop</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksInformation" style="Z-INDEX: 104; LEFT: 616px; POSITION: absolute; TOP: 8px"
										tabIndex="1" runat="server" Font-Bold="True" ForeColor="White" Font-Names="Arial" Height="8px"
										Width="176px" CommandName="wfPaddocksInformation.aspx">Paddock's Information</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Label id="lblYear" style="Z-INDEX: 104; LEFT: 192px; POSITION: absolute; TOP: 304px" runat="server"
								Font-Names="Arial" Width="304px" Font-Size="Small" ForeColor="Black">Please select a year to view your rainfall for:</asp:Label>
							<asp:DropDownList id="cboYear" style="Z-INDEX: 105; LEFT: 512px; POSITION: absolute; TOP: 304px" runat="server"
								Font-Names="Arial" Width="64px" AutoPostBack="True" Height="24px" tabIndex="1">
								<asp:ListItem Value="2003">2003</asp:ListItem>
								<asp:ListItem Value="2004">2004</asp:ListItem>
								<asp:ListItem Value="2005">2005</asp:ListItem>
								<asp:ListItem Value="2006">2006</asp:ListItem>
								<asp:ListItem Value="2007">2007</asp:ListItem>
								<asp:ListItem Value="2008">2008</asp:ListItem>
							</asp:DropDownList>
							<jwg:gridex id=grdRainfall style="Z-INDEX: 106; LEFT: 128px; POSITION: absolute; TOP: 336px" runat="server" Height="542px" Width="536px" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" GridLineColor="ScrollBar" AllowEdit="True" DataSource="<%# dsRainfall %>" DataMember="Rainfall" GroupByBoxVisible="False" TotalRow="True" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/images/blank.html" AllowColumnDrag="False" AutomaticSort="False" Font-Names="Arial" tabIndex=2>
								<TotalRowFormatStyle BackColor="MediumBlue" ForeColor="White" Height="15px" Font-Size="X-Small"></TotalRowFormatStyle>
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
								<RootTable DataMember="Rainfall" Key="Rainfall">
									<Columns>
										<jwg:GridEXColumn TotalFormatMode="UseStringFormat" UseType="System.String" EditType="NoEdit" Key="Day"
											DataMember="Day" DefaultGroupPrefix="Day:" InvalidValueAction="DiscardChanges" TotalFormatString="Totals"
											Caption="Day" Width="40px">
											<CellStyle BackColor="DarkGray" Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Jan" DataMember="Jan" DefaultGroupPrefix="Jan:" InvalidValueAction="DiscardChanges"
											Caption="Jan" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Feb" DataMember="Feb" DefaultGroupPrefix="Feb:" InvalidValueAction="DiscardChanges"
											Caption="Feb" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Mar" DataMember="Mar" DefaultGroupPrefix="Mar:" InvalidValueAction="DiscardChanges"
											Caption="Mar" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Apr" DataMember="Apr" DefaultGroupPrefix="Apr:" InvalidValueAction="DiscardChanges"
											Caption="Apr" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="May" DataMember="May" DefaultGroupPrefix="May:" InvalidValueAction="DiscardChanges"
											Caption="May" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Jun" DataMember="Jun" DefaultGroupPrefix="Jun:" InvalidValueAction="DiscardChanges"
											Caption="Jun" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Jul" DataMember="Jul" DefaultGroupPrefix="Jul:" InvalidValueAction="DiscardChanges"
											Caption="Jul" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Aug" DataMember="Aug" DefaultGroupPrefix="Aug:" InvalidValueAction="DiscardChanges"
											Caption="Aug" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Sep" DataMember="Sep" DefaultGroupPrefix="Sep:" InvalidValueAction="DiscardChanges"
											Caption="Sep" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Oct" DataMember="Oct" DefaultGroupPrefix="Oct:" InvalidValueAction="DiscardChanges"
											Caption="Oct" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Nov" DataMember="Nov" DefaultGroupPrefix="Nov:" InvalidValueAction="DiscardChanges"
											Caption="Nov" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Double" Key="Dec" DataMember="Dec" DefaultGroupPrefix="Dec:" InvalidValueAction="DiscardChanges"
											Caption="Dec" Width="40px" AggregateFunction="Sum">
											<CellStyle Width="40px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="DarkGray" ForeColor="ControlText" Height="16px" Appearance="RaisedLight"
									BorderWidth="1px" Font-Size="X-Small" BorderColor="Control"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="15px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="X-Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:Label id="lblTotalRainfall" style="Z-INDEX: 107; LEFT: 272px; POSITION: absolute; TOP: 880px"
								runat="server" Height="16px" Width="310px" Font-Names="Arial">Total rainfall for the year from the 1st of April:</asp:Label>
							<asp:TextBox id="edtTotalRainfall" style="Z-INDEX: 108; LEFT: 584px; POSITION: absolute; TOP: 880px"
								runat="server" Height="24px" Width="56px" ReadOnly="True" Font-Names="Arial" tabIndex="3"></asp:TextBox>
							<asp:TextBox id="edtTotalRainfallTwo" style="Z-INDEX: 109; LEFT: 584px; POSITION: absolute; TOP: 912px"
								runat="server" Height="24px" Width="56px" ReadOnly="True" Font-Names="Arial" tabIndex="4"></asp:TextBox>
							<asp:Label id="lblTotalRainfallTwo" style="Z-INDEX: 110; LEFT: 360px; POSITION: absolute; TOP: 912px"
								runat="server" Height="16px" Width="217px" Font-Names="Arial">Total rainfall for the whole year:</asp:Label>
							<asp:Button id="btnSave" style="Z-INDEX: 112; LEFT: 256px; POSITION: absolute; TOP: 960px" runat="server"
								Text="Save changes" Height="32px" Width="120px" tabIndex="5"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 111; LEFT: 408px; POSITION: absolute; TOP: 960px"
								runat="server" Text="Cancel changes" Height="32px" Width="120px" tabIndex="6"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 113; LEFT: 0px; POSITION: absolute; TOP: 1016px"
								runat="server" BackColor="MediumBlue" Height="16px" Width="800px"></asp:Panel>
							<asp:Button id="btnSaveTwo" style="Z-INDEX: 114; LEFT: 256px; POSITION: absolute; TOP: 240px"
								tabIndex="9" runat="server" Height="32px" Width="120px" Text="Save changes"></asp:Button>
							<asp:Button id="btnCancelTwo" style="Z-INDEX: 115; LEFT: 432px; POSITION: absolute; TOP: 240px"
								tabIndex="11" runat="server" Height="32px" Width="120px" Text="Cancel changes"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 116; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="imgHelpYear" style="Z-INDEX: 117; LEFT: 584px; POSITION: absolute; TOP: 304px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpRainfallGrid" style="Z-INDEX: 118; LEFT: 672px; POSITION: absolute; TOP: 336px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpPaddockRainfallPage" style="Z-INDEX: 119; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="imgHelpRainfall1April" style="Z-INDEX: 120; LEFT: 648px; POSITION: absolute; TOP: 880px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpRaifallYear" style="Z-INDEX: 121; LEFT: 648px; POSITION: absolute; TOP: 912px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
						</DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
