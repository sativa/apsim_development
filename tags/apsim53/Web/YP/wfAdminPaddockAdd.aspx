<%@ Page language="c#" Codebehind="wfAdminPaddockAdd.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminPaddockAdd" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Add New Paddock</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 571px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" ForeColor="MediumBlue" Font-Names="Arial Black" Font-Bold="True"> Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 101; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Size="Large" ForeColor="DarkGray" Font-Names="Arial Black"
									Font-Bold="True">Add GrowerPlaceHolder's New Paddock</asp:label></DIV>
							<asp:Button id="btnSave" style="Z-INDEX: 104; LEFT: 264px; POSITION: absolute; TOP: 480px" tabIndex="8"
								runat="server" Height="32px" Width="120px" Text="Save changes"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 103; LEFT: 408px; POSITION: absolute; TOP: 480px"
								tabIndex="9" runat="server" Height="32px" Width="120px" Text="Cancel changes"></asp:Button>
							<asp:checkbox id="chkDefaultRainfall" style="Z-INDEX: 105; LEFT: 64px; POSITION: absolute; TOP: 416px"
								tabIndex="7" runat="server" Width="232px" Text=" Use weather station's rainfall?    " Font-Names="Arial"
								AutoPostBack="True" TextAlign="Left"></asp:checkbox>
							<asp:dropdownlist id="cboLinkedRainfall" style="Z-INDEX: 106; LEFT: 320px; POSITION: absolute; TOP: 376px"
								tabIndex="6" runat="server" Height="24px" Width="322px" Font-Names="Arial"></asp:dropdownlist>
							<asp:checkbox id="chkLinkedRainfall" style="Z-INDEX: 107; LEFT: 64px; POSITION: absolute; TOP: 376px"
								tabIndex="5" runat="server" Text="Link rainfall to existing paddock?" Font-Names="Arial" AutoPostBack="True"
								TextAlign="Left"></asp:checkbox>
							<asp:label id="lblWeatherStation" style="Z-INDEX: 108; LEFT: 64px; POSITION: absolute; TOP: 336px"
								runat="server" Height="16px" Width="168px" Font-Names="Arial">Closest weather station:</asp:label>
							<asp:dropdownlist id="cboWeatherStation" style="Z-INDEX: 109; LEFT: 296px; POSITION: absolute; TOP: 336px"
								tabIndex="4" runat="server" Height="24px" Width="344px" Font-Names="Arial"></asp:dropdownlist>
							<asp:dropdownlist id="cboRegion" style="Z-INDEX: 110; LEFT: 296px; POSITION: absolute; TOP: 296px"
								tabIndex="3" runat="server" Height="24px" Width="344px" Font-Names="Arial" AutoPostBack="True"></asp:dropdownlist>
							<asp:label id="lbRegion" style="Z-INDEX: 111; LEFT: 64px; POSITION: absolute; TOP: 296px" runat="server"
								Height="16px" Width="48px" Font-Names="Arial">Region:</asp:label>
							<asp:label id="lblStartGrowingSeason" style="Z-INDEX: 112; LEFT: 64px; POSITION: absolute; TOP: 256px"
								runat="server" Font-Names="Arial">Start of growing season:</asp:label>
							<asp:textbox id="edtPaddockName" style="Z-INDEX: 113; LEFT: 296px; POSITION: absolute; TOP: 216px"
								tabIndex="1" runat="server" Width="336px" Font-Names="Arial"></asp:textbox>
							<asp:label id="lblPaddockName" style="Z-INDEX: 114; LEFT: 64px; POSITION: absolute; TOP: 216px"
								runat="server" Font-Names="Arial">Paddock name:</asp:label>
							<jwg:gridex id=grdStartOfGrowingSeason style="Z-INDEX: 115; LEFT: 296px; POSITION: absolute; TOP: 256px" tabIndex=2 runat="server" Height="20px" Width="168px" Font-Names="Arial" ColumnHeaders="False" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar" DataSource="<%# dsStartOfGrowingSeason %>" DataMember="StartOfGrowingSeason" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch">
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
							<asp:panel id="pnlConsultant" style="Z-INDEX: 116; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" Height="64px" Width="800px" BorderColor="White" BorderStyle="None">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetailsConsultant" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="160px" Height="8px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="88px" Height="8px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageGrowers" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="104px" Height="8px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="382px" Height="8px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="368px" Height="8px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenuConsultant" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Font-Bold="True" Font-Names="Arial" ForeColor="White" Width="112px" Height="8px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 117; LEFT: 0px; POSITION: absolute; TOP: 552px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
