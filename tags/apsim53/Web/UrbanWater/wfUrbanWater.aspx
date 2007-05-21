<%@ Page language="c#" Codebehind="wfUrbanWater.aspx.cs" AutoEventWireup="false" Inherits="UrbanWater.wfUrbanWater" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Garden Water Wizard</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body bottomMargin="0" bgColor="#0066ff" leftMargin="0" topMargin="0" rightMargin="0">
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 100%; POSITION: static; HEIGHT: 525px" cellSpacing="0"
				cellPadding="0" width="758" border="0">
				<TR>
					<TD style="WIDTH: 197px; HEIGHT: 32px" bgColor="#000099"></TD>
					<TD style="HEIGHT: 32px" bgColor="#000099" colSpan="2"></TD>
				</TR>
				<TR>
					<TD style="PADDING-RIGHT: 0px; PADDING-LEFT: 0px; PADDING-BOTTOM: 0px; MARGIN: 0px; WIDTH: 197px; PADDING-TOP: 0px; HEIGHT: 330px"
						noWrap><asp:image id="imgSideBar" runat="server" ImageAlign="Top" ImageUrl="Images\SideBar.JPG" Height="450px"
							Width="200px"></asp:image></TD>
					<TD style="HEIGHT: 330px" vAlign="top" align="center" colSpan="2"><asp:panel id="pnlFiller" runat="server" Height="20px" Width="100%" BackColor="#000099" HorizontalAlign="Center"></asp:panel><asp:panel id="pnlHeader" runat="server" Height="32px" Width="100%" BackColor="#CC9900" HorizontalAlign="Center"
							Font-Size="Large" Font-Names="Arial" ForeColor="White" Font-Bold="True">GARDEN WATER 
      WIZARD</asp:panel>
						<DIV style="WIDTH: 517px; POSITION: relative; HEIGHT: 376px" ms_positioning="GridLayout"><asp:label id="lblLocation" style="Z-INDEX: 101; LEFT: 16px; POSITION: absolute; TOP: 24px"
								runat="server" Font-Size="Small" Font-Names="Arial" ForeColor="White" Font-Bold="True">1. Select a location:</asp:label><asp:dropdownlist id="cboPlantTypes" style="Z-INDEX: 107; LEFT: 192px; POSITION: absolute; TOP: 104px"
								runat="server" Width="200px"></asp:dropdownlist><asp:dropdownlist id="cboSoilTypes" style="Z-INDEX: 106; LEFT: 192px; POSITION: absolute; TOP: 64px"
								runat="server" Width="200px"></asp:dropdownlist><asp:dropdownlist id="cboLocations" style="Z-INDEX: 102; LEFT: 192px; POSITION: absolute; TOP: 24px"
								runat="server" Width="200px"></asp:dropdownlist><asp:label id="lblSoilType" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 64px"
								runat="server" Font-Size="Small" Font-Names="Arial" ForeColor="White" Font-Bold="True">2. Select a soil type:</asp:label><asp:label id="lblPlantType" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 104px"
								runat="server" Font-Size="Small" Font-Names="Arial" ForeColor="White" Font-Bold="True">3. Select a plant type:</asp:label><asp:label id="lblWaterings" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 144px"
								runat="server" Font-Size="Small" Font-Names="Arial" ForeColor="White" Font-Bold="True">4. Enter waterings:</asp:label><asp:label id="lblDisplayResults" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 352px"
								runat="server" Font-Size="Small" Font-Names="Arial" ForeColor="White" Font-Bold="True">6. Display results:</asp:label><asp:button id="btnDisplayResults" style="Z-INDEX: 109; LEFT: 280px; POSITION: absolute; TOP: 352px"
								runat="server" Width="64px" Text="GO"></asp:button><jwg:gridex id=grdWaterings style="Z-INDEX: 110; LEFT: 32px; POSITION: absolute; TOP: 168px" runat="server" Height="120px" Width="402px" ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images" ImagesFolderPath="/gridex/images" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="Watering" DataSource="<%# dsWaterings %>" ExpandableGroups="False" >
								<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
									Padding="4px 4px"></GroupByBoxInfoFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="#CC9900" ForeColor="White" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" Font-Size="Small" Font-Names="Arial" Font-Bold="True" BorderColor="GrayText"></HeaderFormatStyle>
								<AlternatingRowFormatStyle BorderStyle="Solid" BackColor="Control" Height="20px" BorderWidth="1px"></AlternatingRowFormatStyle>
								<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
								<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
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
								<RootTable DataMember="Watering" Key="Watering">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" EditType="NoEdit" Key="ID" DataMember="ID" DefaultGroupPrefix="ID:"
											InvalidValueAction="DiscardChanges" Caption="ID" Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="Date" FormatString="dd/MM/yyyy"
											DataMember="Date" DefaultGroupPrefix="Date of watering:" InvalidValueAction="DiscardChanges" Caption="Date of watering"
											Width="200px">
											<CellStyle Width="200px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Amount" DataMember="Amount" DefaultGroupPrefix="Amount of watering (mm):"
											InvalidValueAction="DiscardChanges" Caption="Amount of Watering (mm)" Width="200px">
											<CellStyle Width="200px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
							</jwg:gridex><asp:label id="Label1" style="Z-INDEX: 111; LEFT: 16px; POSITION: absolute; TOP: 312px" runat="server"
								Font-Size="Small" Font-Names="Arial" ForeColor="White" Font-Bold="True">5. Select calculation period:</asp:label><asp:dropdownlist id="cboPeriod" style="Z-INDEX: 112; LEFT: 240px; POSITION: absolute; TOP: 312px"
								runat="server" Width="152px">
								<asp:ListItem Value="1W">1 Week</asp:ListItem>
								<asp:ListItem Value="2W" Selected="True">2 Weeks</asp:ListItem>
								<asp:ListItem Value="3W">3 Weeks</asp:ListItem>
								<asp:ListItem Value="1M">1 Month</asp:ListItem>
								<asp:ListItem Value="2M">2 Months</asp:ListItem>
								<asp:ListItem Value="3M">3 Months</asp:ListItem>
							</asp:dropdownlist></DIV>
					</TD>
				</TR>
				<TR>
					<TD style="WIDTH: 197px; HEIGHT: 95px" align="center"><asp:image id="imgLogo" runat="server" ImageUrl="Images\csiro.gif"></asp:image></TD>
					<TD style="HEIGHT: 95px" colSpan="2"></TD>
				</TR>
				<TR>
				</TR>
				<TR>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
