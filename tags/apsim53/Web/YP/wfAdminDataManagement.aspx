<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfAdminDataManagement.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminDataManagement" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Data Management</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 850px" height="850" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 835px" align="left" ms_positioning="GridLayout">
							<asp:Label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" Font-Bold="True" Font-Size="X-Large" ForeColor="MediumBlue"> Yield Prophet<sup>
									®</sup></asp:Label>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:Label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" Font-Bold="True" Font-Size="Large" ForeColor="DarkGray" Width="785px"
									Height="20px">Data Management</asp:Label></DIV>
							<asp:Label id="lblMultipleSelect" style="Z-INDEX: 103; LEFT: 264px; POSITION: absolute; TOP: 568px"
								runat="server" Font-Names="Arial" Font-Size="Smaller" Width="264px" Height="16px">Hold Ctrl to select multiple growers/paddocks</asp:Label>
							<jwg:gridEX id="grdUsers" style="Z-INDEX: 104; LEFT: 144px; POSITION: absolute; TOP: 256px"
								tabIndex="3" runat="server" Font-Names="Arial" Width="500px" Height="291px" BorderStyle="Solid"
								GroupByBoxVisible="False" AllowPaging="Never" Indent="20px" TableSpacing="10px" GroupIndent="20px"
								BorderWidth="1px" ChildLoadingMode="OnPageLoad" GridLineColor="ScrollBar" Hierarchical="True"
								ScriptsFolderPath="/gridex/scripts" EditorsFrameUrl="/gridex/images/blank.html" ImagesFolderPath="/gridex/images"
								RecordExpandImage="/gridex/images/jsclosed.gif" RecordCollapseImage="/gridex/images/jsopen.gif"
								SelectOnExpand="False" SelectionMode="MultipleSelection">
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
							<asp:Button id="btnFind" style="Z-INDEX: 108; LEFT: 560px; POSITION: absolute; TOP: 192px" tabIndex="2"
								runat="server" Font-Names="Arial" Text="Find"></asp:Button>
							<asp:Label id="lblGrowers" style="Z-INDEX: 105; LEFT: 144px; POSITION: absolute; TOP: 232px"
								runat="server" Font-Names="Arial">Select growers/paddocks:</asp:Label>
							<asp:Label id="lblFind" style="Z-INDEX: 106; LEFT: 192px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial">User's name:</asp:Label>
							<asp:TextBox id="edtFind" style="Z-INDEX: 107; LEFT: 296px; POSITION: absolute; TOP: 192px" tabIndex="1"
								runat="server" Font-Names="Arial" Width="256px"></asp:TextBox>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 816px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="16px"></asp:Panel>
							<asp:CheckBox id="chkSelectAll" style="Z-INDEX: 110; LEFT: 304px; POSITION: absolute; TOP: 592px"
								runat="server" Font-Names="Arial" TextAlign="Left" AutoPostBack="True" Text="Select all paddocks"></asp:CheckBox>
							<asp:Image id="imgBanner" style="Z-INDEX: 111; LEFT: 64px; POSITION: absolute; TOP: 8px" runat="server"
								Width="120px" Height="56px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:CheckBoxList id="chklstPaddockInformation" style="Z-INDEX: 112; LEFT: 144px; POSITION: absolute; TOP: 656px"
								runat="server" Font-Names="Arial" Width="488px" Height="28px" RepeatDirection="Horizontal">
								<asp:ListItem Value="Core" Selected="True">Core Details</asp:ListItem>
								<asp:ListItem Value="Rainfall" Selected="True">Rainfall Events</asp:ListItem>
								<asp:ListItem Value="Irrigation" Selected="True">Irrigation Applications</asp:ListItem>
								<asp:ListItem Value="Nitrogen" Selected="True">Nitrogen Applications</asp:ListItem>
								<asp:ListItem Value="Soil" Selected="True">Soil Samples</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblPaddockComponents" style="Z-INDEX: 113; LEFT: 144px; POSITION: absolute; TOP: 632px"
								runat="server" Font-Names="Arial">Select paddock information to include:</asp:Label>
							<asp:Button id="btnExport" style="Z-INDEX: 114; LEFT: 144px; POSITION: absolute; TOP: 736px"
								runat="server" Width="100px" Height="32px" Text="Export"></asp:Button>
							<asp:Button id="btnCopy" style="Z-INDEX: 115; LEFT: 352px; POSITION: absolute; TOP: 736px" runat="server"
								Width="100px" Height="32px" Text="Copy"></asp:Button>
							<asp:Button id="btnPaste" style="Z-INDEX: 116; LEFT: 536px; POSITION: absolute; TOP: 736px"
								runat="server" Width="100px" Height="32px" Text="Paste"></asp:Button>
							<asp:Panel id="pnlAdministration" style="Z-INDEX: 117; LEFT: 0px; POSITION: absolute; TOP: 112px"
								runat="server" BackColor="MediumBlue" Width="800px" Height="40px" BorderStyle="None" BorderColor="White">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 103; LEFT: 592px; POSITION: absolute; TOP: 8px"
										runat="server" ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="88px"
										CommandName="wfMainMenu.aspx">Main Menu</asp:LinkButton>
									<asp:LinkButton id="btnAdmin" style="Z-INDEX: 103; LEFT: 104px; POSITION: absolute; TOP: 8px" runat="server"
										ForeColor="White" Font-Bold="True" Font-Names="Arial" Height="8px" Width="168px" CommandName="wfAdminMenu.aspx">Administration Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<asp:Label id="Label1" style="Z-INDEX: 118; LEFT: 88px; POSITION: absolute; TOP: 160px" runat="server"
								ForeColor="Red" Font-Size="Large" Font-Bold="True" Font-Names="Arial" Width="660px">THIS PAGE IS UNDER CONSTRUCTION DO NOT USE</asp:Label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
