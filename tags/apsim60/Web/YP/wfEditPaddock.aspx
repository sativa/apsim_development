<%@ Page language="c#" Codebehind="wfEditPaddock.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditPaddock" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfEditPaddock</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 108; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 776px; POSITION: relative; HEIGHT: 44px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 100; LEFT: 104px; POSITION: absolute; TOP: 16px"
						runat="server" EnableViewState="False" Font-Size="Smaller">Cancel</asp:LinkButton>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 101; LEFT: 80px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 102; LEFT: 8px; POSITION: absolute; TOP: 16px" runat="server"
						ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSettingUpImg" style="Z-INDEX: 103; LEFT: 160px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\preferences.gif"></asp:ImageButton>
					<asp:ImageButton id="btnRainfallImg" style="Z-INDEX: 104; LEFT: 264px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\rainfall.gif"></asp:ImageButton>
					<asp:ImageButton id="btnReportsImg" style="Z-INDEX: 105; LEFT: 536px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\reports.gif"></asp:ImageButton>
					<asp:LinkButton id="btnSettingUp" style="Z-INDEX: 106; LEFT: 184px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller">Setting Up</asp:LinkButton>
					<asp:LinkButton id="btnRainfall" style="Z-INDEX: 108; LEFT: 288px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller">Rainfall</asp:LinkButton>
					<asp:CheckBox id="chkEmail" style="Z-INDEX: 109; LEFT: 672px; POSITION: absolute; TOP: 16px" runat="server"
						Height="16px" Font-Size="Smaller" Text="Email con/par" ForeColor="Blue"></asp:CheckBox>
					<asp:LinkButton id="btnReport" style="Z-INDEX: 110; LEFT: 560px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller">Report Options</asp:LinkButton>
					<asp:DropDownList id="cboReport" style="Z-INDEX: 111; LEFT: 352px; POSITION: absolute; TOP: 16px"
						runat="server" Width="184px" Font-Size="Smaller"></asp:DropDownList>
					<asp:Button id="btnSave" style="Z-INDEX: 112; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						Width="40px" Height="16px" BackColor="Transparent" Font-Size="Smaller" Text="Save" ForeColor="Blue"
						Font-Names="Times New Roman" Font-Underline="True" BorderStyle="None" BorderColor="Transparent"></asp:Button></DIV>
			</asp:panel><asp:label id="lblCropManagement" style="Z-INDEX: 101; LEFT: 16px; POSITION: absolute; TOP: 80px"
				runat="server" Height="16px" Width="176px">In crop management for user: </asp:label><asp:label id="lblName" style="Z-INDEX: 102; LEFT: 208px; POSITION: absolute; TOP: 80px" runat="server"
				Height="16px">Name</asp:label><asp:checkbox id="chkSown" style="Z-INDEX: 103; LEFT: 64px; POSITION: absolute; TOP: 160px" tabIndex="1"
				runat="server" Height="16px" Width="144px" Text="Have you sown yet?" AutoPostBack="True" TextAlign="Left"></asp:checkbox><asp:dropdownlist id="cboCrops" style="Z-INDEX: 104; LEFT: 216px; POSITION: absolute; TOP: 200px"
				tabIndex="3" runat="server" Height="24px" Width="184px" AutoPostBack="True"></asp:dropdownlist><asp:dropdownlist id="cboCultivars" style="Z-INDEX: 105; LEFT: 216px; POSITION: absolute; TOP: 240px"
				tabIndex="4" runat="server" Height="24px" Width="184px"></asp:dropdownlist><asp:label id="lblCultivar" style="Z-INDEX: 107; LEFT: 112px; POSITION: absolute; TOP: 240px"
				runat="server" Height="16px" Width="88px">Cultivar Type:</asp:label><asp:label id="lblCrop" style="Z-INDEX: 106; LEFT: 128px; POSITION: absolute; TOP: 200px" runat="server"
				Height="16px" Width="72px">Crop Type:</asp:label><asp:label id="lblNitrogen" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 320px"
				runat="server"> Nitrogen fertiliser applications:</asp:label><jwg:gridex id=grdNitrogen style="Z-INDEX: 110; LEFT: 216px; POSITION: absolute; TOP: 320px" runat="server" Height="120px" Width="302px" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="Nitrogen" DataSource="<%# dsNitrogen %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False">
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
							InvalidValueAction="DiscardChanges" NullText="" Caption="Application Rate (kg/ha)" Width="160px">
							<CellStyle Width="160px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"
					Padding="0"></SelectedFormatStyle>
				<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
				<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
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
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
					VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="20px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridex><jwg:gridex id=grdSowDate style="Z-INDEX: 111; LEFT: 216px; POSITION: absolute; TOP: 160px" runat="server" Height="20px" Width="270px" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="SowDate" DataSource="<%# dsSowDate %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" ColumnHeaders="False">
				<RootTable DataMember="SowDate" Key="SowDate">
					<Columns>
						<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="SowDate" FormatString="dd/MM/yyyy"
							HasValueList="True" DataMember="SowDate" DefaultGroupPrefix="SowDate:" InvalidValueAction="DiscardChanges"
							NullText="" Caption="SowDate" Width="248px">
							<CellStyle Width="248px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
				<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
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
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
					VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
					BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridex><asp:label id="lblPaddockName" style="Z-INDEX: 112; LEFT: 96px; POSITION: absolute; TOP: 120px"
				runat="server">Paddock Name:</asp:label><asp:textbox id="edtPaddockName" style="Z-INDEX: 113; LEFT: 216px; POSITION: absolute; TOP: 120px"
				runat="server" Width="248px"></asp:textbox><asp:checkbox id="chkTriazine" style="Z-INDEX: 115; LEFT: 216px; POSITION: absolute; TOP: 280px"
				runat="server" Height="20px" Width="8px" Text=" "></asp:checkbox><asp:label id="lblTriazine" style="Z-INDEX: 116; LEFT: 96px; POSITION: absolute; TOP: 280px"
				runat="server" Height="16px" Width="112px">Triazine tolerant?</asp:label><jwg:gridex id=grdIrrigation style="Z-INDEX: 117; LEFT: 216px; POSITION: absolute; TOP: 464px" runat="server" Height="220px" Width="482px" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" DataMember="Irrigation" DataSource="<%# dsIrrigation %>" GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False">
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
							InvalidValueAction="DiscardChanges" NullText="" Caption="Application Amount (mm/ha)" Width="180px">
							<CellStyle Width="180px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn UseType="System.String" Key="Efficency" HasValueList="True" DataMember="Efficency"
							DefaultGroupPrefix="Efficency (%):" InvalidValueAction="DiscardChanges" NullText="" Caption="Efficency (%)"
							Width="160px">
							<CellStyle Width="160px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
				<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
				<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
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
				<GroupByBoxInfoFormatStyle BackColor="Control" ForeColor="ControlDark" Height="100%" VerticalAlign="middle"
					Padding="4px 4px"></GroupByBoxInfoFormatStyle>
				<TotalRowFormatStyle BackColor="Window" Height="20px"></TotalRowFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
					VerticalAlign="top" BorderWidth="1px" Font-Size="Small" Padding="0"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="20px"
					Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridex><asp:label id="lblIrrigation" style="Z-INDEX: 114; LEFT: 64px; POSITION: absolute; TOP: 464px"
				runat="server">Irrigation applications:</asp:label><asp:label id="lblRowConfiguration" style="Z-INDEX: 118; LEFT: 496px; POSITION: absolute; TOP: 160px"
				runat="server">Row Configuration:</asp:label><asp:label id="lblPopulation" style="Z-INDEX: 119; LEFT: 544px; POSITION: absolute; TOP: 120px"
				runat="server">Population: </asp:label><asp:textbox id="edtPopulation" style="Z-INDEX: 120; LEFT: 624px; POSITION: absolute; TOP: 120px"
				runat="server" Width="96px"></asp:textbox><asp:dropdownlist id="cboRowConfiguration" style="Z-INDEX: 121; LEFT: 624px; POSITION: absolute; TOP: 160px"
				runat="server" Width="168px"></asp:dropdownlist><asp:label id="lblPopulationUnit" style="Z-INDEX: 122; LEFT: 736px; POSITION: absolute; TOP: 120px"
				runat="server">plants/ha</asp:label><asp:textbox id="edtTiller" style="Z-INDEX: 123; LEFT: 560px; POSITION: absolute; TOP: 240px"
				runat="server" Width="80px"></asp:textbox><asp:label id="lblTiller" style="Z-INDEX: 124; LEFT: 520px; POSITION: absolute; TOP: 240px"
				runat="server">NFT:</asp:label>
			<asp:Label id="lblRowSpacing" style="Z-INDEX: 125; LEFT: 528px; POSITION: absolute; TOP: 200px"
				runat="server">Row Spacing:</asp:Label>
			<asp:TextBox id="edtRowSpacing" style="Z-INDEX: 127; LEFT: 624px; POSITION: absolute; TOP: 200px"
				runat="server" Width="96px"></asp:TextBox>
			<asp:Label id="lblRowSpacingUnit" style="Z-INDEX: 126; LEFT: 688px; POSITION: absolute; TOP: 240px"
				runat="server">m</asp:Label>
			<asp:Button id="btnCalculate" style="Z-INDEX: 129; LEFT: 648px; POSITION: absolute; TOP: 240px"
				runat="server" Text="Re-Calculate NFT" Width="144px"></asp:Button>
			<asp:CheckBox id="chkAutoCalculate" style="Z-INDEX: 120; LEFT: 528px; POSITION: absolute; TOP: 280px"
				runat="server" Width="264px" Height="16px" Text="Auto Calculate Number of Fertile Tillers"
				TextAlign="Left" AutoPostBack="True"></asp:CheckBox></form>
	</body>
</HTML>
