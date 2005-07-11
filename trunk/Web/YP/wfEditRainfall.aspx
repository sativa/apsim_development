<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfEditRainfall.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfEditRainfall" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfRainfall</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:panel id="pnlTop" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				HorizontalAlign="Left" BackColor="PaleGoldenrod" Height="48px" Width="100%">
				<DIV style="WIDTH: 288px; POSITION: relative; HEIGHT: 40px" ms_positioning="GridLayout">
					<asp:LinkButton id="btnCancel" style="Z-INDEX: 100; LEFT: 104px; POSITION: absolute; TOP: 16px"
						runat="server" Font-Size="Smaller" EnableViewState="False">Cancel</asp:LinkButton>
					<asp:Button id="btnSave" style="Z-INDEX: 107; LEFT: 32px; POSITION: absolute; TOP: 16px" runat="server"
						Width="40px" Height="16px" BackColor="Transparent" Font-Size="Smaller" Font-Names="Times New Roman"
						Text="Save" BorderStyle="None" BorderColor="Transparent" ForeColor="Blue" Font-Underline="True"></asp:Button>
					<asp:ImageButton id="btnCancelImg" style="Z-INDEX: 101; LEFT: 80px; POSITION: absolute; TOP: 16px"
						runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
					<asp:ImageButton id="btnSaveImg" style="Z-INDEX: 104; LEFT: 8px; POSITION: absolute; TOP: 16px" runat="server"
						ImageUrl="Images\save.gif"></asp:ImageButton>
					<asp:Label id="lblYear" style="Z-INDEX: 106; LEFT: 176px; POSITION: absolute; TOP: 16px" runat="server"
						Width="32px" Font-Size="Smaller" ForeColor="Blue"> Year:</asp:Label>
					<asp:DropDownList id="cboYear" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 16px" runat="server"
						Width="64px" AutoPostBack="True">
						<asp:ListItem Value="2003">2003</asp:ListItem>
						<asp:ListItem Value="2004">2004</asp:ListItem>
						<asp:ListItem Value="2005">2005</asp:ListItem>
						<asp:ListItem Value="2006">2006</asp:ListItem>
						<asp:ListItem Value="2007">2007</asp:ListItem>
						<asp:ListItem Value="2008">2008</asp:ListItem>
					</asp:DropDownList></DIV>
			</asp:panel><jwg:gridex id=grdRainfall style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 88px" runat="server" Height="501px" Width="522px" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images" GridLineColor="ScrollBar" AllowEdit="True" DataSource="<%# dsRainfall %>" DataMember="Rainfall" GroupByBoxVisible="False" TotalRow="True" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/images/blank.html" AllowColumnDrag="False" AutomaticSort="False">
				<RootTable DataMember="Rainfall" Key="Rainfall">
					<Columns>
						<jwg:GridEXColumn TotalFormatMode="UseStringFormat" UseType="System.String" EditType="NoEdit" Key="Day"
							DataMember="Day" DefaultGroupPrefix="Day:" InvalidValueAction="DiscardChanges" TotalFormatString="Totals"
							Caption="Day" Width="40px">
							<CellStyle BackColor="PaleGoldenrod" Width="40px"></CellStyle>
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
				<TotalRowFormatStyle BackColor="LightSteelBlue" ForeColor="Black" Height="15px" Font-Size="X-Small"></TotalRowFormatStyle>
				<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="15px"
					VerticalAlign="top" BorderWidth="1px" Font-Size="X-Small"></RowFormatStyle>
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="PaleGoldenrod" ForeColor="ControlText" Height="16px"
					Appearance="RaisedLight" BorderWidth="1px" Font-Size="X-Small" BorderColor="Control"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridex>
			<asp:Label id="lblTotalRainfall" style="Z-INDEX: 103; LEFT: 200px; POSITION: absolute; TOP: 600px"
				runat="server" Width="285px" Height="16px">Total rainfall for the year from the 1st of April:</asp:Label>
			<asp:TextBox id="edtTotalRainfall" style="Z-INDEX: 104; LEFT: 480px; POSITION: absolute; TOP: 600px"
				runat="server" Width="56px" ReadOnly="True" Height="18px"></asp:TextBox>
			<asp:label id="lblName" style="Z-INDEX: 108; LEFT: 216px; POSITION: absolute; TOP: 64px" runat="server"
				Height="16px">Name</asp:label>
			<asp:label id="lblRainfallManagement" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 64px"
				runat="server" Width="200px" Height="16px">In rainfall management for user: </asp:label>
			<asp:Label id="lblTotalRainfallTwo" style="Z-INDEX: 108; LEFT: 288px; POSITION: absolute; TOP: 632px"
				runat="server" Width="192px" Height="16px">Total rainfall for the whole year:</asp:Label>
			<asp:TextBox id="edtTotalRainfallTwo" style="Z-INDEX: 108; LEFT: 480px; POSITION: absolute; TOP: 632px"
				runat="server" Width="56px" Height="18px" ReadOnly="True"></asp:TextBox></form>
	</body>
</HTML>
