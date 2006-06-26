<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Page language="c#" Codebehind="wfManageUsers.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfManageUsers" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfManageUsers</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<jwg:gridEX id="grdUsers" style="Z-INDEX: 101; LEFT: 24px; POSITION: absolute; TOP: 80px" runat="server"
				SelectOnExpand="False" GroupByBoxVisible="False" ColumnHeaders="False" RecordCollapseImage="/gridex/images/jsopen.gif"
				RecordExpandImage="/gridex/images/jsclosed.gif" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html"
				ScriptsFolderPath="/gridex/scripts" Hierarchical="True" Width="400px" Height="400px" GridLineColor="ScrollBar"
				ChildLoadingMode="OnPageLoad" Font-Size="8pt" Font-Names="Verdana" BorderStyle="Solid" BorderWidth="1px"
				GroupIndent="20px" TableSpacing="10px" Indent="20px" AllowPaging="Never">
				<RootTable HierarchicalMode="SelfTable">
					<Columns>
						<jwg:GridEXColumn Key="Name" DataMember="Name" DefaultGroupPrefix=":" InvalidValueAction="DiscardChanges"
							NullText="" Width="250px">
							<CellStyle Width="250px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn Key="UserName" DataMember="UserName" DefaultGroupPrefix="UserName:" InvalidValueAction="DiscardChanges"
							Caption="UserName" Width="1px" Visible="False">
							<CellStyle Width="1px"></CellStyle>
						</jwg:GridEXColumn>
						<jwg:GridEXColumn Key="AccessType" DataMember="AccessType" DefaultGroupPrefix="AccessType:" InvalidValueAction="DiscardChanges"
							Caption="AccessType" Width="130px">
							<CellStyle Width="130px"></CellStyle>
						</jwg:GridEXColumn>
					</Columns>
					<SelfReferencingSettings ParentMember="ParentID" ChildMember="ID" AutoSizeColumnOnExpand="True" UseExpandColumn="0"></SelfReferencingSettings>
				</RootTable>
				<NewRowFormatStyle BackColor="Window" ForeColor="WindowText" Height="20px"></NewRowFormatStyle>
				<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
				<SelectedFormatStyle BackColor="Highlight" ForeColor="Window" Height="20px" VerticalAlign="top" Font-Size="8pt"></SelectedFormatStyle>
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
				<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText" Font-Size="8pt"></FilterRowFormatStyle>
				<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
				<GroupByBoxFormatStyle BackColor="ControlDark" Padding="5px 4px 5px 4px"></GroupByBoxFormatStyle>
				<GroupRowFormatStyle TextAlign="left" BackColor="ActiveCaption" ForeColor="ControlText" Height="20px"
					VerticalAlign="top"></GroupRowFormatStyle>
				<GroupRowIndentJunctionFormatStyle BackColor="Control"></GroupRowIndentJunctionFormatStyle>
				<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
					BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
				<GroupIndentFormatStyle BackColor="Control" ForeColor="Black"></GroupIndentFormatStyle>
				<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
			</jwg:gridEX>
			<asp:Panel id="pnlTop" style="Z-INDEX: 119; LEFT: 0px; POSITION: absolute; TOP: 0px" runat="server"
				Height="48px" Width="100%" BackColor="PaleGoldenrod" HorizontalAlign="Left">&nbsp;</asp:Panel>&nbsp;
			<jwg:gridEX id="grdPaddocks" style="Z-INDEX: 102; LEFT: 464px; POSITION: absolute; TOP: 80px"
				runat="server" BorderWidth="1px" BorderStyle="Solid" Font-Names="Verdana" Font-Size="8pt"
				GridLineColor="ScrollBar" Height="400px" Width="340px" ColumnHeaders="False" GroupByBoxVisible="False"
				SelectOnExpand="False">
				<RootTable>
					<Columns>
						<jwg:GridEXColumn Key="Name" DataMember="Name" DefaultGroupPrefix="Name:" InvalidValueAction="DiscardChanges"
							Caption="Name" Width="338px">
							<CellStyle Width="338px"></CellStyle>
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
			</jwg:gridEX>
			<asp:LinkButton id="btnAddUser" style="Z-INDEX: 103; LEFT: 48px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">Add User</asp:LinkButton>
			<asp:ImageButton id="btnAddUserImg" style="Z-INDEX: 104; LEFT: 24px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\growers.gif"></asp:ImageButton>
			<asp:ImageButton id="btnEditUserImg" style="Z-INDEX: 105; LEFT: 120px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\users_details.gif"></asp:ImageButton>
			<asp:LinkButton id="btnEditUser" style="Z-INDEX: 106; LEFT: 144px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">Edit User</asp:LinkButton>
			<asp:ImageButton id="btnViewReportsImg" style="Z-INDEX: 107; LEFT: 320px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\reports.gif"></asp:ImageButton>
			<asp:LinkButton id="btnViewReports" style="Z-INDEX: 108; LEFT: 344px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">View Reports</asp:LinkButton>
			<asp:ImageButton id="btnDeleteUserImg" style="Z-INDEX: 109; LEFT: 216px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
			<asp:LinkButton id="btnDeleteUser" style="Z-INDEX: 110; LEFT: 240px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">Delete User</asp:LinkButton>
			<asp:Label id="lblUsers" style="Z-INDEX: 111; LEFT: 176px; POSITION: absolute; TOP: 56px" runat="server">Users</asp:Label>
			<asp:Label id="lblPaddocks" style="Z-INDEX: 112; LEFT: 592px; POSITION: absolute; TOP: 56px"
				runat="server">Paddocks</asp:Label>
			<asp:ImageButton id="btnAddPaddockImg" style="Z-INDEX: 113; LEFT: 464px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\add.gif"></asp:ImageButton>
			<asp:LinkButton id="btnAddPaddock" style="Z-INDEX: 114; LEFT: 488px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">Add Paddock</asp:LinkButton>
			<asp:ImageButton id="btnEditPaddockImg" style="Z-INDEX: 115; LEFT: 584px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\paddock.gif"></asp:ImageButton>
			<asp:LinkButton id="btnEditPaddock" style="Z-INDEX: 116; LEFT: 608px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">Edit Paddock</asp:LinkButton>
			<asp:ImageButton id="btnDeletePaddockImg" style="Z-INDEX: 117; LEFT: 696px; POSITION: absolute; TOP: 488px"
				runat="server" ImageUrl="Images\cancel.gif"></asp:ImageButton>
			<asp:LinkButton id="btnDeletePaddock" style="Z-INDEX: 118; LEFT: 720px; POSITION: absolute; TOP: 496px"
				runat="server" Font-Size="Smaller">Delete Paddock</asp:LinkButton>
			<asp:CheckBox id="chkSetPosition" style="Z-INDEX: 120; LEFT: 40px; POSITION: absolute; TOP: 608px"
				runat="server" Visible="False" Enabled="False"></asp:CheckBox>
			<asp:TextBox id="edtFind" style="Z-INDEX: 121; LEFT: 120px; POSITION: absolute; TOP: 536px" runat="server"
				Width="256px"></asp:TextBox>
			<asp:Label id="lblFind" style="Z-INDEX: 122; LEFT: 32px; POSITION: absolute; TOP: 536px" runat="server">User's name:</asp:Label>
			<asp:Button id="btnFind" style="Z-INDEX: 123; LEFT: 384px; POSITION: absolute; TOP: 536px" runat="server"
				Text="Find"></asp:Button>
			<asp:CheckBox id="chkRefreshUsersGrid" style="Z-INDEX: 124; LEFT: 176px; POSITION: absolute; TOP: 608px"
				runat="server" Enabled="False" Visible="False" Checked="True"></asp:CheckBox>
		</form>
	</body>
</HTML>
