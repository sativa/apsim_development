<%@ Page language="c#" Codebehind="wfAdminUserEdit.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfAdminUserEdit" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
  <HEAD>
		<title>Edit Existing User</title>
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
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 907px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" ForeColor="MediumBlue" Font-Bold="True" Font-Names="Arial Black"> Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Size="Large" ForeColor="DarkGray" Font-Bold="True" Font-Names="Arial Black">Edit GrowerPlaceHolder's Details</asp:label></DIV>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" BackColor="MediumBlue" Height="40px" Width="800px" BorderColor="White" BorderStyle="None">
      <DIV id=divConsultant 
      style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: " 
      ms_positioning="GridLayout">
<asp:LinkButton id=btnPersonalDetailsConsultant style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px" tabIndex=13 runat="server" Font-Names="Arial" Font-Bold="True" ForeColor="White" Width="160px" Height="8px" CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
<asp:LinkButton id=btnManageReports style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px" tabIndex=14 runat="server" Font-Names="Arial" Font-Bold="True" ForeColor="White" Width="88px" Height="8px" CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
<asp:LinkButton id=btnManageGrowers style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px" tabIndex=12 runat="server" Font-Names="Arial" Font-Bold="True" ForeColor="White" Width="104px" Height="8px" CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
<asp:LinkButton id=btnMainMenuConsultant style="Z-INDEX: 103; LEFT: 208px; POSITION: absolute; TOP: 8px" runat="server" Font-Names="Arial" Font-Bold="True" ForeColor="White" Width="112px" Height="8px" CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:Panel>
							<asp:Label id="lblConsultantTwo" style="Z-INDEX: 105; LEFT: 280px; POSITION: absolute; TOP: 776px"
								runat="server" Height="16px" Width="240px" Font-Size="Smaller" Font-Names="Arial">Hold Ctrl to select multiple Consultants</asp:Label>
							<asp:Label id="lblConsultant" style="Z-INDEX: 106; LEFT: 112px; POSITION: absolute; TOP: 552px"
								runat="server" Height="16px" Font-Names="Arial">Consultant:</asp:Label>
							<asp:Label id="lblAccess" style="Z-INDEX: 107; LEFT: 144px; POSITION: absolute; TOP: 480px"
								runat="server" Height="16px" Width="104px" Font-Names="Arial">Access type:</asp:Label>
							<asp:DropDownList id="cboAccessType" style="Z-INDEX: 108; LEFT: 256px; POSITION: absolute; TOP: 480px"
								tabIndex="6" runat="server" Height="24px" Width="249px" AutoPostBack="True" Font-Names="Arial"></asp:DropDownList>
							<asp:Label id="lblUsersCropsTwo" style="Z-INDEX: 109; LEFT: 280px; POSITION: absolute; TOP: 448px"
								runat="server" Height="16px" Width="208px" Font-Size="Smaller" Font-Names="Arial">Hold Ctrl to select multiple Crops</asp:Label>
							<asp:ListBox id="lstUsersCrops" style="Z-INDEX: 110; LEFT: 256px; POSITION: absolute; TOP: 336px"
								runat="server" Height="104px" Width="248px" SelectionMode="Multiple" Font-Names="Arial" tabIndex="5"></asp:ListBox>
							<asp:Label id="lblUsersCrops" style="Z-INDEX: 111; LEFT: 168px; POSITION: absolute; TOP: 336px"
								runat="server" Height="16px" Font-Names="Arial">Crops:</asp:Label>
							<asp:Label id="lblEmail" style="Z-INDEX: 112; LEFT: 168px; POSITION: absolute; TOP: 256px"
								runat="server" Width="32px" Height="16px" ForeColor="Black" Font-Names="Arial">Email:</asp:Label>
							<asp:TextBox id="edtEmail" style="Z-INDEX: 113; LEFT: 256px; POSITION: absolute; TOP: 256px"
								tabIndex="3" runat="server" Width="248px" Height="24px" EnableViewState="False" Font-Names="Arial"></asp:TextBox>
							<asp:Label id="lblUserName" style="Z-INDEX: 114; LEFT: 168px; POSITION: absolute; TOP: 216px"
								runat="server" Font-Names="Arial">Username: </asp:Label>
							<asp:TextBox id="edtUserName" style="Z-INDEX: 115; LEFT: 256px; POSITION: absolute; TOP: 216px"
								tabIndex="2" runat="server" Height="24px" Width="248px" EnableViewState="False" Font-Names="Arial"></asp:TextBox>
							<asp:TextBox id="edtName" style="Z-INDEX: 116; LEFT: 256px; POSITION: absolute; TOP: 176px" tabIndex="1"
								runat="server" Width="248px" Height="24px" EnableViewState="False" Font-Names="Arial"></asp:TextBox>
							<asp:Label id="lblName" style="Z-INDEX: 117; LEFT: 168px; POSITION: absolute; TOP: 176px" runat="server"
								Width="48px" Height="16px" ForeColor="Black" Font-Names="Arial">Name:</asp:Label>
							<asp:Button id="btnSave" style="Z-INDEX: 118; LEFT: 192px; POSITION: absolute; TOP: 816px" runat="server"
								Height="32px" Width="120px" Text="Save changes" tabIndex="9"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 119; LEFT: 480px; POSITION: absolute; TOP: 816px"
								runat="server" Height="32px" Width="120px" Text="Cancel changes" tabIndex="11"></asp:Button>
							<asp:Button id="btnPassword" style="Z-INDEX: 104; LEFT: 336px; POSITION: absolute; TOP: 816px"
								runat="server" Text="Change password" Height="32px" Width="120px" Font-Names="Arial" tabIndex="10"></asp:Button>
							<jwg:gridex id="grdConsultants" style="Z-INDEX: 120; LEFT: 192px; POSITION: absolute; TOP: 552px"
								runat="server" Height="192px" Width="424px" SelectionMode="MultipleSelection" Font-Names="Arial"
								AllowPaging="Never" AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch"
								EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images"
								GridLineColor="ScrollBar" AllowEdit="True" GroupByBoxVisible="False" tabIndex="8">
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
										<jwg:GridEXColumn UseType="System.String" Key="UserName" HasValueList="True" DataMember="UserName" 
 DefaultGroupPrefix="UserName:" InvalidValueAction="DiscardChanges" NullText="" Caption="UserName" 
 Width="0px" Visible="False">
											<CellStyle Width="0px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" EditType="NoEdit" Key="Name" HasValueList="True" DataMember="Name" 
 DefaultGroupPrefix="Name:" InvalidValueAction="DiscardChanges" NullText="" Caption="Name" Width="200px">
											<CellStyle Width="200px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn EditType="CheckBox" Key="Email" HasValueList="True" DataMember="Email" ColumnType="CheckBox" 
 DefaultGroupPrefix="Email:" InvalidValueAction="DiscardChanges" NullText="" Caption="Email"></jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.Boolean" EditType="CheckBox" Key="ReadOnly" HasValueList="True" 
 DataMember="ReadOnly" ColumnType="CheckBox" DefaultGroupPrefix="ReadOnly:" InvalidValueAction="DiscardChanges" 
 NullText="" Caption="Read Only"></jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" TextAlign="center" BackColor="DarkGray" ForeColor="White" Height="20px"
									Appearance="RaisedLight" BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
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
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 121; LEFT: 0px; POSITION: absolute; TOP: 888px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:CheckBox id="chkReadOnly" style="Z-INDEX: 122; LEFT: 328px; POSITION: absolute; TOP: 512px"
								runat="server" Font-Names="Arial" Text="Read only?" TextAlign="Left" tabIndex="7"></asp:CheckBox>
							<asp:Label id="lblBannerImage" style="Z-INDEX: 123; LEFT: 168px; POSITION: absolute; TOP: 296px"
								runat="server" Font-Names="Arial" ForeColor="Black" Width="107px" Height="16px">Banner image:</asp:Label>
							<asp:DropDownList id="cboBannerImage" style="Z-INDEX: 124; LEFT: 280px; POSITION: absolute; TOP: 296px"
								runat="server" Font-Names="Arial" Width="224px" Height="24px" tabIndex="4"></asp:DropDownList></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
