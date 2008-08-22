<%@ Page language="c#" Codebehind="wfRegistrationConsultant.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfRegistrationConsultant" %>
<%@ Register TagPrefix="jwge" Namespace="Janus.Web.GridEX.EditControls" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Consultant Registration</title>
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
					<TD style="WIDTH: 774px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 801px; POSITION: relative; HEIGHT: 1004px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large">Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Medium" Height="20px"
									Width="785px">Yield Prophet<sup>®</sup> 2006 - Consultant Registration & Delivery Information</asp:label></DIV>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 37px" ms_positioning="GridLayout">
									<asp:HyperLink id="hylHome" style="Z-INDEX: 100; LEFT: 248px; POSITION: absolute; TOP: 8px" tabIndex="18"
										runat="server" Font-Size="Medium" Font-Bold="True" ForeColor="White" Font-Names="Arial" NavigateUrl="http://www.yieldprophet.com.au">Home</asp:HyperLink>
									<asp:HyperLink id="hylRegistrationMeu" style="Z-INDEX: 101; LEFT: 456px; POSITION: absolute; TOP: 8px"
										tabIndex="19" runat="server" Font-Size="Medium" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										NavigateUrl="http://apsru.webstrikesolutions.com/YP/wfRegistrationMenu.aspx">Registration Menu</asp:HyperLink></DIV>
							</asp:Panel>
							<asp:Label id="Label2" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 184px" runat="server"
								Font-Names="Arial" Width="80px">First name:</asp:Label>
							<asp:Label id="Label3" style="Z-INDEX: 105; LEFT: 416px; POSITION: absolute; TOP: 184px" runat="server"
								Font-Names="Arial" Width="104px">Second name:</asp:Label>
							<asp:Label id="Label10" style="Z-INDEX: 106; LEFT: 288px; POSITION: absolute; TOP: 424px" runat="server"
								Font-Names="Arial" Width="56px">Mobile:</asp:Label>
							<asp:Label id="Label7" style="Z-INDEX: 107; LEFT: 16px; POSITION: absolute; TOP: 424px" runat="server"
								Font-Names="Arial" Width="48px">Phone:</asp:Label>
							<asp:Label id="Label12" style="Z-INDEX: 108; LEFT: 576px; POSITION: absolute; TOP: 424px" runat="server"
								Font-Names="Arial" Width="32px">Fax:</asp:Label>
							<asp:Label id="Label14" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 472px" runat="server"
								Font-Names="Arial" Width="56px">Email:</asp:Label>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 110; LEFT: 0px; POSITION: absolute; TOP: 984px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:TextBox id="edtFirstName" style="Z-INDEX: 111; LEFT: 104px; POSITION: absolute; TOP: 184px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="1"></asp:TextBox>
							<asp:TextBox id="edtSecondName" style="Z-INDEX: 112; LEFT: 528px; POSITION: absolute; TOP: 184px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="2"></asp:TextBox>
							<jwge:IntegerUpDown id="edtNumberOfPaddocks" style="Z-INDEX: 113; LEFT: 520px; POSITION: absolute; TOP: 520px"
								runat="server" Width="136px" ScriptsFolderPath="/gridex/scripts" ImagesFolderPath="/gridex/images"
								VisualStyle="Standard" Maximum="50" Minimum="1" Font-Names="Arial" tabIndex="13" Height="30px"></jwge:IntegerUpDown>
							<asp:Label id="Label1" style="Z-INDEX: 114; LEFT: 16px; POSITION: absolute; TOP: 520px" runat="server"
								Font-Names="Arial" Width="480px">How many paddocks would you like to subscribe (up to six paddocks are included in subscription fee, extra paddocks are $55 each):</asp:Label>
							<asp:TextBox id="edtEmail" style="Z-INDEX: 115; LEFT: 96px; POSITION: absolute; TOP: 472px" runat="server"
								Width="632px" Font-Names="Arial" tabIndex="12"></asp:TextBox>
							<jwg:gridex id="grdPaddocks" style="Z-INDEX: 116; LEFT: 0px; POSITION: absolute; TOP: 568px"
								runat="server" Font-Names="Arial" Height="240px" Width="790px" ScriptsFolderPath="/gridex/scripts"
								ImagesFolderPath="/gridex/images" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar"
								EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" AutomaticSort="False"
								AllowColumnDrag="False" AllowPaging="Never" tabIndex="15">
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
										<jwg:GridEXColumn UseType="System.String" Key="PaddockName" HasValueList="True" DataMember="PaddockName"
											DefaultGroupPrefix="PaddockName:" InvalidValueAction="DiscardChanges" NullText="" Caption="Paddock Name"
											Width="150px">
											<CellStyle Width="150px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="SoilType" DataMember="SoilType" DefaultGroupPrefix="Soil Type:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="Soil Type" Width="150px">
											<CellStyle Width="150px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="NearestTown" DataMember="NearestTown" DefaultGroupPrefix="Name of Nearest Town:"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Name of Nearest Town" Width="170px">
											<CellStyle Width="170px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn Key="Distance" DataMember="Distance" DefaultGroupPrefix="Distance &amp; Directions to Nearest Town:"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Distance &amp; Bearing to Nearest Town"
											Width="300px">
											<CellStyle Width="300px"></CellStyle>
										</jwg:GridEXColumn>
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
							<asp:Button id="btnShow" style="Z-INDEX: 117; LEFT: 672px; POSITION: absolute; TOP: 528px" runat="server"
								Text="Show" tabIndex="14"></asp:Button>
							<asp:Label id="Label4" style="Z-INDEX: 118; LEFT: 16px; POSITION: absolute; TOP: 224px" runat="server"
								Font-Names="Arial" Width="112px">Business name:</asp:Label>
							<asp:TextBox id="edtBusinessName" style="Z-INDEX: 119; LEFT: 144px; POSITION: absolute; TOP: 224px"
								runat="server" Width="569px" Font-Names="Arial" tabIndex="3"></asp:TextBox>
							<asp:Label id="Label6" style="Z-INDEX: 120; LEFT: 16px; POSITION: absolute; TOP: 848px" runat="server"
								Font-Names="Arial" Width="744px">If you would like corporate branding to appear on your Yield Prophet<sup>
									®</sup> account pages and reports, please e-mail a copy of your logo to <a href="mailto:james.hunt@aanet.com.au?subject=YP 2006 Logos">
									james.hunt@aanet.com.au</a></asp:Label>
							<asp:Button id="btnFinish" style="Z-INDEX: 121; LEFT: 232px; POSITION: absolute; TOP: 928px"
								runat="server" Width="120px" Height="32px" Text="Finish" tabIndex="16"></asp:Button>
							<asp:Button id="btnCancel" style="Z-INDEX: 122; LEFT: 384px; POSITION: absolute; TOP: 928px"
								runat="server" Width="120px" Height="32px" Text="Cancel" tabIndex="17"></asp:Button>
							<asp:TextBox id="edtPhone" style="Z-INDEX: 123; LEFT: 88px; POSITION: absolute; TOP: 424px" runat="server"
								Font-Names="Arial" tabIndex="9"></asp:TextBox>
							<asp:TextBox id="edtMobile" style="Z-INDEX: 124; LEFT: 368px; POSITION: absolute; TOP: 424px"
								runat="server" Font-Names="Arial" tabIndex="10"></asp:TextBox>
							<asp:TextBox id="edtFax" style="Z-INDEX: 125; LEFT: 624px; POSITION: absolute; TOP: 424px" runat="server"
								Font-Names="Arial" tabIndex="11"></asp:TextBox>
							<asp:label id="Label5" style="Z-INDEX: 126; LEFT: 16px; POSITION: absolute; TOP: 264px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 1:</asp:label>
							<asp:label id="Label11" style="Z-INDEX: 127; LEFT: 16px; POSITION: absolute; TOP: 304px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 2:</asp:label>
							<asp:textbox id="edtPostalAddressOne" style="Z-INDEX: 128; LEFT: 192px; POSITION: absolute; TOP: 264px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="4"></asp:textbox>
							<asp:textbox id="edtPostalAddressTwo" style="Z-INDEX: 129; LEFT: 192px; POSITION: absolute; TOP: 304px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="5"></asp:textbox>
							<asp:textbox id="edtPostalAddressThree" style="Z-INDEX: 130; LEFT: 192px; POSITION: absolute; TOP: 344px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="6"></asp:textbox>
							<asp:textbox id="edtTown" style="Z-INDEX: 131; LEFT: 72px; POSITION: absolute; TOP: 384px" runat="server"
								Font-Names="Arial" Width="400px" tabIndex="7"></asp:textbox>
							<asp:label id="Label16" style="Z-INDEX: 132; LEFT: 496px; POSITION: absolute; TOP: 384px" runat="server"
								Font-Names="Arial" Width="80px">Post Code:</asp:label>
							<asp:textbox id="edtPostCode" style="Z-INDEX: 133; LEFT: 584px; POSITION: absolute; TOP: 384px"
								runat="server" Font-Names="Arial" Width="192px" tabIndex="8"></asp:textbox>
							<asp:label id="Label15" style="Z-INDEX: 134; LEFT: 16px; POSITION: absolute; TOP: 384px" runat="server"
								Font-Names="Arial" Width="40px">Town:</asp:label>
							<asp:label id="Label13" style="Z-INDEX: 135; LEFT: 16px; POSITION: absolute; TOP: 344px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 3:</asp:label></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
