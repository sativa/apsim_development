<%@ Page language="c#" Codebehind="wfRegistrationGrower.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfRegistrationGrower" %>
<%@ Register TagPrefix="jwge" Namespace="Janus.Web.GridEX.EditControls" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Grower Registration</title>
		<meta content="Microsoft Visual Studio .NET 7.1" name="GENERATOR">
		<meta content="C#" name="CODE_LANGUAGE">
		<meta content="JavaScript" name="vs_defaultClientScript">
		<meta content="http://schemas.microsoft.com/intellisense/ie5" name="vs_targetSchema">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 800px; HEIGHT: 488px" height="488" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 774px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 1158px" align="left" ms_positioning="GridLayout"><asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black">Yield Prophet<sup>®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 101; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout"><asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="Medium" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="785px" Height="20px">Yield Prophet<sup>®</sup> 2006 - Grower Registration & Delivery Information</asp:label></DIV>
							<asp:panel id="pnlConsultant" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Width="800px" Height="40px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 37px" ms_positioning="GridLayout">
									<asp:HyperLink id="hylHome" style="Z-INDEX: 100; LEFT: 248px; POSITION: absolute; TOP: 8px" tabIndex="22"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Font-Size="Medium" NavigateUrl="http://www.yieldprophet.com.au">Home</asp:HyperLink>
									<asp:HyperLink id="hylRegistrationMeu" style="Z-INDEX: 101; LEFT: 456px; POSITION: absolute; TOP: 8px"
										tabIndex="23" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Font-Size="Medium"
										NavigateUrl="http://apsru.webstrikesolutions.com/YP/wfRegistrationMenu.aspx">Registration Menu</asp:HyperLink></DIV>
							</asp:panel><asp:label id="Label2" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial" Width="80px">First name:</asp:label><asp:label id="Label3" style="Z-INDEX: 104; LEFT: 416px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial" Width="104px">Second name:</asp:label><asp:label id="Label5" style="Z-INDEX: 105; LEFT: 16px; POSITION: absolute; TOP: 240px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 1:</asp:label><asp:label id="Label10" style="Z-INDEX: 106; LEFT: 288px; POSITION: absolute; TOP: 416px" runat="server"
								Font-Names="Arial" Width="56px">Mobile:</asp:label><asp:label id="Label7" style="Z-INDEX: 107; LEFT: 16px; POSITION: absolute; TOP: 416px" runat="server"
								Font-Names="Arial" Width="48px">Phone:</asp:label><asp:label id="Label12" style="Z-INDEX: 108; LEFT: 576px; POSITION: absolute; TOP: 416px" runat="server"
								Font-Names="Arial" Width="32px">Fax:</asp:label><asp:label id="Label14" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 464px" runat="server"
								Font-Names="Arial" Width="56px">Email:</asp:label><asp:panel id="pnlBottomBorder" style="Z-INDEX: 110; LEFT: 0px; POSITION: absolute; TOP: 1136px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:panel><asp:textbox id="edtFirstName" style="Z-INDEX: 111; LEFT: 104px; POSITION: absolute; TOP: 192px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="1"></asp:textbox><asp:textbox id="edtSecondName" style="Z-INDEX: 112; LEFT: 528px; POSITION: absolute; TOP: 192px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="2"></asp:textbox><asp:textbox id="edtPostalAddressOne" style="Z-INDEX: 113; LEFT: 192px; POSITION: absolute; TOP: 240px"
								runat="server" Width="584px" Font-Names="Arial" tabIndex="3"></asp:textbox><jwge:integerupdown id="edtNumberOfPaddocks" style="Z-INDEX: 114; LEFT: 520px; POSITION: absolute; TOP: 672px"
								runat="server" Width="136px" VisualStyle="Standard" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" Minimum="1" Maximum="50" Font-Names="Arial" tabIndex="17"
								Height="30px"></jwge:integerupdown><asp:label id="Label1" style="Z-INDEX: 115; LEFT: 16px; POSITION: absolute; TOP: 672px" runat="server"
								Font-Names="Arial" Width="480px">How many paddocks would you like to subscribe (up to six paddocks are included in subscription fee, extra paddocks are $55 each):</asp:label><asp:textbox id="edtEmail" style="Z-INDEX: 116; LEFT: 96px; POSITION: absolute; TOP: 464px" runat="server"
								Width="632px" Font-Names="Arial" tabIndex="11"></asp:textbox><jwg:gridex id="grdPaddocks" style="Z-INDEX: 117; LEFT: 0px; POSITION: absolute; TOP: 736px"
								runat="server" Font-Names="Arial" Width="790px" Height="240px" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts" AllowPaging="Never"
								AllowColumnDrag="False" AutomaticSort="False" UpdateMode="RowUpdateBatch" EditorsFrameUrl="/gridex/images/blank.html" GridLineColor="ScrollBar" AllowEdit="True"
								GroupByBoxVisible="False" tabIndex="19">
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
							</jwg:gridex><asp:label id="Label8" style="Z-INDEX: 118; LEFT: 16px; POSITION: absolute; TOP: 576px" runat="server"
								Font-Names="Arial" Width="136px">Consultant's name:</asp:label><asp:label id="Label4" style="Z-INDEX: 119; LEFT: 464px; POSITION: absolute; TOP: 576px" runat="server"
								Font-Names="Arial" Width="136px">Consultant's phone:</asp:label><asp:label id="Label6" style="Z-INDEX: 120; LEFT: 16px; POSITION: absolute; TOP: 616px" runat="server"
								Font-Names="Arial" Width="136px">Consultant's email:</asp:label><asp:textbox id="edtConsultantsName" style="Z-INDEX: 121; LEFT: 160px; POSITION: absolute; TOP: 576px"
								runat="server" Width="256px" Enabled="False" Font-Names="Arial" tabIndex="14"></asp:textbox><asp:textbox id="edtConsultantsEmail" style="Z-INDEX: 122; LEFT: 184px; POSITION: absolute; TOP: 616px"
								runat="server" Width="576px" Enabled="False" Font-Names="Arial" tabIndex="16"></asp:textbox><asp:label id="Label9" style="Z-INDEX: 123; LEFT: 16px; POSITION: absolute; TOP: 512px" runat="server"
								Font-Names="Arial" Width="696px">Do you wish to use the Yield Prophet<sup>®</sup> in conjunction with an agricultural consultant who you would like to be given access to your Yield Prophet<sup>®</sup> account?</asp:label><asp:checkbox id="chkYes" style="Z-INDEX: 124; LEFT: 720px; POSITION: absolute; TOP: 512px" runat="server"
								AutoPostBack="True" Text="Yes" Font-Names="Arial" tabIndex="12"></asp:checkbox><asp:checkbox id="chkNo" style="Z-INDEX: 125; LEFT: 720px; POSITION: absolute; TOP: 536px" runat="server"
								AutoPostBack="True" Text="No" Checked="True" Font-Names="Arial" tabIndex="13"></asp:checkbox><asp:button id="btnShow" style="Z-INDEX: 126; LEFT: 672px; POSITION: absolute; TOP: 680px" runat="server"
								Text="Show" tabIndex="18"></asp:button>
							<asp:Button id="btnCancel" style="Z-INDEX: 127; LEFT: 392px; POSITION: absolute; TOP: 1048px"
								runat="server" Width="120px" Height="32px" Text="Cancel" tabIndex="21"></asp:Button>
							<asp:Button id="btnFinish" style="Z-INDEX: 128; LEFT: 240px; POSITION: absolute; TOP: 1048px"
								runat="server" Width="120px" Height="32px" Text="Finish" tabIndex="20"></asp:Button>
							<asp:TextBox id="edtPhone" style="Z-INDEX: 129; LEFT: 80px; POSITION: absolute; TOP: 416px" runat="server"
								Font-Names="Arial" tabIndex="8"></asp:TextBox>
							<asp:TextBox id="edtMobile" style="Z-INDEX: 130; LEFT: 360px; POSITION: absolute; TOP: 416px"
								runat="server" Font-Names="Arial" tabIndex="9"></asp:TextBox>
							<asp:TextBox id="edtFax" style="Z-INDEX: 131; LEFT: 616px; POSITION: absolute; TOP: 416px" runat="server"
								Font-Names="Arial" tabIndex="10"></asp:TextBox>
							<asp:TextBox id="edtConsultantsPhone" style="Z-INDEX: 132; LEFT: 608px; POSITION: absolute; TOP: 576px"
								runat="server" Font-Names="Arial" Enabled="False" tabIndex="15"></asp:TextBox>
							<asp:label id="Label11" style="Z-INDEX: 133; LEFT: 16px; POSITION: absolute; TOP: 280px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 2:</asp:label>
							<asp:textbox id="edtPostalAddressTwo" style="Z-INDEX: 134; LEFT: 192px; POSITION: absolute; TOP: 280px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="4"></asp:textbox>
							<asp:label id="Label13" style="Z-INDEX: 135; LEFT: 16px; POSITION: absolute; TOP: 320px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 3:</asp:label>
							<asp:textbox id="edtPostalAddressThree" style="Z-INDEX: 136; LEFT: 192px; POSITION: absolute; TOP: 320px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="5"></asp:textbox>
							<asp:label id="Label15" style="Z-INDEX: 137; LEFT: 16px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Width="40px">Town:</asp:label>
							<asp:textbox id="edtTown" style="Z-INDEX: 138; LEFT: 72px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Width="400px" tabIndex="6"></asp:textbox>
							<asp:label id="Label16" style="Z-INDEX: 140; LEFT: 496px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Width="80px">Post Code:</asp:label>
							<asp:textbox id="edtPostCode" style="Z-INDEX: 141; LEFT: 584px; POSITION: absolute; TOP: 360px"
								runat="server" Font-Names="Arial" Width="192px" tabIndex="7"></asp:textbox></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
