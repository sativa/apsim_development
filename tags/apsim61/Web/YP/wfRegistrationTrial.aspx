<%@ Page language="c#" Codebehind="wfRegistrationTrial.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfRegistrationTrial" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="jwge" Namespace="Janus.Web.GridEX.EditControls" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Trial Registration</title>
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
						<DIV style="WIDTH: 801px; POSITION: relative; HEIGHT: 843px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large">Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Medium" Height="20px"
									Width="785px">Yield Prophet<sup>®</sup> 2006 -Trial Registration & Delivery Information</asp:label></DIV>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 37px" ms_positioning="GridLayout">
									<asp:HyperLink id="hylHome" style="Z-INDEX: 100; LEFT: 248px; POSITION: absolute; TOP: 8px" tabIndex="20"
										runat="server" Font-Size="Medium" Font-Bold="True" ForeColor="White" Font-Names="Arial" NavigateUrl="http://www.yieldprophet.com.au">Home</asp:HyperLink>
									<asp:HyperLink id="hylRegistrationMeu" style="Z-INDEX: 101; LEFT: 456px; POSITION: absolute; TOP: 8px"
										tabIndex="21" runat="server" Font-Size="Medium" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										NavigateUrl="http://apsru.webstrikesolutions.com/YP/wfRegistrationMenu.aspx">Registration Menu</asp:HyperLink></DIV>
							</asp:Panel>
							<asp:Label id="Label2" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial" Width="80px">First name:</asp:Label>
							<asp:Label id="Label3" style="Z-INDEX: 105; LEFT: 416px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial" Width="104px">Second name:</asp:Label>
							<asp:Label id="Label10" style="Z-INDEX: 106; LEFT: 288px; POSITION: absolute; TOP: 408px" runat="server"
								Font-Names="Arial" Width="56px">Mobile:</asp:Label>
							<asp:Label id="Label7" style="Z-INDEX: 107; LEFT: 16px; POSITION: absolute; TOP: 408px" runat="server"
								Font-Names="Arial" Width="48px">Phone:</asp:Label>
							<asp:Label id="Label12" style="Z-INDEX: 108; LEFT: 576px; POSITION: absolute; TOP: 408px" runat="server"
								Font-Names="Arial" Width="32px">Fax:</asp:Label>
							<asp:Label id="Label14" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 456px" runat="server"
								Font-Names="Arial" Width="56px">Email:</asp:Label>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 110; LEFT: 0px; POSITION: absolute; TOP: 824px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:Panel>
							<asp:TextBox id="edtFirstName" style="Z-INDEX: 111; LEFT: 104px; POSITION: absolute; TOP: 192px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="1"></asp:TextBox>
							<asp:TextBox id="edtSecondName" style="Z-INDEX: 112; LEFT: 528px; POSITION: absolute; TOP: 192px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="2"></asp:TextBox>
							<asp:TextBox id="edtEmail" style="Z-INDEX: 113; LEFT: 96px; POSITION: absolute; TOP: 456px" runat="server"
								Width="632px" tabIndex="11"></asp:TextBox>
							<jwg:gridex id="grdPaddocks" style="Z-INDEX: 114; LEFT: 0px; POSITION: absolute; TOP: 656px"
								runat="server" Font-Names="Arial" Height="40px" Width="790px" ScriptsFolderPath="/gridex/scripts"
								ImagesFolderPath="/gridex/images" GroupByBoxVisible="False" AllowEdit="True" GridLineColor="ScrollBar"
								EditorsFrameUrl="/gridex/images/blank.html" UpdateMode="RowUpdateBatch" AutomaticSort="False"
								AllowColumnDrag="False" AllowPaging="Never" tabIndex="17">
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
							<asp:Label id="Label8" style="Z-INDEX: 115; LEFT: 16px; POSITION: absolute; TOP: 568px" runat="server"
								Font-Names="Arial" Width="136px">Consultant's name:</asp:Label>
							<asp:Label id="Label4" style="Z-INDEX: 116; LEFT: 464px; POSITION: absolute; TOP: 568px" runat="server"
								Font-Names="Arial" Width="136px">Consultant's phone:</asp:Label>
							<asp:Label id="Label6" style="Z-INDEX: 117; LEFT: 16px; POSITION: absolute; TOP: 608px" runat="server"
								Font-Names="Arial" Width="136px">Consultant's email:</asp:Label>
							<asp:TextBox id="edtConsultantsName" style="Z-INDEX: 118; LEFT: 160px; POSITION: absolute; TOP: 568px"
								runat="server" Width="256px" Enabled="False" Font-Names="Arial" tabIndex="14"></asp:TextBox>
							<asp:TextBox id="edtConsultantsEmail" style="Z-INDEX: 119; LEFT: 184px; POSITION: absolute; TOP: 608px"
								runat="server" Width="576px" Enabled="False" Font-Names="Arial" tabIndex="16"></asp:TextBox>
							<asp:Label id="Label9" style="Z-INDEX: 120; LEFT: 16px; POSITION: absolute; TOP: 504px" runat="server"
								Font-Names="Arial" Width="696px">Do you wish to use the Yield Prophet<sup>®</sup> in conjunction with an agricultural consultant who you would like to be given access to your Yield Prophet account?</asp:Label>
							<asp:CheckBox id="chkYes" style="Z-INDEX: 121; LEFT: 720px; POSITION: absolute; TOP: 504px" runat="server"
								Text="Yes" AutoPostBack="True" Font-Names="Arial" tabIndex="12"></asp:CheckBox>
							<asp:CheckBox id="chkNo" style="Z-INDEX: 122; LEFT: 720px; POSITION: absolute; TOP: 528px" runat="server"
								Text="No" AutoPostBack="True" Checked="True" Font-Names="Arial" tabIndex="13"></asp:CheckBox>
							<asp:Button id="btnCancel" style="Z-INDEX: 123; LEFT: 408px; POSITION: absolute; TOP: 752px"
								runat="server" Height="32px" Width="120px" Text="Cancel" tabIndex="19"></asp:Button>
							<asp:Button id="btnFinish" style="Z-INDEX: 124; LEFT: 256px; POSITION: absolute; TOP: 752px"
								runat="server" Height="32px" Width="120px" Text="Finish" tabIndex="18"></asp:Button>
							<asp:TextBox id="edtPhone" style="Z-INDEX: 125; LEFT: 80px; POSITION: absolute; TOP: 408px" runat="server"
								Font-Names="Arial" tabIndex="8"></asp:TextBox>
							<asp:TextBox id="edtMobile" style="Z-INDEX: 126; LEFT: 368px; POSITION: absolute; TOP: 408px"
								runat="server" Font-Names="Arial" tabIndex="9"></asp:TextBox>
							<asp:TextBox id="edtFax" style="Z-INDEX: 127; LEFT: 616px; POSITION: absolute; TOP: 408px" runat="server"
								Font-Names="Arial" tabIndex="10"></asp:TextBox>
							<asp:TextBox id="edtConsultantsPhone" style="Z-INDEX: 128; LEFT: 608px; POSITION: absolute; TOP: 560px"
								runat="server" Font-Names="Arial" Enabled="False" tabIndex="15"></asp:TextBox>
							<asp:label id="Label5" style="Z-INDEX: 129; LEFT: 16px; POSITION: absolute; TOP: 240px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 1:</asp:label>
							<asp:textbox id="edtPostalAddressOne" style="Z-INDEX: 130; LEFT: 192px; POSITION: absolute; TOP: 240px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="3"></asp:textbox>
							<asp:textbox id="edtPostalAddressTwo" style="Z-INDEX: 131; LEFT: 192px; POSITION: absolute; TOP: 280px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="4"></asp:textbox>
							<asp:textbox id="edtPostalAddressThree" style="Z-INDEX: 132; LEFT: 192px; POSITION: absolute; TOP: 320px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="5"></asp:textbox>
							<asp:textbox id="edtTown" style="Z-INDEX: 133; LEFT: 72px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Width="400px" tabIndex="6"></asp:textbox>
							<asp:label id="Label16" style="Z-INDEX: 134; LEFT: 496px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Width="80px">Post Code:</asp:label>
							<asp:textbox id="edtPostCode" style="Z-INDEX: 135; LEFT: 584px; POSITION: absolute; TOP: 360px"
								runat="server" Font-Names="Arial" Width="192px" tabIndex="7"></asp:textbox>
							<asp:label id="Label15" style="Z-INDEX: 136; LEFT: 16px; POSITION: absolute; TOP: 360px" runat="server"
								Font-Names="Arial" Width="40px">Town:</asp:label>
							<asp:label id="Label13" style="Z-INDEX: 137; LEFT: 16px; POSITION: absolute; TOP: 320px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 3:</asp:label>
							<asp:label id="Label11" style="Z-INDEX: 138; LEFT: 16px; POSITION: absolute; TOP: 280px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 2:</asp:label></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
