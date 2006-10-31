<%@ Page language="c#" Codebehind="wfRegistrationUnSupportedConsultant.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfRegistrationUnSupportedConsultant" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="jwge" Namespace="Janus.Web.GridEX.EditControls" Assembly="Janus.Web.GridEX" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Consultant with Un-Supported Growers Registration</title>
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
						<DIV style="WIDTH: 801px; POSITION: relative; HEIGHT: 755px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue" Font-Names="Arial Black">Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 101; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Size="X-Small" Font-Bold="True" ForeColor="DarkGray" Font-Names="Arial Black" Width="785px"
									Height="20px">Yield Prophet<sup>®</sup> 2006 - Un-Supported Consultant Registration & Delivery Information</asp:label></DIV>
							<asp:Panel id="pnlConsultant" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Width="800px" Height="40px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 37px" ms_positioning="GridLayout">
									<asp:HyperLink id="hylHome" style="Z-INDEX: 100; LEFT: 248px; POSITION: absolute; TOP: 8px" tabIndex="16"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Font-Size="Medium" NavigateUrl="http://www.yieldprophet.com.au">Home</asp:HyperLink>
									<asp:HyperLink id="hylRegistrationMeu" style="Z-INDEX: 101; LEFT: 456px; POSITION: absolute; TOP: 8px"
										tabIndex="17" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Font-Size="Medium"
										NavigateUrl="http://apsru.webstrikesolutions.com/YP/wfRegistrationMenu.aspx">Registration Menu</asp:HyperLink></DIV>
							</asp:Panel>
							<asp:Label id="Label2" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial" Width="80px">First name:</asp:Label>
							<asp:Label id="Label3" style="Z-INDEX: 104; LEFT: 416px; POSITION: absolute; TOP: 192px" runat="server"
								Font-Names="Arial" Width="104px">Second name:</asp:Label>
							<asp:Label id="Label10" style="Z-INDEX: 105; LEFT: 288px; POSITION: absolute; TOP: 432px" runat="server"
								Font-Names="Arial" Width="56px">Mobile:</asp:Label>
							<asp:Label id="Label7" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 432px" runat="server"
								Font-Names="Arial" Width="48px">Phone:</asp:Label>
							<asp:Label id="Label12" style="Z-INDEX: 107; LEFT: 576px; POSITION: absolute; TOP: 432px" runat="server"
								Font-Names="Arial" Width="32px">Fax:</asp:Label>
							<asp:Label id="Label14" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 480px" runat="server"
								Font-Names="Arial" Width="56px">Email:</asp:Label>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 109; LEFT: 0px; POSITION: absolute; TOP: 736px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:TextBox id="edtFirstName" style="Z-INDEX: 110; LEFT: 104px; POSITION: absolute; TOP: 192px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="1"></asp:TextBox>
							<asp:TextBox id="edtSecondName" style="Z-INDEX: 111; LEFT: 528px; POSITION: absolute; TOP: 192px"
								runat="server" Width="250px" Font-Names="Arial" tabIndex="2"></asp:TextBox>
							<jwge:IntegerUpDown id="edtNumberOfGrowers" style="Z-INDEX: 112; LEFT: 528px; POSITION: absolute; TOP: 544px"
								runat="server" Width="136px" VisualStyle="Standard" ImagesFolderPath="/gridex/images" ScriptsFolderPath="/gridex/scripts"
								Minimum="1" Maximum="50" Font-Names="Arial" tabIndex="13"></jwge:IntegerUpDown>
							<asp:Label id="Label1" style="Z-INDEX: 114; LEFT: 16px; POSITION: absolute; TOP: 536px" runat="server"
								Font-Names="Arial" Width="488px">How many growers would you like to subscribe (each grower $165 for the first paddock and $55 a paddock thereafter):</asp:Label>
							<asp:TextBox id="edtEmail" style="Z-INDEX: 115; LEFT: 96px; POSITION: absolute; TOP: 480px" runat="server"
								Width="680px" Font-Names="Arial" tabIndex="12"></asp:TextBox>
							<asp:Button id="btnNext" style="Z-INDEX: 116; LEFT: 264px; POSITION: absolute; TOP: 672px" runat="server"
								Text="Next" Height="32px" Width="120px" tabIndex="14"></asp:Button>
							<asp:Label id="Label4" style="Z-INDEX: 117; LEFT: 16px; POSITION: absolute; TOP: 232px" runat="server"
								Font-Names="Arial" Width="112px">Business name:</asp:Label>
							<asp:TextBox id="edtBusinessName" style="Z-INDEX: 118; LEFT: 144px; POSITION: absolute; TOP: 232px"
								runat="server" Width="632px" Font-Names="Arial" tabIndex="3"></asp:TextBox>
							<asp:Label id="Label6" style="Z-INDEX: 119; LEFT: 16px; POSITION: absolute; TOP: 600px" runat="server"
								Font-Names="Arial" Width="744px">If you would like corporate branding to appear on your Yield Prophet<sup>
									®</sup> account pages and reports, please e-mail a copy of your logo to <a href="mailto:james.hunt@aanet.com.au?subject=YP 2006 Logos">
									james.hunt@aanet.com.au</a></asp:Label>
							<asp:Button id="btnCancel" style="Z-INDEX: 120; LEFT: 416px; POSITION: absolute; TOP: 672px"
								runat="server" Height="32px" Width="120px" Text="Cancel" tabIndex="15"></asp:Button>
							<asp:TextBox id="edtPhone" style="Z-INDEX: 121; LEFT: 72px; POSITION: absolute; TOP: 432px" runat="server"
								Font-Names="Arial" tabIndex="9"></asp:TextBox>
							<asp:TextBox id="edtMobile" style="Z-INDEX: 122; LEFT: 344px; POSITION: absolute; TOP: 432px"
								runat="server" Font-Names="Arial" tabIndex="10"></asp:TextBox>
							<asp:TextBox id="edtFax" style="Z-INDEX: 123; LEFT: 616px; POSITION: absolute; TOP: 432px" runat="server"
								Font-Names="Arial" Width="160px" tabIndex="11"></asp:TextBox>
							<asp:label id="Label5" style="Z-INDEX: 124; LEFT: 16px; POSITION: absolute; TOP: 272px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 1:</asp:label>
							<asp:textbox id="edtPostalAddressOne" style="Z-INDEX: 133; LEFT: 192px; POSITION: absolute; TOP: 272px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="4"></asp:textbox>
							<asp:label id="Label11" style="Z-INDEX: 134; LEFT: 16px; POSITION: absolute; TOP: 312px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 2:</asp:label>
							<asp:textbox id="edtPostalAddressTwo" style="Z-INDEX: 135; LEFT: 192px; POSITION: absolute; TOP: 312px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="5"></asp:textbox>
							<asp:textbox id="edtPostalAddressThree" style="Z-INDEX: 136; LEFT: 192px; POSITION: absolute; TOP: 352px"
								runat="server" Font-Names="Arial" Width="584px" tabIndex="6"></asp:textbox>
							<asp:label id="Label13" style="Z-INDEX: 137; LEFT: 16px; POSITION: absolute; TOP: 352px" runat="server"
								Font-Names="Arial" Width="152px">Postal address line 3:</asp:label>
							<asp:label id="Label15" style="Z-INDEX: 138; LEFT: 16px; POSITION: absolute; TOP: 392px" runat="server"
								Font-Names="Arial" Width="40px">Town:</asp:label>
							<asp:textbox id="edtTown" style="Z-INDEX: 139; LEFT: 72px; POSITION: absolute; TOP: 392px" runat="server"
								Font-Names="Arial" Width="400px" tabIndex="7"></asp:textbox>
							<asp:label id="Label16" style="Z-INDEX: 140; LEFT: 496px; POSITION: absolute; TOP: 392px" runat="server"
								Font-Names="Arial" Width="80px">Post Code:</asp:label>
							<asp:textbox id="edtPostCode" style="Z-INDEX: 141; LEFT: 584px; POSITION: absolute; TOP: 392px"
								runat="server" Font-Names="Arial" Width="192px" tabIndex="8"></asp:textbox></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
