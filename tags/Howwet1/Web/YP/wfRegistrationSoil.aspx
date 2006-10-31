<%@ Page language="c#" Codebehind="wfRegistrationSoil.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfRegistrationSoil" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Soil Information</title>
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
						<DIV style="WIDTH: 801px; POSITION: relative; HEIGHT: 1062px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large">Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 101; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Medium" Height="20px"
									Width="785px">Yield Prophet<sup>®</sup> 2006 - Soil Sampling and Analysis Information</asp:label></DIV>
							<asp:panel id="pnlConsultant" style="Z-INDEX: 102; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 37px" ms_positioning="GridLayout">
									<asp:HyperLink id="hylHome" style="Z-INDEX: 100; LEFT: 248px; POSITION: absolute; TOP: 8px" tabIndex="1"
										runat="server" Font-Size="Medium" Font-Bold="True" ForeColor="White" Font-Names="Arial" NavigateUrl="http://www.yieldprophet.com.au">Home</asp:HyperLink>
									<asp:HyperLink id="hylRegistrationMeu" style="Z-INDEX: 101; LEFT: 456px; POSITION: absolute; TOP: 8px"
										tabIndex="2" runat="server" Font-Size="Medium" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										NavigateUrl="http://apsru.webstrikesolutions.com/YP/wfRegistrationMenu.aspx">Registration Menu</asp:HyperLink></DIV>
							</asp:panel>
							<asp:label id="Label1" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 184px" runat="server"
								Font-Names="Arial" Width="768px">Collection of the initial soil data required for Yield Prophet<sup>
									®</sup> is the responsibility of the subscriber.</asp:label>
							<asp:label id="Label2" style="Z-INDEX: 104; LEFT: 16px; POSITION: absolute; TOP: 232px" runat="server"
								Font-Names="Arial" Width="768px">For accurate simulations, the Yield Prophet<sup>®</sup> requires data from different depth intervals down the soil profile to maximum rooting depth (0-10, 10-40, 40-70, 70-100cm intervals are recommended as a minimum).  The data required from these intervals prior to sowing are;</asp:label>
							<asp:label id="Label3" style="Z-INDEX: 105; LEFT: 8px; POSITION: absolute; TOP: 288px" runat="server"
								Font-Names="Arial" Width="753px">
								<ul>
									<li>
										Organic carbon (topsoil only %)</li><li>Water Content (% volumetric)</li><li>Nitrate 
										(ppm)</li><li>EC (dS/m)</li><li>Chloride (ppm)</li><li>pH (CaCl2)</li></ul>
							</asp:label>
							<asp:label id="Label5" style="Z-INDEX: 106; LEFT: 16px; POSITION: absolute; TOP: 416px" runat="server"
								Font-Names="Arial" Width="536px">In regions with suspected sodicity, ESP measurements are also required.</asp:label>
							<asp:label id="Label9" style="Z-INDEX: 107; LEFT: 16px; POSITION: absolute; TOP: 912px" runat="server"
								Font-Names="Arial">James Hunt <br>Yield Prophet<sup>®</sup> Coordinator <br>Phone: 03 9354 1654<br>Mobile: 0429 922 787<br>E-mail: <a href="mailto:james.hunt@aanet.com.au?subject=YP 2006 subscriptions">
									james.hunt@aanet.com.au</a></asp:label>
							<asp:label id="Label11" style="Z-INDEX: 108; LEFT: 16px; POSITION: absolute; TOP: 504px" runat="server"
								Font-Names="Arial" Font-Bold="True">CSBP</asp:label>
							<asp:label id="Label10" style="Z-INDEX: 109; LEFT: 16px; POSITION: absolute; TOP: 528px" runat="server"
								Font-Names="Arial" Width="384px">Standard top-soil: $22 per sample plus GST<BR>Standard sub-soil: $18 per sample plus GST<BR>Suspected sodic top-soil: $34 per sample plus GST<BR>Suspected sodic sub-soil: $30 per sample plus GST</asp:label>
							<asp:label id="Label7" style="Z-INDEX: 110; LEFT: 16px; POSITION: absolute; TOP: 472px" runat="server"
								Font-Names="Arial" Width="672px">BCG has negotiated Yield Prophet<sup>®</sup> soil analysis prices with CSBP and Incitec-Pivot, these are;</asp:label>
							<asp:label id="Label12" style="Z-INDEX: 111; LEFT: 16px; POSITION: absolute; TOP: 752px" runat="server"
								Font-Names="Arial" Width="768px"> Information on sending soil samples in for chemical and water analysis is provided following subscription.</asp:label>
							<asp:label id="Label14" style="Z-INDEX: 112; LEFT: 16px; POSITION: absolute; TOP: 784px" runat="server"
								Font-Names="Arial" Width="768px">BCG will be offering a soil-sampling service in its catchment area during March and April 2006, call 5492 2787 for further details and to book. It is recommended that those outside the BCG catchment area contact their local agronomist or soil sampling contractor and discuss Yield Prophet<sup>
									®</sup> sampling requirements with them.</asp:label>
							<asp:label id="Label16" style="Z-INDEX: 114; LEFT: 16px; POSITION: absolute; TOP: 880px" runat="server"
								Font-Names="Arial">If you have any questions regarding Yield Prophet<sup>®</sup> data requirements, please contact;</asp:label>
							<asp:panel id="pnlBottomBorder" style="Z-INDEX: 115; LEFT: 0px; POSITION: absolute; TOP: 1040px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:panel>
							<asp:label id="Label6" style="Z-INDEX: 116; LEFT: 16px; POSITION: absolute; TOP: 616px" runat="server"
								Font-Bold="True" Font-Names="Arial">Incitec-Pivot</asp:label>
							<asp:label id="Label4" style="Z-INDEX: 117; LEFT: 16px; POSITION: absolute; TOP: 640px" runat="server"
								Font-Names="Arial" Width="384px">Standard top-soil: $33.77 per sample plus GST<BR>Standard sub-soil: $23.11 per sample plus GST<BR>Suspected sodic top-soil: $40.90 per sample plus GST<BR>Suspected sodic sub-soil: $33.00 per sample plus GST</asp:label></DIV>
					</TD>
				</TR>
			</TABLE>
			&nbsp;
		</form>
	</body>
</HTML>
