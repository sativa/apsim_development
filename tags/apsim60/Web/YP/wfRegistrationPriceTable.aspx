<%@ Page language="c#" Codebehind="wfRegistrationPriceTable.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfRegistrationPriceTable" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Price Table</title>
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
						<DIV style="WIDTH: 801px; POSITION: relative; HEIGHT: 513px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 101; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Bold="True" Font-Size="X-Large">Yield Prophet<sup>
									®</sup></asp:label>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 48px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Font-Names="Arial Black" ForeColor="DarkGray" Font-Bold="True" Font-Size="Medium" Height="20px"
									Width="785px">Yield Prophet<sup>®</sup> 2006 - Registration & Delivery Price Table</asp:label></DIV>
							<asp:panel id="pnlConsultant" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 96px"
								runat="server" Height="40px" Width="800px" BorderColor="White" BorderStyle="None" BackColor="MediumBlue">
								<DIV style="WIDTH: 800px; POSITION: relative; HEIGHT: 35px" ms_positioning="GridLayout">
									<asp:HyperLink id="hylHome" style="Z-INDEX: 100; LEFT: 216px; POSITION: absolute; TOP: 8px" tabIndex="1"
										runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Font-Size="Medium" NavigateUrl="http://www.yieldprophet.com.au">Home</asp:HyperLink>
									<asp:HyperLink id="hylRegistrationMeu" style="Z-INDEX: 101; LEFT: 440px; POSITION: absolute; TOP: 8px"
										tabIndex="2" runat="server" Font-Names="Arial" ForeColor="White" Font-Bold="True" Font-Size="Medium"
										NavigateUrl="http://apsru.webstrikesolutions.com/YP/wfRegistrationMenu.aspx">Registration Menu</asp:HyperLink></DIV>
							</asp:panel>
							<asp:panel id="pnlBottomBorder" style="Z-INDEX: 104; LEFT: 0px; POSITION: absolute; TOP: 496px"
								runat="server" Height="16px" Width="800px" BackColor="MediumBlue"></asp:panel>
							<TABLE id="Table1" style="Z-INDEX: 105; LEFT: 8px; WIDTH: 784px; POSITION: absolute; TOP: 192px; HEIGHT: 126px"
								cellSpacing="1" cellPadding="1" width="784" border="0">
								<TR>
									<TD style="WIDTH: 96px" bgColor="silver" colSpan="" rowSpan=""><B style="mso-bidi-font-weight: normal"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Subscription</SPAN></B></TD>
									<TD bgColor="silver"><B style="mso-bidi-font-weight: normal"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Initial 
												Cost</SPAN></B></TD>
									<TD bgColor="silver"><B style="mso-bidi-font-weight: normal"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Extra 
												Paddocks</SPAN></B></TD>
									<TD bgColor="silver"><B><SPAN style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-AU; mso-fareast-language: EN-AU; mso-bidi-language: AR-SA">Paddock 
												Creation</SPAN></B></TD>
									<TD bgColor="silver"><B><SPAN style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-AU; mso-fareast-language: EN-AU; mso-bidi-language: AR-SA">Branding</SPAN></B></TD>
									<TD bgColor="silver"><B><SPAN style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-AU; mso-fareast-language: EN-AU; mso-bidi-language: AR-SA">Log-in 
												for consultant</SPAN></B></TD>
									<TD bgColor="silver"><B><SPAN style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-AU; mso-fareast-language: EN-AU; mso-bidi-language: AR-SA">Support 
												to consultant</SPAN></B></TD>
									<TD bgColor="silver"><B><SPAN style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-AU; mso-fareast-language: EN-AU; mso-bidi-language: AR-SA">Log-in 
												for grower</SPAN></B></TD>
									<TD bgColor="silver"><B><SPAN style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-AU; mso-fareast-language: EN-AU; mso-bidi-language: AR-SA">Support 
												to grower</SPAN></B></TD>
								</TR>
								<TR>
									<TD style="WIDTH: 96px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Trial</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$220</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">N/A</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">No</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
								</TR>
								<TR>
									<TD style="WIDTH: 96px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Grower</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$550</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$55</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">No</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
								</TR>
								<TR>
									<TD style="WIDTH: 96px; HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Consultant</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$550</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$55</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD style="HEIGHT: 15px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">No</SPAN></SPAN></SPAN></SPAN></TD>
								</TR>
								<TR>
									<TD style="WIDTH: 96px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Consultant 
											and Un-supported Growers</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$165 
											per grower</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$55</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">No</SPAN></TD>
								</TR>
								<TR>
									<TD style="WIDTH: 96px"><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Consultant 
											and Supported Growers</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$220 
											per grower</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">$110</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
									<TD><SPAN lang="EN-US" style="FONT-SIZE: 10pt; FONT-FAMILY: Arial; mso-fareast-font-family: 'Times New Roman'; mso-ansi-language: EN-US; mso-fareast-language: EN-US; mso-bidi-language: AR-SA">Yes</SPAN></TD>
								</TR>
							</TABLE>
						</DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
