<%@ Page language="c#" Codebehind="wfSurvey.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfSurvey" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfSurvey</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<TABLE id="MainTable" style="WIDTH: 803px; HEIGHT: 3000px" height="736" cellSpacing="0"
				cellPadding="0" width="803" align="center" border="0">
				<TR>
					<TD style="WIDTH: 538px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 802px; POSITION: relative; HEIGHT: 4612px" align="left" ms_positioning="GridLayout">
							<asp:Panel id="pnlGrower" style="Z-INDEX: 100; LEFT: 0px; POSITION: absolute; TOP: 168px" runat="server"
								Width="800px" Height="40px" BackColor="MediumBlue" BorderStyle="None" BorderColor="White">
								<DIV id="divGrower" style="WIDTH: 801px; POSITION: relative; HEIGHT: 32px; WIDTH800px: "
									ms_positioning="GridLayout"></DIV>
							</asp:Panel>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 4592px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Label id="lblQuestionEight" style="Z-INDEX: 102; LEFT: 112px; POSITION: absolute; TOP: 2424px"
								runat="server" Width="544px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">8. Did you discuss your Yield Prophet<sup>
									®</sup> reports with anyone?</asp:Label>
							<asp:CheckBoxList id="chkQuestionEight" style="Z-INDEX: 103; LEFT: 128px; POSITION: absolute; TOP: 2456px"
								runat="server" Width="528px" Font-Names="Tahoma" Font-Size="Small">
								<asp:ListItem Value="No">No</asp:ListItem>
								<asp:ListItem Value="Yes, other subscribers">Yes, other subscribers</asp:ListItem>
								<asp:ListItem Value="Yes, other growers who are not subscribers">Yes, other growers who are not subscribers</asp:ListItem>
								<asp:ListItem Value="Yes, government extension officer">Yes, government extension officer</asp:ListItem>
								<asp:ListItem Value="Yes, agronomic consultant">Yes, agronomic consultant</asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:TextBox id="edtOtherEight" style="Z-INDEX: 104; LEFT: 200px; POSITION: absolute; TOP: 2576px"
								runat="server" Width="464px" Height="64px" Rows="3" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:Label id="lblQuestionSixteen" style="Z-INDEX: 105; LEFT: 112px; POSITION: absolute; TOP: 4144px"
								runat="server" Width="536px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">16. Do you have any general comments regarding Yield Prophet<sup>
									®</sup> in 2005?</asp:Label>
							<asp:TextBox id="edtSixteen" style="Z-INDEX: 106; LEFT: 136px; POSITION: absolute; TOP: 4192px"
								runat="server" Width="528px" Height="102px" Rows="5" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:TextBox id="edtSeventeen" style="Z-INDEX: 107; LEFT: 136px; POSITION: absolute; TOP: 4360px"
								runat="server" Width="528px" Height="102px" Rows="5" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:Label id="lblQuestionSeventeen" style="Z-INDEX: 108; LEFT: 112px; POSITION: absolute; TOP: 4328px"
								runat="server" Width="504px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">17. Do you have any suggestions for Yield Prophet<sup>
									®</sup> in 2006?</asp:Label>
							<asp:Label id="lblQuestionSix_Two" style="Z-INDEX: 109; LEFT: 136px; POSITION: absolute; TOP: 1672px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">David Stephens DAWA:</asp:Label>
							<asp:Label id="lblQuestionSix_One" style="Z-INDEX: 110; LEFT: 136px; POSITION: absolute; TOP: 1648px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">SOI:</asp:Label>
							<asp:CheckBoxList id="chkQuestionSix_One" style="Z-INDEX: 111; LEFT: 312px; POSITION: absolute; TOP: 1632px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSix_Two" style="Z-INDEX: 112; LEFT: 312px; POSITION: absolute; TOP: 1664px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lbl1_6" style="Z-INDEX: 113; LEFT: 320px; POSITION: absolute; TOP: 1608px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">1</asp:Label>
							<asp:Label id="lbl2_6" style="Z-INDEX: 114; LEFT: 352px; POSITION: absolute; TOP: 1608px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">2</asp:Label>
							<asp:Label id="lbl3_6" style="Z-INDEX: 115; LEFT: 384px; POSITION: absolute; TOP: 1608px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">3</asp:Label>
							<asp:Label id="lbl4_6" style="Z-INDEX: 116; LEFT: 416px; POSITION: absolute; TOP: 1608px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">4</asp:Label>
							<asp:Label id="lbl5_6" style="Z-INDEX: 117; LEFT: 448px; POSITION: absolute; TOP: 1608px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">5</asp:Label>
							<asp:Label id="lblQuestionSix" style="Z-INDEX: 118; LEFT: 120px; POSITION: absolute; TOP: 1560px"
								runat="server" Width="544px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">6. How useful did you find the seasonal climate forecasts in Yield Prophet<sup>
									®</sup>? (1=very useful, 5=not at all useful)</asp:Label>
							<asp:CheckBoxList id="chkQuestionFourteen" style="Z-INDEX: 119; LEFT: 120px; POSITION: absolute; TOP: 3792px"
								runat="server" Width="528px" Font-Names="Tahoma" Font-Size="Small">
								<asp:ListItem Value="&#177; 0.5%">&#177; 0.5%</asp:ListItem>
								<asp:ListItem Value="&#177; 1%">&#177; 1%</asp:ListItem>
								<asp:ListItem Value="&#177; 2%">&#177; 2%</asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionFourteen" style="Z-INDEX: 120; LEFT: 104px; POSITION: absolute; TOP: 3744px"
								runat="server" Width="552px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">14. What is your expectation of the level of error that is acceptable from the Yield Prophet<sup>
									®</sup> for protein predictions?</asp:Label>
							<asp:CheckBoxList id="chkQuestionThirteen" style="Z-INDEX: 121; LEFT: 128px; POSITION: absolute; TOP: 3536px"
								runat="server" Width="528px" Font-Names="Tahoma" Font-Size="Small">
								<asp:ListItem Value="&#177; 0.25 t/ha">&#177; 0.25 t/ha</asp:ListItem>
								<asp:ListItem Value="&#177; 0.5 t/ha">&#177; 0.5 t/ha</asp:ListItem>
								<asp:ListItem Value="&#177; 0.75 t/ha">&#177; 0.75 t/ha</asp:ListItem>
								<asp:ListItem Value="&#177; 1.0 t/ha">&#177; 1.0 t/ha</asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionThirteen" style="Z-INDEX: 122; LEFT: 112px; POSITION: absolute; TOP: 3488px"
								runat="server" Width="552px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">13. What is your expectation of the level of error that is acceptable from the Yield Prophet<sup>
									®</sup> for yield predictions?</asp:Label>
							<asp:Label id="lblQuestionTwelve_Two" style="Z-INDEX: 123; LEFT: 128px; POSITION: absolute; TOP: 3424px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Protein:</asp:Label>
							<asp:CheckBoxList id="chkQuestionTwelve_Two" style="Z-INDEX: 124; LEFT: 240px; POSITION: absolute; TOP: 3416px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionTwelve_One" style="Z-INDEX: 125; LEFT: 128px; POSITION: absolute; TOP: 3392px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Yield:</asp:Label>
							<asp:CheckBoxList id="chkQuestionTwelve_One" style="Z-INDEX: 126; LEFT: 240px; POSITION: absolute; TOP: 3384px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lbl1_12" style="Z-INDEX: 127; LEFT: 248px; POSITION: absolute; TOP: 3360px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">1</asp:Label>
							<asp:Label id="lbl2_12" style="Z-INDEX: 128; LEFT: 280px; POSITION: absolute; TOP: 3360px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">2</asp:Label>
							<asp:Label id="lbl3_12" style="Z-INDEX: 129; LEFT: 312px; POSITION: absolute; TOP: 3360px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">3</asp:Label>
							<asp:Label id="lbl4_12" style="Z-INDEX: 130; LEFT: 344px; POSITION: absolute; TOP: 3360px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">4</asp:Label>
							<asp:Label id="lbl5_12" style="Z-INDEX: 131; LEFT: 376px; POSITION: absolute; TOP: 3360px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">5</asp:Label>
							<asp:Label id="lblQuestionTwelve" style="Z-INDEX: 132; LEFT: 112px; POSITION: absolute; TOP: 3288px"
								runat="server" Width="544px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">12. How happy were you with the general level of simulation accuracy for all subscribers in 2005, as outlined in the results summary? (1= very happy, 5=very unhappy)</asp:Label>
							<asp:Label id="lblQuestionEleven_Three" style="Z-INDEX: 133; LEFT: 128px; POSITION: absolute; TOP: 3224px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Growth Stage:</asp:Label>
							<asp:Label id="lblQuestionEleven_Two" style="Z-INDEX: 134; LEFT: 128px; POSITION: absolute; TOP: 3192px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Protein:</asp:Label>
							<asp:Label id="lblQuestionEleven_One" style="Z-INDEX: 135; LEFT: 128px; POSITION: absolute; TOP: 3160px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Yield:</asp:Label>
							<asp:CheckBoxList id="chkQuestionEleven_One" style="Z-INDEX: 136; LEFT: 240px; POSITION: absolute; TOP: 3152px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionEleven_Two" style="Z-INDEX: 137; LEFT: 240px; POSITION: absolute; TOP: 3184px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionEleven_Three" style="Z-INDEX: 138; LEFT: 240px; POSITION: absolute; TOP: 3216px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lbl1_11" style="Z-INDEX: 139; LEFT: 248px; POSITION: absolute; TOP: 3128px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">1</asp:Label>
							<asp:Label id="lbl2_11" style="Z-INDEX: 140; LEFT: 280px; POSITION: absolute; TOP: 3128px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">2</asp:Label>
							<asp:Label id="lbl3_11" style="Z-INDEX: 141; LEFT: 312px; POSITION: absolute; TOP: 3128px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">3</asp:Label>
							<asp:Label id="lbl4_11" style="Z-INDEX: 142; LEFT: 344px; POSITION: absolute; TOP: 3128px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">4</asp:Label>
							<asp:Label id="lbl5_11" style="Z-INDEX: 143; LEFT: 376px; POSITION: absolute; TOP: 3128px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">5</asp:Label>
							<asp:Label id="lblQuestionEleven" style="Z-INDEX: 144; LEFT: 112px; POSITION: absolute; TOP: 3080px"
								runat="server" Width="544px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">11. How happy were you with the level of simulation accuracy for your paddock in 2005? (1= very happy, 5=very unhappy)</asp:Label>
							<asp:TextBox id="edtQuestionSevenElaborate" style="Z-INDEX: 145; LEFT: 168px; POSITION: absolute; TOP: 2288px"
								runat="server" Width="464px" Height="102px" Rows="5" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:Label id="lblQuestionSevenElaborate" style="Z-INDEX: 146; LEFT: 112px; POSITION: absolute; TOP: 2240px"
								runat="server" Width="544px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">If you did have difficulty understanding what Yield Prophet<sup>
									®</sup> output meant, please elaborate:</asp:Label>
							<asp:TextBox id="edtOtherSeven" style="Z-INDEX: 147; LEFT: 176px; POSITION: absolute; TOP: 2160px"
								runat="server" Width="464px" Height="64px" Rows="3" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:Label id="lbSeven_Other" style="Z-INDEX: 148; LEFT: 128px; POSITION: absolute; TOP: 2160px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Other:</asp:Label>
							<asp:Label id="lblQuestionSeven_Nine" style="Z-INDEX: 149; LEFT: 128px; POSITION: absolute; TOP: 2088px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Lack of support or instruction:</asp:Label>
							<asp:Label id="lblQuestionSeven_Eight" style="Z-INDEX: 150; LEFT: 128px; POSITION: absolute; TOP: 2056px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Lack of interest:</asp:Label>
							<asp:Label id="lblQuestionSeven_Seven" style="Z-INDEX: 151; LEFT: 128px; POSITION: absolute; TOP: 2024px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Lack of time:</asp:Label>
							<asp:Label id="lblQuestionSeven_Six" style="Z-INDEX: 152; LEFT: 128px; POSITION: absolute; TOP: 1992px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Difficulties with computer use in general:</asp:Label>
							<asp:Label id="lblQuestionSeven_Five" style="Z-INDEX: 153; LEFT: 128px; POSITION: absolute; TOP: 1960px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Difficulties in understanding what Yield Prophet<sup>
									®</sup> output meant:</asp:Label>
							<asp:Label id="lblQuestionSeven_Four" style="Z-INDEX: 154; LEFT: 128px; POSITION: absolute; TOP: 1928px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Difficulties in understanding how the web site worked:</asp:Label>
							<asp:Label id="lblQuestionSeven_Three" style="Z-INDEX: 155; LEFT: 128px; POSITION: absolute; TOP: 1896px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Yield Prophet web-site not working:</asp:Label>
							<asp:Label id="lblQuestionSeven_Two" style="Z-INDEX: 156; LEFT: 128px; POSITION: absolute; TOP: 1864px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Your own PC not working:</asp:Label>
							<asp:Label id="lblQuestionSeven_One" style="Z-INDEX: 157; LEFT: 128px; POSITION: absolute; TOP: 1832px"
								runat="server" Font-Names="Tahoma" Font-Size="Small">Your internet connection line-speed:</asp:Label>
							<asp:CheckBoxList id="chkQuestionSeven_One" style="Z-INDEX: 158; LEFT: 584px; POSITION: absolute; TOP: 1824px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Two" style="Z-INDEX: 159; LEFT: 584px; POSITION: absolute; TOP: 1856px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Three" style="Z-INDEX: 160; LEFT: 584px; POSITION: absolute; TOP: 1888px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Four" style="Z-INDEX: 161; LEFT: 584px; POSITION: absolute; TOP: 1920px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Five" style="Z-INDEX: 162; LEFT: 584px; POSITION: absolute; TOP: 1952px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Six" style="Z-INDEX: 163; LEFT: 584px; POSITION: absolute; TOP: 1984px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Seven" style="Z-INDEX: 164; LEFT: 584px; POSITION: absolute; TOP: 2016px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Eight" style="Z-INDEX: 165; LEFT: 584px; POSITION: absolute; TOP: 2048px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionSeven_Nine" style="Z-INDEX: 166; LEFT: 584px; POSITION: absolute; TOP: 2080px"
								runat="server" Width="160px" Height="38px" Font-Size="XX-Small" ForeColor="White" RepeatColumns="5"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lbl1_7" style="Z-INDEX: 167; LEFT: 592px; POSITION: absolute; TOP: 1800px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">1</asp:Label>
							<asp:Label id="lbl2_7" style="Z-INDEX: 168; LEFT: 624px; POSITION: absolute; TOP: 1800px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">2</asp:Label>
							<asp:Label id="lbl3_7" style="Z-INDEX: 169; LEFT: 656px; POSITION: absolute; TOP: 1800px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">3</asp:Label>
							<asp:Label id="lbl4_7" style="Z-INDEX: 170; LEFT: 688px; POSITION: absolute; TOP: 1800px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">4</asp:Label>
							<asp:Label id="lbl5_7" style="Z-INDEX: 171; LEFT: 720px; POSITION: absolute; TOP: 1800px" runat="server"
								Font-Names="Tahoma" Font-Size="Small">5</asp:Label>
							<asp:Label id="lblQuestionSeven" style="Z-INDEX: 172; LEFT: 120px; POSITION: absolute; TOP: 1736px"
								runat="server" Width="544px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">7. Did you feel that your use of Yield Prophet<sup>
									®</sup> was restricted or limited by any of the following factors? (1= Not limited to 5=very limited)</asp:Label>
							<asp:CheckBoxList id="chkQuestionFour" style="Z-INDEX: 173; LEFT: 144px; POSITION: absolute; TOP: 1072px"
								runat="server" Width="528px" Font-Names="Tahoma" Font-Size="Small">
								<asp:ListItem Value="Yield Prophet output not relevant to management decisions">Yield Prophet&lt;sup&gt;&#174;&lt;/sup&gt; output not relevant to management decisions</asp:ListItem>
								<asp:ListItem Value="I didn't trust the Yield Prophet predictions">I didn't trust the Yield Prophet&lt;sup&gt;&#174;&lt;/sup&gt; predictions</asp:ListItem>
								<asp:ListItem Value="Yield Prophet didn't tell me anything I didn't already know ">Yield Prophet&lt;sup&gt;&#174;&lt;/sup&gt; didn't tell me anything I didn't already know </asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionFour" style="Z-INDEX: 174; LEFT: 128px; POSITION: absolute; TOP: 1024px"
								runat="server" Width="552px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">4. If you didn't find Yield Prophet<sup>
									®</sup> useful from a management perspective, please elaborate:</asp:Label>
							<asp:CheckBoxList id="chkQuestionThree" style="Z-INDEX: 175; LEFT: 144px; POSITION: absolute; TOP: 824px"
								runat="server" Width="528px" Font-Names="Tahoma" Font-Size="Small">
								<asp:ListItem Value="Very useful from a management perspective">Very useful from a management perspective</asp:ListItem>
								<asp:ListItem Value="Moderately useful from a management perspective">Moderately useful from a management perspective</asp:ListItem>
								<asp:ListItem Value="Of limited usefulness from a management perspective">Of limited usefulness from a management perspective</asp:ListItem>
								<asp:ListItem Value="Not useful from a management perspective">Not useful from a management perspective</asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionThree" style="Z-INDEX: 176; LEFT: 128px; POSITION: absolute; TOP: 784px"
								runat="server" Width="560px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">3. Overall, how would you describe your Yield Prophet<sup>
									®</sup> experience in 2005?</asp:Label>
							<asp:Label id="lblInstructions" style="Z-INDEX: 177; LEFT: 64px; POSITION: absolute; TOP: 240px"
								runat="server" Width="672px" Font-Names="Tahoma" Font-Size="Small">This is a short, completely anonymous survey for Yield Prophet<sup>
									®</sup> subscribers in 2005 designed to improve the quality of delivery for Yield Prophet<sup>®</sup> in 2006. The results will also be reported as part of a CSIRO research project on adoption of decision support technologies. The survey will take only a few minutes to complete.  Please fill out the online form below and click the ‘Submit’ button at the end of the page. Thanks in advance!</asp:Label>
							<asp:Image id="imgBCGLogo" style="Z-INDEX: 178; LEFT: 48px; POSITION: absolute; TOP: 16px"
								runat="server" Width="104px" Height="144px" ImageUrl="Images\BCG_logo.JPG"></asp:Image>
							<asp:Label id="lblHeadingTwo" style="Z-INDEX: 179; LEFT: 208px; POSITION: absolute; TOP: 104px"
								runat="server" Font-Names="Tahoma" Font-Size="X-Large" Font-Bold="True" ForeColor="MediumBlue">Subscriber survey & feedback</asp:Label>
							<asp:Label id="lblHeadingOne" style="Z-INDEX: 180; LEFT: 184px; POSITION: absolute; TOP: 40px"
								runat="server" Width="504px" Font-Names="Tahoma" Font-Size="XX-Large" Font-Bold="True"
								ForeColor="MediumBlue">Yield Prophet<sup>®</sup> 2005</asp:Label>
							<asp:Button id="btnSubmit" style="Z-INDEX: 181; LEFT: 344px; POSITION: absolute; TOP: 4512px"
								runat="server" Width="104px" Height="32px" Font-Names="Tahoma" Font-Size="Small" Text="Submit"></asp:Button>
							<asp:Label id="lblQuestionFifteen" style="Z-INDEX: 182; LEFT: 112px; POSITION: absolute; TOP: 3960px"
								runat="server" Width="424px" Height="20px" Font-Names="Tahoma" Font-Size="Small" Font-Bold="True">15. Does Yield Prophet<sup>
									®</sup> offer value for money?</asp:Label>
							<asp:CheckBoxList id="chkQuestionFifteen" style="Z-INDEX: 183; LEFT: 120px; POSITION: absolute; TOP: 3984px"
								runat="server" Width="528px" Font-Names="Tahoma" Font-Size="Small">
								<asp:ListItem Value="Yes">Yes</asp:ListItem>
								<asp:ListItem Value="No">No</asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:TextBox id="edtOtherFifteen" style="Z-INDEX: 184; LEFT: 200px; POSITION: absolute; TOP: 4040px"
								runat="server" Width="464px" Height="64px" Rows="3" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:TextBox id="edtOtherFourteen" style="Z-INDEX: 185; LEFT: 200px; POSITION: absolute; TOP: 3864px"
								runat="server" Width="464px" Height="64px" Rows="3" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:TextBox id="edtOtherThirteen" style="Z-INDEX: 186; LEFT: 200px; POSITION: absolute; TOP: 3640px"
								runat="server" Width="464px" Height="64px" Rows="3" TextMode="MultiLine" Font-Names="Tahoma"
								Font-Size="Small"></asp:TextBox>
							<asp:CheckBoxList id="chkQuestionTen" style="Z-INDEX: 187; LEFT: 128px; POSITION: absolute; TOP: 2856px"
								runat="server" Width="528px" Font-Size="Small" Font-Names="Tahoma">
								<asp:ListItem Value="Regularly (several runs per month)">Regularly (several runs per month)</asp:ListItem>
								<asp:ListItem Value="Often (15-30 runs)">Often (15-30 runs)</asp:ListItem>
								<asp:ListItem Value="Occasionally (5-10 runs)">Occasionally (5-10 runs)</asp:ListItem>
								<asp:ListItem Value="Rarely (less than 5 times)">Rarely (less than 5 times)</asp:ListItem>
								<asp:ListItem Value="Never">Never</asp:ListItem>
								<asp:ListItem Value="Other: ">Other: </asp:ListItem>
							</asp:CheckBoxList>
							<asp:TextBox id="edtOtherTen" style="Z-INDEX: 188; LEFT: 200px; POSITION: absolute; TOP: 2976px"
								runat="server" Height="64px" Width="464px" Font-Size="Small" Font-Names="Tahoma" TextMode="MultiLine"
								Rows="3"></asp:TextBox>
							<asp:Label id="lblQuestionTen" style="Z-INDEX: 189; LEFT: 112px; POSITION: absolute; TOP: 2816px"
								runat="server" Height="20px" Width="544px" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma">10. How often did you (including your consultant) run Yield Prophet<sup>
									®</sup> for your paddocks in 2005?</asp:Label>
							<asp:Label id="lblQuestionNine" style="Z-INDEX: 190; LEFT: 112px; POSITION: absolute; TOP: 2680px"
								runat="server" Height="20px" Width="544px" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma">9. Who ran Yield Prophet<sup>
									®</sup> for your paddocks in 2005?</asp:Label>
							<asp:CheckBoxList id="chkQuestionNine" style="Z-INDEX: 191; LEFT: 128px; POSITION: absolute; TOP: 2704px"
								runat="server" Width="528px" Font-Size="Small" Font-Names="Tahoma">
								<asp:ListItem Value="A consultant always, or nearly always, ran Yield Prophet for me">A consultant always, or nearly always, ran Yield Prophet for me</asp:ListItem>
								<asp:ListItem Value="Both a consultant and I ran Yield Prophet">Both a consultant and I ran Yield Prophet</asp:ListItem>
								<asp:ListItem Value="I ran Yield Prophet myself">I ran Yield Prophet myself</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionSeven_Ten" style="Z-INDEX: 192; LEFT: 128px; POSITION: absolute; TOP: 2120px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Inaccurate soil characterisation data:</asp:Label>
							<asp:CheckBoxList id="chkQuestionSeven_Ten" style="Z-INDEX: 193; LEFT: 584px; POSITION: absolute; TOP: 2120px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:TextBox id="edtOtherFour" style="Z-INDEX: 194; LEFT: 216px; POSITION: absolute; TOP: 1152px"
								runat="server" Height="64px" Width="464px" Font-Size="Small" Font-Names="Tahoma" TextMode="MultiLine"
								Rows="3"></asp:TextBox>
							<asp:TextBox id="edtOtherThree" style="Z-INDEX: 195; LEFT: 216px; POSITION: absolute; TOP: 920px"
								runat="server" Height="64px" Width="464px" Font-Size="Small" Font-Names="Tahoma" TextMode="MultiLine"
								Rows="3"></asp:TextBox>
							<asp:Label id="lblQuestionFive" style="Z-INDEX: 196; LEFT: 128px; POSITION: absolute; TOP: 1256px"
								runat="server" Height="20px" Width="544px" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma">5. How useful was Yield Prophet<sup>
									®</sup> for the following management decisions in 2005? (1=very useful, 5=not useful at all)</asp:Label>
							<asp:Label id="lblQuestionFive_Three" style="Z-INDEX: 197; LEFT: 144px; POSITION: absolute; TOP: 1392px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Calculating yield potential for insurance purposes</asp:Label>
							<asp:Label id="lblQuestionFive_Four" style="Z-INDEX: 198; LEFT: 144px; POSITION: absolute; TOP: 1424px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Calculating yield potential for marketing purposes</asp:Label>
							<asp:Label id="lblQuestionFive_Five" style="Z-INDEX: 199; LEFT: 144px; POSITION: absolute; TOP: 1464px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Other 1 (please list):</asp:Label>
							<asp:Label id="lblQuestionFive_Two" style="Z-INDEX: 200; LEFT: 144px; POSITION: absolute; TOP: 1360px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Determining nitrogen application date and rate</asp:Label>
							<asp:Label id="lblQuestionFive_One" style="Z-INDEX: 201; LEFT: 144px; POSITION: absolute; TOP: 1328px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Selecting sowing date and variety</asp:Label>
							<asp:TextBox id="edtQuestionFive_One" style="Z-INDEX: 202; LEFT: 296px; POSITION: absolute; TOP: 1456px"
								runat="server" Height="26px" Width="225px" Font-Size="Small" Font-Names="Tahoma" Rows="3"></asp:TextBox>
							<asp:CheckBoxList id="chkQuestionFive_Four" style="Z-INDEX: 203; LEFT: 536px; POSITION: absolute; TOP: 1416px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionFive_Three" style="Z-INDEX: 204; LEFT: 536px; POSITION: absolute; TOP: 1384px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionFive_Two" style="Z-INDEX: 205; LEFT: 536px; POSITION: absolute; TOP: 1352px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionFive_One" style="Z-INDEX: 206; LEFT: 536px; POSITION: absolute; TOP: 1320px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="Label3" style="Z-INDEX: 207; LEFT: 640px; POSITION: absolute; TOP: 1296px" runat="server"
								Font-Size="Small" Font-Names="Tahoma">4</asp:Label>
							<asp:Label id="Label2" style="Z-INDEX: 208; LEFT: 672px; POSITION: absolute; TOP: 1296px" runat="server"
								Font-Size="Small" Font-Names="Tahoma">5</asp:Label>
							<asp:Label id="Label4" style="Z-INDEX: 209; LEFT: 608px; POSITION: absolute; TOP: 1296px" runat="server"
								Font-Size="Small" Font-Names="Tahoma">3</asp:Label>
							<asp:Label id="Label5" style="Z-INDEX: 210; LEFT: 576px; POSITION: absolute; TOP: 1296px" runat="server"
								Font-Size="Small" Font-Names="Tahoma">2</asp:Label>
							<asp:Label id="Label6" style="Z-INDEX: 211; LEFT: 544px; POSITION: absolute; TOP: 1296px" runat="server"
								Font-Size="Small" Font-Names="Tahoma">1</asp:Label>
							<asp:Label id="lblQuestionFive_Six" style="Z-INDEX: 212; LEFT: 144px; POSITION: absolute; TOP: 1504px"
								runat="server" Font-Size="Small" Font-Names="Tahoma">Other 2 (please list):</asp:Label>
							<asp:TextBox id="edtQuestionFive_Two" style="Z-INDEX: 213; LEFT: 296px; POSITION: absolute; TOP: 1496px"
								runat="server" Height="26px" Width="225px" Font-Size="Small" Font-Names="Tahoma" Rows="3"></asp:TextBox>
							<asp:CheckBoxList id="chkQuestionFive_Five" style="Z-INDEX: 214; LEFT: 536px; POSITION: absolute; TOP: 1448px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:CheckBoxList id="chkQuestionFive_Six" style="Z-INDEX: 215; LEFT: 536px; POSITION: absolute; TOP: 1488px"
								runat="server" Height="38px" Width="160px" Font-Size="XX-Small" RepeatColumns="5" ForeColor="White"
								RepeatDirection="Horizontal">
								<asp:ListItem Value="1">1</asp:ListItem>
								<asp:ListItem Value="2">2</asp:ListItem>
								<asp:ListItem Value="3">3</asp:ListItem>
								<asp:ListItem Value="4">4</asp:ListItem>
								<asp:ListItem Value="5">5</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionTwo" style="Z-INDEX: 216; LEFT: 128px; POSITION: absolute; TOP: 672px"
								runat="server" Height="20px" Width="512px" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma">2. Was 2005 your first year of subscription to Yield Prophet<sup>
									®</sup>? </asp:Label>
							<asp:CheckBoxList id="chkQuestionTwo" style="Z-INDEX: 217; LEFT: 144px; POSITION: absolute; TOP: 696px"
								runat="server" Width="528px" Font-Size="Small" Font-Names="Tahoma">
								<asp:ListItem Value="Yes">Yes</asp:ListItem>
								<asp:ListItem Value="No">No</asp:ListItem>
							</asp:CheckBoxList>
							<asp:Label id="lblQuestionOne" style="Z-INDEX: 219; LEFT: 128px; POSITION: absolute; TOP: 384px"
								runat="server" Height="20px" Width="496px" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma">1. Please tick the box that best describes your location</asp:Label>
							<asp:CheckBoxList id="chkQuestionOne" style="Z-INDEX: 220; LEFT: 144px; POSITION: absolute; TOP: 416px"
								runat="server" Width="528px" Font-Size="Small" Font-Names="Tahoma">
								<asp:ListItem Value="Victoria - Wimmera / Mallee">Victoria - Wimmera / Mallee</asp:ListItem>
								<asp:ListItem Value="Victoria - Other">Victoria - Other</asp:ListItem>
								<asp:ListItem Value="NSW - Northern">NSW - Northern</asp:ListItem>
								<asp:ListItem Value="NSW - Southern">NSW - Southern</asp:ListItem>
								<asp:ListItem Value="SA - Yorke Peninsula">SA - Yorke Peninsula</asp:ListItem>
								<asp:ListItem Value="SA - Other">SA - Other</asp:ListItem>
								<asp:ListItem Value="WA - Southern grainbelt">WA - Southern grainbelt</asp:ListItem>
								<asp:ListItem Value="WA - Northern grainbelt">WA - Northern grainbelt</asp:ListItem>
								<asp:ListItem Value="Queensland">Queensland</asp:ListItem>
							</asp:CheckBoxList></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
