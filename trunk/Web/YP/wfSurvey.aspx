<%@ Page language="c#" Codebehind="wfSurvey.aspx.cs" AutoEventWireup="false" Inherits="YieldProphet.wfSurvey" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>wfSurvey</title>
		<meta name="GENERATOR" Content="Microsoft Visual Studio .NET 7.1">
		<meta name="CODE_LANGUAGE" Content="C#">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body MS_POSITIONING="GridLayout">
		<form id="Form1" method="post" runat="server">
			<asp:Image id="imgBCGLogo" style="Z-INDEX: 101; LEFT: 112px; POSITION: absolute; TOP: 40px"
				runat="server" ImageUrl="Images\BCG.JPG" Height="112px" Width="88px"></asp:Image>
			<asp:TextBox id="edtEleven" style="Z-INDEX: 195; LEFT: 136px; POSITION: absolute; TOP: 3176px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px" TextMode="MultiLine" Rows="5"
				Height="102px"></asp:TextBox>
			<asp:TextBox id="edtTwelve" style="Z-INDEX: 194; LEFT: 136px; POSITION: absolute; TOP: 3360px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px" TextMode="MultiLine" Rows="5"
				Height="102px"></asp:TextBox>
			<asp:Label id="lblQuestionTwelve" style="Z-INDEX: 193; LEFT: 112px; POSITION: absolute; TOP: 3328px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">12. Do you have any suggestions for Yield Prophet in 2005?</asp:Label>
			<asp:Label id="lblQuestionEleven" style="Z-INDEX: 192; LEFT: 112px; POSITION: absolute; TOP: 3128px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">11. Do you have any general comments regarding Yield Prophet in 2004?</asp:Label>
			<asp:Label id="lblQuestionTen" style="Z-INDEX: 191; LEFT: 112px; POSITION: absolute; TOP: 2864px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">10. Did you discuss your Yield prophet reports with anyone?</asp:Label>
			<asp:CheckBoxList id="chkQuestionTen" style="Z-INDEX: 174; LEFT: 128px; POSITION: absolute; TOP: 2896px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px">
				<asp:ListItem Value="No">No</asp:ListItem>
				<asp:ListItem Value="Yes, other subscribers">Yes, other subscribers</asp:ListItem>
				<asp:ListItem Value="Yes, other growers who are not subscribers">Yes, other growers who are not subscribers</asp:ListItem>
				<asp:ListItem Value="Yes, government extension officer">Yes, government extension officer</asp:ListItem>
				<asp:ListItem Value="Yes, agronomic consultant">Yes, agronomic consultant</asp:ListItem>
				<asp:ListItem Value="Other: ">Other: </asp:ListItem>
			</asp:CheckBoxList>
			<asp:TextBox id="edtOtherTen" style="Z-INDEX: 176; LEFT: 200px; POSITION: absolute; TOP: 3016px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:Label id="lblQuestionNine" style="Z-INDEX: 190; LEFT: 112px; POSITION: absolute; TOP: 2640px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">9. How useful did you find the seasonal climate forecasts in Yield Prophet?</asp:Label>
			<asp:Label id="lblQuestionNine_One" style="Z-INDEX: 189; LEFT: 128px; POSITION: absolute; TOP: 2768px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">SOI:</asp:Label>
			<asp:Label id="lblQuestionNine_Two" style="Z-INDEX: 128; LEFT: 128px; POSITION: absolute; TOP: 2792px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">David Stephens DAWA:</asp:Label>
			<asp:CheckBoxList id="chkQuestionNine_Two" style="Z-INDEX: 129; LEFT: 304px; POSITION: absolute; TOP: 2784px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionNine_One" style="Z-INDEX: 134; LEFT: 304px; POSITION: absolute; TOP: 2752px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:Label id="lbl1_9" style="Z-INDEX: 106; LEFT: 312px; POSITION: absolute; TOP: 2728px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">1</asp:Label>
			<asp:Label id="lbl2_9" style="Z-INDEX: 109; LEFT: 344px; POSITION: absolute; TOP: 2728px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">2</asp:Label>
			<asp:Label id="lbl3_9" style="Z-INDEX: 117; LEFT: 376px; POSITION: absolute; TOP: 2728px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">3</asp:Label>
			<asp:Label id="lbl4_9" style="Z-INDEX: 114; LEFT: 408px; POSITION: absolute; TOP: 2728px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">4</asp:Label>
			<asp:Label id="lbl5_9" style="Z-INDEX: 119; LEFT: 440px; POSITION: absolute; TOP: 2728px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">5</asp:Label>
			<asp:Label id="lblNotAtAllUseful_9" style="Z-INDEX: 122; LEFT: 392px; POSITION: absolute; TOP: 2688px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="64px">Not at all useful</asp:Label>
			<asp:Label id="lblVeryUseful_9" style="Z-INDEX: 132; LEFT: 312px; POSITION: absolute; TOP: 2688px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px" Height="40px">Very useful</asp:Label>
			<asp:Label id="lblQuestionEight" style="Z-INDEX: 188; LEFT: 112px; POSITION: absolute; TOP: 2408px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">8. What is your expectation of the level of error that is acceptable from the Yield Prophet for protein predictions?</asp:Label>
			<asp:CheckBoxList id="chkQuestionEight" style="Z-INDEX: 185; LEFT: 128px; POSITION: absolute; TOP: 2456px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px">
				<asp:ListItem Value="&#177; 0.5%">&#177; 0.5%</asp:ListItem>
				<asp:ListItem Value="&#177; 1%">&#177; 1%</asp:ListItem>
				<asp:ListItem Value="&#177; 2%">&#177; 2%</asp:ListItem>
				<asp:ListItem Value="Other: ">Other: </asp:ListItem>
			</asp:CheckBoxList>
			<asp:TextBox id="edtOtherEight" style="Z-INDEX: 186; LEFT: 200px; POSITION: absolute; TOP: 2528px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:TextBox id="edtOtherSeven" style="Z-INDEX: 187; LEFT: 200px; POSITION: absolute; TOP: 2296px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:CheckBoxList id="chkQuestionSeven" style="Z-INDEX: 184; LEFT: 128px; POSITION: absolute; TOP: 2200px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px">
				<asp:ListItem Value="&#177; 0.25 t/ha">&#177; 0.25 t/ha</asp:ListItem>
				<asp:ListItem Value="&#177; 0.5 t/ha">&#177; 0.5 t/ha</asp:ListItem>
				<asp:ListItem Value="&#177; 0.75 t/ha">&#177; 0.75 t/ha</asp:ListItem>
				<asp:ListItem Value="&#177; 1.0 t/ha">&#177; 1.0 t/ha</asp:ListItem>
				<asp:ListItem Value="Other: ">Other: </asp:ListItem>
			</asp:CheckBoxList>
			<asp:Label id="lblQuestionSeven" style="Z-INDEX: 183; LEFT: 112px; POSITION: absolute; TOP: 2152px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">7. What is your expectation of the level of error that is acceptable from the Yield Prophet for yield predictions?</asp:Label>
			<asp:Label id="lblQuestionSix_One" style="Z-INDEX: 182; LEFT: 128px; POSITION: absolute; TOP: 2048px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Yield:</asp:Label>
			<asp:Label id="lblQuestionSix_Two" style="Z-INDEX: 138; LEFT: 128px; POSITION: absolute; TOP: 2080px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Protein:</asp:Label>
			<asp:CheckBoxList id="chkQuestionSix_Two" style="Z-INDEX: 140; LEFT: 240px; POSITION: absolute; TOP: 2072px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionSix_One" style="Z-INDEX: 144; LEFT: 240px; POSITION: absolute; TOP: 2040px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:Label id="lbl1_6" style="Z-INDEX: 107; LEFT: 248px; POSITION: absolute; TOP: 2016px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">1</asp:Label>
			<asp:Label id="lbl2_6" style="Z-INDEX: 111; LEFT: 280px; POSITION: absolute; TOP: 2016px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">2</asp:Label>
			<asp:Label id="lbl3_6" style="Z-INDEX: 123; LEFT: 312px; POSITION: absolute; TOP: 2016px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">3</asp:Label>
			<asp:Label id="lbl4_6" style="Z-INDEX: 116; LEFT: 344px; POSITION: absolute; TOP: 2016px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">4</asp:Label>
			<asp:Label id="lbl5_6" style="Z-INDEX: 124; LEFT: 376px; POSITION: absolute; TOP: 2016px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">5</asp:Label>
			<asp:Label id="lblVeryUnhappy_6" style="Z-INDEX: 130; LEFT: 328px; POSITION: absolute; TOP: 1976px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px">Very unhappy</asp:Label>
			<asp:Label id="lblVeryHappy_6" style="Z-INDEX: 142; LEFT: 248px; POSITION: absolute; TOP: 1976px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px" Height="40px">Very happy</asp:Label>
			<asp:Label id="lblQuestionSix" style="Z-INDEX: 181; LEFT: 112px; POSITION: absolute; TOP: 1928px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">6. Are you happy with the general level of simulation accuracy for all subscribers in 2004, as outlined in the results summary?</asp:Label>
			<asp:CheckBoxList id="chkQuestionFive_One" style="Z-INDEX: 180; LEFT: 240px; POSITION: absolute; TOP: 1784px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFive_Two" style="Z-INDEX: 165; LEFT: 240px; POSITION: absolute; TOP: 1816px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFive_Three" style="Z-INDEX: 166; LEFT: 240px; POSITION: absolute; TOP: 1848px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:Label id="lbl1_5" style="Z-INDEX: 136; LEFT: 248px; POSITION: absolute; TOP: 1760px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">1</asp:Label>
			<asp:Label id="lbl2_5" style="Z-INDEX: 139; LEFT: 280px; POSITION: absolute; TOP: 1760px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">2</asp:Label>
			<asp:Label id="lbl3_5" style="Z-INDEX: 145; LEFT: 312px; POSITION: absolute; TOP: 1760px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">3</asp:Label>
			<asp:Label id="lbl4_5" style="Z-INDEX: 143; LEFT: 344px; POSITION: absolute; TOP: 1760px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">4</asp:Label>
			<asp:Label id="lbl5_5" style="Z-INDEX: 147; LEFT: 376px; POSITION: absolute; TOP: 1760px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">5</asp:Label>
			<asp:Label id="lblVeryUnhappy_5" style="Z-INDEX: 151; LEFT: 336px; POSITION: absolute; TOP: 1720px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px">Very unhappy</asp:Label>
			<asp:Label id="lblVeryHappy_5" style="Z-INDEX: 169; LEFT: 248px; POSITION: absolute; TOP: 1720px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px" Height="40px">Very happy</asp:Label>
			<asp:Label id="lblQuestionFive_One" style="Z-INDEX: 131; LEFT: 128px; POSITION: absolute; TOP: 1792px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Yield:</asp:Label>
			<asp:Label id="lblQuestionFive_Two" style="Z-INDEX: 159; LEFT: 128px; POSITION: absolute; TOP: 1824px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Protein:</asp:Label>
			<asp:Label id="lblQuestionFive_Three" style="Z-INDEX: 155; LEFT: 128px; POSITION: absolute; TOP: 1856px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Growth Stage:</asp:Label>
			<asp:Label id="lblQuestionFive" style="Z-INDEX: 179; LEFT: 112px; POSITION: absolute; TOP: 1672px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">5. Are you happy with the level of simulation accuracy for your paddock in 2004?</asp:Label>
			<asp:TextBox id="edtQuestionFourElaborate" style="Z-INDEX: 178; LEFT: 176px; POSITION: absolute; TOP: 1528px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="5"
				Height="102px"></asp:TextBox>
			<asp:Label id="lblQuestionFourElaborate" style="Z-INDEX: 177; LEFT: 112px; POSITION: absolute; TOP: 1480px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">If you did have difficulty understanding what Yield Prophet output meant, please elaborate:</asp:Label>
			<asp:CheckBoxList id="chkQuestionFour_Four" style="Z-INDEX: 175; LEFT: 584px; POSITION: absolute; TOP: 1200px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Five" style="Z-INDEX: 173; LEFT: 584px; POSITION: absolute; TOP: 1232px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Eight" style="Z-INDEX: 172; LEFT: 584px; POSITION: absolute; TOP: 1328px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Nine" style="Z-INDEX: 171; LEFT: 584px; POSITION: absolute; TOP: 1360px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Six" style="Z-INDEX: 170; LEFT: 584px; POSITION: absolute; TOP: 1264px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Seven" style="Z-INDEX: 168; LEFT: 584px; POSITION: absolute; TOP: 1296px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" RepeatDirection="Horizontal" ForeColor="White"
				Font-Size="XX-Small">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Three" style="Z-INDEX: 167; LEFT: 584px; POSITION: absolute; TOP: 1168px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" ForeColor="White" Font-Size="XX-Small"
				RepeatDirection="Horizontal">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:CheckBoxList id="chkQuestionFour_Two" style="Z-INDEX: 164; LEFT: 584px; POSITION: absolute; TOP: 1136px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" RepeatDirection="Horizontal" ForeColor="White"
				Font-Size="XX-Small">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:TextBox id="edtOtherFour" style="Z-INDEX: 163; LEFT: 176px; POSITION: absolute; TOP: 1400px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:Label id="lblFour_Other" style="Z-INDEX: 162; LEFT: 128px; POSITION: absolute; TOP: 1400px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Other:</asp:Label>
			<asp:Label id="lblQuestionFour_Six" style="Z-INDEX: 161; LEFT: 128px; POSITION: absolute; TOP: 1272px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Difficulties with computer use in general:</asp:Label>
			<asp:Label id="lblQuestionFour_Seven" style="Z-INDEX: 160; LEFT: 128px; POSITION: absolute; TOP: 1304px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Lack of time:</asp:Label>
			<asp:Label id="lblQuestionFour_Two" style="Z-INDEX: 158; LEFT: 128px; POSITION: absolute; TOP: 1144px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Your own PC not working:</asp:Label>
			<asp:Label id="lblQuestionFour_Eight" style="Z-INDEX: 157; LEFT: 128px; POSITION: absolute; TOP: 1336px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Lack of interest:</asp:Label>
			<asp:Label id="lblQuestionFour_Three" style="Z-INDEX: 156; LEFT: 128px; POSITION: absolute; TOP: 1176px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Yield Prophet web-site not working:</asp:Label>
			<asp:Label id="lblQuestionFour_Nine" style="Z-INDEX: 154; LEFT: 128px; POSITION: absolute; TOP: 1368px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Lack of support or instruction:</asp:Label>
			<asp:Label id="lblQuestionFour_Five" style="Z-INDEX: 153; LEFT: 128px; POSITION: absolute; TOP: 1240px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Difficulties in understanding what Yield Prophet reports meant:</asp:Label>
			<asp:Label id="lblQuestionFour_Four" style="Z-INDEX: 152; LEFT: 128px; POSITION: absolute; TOP: 1208px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Difficulties in understanding how the web site worked:</asp:Label>
			<asp:Label id="lblVeryLimited_4" style="Z-INDEX: 150; LEFT: 680px; POSITION: absolute; TOP: 1040px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px">Very limited</asp:Label>
			<asp:Label id="lbl5_4" style="Z-INDEX: 148; LEFT: 720px; POSITION: absolute; TOP: 1080px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">5</asp:Label>
			<asp:Label id="lbl3_4" style="Z-INDEX: 146; LEFT: 656px; POSITION: absolute; TOP: 1080px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">3</asp:Label>
			<asp:Label id="lbl4_4" style="Z-INDEX: 141; LEFT: 688px; POSITION: absolute; TOP: 1080px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">4</asp:Label>
			<asp:Label id="lblQuestionFour" style="Z-INDEX: 126; LEFT: 112px; POSITION: absolute; TOP: 992px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="544px" Height="20px">4.  Did you feel that your use of Yield Prophet was restricted or limited by any of the following factors?</asp:Label>
			<asp:TextBox id="edtOtherThree" style="Z-INDEX: 121; LEFT: 200px; POSITION: absolute; TOP: 880px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:CheckBoxList id="chkQuestionThree" style="Z-INDEX: 120; LEFT: 128px; POSITION: absolute; TOP: 808px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px">
				<asp:ListItem Value="Yield Prophet output didn't apply to any management decisions">Yield Prophet output didn't apply to any management decisions</asp:ListItem>
				<asp:ListItem Value="I didn't trust the Yield Prophet predictions">I didn't trust the Yield Prophet predictions</asp:ListItem>
				<asp:ListItem Value="Yield Prophet didn't tell me anything I didn't already know ">Yield Prophet didn't tell me anything I didn't already know </asp:ListItem>
				<asp:ListItem Value="Other: ">Other: </asp:ListItem>
			</asp:CheckBoxList>
			<asp:Label id="lblQuestionThree" style="Z-INDEX: 118; LEFT: 104px; POSITION: absolute; TOP: 760px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="552px" Height="20px">3. If you didn't find the model useful from a management perspective, please elaborate:</asp:Label>
			<asp:Label id="lblQuestionTwo" style="Z-INDEX: 115; LEFT: 104px; POSITION: absolute; TOP: 520px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="552px" Height="20px">2.  If you did find Yield Prophet useful in 2004, how so?</asp:Label>
			<asp:CheckBoxList id="chkQuestionTwo" style="Z-INDEX: 110; LEFT: 128px; POSITION: absolute; TOP: 552px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px">
				<asp:ListItem Value="Selecting sowing date and variety">Selecting sowing date and variety</asp:ListItem>
				<asp:ListItem Value="Determining nitrogen application date and rate">Determining nitrogen application date and rate</asp:ListItem>
				<asp:ListItem Value="Calculating yield potential for insurance purposes">Calculating yield potential for insurance purposes</asp:ListItem>
				<asp:ListItem Value="Calculating yield potential for marketing purposes">Calculating yield potential for marketing purposes</asp:ListItem>
				<asp:ListItem Value="Other: ">Other: </asp:ListItem>
			</asp:CheckBoxList>
			<asp:TextBox id="edtOtherOne" style="Z-INDEX: 112; LEFT: 200px; POSITION: absolute; TOP: 408px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:Label id="lblHeadingOne" style="Z-INDEX: 102; LEFT: 248px; POSITION: absolute; TOP: 32px"
				runat="server" Font-Bold="True" Font-Size="XX-Large" Font-Names="Tahoma" Width="474px">Yield Prophet 2004</asp:Label>
			<asp:Label id="lblHeadingTwo" style="Z-INDEX: 103; LEFT: 256px; POSITION: absolute; TOP: 96px"
				runat="server" Font-Bold="True" Font-Size="X-Large" Font-Names="Tahoma">Subsriber survey & feedback</asp:Label>
			<asp:Label id="lblInstructions" style="Z-INDEX: 104; LEFT: 112px; POSITION: absolute; TOP: 168px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="672px">This is a short survey for Yield Prophet users in 2004 designed to improve the quality of delivery for Yield Prophet in 2005. You can complete this survey anonymously on-line, simply follow the links from the Yield Prophet front page (www.yieldprophet.com.au), or fill out in hard-copy and return to the BCG office - PO Box 85, Birchip 3483, Fax - 03 5492 2753. Thanks in advance!</asp:Label>
			<asp:Label id="lblQuestionOne" style="Z-INDEX: 105; LEFT: 112px; POSITION: absolute; TOP: 304px"
				runat="server" Font-Bold="True" Font-Size="Small" Font-Names="Tahoma" Width="551px" Height="20px">1. How would you describe your Yield Prophet experience in 2004?</asp:Label>
			<asp:CheckBoxList id="chkQuestionOne" style="Z-INDEX: 108; LEFT: 128px; POSITION: absolute; TOP: 336px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="528px">
				<asp:ListItem Value="Interesting and useful from a management perspective">Interesting and useful from a management perspective</asp:ListItem>
				<asp:ListItem Value="Interesting, but not useful from a management perspective">Interesting, but not useful from a management perspective</asp:ListItem>
				<asp:ListItem Value="Un-interesting and not useful from a management perspective">Un-interesting and not useful from a management perspective</asp:ListItem>
				<asp:ListItem Value="Other: ">Other: </asp:ListItem>
			</asp:CheckBoxList>
			<asp:TextBox id="edtOtherTwo" style="Z-INDEX: 113; LEFT: 200px; POSITION: absolute; TOP: 648px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="464px" TextMode="MultiLine" Rows="3"
				Height="64px"></asp:TextBox>
			<asp:Button id="btnSubmit" style="Z-INDEX: 125; LEFT: 320px; POSITION: absolute; TOP: 3492px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="104px" Height="32px" Text="Submit"></asp:Button>
			<asp:CheckBoxList id="chkQuestionFour_One" style="Z-INDEX: 127; LEFT: 584px; POSITION: absolute; TOP: 1104px"
				runat="server" Width="160px" RepeatColumns="5" Height="38px" RepeatDirection="Horizontal" ForeColor="White"
				Font-Size="XX-Small">
				<asp:ListItem Value="1">1</asp:ListItem>
				<asp:ListItem Value="2">2</asp:ListItem>
				<asp:ListItem Value="3">3</asp:ListItem>
				<asp:ListItem Value="4">4</asp:ListItem>
				<asp:ListItem Value="5">5</asp:ListItem>
			</asp:CheckBoxList>
			<asp:Label id="lblQuestionFour_One" style="Z-INDEX: 133; LEFT: 128px; POSITION: absolute; TOP: 1112px"
				runat="server" Font-Size="Small" Font-Names="Tahoma">Your internet connection line-speed:</asp:Label>
			<asp:Label id="lbl1_4" style="Z-INDEX: 135; LEFT: 592px; POSITION: absolute; TOP: 1080px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">1</asp:Label>
			<asp:Label id="lbl2_4" style="Z-INDEX: 137; LEFT: 624px; POSITION: absolute; TOP: 1080px" runat="server"
				Font-Size="Small" Font-Names="Tahoma">2</asp:Label>
			<asp:Label id="lblNotLimited_4" style="Z-INDEX: 149; LEFT: 584px; POSITION: absolute; TOP: 1040px"
				runat="server" Font-Size="Small" Font-Names="Tahoma" Width="48px" Height="40px">Not limited</asp:Label>
		</form>
	</body>
</HTML>
