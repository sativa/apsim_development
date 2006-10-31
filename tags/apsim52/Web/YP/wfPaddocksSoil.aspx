<%@ Page language="c#" Codebehind="wfPaddocksSoil.aspx.cs" AutoEventWireup="false" Inherits="YP2006.wfPaddocksSoil" %>
<%@ Register TagPrefix="jwg" Namespace="Janus.Web.GridEX" Assembly="Janus.Web.GridEX" %>
<%@ Register TagPrefix="xceedchart" Namespace="Xceed.Chart.Server" Assembly="Xceed.Chart.Server, Version=3.0.100.0, Culture=neutral, PublicKeyToken=ba83ff368b7563c6" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" >
<HTML>
	<HEAD>
		<title>Paddock's Soil</title>
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
					<TD style="WIDTH: 881px; HEIGHT: 522px" vAlign="top" align="left" colSpan="2">
						<DIV style="WIDTH: 804px; POSITION: relative; HEIGHT: 1579px" align="left" ms_positioning="GridLayout">
							<asp:label id="lblYieldProphet" style="Z-INDEX: 100; LEFT: 264px; POSITION: absolute; TOP: 8px"
								runat="server" Font-Names="Arial Black" ForeColor="MediumBlue" Font-Size="X-Large" Font-Bold="True"
								Height="42px">Yield Prophet<sup>®</sup></asp:label>
							<asp:panel id="pnlNavigationMenu" style="Z-INDEX: 101; LEFT: 0px; POSITION: absolute; TOP: 104px"
								runat="server" BackColor="MediumBlue" Height="64px" Width="800px" BorderColor="White" BorderStyle="None">
								<DIV id="divConsultant" style="WIDTH: 801px; POSITION: relative; HEIGHT: 51px; WIDTH800px: "
									align="center" ms_positioning="GridLayout">
									<asp:LinkButton id="btnPersonalDetails" style="Z-INDEX: 100; LEFT: 440px; POSITION: absolute; TOP: 8px"
										runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="160px"
										CommandName="wfPersonalDetails.aspx">My Personal Details</asp:LinkButton>
									<asp:LinkButton id="btnManageReports" style="Z-INDEX: 101; LEFT: 696px; POSITION: absolute; TOP: 8px"
										runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="88px"
										CommandName="wfReportsMenuConsultant.aspx">My Reports</asp:LinkButton>
									<asp:LinkButton id="btnManageItems" style="Z-INDEX: 102; LEFT: 16px; POSITION: absolute; TOP: 8px"
										runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="104px"
										CommandName="wfManageGrowers.aspx">My Growers</asp:LinkButton>
									<asp:LinkButton id="btnGrowersPaddocks" style="Z-INDEX: 103; LEFT: 16px; POSITION: absolute; TOP: 32px"
										runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="382px"
										CommandName="wfPaddocksMenu.aspx">GrowerPlaceHolder's Paddocks</asp:LinkButton>
									<asp:LinkButton id="btnGrowersReports" style="Z-INDEX: 104; LEFT: 408px; POSITION: absolute; TOP: 32px"
										runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="368px"
										CommandName="wfReportsMenu.aspx">GrowerPlaceHolder's Reports</asp:LinkButton>
									<asp:LinkButton id="btnMainMenu" style="Z-INDEX: 105; LEFT: 208px; POSITION: absolute; TOP: 8px"
										runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial" Width="112px"
										CommandName="wfMainMenu.aspx">My Main Menu</asp:LinkButton></DIV>
							</asp:panel>
							<DIV id="divPage" style="Z-INDEX: 102; LEFT: 0px; WIDTH: 800px; POSITION: absolute; TOP: 64px; HEIGHT: 48px"
								align="center" ms_positioning="GridLayout">
								<asp:label id="lblHeading" style="Z-INDEX: 106; LEFT: 8px; POSITION: absolute; TOP: 8px" runat="server"
									Height="20px" Width="785px" Font-Names="Arial Black" ForeColor="DarkGray" Font-Size="Medium"
									Font-Bold="True">Soil for GrowerPlaceHolder's paddock PaddockPlaceHolder</asp:label></DIV>
							<asp:panel id="pnlPaddock" style="Z-INDEX: 103; LEFT: 0px; POSITION: absolute; TOP: 168px"
								runat="server" BackColor="DarkGray" Height="40px" Width="800px" BorderColor="White" BorderStyle="None">
								<DIV id="divPaddock" style="WIDTH: 801px; POSITION: relative; HEIGHT: 46px; WIDTH800px: "
									ms_positioning="GridLayout">
									<asp:LinkButton id="btnPaddocksRainfall" style="Z-INDEX: 100; LEFT: 8px; POSITION: absolute; TOP: 8px"
										tabIndex="12" runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										Width="144px" CommandName="wfPaddocksRainfall.aspx">Paddock's Rainfall</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksApplictions" style="Z-INDEX: 101; LEFT: 288px; POSITION: absolute; TOP: 8px"
										tabIndex="14" runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										Width="185px" CommandName="wfPaddocksApplications.aspx">Paddock's Applications</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksSoil" style="Z-INDEX: 102; LEFT: 160px; POSITION: absolute; TOP: 8px"
										tabIndex="13" runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										Width="120px" CommandName="wfPaddocksSoil.aspx">Paddock's Soil</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksCrop" style="Z-INDEX: 103; LEFT: 480px; POSITION: absolute; TOP: 8px"
										tabIndex="15" runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										Width="128px" CommandName="wfPaddocksCrop.aspx">Paddock's Crop</asp:LinkButton>
									<asp:LinkButton id="btnPaddocksInformation" style="Z-INDEX: 104; LEFT: 616px; POSITION: absolute; TOP: 8px"
										tabIndex="16" runat="server" Height="8px" Font-Bold="True" ForeColor="White" Font-Names="Arial"
										Width="176px" CommandName="wfPaddocksInformation.aspx">Paddock's Information</asp:LinkButton></DIV>
							</asp:panel>
							<asp:label id="lblSoilType" style="Z-INDEX: 104; LEFT: 104px; POSITION: absolute; TOP: 328px"
								runat="server" Width="80px" Height="16px" Font-Names="Arial">Soil Type:</asp:label>
							<asp:dropdownlist id="cboSoilType" style="Z-INDEX: 105; LEFT: 336px; POSITION: absolute; TOP: 328px"
								tabIndex="1" runat="server" Height="24px" Width="344px" Font-Names="Arial"></asp:dropdownlist>
							<asp:label id="lblRootingDepth" style="Z-INDEX: 106; LEFT: 104px; POSITION: absolute; TOP: 368px"
								runat="server" Width="152px" Height="16px" Font-Names="Arial">Max Rooting Depth:</asp:label>
							<asp:textbox id="edtRootingDepth" style="Z-INDEX: 107; LEFT: 336px; POSITION: absolute; TOP: 368px"
								runat="server" Width="312px" Font-Names="Arial" tabIndex="2"></asp:textbox>
							<asp:Label id="lblRootingDepthUnit" style="Z-INDEX: 108; LEFT: 664px; POSITION: absolute; TOP: 376px"
								runat="server" Font-Names="Arial">cm</asp:Label>
							<asp:Label id="InvalidSWLabel" style="Z-INDEX: 109; LEFT: 552px; POSITION: absolute; TOP: 424px"
								runat="server" Height="92px" Width="208px" ForeColor="Red" Font-Bold="True" Visible="False">The water values entered in the grid don't match the soil type you have choosen. Please contact James Hunt.</asp:Label>
							<jwg:gridex id=grdInitialDate style="Z-INDEX: 110; LEFT: 336px; POSITION: absolute; TOP: 448px" runat="server" Height="20px" Width="168px" Font-Names="Garamond" ColumnHeaders="False" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="InitialDate" DataSource="<%# dsInitialDate %>" tabIndex=4>
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
								<RootTable DataMember="InitialDate" Key="SowDate">
									<Columns>
										<jwg:GridEXColumn UseType="System.DateTime" EditType="CalendarDropDown" Key="InitialDate" FormatString="dd/MM/yyyy"
											HasValueList="True" DataMember="InitialDate" DefaultGroupPrefix="InitialDate:" InvalidValueAction="DiscardChanges"
											NullText="" Caption="InitialDate" Width="148px">
											<CellStyle Width="148px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="Control" ForeColor="ControlText" Height="20px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<asp:CheckBox id="chkUseEC" style="Z-INDEX: 111; LEFT: 104px; POSITION: absolute; TOP: 408px"
								runat="server" Width="264px" Height="24px" Text="Use EC to constrain crop growth?" TextAlign="Left"
								AutoPostBack="True" Font-Names="Arial" tabIndex="3"></asp:CheckBox>
							<asp:label id="lblInitialConditions" style="Z-INDEX: 112; LEFT: 104px; POSITION: absolute; TOP: 448px"
								runat="server" Width="120px" Height="16px" Font-Names="Arial">Initial Conditions:</asp:label>
							<asp:label id="lblWaterType" style="Z-INDEX: 113; LEFT: 104px; POSITION: absolute; TOP: 488px"
								runat="server" Font-Names="Arial"> Starting water is in %</asp:label>
							<asp:radiobuttonlist id="rdbSWUnit" style="Z-INDEX: 114; LEFT: 256px; POSITION: absolute; TOP: 480px"
								runat="server" Width="232px" Height="34px" RepeatDirection="Horizontal" Font-Names="Arial" tabIndex="5">
								<asp:ListItem Value="GravimetricPercent" Selected="True">Gravimetric</asp:ListItem>
								<asp:ListItem Value="VolumetricPercent">Volumetric</asp:ListItem>
							</asp:radiobuttonlist>
							<asp:label id="lblDepthOne" style="Z-INDEX: 115; LEFT: 224px; POSITION: absolute; TOP: 528px"
								runat="server" Font-Names="Arial" ForeColor="Red">Depths should be entered as 0-10, 10-20, etc.</asp:label>
							<jwg:gridex id=grdSoilSampleOne style="Z-INDEX: 116; LEFT: 56px; POSITION: absolute; TOP: 552px" runat="server" Height="184px" Width="656px" Font-Names="Arial" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="SoilSampleOne" DataSource="<%# dsSoilSampleOne %>" AutomaticSort="False" AllowColumnDrag="False" tabIndex=6>
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
								<RootTable DataMember="SoilSampleOne" Key="SoilSample">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="Water" DataMember="Water" DefaultGroupPrefix="Water (% Grav):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Water (%)" Width="183px">
											<CellStyle Width="183px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="NO3" DataMember="NO3" DefaultGroupPrefix="NO3 (ppm or mg/kg):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="NO&lt;sub&gt;3&lt;/sub&gt; (ppm or mg/kg)"
											Width="183px">
											<CellStyle Width="183px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="NH4" DataMember="NH4" DefaultGroupPrefix="NH4 (ppm or mg/kg):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="NH&lt;sub&gt;4&lt;/sub&gt; (ppm or mg/kg)"
											Width="183px">
											<CellStyle Width="183px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="DarkGray" ForeColor="ControlText" Height="24px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<jwg:gridex id=grdSoilSampleTwo style="Z-INDEX: 117; LEFT: 56px; POSITION: absolute; TOP: 752px" runat="server" Height="184px" Width="656px" Font-Names="Arial" UpdateMode="RowUpdateBatch" ImagesFolderPath="/gridex/images" EditorsFrameUrl="/gridex/images/blank.html" ScriptsFolderPath="/gridex/scripts" AllowEdit="True" GroupByBoxVisible="False" GridLineColor="ScrollBar" DataMember="SoilSampleTwo" DataSource="<%# dsSoilSampleTwo %>" AutomaticSort="False" AllowColumnDrag="False" tabIndex=7>
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
								<RootTable DataMember="SoilSampleTwo" Key="SoilSample">
									<Columns>
										<jwg:GridEXColumn UseType="System.String" Key="Depth" DataMember="Depth" DefaultGroupPrefix="Depth (cm):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Depth (cm)"></jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="OC" DataMember="OC" DefaultGroupPrefix="OC (%C):" InvalidValueAction="DiscardChanges"
											NullText="" Caption="OC (%C)" Width="103px">
											<CellStyle Width="103px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="EC" DataMember="EC" DefaultGroupPrefix="EC (dS/m):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="EC (dS/m)" Width="103px">
											<CellStyle Width="103px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="PH" DataMember="PH" DefaultGroupPrefix="PH (CaCl2):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="pH (CaCl&lt;sub&gt;2&lt;/sub&gt;)" Width="103px">
											<CellStyle Width="103px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="ESP" DataMember="ESP" DefaultGroupPrefix="ESP (%):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="ESP (%)" Width="103px">
											<CellStyle Width="103px"></CellStyle>
										</jwg:GridEXColumn>
										<jwg:GridEXColumn UseType="System.String" Key="CL" DataMember="CL" DefaultGroupPrefix="Cl (ppm or mg/kg):"
											InvalidValueAction="DiscardChanges" NullText="" Caption="Cl (ppm or mg/kg)" Width="140px">
											<CellStyle Width="140px"></CellStyle>
										</jwg:GridEXColumn>
									</Columns>
								</RootTable>
								<HeaderFormatStyle BorderStyle="Solid" BackColor="DarkGray" ForeColor="ControlText" Height="24px" Appearance="RaisedLight"
									BorderWidth="1px" BorderColor="GrayText"></HeaderFormatStyle>
								<GroupRowFormatStyle TextAlign="left" BackColor="Control" ForeColor="ControlText" Height="20px" VerticalAlign="top"></GroupRowFormatStyle>
								<PreviewRowFormatStyle ForeColor="Blue" Height="100%"></PreviewRowFormatStyle>
								<EditorsFormatStyle BackColor="Control"></EditorsFormatStyle>
								<SelectedFormatStyle BackColor="Highlight" ForeColor="HighlightText" Height="20px" VerticalAlign="top"></SelectedFormatStyle>
								<RowFormatStyle BorderStyle="Solid" TextAlign="left" BackColor="Window" ForeColor="WindowText" Height="20px"
									VerticalAlign="top" BorderWidth="1px" Font-Size="Small"></RowFormatStyle>
								<GroupTotalRowFormatStyle BackColor="Control" Height="20px"></GroupTotalRowFormatStyle>
								<PageNavigatorFormatStyle BackColor="Control" Appearance="RaisedLight" Width="100%"></PageNavigatorFormatStyle>
								<FilterRowFormatStyle BackColor="Window" ForeColor="WindowText"></FilterRowFormatStyle>
								<FocusCellFormatStyle BorderStyle="Solid" BorderWidth="1px" BorderColor="Highlight"></FocusCellFormatStyle>
							</jwg:gridex>
							<xceedchart:chartservercontrol id="cscSoilChart" style="Z-INDEX: 119; LEFT: 88px; POSITION: absolute; TOP: 944px"
								runat="server" Height="552px" Width="592px" tabIndex="8">
								<Xceed.Chart.Server.StringHolderCount Count="121"></Xceed.Chart.Server.StringHolderCount>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;ED3D077C1455FA994D76C9A690D03B84AE821008100B288180705272846A8B93EC242C6E89BB9B40384F51B163EF0D51B1D753CF729EA77736ACD8DB8162011541383D4D13F8BFEFCDBC996F66DEDB59CED5BFF7BB597EF99879EF7BE5ABEFCDF7DEBCC990323232F6911FFC0FBF3C0F010B16D7284A60D4D4A5722C316A6A34&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A68C2C5AA8C4E2C1686452C9A8E251638A8B47158F2C9ADA104A34C4944911A521119343238B2A1AAA43C19AA395A6F9D19394C8A46AF99092DADA92098754978E9F505233212F9354BD04575D999023013916484BF55E2061A0B5E7EAE5D46824118B86A6C871A503C1CA0E57D1E4B83F5C354BA953228138499A25572BA178&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5EB86A919C506261397612DC4C916B4EAA8B451B22819C7055A59248042375F15EE1AA99118224D724828DC144D3DC7A729D2004C4B3D45F91A81FA1905203881E6E67D5CE98B10EE4714CBDA53D36B081BF0770B1758ACCC803B8C806C580D3CBD649C604E8DC046E0D02DE981B87E2FE2C005E003E00201C7F36003F01395E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C87664650E41EA18AE8A9441830ACDF485ABE637D52B1DC255F1397258E93D4FA90E92AE558794B2584C6E9A158C2746A8287DF859B458374008584B74B7A552E44223A52A9850C2F10294100FAE543AA3FB4655DD333225F847E8CF1E50D91427A5462D881096552AB1A01C0AAEA46C9B110D0594D8CFCDCFCEF66816EE077E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E598602E81922612AF3725CD04F9FC97705967F2AFC3637F1EE52986F998BFBEFDB26997CF3C3E83FFF077A4BCC5B040E331647B3BEC8F3774D92C647321652D869D344F022AEDCD4E711C216C9D1E23249321757A30149A565B4B385F601947C9C839C4A12E5A49A6D08610A2DE0A600F721EA83235CDF17706D0054057A0D0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9F741404FA3303F5D599F1B1D14C6559227359224EFE02998A2C67D6C463DE58A83A5A9FC158962B65E74A92A439E355476678B40B929805DED8492AD0C3AC723921FB2906E8555E593CAE84AB434DA052528694EDEB4690BAF3C74D18527DDD7FB1495E218C25A026FE1E04E41748D403429B3D09E8211857E834003A5598CF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8AF7568B7764C5FB08F5CC709B94B6BEBFE82CB3B08075B01F80FED0CB8E5221EBE50002FA27773A9456E8646127ADAAFCC2BC81E4BFF99AE4CB63F272A2594677C790EE8E2F4EB1BFC525B5E36B4B6BC78C098C2F964B642F28734A363598207622D3A89961B94EA129A05B9DF00C98F8CB682C4F336476533D438E572E9503&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D1E5E426A217CC2737731B08CD9589A610BD9B1989A0BB294AA3125A140C249616B2BB72E2E6E4488DD295B489EF594BB48259C1BAA5097243EBA637B95A4E39616EAE9601D7C4E3D092646A1FA10D91097CA4424E90EC881FBA4A0C3F1AEB477B5C118B92696A22A8C44798FACDCB35E8E0E51A74158972195D43EC0876BAFB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DA910C3E70320DBEF4119404DEF4111484BC1EF63CDA939E1C7A28A1BD39191A9F7B71B254BE779F158C701AE9614B56DBE8654FD79AE869CF515B20834906FC65A88F64E0374D37E46F307F5430590038DCAE66D31C45BB3A30598674402A36C7EA170CA0A00C54D152C5C9CEDEFF9EFE1A19D9D9D9FF032426CBA353D28FF7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EDDB978ACAC1C4AA43A31C6A50AAAA326851F8793793E2DC364095BD143FE7A448747984267A8913492859115227E8A3BF43878119C66F3D99BDE5489B488D50AB39D5FB4F9294B2FE0A3AFB11A9C3499EFCA2D287A4E8475AAF585C4AFAC0D2551FEDEAFB96542F4D7DCF92EAA7A9EF5A523BD0D477B8F5BE6D491D0CA91934&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;56A6F6497A0B77F24D5E8FDFE0D6BC91DBE3D7B93D7E8DDBE357B9F5BEE2D8E3979D30F24A0958846576D4ACB44D0CBD5DF66BCE0E9341325ECF8659139902930785F9CA0A6870B65C4F3C7D47C3FF830EC214E2A8981C082A9104D5ACCE2861A14C6A8824084E5C35B36048819972B636E48C611763FB85ABCA028D30F40658&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E172255E130BD6C3FC9BA04D0926C2723D9958D056BB19DD1D6174B5B729D5D4EB5EA62C4C401F738E89967EFC3C8D2C734933855D4C792AB19CB4B10798D292B0C05C58E5466773F7801A3AC44A59FA4FBDEC67D72CF21F635BA9F5214DC730B1B0D43AC1D6D0102F4BAD4143561366AAB03933774BFFA3B1A0984B4712BE42&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;33DD2CB5A9DC1D289CA5186C67B30897C1BF3083E920BE81B8D0E49C86E73A8F5CEC91C778E4B11EB9C41328F604C67802633D81124FBCD8131FE3898FF5C44B3C4AB14719E351C67A1492BE34C3FACBB5FE4A59D060BFFEF00FEE7797F90F05425E20843848DA3C4097B221EB7952D2510104659F236553D00B7E69DF243AB2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FE833B6DF93B6F7CCBC9F13E239C7599252C988D3C4D8A6FA0D5A689F1D253A4B617D86CE1AFE4E27976F324B9784EBBF103ADD2135C521FE7932A3D46D29ED1D2BD5D530B7BF9D4D0677CC1BC5964C08DCF8F464389607D6F7239B5219E8886895B288B29725922110B569367C4383CBD4F6D88C5A331E01A1969ABE7D62BE4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A9788EB27C513012882E27837564412478728332A7215CADC420B4250CDEE81501C32535E438C904BC8F0AA7924669BEF0328AA17C0F71342C4F5D0D84389B12275C285BA1C43BC00A6088AE04D2E7E8D9D18012224825E50B83CA72C2A3EA85C178B03AA4478A495E2D7D48F5938B190A9481A472A53EB1142622E479BE3E24&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;37CD8DA8C1AEEEE1AA8A9812506AC9736B80F682DA8BFA5C4A678F590339E146E8A1796570800DAB6C45D08233844F371098A4329DC1060332AD4EDF1C7F53994365481C55F6505BB33C923D6C19F06800B300CC063007C05CA8CB570102A4253232D64CC9C8386E8A3E717E80A8456AED9895832D84797B2789488262E412D9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;CD8B2E9F4A66A6106482C72A7ADD855CCF88C6822BA391841C9A2DC7EA8211889C2D8408440D4B81592270AEB309BB2C14AC8B1422649A608D8313F599A1C810830F574D8F46494E1E5BA986D06FAE1A94234A1551A8BE683FF29C99DD47401074C52318932DDD138DEFA62E97A61E54177509E61D9E9F910FACF00803AE3A87&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D862738EF6570DF17C90FFFD448192B1CBAE3674C5E33EC10866E1A27D04A3CB7FF70AC64E1373ED6521C5BF00C042008B002C06B0042AED9742701CD675BB8083B26B6EAD4D73E3C0FB5CF5F105626B71AB86921E6565E5E6F617370A45F773E52523E35162DE33A6F88E81A7D18AB245538B26152D5EBC381CF61F0BB41E07&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8C80907B3F1DF9B829BEE3C15409B28E7B02E056015F0E4D6DCE0BF19342236596DC146D48E42FD6522A096394FC25F80E1E3F193E511428DF97D38C5E57299DCD51417AD71105488ACC9FFAB069853484D47134778DDE3A46E4AA117D35797E3444184EA6C7DD8854E9754209685964FCFC2F59FCCCCAD2973F8B04F4EB84F2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5D8741B227E3D7DA12E0BDC96AF4BCFE0A46A9B55C2F6590C12FE6AFA14BB51806F0B2EDAC14E60FE02C61A882D499013AF42931776B0D5FC674EB9242198D61AD9605CB86DED9A94EC8DCED08C2DD357594AD182EC52C9E93C25416903D4AC4136A343DA587E2AA7D64493085CD3D485C4B65B4214697E7D076363AF5CDF86E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;77FD11EC0FBCB53F089D9A9BCADC59D236261239C4E4AC3EDC514BCD2C651B1596419B3029E86A7F6A92AE207EE351F68CF81D99CF7A173ACCB5B2D46D1ABCB19F3CDED586C888A12FD86AF7EAD30E3CDEA53013D02448BB18D656E0E04F82D9CC423D2B8AB2BC8B1DE67FB05F001E48A7ADA897D5591BCCD4C93CE6A44AA2AF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C966F01A05539590BA5CDBD59CA65296AF0DF4736B6BE38A8929845133C144C8D39C31A382A5C838CD26B5B1499591D88FAEB286C83DAF481F3DD75E324B534DFA13EDCE3378E051179DC82F05B11C9884BD40A2F93971B0C02CF01A6CE6FF231E1D6F2F217AEFCC25FEC8E9438F0A59E8CF7F328018007014FE048006008DD0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E412E707915C754D83850F4C7B27C0A6D42B55D5165B1396C09C735AA04EA95088EF89C0D6A95A8B51D69A8C12B63DD0A75FD38E2BF234AAD66A2475D23674187CCC30B44D928812E5E60E4E4E99C30AAE792B562A38FC1D2B966E1A6ED74B17D2543FA8C707CE13C6FDCCFDE6878EFC2B0034015809B51D9BE1E8E3BAA8A3F6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7462D726D982E4E6C620B8498733A202B59A908DCB25203FAB029812961097524D7D1119794372BDBAD4D309BB1F164B303D4BE26001E95DED3C2500D1A9A3628A024F7AB55308E1905016AA5F2AE7E98D12B48EFA0D45CED76FA18891490B66193AC304D0C5126F07BED0AD11D930BC4AE90C44A410016031743A70FE21C3FE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9B0C7F92B49A68CD7D2C1E7B26B9B897C5634FB515A1415ED39F749C60345E8547E3F9C4B748F0944A9F6A4F1777E654DC993FE2CEAC4EA5335582CEACC49D59089DA9611BD8CE49F2D004AF4F183BF502FAEE354961A5CF836516EE538551AE966DA0BB00C01A001702B808C0C5002E01702954ED97EA58D59711D05B3C7736&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EA5FCAEABF02C09500AE027035806B005C0BE03A5ABF17E667FB39D3FB4F66C9F01CA2C6B6AFD79EC0E853D8322BB72D33BD02350A593B2FAABA0E30CF6921A591DE80F11E138D866900874C209518432BD45288D52D8C861AC2B0E1AE7696529B50E33B60BAF3E86C98DE42A5F3A3F5EA0D0C2B53A2091AF787FBAEEA6C8A5C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4D0F262A131035A86B82602771C3CB549EC0B3042956BD20AEFC2E084B47A459083491812A5EAFD09732CA22752105A2E38B69501CAE96D02B5D7B55B7A1AF740DE106482CDD28B54E0B74EE993B57AA45A54B4DF6B2418B24779F4A9C4C5946C63A72BF8ED8DDDFA67803C43652EA013F4EE7ADB18E3C828EF18B4B39EF0F9E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A86F180E3337B156EC2664EC264EC46EE2E654DC449435718BB889E37113C7E1266E4BA589935913B78B9B58A2354179B0183771570A4D7863FB3595759FB0854FD877D3A76A0CEF61460ACA1D4F7132AE3D4DA6B00537CB615F1D9DE3CDE72D794A787F134CC8A17352A5236A03439DE788DAC8507FEF84EA5D91C2E436F7BF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;605F93BB67E7D7DD522255A4738BC19FA0C639788BC16CBCC560966D8BC1EFB85B0C660AB618CC405B0C24783C02AD978ED229307E2F6D987184FD3E050AFE02354EC31494630AA6DA2828E352305940C1917893C4CA541F363B6981A6FF86771A7E8DBDF5969DF4EEF6EE0CE908BC0B78126F4BF044EED6DDC3B95B820FE36E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;093E94BB25F8106EBDA58E83DB04C731ED0F8207792A77186DB220CEEA5577B0C0C442227A91DB935382A200BBFA5832896FAD5F1AAC894369C8873FDF2BD078190C32F0C0E51D473A29ACD43C8D66BBDCBD25B07096A42D7B312A4578CC5FA2C90147959E990C7FD25852EB79CCFB6C04F0068037A1283CC903C37C6F997A2F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1D4CF0C7315D18492EA06BB4B1D5C91B1B811B7B0FC0FB003E80E5F10F0938065BD48244301404334DCF7EED0BAC3B4ACDA117F24C3F9F06F2D9FA6027D83D158CA34057AE9A426584E29EFA76299F3AF931C5276132128D91C928A94E8EC566CB910639A49ECC00CF98C46BD4D529312816A70132F55E8D90D190A99A300B9C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4ABC338D8912373A3F5873D22C255247F763D5526F6A4E9A1D8C44515257CDFDD214CD054248DF2889128DB22CB11BEAFB6C79999A6B4E6565800A7AA317860428A32774D1CA6969746F9A96C60AD2B46C58BA092642B0EA41FF072E156832805D0B10F3EE8AEECB836125027AD21925CE690893B95D4D7794342B5A47665689&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A5E1604D175C9CCC02E7076970612158D1F4682C2C4334B9B6221A0F8202A86229C0FBDD485667DA77A886E1D976979069F0540512162A30DA90127A5FE36A92A65BF31A424A2C9FE919999929C03F75193B12A78B29718D3A7537A3AA4A1D216184AEBADDB55B8BFE16A264AAC4055A02D3E47CED5E55E75CED0ED8DE49BB36&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;149BE1AADACD5AB4A87857D600D6F36EAC1593B2B354B3C6F760B816B5D7D32DBAAFA75B0CA097966EB702966337059663B787DE566A0DA3B067E996D10DD767D8164B35D9484F5C0D36149C61B216260FD56498A875BBE98A65CF8CA7973551D7CA1ED61CCD8CFA58D3912DF5B4D5A61954672D035915D3178B69B15E9AECAB&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8741B2C9C8BAEA324596D6454BC4E6C62AB0DA1CB6076A789D4D0642ADAF1B4A324C10B307DBA1FE46265EDE44EF8E1829A96366177103D0C8AE3DDC50B26EE574A35F2A3BDB5259C84A75D9720877241F658E227D487B06CB27FF5FABAB29773365C4DF7E0F53D084FE7C6D627EC3B66E6E46D1ED8C7F2894D5A700D6503116&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F230B6FDBE9676357F63ABD0E006724170DA05B0237738B746930B0A32333BC0863B07FB24F3D682FF643BB11EA9515D54690AF97CD3A70E2D096BA97B33F7F700C1FE39E2F5402F9D36E203B2E5B41FBA6FC3F55FAEFF72FD97EBBFFE4BFD97BE71791877D3B3C59709363D0F256593B8387E2992FACC91FE4D1095D80CE063&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;009FA879F5933DA69727338EF46F81EC4F017C06E073005F00D80A601B802F017C05119DAF61DD862E59171D100E1FE8DF0EBDEC667D83D26E3AFC7748FC3B00EC04F02D805D007633E20B48B5A92AA960F7F77700BE87CAF2ACDBC72D1A45C3502BA42669A5BAFEAE2E92E7FDC338B18A5490476350CF1CA9BE7F4598FC23D4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DF0CA005402B43F611C4612CD2E58517B48C10E82F251B3FC8860A44FA61EFBE7DDDB425E4A43C967611CC021659D3B925EDD8CB88451CF8666F6A1C906033CB056C157FDB5EC48AAD7B0D5664D0F27D2538430A407F000324112B8A207B2080410006031802602880610086033840627B7C0E2257D2BB98152321EF6000A300&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8C0650AC45A5A58D1A2BE8E2EF58C82A810A5E3558C1DEB95B25BDC263C50428530AE01000874A1A075EC01C78FE57E7C09F53E6C0037C0EDCCBE3C03D2972C07B21F7ECD239688B9B1B427543A86E08D50DA1BA21543784EA8650DD10841B827043106E08C20DA1BAFECBF55FAEFFFA1F0FA1521774BB1641A1E194DBF65AC3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;69C741B0E178002700A81246504E846C194035801A000100109FF4D702A803B094005F9080AEF4B8F2A203861635D2178808E93507FA974154E44A1C560941A930008844FAA300EA5958E5622DAC42DFCD8A41561C2A58A34750E0B7A98CE25EA027AE99A28656087D8D50065E8CF7AF0000AFDB50B69C8DD972D6AFC3163FB0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;45E54043CA1C88F23910E271E0A4143920C18B7217B2DD8AB598158A2DC6760394BA11C05A00370959B10EB26F06700B805B01AC07701B80DB01DC01E04E3DC67637F46581C60A1A46BD17F2EE03703F8007003CC85831576305FDD8CF4390F5305430CBCE8A55D2D13C563C0A651E03F0388027248D03D33107A6FDEA1C189F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;320746F3393092C781112972C07B31F77DCD3974A5C20D30BA014637C0E80618DD00A31B6074038CEE03BAFB80EE3EA0BB0FE86E80D1F55FAEFF72FDD7FF7A80919EBC384C0B9E507F34D4163CD9047186CD003E06F0893078B205B23F05F01980CF017C01602B806D00BE04F0951E3CD90EB18F6E3878B203F27602F816C02E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;00BB59F0A4400B9ED0CF447F0759DF430579A62D5A9BFEAEEE58346DD17A861E32E0FF11CA34036801D02A691CF0610E787F750EFCB027550EECDAC3E5C08E3D1C0E7CB327350E782FE1EAEA3C995805A8CE6AF5B492DAC52425D81087CB25EA251CF0A43E1F942B09B96629F12E0935E031154E468AC69AD4DC6EE441B9AC21&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1135A7E6B1AFAD837373C3536E78EABF2F3CD51DBF096F84A9FA5B922DFADC93934D95BB872583697A374BBAAAF65D2CA920A65E9634C320AC75A8D661EDA9C554FA5A3B84EDA69FB5572623B2E69A2D6A80B5ACC5BC6CF9165BB3E55B0C6FA025DF6E85560CBB495A31ECF63948C43DC358C528BAE5F6E3B5A3B761CB35D974&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;11AF7A6CE03C0493B55BF542357DAB8AEA7EA02F4F7799531828CAD4AD6E80084373178345F9C87714095BD11C496F0B02F22A567DB7B8182B75267F33C0CE4A93F3E96BD339E489FA5832B15BB2566CF5513C7F411D566FAE23A1DEAB1F27CB70653C31B8617737ECEE86DD7F636177783EC9FA2582EFA9635A0F624D1A334A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E503006ED8CC0D9BB96133376CE686FD5DFFE5FA2FD77FB9FECBF55FAEFF72FD97EBBF5CFFB51F47CBA8BF7593FDBDE9890D7D3CEAD1B9D2177BD081139FEFB1AEE5F5F7C03113008A000CF488D6F20641F6600043000C05300CC0700007003810C0411EB69637925C496FEF41EF058C82BCD1008A018C01305623407A6D0F3A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7D641C648D870A5EDE83B7826BB82F7112FD87409943011C06E070C681E730079EFDD539F070CA1CB88FCF81BB791CB86B7F38B01E73E0D65F9D0397A7CC810BF91C389FC781F352E5C0A55C23AE8886D4F5DCB385EBB9F085A0294A5D30423FD5039F04A2179509A5DEB458CB5BF72D0857A9D61A57DD61175600B9487791D7&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5DE4751779DD455E7791D75DE4751779DD455E7791D75DE4751779D58FEA49FFFFCBBCA9C4F472530F84B8814637D0E8061ADD40A3BB50E2FA2FD77FB9FECBF55FAEFF72FD97EBBF5CFFE5FAAFFD5FE8A52B7465FE533CEAAAAFEF8FE4227B6240A98B29CA11FE533DF0BD5BF8FA2A5EF95B655BF93B1D16C9CE00702680D5C2&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;95BFB320FB6C00E7003817C07900CE0770018035002ED457FE2E8685BB7ABCF27729E45D06E072005700B892ADFC2DC32B7F5743D63550411D6FE5AF96B7F2773D94B901C08D00D6320EC8980327FEEA1CA8489903BFE373E0281E07A6EF0F07CA300726FFEA1C284E990307F139309CC781612972C07B05C9E865B3B1594A6D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;02427E9DE13B36E12A70EEBC5556FEF2AA75C54D5F94821A47A8B5F5B0A4D996B5B474B59D5E9654CE129696A3F6A0AF25D5D49D424A27EA4B579CC03AD21927AABDE88E938C2E9830D5F67BE12453E37A7C4D87F631502F0C9D536755E99C32A5385CB8FDDABF7E41C3DE817B785F77327590FFF525F29B3CD9BF05CCF25300&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9F8165F723B5418D42845E0E08DE2BB9D63D45AE39C9B56ED7BADD7EED97754BDD3473937473DB0196B613C0B7608F9D9C10F21D10BC57F10FD708D62D75C763D762DD7EEDA7C5FA357BF3E8F6D602A6D60AA00D0CD2EB8420392078AFE65AECF45834E25AAC6BB16EBFF6D362F7FCA4DA5BA66E6F59E4D2EF05E023406A7542&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F8C101C17B0DF7FC737A393D148DC6F2D261B2BF191B71B5F03FD0C27F693A94A5EB5021A84F27009D41C9763A2078AF159F920624BA3AF63FAF635F6B2AE4D555A827684F2F00BD41C7B63A2078AF13BFB9E1EA98AB63A4E14F3515F2E92A5404DA3310C020D0B1CD0E0885D793D442081E0F2777F905D25A72FD0750AC0340&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;03CB6241394410CAA4F74945E35814FD3D7253B24F1D82A59B0958A27EA55CFBEBAAB505A173E95D827B1E8B6A8F84760F06300A7A770B6B6CB4B9B13770631B7163B7256FEC75DC5809B4330EC07868EC76D6D80473632FE1C636E0C6EE4ADED88BB8B1C3A09DC3014C84C6EE86B7AAA0B149999605712DFE5EA74402E57242&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9E9950C2F45340DD4110F768D2C82F94FE44FE3B147AF177D2CC3A3DD4AF2E48484348C65F18C633228C8DE47605603C4D3036500CB6A091EA1FFEC1FDEE32FF1420F02952E30B8C6B7F2537CFB39B27C9CD738C2DB00E223D415236A3C58AF51952468EF4B8257530A4E6E4488F91F46758656F300A1ED529307E2F6D987184&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;FD3E050A8E060A1EC1143C8C2978C846C1835C0A1E1050703FA6E04D0256D2D71B49EA472CF55E7CC31675EEB1D4E7A3ADDC6D49F5D2D4BB2CA97E9A7AA725B5034DBD835BEFEDBCDE435F2049FDBA9823C67B4C3CEBD3A960C753DF80C5733316CF3A9B78D672C573A3403C3760F1BCCF28B83E9D0A560B145C8B29B8065370&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B58D822BB9145C21A0E0724CC1074CC12EC33A75294FC12EE12AC2C55C05BB88AB601772156C0DB7DE0B1CD5E77C478C4D041C0BAD2FCFB4ADFF674C863F493A97D4721F3BAEF91C72732F63ED4A5B21752D17FF499B99069C65D2615C8277EFA001A780069C8935E00CAC01A7DB34E034AE069C2AD0803F620DF89800786F40&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3AC5899FDE4F52DDE1E33526971DC255F139725829D433475405C9B0152F4009F1E04AA533BA6F5462B075275322150DA86C8A13FC510B2264EE52A9C0C81B5C49B79CCD8886C804313B1B36D0F8CF222CF39D4D4067F5B59722D80952A4BE15E13F078952DAC2085EE1A8409F32D4E58EA89F31D44647D4CF090056FA2F803E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AF417DA6AFDA14D19775FC17E23E7F612A72312E022FE168452EC145B6B20E452D1DFA9DAD43DB186A4443DDBC79B386EEB1A07EC93A7239692BC77F45A6960E7F5F99F3AEC2BDD9CE4CF16A483D8626EEFEBBD914976AA6480BD469A6480F64B82E0553F4EE4871435807D81016AE8ACC690857C3CB5F3091AA88062389F814&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;25B15C5122EA7B98C93054B52A228F38E5C1787D486EE2D791AF9D55AEA2772235EA2F634D8D3644123DF4CE8D3061F6C1E996321AFDD9D992942DD115629DFF92EEAEBD3B53DAF4D687609107BDEA0571656E2C58178CF8C35501F58A743DA0BE58046FEC1768AF87412F60EF5DA17A30BB9E0FB4D1FE1B2F9BE5B01479458E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;910978F3081981F9515A97BCC29A128CE4E94D936C74138C140037E4153A2F9CF85BA4134CB7DC8DB075B29700415ED14B589453A99922078460A48F0DC1A0579C178CF4B7E599B8D197DF654D573965D3A18BEC6D30EB4FF2F9EC7BFE4C3A84365EA6BE4F177E3E1F517C74A51B01FAED3ED2AB1007925A17EC9B2EE876AB1B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;32B506ADB4E5B0F61C72A9257E9BF2C6D2DE04936368011D658A1C57CC76091F4398B6A23E1A51220943AB5D63FC5F3446812192DF81C9ADC0B2B7D9E793C466E6AD2156957A7D7CCB527F432633534D8395ED4A6D533698583E702E1A492C8D6B67D118E6641BD6ACE6E85AD6FFE630271AE5B2F3521EBA8625371AFCDA80C3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;28F74CF0A097607C5A0B914899D8638A55F38DF1E7DBDEEE94DF74C854CD6F6A4328D1105366466AA3D9E12A351B8E7C523540BDCFCC928AB407BFA342D16AFD996F142A3CD0B151603C7D3A5C07BCAA8270B66319339B3ED482E5F431DBFBDDFEBC7DE1B73F04F79EA754072301B93AA4184FBB2A4A1F7E162DD6CDAC4E5A89&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;EEB6D49FF1A00DFF5278D8FEB9F9D954EFFCB7D207440CD767AA5F5FA79FD0FA5E0B96AB1BE175201D4304F8287B923D299F08E4C7E4AF9E8008FA107B260F383126A0CA04C1AB50623564CED4CB92372D12D072728977981921EC492801E29C2B64F24C5407D6446E22E44EA19E816005E0A692B0B51328762C46CA420AA076&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;21A828696624A0ACE8423D9D9E464F4853C9CEA20F75F6F7EC282146FB947F9CB93545B37440C565BB8803F483644B080F1D1BE16F43F69ABCC090C9DEC5DC2936AF27C27DCDA69FB739E557855CE3121AD75DD4A030BC5B0B9FD0205ECBFEBC64E5B259C8E67B286B31BC17B3B935D5D7D35C160B597C1F652B86F723164B7D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;252DA6F88038BC5FA1C5146981B938BCFF502AE1FD7E9216DE9F9DCEF0FE231059381A87F77F87C3FB336DE1FDA3B8E1FDE982F0FE341CDEEF2F69B1DD72C7B8F400498BDF422CDD0FB174FFD338865BC4AA9AEC58D540867AA423EA20867A8423EA600945C1FD1038F73F8F3B38C4940F5172FF069C3F943555EA18121FC650&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;275850C7D950874BA6B0F7ABB8C503CC79AFE3BC8398FA6E84D48E7432BDBBCCACBEC538243E1A87C4DF4A457D47924A77F043C4D2C1E4BF9DBCC89974306907A267340AF64E3AA260D228F21F44C2A411A4DE9A9FF4771CD21C07904693FF76591F94DE03120E248DCA5AC33FBF9D62F21F3CF7F83F80BA87937AE1E1427F50&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;90C692FCEFF833DD4D78A65B22F167BA83F04C379C0F7A48307F443339626304076673BCA99954443216FF645E90D635AF94D4D5CC9F2A7C861DEC2104B6F0C7BA2F30DEA104B6F21DF6368C771CD3F82FC50EBB27D6F81E58E3B7A7A2F1C73387DD2D9D0E7B0748B90B76D89DB1C3EE6473D8055C87DD51E0B0F3B1C33E8179&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9E3C477F586577D8CDD8C39CC8AACA76AC4A66A81D1C51AB19AACF11B5C6EEB0F7E00E06EC0E3B03855A248535B5AFDDC961D732D4BDED4E6B987566A70CC11DDD412E35E7F9706F9631F5ED600E07D1754C4D7D5BDA91FA36B7A3F9464E560AEA1BB2386C0CC24287FD433B72D87959D4CF277570198E0E2EC21CF6F7EDBFA8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C38EF21C764720E15FEDE974D8F5CC611742DDBBDAAD0E3B2676D85DB390C38E0B1CF6F676E4B0A3E0B01B6D0EFBEBF6240EFBABF6240E7BB9D861F7CE420E7685D861F7C5784D6287DD1FE3DDC0347E4096D0616FC11AFF493B72D88352D1F81B99C3DEDC9E46873D04A4FCCF76E4B03F6A470EFBC376ABC37EBF9DE7B0DF6B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E73BEC77DB91C35ECB3CCF3BED4EFEF026BBC31E8D3DCC3A56D51B8E55DDCC50373AA2DEC2505F7744BDD5EEB0C7E30EAEB73BEC4370FE6DACA9171D1DF6ED0CF58576A719F61D66A73C11B778A739EF089C773753DF23C50EFB19ACBE4F63873D2515F5BD37C90CFB3EA1C37E0A3BECF2AC7438B8FB99C37EF29775D80FF01C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F67420E189B43AEC0799C39E01753F6673D80F891DF62CECB01F1638EC87B0C33E191CF6A33687FDA7640EFBC1640EFB31B1C39E871DECE362873D1FE33D2176D80B31DE26A6F18BC40EFBCE761412B943D378AA89C7A4A2F19B99C3BE2D9D0EFB3890F2ADD861DF821DF6CD36877D13D761AF1538EC1BB1C3FE98799E1B1CFD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E12776875D873DCC1656D5358E557DCA50AF7644FD8CA15EE588FAB9DD61877107BFB03BEC7A9CBF953575B1A3C3DEC6502F7274D85F9A9D32DE9B207D65CE6BC479DB99FA2E3752D7594222E762F53D07ABEFCA54D477471287BD53E8B0CFC20EFB94B438EC6F99C33EF39775D8BB780EFB5420E1F4B43AECDDCC61AF82BA4F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B339ECEFC40E7BB5BAFCA57E6FE97B81C35E891D761C1CF68F3687DD94CC61AF48E6B09BC50EFB7CEC605BC40E7B0DC66B153BEC8B301E7C704C7730BE4B48565FFA1A68D15439A1D445634D45EAAA38DBC97C293617F84E193597CBC4DE3E8CE737213CBFB9321573E9CF9AB84ADC44B01D6DA15F8A9BB8369526E0E3597440&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A94DE780723D6861000F28357840A9B60D28277207942AC18072021E50E0CB5FD4331EEFE8AF077A6C03CAED58A48358554B1CAB1ACC50173BA20E61A88B1C51877A6C03CABDB883C33CB601E5019C3F9C3555E138A01CC050E73A0E28077A4C83C6C3B8C583CC797FC6792399FA3E9A25DC763E135BC80CFC00FB442AEA0BDF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;64130D28F0A936FE80321D0F284FA66540814FC2D101A5FC971D50E0AB73B601E52920614A5A0714F8B01D1D509E86BA27DB0614F8C49D604079163F01C057F07803CAE17840698001053E80671E500E4B36A01C9A6C4081EFE809069497F000009FDA130C28AF603CF81A9F6040790DE39DC234FE75B1C31E8335BE183BEC37&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;53D1F8539960DE02C11C6C13CCE9AC0BEF8ABB70101E330EC45DF820952E9CC1C68CE1E91C333E027A86E23163081E3306DBC68C81DC31A34830660CC063C699CCF9F57774C9ABED63C656ECE4CE6255F576ACEA6C86DACB11F51C86DAD311F55CFB98F10DEEE079F631E35B9C7F3E6BAA93E3987101432D741C33D698C785EF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;708B179AF3FE8DF32E66EAFB8378CCC8C116E4C763464B2AEA7B699231E332E198D1018F196D6919332E676386F7971D33AEE08D193F010999691D33AE64AE692F658FCD355D2D1E333C5E34665C2318337E6A4363C6721833AEB78D19ED6D49C68CB6B62463C60DE23123DB8B7CFC8DE2312307E3AD158F1979186F0BF3A6DF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B7A5FA22B64DC1EDDEB4A3175671DA9037DDDD86BCE9AE36AB37DDD9C6F3A63BDAF8DEF49B36E44D3F656E617B9B935BF84C20DAAFB1685781687730B67C954EB6F402B66CC36CD98AD9F2858D2D9F71D9F2A9802D5B305B7632B67CE2C8966F056CF918B3E50C604B0B63CBE674B26508B0E59F982D1F61B67C6863CBFB5CB6&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BC2760CBBB982DAD8C2DEF38B2A54DC096B7315B56035BE01C31CA96B7D2C996D1C09637305B3662B6BC6E63CBAB5CB6BC2260CBCB982D70081A65CB4B8E6C81A3D2786CD980D97236B0A590B1E5C574B2E53060CBF3982DCF61B63C6B63CBDFB96C7946C096A7315B3A31B6FCCD912D9D056C790AB3E55C604B4FC696BFA693&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2DD3802D7FC16C7902B3E5711B5B1EE5B2E5CF02B63C82D9D28BB1E56147B6F416B0E521CC96F3812D458C2D7F4A275B2A802D0F60B6DC8FD9729F8D2DF770D972B7802D7761B60C646CB993C7168F69E7A3802D7760B6AC81D71CE078A522C10950754B1395D186588DD20BB6BAC46B3C35B227247B4E963D71C523177BE431&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;1E79AC472EF1048A3D81319EC0584FA0C4132FF6C4C778E2633DF192CCFA7831F91B43FEC692BF92CC40AC98FC8D217F63C95F89F0B5C25CD14FDDE1336EAA551EA7AF5A7504FB4B4D961BA664647427F55C35D59E0FD11DA62DB7B7A5F1009963415BD6636DB9156BCB2D366D59C7D5969B04DAB2166BCBC18C821BDBD27880&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;4C0028B81E53701DA6E05A1B05577329B84A40C1959802388A8B1E2073451B3A33E6F236CE013297B5F10E7AB9B48D7780CC256DBC03642E6EE31D207311B7DE0BDB9C1E5BD738629430F15C904E056B04F19C87C5732E16CF3936F19CC515CF6A8178CEC4E219C72838239D0A761A50B00A53701AA6E0541B05A77029F88380&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;82959882F14CC19AB04EADE029D872AE22347215AC81AB6009AE82C5B9F5C61CD5E764478CC39878EAD3A96097827822583C612C9E904D3CCBB8E2090AC4B3148BE77046415D3A15EC3AA040C11404300535360A642E05270A28A8C2144C640A7602D6A9E3790A761C57118EE52AD8315C055BC255B0C5DC7A1739AACF42470C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;386E8F1EF3B7A04D70CCDFD10C63BE08E378865129C2A86518F344187002173D3FF101AFE9FCC48A36747EE2DC36747E221CBF95E4FCC4396DE8FCC48749B6FF11007F06C5398575E777A2EE4024959EB0F818C1CFD7DE4682770C23747B87EF71923C271CAF89C642C1EA91450BD5F79F268D19553C6A7C7171F1A8E29145DA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8BC893224A432226874616553454878235472B4DF3A327299149D5A5A5F2F89AF113C61C5A324E293EE4D04238844A3B9EF102D6FC13A8F9F228A940A1C73942F38517EAF8859718452F67459FF43A1E2F6A1C0D79855681FF2968B0A0F02AA3C2AB9964FE66964C1996CC642C99EB924BE6482C997F80509E05F01C4806CE54&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;31CEB17AC10BD33B757D7B662CA6D43584E458113D79C1FFA2178547D7B2621B48AAEF25540C5E6C2FAAAC97235AB19751B12C78EBDBF12DF21CF5FDBB881C86837003DAA99A9DC2550D7105DE0D9EDBA8C462C180E2AFD1CA94E785AB82F1798A1C981B0935E5D644C3F5B2F6367B425991808B0E918630FC9F17D0DEBDA7B9&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;70BA00CCDAA93BC9841FBCD82FE8A0516B3F2EC27CADA9A1DC5CF5E42AF58576C01ACEC56227031878C3B87847C5602B42508E4CD528F0BD02BAA2440E2E5B308246C0E13461FFAB20E6D700BC0E60238037401EB7329D7D93ABB3A6F7E70D9D5DCF74F66DA8E81D00EF82F6E64B77B1FADEF3F20E6B37DE17362ABBDB50F87B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;58E90FBCA28F4CEA6FC21A15DC6B54701FABE023AFE8ED73BA5BC3287CBF51F801666E9BCCE636189BDB206C6E0F2537B781D8DCB600973E05F01930FE11E608FB091C61E1D3869379DEB8DC605CBE6A5CBE6E50B19151F185998A3E988ADE988AB79253D10B53F11510F03580ED40C53BD869F8C169F87762EFF01EF60E7EF0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0EFEDD381FDED10257E007ADE5EAEBBF01FCC0F4B57013D3BC66486A01D04A35AFF033831B5F1897DB0CC67CC918D366668C1733260B33667B72C66462C6EC85AEC091DF7EF83AA1B48389775FAB40BCCD4627F71897B0C6A15D6669977E8F8F0E0C3E3D4B82B75C2829993E13293FB52252DA5B1129F08A4B1252DA5A11291D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;48A5FE6C007E2005DE5AB1C838CF8764D831CB26E3029C0FAF752497711768ABAB8FC9B82B23BC3B24F500D0D34765DCDBE04E5FE3B2BFC198018C31BDCC8CD98619B31533665072C67C8119D30FBAD21FC0002010DE64A032DE2292F168A393E38DCB438CCB89C6E511061547322A069AA9F81853B11953312539159B301543&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;81806100860315E576F11E84C537DD2EDE8371FE0C47F18E81B6C6EAE29DC5C43B0E92C60398A08A779EC18DF9C6E54283318B18634ACD8CD98019F32266CC31C919F30266CCE1D0958900260181C731F1FE4324DE3AA39361E3B2DEB84C18978D0615CB1915479AA9780653F134A66265722AFE86A9980A0494039806549C62&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;17EF0C2CBE53EDE23D1AE7AF7214EF5C68AB4217EF6A26DE79905409603E800520E3BCC2F30D96AC312E2F322E2F3518751963D44233A3EEC28CBA1333EACAE48CBA0333EA18E8D5B1008E0382AF628D1D6F6EEC16DCD8CDB8B16B9337B60E3726433BD5006AA0B1EB996EDD20D2ADDB0D8EDC6B5C3E605C3E6C5CFED960D9A3&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;8C0AC54CC575988A6B31154F24A7E21A4C4510085806E024A0E249BB6E45B0EE3C65D7AD9371FED38EBAD5006D35EABAF52CD3AD1590D40460A5EA3A5E32B8F18A71F99AC198D71963FE6066CC6ACC98333163DE4CCE983330634E83AEAC02703A10F89623616701EED98C30E95DD6BB73CDBD5B8E7BD7887BF741F2DE35E0DE&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AD81762E047011F4EE23A67C278B946FABC1C26F8CCB6F8DCBEF8CCB7F1B3CFE81517189998A28A62282A968494E451853710510702580AB808A36BBF25D8B95EB27BBF2DD80F3F73ACA681DB475B3AE7CB00B852ADFAD90B41EC06DAAF2657B756EE4189779FAA504FB2D28C78F11705CEAC53096883086308CC5228CD10C63&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;9108E33086B15084318D612C106154308CF9228C631946A50823C030E689301A19C6EF4518A7318C0A11C6A50C63AE08E33A8631478401E12BF59499D63446811F01453CBA159F32D38A4F9969B59D32D3CA3D65A65570CA4C2B8AA13EC228286F4D6314F829A0600AA6A00C5330D946C1115C0A26092898882980C0218D021F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;DE8A02BF87B572A2C087B6F2A2B587B4F2A2C0A5ADBC28F084565E14783CB7DE71AD8EAB548E1810F5635FBD93FEC16435369DDAB61164558C65351ACB6A944D5623B9B21A2190D5415856CF320A0E4CA7B67D00140CC7140CC3140CB55130984BC1200105033105CF316D2BC20A3680A76DFDB95AD18FAB6D7DB9DAD687AB6D&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;BDB9F5F672D4A59E4E18852F6A6352762EF036BF4082C19106CCBE262C2ED0428C107884C0AD11F07E9995CB635B44F30BB260A4740C91D20D9ECB839192B1B3A6CE2CEFA0456AE14045186EE12F0B06DBE49154308EDC7055A45C1802CE0F57A98DC891BA99E5D053295B1FD4B320E8995A347600702942138F8A451BEAE1F8&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;C878E71A7A64624D9391D4A95E3D8CD248C9ABA761D246A5325817C98B287532BBE9AED657AED404C372A85221AC9113D15857DC0C4BEC6E6E8A25F764C9D64A3AB28CCAA6707534D4598EC483534D49FE881C51AFBAB31E124283F478373599F5D59CDC43A3D0DA623713E52C355F4B55CB762477B383A190A2DEA2E07D1713&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2FCA8375C144BC9B853835B5074BADD07A5D2127124A2CA2A7CFD1BAADA577536BB6A476D7FA65A984255BB0BB9A69563B9263AC28D8F5AE5B23D1A500D19F0A391657CAE2AA3EF5B0A4328974E8D04132FF4CDE2E1BFD20D3FF0D4C327700802BDFB7E00647F876C17F07FB76C37FA37CFF82FF46FABE53FFFB5E4DFC378D4F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;E5F87E20FF67CE91E7F87E2417D94CC0BE66A8F26076EB879AFC508FAF0DCA0DF5B543B1CF563D3822433D62979D0A2A5936BF7BB4CDDF59B0A090F24206788424A6DC05D670CA955A99B80AB6AAE167AB42E585F0490C964C1772E470B9122766066A98578F6EF2618947D7D082503452073DD1645D185F1A8D2550428F3A25&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;A2C488A65BD2BB6BE9B3CCE5F313E025F5DAC3706E79B9DCC434490ED92A8A772189965AE2FAD253C7DA602C9E2035CCAD5DA4282715B274B881735BBBD43684428C9B5A694A13A49968C2094D8A1CA347AA6B095DE4EAEA98D218242801D2161C3719CF0E6817DD50262D445373C2FAA54E156A42A70AA7118B272CA365E0E4&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;765AED342DA1879E10A90B05E34B597A2EF512CAA2682C10EF14AD07ED91434CD271D3CA5E6722126ACE462F3B8714B97E09A356ADB096EADCF4905C1757D5D52379B45FA6E98EFC3A90FB4CFAE5961417DAC0460F4841E569F37428521F3073727C3F81958DCED17EF4E9729FF623B97BC11A03A367CF1EDD447E39C6CF4FBF&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;F69893939541C6F9949B369FEF0B6D6441832952A97D2E2AD1540FA728ABAE374124464FE52E482C8F521F49F92EAFE8CD4A8DB0A1F6D4B32C65E88EC411A9750616CEE3E01CB3F6B5ECDBB73F85CC4C50A744E69FF76DE70550E82BD1C3CAA6480D24CF0C641395A5E4750C575543F2D25834422603E0CB7D127847D3C17812&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;AC94423BA65396A577B554D3EB49D2163693DEDB92C66781CC0E101A694133E9F61634936E6BB1CEA45B5A7833E9E616FE4CFAC716FCE607A3E08796343E0BE40105DF630ABEC314FCCB46C12E2E05DF0A28D88929F88C3D0BEC6841D3FF6F5A38CF02DB5B7873F6AF5B78CF025FB5F09E05BE6CE13D0B6CE3D6BBB5C5E959E0&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0B478CAF98783E4FA782F507F17C8AC5B3058BE7139B783673C5B349209E7F62F17CCD28F8289D0A361C28F80053F03EA6E03D1B05EF7029785B40C15B9882ED4CC1DEC43AF5064FC1367215E175AE82BDC655B057B90AF60AB7DE971DD5E725278CC29D9687CDC2DD9CA7480916F8E9A614F364D70F935D3F4C758DE9A91F26&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B97E98E2FA61829BE387E9AD1FA6B6FE663322CC63FD308B4D75062BC116838D2C12CC466A3F8CD48271DABF17E5A1D1597A9830064668F849CD9A6F378F032DDC71A0953B0EEC654AFE503ACD742E28F98358C91FC04A7EBF4DC9EFE52AF93D0225BF1B2BF93E46C15DE934D3C540C11D9882DB3105B7D928B8954BC12D020A&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6EC614C0960E6AA6EBB065DEC433D3B55C73BA916BA63770CDF47AAE995EC7ADF75A4733BDC61103B697E81148D8FB416575753AB56D19C8EA4A2CAB2BB0AC2EB7C9EA52AEAC2E11C8EA622CAB6C46C145E9D4B63850B006537001A6E07C1B05E77229384740C1D998023FD3B6B3B082ADE669DB995CAD3883AB6DA773B56D15&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;57DB4EE3D67BAAA32EFDD1715080AD43A641A1C0C719146047D06F6050803D49E91914EAF0A000FB9ADEF65A5F99EAA1A59A6242524F1F6F50E8C794BC369D667A3D2879002B790D56F26A9B929FC855F22A81929F8095BC3FA3E0F8749AE97AA0E0584CC131988225360A1671295828A06001A6600033D3F9D8322B79663A8F&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6B4EBFE79A6905D74CE772CD740EB7DED98E663ACB11632813CFD1E954B047413C33B1786660F11C6513CF34AE78CA05E2998AC5338C5130259D0AF634503019537024A6E0081B0513B9141C2EA0E0304CC170A66087629D3A84A760A55C4598C055B0F15C051BC755B0126EBD631DD5678CE3387090751C3898370E8CF96D8C&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;0363D3360EF4C1E3C038EE38309E3B0E4CE08E03873325EF9D4E33FD1294BC2756F21E58C9BBDB94BC2B57C9BB0894BC3356F2898C824EE934D3DD404101A6A023A620DF46412E97821C01057E4CC12466A6D9D8323BF0CCD4C735272FD74CB3B8669AC935530FB75EC9D14C331C31A632F1EC6B4EA38279B2498D7B9A91787E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;6A46E2696FB68AA7B599279E9666BE789A9B9178CA19053F36A751C17281827F630ABEC7147C67A3603797825D020ABEC5144C630AB6B319E9D48E668E827DD3CC5384EDCD3C05FBBA99A7605F35F314EC4B6EBDDB9A9DD467AB1346E10CEB3870346F1C98FBDB18072AD2360E6C6C46E3C03CEE3850C91D07E6FB7801A5053E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;5E40E918A6FAAFA7D3784B40F55FC5AAFF0A56FD976DAABF81ABFA2F0A54FF05ACFAC7320A9E4FA7F14E040A9EC514FC0353F0771B054F7329F89B8082A73005C731E3FD2BB6D72779C6FB17AE913DC135DEC7B9C6FB18D7781FE5D6FB6747E37DC4114366E279389D0A3607C4F3272C9E07B1781EB089E73EAE78EE1588E71E&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;2C9E6A46C1DDE954B04540C19D98823B3005B7DB2858CFA5E0560105B7600A6A9882DD8C756A1D4FC16EE22AC25AAE82DDC855B01BB80A763DB7DEEB1CD5E75A478C2013CF35E954B02088E72A2C9E2BB178AEB089E732AE782E1588E7122C9E658C828BD3A96031A0E0424CC11A4CC105360ACEE35270AE8082733005273105&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;3B1BEBD4593C055BCD558433B90A760657C14EE72AD82A6EBDA739AACFA98ED38F8875FA71326FFAD1F0DB987E34A66DFAB1144F3F5670A7144DDC29C54AEE63E8694CC9EBD269A6D781922B58C90358C96B6C4A2E7395FC4481925761255FC52838219D667A2B50701CA6E0584CC131360A1673295824A06021A6E07466A60B&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B065CEE7996925D79CE671CDF4F75C33ADE09AE95C6EBD731CCD74B623C659BF0D033C3B6D06588A0D700D53BE09E9349F5740F9C661E52BC1CA37D6A67CC55CE51B2D50BE5158F92E64141C9C4EF379072818812938085370A08D82E15C0A860928188A29B88899CF106C318379E63388ABE603B9E653C4359F015CF3E9CFAD&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B79FA371F475C4B88289A74F3A156C1B88A717164F4F2C9E1E36F174E38AA7AB403C5DB078AE6414744EA782ED020A0A31050598828E360AF2B814E40A28C8C1145CC514CC8F752A9BA7601DB88AE0E32A9897AB60595C05CBE4D6EB71541FC9711A75AD751A75036F1AB5EEB7E1C56F4E9B17FFE647E4C56FE54EA3D673A751&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;B771A751F05A207D1D71FB8F82D7119F62185F8B3036328CAF44181F308C2F051805DF68EFC9660329053B4C7740BA87BE5FA3B154822DA5B4BE6DA216F318C65611467F86F185086338C3F85C843197617C26C258CC303E15612C63185B44187186F18908E37A86F1B108633DC3D82CC27894616C12613CCD30FE29C2F89261&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;7C24C2D8CD303E146140A09E627C20C2C86518EF8B304A18C67B228C890CE35D11C61C86F18E086311C3785B841164186F8930620CE34D11C6750CE30D11C6AD0C63A308E31586F1BA08E31D86F19A08631BC3785584B18B61BC22C0C8CD60DBE09993CC03735EC23BCBD83866AF6454F1A831299EB2271F52525B5B32E190EA&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;D2F1134A6A2678A19D09DCA3926746124A4CAE49041B8389A6B9F5E41AB6DBC7A7464321A5062E73D5D710602F7F8770551C5EEF38E8A8598EC546A84546A4864AABED3D4FA90E925E558794B2584C6E9A158C27B46AFAF0B368B16E8010B096E86E4BA5C885464A5530A184E30528211E5CA97446F78D2AE73325F51F193406&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
								<Xceed.Chart.Server.StringHolder Data="&lt;State&gt;682F3E2CA02FEB2970CC82F6F6C38C6828A0C47EE9FCEC6CFA52270C07391C88C7343879DFB1BFF4D31EF03E989F6200CFF2CAE271255C1DA2EF2549B0A90F96474660E5396A167D4143245A7A56240CD38BEC85D2A2CE70AC9576BE43EEFF01&lt;/State&gt;"></Xceed.Chart.Server.StringHolder>
							</xceedchart:chartservercontrol>
							<asp:Button id="btnSave" style="Z-INDEX: 120; LEFT: 176px; POSITION: absolute; TOP: 1496px"
								runat="server" Text="Save changes" Width="120px" Height="32px" tabIndex="9"></asp:Button>
							<asp:button id="btnUpdateGraph" style="Z-INDEX: 118; LEFT: 328px; POSITION: absolute; TOP: 1496px"
								runat="server" Text="Update chart" Font-Names="Arial" Width="120px" Height="32px" tabIndex="10"></asp:button>
							<asp:Button id="btnCancel" style="Z-INDEX: 121; LEFT: 480px; POSITION: absolute; TOP: 1496px"
								runat="server" Text="Cancel changes" Width="120px" Height="32px" tabIndex="11"></asp:Button>
							<asp:Panel id="pnlBottomBorder" style="Z-INDEX: 122; LEFT: 0px; POSITION: absolute; TOP: 1560px"
								runat="server" Width="800px" Height="16px" BackColor="MediumBlue"></asp:Panel>
							<asp:Button id="btnSaveTwo" style="Z-INDEX: 123; LEFT: 248px; POSITION: absolute; TOP: 264px"
								tabIndex="9" runat="server" Height="32px" Width="120px" Text="Save changes"></asp:Button>
							<asp:Button id="btnCancelTwo" style="Z-INDEX: 124; LEFT: 424px; POSITION: absolute; TOP: 264px"
								tabIndex="11" runat="server" Height="32px" Width="120px" Text="Cancel changes"></asp:Button>
							<asp:Image id="imgBanner" style="Z-INDEX: 125; LEFT: 80px; POSITION: absolute; TOP: 8px" runat="server"
								Height="56px" Width="120px" ImageUrl="BannerImages\defaultlogo.jpg"></asp:Image>
							<asp:ImageButton id="btnHelpSoilType" style="Z-INDEX: 126; LEFT: 688px; POSITION: absolute; TOP: 328px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpPaddockSoilPage" style="Z-INDEX: 127; LEFT: 768px; POSITION: absolute; TOP: 8px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpRootingDepth" style="Z-INDEX: 128; LEFT: 696px; POSITION: absolute; TOP: 368px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpEC" style="Z-INDEX: 129; LEFT: 360px; POSITION: absolute; TOP: 408px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpInitialConditions" style="Z-INDEX: 130; LEFT: 512px; POSITION: absolute; TOP: 448px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnWaterType" style="Z-INDEX: 131; LEFT: 496px; POSITION: absolute; TOP: 488px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpGridOne" style="Z-INDEX: 132; LEFT: 728px; POSITION: absolute; TOP: 552px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpGridTwo" style="Z-INDEX: 133; LEFT: 728px; POSITION: absolute; TOP: 752px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton>
							<asp:ImageButton id="btnHelpGraph" style="Z-INDEX: 134; LEFT: 696px; POSITION: absolute; TOP: 1224px"
								runat="server" ImageUrl="Images\help.gif"></asp:ImageButton></DIV>
					</TD>
				</TR>
			</TABLE>
		</form>
	</body>
</HTML>
