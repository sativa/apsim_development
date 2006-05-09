using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;
using System.Xml;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfGeneratePreSeasonReport.
	/// </summary>
	public class wfGeneratePreSeasonReport : System.Web.UI.Page
	{
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDateOne;
		protected System.Data.DataColumn dcSowDateOne;
		protected System.Data.DataTable dtSowDateTwo;
		protected System.Data.DataColumn dcSowDateTwo;
		protected System.Data.DataTable dtSowDateThree;
		protected System.Data.DataColumn dcSowDateThree;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.CheckBox chkFavourite;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnGenerate;
		protected System.Web.UI.WebControls.CheckBox chkTriazineThree;
		protected System.Web.UI.WebControls.Panel pnlCanolaThree;
		protected System.Web.UI.WebControls.Label lblPopulationUnitTwo;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnitTwo;
		protected System.Web.UI.WebControls.Button btnCalculateTwo;
		protected System.Web.UI.WebControls.CheckBox chkAutoCalculateTwo;
		protected System.Web.UI.WebControls.Label lblTillerTwo;
		protected System.Web.UI.WebControls.TextBox edtTillerTwo;
		protected System.Web.UI.WebControls.TextBox edtRowSpacingTwo;
		protected System.Web.UI.WebControls.Label lblRowSpacingTwo;
		protected System.Web.UI.WebControls.DropDownList cboRowConfigurationTwo;
		protected System.Web.UI.WebControls.Label lblRowConfigurationTwo;
		protected System.Web.UI.WebControls.TextBox edtPopulationTwo;
		protected System.Web.UI.WebControls.Label lblPopulationTwo;
		protected System.Web.UI.WebControls.Panel pnlSorgumTwo;
		protected System.Web.UI.WebControls.Label lblPopulationUnitOne;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnitOne;
		protected System.Web.UI.WebControls.Button btnCalculateOne;
		protected System.Web.UI.WebControls.CheckBox chkAutoCalculateOne;
		protected System.Web.UI.WebControls.Label lblTillerOne;
		protected System.Web.UI.WebControls.TextBox edtTillerOne;
		protected System.Web.UI.WebControls.TextBox edtRowSpacingOne;
		protected System.Web.UI.WebControls.Label lblRowSpacingOne;
		protected System.Web.UI.WebControls.DropDownList cboRowConfigurationOne;
		protected System.Web.UI.WebControls.Label lblRowConfigurationOne;
		protected System.Web.UI.WebControls.TextBox edtPopulationOne;
		protected System.Web.UI.WebControls.Label lblPopulationOne;
		protected System.Web.UI.WebControls.Panel pnlSorgumOne;
		protected System.Web.UI.WebControls.Label lblScenarioOne;
		protected System.Web.UI.WebControls.Label lblPopulationUnitThree;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnitThree;
		protected System.Web.UI.WebControls.Button btnCalculateThree;
		protected System.Web.UI.WebControls.CheckBox chkAutoCalculateThree;
		protected System.Web.UI.WebControls.Label lblTillerThree;
		protected System.Web.UI.WebControls.TextBox edtTillerThree;
		protected System.Web.UI.WebControls.TextBox edtRowSpacingThree;
		protected System.Web.UI.WebControls.Label lblRowSpacingThree;
		protected System.Web.UI.WebControls.DropDownList cboRowConfigurationThree;
		protected System.Web.UI.WebControls.Label lblRowConfigurationThree;
		protected System.Web.UI.WebControls.TextBox edtPopulationThree;
		protected System.Web.UI.WebControls.Label lblPopulationThree;
		protected System.Web.UI.WebControls.Panel pnlSorgumThree;
		protected System.Web.UI.WebControls.Label lblVarietyThree;
		protected System.Web.UI.WebControls.DropDownList cboVarietyThree;
		protected Janus.Web.GridEX.GridEX grdSowDateThree;
		protected System.Web.UI.WebControls.Label lblSowingDateThree;
		protected System.Web.UI.WebControls.Label lblScenarioThree;
		protected System.Web.UI.WebControls.CheckBox chkTriazineTwo;
		protected System.Web.UI.WebControls.Panel pnlCanolaTwo;
		protected System.Web.UI.WebControls.Label lblVarietyTwo;
		protected System.Web.UI.WebControls.DropDownList cboVarietyTwo;
		protected Janus.Web.GridEX.GridEX grdSowDateTwo;
		protected System.Web.UI.WebControls.Label lblSowingDateTwo;
		protected System.Web.UI.WebControls.Label lblScenarioTwo;
		protected System.Web.UI.WebControls.CheckBox chkTriazineOne;
		protected System.Web.UI.WebControls.Panel pnlCanolaOne;
		protected System.Web.UI.WebControls.DropDownList cboVarietyOne;
		protected System.Web.UI.WebControls.Label lblVarietyOne;
		protected Janus.Web.GridEX.GridEX grdSowDateOne;
		protected System.Web.UI.WebControls.Label lblSowingDateOne;
		protected Janus.Web.GridEX.GridEX grdNitrogen;
		protected System.Web.UI.WebControls.Label lblNitrogen;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.DropDownList cboCropOne;
		protected System.Web.UI.WebControls.Label lblCropOne;
		protected System.Web.UI.WebControls.DropDownList cboCropTwo;
		protected System.Web.UI.WebControls.Label lblCropTwo;
		protected System.Web.UI.WebControls.DropDownList cboCropThree;
		protected System.Web.UI.WebControls.Label lblCropThree;
		protected System.Data.DataColumn dcRate;
	

		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			//
			// CODEGEN: This call is required by the ASP.NET Web Form Designer.
			//
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.dsSowDate = new System.Data.DataSet();
			this.dtSowDateOne = new System.Data.DataTable();
			this.dcSowDateOne = new System.Data.DataColumn();
			this.dtSowDateTwo = new System.Data.DataTable();
			this.dcSowDateTwo = new System.Data.DataColumn();
			this.dtSowDateThree = new System.Data.DataTable();
			this.dcSowDateThree = new System.Data.DataColumn();
			this.dsNitrogen = new System.Data.DataSet();
			this.dtNitrogen = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcApplicationDate = new System.Data.DataColumn();
			this.dcRate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDateOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDateTwo)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDateThree)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.cboCropOne.SelectedIndexChanged += new System.EventHandler(this.cboCropOne_SelectedIndexChanged);
			this.grdNitrogen.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdNitrogen_UpdatingCell);
			this.chkAutoCalculateThree.CheckedChanged += new System.EventHandler(this.chkAutoCalculateThree_CheckedChanged);
			this.btnCalculateThree.Click += new System.EventHandler(this.btnReCalculateThree_Click);
			this.chkAutoCalculateOne.CheckedChanged += new System.EventHandler(this.chkAutoCalculateOne_CheckedChanged);
			this.btnCalculateOne.Click += new System.EventHandler(this.btnReCalculateOne_Click);
			this.chkAutoCalculateTwo.CheckedChanged += new System.EventHandler(this.chkAutoCalculateTwo_CheckedChanged);
			this.btnCalculateTwo.Click += new System.EventHandler(this.btnReCalculateTwo_Click);
			this.btnGenerate.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.cboCropTwo.SelectedIndexChanged += new System.EventHandler(this.cboCropTwo_SelectedIndexChanged);
			this.cboCropThree.SelectedIndexChanged += new System.EventHandler(this.cboCropThree_SelectedIndexChanged);
			// 
			// dsSowDate
			// 
			this.dsSowDate.DataSetName = "NewDataSet";
			this.dsSowDate.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsSowDate.Tables.AddRange(new System.Data.DataTable[] {
																		   this.dtSowDateOne,
																		   this.dtSowDateTwo,
																		   this.dtSowDateThree});
			// 
			// dtSowDateOne
			// 
			this.dtSowDateOne.Columns.AddRange(new System.Data.DataColumn[] {
																				this.dcSowDateOne});
			this.dtSowDateOne.TableName = "SowDateOne";
			// 
			// dcSowDateOne
			// 
			this.dcSowDateOne.ColumnName = "SowDate";
			this.dcSowDateOne.DataType = typeof(System.DateTime);
			// 
			// dtSowDateTwo
			// 
			this.dtSowDateTwo.Columns.AddRange(new System.Data.DataColumn[] {
																				this.dcSowDateTwo});
			this.dtSowDateTwo.TableName = "SowDateTwo";
			// 
			// dcSowDateTwo
			// 
			this.dcSowDateTwo.ColumnName = "SowDate";
			this.dcSowDateTwo.DataType = typeof(System.DateTime);
			// 
			// dtSowDateThree
			// 
			this.dtSowDateThree.Columns.AddRange(new System.Data.DataColumn[] {
																				  this.dcSowDateThree});
			this.dtSowDateThree.TableName = "SowDateThree";
			// 
			// dcSowDateThree
			// 
			this.dcSowDateThree.ColumnName = "SowDate";
			this.dcSowDateThree.DataType = typeof(System.DateTime);
			// 
			// dsNitrogen
			// 
			this.dsNitrogen.DataSetName = "NewDataSet";
			this.dsNitrogen.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsNitrogen.Tables.AddRange(new System.Data.DataTable[] {
																			this.dtNitrogen});
			// 
			// dtNitrogen
			// 
			this.dtNitrogen.Columns.AddRange(new System.Data.DataColumn[] {
																			  this.dcID,
																			  this.dcApplicationDate,
																			  this.dcRate});
			this.dtNitrogen.TableName = "Nitrogen";
			// 
			// dcID
			// 
			this.dcID.ColumnName = "ID";
			// 
			// dcApplicationDate
			// 
			this.dcApplicationDate.ColumnName = "ApplicationDate";
			this.dcApplicationDate.DataType = typeof(System.DateTime);
			// 
			// dcRate
			// 
			this.dcRate.ColumnName = "Rate";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDateOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDateTwo)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDateThree)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();

		}
		#endregion




		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private DataTable CreateTempCropStorage()
		{
			DataTable dtCrops = new DataTable("Crops");
			dtCrops.Columns.Add("CropOne");
			dtCrops.Columns.Add("CropTwo");
			dtCrops.Columns.Add("CropThree");
			dtCrops.Columns.Add("CultivarOne");
			dtCrops.Columns.Add("CultivarTwo");
			dtCrops.Columns.Add("CultivarThree");
			dtCrops.Columns.Add("RowConfigurationOne");
			dtCrops.Columns.Add("RowConfigurationTwo");
			dtCrops.Columns.Add("RowConfigurationThree");
			dtCrops.Rows.Add(dtCrops.NewRow());
			return dtCrops;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetCropDropDowns(DataTable dtCrops)
		{
			if(dtCrops.Rows.Count > 0)
			{
				cboCropOne.SelectedValue = dtCrops.Rows[0]["CropOne"].ToString();
				cboCropTwo.SelectedValue = dtCrops.Rows[0]["CropTwo"].ToString();
				cboCropThree.SelectedValue = dtCrops.Rows[0]["CropThree"].ToString();
				cboVarietyOne.SelectedValue = dtCrops.Rows[0]["CultivarOne"].ToString();
				cboVarietyTwo.SelectedValue = dtCrops.Rows[0]["CultivarTwo"].ToString();
				cboVarietyThree.SelectedValue = dtCrops.Rows[0]["CultivarThree"].ToString();
				cboRowConfigurationOne.SelectedValue = dtCrops.Rows[0]["RowConfigurationOne"].ToString();
				cboRowConfigurationTwo.SelectedValue = dtCrops.Rows[0]["RowConfigurationTwo"].ToString();
				cboRowConfigurationThree.SelectedValue = dtCrops.Rows[0]["RowConfigurationThree"].ToString();
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StorePaddockSelection()
		{
			try
			{
				string szPreviousPage = Context.Handler.ToString();
				ViewState["PreviousPage"] = szPreviousPage;

				switch(szPreviousPage)
				{
					case "ASP.wfReportsGenerateConsultant_aspx":
						FunctionsClass.StorePaddockSelectionConsultant(ViewState);
						InitialiseEmptyForm();
						break;

					case "ASP.wfReportsGenerate_aspx":
						FunctionsClass.StorePaddockSelectionGrower(ViewState);
						InitialiseEmptyForm();
						break;

					case "ASP.wfReportsFavouritesConsultant_aspx":
						FunctionsClass.StoreFavouriteSelectionConsultant(ViewState);
						FillFromForFavouriteMode();
						break;

					case "ASP.wfReportsFavourites_aspx":
						FunctionsClass.StoreFavouriteSelectionGrower(ViewState);
						FillFromForFavouriteMode();
						break;

					default:
						break;
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void InitialiseEmptyForm()
		{
			edtReportName.Text = "[PaddockName] "+ ((DataTable)ViewState["Paddocks"]).Rows[0]["ReportType"].ToString();
			FunctionsClass.InitialiseApplicationGrid(dsNitrogen, 5, this);
			SetSowDate(null, 0, grdSowDateOne);
			SetSowDate(null, 1, grdSowDateTwo);
			SetSowDate(null, 2, grdSowDateThree);
			FillCropsCombo();
			FillCultivarsCombo(cboCropOne, cboVarietyOne);
			FillCultivarsCombo(cboCropTwo, cboVarietyTwo);
			FillCultivarsCombo(cboCropThree, cboVarietyThree);
			FillRowConfigurationCombo();
			DisplayCropTypeComponents(pnlSorgumOne, pnlCanolaOne, cboCropOne);
			DisplayCropTypeComponents(pnlSorgumTwo, pnlCanolaTwo, cboCropTwo);
			DisplayCropTypeComponents(pnlSorgumThree, pnlCanolaThree, cboCropOne);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillFromForFavouriteMode()
		{
			edtReportName.Text = ((DataTable)ViewState["Paddocks"]).Rows[0]["ReportName"].ToString();
			FillCropsCombo();
			FillRowConfigurationCombo();
			DataTable dtCrops = CreateTempCropStorage();
			FillFormFromFavouriteXML(ref dtCrops);
			FunctionsClass.InitialiseApplicationGrid(dsNitrogen, 5, this);	
			FunctionsClass.SetToEditFavouriteMode(chkFavourite, btnGenerate);
			SetCropDropDowns(dtCrops);	
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillFormFromFavouriteXML(ref DataTable dtCrops)
		{
			DataTable dtNitrogenApps = DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString());

			int iNitrogenLimit = dtNitrogenApps.Rows.Count;

			string szFavouriteXML = DataAccessClass.GetFavouriteReport(FunctionsClass.GetActiveUserName(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["ReportName"].ToString(),
				((DataTable)ViewState["Paddocks"]).Rows[0]["ReportType"].ToString());


			System.Xml.XmlDocument me = new XmlDocument();
			// Create the XmlReader object.
			XmlTextReader reader = new XmlTextReader(new System.IO.StringReader(szFavouriteXML));
			me.Load(reader);
			XmlNode xmlScenario = me.DocumentElement;
			xmlScenario = xmlScenario.FirstChild;
			int iScenarioCount = 0;
			int iNitrogenCount = 0;

			foreach(XmlNode xmlUpperChild in xmlScenario.ChildNodes)
			{
				if(xmlUpperChild.Name == "scenario")
				{
					iScenarioCount++;
					iNitrogenCount = 0;
				}

				foreach(XmlNode xmlChildNode in xmlUpperChild.ChildNodes)
				{	
					switch(xmlChildNode.Name)
					{
						case "cultivar":
							if(iScenarioCount == 1)
							{
								cboVarietyOne.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["CultivarOne"] = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 2)
							{
								cboVarietyTwo.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["CultivarTwo"] = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 3)
							{
								cboVarietyThree.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["CultivarThree"] = xmlChildNode.InnerText;
							}
							break;

						case "crop":
							if(iScenarioCount == 1)
							{
								cboCropOne.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["CropOne"] = xmlChildNode.InnerText;
								FillCultivarsCombo(cboCropOne, cboVarietyOne);
								DisplayCropTypeComponents(pnlSorgumOne, pnlCanolaOne, cboCropOne);
							}
							else if(iScenarioCount == 2)
							{
								cboCropTwo.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["CropTwo"] = xmlChildNode.InnerText;
								FillCultivarsCombo(cboCropTwo, cboVarietyTwo);
								DisplayCropTypeComponents(pnlSorgumTwo, pnlCanolaTwo, cboCropTwo);
							}
							else if(iScenarioCount == 3)
							{
								cboCropThree.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["CropThree"] = xmlChildNode.InnerText;
								FillCultivarsCombo(cboCropThree, cboVarietyThree);
								DisplayCropTypeComponents(pnlSorgumThree, pnlCanolaThree, cboCropThree);
							}
							break;

						case "sowdate":
							if(iScenarioCount == 1)
							{
								SetSowDate(xmlChildNode.InnerText, iScenarioCount-1, grdSowDateOne);
							}
							else if(iScenarioCount == 2)
							{
								SetSowDate(xmlChildNode.InnerText, iScenarioCount-1, grdSowDateTwo);
							}
							else if(iScenarioCount == 3)
							{
								SetSowDate(xmlChildNode.InnerText, iScenarioCount-1, grdSowDateThree);
							}
							break;

						case "rowconfiguration":
							if(iScenarioCount == 1)
							{
								cboRowConfigurationOne.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["RowConfigurationOne"] = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 2)
							{
								cboRowConfigurationTwo.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["RowConfigurationTwo"] = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 3)
							{
								cboRowConfigurationThree.SelectedValue = xmlChildNode.InnerText;
								dtCrops.Rows[0]["RowConfigurationThree"] = xmlChildNode.InnerText;
							}
							break;

						case "population":
							if(iScenarioCount == 1)
							{
								edtPopulationOne.Text = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 2)
							{
								edtPopulationTwo.Text = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 3)
							{
								edtPopulationThree.Text = xmlChildNode.InnerText;
							}
							break;

						case "ftn":
							if(iScenarioCount == 1)
							{
								edtTillerOne.Text = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 2)
							{
								edtTillerTwo.Text = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 3)
							{
								edtTillerThree.Text = xmlChildNode.InnerText;
							}
							break;

						case "rowspacing":
							if(iScenarioCount == 1)
							{
								edtRowSpacingOne.Text = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 2)
							{
								edtRowSpacingTwo.Text = xmlChildNode.InnerText;
							}
							else if(iScenarioCount == 3)
							{
								edtRowSpacingThree.Text = xmlChildNode.InnerText;
							}
							break;

						case "fertilise":
							if(iScenarioCount == 1)
							{
								iNitrogenCount++;
								if(iNitrogenCount > iNitrogenLimit)
									FunctionsClass.AddNitrogenNodeToNitrogenDataSet(iScenarioCount-1, 
										xmlChildNode, dsNitrogen);
							}
							break;
					}
				}
			}
		}
		//-------------------------------------------------------------------------
		//Set the date shown in the sow date grid
		//-------------------------------------------------------------------------
		private void SetSowDate(string szSowDate, int iTableNumber, 
			Janus.Web.GridEX.GridEX grdSowDate)
		{
			DataRow drSowDate;
			if(szSowDate != null && szSowDate != "")
			{
				drSowDate = dsSowDate.Tables[iTableNumber].NewRow();
				drSowDate["SowDate"] = DateTime.ParseExact(szSowDate, "dd/MM/yyyy", null);
				dsSowDate.Tables[iTableNumber].Rows.Add(drSowDate);
			}
			else
			{
				drSowDate = dsSowDate.Tables[iTableNumber].NewRow();
				drSowDate["SowDate"] = DateTime.Today;
				dsSowDate.Tables[iTableNumber].Rows.Add(drSowDate);
			}
			grdSowDate.DataBind();
		}
		//-------------------------------------------------------------------------
		//Gets all the crops the database and fills the crops combo box with them
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
		{
			DataTable dtCropList = DataAccessClass.GetUsersCrops(FunctionsClass.GetActiveUserName());
			cboCropOne.DataSource = dtCropList;
			cboCropOne.DataTextField = "Type";
			cboCropOne.DataValueField = "Type";
			cboCropOne.DataBind();

			cboCropTwo.DataSource = dtCropList;
			cboCropTwo.DataTextField = "Type";
			cboCropTwo.DataValueField = "Type";
			cboCropTwo.DataBind();

			cboCropThree.DataSource = dtCropList;
			cboCropThree.DataTextField = "Type";
			cboCropThree.DataValueField = "Type";
			cboCropThree.DataBind();
		}
		//-------------------------------------------------------------------------
		//Fills the row configuration combo box with all the row configuration types form the database
		//-------------------------------------------------------------------------
		private void FillRowConfigurationCombo()
		{
			try
			{
				DataTable dtRowConfiguration = DataAccessClass.GetAllRowConfigurationTypes();
				cboRowConfigurationOne.DataSource = dtRowConfiguration;
				cboRowConfigurationOne.DataTextField = "Type";
				cboRowConfigurationOne.DataValueField = "Type";
				cboRowConfigurationOne.DataBind();

				cboRowConfigurationTwo.DataSource = dtRowConfiguration;
				cboRowConfigurationTwo.DataTextField = "Type";
				cboRowConfigurationTwo.DataValueField = "Type";
				cboRowConfigurationTwo.DataBind();

				cboRowConfigurationThree.DataSource = dtRowConfiguration;
				cboRowConfigurationThree.DataTextField = "Type";
				cboRowConfigurationThree.DataValueField = "Type";
				cboRowConfigurationThree.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Gets all the cultivars for the selected crop and fills the cultivars
		//combo box with them
		//-------------------------------------------------------------------------
		private void FillCultivarsCombo(DropDownList cboCrop, DropDownList cboVariety)
		{
			if(cboCrop.SelectedValue != "")
			{
				DataTable dtCultivarList = DataAccessClass.GetAllCultivarsOfCrop(cboCrop.SelectedValue);
				cboVariety.DataSource = dtCultivarList;
				cboVariety.DataTextField = "Type";
				cboVariety.DataValueField = "Type";
				cboVariety.DataBind();
			}
		}
		//-------------------------------------------------------------------------
		//A report is generated and sent to the apsim run machine
		//-------------------------------------------------------------------------
		private void GenerateReport()
		{
			try
			{
				string szReportName = edtReportName.Text;
				szReportName = szReportName.Trim();
				//Check that there is a report name
				if(szReportName != "")
				{
					//Check that the name of the report won't cause a problem when it is stored in the file system
					if(InputValidationClass.IsInputAValidFileLocationString(szReportName) == true)
					{
						//Check that a valid crop is selected
						if(cboCropOne.SelectedValue != "None" && cboCropTwo.SelectedValue != "None" &&
							cboCropThree.SelectedValue != "None")
						{
							if(grdSowDateOne.GetRow(0).Cells["SowDate"].Text != "" && 
								grdSowDateTwo.GetRow(0).Cells["SowDate"].Text != "" &&
								grdSowDateThree.GetRow(0).Cells["SowDate"].Text != "")
							{
								DataTable dtPaddocks = (DataTable)ViewState["Paddocks"];
								string szNewReportName = "";
								for(int iIndex = 0; iIndex < dtPaddocks.Rows.Count; iIndex++)
								{
									szNewReportName = szReportName.Replace("[PaddockName]", dtPaddocks.Rows[iIndex]["PaddockName"].ToString());
									InputValidationClass.ReplaceInvalidFileLocationCharacters(ref szNewReportName);
									RunFTNCalculations(dtPaddocks.Rows[iIndex]["UserName"].ToString(), dtPaddocks.Rows[iIndex]["PaddockName"].ToString());
									//Generate a data table that stores the values particular to the Sow X Variety report
									string szReportXML = 
										ReportClass.PreparePreSeasonXML(szNewReportName, dtPaddocks.Rows[iIndex]["ReportType"].ToString(), 
										cboCropOne.SelectedValue, cboVarietyOne.SelectedValue, grdSowDateOne.GetRow(0).Cells["SowDate"].Text, 
										cboCropTwo.SelectedValue, cboVarietyTwo.SelectedValue, grdSowDateTwo.GetRow(0).Cells["SowDate"].Text, 
										cboCropThree.SelectedValue, cboVarietyThree.SelectedValue, grdSowDateThree.GetRow(0).Cells["SowDate"].Text, 
										cboRowConfigurationOne.SelectedValue, InputValidationClass.ReturnTextBoxValueAsInteger(edtPopulationOne, 0), 
										InputValidationClass.ReturnTextBoxValueAsDouble(edtTillerOne, 0), 
										InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacingOne, 0),
										cboRowConfigurationTwo.SelectedValue, InputValidationClass.ReturnTextBoxValueAsInteger(edtPopulationTwo, 0), 
										InputValidationClass.ReturnTextBoxValueAsDouble(edtTillerTwo, 0), 
										InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacingTwo, 0),
										cboRowConfigurationThree.SelectedValue, InputValidationClass.ReturnTextBoxValueAsInteger(edtPopulationThree, 0), 
										InputValidationClass.ReturnTextBoxValueAsDouble(edtTillerThree, 0), 
										InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacingThree, 0), 
										Convert.ToInt32(chkTriazineOne.Checked), Convert.ToInt32(chkTriazineTwo.Checked) , Convert.ToInt32(chkTriazineThree.Checked),  
										grdNitrogen, dtPaddocks.Rows[iIndex]["UserName"].ToString(), 
										dtPaddocks.Rows[iIndex]["PaddockName"].ToString());

									if(chkFavourite.Checked == true)
									{
										DataAccessClass.SetFavouriteReport(FunctionsClass.GetActiveUserName(), dtPaddocks.Rows[iIndex]["UserName"].ToString(),
											dtPaddocks.Rows[iIndex]["PaddockName"].ToString(), DateTime.Today.ToString("yyyy-MM-dd"), 
											dtPaddocks.Rows[iIndex]["ReportType"].ToString(), szNewReportName, szReportXML);
									}
									if(FunctionsClass.IsSendEmail(btnGenerate))
									{
										////////////////////////////////////////////THIS IS ONLY USING ONE CROP TYPE!!!!!!!!!!!!//////////////////////////////////////////
										//Generate the files needed to generate a report and then email these files to the ApsimRun machine
										if(EmailClass.SendReportEmail(szNewReportName, cboCropOne.SelectedValue, 
											dtPaddocks.Rows[iIndex]["ReportType"].ToString(), szReportXML,  dtPaddocks.Rows[iIndex]["UserName"].ToString(),
											dtPaddocks.Rows[iIndex]["PaddockName"].ToString()) == true)
										{
										}
										else
											throw new Exception("Error requesting report");
									}
								}
								FunctionsClass.TransferAfterCompletion(btnGenerate);
							}
							else
								throw new Exception("Please ensure all sowing date fields contain a date");
						}
						else
							throw new Exception("Please select a crop type and variety type");
					}
					else
						throw new Exception(InputValidationClass.ReturnInvalidLocationMessage("Report description"));
				}
				else
					throw new Exception("Please enter a report name");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DisplayCropTypeComponents(Panel pnlSorgum, Panel pnCanola, DropDownList cboCrop)
		{
			switch(cboCrop.SelectedValue)
			{
				case "Sorghum":
					pnlSorgum.Visible = true;
					break;

				case "Canola":
					pnCanola.Visible = true;
					break;

				default:
					pnlSorgum.Visible = false;
					pnCanola.Visible = false;
					break;
			}
		}
		//-------------------------------------------------------------------------
		//Checks to see if the auto calculate option is selected for each
		//scenario and if it is runs the calculation to update the result
		//-------------------------------------------------------------------------
		private void RunFTNCalculations(string szUserName, string szPaddockName)
		{
			if(chkAutoCalculateOne.Checked == true)
				FunctionsClass.SetFertileTillerNumber(szUserName, szPaddockName, edtTillerOne, 
					FunctionsClass.ReturnPopulationValue(edtPopulationOne), 
					cboRowConfigurationOne.SelectedValue, 
					DateTime.ParseExact(grdSowDateOne.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
			if(chkAutoCalculateTwo.Checked == true)
				FunctionsClass.SetFertileTillerNumber(szUserName, szPaddockName, edtTillerTwo, 
					FunctionsClass.ReturnPopulationValue(edtPopulationTwo), 
					cboRowConfigurationTwo.SelectedValue, 
					DateTime.ParseExact(grdSowDateTwo.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
			if(chkAutoCalculateThree.Checked == true)
				FunctionsClass.SetFertileTillerNumber(szUserName, szPaddockName, edtTillerThree, 
					FunctionsClass.ReturnPopulationValue(edtPopulationThree), 
					cboRowConfigurationThree.SelectedValue, 
					DateTime.ParseExact(grdSowDateThree.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Cancel()
		{
			string szPreviousPage = ViewState["PreviousPage"].ToString();

			switch(szPreviousPage)
			{
				case "ASP.wfReportsFavouritesConsultant_aspx":
					Server.Transfer("wfReportsFavouritesConsultant.aspx");
					break;

				case "ASP.wfReportsFavourites_aspx":
					Server.Transfer("wfReportsFavourites.aspx");
					break;

				case "ASP.wfReportsGenerateConsultant_aspx":
					Server.Transfer("wfReportsGenerateConsultant.aspx");
					break;

				case "ASP.wfReportsGenerate_aspx":
					Server.Transfer("wfReportsGenerate.aspx");
					break;

				default:
					break;
			}
		}
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{	
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				FunctionsClass.CheckForWritePriviledges();

				StorePaddockSelection();
				FunctionsClass.SetControlFocus("edtReportName", this);
			
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetReportNavigationButtons(btnReportsView, btnNewReports, btnFavouriteReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnReCalculateOne_Click(object sender, System.EventArgs e)
		{
			FunctionsClass.SetFertileTillerNumber(((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), edtTillerOne, 
				FunctionsClass.ReturnPopulationValue(edtPopulationOne), 
				cboRowConfigurationOne.SelectedValue, 
				DateTime.ParseExact(grdSowDateOne.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnReCalculateTwo_Click(object sender, System.EventArgs e)
		{
			FunctionsClass.SetFertileTillerNumber(((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), edtTillerTwo,
				FunctionsClass.ReturnPopulationValue(edtPopulationTwo), 
				cboRowConfigurationTwo.SelectedValue, 
				DateTime.ParseExact(grdSowDateTwo.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnReCalculateThree_Click(object sender, System.EventArgs e)
		{
			FunctionsClass.SetFertileTillerNumber(((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), edtTillerThree, 
				FunctionsClass.ReturnPopulationValue(edtPopulationThree), 
				cboRowConfigurationThree.SelectedValue, 
				DateTime.ParseExact(grdSowDateThree.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void chkAutoCalculateOne_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkAutoCalculateOne.Checked == true)
			{
				edtTillerOne.Enabled = false;
			}
			else
			{
				edtTillerOne.Enabled = true;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void chkAutoCalculateTwo_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkAutoCalculateTwo.Checked == true)
			{
				edtTillerTwo.Enabled = false;
			}
			else
			{
				edtTillerTwo.Enabled = true;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void chkAutoCalculateThree_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkAutoCalculateThree.Checked == true)
			{
				edtTillerThree.Enabled = false;
			}
			else
			{
				edtTillerThree.Enabled = true;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void grdNitrogen_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
		{
			if(e.Column.Key == "Rate")
			{
				if(e.Value != null)
				{
					if(InputValidationClass.IsInputAPositiveDecimal(e.Value.ToString()) == false)
					{
						e.Value = "0";
					}
				}
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnGenerate_Click(object sender, System.EventArgs e)
		{
			GenerateReport();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Cancel();
		}
		//-------------------------------------------------------------------------
		//When the user selects a different crop type from the crops combo box
		//the cultivar combo box is updated with the corresponding cultivars linked
		//to that crop
		//-------------------------------------------------------------------------
		private void cboCropOne_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			FillCultivarsCombo(cboCropOne, cboVarietyOne);
			DisplayCropTypeComponents(pnlSorgumOne, pnlCanolaOne, cboCropOne);
		}
		//-------------------------------------------------------------------------
		//When the user selects a different crop type from the crops combo box
		//the cultivar combo box is updated with the corresponding cultivars linked
		//to that crop
		//-------------------------------------------------------------------------
		private void cboCropTwo_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			FillCultivarsCombo(cboCropTwo, cboVarietyTwo);
			DisplayCropTypeComponents(pnlSorgumTwo, pnlCanolaTwo, cboCropTwo);
		}
		//-------------------------------------------------------------------------
		//When the user selects a different crop type from the crops combo box
		//the cultivar combo box is updated with the corresponding cultivars linked
		//to that crop
		//-------------------------------------------------------------------------
		private void cboCropThree_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			FillCultivarsCombo(cboCropThree, cboVarietyThree);
			DisplayCropTypeComponents(pnlSorgumThree, pnlCanolaThree, cboCropThree);
		}
		//-------------------------------------------------------------------------
		#endregion



	}
}
