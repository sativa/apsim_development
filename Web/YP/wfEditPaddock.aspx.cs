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

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for wfEditPaddock.
	/// </summary>
	public class wfEditPaddock : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblCrop;
		protected System.Web.UI.WebControls.Label lblCultivar;
		protected System.Web.UI.WebControls.DropDownList cboCultivars;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.CheckBox chkSown;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.Label lblCropManagement;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.Label lblNitrogen;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnSettingUpImg;
		protected System.Web.UI.WebControls.ImageButton btnRainfallImg;
		protected System.Web.UI.WebControls.ImageButton btnReportsImg;
		protected System.Web.UI.WebControls.LinkButton btnSettingUp;
		protected System.Web.UI.WebControls.LinkButton btnRainfall;
		protected System.Web.UI.WebControls.CheckBox chkEmail;
		protected System.Web.UI.WebControls.LinkButton btnReport;
		protected System.Web.UI.WebControls.DropDownList cboReport;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected Janus.Web.GridEX.GridEX grdNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDate;
		protected System.Data.DataColumn dcSowDate;
		protected Janus.Web.GridEX.GridEX grdSowDate;
		protected System.Web.UI.WebControls.Label lblPaddockName;
		protected System.Web.UI.WebControls.TextBox edtPaddockName;
		protected System.Web.UI.WebControls.CheckBox chkTriazine;
		protected System.Web.UI.WebControls.Label lblTriazine;
		protected System.Data.DataSet dsIrrigation;
		protected System.Data.DataTable dtIrrigation;
		protected System.Data.DataColumn dcIrrigationID;
		protected System.Data.DataColumn dcIrrigationDate;
		protected System.Data.DataColumn dcIrrigationAmount;
		protected System.Data.DataColumn dcIrrigationEfficency;
		protected Janus.Web.GridEX.GridEX grdIrrigation;
		protected System.Web.UI.WebControls.Label lblIrrigation;
		protected System.Web.UI.WebControls.Label lblRowConfiguration;
		protected System.Web.UI.WebControls.Label lblPopulation;
		protected System.Web.UI.WebControls.TextBox edtPopulation;
		protected System.Web.UI.WebControls.DropDownList cboRowConfiguration;
		protected System.Web.UI.WebControls.Label lblPopulationUnit;
		protected System.Web.UI.WebControls.TextBox edtTiller;
		protected System.Web.UI.WebControls.Label lblTiller;
		protected System.Web.UI.WebControls.Button btnCalculate;
		protected System.Web.UI.WebControls.Label lblRowSpacing;
		protected System.Web.UI.WebControls.TextBox edtRowSpacing;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnit;
		protected System.Web.UI.WebControls.HyperLink HyperLink1;



		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			System.Globalization.DateTimeFormatInfo.CurrentInfo.ShortDatePattern = "dd/MM/yyyy";
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.dsNitrogen = new System.Data.DataSet();
			this.dtNitrogen = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcApplicationDate = new System.Data.DataColumn();
			this.dcRate = new System.Data.DataColumn();
			this.dsSowDate = new System.Data.DataSet();
			this.dtSowDate = new System.Data.DataTable();
			this.dcSowDate = new System.Data.DataColumn();
			this.dsIrrigation = new System.Data.DataSet();
			this.dtIrrigation = new System.Data.DataTable();
			this.dcIrrigationID = new System.Data.DataColumn();
			this.dcIrrigationDate = new System.Data.DataColumn();
			this.dcIrrigationAmount = new System.Data.DataColumn();
			this.dcIrrigationEfficency = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsIrrigation)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtIrrigation)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.btnSettingUpImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSettingUpImg_Click);
			this.btnRainfallImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnRainfallImg_Click);
			this.btnReportsImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnReportsImg_Click);
			this.btnSettingUp.Click += new System.EventHandler(this.btnSettingUp_Click);
			this.btnRainfall.Click += new System.EventHandler(this.btnRainfall_Click);
			this.btnReport.Click += new System.EventHandler(this.btnReport_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.chkSown.CheckedChanged += new System.EventHandler(this.chkSown_CheckedChanged);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
			this.grdNitrogen.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdNitrogen_UpdatingCell);
			this.grdIrrigation.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdIrrigation_UpdatingCell_1);
			this.btnCalculate.Click += new System.EventHandler(this.btnCalculate_Click);
			// 
			// dsNitrogen
			// 
			this.dsNitrogen.DataSetName = "NewDataSet";
			this.dsNitrogen.Locale = new System.Globalization.CultureInfo("en-US");
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
			// 
			// dsSowDate
			// 
			this.dsSowDate.DataSetName = "NewDataSet";
			this.dsSowDate.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsSowDate.Tables.AddRange(new System.Data.DataTable[] {
																		   this.dtSowDate});
			// 
			// dtSowDate
			// 
			this.dtSowDate.Columns.AddRange(new System.Data.DataColumn[] {
																			 this.dcSowDate});
			this.dtSowDate.TableName = "SowDate";
			// 
			// dcSowDate
			// 
			this.dcSowDate.ColumnName = "SowDate";
			this.dcSowDate.DataType = typeof(System.DateTime);
			// 
			// dsIrrigation
			// 
			this.dsIrrigation.DataSetName = "NewDataSet";
			this.dsIrrigation.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsIrrigation.Tables.AddRange(new System.Data.DataTable[] {
																			  this.dtIrrigation});
			// 
			// dtIrrigation
			// 
			this.dtIrrigation.Columns.AddRange(new System.Data.DataColumn[] {
																				this.dcIrrigationID,
																				this.dcIrrigationDate,
																				this.dcIrrigationAmount,
																				this.dcIrrigationEfficency});
			this.dtIrrigation.TableName = "Irrigation";
			// 
			// dcIrrigationID
			// 
			this.dcIrrigationID.Caption = "ID";
			this.dcIrrigationID.ColumnName = "ID";
			// 
			// dcIrrigationDate
			// 
			this.dcIrrigationDate.Caption = "Date";
			this.dcIrrigationDate.ColumnName = "Date";
			this.dcIrrigationDate.DataType = typeof(System.DateTime);
			// 
			// dcIrrigationAmount
			// 
			this.dcIrrigationAmount.Caption = "Amount (mm/ha)";
			this.dcIrrigationAmount.ColumnName = "Amount";
			// 
			// dcIrrigationEfficency
			// 
			this.dcIrrigationEfficency.Caption = "Efficency (%)";
			this.dcIrrigationEfficency.ColumnName = "Efficency";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsIrrigation)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtIrrigation)).EndInit();

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Checks to see if a sow date has been entered for the selected paddock,
		//if it has then the form is filled with data from the database.  If 
		//no sow date has been entered then the form is initialised.
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			DisplayGrowersName();
			
			edtPaddockName.Text = Session["SelectedPaddockName"].ToString();
			try
				{
				DataTable dtPaddockDetails = 
					DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				string szSowDate = dtPaddockDetails.Rows[0]["SowDate"].ToString();
				//If the sow date is not empty then fill the form with data from the database
				if(szSowDate != null && szSowDate != "")
					{
					//Fills the crops combo box
					FillCropsCombo();
					//Fills the row configuration combo
					FillRowConfigurationCombo();
					//Enables all the components on the form
					ChangeEnableCropDetails(true);
					//Sets the selected crop to the crop returned from the database		
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();
					//Sets the selected row configuration option to the row configuration option returned from the database	
					cboRowConfiguration.SelectedValue = dtPaddockDetails.Rows[0]["RowConfigurationType"].ToString();
					//Set the sow date on the calander
					chkSown.Checked = true;
					//Checks to ensure that a triazine value is in the database, if it is set
					//its value on the form
					if(dtPaddockDetails.Rows[0]["Triazine"].ToString() != null &&
						dtPaddockDetails.Rows[0]["Triazine"].ToString() != "")
						{
						chkTriazine.Checked = Convert.ToBoolean(Convert.ToInt32(dtPaddockDetails.Rows[0]["Triazine"].ToString()));
						}
					//Checks to ensure that a population value is in the database, if it is set
					//its value on the form
					if(dtPaddockDetails.Rows[0]["Population"].ToString() != null &&
						dtPaddockDetails.Rows[0]["Population"].ToString() != "")
					{
						int population = Convert.ToInt32(dtPaddockDetails.Rows[0]["Population"]);
						population = population * 10000;
						edtPopulation.Text = population.ToString();
					}
					//Check to see if the tiller number from the db isn't null, 
					//if it isn't then set the value of the textbox
					if(dtPaddockDetails.Rows[0]["TillerNumber"].ToString() != null &&
						dtPaddockDetails.Rows[0]["TillerNumber"].ToString() != "")
						{
						edtTiller.Text = dtPaddockDetails.Rows[0]["TillerNumber"].ToString();
						}
					//Check to see if the Row Spacing from the db isn't null, 
					//if it isn't then set the value of the textbox
					if(dtPaddockDetails.Rows[0]["RowSpacing"].ToString() != null &&
						dtPaddockDetails.Rows[0]["RowSpacing"].ToString() != "")
						{
						edtRowSpacing.Text = dtPaddockDetails.Rows[0]["RowSpacing"].ToString();
						}
					
					SetSowDate(szSowDate);
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();
					//Fills the cultivar combo box
					FillCultivarsCombo();
					//Fills the Report type combo
					FillReportTypesCombo();		
					//Display Crop type dependant components
					DisplayCropTypeComponents();
					//Sets up and fills the nitrogen application grid
					FillNitrogenApplicationGrid();
					//Sets up and fills the irrigation application grid
					FillIrrigationApplicationGrid();
					//Restores the selected crop to the crop returned from the database	
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();		
					//Sets the selected row configuration option to the row configuration option returned from the database	
					cboRowConfiguration.SelectedValue = dtPaddockDetails.Rows[0]["RowConfigurationType"].ToString();
					//Sets the selected cultivar to the cultivar returned from the database
					cboCultivars.SelectedValue = dtPaddockDetails.Rows[0]["CultivarType"].ToString();
					}
				//If no sow date has been saved, then initialise the form.
				else
					{
					ChangeEnableCropDetails(false);
					FillNitrogenApplicationGrid();
					FillIrrigationApplicationGrid();
					FillCropsCombo();
					FillRowConfigurationCombo();
					FillReportTypesCombo();
					FillCultivarsCombo();
					SetSowDate(szSowDate);
					DisplayCropTypeComponents();
					}
				if(FunctionsClass.IsAdministrator(FunctionsClass.GetActiveUserName()) == true)
					{
					chkEmail.Visible = true;
					}
				else
					{
					chkEmail.Visible = false;
					}
				//If the Linked Temporal Paddock ID is not zero then don't allow the user to enter rainfall
				//as this paddocks rainfall events are linked another paddocks. 
				if(dtPaddockDetails.Rows[0]["LinkedRainfallPaddockName"].ToString() != "" || 
					Convert.ToBoolean(Convert.ToInt32(dtPaddockDetails.Rows[0]["DefaultRainfall"].ToString())) == true)
					{
					btnRainfall.Enabled = false;
					btnRainfallImg.Enabled = false;
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Set the date shown in the sow date grid
		//-------------------------------------------------------------------------
		private void SetSowDate(string szSowDate)
			{
			DataRow drSowDate;
			if(szSowDate != null && szSowDate != "")
				{
				drSowDate = dsSowDate.Tables["SowDate"].NewRow();
				drSowDate["SowDate"] = DateTime.ParseExact(szSowDate, "yyyy-MM-dd", null);
				dsSowDate.Tables["SowDate"].Rows.Add(drSowDate);
				}
			else
				{
				drSowDate = dsSowDate.Tables["SowDate"].NewRow();
				drSowDate["SowDate"] = DateTime.Today;
				dsSowDate.Tables["SowDate"].Rows.Add(drSowDate);
				}
			this.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets the grower's name and the paddock's name and displays them on a label
		//-------------------------------------------------------------------------
		private void DisplayGrowersName()
			{
			try
				{
				DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());
				lblName.Text = dtUsersDetails.Rows[0]["Name"].ToString();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Gets all the report types the database and fills the report types combo box with them
		//-------------------------------------------------------------------------
		private void FillReportTypesCombo()
			{
			try
				{
				if(cboCrops.SelectedValue != "")
					{
					DataTable dtReport = DataAccessClass.GetAllReportTypesForCropType(cboCrops.SelectedValue);
					cboReport.DataSource = dtReport;
					cboReport.DataTextField = "Type";
					cboReport.DataValueField = "Type";
					cboReport.DataBind();
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Gets all the crops the database and fills the crops combo box with them
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
			{
			try
				{
				DataTable dtCropList = DataAccessClass.GetUsersCrops(FunctionsClass.GetActiveUserName());
				cboCrops.DataSource = dtCropList;
				cboCrops.DataTextField = "Type";
				cboCrops.DataValueField = "Type";
				cboCrops.DataBind();
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
		private void FillCultivarsCombo()
			{
			try
				{
				if(cboCrops.SelectedValue != "")
					{
					DataTable dtCultivarList = DataAccessClass.GetAllCultivarsOfCrop(cboCrops.SelectedValue);
					cboCultivars.DataSource = dtCultivarList;
					cboCultivars.DataTextField = "Type";
					cboCultivars.DataValueField = "Type";
					cboCultivars.DataBind();
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the row configuration combo box with all the row configuration types form the database
		//-------------------------------------------------------------------------
		private void FillRowConfigurationCombo()
		{
			try
			{
				DataTable dtRowConfiguration = DataAccessClass.GetAllRowConfigurationTypes();
				cboRowConfiguration.DataSource = dtRowConfiguration;
				cboRowConfiguration.DataTextField = "Type";
				cboRowConfiguration.DataValueField = "Type";
				cboRowConfiguration.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Fills the nitrogen application grid with the vales stored in the database
		//-------------------------------------------------------------------------	
		private void FillNitrogenApplicationGrid()
			{
			try
				{
				DataTable dtNitrogenApplications =
					DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
				
				DataRow drNitrogen;
				int iMaxNumberOfRows = 5;
				foreach(DataRow drNitrogenApplication in dtNitrogenApplications.Rows)
					{
					drNitrogen = dsNitrogen.Tables["Nitrogen"].NewRow();
					drNitrogen["Rate"] = drNitrogenApplication["Rate"];
					drNitrogen["ApplicationDate"] = DateTime.ParseExact(drNitrogenApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null);
					dsNitrogen.Tables["Nitrogen"].Rows.Add(drNitrogen);
					}
				for(int iIndex = dsNitrogen.Tables["Nitrogen"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
					{
					drNitrogen = dsNitrogen.Tables["Nitrogen"].NewRow();
					dsNitrogen.Tables["Nitrogen"].Rows.Add(drNitrogen);
					}
				this.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the irrigation application grid with the vales stored in the database
		//-------------------------------------------------------------------------	
		private void FillIrrigationApplicationGrid()
		{
			try
			{
				//Gets the data and converts it from xml format to a datatable
				DataTable dtIrrigationApplication =
					DataAccessClass.GetPaddocksIrrigationApplications(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				//Ensure that a sample has been added
				DataRow drIrrigation;
				int iMaxNumberOfRows = 10;
				foreach(DataRow drIrrigationApplication in dtIrrigationApplication.Rows)
				{
					drIrrigation = dsIrrigation.Tables["Irrigation"].NewRow();
					drIrrigation["Amount"] = drIrrigationApplication["Amount"];
					drIrrigation["Date"] = DateTime.ParseExact(drIrrigationApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null);
					drIrrigation["Efficency"] = drIrrigationApplication["Efficency"];
					dsIrrigation.Tables["Irrigation"].Rows.Add(drIrrigation);
				}
				for(int iIndex = dsIrrigation.Tables["Irrigation"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
				{
					drIrrigation = dsIrrigation.Tables["Irrigation"].NewRow();
					dsIrrigation.Tables["Irrigation"].Rows.Add(drIrrigation);
				}
				this.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void ChangeEnableCropDetails(bool bEnableCropDetails)
			{
			cboCrops.Enabled = bEnableCropDetails;
			cboCultivars.Enabled = bEnableCropDetails;
			grdSowDate.Enabled = bEnableCropDetails;
			grdNitrogen.Enabled = bEnableCropDetails;
			chkTriazine.Enabled =  bEnableCropDetails;
			cboRowConfiguration.Enabled = bEnableCropDetails;
			edtPopulation.Enabled = bEnableCropDetails;
			edtTiller.Enabled = bEnableCropDetails;
			btnCalculate.Enabled = bEnableCropDetails;
			edtRowSpacing.Enabled = bEnableCropDetails;
			}	
		//-------------------------------------------------------------------------
		//Displays visual components that are dependant on the type of crop selected
		//-------------------------------------------------------------------------
		private void DisplayCropTypeComponents()
		{
			SetVisabilityOfCanolaComponents();
			SetVisibilityOfSorgumComponents();
		}
		//-------------------------------------------------------------------------
		//Sets the visibility of the triazine option depending on the crop selected
		//-------------------------------------------------------------------------
		private void SetVisabilityOfCanolaComponents()
		{
			bool bTriazineVisibility = false;
			if(cboCrops.SelectedValue == "Canola")
				bTriazineVisibility = true;

			lblTriazine.Visible = bTriazineVisibility;
			chkTriazine.Visible = bTriazineVisibility;
		}	
		//-------------------------------------------------------------------------
		//Sets the visibility of the triazine option depending on the crop selected
		//-------------------------------------------------------------------------
		private void SetVisibilityOfSorgumComponents()
		{
			bool bSorgumComponentVisibility = false;
			if(cboCrops.SelectedValue == "Sorghum")
				bSorgumComponentVisibility = true;

			lblRowConfiguration.Visible = bSorgumComponentVisibility;
			cboRowConfiguration.Visible = bSorgumComponentVisibility;
			lblPopulation.Visible = bSorgumComponentVisibility;
			edtPopulation.Visible = bSorgumComponentVisibility;
			lblPopulationUnit.Visible = bSorgumComponentVisibility;
			edtTiller.Visible = bSorgumComponentVisibility;
			lblTiller.Visible = bSorgumComponentVisibility;
			btnCalculate.Visible = bSorgumComponentVisibility;
			lblRowSpacing.Visible  = bSorgumComponentVisibility;
			edtRowSpacing.Visible = bSorgumComponentVisibility;
			lblRowSpacingUnit.Visible = bSorgumComponentVisibility;
		}	
		//-------------------------------------------------------------------------
		//The paddock details are updated, but firstly a check is run to determine
		//if the the sown check box is checked.  If it is, then the paddock is updated
		//with the details from the from.  If it hasn't been checked then the paddock
		//is updated with blank details.
		//-------------------------------------------------------------------------
		private bool SavePaddockDetails()
			{
			bool bPaddockSaved = false;
			string szPaddockName = InputValidationClass.ValidateString(edtPaddockName.Text);
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				if(szPaddockName != "" && szPaddockName != null)
					{
					try
						{
						//If the sown check box is checked the paddock record is updated with 
						//the details from the form
						if(chkSown.Checked == true)
							{
							//If a cultivar is selected, update the paddock
							if( cboCultivars.SelectedValue != "" && 
								cboCultivars.SelectedValue != "None" && 
								(cboRowConfiguration.Visible == false || cboRowConfiguration.SelectedValue != "None") &&
								grdSowDate.GetRow(0).Cells["SowDate"].Text != "")
								{
								DataAccessClass.UpdatePaddock((DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
									cboCultivars.SelectedItem.Text, Convert.ToInt32(chkTriazine.Checked), "", "", cboRowConfiguration.SelectedValue, -1, "", "", 
									ReturnPopulationValue(), -1, InputValidationClass.ReturnTextBoxValueAsDouble(edtTiller, 0), 
									 InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacing, 0), szPaddockName, Session["SelectedPaddockName"].ToString(), 
									FunctionsClass.GetActiveUserName());
								Session["SelectedPaddockName"] = szPaddockName;
								SaveNitrogenApplications();
								SaveIrrigationApplications();
								bPaddockSaved = true;
								}
								//If no cultivar is selected display an error to the user
							else
								{
								FunctionsClass.DisplayMessage(Page,"Please ensure that all fields contain data");
								}
							}
							//If the sown check box hasn't been checked, the paddock is updated with default
							//settings.
						else
							{
							DataAccessClass.DeletePaddocksFertiliserApplications("Nitrogen", 
								Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
							DataAccessClass.DeletePaddocksIrrigationApplications(
								Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
							DataAccessClass.ResetPaddock(Session["SelectedPaddockName"].ToString(), szPaddockName, FunctionsClass.GetActiveUserName());
							Session["SelectedPaddockName"] = szPaddockName;
							bPaddockSaved = true;
							}
						}
					catch(Exception E)
						{
						FunctionsClass.DisplayMessage(Page, E.Message);
						}
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "Missing paddock name");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
				}
			return bPaddockSaved;
			}
		//-------------------------------------------------------------------------
		//Saves the values from the grid into the database
		//-------------------------------------------------------------------------
		private void SaveNitrogenApplications()
			{
			try
				{
				Janus.Web.GridEX.GridEXRow grdRow;
				grdNitrogen.UpdateOnLeave = true;
				DataAccessClass.DeletePaddocksFertiliserApplications("Nitrogen", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
				for(int iIndex = 0; iIndex < grdNitrogen.RowCount; iIndex++)
					{
					grdRow = grdNitrogen.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Rate"].Value != null && grdRow.Cells["ApplicationDate"].Value != null)
						{
						DataAccessClass.InsertFertiliserApplication((DateTime.ParseExact(grdRow.Cells["ApplicationDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
							grdRow.Cells["Rate"].Text, "Nitrogen", Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
						}
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Saves the values from the grid into the database
		//-------------------------------------------------------------------------
		private void SaveIrrigationApplications()
		{
			try
			{
				Janus.Web.GridEX.GridEXRow grdRow;
				grdIrrigation.UpdateOnLeave = true;
				DataAccessClass.DeletePaddocksIrrigationApplications(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				for(int iIndex = 0; iIndex < grdNitrogen.RowCount; iIndex++)
				{
					grdRow = grdIrrigation.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Amount"].Value != null && grdRow.Cells["Date"].Value != null && grdRow.Cells["Efficency"].Value != null)
					{
						DataAccessClass.InsertIrrigationApplication((DateTime.ParseExact(grdRow.Cells["Date"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
							grdRow.Cells["Amount"].Text, grdRow.Cells["Efficency"].Text, Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
					}
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//If the user isn't a visitor then then the needed details are stored
		//in session variables and the user is sent to the generate report page
		//-------------------------------------------------------------------------			
		private void SendToGenerateReportPage()
			{
			try
				{
				//If the user isn't a visitor
				if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
					{
					if(SavePaddockDetails() == true)
						{
						if(FunctionsClass.IsSampleValid(Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName()))
							{	
							//Checks that the report type is selected
							if(cboReport.SelectedItem.Text != "")
								{
								if(ReportClass.DoesUsersReportDirectoryExisit(FunctionsClass.GetActiveUserName(), DateTime.Today.Year) == false)
									{
									ReportClass.CreateUsersReportDirectory(FunctionsClass.GetActiveUserName(), DateTime.Today.Year); 
									}


								switch(cboReport.SelectedItem.Text)
									{
									case  ReportClass.szNitrogenComparisonReport:
										if(chkSown.Checked == true)
											{
											Server.Transfer("wfGenerateNitrogenComparisonReport.aspx");
											}
										else
											throw new Exception("This report requires a sowing date");
										break;

									case ReportClass.szNitrogenProfitReport:
										if(chkSown.Checked == true)
											{
											Server.Transfer("wfGenerateNitrogenProfitReport.aspx");
											}
										else
											throw new Exception("This report requires a sowing date");
										break;

									case ReportClass.szIrrigationComparisonReport:
										if(chkSown.Checked == true)
										{
											Server.Transfer("wfGenerateIrrigationComparisonReport.aspx");
										}
										else
											throw new Exception("This report requires a sowing date");
										break;

									case ReportClass.szAgronomicReport:
										if(chkSown.Checked == true)
											{
											Server.Transfer("wfGenerateReport.aspx");
											}
										else
											throw new Exception("This report requires a sowing date");
										break;

									case ReportClass.szClimateReport:
										if(chkSown.Checked == true)
											{
											Server.Transfer("wfGenerateReport.aspx");
											}
										else
											throw new Exception("This report requires a sowing date");
										break;

									case ReportClass.szSowingXVarietyReport:
										Server.Transfer("wfGenerateSowingXVarietyReport.aspx");
										break;


									case ReportClass.szFallowReport:
										Server.Transfer("wfGenerateFallowReport.aspx");
										break;

									default:
										throw new Exception("Not a valid report type");
									}
								}
							else
								throw new Exception("Please select a report type");
							}
						else
							throw new Exception("Please visit the paddock setup page and set the initial water and nitrogen conditions.");
						}//END OF SAVE PADDOCK IF STATEMENT (THIS FUNCTION GIVES ITS OWN WARNINGS IF IT FAILS
					}
				else
					throw new Exception("Functionality not available to visitors");
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Returns the value of the Population text box after it is checked to insure 
		//that the value is a valid integer
		//-------------------------------------------------------------------------
		private int ReturnPopulationValue()
			{
			int iPopulation = 0;
			if(edtPopulation.Text != "")
				{
				if(InputValidationClass.IsInputAPositiveInteger(edtPopulation.Text))
					{
					iPopulation = Convert.ToInt32(edtPopulation.Text) / 10000;
					}
				}
			return iPopulation;
			}
		//-------------------------------------------------------------------------
		//Calculates and displays the fertile Tiller number
		//-------------------------------------------------------------------------
		private void SetFertileTillerNumber()
		{
			try
				{
				DataTable dtPaddockDetails = 
					DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());

				if(dtPaddockDetails.Rows[0]["RegionType"].ToString() != null && 
					dtPaddockDetails.Rows[0]["RegionType"].ToString() != "" && 
					dtPaddockDetails.Rows[0]["RegionType"].ToString() != "None" &&
					grdSowDate.GetRow(0).Cells["SowDate"].Text != "" &&
					InputValidationClass.IsInputAPositiveInteger(edtPopulation.Text) == true)
					{
					edtTiller.Text = FunctionsClass.ReturnTillerNumber(cboRowConfiguration.SelectedValue, dtPaddockDetails.Rows[0]["RegionType"].ToString(),
						DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null), Convert.ToInt32(edtPopulation.Text)).ToString();
					}
				else
					{
					throw new Exception("Please ensure that a valid region is selected and a valid population is entered");
					}
				}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
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
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FillForm();
				btnSave.Style.Add("cursor", "hand");
				if(FunctionsClass.IsConsultantOrHigher(Session["UserName"].ToString()) == true)
					{
					edtPaddockName.Enabled = true;
					}
				else
					{
					edtPaddockName.Enabled = false;
					}
				}
			}
		//-------------------------------------------------------------------------
		//Returns the report type, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnReportType()
			{
			return cboReport.SelectedItem.Text;
			} 
		//-------------------------------------------------------------------------
		//Returns the status of the email check box, can be called from other pages
		//on the page load event
		//-------------------------------------------------------------------------
		public bool ReturnEmailConParFiles()
			{
			return chkEmail.Checked;
			} 
		//-------------------------------------------------------------------------
		//When the save button is pressed by the user, the paddock details are updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SavePaddockDetails();
			}
		//-------------------------------------------------------------------------
		//When the save imgae is pressed by the user, the paddock details are updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SavePaddockDetails();
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button, the page is refreshed
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image, the page is refreshed
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
			}
		//-------------------------------------------------------------------------
		//When the Setting up button is pressed by the user, the page details are 
		//save and the user is transfered to the Setup Paddock page
		//-------------------------------------------------------------------------
		private void btnSettingUp_Click(object sender, System.EventArgs e)
			{
			SavePaddockDetails();
			Server.Transfer("wfEditSetupPaddock.aspx");
			}
		//-------------------------------------------------------------------------
		//When the Setting up image is pressed by the user, the page details are
		//saved and the user is transfered to the Setup Paddock page
		//-------------------------------------------------------------------------
		private void btnSettingUpImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SavePaddockDetails();
			Server.Transfer("wfEditSetupPaddock.aspx");			
			}
		//-------------------------------------------------------------------------
		//When the Rainfall button is pressed by the user, the page details are 
		//saved and the user is transfered to the rainfall page
		//-------------------------------------------------------------------------
		private void btnRainfall_Click(object sender, System.EventArgs e)
			{
			SavePaddockDetails();
			Server.Transfer("wfEditRainfall.aspx");
			}
		//-------------------------------------------------------------------------
		//When the Rainfall image is pressed by the user, the page details are saved
		//and the user is transfered to the rainfall page
		//-------------------------------------------------------------------------
		private void btnRainfallImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SavePaddockDetails();
			Server.Transfer("wfEditRainfall.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the report button, the page details are saved and
		//they are sent to the generate report page
		//-------------------------------------------------------------------------
		private void btnReport_Click(object sender, System.EventArgs e)
			{
			SavePaddockDetails();
			SendToGenerateReportPage();
			}
		//-------------------------------------------------------------------------
		//When the user presses the report image, the page details are saved 
		//and they are sent to the generate report page
		//-------------------------------------------------------------------------
		private void btnReportsImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			btnReport_Click(null, null);
			}
		//-------------------------------------------------------------------------
		//When the user changes the sown check box, the components on the form are
		//enable or disabled depending on the state of the sown check box
		//-------------------------------------------------------------------------
		private void chkSown_CheckedChanged(object sender, System.EventArgs e)
			{
			ChangeEnableCropDetails(chkSown.Checked);
			}
		//-------------------------------------------------------------------------
		//When the user selects a different crop type from the crops combo box
		//the cultivar combo box is updated with the corresponding cultivars linked
		//to that crop
		//-------------------------------------------------------------------------
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillCultivarsCombo();
			FillReportTypesCombo();
			DisplayCropTypeComponents();
			}
		//-------------------------------------------------------------------------
		//On the update of the grid cell, it runs a check to make sure that the 
		//values entered are valid
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
		//On the update of the grid cell, it runs a check to make sure that the 
		//values entered are valid
		//-------------------------------------------------------------------------
		private void grdIrrigation_UpdatingCell_1(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
		{
			if(e.Column.Key == "Amount" || e.Column.Key == "Efficency")
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

		private void btnCalculate_Click(object sender, System.EventArgs e)
		{
			SetFertileTillerNumber();
		}

		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
