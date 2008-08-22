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

namespace YP2006
{
	/// <summary>
	/// Summary description for wfPaddocksCrop.
	/// </summary>
	public class wfPaddocksCrop : System.Web.UI.Page
	{
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDate;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksRainfall;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksApplictions;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksSoil;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksCrop;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksInformation;
		protected System.Web.UI.WebControls.Panel pnlCanola;
		protected System.Web.UI.WebControls.CheckBox chkTriazine;
		protected System.Web.UI.WebControls.Panel pnlSorgum;
		protected System.Web.UI.WebControls.Label lblPopulation;
		protected System.Web.UI.WebControls.TextBox edtPopulation;
		protected System.Web.UI.WebControls.Label lblPopulationUnit;
		protected System.Web.UI.WebControls.Label lblRowConfiguration;
		protected System.Web.UI.WebControls.DropDownList cboRowConfiguration;
		protected System.Web.UI.WebControls.Label lblRowSpacing;
		protected System.Web.UI.WebControls.TextBox edtRowSpacing;
		protected System.Web.UI.WebControls.TextBox edtTiller;
		protected System.Web.UI.WebControls.Label lblTiller;
		protected System.Web.UI.WebControls.Button btnCalculate;
		protected System.Web.UI.WebControls.CheckBox chkAutoCalculate;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnit;
		protected System.Web.UI.WebControls.DropDownList cboCultivars;
		protected System.Web.UI.WebControls.Label lblCultivar;
		protected System.Web.UI.WebControls.Label lblCrop;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.CheckBox chkSown;
		protected Janus.Web.GridEX.GridEX grdSowDate;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnSaveTwo;
		protected System.Web.UI.WebControls.Button btnCancelTwo;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.ImageButton imgHelpCrop;
		protected System.Web.UI.WebControls.ImageButton imgHelpSowDate;
		protected System.Web.UI.WebControls.ImageButton imgHelpCultivar;
		protected System.Web.UI.WebControls.ImageButton btnHelpPaddockCropPage;
		protected System.Data.DataColumn dcSowDate;
	


		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			System.Threading.Thread.CurrentThread.CurrentCulture = System.Globalization.CultureInfo.CreateSpecificCulture("en-AU");
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
			this.dtSowDate = new System.Data.DataTable();
			this.dcSowDate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).BeginInit();
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksRainfall.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksApplictions.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksSoil.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksCrop.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksInformation.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnCalculate.Click += new System.EventHandler(this.btnCalculate_Click);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
			this.chkSown.CheckedChanged += new System.EventHandler(this.chkSown_CheckedChanged);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSaveTwo.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelTwo.Click += new System.EventHandler(this.btnCancel_Click);
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
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).EndInit();

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
					//Check to see if the auto fertile tiller number value from the db isn't null, 
					//if it isn't then set the value of the checkbox
					if(dtPaddockDetails.Rows[0]["AutoFTN"].ToString() != null &&
						dtPaddockDetails.Rows[0]["AutoFTN"].ToString() != "")
					{
						chkAutoCalculate.Checked = Convert.ToBoolean(Convert.ToInt32(dtPaddockDetails.Rows[0]["AutoFTN"].ToString()));
					}
					
					SetSowDate(szSowDate);
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();

					//Fills the cultivar combo box
					FillCultivarsCombo();
					//Fills the Report type combo		
					//Display Crop type dependant components
					DisplayCropTypeComponents();
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
					FillCropsCombo();
					FillRowConfigurationCombo();
					FillCultivarsCombo();
					SetSowDate(szSowDate);
					DisplayCropTypeComponents();
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
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void ChangeEnableCropDetails(bool bEnableCropDetails)
		{
			cboCrops.Enabled = bEnableCropDetails;
			cboCultivars.Enabled = bEnableCropDetails;
			grdSowDate.Enabled = bEnableCropDetails;
			if(bEnableCropDetails == false)
			{
				pnlCanola.Visible = bEnableCropDetails;
				pnlSorgum.Visible = bEnableCropDetails;
			}
			else
			{
				DisplayCropTypeComponents();
			}
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DisplayCropTypeComponents()
		{
			pnlCanola.Visible = false;
			pnlSorgum.Visible = false;

			switch(cboCrops.SelectedValue)
			{
				case "Sorghum":
					pnlSorgum.Visible = true;
					break;

				case "Canola":
					pnlCanola.Visible = true;
					break;

				default:
					pnlCanola.Visible = false;
					pnlSorgum.Visible = false;
					break;
			}
		}
		//-------------------------------------------------------------------------
		//The paddock details are updated, but firstly a check is run to determine
		//if the the sown check box is checked.  If it is, then the paddock is updated
		//with the details from the from.  If it hasn't been checked then the paddock
		//is updated with blank details.
		//-------------------------------------------------------------------------
		private void SaveCropDetails()
		{
			try
			{
				if(FunctionsClass.IsReadOnly() == false)
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
								if(chkAutoCalculate.Checked == true)
									FunctionsClass.SetFertileTillerNumber(FunctionsClass.GetActiveUserName(), 
										Session["SelectedPaddockName"].ToString(), edtTiller, FunctionsClass.ReturnPopulationValue(edtPopulation), 
										cboRowConfiguration.SelectedValue, 
										DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));

								DataAccessClass.UpdatePaddock((DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
									cboCultivars.SelectedItem.Text, Convert.ToInt32(chkTriazine.Checked), "", "", cboRowConfiguration.SelectedValue, -1, "", "", 
									FunctionsClass.ReturnPopulationValue(edtPopulation), -1, InputValidationClass.ReturnTextBoxValueAsDouble(edtTiller, 0), 
									InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacing, 0), Convert.ToInt32(chkAutoCalculate.Checked), 
									-1, Session["SelectedPaddockName"].ToString(), Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
							}
							//If no cultivar is selected display an error to the user
							else
								throw new Exception("Please ensure that all fields contain data");
						}
						//If the sown check box hasn't been checked, the paddock is updated with default
						//settings.
						else
						{
							DataAccessClass.ResetPaddock(Session["SelectedPaddockName"].ToString(), Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
						}
				}
				else
					throw new Exception(FunctionsClass.ReturnReadOnlyMessage());
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
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();

				FillForm();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
				HelpClass.SetHelpForPaddockCropPage(imgHelpSowDate, imgHelpCrop, imgHelpCultivar, 
					btnHelpPaddockCropPage);
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
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			DisplayCropTypeComponents();
			FillCultivarsCombo();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void chkSown_CheckedChanged(object sender, System.EventArgs e)
		{
			ChangeEnableCropDetails(chkSown.Checked);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
		SaveCropDetails();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
		Server.Transfer("wfPaddocksCrop.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCalculate_Click(object sender, System.EventArgs e)
		{
			FunctionsClass.SetFertileTillerNumber(FunctionsClass.GetActiveUserName(), 
				Session["SelectedPaddockName"].ToString(), edtTiller, 
				FunctionsClass.ReturnPopulationValue(edtPopulation), 
				cboRowConfiguration.SelectedValue, 
				DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null));
		}
		//-------------------------------------------------------------------------
		#endregion

	}//END OF CLASS
}//END OF NAMESPACE
