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
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).BeginInit();
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
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();
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
			DisplayGrowersName();
			FillReportTypesCombo();
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
					//Enables all the components on the form
					ChangeEnableCropDetails(true);
					//Sets the selected crop to the crop returned from the database		
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();
					//Set the sow date on the calander
					chkSown.Checked = true;
					SetSowDate(szSowDate);
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();
					//Fills the cultivar combo box
					FillCultivarsCombo();
					//Sets up and fills the nitrogen application grid
					FillNitrogenApplicationGrid();
					//Restores the selected crop to the crop returned from the database	
					cboCrops.SelectedValue = dtPaddockDetails.Rows[0]["CropType"].ToString();
					//Sets the selected cultivar to the cultivar returned from the database
					cboCultivars.SelectedValue = dtPaddockDetails.Rows[0]["CultivarType"].ToString();
					}
				//If no sow date has been saved, then initialise the form.
				else
					{
					ChangeEnableCropDetails(false);
					FillNitrogenApplicationGrid();
					FillCropsCombo();
					FillCultivarsCombo();
					SetSowDate(szSowDate);
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
				if(dtPaddockDetails.Rows[0]["LinkedRainfallPaddockName"].ToString() != "")
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
				lblName.Text = dtUsersDetails.Rows[0]["Name"].ToString()+ 
					" and paddock:  "+Session["SelectedPaddockName"].ToString();
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
			DataTable dtReport = DataAccessClass.GetAllReportTypes("ApsimReport");
			cboReport.DataSource = dtReport;
			cboReport.DataTextField = "Type";
			cboReport.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets all the crops the database and fills the crops combo box with them
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
			{
			DataTable dtCropList = DataAccessClass.GetAllCrops();
			cboCrops.DataSource = dtCropList;
			cboCrops.DataTextField = "Type";
			cboCrops.DataValueField = "Type";
			cboCrops.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets all the cultivars for the selected crop and fills the cultivars
		//combo box with them
		//-------------------------------------------------------------------------
		private void FillCultivarsCombo()
			{
			if(cboCrops.SelectedItem.Text != "")
				{
				DataTable dtCultivarList = DataAccessClass.GetAllCultivarsOfCrop(cboCrops.SelectedItem.Text);
				cboCultivars.DataSource = dtCultivarList;
				cboCultivars.DataTextField = "Type";
				cboCultivars.DataValueField = "Type";
				cboCultivars.DataBind();
				}
			}
		//-------------------------------------------------------------------------
		//Fills the nitrogen application grid with the vales stored in the database
		//-------------------------------------------------------------------------	
		private void FillNitrogenApplicationGrid()
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
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				try
					{
					//If the sown check box is checked the paddock record is updated with 
					//the details from the form
					if(chkSown.Checked == true)
						{
						//If a cultivar is selected, update the paddock
						if( cboCultivars.SelectedItem.Text != "" && 
							cboCultivars.SelectedItem.Text != "None" && 
							grdSowDate.GetRow(0).Cells["SowDate"].Text != "")
							{
							DataAccessClass.UpdatePaddock((DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
								cboCultivars.SelectedItem.Text, "", "", "", "", "", Session["SelectedPaddockName"].ToString(), 
								FunctionsClass.GetActiveUserName());
							SaveNitrogenApplications();
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
						DataAccessClass.ResetPaddock(Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
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
		//If the user isn't a visitor then then the needed details are stored
		//in session variables and the user is sent to the generate report page
		//-------------------------------------------------------------------------			
		private void SendToGenerateReportPage()
			{
			//If the user isn't a visitor
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				try
					{
					if(SavePaddockDetails() == true)
						{
						if(SoilSampleClass.IsSampleValid(Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName()))
							{
							//Checks that the report type is selected
							if(cboReport.SelectedItem.Text != "")
								{
								if(ReportClass.DoesUsersReportDirectoryExisit(FunctionsClass.GetActiveUserName(), DateTime.Today.Year) == false)
									{
									ReportClass.CreateUsersReportDirectory(FunctionsClass.GetActiveUserName(), DateTime.Today.Year); 
									}
								//If the report is a nitrogen report send them to the nitrogen report
								//generation page, other wise send them to the default report 
								//generation page
								if(cboReport.SelectedItem.Text == ReportClass.szNitrogenComparisonReport)
									{
									if(chkSown.Checked == true)
										{
										Server.Transfer("wfGenerateNitrogenComparisonReport.aspx");
										}
									else
										{
										FunctionsClass.DisplayMessage(Page, "This report requires a sowing date");
										}
									}
								else
									{
									if(chkSown.Checked == true)
										{
										Server.Transfer("wfGenerateReport.aspx");
										}
									else
										{
										FunctionsClass.DisplayMessage(Page, "This report requires a sowing date");
										}
									}
								}
							else
								{
								FunctionsClass.DisplayMessage(Page, "Please select a report type");
								}
							}
						else
							{
							FunctionsClass.DisplayMessage(Page, "Please visit the paddock setup page and set the initial water and nitrogen conditions.");
							}
						}
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
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
			SavePaddockDetails();
			SendToGenerateReportPage();
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
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
