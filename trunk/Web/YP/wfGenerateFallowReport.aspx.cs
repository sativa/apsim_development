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
	/// Summary description for wfGenerateFallowReport.
	/// </summary>
	public class wfGenerateFallowReport : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.DropDownList cboVariety;
		protected System.Web.UI.WebControls.Label lblVariety;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.Label lblCrop;
		protected System.Web.UI.WebControls.Label lblSowingDate;
		protected System.Web.UI.WebControls.Label lblNitrogen;
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDate;
		protected System.Data.DataColumn dcSowDate;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected Janus.Web.GridEX.GridEX grdNitrogen;
		protected Janus.Web.GridEX.GridEX grdSowDate;
		protected System.Web.UI.WebControls.Label lblPopulation;
		protected System.Web.UI.WebControls.TextBox edtTiller;
		protected System.Web.UI.WebControls.TextBox edtRowSpacing;
		protected System.Web.UI.WebControls.DropDownList cboRowConfiguration;
		protected System.Web.UI.WebControls.TextBox edtPopulation;
		protected System.Web.UI.WebControls.Label lblPopulationUnit;
		protected System.Web.UI.WebControls.Label lblRowConfiguration;
		protected System.Web.UI.WebControls.Label lblRowSpacing;
		protected System.Web.UI.WebControls.Label lblTiller;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnit;
		protected System.Web.UI.WebControls.Button btnRefresh;
		protected System.Web.UI.WebControls.CheckBox chkAutoCalculate;
		protected System.Web.UI.WebControls.Panel pnlTop;
	


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
			this.dsSowDate = new System.Data.DataSet();
			this.dtSowDate = new System.Data.DataTable();
			this.dcSowDate = new System.Data.DataColumn();
			this.dsNitrogen = new System.Data.DataSet();
			this.dtNitrogen = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcApplicationDate = new System.Data.DataColumn();
			this.dcRate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
			this.grdNitrogen.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdNitrogen_UpdatingCell);
			this.chkAutoCalculate.CheckedChanged += new System.EventHandler(this.chkAutoCalculate_CheckedChanged);
			this.btnRefresh.Click += new System.EventHandler(this.btnRefresh_Click);
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
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			InitialiseGrid();
			SetSowDate();
			FillCropsCombo();
			FillCultivarsCombo();
			FillRowConfigurationCombo();
			SetVisibilityOfSorgumComponents();
		}
		//-------------------------------------------------------------------------
		//Stores the report type selection from the previous page in view state
		//variables.
		//-------------------------------------------------------------------------
		private void StoreReportSelection()
			{
			try
				{
				wfEditPaddock EditPaddock = (wfEditPaddock) Context.Handler;
				ViewState["ReportType"] = EditPaddock.ReturnReportType();
				ViewState["EmailConParFiles"] = EditPaddock.ReturnEmailConParFiles();
				edtReportName.Text = Session["SelectedPaddockName"].ToString() +" "+ ViewState["ReportType"].ToString();
				}
			catch(Exception)
				{}
			}	
		//-------------------------------------------------------------------------
		//Set the date shown in the sow date grid
		//-------------------------------------------------------------------------
		private void SetSowDate()
		{
			DataRow drSowDate;
			drSowDate = dsSowDate.Tables["SowDate"].NewRow();
			drSowDate["SowDate"] = DateTime.Today;
			dsSowDate.Tables["SowDate"].Rows.Add(drSowDate);
			this.DataBind();
		}
		//-------------------------------------------------------------------------
		//Intitialise the nitrogen grid to contain 5 blank rows
		//-------------------------------------------------------------------------
		private void InitialiseGrid()
		{
			DataRow drNitrogen;
			int iMaxNumberOfRows = 5;
			for(int iIndex = dsNitrogen.Tables["Nitrogen"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
			{
				drNitrogen = dsNitrogen.Tables["Nitrogen"].NewRow();
				drNitrogen["ID"] = 0;	
				dsNitrogen.Tables["Nitrogen"].Rows.Add(drNitrogen);
			}
			this.DataBind();
		}
		//-------------------------------------------------------------------------
		//Gets all the crops the database and fills the crops combo box with them
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
		{
			DataTable dtCropList = DataAccessClass.GetUsersCrops(FunctionsClass.GetActiveUserName());
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
			if(cboCrops.SelectedValue != "")
				{
				DataTable dtCultivarList = DataAccessClass.GetAllCultivarsOfCrop(cboCrops.SelectedValue);
				cboVariety.DataSource = dtCultivarList;
				cboVariety.DataTextField = "Type";
				cboVariety.DataValueField = "Type";
				cboVariety.DataBind();
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
		//A report is generated and sent to the apsim run machine
		//-------------------------------------------------------------------------
		private void GenerateReport()
			{
			try
				{
				//Check that their is a report name
				if(edtReportName.Text != "")
					{
					//Check that the name of the report won't cause a problem when it is stored in the file system
					if(InputValidationClass.IsInputAValidFileLocationString(edtReportName.Text) == true)
						{
						//Check that a valid crop is selected
						if(cboCrops.SelectedValue != "None" && cboVariety.SelectedValue != "" &&
							cboVariety.SelectedValue != "None")
							{
							if(grdSowDate.GetRow(0).Cells["SowDate"].Text != "")
								{
								if(chkAutoCalculate.Checked == true)
									SetFertileTillerNumber();

								//Generate a data table that stores the values particular to the Sow X Variety report
								DataTable dtOtherValues = 
									ReportClass.CreateFallowReportOtherValues(ReturnScenarioDataTable(grdNitrogen), 
									cboVariety.SelectedValue, (DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"));
								string szReportXML = 
									ReportClass.PrepareFallowXML(edtReportName.Text, ViewState["ReportType"].ToString(),
									cboVariety.SelectedValue, grdSowDate.GetRow(0).Cells["SowDate"].Text, 
									cboCrops.SelectedValue, cboRowConfiguration.SelectedValue, 
									InputValidationClass.ReturnTextBoxValueAsInteger(edtPopulation, 0), InputValidationClass.ReturnTextBoxValueAsDouble(edtTiller, 0), 
									InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacing, 0), grdNitrogen);
								//Generate the files needed to generate a report and then email these files to the ApsimRun machine
								if(EmailClass.SendReportEmail(edtReportName.Text, cboCrops.SelectedValue,  
									ViewState["ReportType"].ToString(), (bool)ViewState["EmailConParFiles"], szReportXML, dtOtherValues) == true)
									{
									Server.Transfer("wfReportGenerated.aspx");
									}
								else
									throw new Exception("Error requesting report");
								}
							else
								throw new Exception("Please ensure all sowing date fields contain a date");
							}
						else
							throw new Exception("Please select a crop type and a variety type");
						}
					else
						throw new Exception("Report Description contains invalid characters. Please remove any of the following characters \\\\ / : * \" ? \\' # < > |");
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
		//Returns the data from the selected grid in the form of a datatable
		//-------------------------------------------------------------------------
		private DataTable ReturnScenarioDataTable(Janus.Web.GridEX.GridEX grdSelectedGrid)
			{
			DataTable dtNitrogen = dsNitrogen.Tables["Nitrogen"].Copy();
			DataRow drNitrogen;
			try
				{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdSelectedGrid.RowCount; iIndex++)
					{	
					grdRow = grdSelectedGrid.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["ApplicationDate"].Value != null && grdRow.Cells["Rate"].Value != null)
					{		
						drNitrogen = dtNitrogen.NewRow();
						drNitrogen["ApplicationDate"] = grdRow.Cells["ApplicationDate"].Text;
						drNitrogen["Rate"] = grdRow.Cells["Rate"].Text;
						dtNitrogen.Rows.Add(drNitrogen);
						}
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Error storing scenario data");
				}
			return dtNitrogen;
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
			lblRowSpacing.Visible  = bSorgumComponentVisibility;
			btnRefresh.Visible = bSorgumComponentVisibility;
			edtRowSpacing.Visible = bSorgumComponentVisibility;
			lblRowSpacingUnit.Visible = bSorgumComponentVisibility;
			chkAutoCalculate.Visible = bSorgumComponentVisibility;
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
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{	
				ViewState["ReportTypeID"] = "0";
				ViewState["ReportType"] = "";
				ViewState["EmailConParFiles"] = false;
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				FunctionsClass.SetControlFocus("edtReportName", this);
				FillForm();
				StoreReportSelection();
				btnSave.Style.Add("cursor", "hand");
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillCultivarsCombo();
			SetVisibilityOfSorgumComponents();
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			GenerateReport();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			GenerateReport();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
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
		private void chkAutoCalculate_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkAutoCalculate.Checked == true)
			{
				edtTiller.Enabled = false;
			}
			else
			{
				edtTiller.Enabled = true;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnRefresh_Click(object sender, System.EventArgs e)
		{
			SetFertileTillerNumber();
		}

		//-------------------------------------------------------------------------
		#endregion

		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
