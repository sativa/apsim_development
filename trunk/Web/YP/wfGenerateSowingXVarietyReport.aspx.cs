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
	/// Summary description for wfGenerateSowingXVarietyReport.
	/// </summary>
	public class wfGenerateSowingXVarietyReport : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.Label lblCrop;
		protected System.Web.UI.WebControls.Label lblNitrogen;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Web.UI.WebControls.DropDownList cboVarietyOne;
		protected System.Web.UI.WebControls.Label lblVarietyOne;
		protected System.Web.UI.WebControls.DropDownList cboVarietyTwo;
		protected System.Web.UI.WebControls.DropDownList cboVarietyThree;
		protected System.Web.UI.WebControls.Label lblVarietyTwo;
		protected System.Web.UI.WebControls.Label lblVarietyThree;
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDate;
		protected System.Data.DataColumn dcSowDate;
		protected Janus.Web.GridEX.GridEX grdSowDateOne;
		protected System.Web.UI.WebControls.Label lblSowingDateOne;
		protected Janus.Web.GridEX.GridEX grdSowDateTwo;
		protected Janus.Web.GridEX.GridEX grdSowDateThree;
		protected Janus.Web.GridEX.GridEX grdNitrogen;
		protected System.Web.UI.WebControls.Label lblSowingDateTwo;
		protected System.Web.UI.WebControls.Label lblSowingDateThree;
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
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.grdNitrogen.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdNitrogen_UpdatingCell);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
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
		//
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			InitialiseGrids();
			SetSowDate();
			FillCropsCombo();
			FillCultivarsCombo();
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
		//Intitialise the grids to contain 5 blank rows
		//-------------------------------------------------------------------------
		private void InitialiseGrids()
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
				cboVarietyOne.DataSource = dtCultivarList;
				cboVarietyOne.DataTextField = "Type";
				cboVarietyOne.DataValueField = "Type";
				cboVarietyOne.DataBind();

				cboVarietyTwo.DataSource = dtCultivarList;
				cboVarietyTwo.DataTextField = "Type";
				cboVarietyTwo.DataValueField = "Type";
				cboVarietyTwo.DataBind();

				cboVarietyThree.DataSource = dtCultivarList;
				cboVarietyThree.DataTextField = "Type";
				cboVarietyThree.DataValueField = "Type";
				cboVarietyThree.DataBind();
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
						if(cboCrops.SelectedValue != "None" && cboVarietyOne.SelectedValue != "" &&
							cboVarietyOne.SelectedValue != "None")
							{
							if(grdSowDateOne.GetRow(0).Cells["SowDate"].Text != "" && 
								grdSowDateTwo.GetRow(0).Cells["SowDate"].Text != "" &&
								grdSowDateThree.GetRow(0).Cells["SowDate"].Text != "")
								{
								DataTable dtPaddockDetails = 
									DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
									FunctionsClass.GetActiveUserName());
								if(dtPaddockDetails.Rows.Count > 0)
									{
									string szCropType = dtPaddockDetails.Rows[0]["CropType"].ToString();
									//Generate a data table that stores the values particular to the Sow X Variety report
									DataTable dtOtherValues = 
										ReportClass.CreateSowingXVarietyReportOtherValues(ReturnScenarioDataTable(grdNitrogen), 
										cboVarietyOne.SelectedValue, (DateTime.ParseExact(grdSowDateOne.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
										cboVarietyTwo.SelectedValue, (DateTime.ParseExact(grdSowDateTwo.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
										cboVarietyThree.SelectedValue, (DateTime.ParseExact(grdSowDateThree.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"));
									//Generate the files needed to generate a report and then email these files to the ApsimRun machine
									if(EmailClass.SendReportEmail(edtReportName.Text, szCropType, 
										ViewState["ReportType"].ToString(), (bool)ViewState["EmailConParFiles"], dtOtherValues) == true)
										{
										Server.Transfer("wfReportGenerated.aspx");
										}
									else
										throw new Exception("Error requesting report");
									}
								else
									throw new Exception("Can not access crop type");
								}
							else
								throw new Exception("Please ensure all sowing date fields contain a date");
							}
						else
							throw new Exception("Please select a crop type and variety type");
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
		//When the user selects a different crop type from the crops combo box
		//the cultivar combo box is updated with the corresponding cultivars linked
		//to that crop
		//-------------------------------------------------------------------------
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillCultivarsCombo();
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
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
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
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			GenerateReport();
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
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
