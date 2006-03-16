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
	/// Summary description for wfGenerateNitrogenProfitReport.
	/// </summary>
	public class wfGenerateNitrogenProfitReport : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Web.UI.WebControls.Label lblScenarioOneApplications;
		protected System.Web.UI.WebControls.Label lblScenarioTwoApplications;
		protected System.Web.UI.WebControls.Label lblScenarioThreeApplications;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable Nitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected Janus.Web.GridEX.GridEX grdScenarioOne;
		protected Janus.Web.GridEX.GridEX grdScenarioTwo;
		protected Janus.Web.GridEX.GridEX grdScenarioThree;
		protected System.Web.UI.WebControls.Label lblClassification;
		protected System.Web.UI.WebControls.Label lblPrice;
		protected System.Web.UI.WebControls.Label lblProteinContent;
		protected System.Web.UI.WebControls.Label lblProteinIncrement;
		protected System.Web.UI.WebControls.Label lblFertiliserCost;
		protected System.Web.UI.WebControls.Label lblApplicationCost;
		protected System.Web.UI.WebControls.Label lblPriceUnit;
		protected System.Web.UI.WebControls.Label lblProteinContentUnit;
		protected System.Web.UI.WebControls.Label lblProteinIncrementUnit;
		protected System.Web.UI.WebControls.Label lblFertiliserCostUnit;
		protected System.Web.UI.WebControls.Label lblApplicationCostUnit;
		protected System.Web.UI.WebControls.Label lblProteinIncrementKey;
		protected System.Web.UI.WebControls.TextBox edtPrice;
		protected System.Web.UI.WebControls.TextBox edtProteinContent;
		protected System.Web.UI.WebControls.TextBox edtProteinIncrement;
		protected System.Web.UI.WebControls.TextBox edtFertiliserCost;
		protected System.Web.UI.WebControls.TextBox edtApplicationCost;
		protected System.Web.UI.WebControls.DropDownList cboClassification;
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
			this.Nitrogen = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcApplicationDate = new System.Data.DataColumn();
			this.dcRate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.Nitrogen)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.grdScenarioOne.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdScenarioTwo.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdScenarioThree.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			// 
			// dsNitrogen
			// 
			this.dsNitrogen.DataSetName = "NewDataSet";
			this.dsNitrogen.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsNitrogen.Tables.AddRange(new System.Data.DataTable[] {
																																		this.Nitrogen});
			// 
			// Nitrogen
			// 
			this.Nitrogen.Columns.AddRange(new System.Data.DataColumn[] {
																																		this.dcID,
																																		this.dcApplicationDate,
																																		this.dcRate});
			this.Nitrogen.TableName = "Nitrogen";
			// 
			// dcID
			// 
			this.dcID.ColumnName = "ID";
			// 
			// dcApplicationDate
			// 
			this.dcApplicationDate.Caption = "ApplicationDate";
			this.dcApplicationDate.ColumnName = "ApplicationDate";
			this.dcApplicationDate.DataType = typeof(System.DateTime);
			// 
			// dcRate
			// 
			this.dcRate.ColumnName = "Rate";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.Nitrogen)).EndInit();

		}
		#endregion

		
		#region Form Functions
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
		//Initialise the form for the selected crop variety
		//-------------------------------------------------------------------------
		private void InitialiseForm()
			{
			try
				{
				DataTable dtPaddockDetails = 
					DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				if(dtPaddockDetails.Rows.Count > 0)
					{
					string szCropType = dtPaddockDetails.Rows[0]["CropType"].ToString();
					cboClassification.Items.Clear();
					if(szCropType == "Wheat")
						{
						cboClassification.Items.Add("APH");
						cboClassification.Items.Add("AH");
						cboClassification.Items.Add("APW");
						cboClassification.Items.Add("ASW");
						cboClassification.Items.Add("ASW");
						cboClassification.Items.Add("Durum");
						}
					else if(szCropType == "Barley")
						{
						cboClassification.Items.Add("Feed");
						cboClassification.Items.Add("Malt");
						edtProteinIncrement.Enabled = false;
						edtProteinIncrement.Text  = "0";
						}
					}
				else
					throw new Exception("Can not access paddock details");
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
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
		//A report is generated and sent to the apsim run machine
		//-------------------------------------------------------------------------
		private void GenerateReport()
		{
			try
				{
				string szReportName = edtReportName.Text;
				szReportName = szReportName.Trim();
				//Check that their is a report name
				if(szReportName != "")
					{
					//Check that the name of the report won't cause a problem when it is stored in the file system
					if(InputValidationClass.IsInputAValidFileLocationString(szReportName) == true)
						{
						if(InputValidationClass.IsInputAPositiveDecimal(edtPrice.Text) &&
							InputValidationClass.IsInputAPositiveDecimal(edtProteinContent.Text) &&
							InputValidationClass.IsInputADecimal(edtProteinIncrement.Text) && 
							InputValidationClass.IsInputAPositiveDecimal(edtFertiliserCost.Text) && 
							InputValidationClass.IsInputAPositiveDecimal(edtApplicationCost.Text) == true)
							{
							DataTable dtPaddockDetails = 
								DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
								FunctionsClass.GetActiveUserName());
							if(dtPaddockDetails.Rows.Count > 0)
							{
								string szCropType = dtPaddockDetails.Rows[0]["CropType"].ToString();
								//Generate a data table that stores the values particular to the nitrogen report
								DataTable dtOtherValues = 
									ReportClass.CreateNitrogenProfitOtherValues(
									cboClassification.SelectedValue,
									edtPrice.Text, edtProteinContent.Text, 
									edtProteinIncrement.Text, edtFertiliserCost.Text, edtApplicationCost.Text, 
									ReturnScenarioDataTable(grdScenarioOne), ReturnScenarioDataTable(grdScenarioTwo), 
									ReturnScenarioDataTable(grdScenarioThree));
								string szReportXML = 
									ReportClass.PrepareNitrogenProfitXML(edtReportName.Text, ViewState["ReportType"].ToString(),
									cboClassification.SelectedValue, edtPrice.Text, edtProteinContent.Text, 
									edtProteinIncrement.Text, edtFertiliserCost.Text, edtApplicationCost.Text,
									grdScenarioOne, grdScenarioTwo, grdScenarioThree);
								//Generate the files needed to generate a report and then email these files to the ApsimRun machine
								if(EmailClass.SendReportEmail(szReportName, szCropType, 
									ViewState["ReportType"].ToString(), (bool)ViewState["EmailConParFiles"], szReportXML, dtOtherValues) == true)
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
							throw new Exception("One or more fields does not contain a valid number");
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
		//Sets the page up and stores the values need to generate the report
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if (!IsPostBack)
			{	
				//View state is used to store values over post back events
				ViewState["ReportTypeID"] = "0";
				ViewState["ReportType"] = "";
				ViewState["EmailConParFiles"] = false;
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				InitialiseForm();
				InitialiseGrids();
				StoreReportSelection();
				FunctionsClass.SetControlFocus("edtReportName", this);
				btnSave.Style.Add("cursor", "hand");
			}
		}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button they are transfered back to the 
		//edit paddock page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfEditPaddock.aspx");
		}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image they are transfered back to the 
		//edit paddock page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
		{
			Server.Transfer("wfEditPaddock.aspx");
		}
		//-------------------------------------------------------------------------
		//When the user presses the save button the report is generated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			GenerateReport();
		}
		//-------------------------------------------------------------------------
		//When the user presses the save image the report is generated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
		{
			GenerateReport();
		}
		//-------------------------------------------------------------------------
		//On the update of the grid cell, it runs a check to make sure that the 
		//values entered are valid
		//-------------------------------------------------------------------------
		private void grd_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
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


		}//END OF CLASS
	}//END OF NAMESPACE
