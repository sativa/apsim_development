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
	/// Summary description for wfGenerateNitrogenComparisonReport.
	/// </summary>
	public class wfGenerateNitrogenComparisonReport : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected Janus.Web.GridEX.GridEX grdScenarioOne;
		protected Janus.Web.GridEX.GridEX grdScenarioTwo;
		protected Janus.Web.GridEX.GridEX grdScenarioThree;
		protected System.Web.UI.WebControls.Label lblScenarioOneApplications;
		protected System.Web.UI.WebControls.Label lblScenarioTwoApplications;
		protected System.Web.UI.WebControls.Label lblScenarioThreeApplications;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		

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
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
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
			this.dcID.Caption = "ID";
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
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();

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
				}
			catch(Exception)
				{}
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
			if(edtReportName.Text != "")
				{
				if(InputValidationClass.IsInputAValidFileLocationString(edtReportName.Text) == true)
					{
					DataTable dtOtherValues = 
						ReportClass.CreateNitrogenComparisonOtherValues(ReturnScenarioDataTable(grdScenarioOne), 
						ReturnScenarioDataTable(grdScenarioTwo), ReturnScenarioDataTable(grdScenarioThree));

					if(EmailClass.SendReportEmail(edtReportName.Text, 
						ViewState["ReportType"].ToString(), (bool)ViewState["EmailConParFiles"], dtOtherValues) == true)
						{
						Server.Transfer("wfEditPaddock.aspx");
						}
					else
						{
						FunctionsClass.DisplayMessage(Page, "Error requesting report");
						}
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "Report Description contains invalid characters. Please remove any of the following characters \\\\ / : * ? \" < > |");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page,"Please enter a report name");
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
				FunctionsClass.SetControlFocus("edtReportName", this);
				InitialiseGrids();
				StoreReportSelection();
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


		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
