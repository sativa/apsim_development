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
using Janus.Web.GridEX;

namespace UrbanWater
	{
	/// <summary>
	/// Summary description for wfUrbanWater.
	/// </summary>
	public class wfUrbanWater : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Image imgLogo;
		protected System.Web.UI.WebControls.Panel pnlFiller;
		protected System.Web.UI.WebControls.Panel pnlHeader;
		protected System.Web.UI.WebControls.Label lblLocation;
		protected System.Web.UI.WebControls.Label lblSoilType;
		protected System.Web.UI.WebControls.Label lblPlantType;
		protected System.Web.UI.WebControls.Label lblWaterings;
		protected System.Data.DataSet dsWaterings;
		protected System.Data.DataTable dtWatering;
		protected System.Data.DataColumn dcID;
		protected System.Web.UI.WebControls.Label lblDisplayResults;
		protected System.Web.UI.WebControls.Button btnDisplayResults;
		protected System.Data.DataColumn dcWateringDate;
		protected System.Data.DataColumn dcWateringAmount;
		protected Janus.Web.GridEX.GridEX grdWaterings;
		protected System.Web.UI.WebControls.DropDownList cboPlantTypes;
		protected System.Web.UI.WebControls.DropDownList cboSoilTypes;
		protected System.Web.UI.WebControls.DropDownList cboLocations;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.DropDownList cboPeriod;
		protected System.Web.UI.WebControls.Image imgSideBar;
	

		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			//Sets the grid to display in the Australian Short Date format, regardless of
			//the users settings.
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
			this.dsWaterings = new System.Data.DataSet();
			this.dtWatering = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcWateringDate = new System.Data.DataColumn();
			this.dcWateringAmount = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsWaterings)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtWatering)).BeginInit();
			this.btnDisplayResults.Click += new System.EventHandler(this.btnDisplayResults_Click);
			this.grdWaterings.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdWaterings_UpdatingCell);
			// 
			// dsWaterings
			// 
			this.dsWaterings.DataSetName = "NewDataSet";
			this.dsWaterings.Locale = new System.Globalization.CultureInfo("en-US");
			this.dsWaterings.Tables.AddRange(new System.Data.DataTable[] {
																																		 this.dtWatering});
			// 
			// dtWatering
			// 
			this.dtWatering.Columns.AddRange(new System.Data.DataColumn[] {
																																			this.dcID,
																																			this.dcWateringDate,
																																			this.dcWateringAmount});
			this.dtWatering.TableName = "Watering";
			// 
			// dcID
			// 
			this.dcID.Caption = "ID";
			this.dcID.ColumnName = "ID";
			// 
			// dcWateringDate
			// 
			this.dcWateringDate.ColumnName = "WateringDate";
			// 
			// dcWateringAmount
			// 
			this.dcWateringAmount.ColumnName = "WateringAmount";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsWaterings)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtWatering)).EndInit();

		}
		#endregion
		
		
		
		#region Setting up the form
		//-------------------------------------------------------------------------
		//Sets up the form to display all the data needed by the user
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			InitialiseWateringTable();
			FillLocationsCombo();
			FillSoilTypesCombo();
			FillPlantTypesCombo();
		}
		//-------------------------------------------------------------------------
		//Adds five blank records to to the watering grid
		//-------------------------------------------------------------------------
		private void InitialiseWateringTable()
			{
			DataRow drWatering;
			int iMaxWateringRecords = 5;
			for(int iIndex = 0; iIndex < iMaxWateringRecords; iIndex++)
				{
				drWatering = dsWaterings.Tables["Watering"].NewRow();
				drWatering["ID"] = 0;
				dsWaterings.Tables["Watering"].Rows.Add(drWatering);
				}
				this.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets all the locations the database and fills the locations combo box with them
		//-------------------------------------------------------------------------
		private void FillLocationsCombo()
			{
			DataTable dtLocations = DataAccessClass.GetAllLocations();
			cboLocations.DataSource = dtLocations;
			cboLocations.DataTextField = "Name";
			cboLocations.DataValueField = "ID";
			cboLocations.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets all the soil types the database and fills the soil types combo box with them
		//-------------------------------------------------------------------------
		private void FillSoilTypesCombo()
			{
			DataTable dtSoilTypes = DataAccessClass.GetAllSoilTypes();
			cboSoilTypes.DataSource = dtSoilTypes;
			cboSoilTypes.DataTextField = "Name";
			cboSoilTypes.DataValueField = "ID";
			cboSoilTypes.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets all the plant types the database and fills the plant types combo box with them
		//-------------------------------------------------------------------------
		private void FillPlantTypesCombo()
			{
			DataTable dtPlantTypes = DataAccessClass.GetAllPlantTypes();
			cboPlantTypes.DataSource = dtPlantTypes;
			cboPlantTypes.DataTextField = "Name";
			cboPlantTypes.DataValueField = "ID";
			cboPlantTypes.DataBind();
			}
		#endregion
		
		
		
		#region Store Form Details
		//-------------------------------------------------------------------------
		//Saves the selected location, plant type and soil type in their respective
		//session variables
		//-------------------------------------------------------------------------
		private void StoreVariables()
			{
			if(cboLocations.SelectedValue != "")
				{
				Session["LocationID"] = cboLocations.SelectedValue;
				}
			if(cboPlantTypes.SelectedValue != "")
				{
				Session["PlantTypeID"] = cboPlantTypes.SelectedValue;
				}
			if(cboSoilTypes.SelectedValue != "")
				{
				Session["SoilTypeID"] = cboSoilTypes.SelectedValue;
				}
			StoreTimePeriod();
			}
		//-------------------------------------------------------------------------
		//Stores the start and end date for the selected period.
		//The end date is set to be the last date found in the met file for the 
		//selected location.  The start date is then caculated from this end date.
		//-------------------------------------------------------------------------	
		private void StoreTimePeriod()
		{
		DateTime dtEndDate = MetCalculations.ReturnLastDateInMetFile(DataAccessClass.GetLocationFileName(cboLocations.SelectedValue));
		try
		{
			Session["EndDate"] = dtEndDate.ToString("dd/MM/yyyy");
			if(cboPeriod.SelectedValue == "1W")
				{
				Session["StartDate"] = dtEndDate.AddDays(-7).ToString("dd/MM/yyyy");
				}
			else if(cboPeriod.SelectedValue == "2W")
				{
					Session["StartDate"] = dtEndDate.AddDays(-14).ToString("dd/MM/yyyy");
				}
			else if(cboPeriod.SelectedValue == "3W")
			{
				Session["StartDate"] = dtEndDate.AddDays(-21).ToString("dd/MM/yyyy");
			}
			else if(cboPeriod.SelectedValue == "1M")
			{
				Session["StartDate"] = dtEndDate.AddMonths(-1).ToString("dd/MM/yyyy");
			}
			else if(cboPeriod.SelectedValue == "2M")
				{
				Session["StartDate"] = dtEndDate.AddMonths(-2).ToString("dd/MM/yyyy");
				}
			else if(cboPeriod.SelectedValue == "3M")
				{
				Session["StartDate"] = dtEndDate.AddMonths(-3).ToString("dd/MM/yyyy");
				}
			else
				{
				Session["StartDate"] = dtEndDate.AddMonths(-1).ToString("dd/MM/yyyy");
				}
			}
		catch(Exception)
			{}
		}
		//-------------------------------------------------------------------------
		//Returns the watering values entered into the grid by the user as a DataTable
		//-------------------------------------------------------------------------
		public DataTable ReturnWateringDataTable()
			{
			DataTable dtWaterings = new DataTable("WateringDataTable");
			dtWaterings.Columns.Add("Date", System.Type.GetType("System.String"));
			dtWaterings.Columns.Add("Amount", System.Type.GetType("System.Double"));
			try
				{
				dtWaterings.Rows.Clear();
				DataRow drWatering;
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdWaterings.RowCount; iIndex++)
					{
					grdRow = grdWaterings.GetRow(iIndex);
					//If there is data in the grid then save it to the viewstate
					if(grdRow.Cells["Date"].Value != null && grdRow.Cells["Amount"].Value != null)
						{
						drWatering = dtWaterings.NewRow();
						drWatering["Date"] = grdRow.Cells["Date"].Text;
						drWatering["Amount"] = grdRow.Cells["Amount"].Text;
						dtWaterings.Rows.Add(drWatering);
						}
					}
				}
			catch(Exception)
				{}
			return dtWaterings;
			}
		
		#endregion

		
		
		#region Form Events
		//-------------------------------------------------------------------------
		//If this is the first time the page is loaded, then initialise the page
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{
				FillForm();
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the display results button the combobox selections
		//are stored and the user is transferred to the display results page
		//-------------------------------------------------------------------------
		private void btnDisplayResults_Click(object sender, System.EventArgs e)
			{
			StoreVariables();
			Server.Transfer("wfDisplayResults.aspx");
			}
		//-------------------------------------------------------------------------
		//When the cells are updated, a check is run on the watering amount cell
		//to ensure it is a positive decimal, if it isn't then it is reset to zero
		//-------------------------------------------------------------------------
		private void grdWaterings_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
			{
			if (e.Column.Key == "Amount")
				{
				if(InputValidationClass.IsInputAPositiveDecimal(e.Value.ToString()) == false)
					{
					e.Value = "0";
					}
				}
			}
		//-------------------------------------------------------------------------
		#endregion
		

		//-------------------------------------------------------------------------	
		}//END CLASS
	}//END NAMESPACE
