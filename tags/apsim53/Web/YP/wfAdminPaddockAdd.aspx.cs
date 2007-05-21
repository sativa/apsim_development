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
	/// Summary description for wfAdminPaddockAdd.
	/// </summary>
	public class wfAdminPaddockAdd : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.TextBox edtPaddockName;
		protected System.Web.UI.WebControls.Label lblPaddockName;
		protected System.Web.UI.WebControls.Label lblStartGrowingSeason;
		protected System.Web.UI.WebControls.DropDownList cboRegion;
		protected System.Web.UI.WebControls.Label lbRegion;
		protected System.Web.UI.WebControls.Label lblWeatherStation;
		protected System.Web.UI.WebControls.DropDownList cboWeatherStation;
		protected System.Web.UI.WebControls.DropDownList cboLinkedRainfall;
		protected System.Web.UI.WebControls.CheckBox chkLinkedRainfall;
		protected System.Web.UI.WebControls.CheckBox chkDefaultRainfall;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Data.DataSet dsStartOfGrowingSeason;
		protected System.Data.DataTable dtStartOfGrowingSeason;
		protected System.Data.DataColumn dcGrowingSeasonDate;
		protected Janus.Web.GridEX.GridEX grdStartOfGrowingSeason;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Button btnCancel;



		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			//System.Globalization.DateTimeFormatInfo.CurrentInfo.ShortDatePattern = "dd/MM/yyyy";
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
			this.dsStartOfGrowingSeason = new System.Data.DataSet();
			this.dtStartOfGrowingSeason = new System.Data.DataTable();
			this.dcGrowingSeasonDate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsStartOfGrowingSeason)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtStartOfGrowingSeason)).BeginInit();
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.chkDefaultRainfall.CheckedChanged += new System.EventHandler(this.chkDefaultRainfall_CheckedChanged);
			this.chkLinkedRainfall.CheckedChanged += new System.EventHandler(this.chkLinkedRainfall_CheckedChanged);
			this.cboRegion.SelectedIndexChanged += new System.EventHandler(this.cboRegion_SelectedIndexChanged);
			this.btnPersonalDetailsConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageGrowers.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			// 
			// dsStartOfGrowingSeason
			// 
			this.dsStartOfGrowingSeason.DataSetName = "NewDataSet";
			this.dsStartOfGrowingSeason.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsStartOfGrowingSeason.Tables.AddRange(new System.Data.DataTable[] {
																						this.dtStartOfGrowingSeason});
			// 
			// dtStartOfGrowingSeason
			// 
			this.dtStartOfGrowingSeason.Columns.AddRange(new System.Data.DataColumn[] {
																						  this.dcGrowingSeasonDate});
			this.dtStartOfGrowingSeason.TableName = "StartOfGrowingSeason";
			// 
			// dcGrowingSeasonDate
			// 
			this.dcGrowingSeasonDate.ColumnName = "GrowingSeasonDate";
			this.dcGrowingSeasonDate.DataType = typeof(System.DateTime);
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsStartOfGrowingSeason)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtStartOfGrowingSeason)).EndInit();

		}
		#endregion



		#region Form Functions


		//-------------------------------------------------------------------------
		//When the user changes the status of the linked rainfall check box
		//the linked rainfall combo box is enabled or disabled
		//-------------------------------------------------------------------------
		private void ChangeLinkedRainfallStatus()
		{
			if(chkLinkedRainfall.Checked == true)
			{
				chkDefaultRainfall.Checked = false;
				chkDefaultRainfall.Enabled = false;
				cboLinkedRainfall.Enabled = true;
			}
			else
			{
				chkDefaultRainfall.Enabled = true;
				cboLinkedRainfall.Enabled = false;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void ChangeDefaultRainfallStatus()
		{
			if(chkDefaultRainfall.Checked == true)
			{
				chkLinkedRainfall.Checked = false;
				chkLinkedRainfall.Enabled = false;
				cboLinkedRainfall.Enabled = false;
			}
			else
			{
				chkLinkedRainfall.Enabled = true;
			}
		}
		//-------------------------------------------------------------------------
		//Set the date shown in the strart of growing season date grid
		//-------------------------------------------------------------------------
		private void SetStartOfGrowingSeasonDate(string szStartOfGrowingSeason)
		{
			DataRow drStartOfGrowingSeason;
			if(szStartOfGrowingSeason != null && szStartOfGrowingSeason != "")
			{
				drStartOfGrowingSeason = dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].NewRow();
				drStartOfGrowingSeason["GrowingSeasonDate"] = DateTime.ParseExact(szStartOfGrowingSeason, "yyyy-MM-dd", null);
				dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].Rows.Add(drStartOfGrowingSeason);
			}
			else
			{
				drStartOfGrowingSeason = dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].NewRow();
				drStartOfGrowingSeason["GrowingSeasonDate"] = new DateTime(DateTime.Today.Year, 4, 1);
				dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].Rows.Add(drStartOfGrowingSeason);
			}
			grdStartOfGrowingSeason.DataBind();
		}
		//-------------------------------------------------------------------------
		//Fills the regions combo box with all the regions from the database
		//-------------------------------------------------------------------------
		private void FillRegionCombo()
		{
			try
			{
				DataTable dtRegions = DataAccessClass.GetAllRegions();
				cboRegion.DataSource = dtRegions;
				cboRegion.DataTextField = "Type";
				cboRegion.DataValueField = "Type";
				cboRegion.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Fills the met station combo box with all the met stations linked to the
		//selected region
		//-------------------------------------------------------------------------
		private void FillMetStationCombo()
		{
			//If a region is selected then fill the combo box
			if(cboRegion.SelectedValue != null && cboRegion.SelectedValue != "")
			{
				try
				{
					string szSelectedRegionID = cboRegion.SelectedValue.ToString(); 
					DataTable dtMetStations = DataAccessClass.GetMetStationsOfRegion(szSelectedRegionID);
					cboWeatherStation.DataSource = dtMetStations;
					cboWeatherStation.DataTextField = "Name";
					cboWeatherStation.DataValueField = "Name";
					cboWeatherStation.DataBind();
				}
				catch(Exception E)
				{
					FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
				//If no region is selected then display and error to the user
			else
			{
				FunctionsClass.DisplayMessage(Page, "Please select a region");
			}
		}
		//-------------------------------------------------------------------------
		//Fills the linked rainfall combo box with all the paddocks from the database
		//-------------------------------------------------------------------------
		private void FillLinkedRainfallCombo()
		{
			try
			{
				DataTable dtPaddocks = 
					DataAccessClass.GetAllPaddocksForTemporalLinking("", 
					FunctionsClass.GetActiveUserName());
				cboLinkedRainfall.DataSource = dtPaddocks;
				cboLinkedRainfall.DataTextField = "Name";
				cboLinkedRainfall.DataValueField = "Name";
				cboLinkedRainfall.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//The paddock details are updated, but firstly a check is run to determine
		//if the the sown check box is checked.  If it is, then the paddock is updated
		//with the details from the from.  If it hasn't been checked then the paddock
		//is updated with blank details.
		//-------------------------------------------------------------------------
		private void SavePaddockDetails()
		{
			try
			{
				if(InputValidationClass.IsInputAValidFileLocationString(edtPaddockName.Text) == true)
				{
					string szPaddockName = edtPaddockName.Text;
					if(szPaddockName != "" && szPaddockName != null)
					{
						if(DataAccessClass.IsPaddockNameAvailable(szPaddockName, FunctionsClass.GetActiveUserName()) == false)
						{
							throw new Exception("You already have a paddock with this name");
						}
						//If a metstation and soil type are selected then save the record
						if(cboWeatherStation.SelectedValue != "" && cboWeatherStation.SelectedValue != "None" &&
							grdStartOfGrowingSeason.GetRow(0).Cells["GrowingSeasonDate"].Text != "")
						{
							string szStartOfGrowingSeasonDate = 
								(DateTime.ParseExact(grdStartOfGrowingSeason.GetRow(0).Cells["GrowingSeasonDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd");
							string szLinkedTemporalPaddock = "";
							if(chkLinkedRainfall.Checked == true)
							{
								szLinkedTemporalPaddock = cboLinkedRainfall.SelectedItem.Text;
							}
							else
							{
								szLinkedTemporalPaddock = "NONE";
							}
							//Cancel all linked rainfall options if user has selected to use default rainfall values
							if(chkDefaultRainfall.Checked == true)
							{
								szLinkedTemporalPaddock = "NONE";
								chkLinkedRainfall.Checked = false;
							}
							DataAccessClass.InsertPaddock(cboWeatherStation.SelectedValue,  
								Convert.ToInt32(chkDefaultRainfall.Checked),
								szLinkedTemporalPaddock, szStartOfGrowingSeasonDate, szPaddockName,  
								FunctionsClass.GetActiveUserName());
							Server.Transfer("wfPaddocksMenu.aspx");
						}
							//If either a metstation or a soil type are not selected display an 
							//error message to the user.
						else
							throw new Exception("Please ensure that all fields contain data");	
					}
					else
						throw new Exception("Missing paddock name");
				}
				else 
					throw new Exception(InputValidationClass.ReturnInvalidLocationMessage("Paddock name"));

			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Sets up the page for display to the user
		//-------------------------------------------------------------------------
		private void InitialisePage()
			{
			//Fills the region combo box
			FillRegionCombo();
			//Fills the linked rainfall paddock combo box
			FillLinkedRainfallCombo();
			//Set the sow date on the calander		
			SetStartOfGrowingSeasonDate("");
			//Fills the met station combo box
			FillMetStationCombo();

			ChangeDefaultRainfallStatus();

			ChangeLinkedRainfallStatus();
			}
		//---------------------------------------------------------------------------
		#endregion



		#region Form Events
		//---------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the users
		//permissions are checked and the page is initialised
		//---------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();

				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationButtonText(btnGrowersPaddocks);
				FunctionsClass.SetNavigationButtonText(btnGrowersReports);
				InitialisePage();
				FunctionsClass.SetControlFocus("edtName", this);
				}
			}
		//-------------------------------------------------------------------------
		//When the save button is pressed the paddock details are saved
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SavePaddockDetails();
			}
		//-------------------------------------------------------------------------
		//When the cancel button is pressed the user is sent back to the 
		//ViewGrowers page.
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfPaddocksMenu.aspx");
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
		private void chkLinkedRainfall_CheckedChanged(object sender, System.EventArgs e)
		{
			ChangeLinkedRainfallStatus();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void chkDefaultRainfall_CheckedChanged(object sender, System.EventArgs e)
		{
			ChangeDefaultRainfallStatus();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void cboRegion_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			FillMetStationCombo();
		}
		//-------------------------------------------------------------------------	
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
