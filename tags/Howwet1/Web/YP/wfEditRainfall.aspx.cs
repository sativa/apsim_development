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
	/// Summary description for wfEditRainfall.
	/// </summary>
	public class wfEditRainfall : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.Label lblYear;
		protected System.Data.DataSet dsRainfall;
		protected System.Data.DataTable dtRainfall;
		protected System.Data.DataColumn dcDay;
		protected System.Data.DataColumn dcJan;
		protected System.Data.DataColumn dcFeb;
		protected System.Data.DataColumn dcMar;
		protected System.Data.DataColumn dcApr;
		protected System.Data.DataColumn dcMay;
		protected System.Data.DataColumn dcJun;
		protected System.Data.DataColumn dcJul;
		protected System.Data.DataColumn dcAug;
		protected System.Data.DataColumn dcSep;
		protected System.Data.DataColumn dcOct;
		protected System.Data.DataColumn dcNov;
		protected System.Data.DataColumn dcDec;
		protected Janus.Web.GridEX.GridEX grdRainfall;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Label lblTotalRainfall;
		protected System.Web.UI.WebControls.TextBox edtTotalRainfall;
		protected System.Web.UI.WebControls.Label lblCropManagement;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.TextBox edtTotalRainfallTwo;
		protected System.Web.UI.WebControls.Label lblTotalRainfallTwo;
		protected System.Web.UI.WebControls.Label lblRainfallManagement;
		protected System.Web.UI.WebControls.DropDownList cboYear;
	

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
			this.dsRainfall = new System.Data.DataSet();
			this.dtRainfall = new System.Data.DataTable();
			this.dcDay = new System.Data.DataColumn();
			this.dcJan = new System.Data.DataColumn();
			this.dcFeb = new System.Data.DataColumn();
			this.dcMar = new System.Data.DataColumn();
			this.dcApr = new System.Data.DataColumn();
			this.dcMay = new System.Data.DataColumn();
			this.dcJun = new System.Data.DataColumn();
			this.dcJul = new System.Data.DataColumn();
			this.dcAug = new System.Data.DataColumn();
			this.dcSep = new System.Data.DataColumn();
			this.dcOct = new System.Data.DataColumn();
			this.dcNov = new System.Data.DataColumn();
			this.dcDec = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsRainfall)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtRainfall)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.cboYear.SelectedIndexChanged += new System.EventHandler(this.cboYear_SelectedIndexChanged);
			this.grdRainfall.LoadingRow += new Janus.Web.GridEX.RowLoadEventHandler(this.grdRainfall_LoadingRow_1);
			this.grdRainfall.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdRainfall_UpdatingCell);
			// 
			// dsRainfall
			// 
			this.dsRainfall.DataSetName = "NewDataSet";
			this.dsRainfall.Locale = new System.Globalization.CultureInfo("en-US");
			this.dsRainfall.Tables.AddRange(new System.Data.DataTable[] {
																																		this.dtRainfall});
			// 
			// dtRainfall
			// 
			this.dtRainfall.Columns.AddRange(new System.Data.DataColumn[] {
																																			this.dcDay,
																																			this.dcJan,
																																			this.dcFeb,
																																			this.dcMar,
																																			this.dcApr,
																																			this.dcMay,
																																			this.dcJun,
																																			this.dcJul,
																																			this.dcAug,
																																			this.dcSep,
																																			this.dcOct,
																																			this.dcNov,
																																			this.dcDec});
			this.dtRainfall.TableName = "Rainfall";
			// 
			// dcDay
			// 
			this.dcDay.ColumnName = "Day";
			// 
			// dcJan
			// 
			this.dcJan.ColumnName = "Jan";
			this.dcJan.DataType = typeof(System.Double);
			// 
			// dcFeb
			// 
			this.dcFeb.ColumnName = "Feb";
			this.dcFeb.DataType = typeof(System.Double);
			// 
			// dcMar
			// 
			this.dcMar.ColumnName = "Mar";
			this.dcMar.DataType = typeof(System.Double);
			// 
			// dcApr
			// 
			this.dcApr.ColumnName = "Apr";
			this.dcApr.DataType = typeof(System.Double);
			// 
			// dcMay
			// 
			this.dcMay.ColumnName = "May";
			this.dcMay.DataType = typeof(System.Double);
			// 
			// dcJun
			// 
			this.dcJun.ColumnName = "Jun";
			this.dcJun.DataType = typeof(System.Double);
			// 
			// dcJul
			// 
			this.dcJul.ColumnName = "Jul";
			this.dcJul.DataType = typeof(System.Double);
			// 
			// dcAug
			// 
			this.dcAug.ColumnName = "Aug";
			this.dcAug.DataType = typeof(System.Double);
			// 
			// dcSep
			// 
			this.dcSep.ColumnName = "Sep";
			this.dcSep.DataType = typeof(System.Double);
			// 
			// dcOct
			// 
			this.dcOct.ColumnName = "Oct";
			this.dcOct.DataType = typeof(System.Double);
			// 
			// dcNov
			// 
			this.dcNov.ColumnName = "Nov";
			this.dcNov.DataType = typeof(System.Double);
			// 
			// dcDec
			// 
			this.dcDec.ColumnName = "Dec";
			this.dcDec.DataType = typeof(System.Double);
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsRainfall)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtRainfall)).EndInit();

		}
		#endregion



		#region Form Functions
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
		//Gets the current year and sets the Year Combo box to display this year
		//-------------------------------------------------------------------------
		private void SetYearComboBoxToCurrentYear()
			{
			string szYear = DateTime.Today.Year.ToString();
			for(int iIndex = 0; iIndex < cboYear.Items.Count; iIndex++)
				{
				if(szYear == cboYear.Items[iIndex].Text)
					{
					cboYear.SelectedIndex = iIndex;
					break;
					}
				}
			}
		//-------------------------------------------------------------------------
		//Fills the rainfall grid with all the rainfall events stored in the
		//database
		//-------------------------------------------------------------------------
		private void FillRainfallGrid()
			{
			DataRow drRainfall;
			try
				{
				//Creates a 31 blank rows in the dataset
				for(int iIndex = 1; iIndex <= 31; iIndex++)
					{
					drRainfall = dsRainfall.Tables["Rainfall"].NewRow();
					drRainfall["Day"] = iIndex.ToString();
					dsRainfall.Tables["Rainfall"].Rows.Add(drRainfall);	
					}	
				DataTable dtUsersRainfall = DataAccessClass.
					GetPaddocksTemporalEvents(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName(), "patch_rain",  
					cboYear.SelectedItem.Text+"-01-01", cboYear.SelectedItem.Text+"-12-31");
				DateTime dtRainfallDate;
				//Adds rainfall events to the grid
				foreach(DataRow drUsersRainfall in dtUsersRainfall.Rows)
					{
					dtRainfallDate = DateTime.ParseExact(drUsersRainfall["EventDate"].ToString(), "yyyy-MM-dd", null);
					dsRainfall.Tables["Rainfall"].Rows[dtRainfallDate.Day-1][dtRainfallDate.Month] = 
						Convert.ToDouble(drUsersRainfall["EventValue"].ToString());
					}
				this.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Calculates the total rainfall for the selected paddock and year.
		//-------------------------------------------------------------------------
		private void CalculateTotalRainfalls()
			{
			try
				{
				DateTime dtStartDate = new DateTime(Convert.ToInt32(cboYear.SelectedItem.Text), 4, 1);
				DateTime dtEndDate = new DateTime((Convert.ToInt32(cboYear.SelectedItem.Text)), 12, 31);	
				
				edtTotalRainfall.Text = DataAccessClass.
					GetPaddocksTotalTemporalEventsValues(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName(), "patch_rain", 
					dtStartDate.ToString("yyyy-MM-dd"), dtEndDate.ToString("yyyy-MM-dd")).ToString();

				dtStartDate = new DateTime(Convert.ToInt32(cboYear.SelectedItem.Text), 1, 1);
				dtEndDate = new DateTime((Convert.ToInt32(cboYear.SelectedItem.Text)), 12, 31);	
				
				edtTotalRainfallTwo.Text = DataAccessClass.
					GetPaddocksTotalTemporalEventsValues(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName(), "patch_rain", 
					dtStartDate.ToString("yyyy-MM-dd"), dtEndDate.ToString("yyyy-MM-dd")).ToString();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Saves all the rainfall events on the grid to the database, to avoid duplicate
		//values all the paddocks rainfall events are deleted then all the rainfall events
		//on the grid are inserted into the database
		//-------------------------------------------------------------------------
		private bool SaveRainfall()
		{
			bool bSaved = false;
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				try
					{
					Janus.Web.GridEX.GridEXRow grdRow;
					DateTime dtRainfallDate;
					int iYear = Convert.ToInt32(cboYear.SelectedItem.Text);
					//Delete all records for this paddock in this year
					DataAccessClass.DeletePaddocksTemporalEvents(Session["SelectedPaddockName"].ToString(), 
						FunctionsClass.GetActiveUserName(), "patch_rain",  
						cboYear.SelectedItem.Text+"-01-01", cboYear.SelectedItem.Text+"-12-31");
					//Save all the records as new records
					for(int iRowIndex = 0; iRowIndex < grdRainfall.RowCount; iRowIndex++)
						{
						grdRow = grdRainfall.GetRow(iRowIndex);
						for(int iColumnIndex = 1; iColumnIndex <= 12; iColumnIndex++)
							{
							//If there is data in the cell then save it to the database
							if(grdRow.Cells[iColumnIndex].Value != null)
								{
								if(InputValidationClass.IsValidDate(iYear,iColumnIndex, iRowIndex+1) == true)
									{
									dtRainfallDate = new DateTime(iYear,iColumnIndex, iRowIndex+1);
									DataAccessClass.InsertTemporalEvent(dtRainfallDate.ToString("yyyy-MM-dd"), 
										Convert.ToDouble(grdRow.Cells[iColumnIndex].Text), "patch_rain", 
										Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
									}
								}
							}//Columns
						}//Rows
					bSaved = true;
					}
				catch(Exception)
					{
					FunctionsClass.DisplayMessage(Page, "Error saving rainfall entries");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
				}
			return bSaved;
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//Intialises the page for the display
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForVisitorLevelPriviledges();
				DisplayGrowersName();
				SetYearComboBoxToCurrentYear();
				FillRainfallGrid();	
				CalculateTotalRainfalls();
				btnSave.Style.Add("cursor", "hand");	
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the rainfall events are saved
		//then the user is taken back to the edit paddock page
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			if(SaveRainfall())
				{
				Server.Transfer("wfEditPaddock.aspx");
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save image the rainfall events are saved
		//then the user is taken back to the edit paddock page
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			if(SaveRainfall())
				{
				Server.Transfer("wfEditPaddock.aspx");
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button the user is taken back to the edit paddock page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image the user is taken back to the edit paddock page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user changes the selected year in the year combobox, the rainfall
		//grid is refilled and the rainfall totals are re calculated
		//-------------------------------------------------------------------------
		private void cboYear_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillRainfallGrid();
			CalculateTotalRainfalls();
			}
		//-------------------------------------------------------------------------
		//When the rainfall grid is displayed, the grids appearance is altered so
		//that invalid days such as 31 of Feb are a different colour.  This only
		//works in IE (Internet Explorer)
		//-------------------------------------------------------------------------
		private void grdRainfall_LoadingRow_1(object sender, Janus.Web.GridEX.RowLoadEventArgs e)
			{
			if(FunctionsClass.IsBrowserIE(Page))
				{
				int iYear = Convert.ToInt32(cboYear.SelectedItem.Text);
				if(e.Row.RowIndex == 28)
					{
					if(!DateTime.IsLeapYear(iYear))
						{
						e.Row.Cells[2].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
						}
					}
				if(e.Row.RowIndex == 29)
					{
					e.Row.Cells[2].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
					}
				if(e.Row.RowIndex == 30)
					{
					e.Row.Cells[2].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
					e.Row.Cells[4].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
					e.Row.Cells[6].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
					e.Row.Cells[9].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
					e.Row.Cells[11].FormatStyle.BackColor = System.Drawing.Color.PaleGoldenrod;
					}
				}
			}
		//-------------------------------------------------------------------------
		//When the Grid is updated, just before the details are saved, a test is run to 
		//ensure that the values are all positive decimals.  
		//-------------------------------------------------------------------------
		private void grdRainfall_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
			{
			if(e.Value != null)
				{
				if(InputValidationClass.IsInputAPositiveDecimal(e.Value.ToString()) == false)
					{
					e.Value = null;
					}
				}
			}
		//-------------------------------------------------------------------------	
		#endregion


		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
