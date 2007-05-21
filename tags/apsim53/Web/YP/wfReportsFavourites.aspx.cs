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
	/// Summary description for wfReportsFavourites.
	/// </summary>
	public class wfReportsFavourites : System.Web.UI.Page
	{
		protected Janus.Web.GridEX.GridEX grdFavourites;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.LinkButton btnAdd;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnDelete;
		protected System.Web.UI.WebControls.Button btnEdit;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnGenerateTwo;
		protected System.Web.UI.WebControls.Button btnEditTwo;
		protected System.Web.UI.WebControls.Button btnDeleteTwo;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.ImageButton btnHelpReportFavouritePage;
		protected System.Web.UI.WebControls.Button btnGenerate;


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
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGenerate.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnEdit.Click += new System.EventHandler(this.btnEdit_Click);
			this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
			this.btnGenerateTwo.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnEditTwo.Click += new System.EventHandler(this.btnEdit_Click);
			this.btnDeleteTwo.Click += new System.EventHandler(this.btnDelete_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion
	

		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillFavouritesGrid()
		{
			try
			{
				DataTable dtFavourites = DataAccessClass.GetAllFavouriteReportsOfUser(FunctionsClass.GetActiveUserName());
				this.grdFavourites.DataSource =  dtFavourites;
				this.grdFavourites.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void EditReport()
		{
			if(FunctionsClass.IsReadOnly() == false)
			{
				if(grdFavourites.SelectedItems.Count > 0)
				{
					if(grdFavourites.SelectedItems.Count == 1)
					{
						int iRowPosition = grdFavourites.SelectedItems[0].Position;
						string szReportType = grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text;

						switch(szReportType)
						{
							case "Fallow report":
								Server.Transfer("wfGenerateFallowReport.aspx");
								break;
							case "Irrigation comparison report":
								Server.Transfer("wfGenerateIrrigationComparisonReport.aspx");
								break;
							case "Agronomic report":
								Server.Transfer("wfGenerateReport.aspx");
								break;
							case "Climate report":
								Server.Transfer("wfGenerateReport.aspx");
								break;
							case "Irrigation scheduling report":
								Server.Transfer("wfGenerateReport.aspx");
								break;
							case "Nitrogen profit report":
								Server.Transfer("wfGenerateNitrogenProfitReport.aspx");
								break;
							case "Nitrogen comparison report":
								Server.Transfer("wfGenerateNitrogenComparisonReport.aspx");
								break;
							case "Pre-sowing nitrogen comparison report":
								Server.Transfer("wfGeneratePreSowingNitrogenComparisonReport.aspx");
								break;
							case "Sowing X variety report":
								Server.Transfer("wfGenerateSowingXVarietyReport.aspx");
								break;
							case "Pre-sowing agronomic report":
								Server.Transfer("wfGeneratePreSowingReport.aspx");
								break;
							case "Pre-sowing climate report":
								Server.Transfer("wfGeneratePreSowingReport.aspx");
								break;
							case "Pre-season report":
								Server.Transfer("wfGeneratePreSeasonReport.aspx");
								break;
						}
					}
					else
						FunctionsClass.DisplayMessage(Page, "You can only edit one report at a time");
				}
				else
					FunctionsClass.DisplayMessage(Page, "Please select a paddock first");
			}
			else
				FunctionsClass.DisplayMessage(Page, FunctionsClass.ReturnReadOnlyMessage());
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DeleteReport()
		{
			try
			{
				if(FunctionsClass.IsReadOnly() == false)
				{
					if(grdFavourites.SelectedItems.Count > 0)
					{
						int iRowPosition = 0;
						for(int iIndex = 0; iIndex < grdFavourites.SelectedItems.Count; iIndex++)
						{
							iRowPosition = grdFavourites.SelectedItems[iIndex].Position;
							DataAccessClass.DeleteFavouriteReport(FunctionsClass.GetActiveUserName(), FunctionsClass.GetActiveUserName(), 
								grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text, grdFavourites.GetRow(iRowPosition).Cells["DateLastModified"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text, grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text);
						}
						Server.Transfer("wfReportsFavourites.aspx");
					}
					else
						throw new Exception("Please select a report first");
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
		//
		//-------------------------------------------------------------------------
		private void GenerateReport()
		{
			try
			{
				string szReportXML = "";
				if(FunctionsClass.IsReadOnly() == false)
				{
					if(grdFavourites.SelectedItems.Count > 0)
					{
						int iRowPosition = 0;
						for(int iIndex = 0; iIndex < grdFavourites.SelectedItems.Count; iIndex++)
						{
							iRowPosition = grdFavourites.SelectedItems[iIndex].Position;
							//Generate the files needed to generate a report and then email these files to the ApsimRun machine
							
							szReportXML = DataAccessClass.GetFavouriteReport(FunctionsClass.GetActiveUserName(), 
								FunctionsClass.GetActiveUserName(), 
								grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text,
								grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text);
							
							ReportClass.UpdateFavourite(ref szReportXML);
							
							if(EmailClass.SendReportEmail(grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["CropType"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text, 
								szReportXML,  FunctionsClass.GetActiveUserName(),
								grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text) == true)
							{
							}
							else
								throw new Exception("Error requesting report");
						}
						Server.Transfer("wfReportsFavourites.aspx");
					}
					else
						throw new Exception("Please select a report first");
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
		//
		//-------------------------------------------------------------------------
		public DataTable ReturnsPaddockSelection()
		{
			DataTable dtPaddocks = new DataTable("Paddocks");
			dtPaddocks.Columns.Add("PaddockName");
			dtPaddocks.Columns.Add("UserName");
			dtPaddocks.Columns.Add("CropType");
			dtPaddocks.Columns.Add("ReportType");
			dtPaddocks.Columns.Add("ReportName");

			if(grdFavourites.SelectedItems.Count > 0)
			{
				int iRowPosition = grdFavourites.SelectedItems[0].Position;
				DataRow drSelectedPaddock = dtPaddocks.NewRow();
				drSelectedPaddock["UserName"] = FunctionsClass.GetActiveUserName();
				drSelectedPaddock["PaddockName"] = grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text;
				drSelectedPaddock["CropType"] = grdFavourites.GetRow(iRowPosition).Cells["CropType"].Text;
				drSelectedPaddock["ReportType"] = grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text;
				drSelectedPaddock["ReportName"] = grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text;
				dtPaddocks.Rows.Add(drSelectedPaddock);
			}
			return dtPaddocks;
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

				HelpClass.SetHelpForReporFavouritePage(btnHelpReportFavouritePage);

				FillFavouritesGrid();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);

				btnDelete.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected favourite(s) \");");
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
		private void btnEdit_Click(object sender, System.EventArgs e)
		{
		EditReport();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDelete_Click(object sender, System.EventArgs e)
		{
			DeleteReport();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnGenerate_Click(object sender, System.EventArgs e)
		{
			GenerateReport();
		}
		//-------------------------------------------------------------------------
		#endregion
	
	}//END OF CLASS
}//END OF NAMESPACE
