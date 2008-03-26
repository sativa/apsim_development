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
	/// Summary description for wfReportsFavouritesConsultant.
	/// </summary>
	public class wfReportsFavouritesConsultant : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected Janus.Web.GridEX.GridEX grdFavourites;
		protected System.Web.UI.WebControls.LinkButton btnAdd;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnDelete;
		protected System.Web.UI.WebControls.Button btnGenerate;
		protected System.Web.UI.WebControls.Button btnEdit;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnDeleteTwo;
		protected System.Web.UI.WebControls.Button btnEditTwo;
		protected System.Web.UI.WebControls.Button btnGenerateTwo;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.ImageButton btnHelpConsultantFavouriteReportPage;
		protected System.Web.UI.WebControls.ImageButton btnAddImg;
	


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
			this.btnPersonalDetailsConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageGrowers.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenuConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGenerate.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnEdit.Click += new System.EventHandler(this.btnEdit_Click);
			this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
			this.btnGenerateTwo.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnDeleteTwo.Click += new System.EventHandler(this.btnDelete_Click);
			this.btnEditTwo.Click += new System.EventHandler(this.btnEdit_Click);
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
				DataTable dtFavourites = DataAccessClass.GetAllFavouriteReportsOfUser(Session["UserName"].ToString());
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
						string szReportType = grdFavourites.GetRow(0).Cells["ReportType"].Text;

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
							DataAccessClass.DeleteFavouriteReport(Session["UserName"].ToString(), grdFavourites.GetRow(iRowPosition).Cells["UserName"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text, grdFavourites.GetRow(iRowPosition).Cells["DateLastModified"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text, grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text);
						}
						Server.Transfer("wfReportsFavouritesConsultant.aspx");
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
							
							szReportXML = DataAccessClass.GetFavouriteReport(Session["UserName"].ToString(), 
								grdFavourites.GetRow(iRowPosition).Cells["UserName"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text,
								grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text);
							
							ReportClass.UpdateFavourite(ref szReportXML);
							
							if(EmailClass.SendReportEmail(grdFavourites.GetRow(iRowPosition).Cells["ReportName"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["CropType"].Text, 
								grdFavourites.GetRow(iRowPosition).Cells["ReportType"].Text, 
								szReportXML,  grdFavourites.GetRow(iRowPosition).Cells["UserName"].Text,
								grdFavourites.GetRow(iRowPosition).Cells["PaddockName"].Text) == true)
							{
							}
							else
								throw new Exception("Error requesting report");
						}
						Server.Transfer("wfReportsFavouritesConsultant.aspx");
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
				drSelectedPaddock["UserName"] = grdFavourites.GetRow(iRowPosition).Cells["UserName"].Text;
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
				FunctionsClass.CheckForConsultantLevelPriviledges();

				HelpClass.SetHelpForConsultantReporFavouritePage(btnHelpConsultantFavouriteReportPage);

				FunctionsClass.SetHeadingString(lblHeading);
				FillFavouritesGrid();
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
