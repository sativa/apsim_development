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
	/// Summary description for wfReportsGenerate.
	/// </summary>
	public class wfReportsGenerate : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblPaddock;
		protected System.Web.UI.WebControls.Button btnNext;
		protected System.Web.UI.WebControls.DropDownList cboReportTypes;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.Label lblReportType;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.ImageButton btnHelpGrid;
		protected System.Web.UI.WebControls.ImageButton btnHelpReportType;
		protected System.Web.UI.WebControls.ImageButton btnHelpReportGeneratePage;
		protected Janus.Web.GridEX.GridEX grdUsers;
	


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
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNext.Click += new System.EventHandler(this.btnNext_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		
		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillPaddockGrid()
		{
			DataTable dtPaddocks = DataAccessClass.GetPaddocksOfUser(FunctionsClass.GetActiveUserName());
			grdUsers.DataSource =  dtPaddocks;
			grdUsers.DataBind();

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillReportTypesCombo()
		{
			DataTable dtReports = DataAccessClass.GetAllReportTypes();
			cboReportTypes.DataSource = dtReports;
			cboReportTypes.DataTextField = "Type";
			cboReportTypes.DataValueField = "Type";
			cboReportTypes.DataBind();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private bool IsPaddockSelectionValid(ref string szErrorMessage)
		{
			bool bIsPaddockSelectionValid = true;
			if(ReportClass.IsPreSowingReport(cboReportTypes.SelectedValue) == false)
			{
				szErrorMessage = "The following paddocks have the wrong crop for this report type: ";
				int iRowPosition = 0;
			
				for(int iIndex = 0; iIndex < grdUsers.SelectedItems.Count; iIndex++)
				{
					iRowPosition = grdUsers.SelectedItems[iIndex].Position;
					if(DataAccessClass.IsReportCropComboValid(grdUsers.GetRow(iRowPosition).Cells["CropType"].Text,
						cboReportTypes.SelectedValue) == false)	
					{
						szErrorMessage = szErrorMessage + grdUsers.GetRow(iRowPosition).Cells["PaddockName"].Text+", ";
						bIsPaddockSelectionValid = false;
					}
				}
				int iLastIndex = szErrorMessage.LastIndexOf(",");
				if(iLastIndex > 0)
					szErrorMessage = szErrorMessage.Remove(iLastIndex, 1);
			}
			return bIsPaddockSelectionValid;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void TransferToReportGenerationPages()
		{
			if(FunctionsClass.IsReadOnly() == false)
			{
				string szErrorMessage = "";
				if(grdUsers.SelectedItems.Count > 0)
				{
					if(IsPaddockSelectionValid(ref szErrorMessage))
					{
						switch(cboReportTypes.SelectedValue)
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
						FunctionsClass.DisplayMessage(Page, szErrorMessage);
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
		public DataTable ReturnsPaddockSelection()
		{
			DataTable dtPaddocks = new DataTable("Paddocks");
			dtPaddocks.Columns.Add("PaddockName");
			dtPaddocks.Columns.Add("UserName");
			dtPaddocks.Columns.Add("CropType");
			dtPaddocks.Columns.Add("ReportType");

			try
			{
				int iRowPosition = 0;
				DataRow drSelectedPaddock;
				string szUserName = FunctionsClass.GetActiveUserName();
				if(cboReportTypes.SelectedValue != "")
				{
					for(int iIndex = 0; iIndex < grdUsers.SelectedItems.Count; iIndex++)
					{
						iRowPosition = grdUsers.SelectedItems[iIndex].Position;
						drSelectedPaddock = dtPaddocks.NewRow();
						drSelectedPaddock["ReportType"] = cboReportTypes.SelectedValue;
						drSelectedPaddock["UserName"] = szUserName;
						drSelectedPaddock["PaddockName"] = grdUsers.GetRow(iRowPosition).Cells["PaddockName"].Text;
						drSelectedPaddock["CropType"] = grdUsers.GetRow(iRowPosition).Cells["CropType"].Text;
						dtPaddocks.Rows.Add(drSelectedPaddock);
					}
				}
				else
					throw new Exception("Please select a report type");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
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
				HelpClass.SetHelpForReportGeneratePage(btnHelpGrid, btnHelpReportType, btnHelpReportGeneratePage);


				FillReportTypesCombo();
				FillPaddockGrid();

				FunctionsClass.SetHeadingString(lblHeading);
				lblHeading.Text = lblHeading.Text.Replace("'s", "");
				lblHeading.Text = lblHeading.Text.Replace("'", "");
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
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
		private void btnNext_Click(object sender, System.EventArgs e)
		{
			TransferToReportGenerationPages();
		}
		//-------------------------------------------------------------------------
		#endregion



	}//END OF CLASS
}//END OF NAMESPACE
