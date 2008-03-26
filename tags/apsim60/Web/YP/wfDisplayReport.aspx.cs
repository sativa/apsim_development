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

using System.Data.OleDb;

namespace YP2006
	{
	/// <summary>
	/// Summary description for wfDisplayReport.
	/// </summary>
	public class wfDisplayReport : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnNextTop;
		protected System.Web.UI.WebControls.Button btnPreviousTop;
		protected System.Web.UI.WebControls.Button btnNextBottom;
		protected System.Web.UI.WebControls.Button btnPreviousBottom;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.ImageButton btnHelpDisplayReportPage;
		protected System.Web.UI.WebControls.Table tblReports;
		protected System.Web.UI.WebControls.Image imgReport;


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
			this.btnNextTop.Click += new System.EventHandler(this.btnNext_Click);
			this.btnPreviousTop.Click += new System.EventHandler(this.btnPrevious_Click);
			this.btnPreviousBottom.Click += new System.EventHandler(this.btnPrevious_Click);
			this.btnNextBottom.Click += new System.EventHandler(this.btnNext_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void InitialiseHeader()
		{
			if((((DataTable)ViewState["Reports"]).Rows.Count-1) == Convert.ToInt32(ViewState["ReportNumber"]))
			{
				btnNextTop.Enabled = false;
				btnNextBottom.Enabled = false;
			}
			btnPreviousTop.Enabled = false;
			btnPreviousBottom.Enabled = false;

			SetHeadingText();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetHeadingText()
		{
			int iReportNumber = Convert.ToInt32(ViewState["ReportNumber"]);
			if(((DataTable)ViewState["Reports"]).Rows.Count > iReportNumber)
			{
				lblHeading.Text = 
					DataAccessClass.GetNameOfUser(((DataTable)ViewState["Reports"]).Rows[iReportNumber]["UserName"].ToString())+ " - "+
					((DataTable)ViewState["Reports"]).Rows[iReportNumber]["ReportName"].ToString();
			}
			
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StoreReportSelection()
		{
			try
			{
				string szPreviousPage = Context.Handler.ToString();

				switch(szPreviousPage)
				{
					case "ASP.wfReportsViewConsultant_aspx":
						StoreReportSelectionConsultant();
						break;

					case "ASP.wfReportsView_aspx":
						StoreReportSelectionGrower();
						break;

					default:
						break;
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StoreReportSelectionConsultant()
		{
			wfReportsViewConsultant ReportsViewConsultant = (wfReportsViewConsultant) HttpContext.Current.Handler;
			ViewState["Reports"] = ReportsViewConsultant.ReturnsReportSelection();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StoreReportSelectionGrower()
		{
			wfReportsView ReportsView = (wfReportsView) HttpContext.Current.Handler;
			ViewState["Reports"] = ReportsView.ReturnsReportSelection();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void IncrementReportCounter()
		{
			btnPreviousTop.Enabled = true;
			btnPreviousBottom.Enabled = true;
			int iReportNumber = Convert.ToInt32(ViewState["ReportNumber"]);
			if((((DataTable)ViewState["Reports"]).Rows.Count-1) > iReportNumber)
			{
				iReportNumber++;
			}
			if((((DataTable)ViewState["Reports"]).Rows.Count-1) == iReportNumber)
			{
				btnNextTop.Enabled = false;
				btnNextBottom.Enabled = false;
			}
			ViewState["ReportNumber"] = iReportNumber;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DecrementReportCounter()
		{
			btnNextTop.Enabled = true;
			btnNextBottom.Enabled = true;
			int iReportNumber = Convert.ToInt32(ViewState["ReportNumber"]);
			if(iReportNumber > 0)
			{
				iReportNumber--;
			}
			if(iReportNumber == 0)
			{
				btnPreviousTop.Enabled = false;
				btnPreviousBottom.Enabled = false;
			}
			ViewState["ReportNumber"] = iReportNumber;
		}
		//---------------------------------------------------------------------------
		//Gets the selected report as an object from the database and then sends it to
		//the web page as a GIF image.
		//---------------------------------------------------------------------------
		private void DisplayImage()
			{	
			int iReportNumber = Convert.ToInt32(ViewState["ReportNumber"]);

			if(((DataTable)ViewState["Reports"]).Rows.Count > iReportNumber)
				{
				int iNumberOfPages = ReportClass.ReturnNumberOfPagesInAReport(((DataTable)ViewState["Reports"]).Rows[iReportNumber]["ReportName"].ToString(),
					((DataTable)ViewState["Reports"]).Rows[iReportNumber]["UserName"].ToString(), 
					Convert.ToInt32(((DataTable)ViewState["Reports"]).Rows[iReportNumber]["ReportYear"].ToString()));
				string szImageDirectory = "<img src= \"Reports\\"+((DataTable)ViewState["Reports"]).Rows[iReportNumber]["UserName"].ToString()+"\\"+
					((DataTable)ViewState["Reports"]).Rows[iReportNumber]["ReportYear"].ToString()+"\\";
				System.Web.UI.WebControls.TableRow trReport;
				System.Web.UI.WebControls.TableCell tcReport;

				for(int iIndex = 1; iIndex <= iNumberOfPages; iIndex++)
					{
					trReport = new TableRow();
					tcReport = new TableCell();
					//tcReport.BorderStyle = System.Web.UI.WebControls.BorderStyle.Solid;
					//tcReport.BorderColor = System.Drawing.Color.Black;
					//If there is only one page, then the report is in the old format and won't have the [pageX] postfix
					if(iNumberOfPages == 1)
						{
						tcReport.Text = szImageDirectory+((DataTable)ViewState["Reports"]).Rows[iReportNumber]["ReportName"].ToString()+".gif\">";
						}
					//If there is more than one page then the report will have the [pageX] postfix
					else
						{
						tcReport.Text = szImageDirectory+((DataTable)ViewState["Reports"]).Rows[iReportNumber]["ReportName"].ToString()+"[page"+iIndex.ToString()+"].gif\"><BR> <HR>";
						
					}
					trReport.Cells.Add(tcReport);

					tblReports.Rows.Add(trReport);
					}
				}
			}
		//----------------------------------------------------------------------------
		#endregion



		#region Form Events
		//---------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//---------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				HelpClass.SetHelpForDisplayReportPage(btnHelpDisplayReportPage);

				ViewState["Reports"] = new DataTable("Reports");
				ViewState["ReportNumber"] = 0;

				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetReportNavigationButtons(btnReportsView, btnNewReports, btnFavouriteReports);

				StoreReportSelection();
				DisplayImage();
				InitialiseHeader();
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
			IncrementReportCounter();
			DisplayImage();
			SetHeadingText();
		}

		private void btnPrevious_Click(object sender, System.EventArgs e)
		{
			DecrementReportCounter();
			DisplayImage();
			SetHeadingText();
		}
		//----------------------------------------------------------------------------
		#endregion


	//----------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
