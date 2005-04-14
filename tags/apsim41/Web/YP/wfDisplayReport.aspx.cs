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

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for wfDisplayReport.
	/// </summary>
	public class wfDisplayReport : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Label Label2;
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
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Stores the report name and report year selections from the previous page in view state
		//variables.
		//-------------------------------------------------------------------------
		private void StoreReportDetails()
			{
			try
				{
				wfViewReports ViewReports = (wfViewReports) Context.Handler;
				ViewState["ReportName"] = ViewReports.ReturnReportName();
				ViewState["ReportYear"] = ViewReports.ReturnReportYear();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//---------------------------------------------------------------------------
		//Gets the selected report as an object from the database and then sends it to
		//the web page as a GIF image.
		//---------------------------------------------------------------------------
		private void DisplayImage()
			{	
			if(ViewState["ReportName"].ToString() != "0")
				{
				string szImageLocation = "Reports\\"+FunctionsClass.GetActiveUserName()+"\\"+
					ViewState["ReportYear"].ToString()+"\\"+
					ViewState["ReportName"].ToString()+".gif";
				imgReport.ImageUrl = szImageLocation;
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
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForVisitorLevelPriviledges();
				StoreReportDetails();
				DisplayImage();
				}
			}
		//----------------------------------------------------------------------------
		#endregion


	//----------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
