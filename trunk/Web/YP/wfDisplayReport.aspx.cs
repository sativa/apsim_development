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
				DisplayImage();
				}
			}

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

		//---------------------------------------------------------------------------
		//Gets the selected report as an object from the database and then sends it to
		//the web page as a GIF image.
		//---------------------------------------------------------------------------
		private void DisplayImage()
			{	
			/*if(Session["SelectedReportID"].ToString() != "0")
				{
				object obReport = DataAccessClass.GetReport(Session["SelectedReportID"].ToString());
				if(obReport != null)
					{
					Response.ContentType =  "image/GIF";
					Response.BinaryWrite( (byte[]) obReport );
					}
				}*/
				if(Session["SelectedReportName"].ToString() != "0")
				{
					string szImageLocation = "Reports\\"+SetUserID()+"\\"+
						Session["SelectedReportYear"].ToString()+"\\"+
						Session["SelectedReportName"].ToString()+".gif";
					imgReport.ImageUrl = szImageLocation;
				}
			}
		//-------------------------------------------------------------------------
		//Determines which UserID to use, either the SelectedUserID or the 
		//UserID.  This depends on whether it is the user viewing their own reports
		//or it a user view their grower's reports
		//-------------------------------------------------------------------------	
		private string SetUserID()
		{
			string szUserID = "";
			if(Session["SelectedUserID"].ToString() != "" && Session["SelectedUserID"].ToString() != "0")
			{
				szUserID = Session["SelectedUserID"].ToString();
			}
			else
			{
				szUserID = Session["UserID"].ToString();
			}
			return szUserID;
		}
	//----------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
