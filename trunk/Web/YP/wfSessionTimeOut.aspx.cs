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
	/// Summary description for wfSessionTimeOut.
	/// </summary>
	public class wfSessionTimeOut : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblWarning;
		protected System.Web.UI.WebControls.Label lblDetail;
		protected System.Web.UI.WebControls.HyperLink hylLogin;
		//-------------------------------------------------------------------------
		//If the Session object isn't null then reset all the variables
		//to their original state
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			FunctionsClass.SetControlFocus("hylLogin", this);
			if(Session != null)
			{
				Session["UserID"] = "0";
				Session["SelectedUserID"] = "0";
				Session["SelectedPaddockID"] = "0";
				Session["SelectedReport"] = "";
				Session["SelectedReportYear"] = "";
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
	}
}
