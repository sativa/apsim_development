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
	/// Summary description for wfRegistrationFinished.
	/// </summary>
	public class wfRegistrationFinished : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.HyperLink hylRegistrationMeu;
		protected System.Web.UI.WebControls.HyperLink hylHome;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
	
		private void Page_Load(object sender, System.EventArgs e)
		{
			// Put user code to initialize the page here
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
