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
	/// Summary description for wfHelp.
	/// </summary>
	public class wfHelp : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Label lblHelp;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
	


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
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------

		//---------------------------------------------------------------------
		#endregion




		#region Form Events
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(Request.QueryString.Count > 1)
			{
				lblHelp.Text = HelpClass.ReAddFormatting(Request.QueryString["Text"]);
				lblHeading.Text = HelpClass.ReAddFormatting(Request.QueryString["Heading"]);
			}
		}
		//---------------------------------------------------------------------
		#endregion

	}//END OF CLASS
}//END OF NAMESPACE
