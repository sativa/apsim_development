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
	/// Summary description for wfRegForm1.
	/// </summary>
	public class wfRegForm1 : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Image Image1;
		protected System.Web.UI.WebControls.Button RegistrationButton;
		protected System.Web.UI.WebControls.Label Label2;
	
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
			this.RegistrationButton.Click += new System.EventHandler(this.RegistrationButton_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		// ------------------------------------------------
		// User is wanting to do a registration.
		// ------------------------------------------------
		private void RegistrationButton_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfRegForm2.aspx");
			}
	}
}
