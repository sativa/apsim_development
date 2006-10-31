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
	/// Summary description for wfAdminSQL.
	/// </summary>
	public class wfAdminSQL : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Button btnClear;
		protected System.Web.UI.WebControls.Button btnExecute;
		protected System.Web.UI.WebControls.Label lblSQL;
		protected System.Web.UI.WebControls.TextBox edtSQL;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
	


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
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnAdmin.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnExecute.Click += new System.EventHandler(this.btnExecute_Click);
			this.btnClear.Click += new System.EventHandler(this.btnClear_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//The sql statement written by the user is executed
		//-------------------------------------------------------------------------
		private void RunSQL()
		{
			if(edtSQL.Text != "")
			{
				DataAccessClass.RunSQLStatement(edtSQL.Text);
				edtSQL.Text = "";
			}
			else
			{
				FunctionsClass.DisplayMessage(Page,"Please enter a SQL statement to execute");
			}
		}
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if (!IsPostBack)
			{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FunctionsClass.SetControlFocus("edtSQL", this);
			}
		}
		//-------------------------------------------------------------------------
		//When the user presses the save button, the sql statement is executed
		//-------------------------------------------------------------------------
		private void btnExecute_Click(object sender, System.EventArgs e)
		{
			RunSQL();
		}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button, the sql text box is cleared
		//-------------------------------------------------------------------------
		private void btnClear_Click(object sender, System.EventArgs e)
		{
			edtSQL.Text = "";
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//-------------------------------------------------------------------------
		#endregion



		//-------------------------------------------------------------------------
	}//END CLASS
}//END NAMESPACE
