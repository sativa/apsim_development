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
	/// Summary description for wfMainMenu.
	/// </summary>
	public class wfMainMenu : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblAdministratorSubscriber;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.Panel pnlAdministratorMain;
		protected System.Web.UI.WebControls.Label lblReportsGrowerVisitor;
		protected System.Web.UI.WebControls.Label lblPaddockVisitors;
		protected System.Web.UI.WebControls.Label lblReportsGrowerSubscriber;
		protected System.Web.UI.WebControls.LinkButton btnReportsMain;
		protected System.Web.UI.WebControls.Label lblPaddocksSubscribers;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksMain;
		protected System.Web.UI.WebControls.Panel pnlGrowerMain;
		protected System.Web.UI.WebControls.Label lblReportsSubscribersCon;
		protected System.Web.UI.WebControls.LinkButton btnManageReportsMain;
		protected System.Web.UI.WebControls.Label lblGrowersSubscribers;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowersMain;
		protected System.Web.UI.WebControls.Panel pnlConsultantMain;
		protected System.Web.UI.WebControls.Label lblPDVisitors;
		protected System.Web.UI.WebControls.Label lblVisitors;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsMain;
		protected System.Web.UI.WebControls.Label lblSubscribers;
		protected System.Web.UI.WebControls.Label lblPDSubscribers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.LinkButton btnLogout;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.ImageButton btnHelpMainMenuPage;
		protected System.Web.UI.WebControls.Label lblHeading;
	

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
			this.btnPersonalDetailsMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageGrowersMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReportsMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnAdmin.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnLogout.Click += new System.EventHandler(this.btnLogout_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		
		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void InitialisePage()
		{
			if(FunctionsClass.IsConsultant() == true)
			{
				FunctionsClass.SetNavigationMenu(btnManageItems, btnManageReports);
				pnlConsultantMain.Visible = true;
				pnlGrowerMain.Visible = false;
				pnlAdministratorMain.Visible = false;
			}
			else if(FunctionsClass.IsAdministrator() == true)
			{
				FunctionsClass.SetNavigationMenu(btnManageItems, btnManageReports);
				pnlConsultantMain.Visible = true;
				pnlGrowerMain.Visible = false;
				pnlAdministratorMain.Visible = true;
			}
			else
			{
				FunctionsClass.SetNavigationMenu(btnManageItems, btnManageReports);
				pnlConsultantMain.Visible = false;
				pnlGrowerMain.Visible = true;
				pnlAdministratorMain.Visible = false;
			}
			Session["SelectedUserName"] = "";
			Session["SelectedPaddockName"] = "";
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
				HelpClass.SetHelpForMainMenuPage(btnHelpMainMenuPage);
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetDisplayBanner(imgBanner);
				InitialisePage();
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
		private void btnLogout_Click(object sender, System.EventArgs e)
		{
			if(Response.Cookies["YP"] != null)
			{
				Response.Cookies["YP"].Expires = DateTime.Now.AddDays(-1) ;
			}
			Server.Transfer("wfSessionTimeOut.aspx");
		}
		//-------------------------------------------------------------------------
		#endregion
	
	}//END OF CLASS
}//END OF NAMESPACE