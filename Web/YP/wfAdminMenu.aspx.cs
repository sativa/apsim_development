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
	/// Summary description for wfAdminMenu.
	/// </summary>
	public class wfAdminMenu : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.LinkButton btnClimateForcast;
		protected System.Web.UI.WebControls.LinkButton btnSQL;
		protected System.Web.UI.WebControls.LinkButton btnCrops;
		protected System.Web.UI.WebControls.LinkButton btnSoils;
		protected System.Web.UI.WebControls.LinkButton btnMetStations;
		protected System.Web.UI.WebControls.LinkButton btnBannerImages;
		protected System.Web.UI.WebControls.LinkButton btnDataManagement;
		protected System.Web.UI.WebControls.Label lblBannerImages;
		protected System.Web.UI.WebControls.Label lblClimateForcast;
		protected System.Web.UI.WebControls.Label lblCrops;
		protected System.Web.UI.WebControls.Label lblMetStations;
		protected System.Web.UI.WebControls.Label lblReportTemplates;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.Label Label3;
		protected System.Web.UI.WebControls.Label Label4;
		protected System.Web.UI.WebControls.LinkButton btnReportTemplates;
	


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
			this.btnClimateForcast.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnSQL.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnSoils.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMetStations.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnCrops.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportTemplates.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnBannerImages.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnDataManagement.Click += new System.EventHandler(this.NavigationButtonClick);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion




		#region Form Events
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			//Checks to ensure that only valid users are permitted to view the page
			FunctionsClass.CheckSession();
			FunctionsClass.CheckForAdministratorLevelPriviledges();
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
	}
}
