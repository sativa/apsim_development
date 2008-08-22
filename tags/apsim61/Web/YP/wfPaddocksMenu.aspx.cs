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
	/// Summary description for wfPaddocksMenu.
	/// </summary>
	public class wfPaddocksMenu : System.Web.UI.Page
	{


		protected System.Web.UI.WebControls.DropDownList cboPaddocks;
		protected System.Web.UI.WebControls.Label lblSelectPaddock;
		protected System.Web.UI.WebControls.Panel pnlHeading;
		protected System.Web.UI.WebControls.Label lblGrower;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksSoil;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksApplictions;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksRainfall;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksCrop;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.Label lblVisitors;
		protected System.Web.UI.WebControls.Label lblRainfallVis;
		protected System.Web.UI.WebControls.Label lblRainfallSub;
		protected System.Web.UI.WebControls.Label lblSubscribers;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksRainfallMain;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksSoilMain;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksApplicationsMain;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksCropMain;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksInformationMain;
		protected System.Web.UI.WebControls.Label lblAppsVis;
		protected System.Web.UI.WebControls.Label lblAppsSub;
		protected System.Web.UI.WebControls.Label lblSoilVis;
		protected System.Web.UI.WebControls.Label lblInfoSub;
		protected System.Web.UI.WebControls.Label lblInfoVis;
		protected System.Web.UI.WebControls.Label lblSoilSub;
		protected System.Web.UI.WebControls.Label lblCropVis;
		protected System.Web.UI.WebControls.Label lblCropSub;
		protected System.Web.UI.WebControls.Button btnDeletePaddock;
		protected System.Web.UI.WebControls.Button btnAddPaddock;
		protected System.Web.UI.WebControls.Panel pnlAdminOptions;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.ImageButton btnHelpPaddockMenuPage;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksInformation;
	


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
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationAwayButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationAwayButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationAwayButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationAwayButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationAwayButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationAwayButtonClick);
			this.btnPaddocksRainfall.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksApplictions.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksSoil.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksCrop.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksInformation.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksInformationMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksSoilMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksRainfallMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksApplicationsMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksCropMain.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnAddPaddock.Click += new System.EventHandler(this.btnAddPaddock_Click);
			this.btnDeletePaddock.Click += new System.EventHandler(this.btnDeletePaddock_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region From Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillPaddockCombo()
		{
			DataTable dtPaddocks = DataAccessClass.GetPaddocksOfUser(FunctionsClass.GetActiveUserName());
			cboPaddocks.DataSource = dtPaddocks;
			cboPaddocks.DataTextField = "Name";
			cboPaddocks.DataValueField = "Name";
			cboPaddocks.DataBind();

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetSelectedPaddock()
		{
			if(cboPaddocks.SelectedValue != "")
			{
				Session["SelectedPaddockName"] = cboPaddocks.SelectedValue;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetAdminPanel()
		{
			if(FunctionsClass.IsAdministrator() == true)
				pnlAdminOptions.Visible = true;
			else
				pnlAdminOptions.Visible = false;
		}
		//-------------------------------------------------------------------------
		//Delete the selected paddock
		//-------------------------------------------------------------------------
		private void DeletePaddock()
		{
			try
			{
				if(cboPaddocks.SelectedValue != "")
				{
					Session["SelectedPaddockName"] = "";
					DataAccessClass.DeletePaddock(cboPaddocks.SelectedValue, Session["SelectedUserName"].ToString());
					Session["SelectedPaddockName"] = "";
					Server.Transfer("wfPaddocksMenu.aspx");
				}
				//If no paddock is selected then an error message is displayed to the user
				else
					throw new Exception("No Paddock Selected");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
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

				HelpClass.SetHelpForPaddockMenuPage(btnHelpPaddockMenuPage);

				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FillPaddockCombo();
				SetAdminPanel();
				FunctionsClass.SetDisplayBanner(imgBanner);

				btnDeletePaddock.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected paddock) \");");
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			if(cboPaddocks.SelectedValue != "")
			{
				SetSelectedPaddock();
				Server.Transfer(((LinkButton)sender).CommandName);
			}
			else
			{
				FunctionsClass.DisplayMessage(Page, "Please select a paddock");
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationAwayButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnAddPaddock_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfAdminPaddockAdd.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDeletePaddock_Click(object sender, System.EventArgs e)
		{
		DeletePaddock();
		}
		//-------------------------------------------------------------------------
		#endregion


	}//END OF CLASS
}//END OF NAMESPACE
