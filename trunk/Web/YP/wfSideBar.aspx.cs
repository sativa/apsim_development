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
	/// Summary description for wfSideBar.
	/// </summary>
	public class wfSideBar : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Panel pnlSideBar;
		protected System.Web.UI.WebControls.Panel pnlGrower;
		protected System.Web.UI.WebControls.HyperLink hylUserDetails;
		protected System.Web.UI.WebControls.HyperLink hylUserDetailsImg;
		protected System.Web.UI.WebControls.Label lblGrower;
		protected System.Web.UI.WebControls.DropDownList cboPaddocks;
		protected System.Web.UI.WebControls.Button btnView;
		protected System.Web.UI.WebControls.Button btnViewReports;
		protected System.Web.UI.WebControls.ImageButton btnViewReportsImg;
		protected System.Web.UI.WebControls.ImageButton btnViewPaddocksImg;
		protected System.Web.UI.WebControls.Panel Panel1;
		protected System.Web.UI.WebControls.HyperLink hylSQLImg;
		protected System.Web.UI.WebControls.HyperLink hylSQL;
		protected System.Web.UI.WebControls.HyperLink hylDropDownsImg;
		protected System.Web.UI.WebControls.HyperLink hylDropDowns;
		protected System.Web.UI.WebControls.HyperLink hylAddUserImg;
		protected System.Web.UI.WebControls.HyperLink hylAddUser;
		protected System.Web.UI.WebControls.Label lblAdministration;
		protected System.Web.UI.WebControls.HyperLink hylReportTemplatesImg;
		protected System.Web.UI.WebControls.HyperLink hylReportTemplates;
		protected System.Web.UI.WebControls.HyperLink hylCropsImg;
		protected System.Web.UI.WebControls.HyperLink hylCrops;
		protected System.Web.UI.WebControls.HyperLink hylRegionsImg;
		protected System.Web.UI.WebControls.HyperLink hylRegions;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.HyperLink hylGrowersImg;
		protected System.Web.UI.WebControls.HyperLink hylGrowers;
		protected System.Web.UI.WebControls.HyperLink hylClimateForecast;
		protected System.Web.UI.WebControls.HyperLink hylClimateForecastImg;
		protected System.Web.UI.WebControls.HyperLink Hyperlink12;
		protected System.Web.UI.WebControls.HyperLink Hyperlink11;
		protected System.Web.UI.WebControls.HyperLink Hyperlink10;
		protected System.Web.UI.WebControls.HyperLink Hyperlink9;
		protected System.Web.UI.WebControls.HyperLink Hyperlink8;
		protected System.Web.UI.WebControls.HyperLink Hyperlink7;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.HyperLink Hyperlink6;
		protected System.Web.UI.WebControls.HyperLink Hyperlink5;
		protected System.Web.UI.WebControls.HyperLink Hyperlink4;
		protected System.Web.UI.WebControls.HyperLink Hyperlink3;
		protected System.Web.UI.WebControls.HyperLink Hyperlink2;
		protected System.Web.UI.WebControls.HyperLink Hyperlink1;
		protected System.Web.UI.WebControls.Label lblConsultant;
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{
				SetSideBarStyle(DataAccessClass.GetAccessTypeOfUser(Session["UserName"].ToString()));
				//Set the side panel to be the the height of the browser window
				pnlSideBar.Height = Unit.Percentage(100);
				DisplayUsersPaddocks();
				btnViewReports.Style.Add("cursor", "hand");

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
			this.btnView.Click += new System.EventHandler(this.btnView_Click);
			this.btnViewReports.Click += new System.EventHandler(this.btnViewReports_Click);
			this.btnViewReportsImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnViewReportsImg_Click);
			this.btnViewPaddocksImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnViewPaddocksImg_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		//-------------------------------------------------------------------------
		//Sets up the side bar to show the options associated with each permission
		//level
		//-------------------------------------------------------------------------
		public void SetSideBarStyle(string szUserAccess)
			{
			switch(szUserAccess)
				{
				//If the access type is that of an administrator then all the
				//options are available
				case FunctionsClass.szAdministrator:
					pnlGrower.Visible = true;
					pnlConsultant.Visible = true;
					pnlAdministration.Visible = true;
					break;
				//If the access type is that of a consultant then the consultant
				//and grower options are available
				case FunctionsClass.szConsultant:
					pnlGrower.Visible = true;
					pnlConsultant.Visible = true;
					pnlAdministration.Visible = false;
					break;
				//If the access type is that of a grower then only the grower
				//options are available
				case FunctionsClass.szGrower:
					pnlGrower.Visible = true;
					pnlConsultant.Visible = false;
					pnlAdministration.Visible = false;
					break;
				//If the access type is that of a visitor then only the grower
				//options are available
				case FunctionsClass.szVisitor:
					pnlGrower.Visible = true;
					pnlConsultant.Visible = false;
					pnlAdministration.Visible = false;
					hylUserDetails.Enabled = false;
					hylUserDetailsImg.Enabled = false;
					break;
				//If the access type is unknown then no options are available
				default:
					pnlGrower.Visible = false;
					pnlConsultant.Visible = false;
					pnlAdministration.Visible = false;
					break;
				}
			}
		//-------------------------------------------------------------------------
		//Fills the paddocks combo with all the user's paddocks
		//-------------------------------------------------------------------------
		public void DisplayUsersPaddocks()
			{
			//If the user is logged in correctly then fill the paddocks combo
			if(Session != null && Session["UserName"].ToString() != "0")
				{
				try
					{
					DataTable dtPaddocks = DataAccessClass.GetPaddocksOfUser(Session["UserName"].ToString());
					cboPaddocks.DataSource = dtPaddocks;
					cboPaddocks.DataTextField = "Name";
					cboPaddocks.DataBind();
					//Disables the View button if there are no paddocks to select
					//This stops a bug from occuring (When the user presses the View
					//button with no paddock selected the side bar is shown twice
					if(cboPaddocks.Items.Count == 0)
						{
						btnView.Enabled = false;
						}
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}	
			//If they are not logged in correctly then send them to the session
			//timeout page
			else
				{
				Server.Transfer("wfSessionTimeOut.aspx");
				}
			}
		//-------------------------------------------------------------------------
		//Users are transfered to the view reports page
		//-------------------------------------------------------------------------
		private void ViewReports()
			{
			Session["SelectedUserName"] = "";
			Server.Transfer("wfViewReports.aspx");
			}
		//-------------------------------------------------------------------------
		//Users are transfered to the edit paddock page
		//-------------------------------------------------------------------------
		private void ViewPaddocks()
			{
			//If a paddock is selected, set it in as a Session variable and transfer
			//the user to the edit paddock page
			if(cboPaddocks.SelectedItem.Text != "" && cboPaddocks.SelectedItem.Text != null)
				{
				Session["SelectedUserName"] = "";
				Session["SelectedPaddockName"] = cboPaddocks.SelectedItem.Text;
				Server.Transfer("wfEditPaddock.aspx");
				}
			//If no paddock is selected, display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page,"No paddock selected");
				}
			}	
		//-------------------------------------------------------------------------
		//When the user presses the view button, they are transfered to the edit
		//paddock page
		//-------------------------------------------------------------------------
		private void btnView_Click(object sender, System.EventArgs e)
			{
			ViewPaddocks();
			}
		//-------------------------------------------------------------------------
		//When the user presses the paddock button, they are transfered to the edit
		//paddock page
		//-------------------------------------------------------------------------
		private void btnViewPaddocksImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			ViewPaddocks();
			}
		//-------------------------------------------------------------------------
		//When the user presses the report button, they are transfered to the view
		//reports page
		//-------------------------------------------------------------------------
		private void btnViewReports_Click(object sender, System.EventArgs e)
			{
			ViewReports();
			}
		//-------------------------------------------------------------------------
		//When the user presses the report image, they are transfered to the view
		//reports page
		//-------------------------------------------------------------------------
		private void btnViewReportsImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			ViewReports();
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
