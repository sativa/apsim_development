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
	/// Summary description for wfAdminMetStationsEdit.
	/// </summary>
	public class wfAdminMetStationsEdit : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Label lblStationNumber;
		protected System.Web.UI.WebControls.TextBox edtStationNumber;
		protected System.Web.UI.WebControls.TextBox edtStationName;
		protected System.Web.UI.WebControls.Label lblStationName;
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
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion
	

		
		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StoreMetStationSelection()
		{
			try
			{
				wfAdminMetStationsView MetStationsView = (wfAdminMetStationsView) Context.Handler;
				ViewState["PreviousMetStationName"] = MetStationsView.ReturnSelectedMetStation();
				ViewState["RegionName"] = MetStationsView.ReturnSelectedRegion();
				edtStationName.Text = ViewState["PreviousMetStationName"].ToString();
			}
			catch(Exception)
			{}
		}	
		//-------------------------------------------------------------------------
		//Fills the station name and the station number edit boxes
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			StoreMetStationSelection();
			try
			{
				if(edtStationName.Text != "")
				{
					DataTable dtMetStation = DataAccessClass.GetMetStation(edtStationName.Text);
					if(dtMetStation.Rows.Count > 0)
					{
						edtStationNumber.Text = dtMetStation.Rows[0]["StationNumber"].ToString();
					}
					else
						throw new Exception("Can not find specified met station");
				}
				else
					throw new Exception("Can not find specified met station");

			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Saves the changes to the station name and number and then transfers the 
		//user back to the view regions page.
		//-------------------------------------------------------------------------
		private void SaveMetStation()
		{
			try
			{
				if(edtStationName.Text != "" && edtStationNumber.Text != "")
				{
					if(InputValidationClass.IsInputAPositiveInteger(edtStationNumber.Text))
					{
						DataAccessClass.UpdateMetStation(ViewState["RegionName"].ToString(),
							ViewState["PreviousMetStationName"].ToString(),
							Convert.ToInt32(edtStationNumber.Text),
							InputValidationClass.ValidateString(edtStationName.Text));
						Server.Transfer("wfAdminMetStationsView.aspx");
					}
					else
						throw new Exception("Station number must be a positive integer");
				}
				else
					throw new Exception("Station name or station number field is blank");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, InputValidationClass.StripSingleQuote(E.Message));
			}
		}
		//-------------------------------------------------------------------------
		//Returns the selected region, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnSelectedRegion()
		{
			return ViewState["RegionName"].ToString();
		} 
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if (!IsPostBack)
			{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				ViewState["PreviousMetStationName"] = "";
				ViewState["RegionName"] = "";
				FillForm();
			}
		}
		//-------------------------------------------------------------------------
		//When the user presses the save button, the changes are saved and they are
		//transfered back to the view regions page.
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			SaveMetStation();
		}

		//-------------------------------------------------------------------------
		//When the user presses the cancel button, they are transfered back to the 
		//view regions page.
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfAdminMetStationsView.aspx");
		}

		//-------------------------------------------------------------------------
		#endregion

	}//END OF CLASS
}//END OF NAMESPACE
