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
	/// Summary description for wfAdminClimateForecast.
	/// </summary>
	public class wfAdminClimateForecast : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Label lblDavidsDiscription;
		protected System.Web.UI.WebControls.TextBox edtDavidsDescription;
		protected System.Web.UI.WebControls.TextBox edtAnalogueYearFive;
		protected System.Web.UI.WebControls.TextBox edtAnalogueYearFour;
		protected System.Web.UI.WebControls.TextBox edtAnalogueYearThree;
		protected System.Web.UI.WebControls.TextBox edtAnalogueYearTwo;
		protected System.Web.UI.WebControls.Label lblAnalogueYears;
		protected System.Web.UI.WebControls.TextBox edtAnalogueYearOne;
		protected System.Web.UI.WebControls.Label lblSOIDescription;
		protected System.Web.UI.WebControls.TextBox edtSOIDescription;
		protected System.Web.UI.WebControls.DropDownList cboSOIPhase;
		protected System.Web.UI.WebControls.Label lblSOIPhase;
		protected System.Web.UI.WebControls.DropDownList cboSOIMonth;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Label lblSOIMonth;


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
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//---------------------------------------------------------------------------
		//Fills all the components on the form with data from the database
		//---------------------------------------------------------------------------
		private void FillForm()
		{
			FillSOIPhasesCombo();
			try
			{
				DataTable dtClimateForecast = DataAccessClass.GetClimateForecast();
				if(dtClimateForecast.Rows.Count > 0)
				{
					edtSOIDescription.Text = dtClimateForecast.Rows[0]["SoiDescription"].ToString();
					edtAnalogueYearOne.Text = dtClimateForecast.Rows[0]["DavidsYearOne"].ToString();
					edtAnalogueYearTwo.Text = dtClimateForecast.Rows[0]["DavidsYearTwo"].ToString();
					edtAnalogueYearThree.Text = dtClimateForecast.Rows[0]["DavidsYearThree"].ToString();
					edtAnalogueYearFour.Text = dtClimateForecast.Rows[0]["DavidsYearFour"].ToString();
					edtAnalogueYearFive.Text = dtClimateForecast.Rows[0]["DavidsYearFive"].ToString();
					edtDavidsDescription.Text = dtClimateForecast.Rows[0]["DavidsDescription"].ToString();
					cboSOIMonth.SelectedValue = dtClimateForecast.Rows[0]["SoiMonth"].ToString();
					cboSOIPhase.SelectedValue = dtClimateForecast.Rows[0]["SoiPhase"].ToString();
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------------
		//Fills the SOI Phases Combo box
		//---------------------------------------------------------------------------
		private void FillSOIPhasesCombo()
		{
			DataTable dtSOIPhases = DataAccessClass.GetAllSOIPhases();
			cboSOIPhase.DataSource = dtSOIPhases;
			cboSOIPhase.DataTextField = "Type";
			cboSOIPhase.DataValueField = "Type";
			cboSOIPhase.DataBind();
		}
		//---------------------------------------------------------------------------
		//The Climate forecast is saved, but firstly a check is run to determine if a climate
		//forecast has already been saved to the database, if there is no other
		//climate forecast in the database a new climate forecast is inserted into
		//the database, if there is already a climate forecast in the database,
		//then the existing record is updated with the new values.  This ensures
		//that there is only every one climate forecast record in the database.
		//---------------------------------------------------------------------------
		private void SaveClimateForecast()
		{
			try
			{
				//Checks that the years entered are integers.
				if(InputValidationClass.IsInputAPositiveInteger(edtAnalogueYearOne.Text) &&
					InputValidationClass.IsInputAPositiveInteger(edtAnalogueYearTwo.Text) && 
					InputValidationClass.IsInputAPositiveInteger(edtAnalogueYearThree.Text) && 
					InputValidationClass.IsInputAPositiveInteger(edtAnalogueYearFour.Text) && 
					InputValidationClass.IsInputAPositiveInteger(edtAnalogueYearFive.Text))
				{
				
					DataAccessClass.SetClimateForecast(cboSOIMonth.SelectedValue, 
						cboSOIPhase.SelectedItem.Text, edtAnalogueYearOne.Text, 
						edtAnalogueYearTwo.Text, edtAnalogueYearThree.Text, edtAnalogueYearFour.Text,
						edtAnalogueYearFive.Text, InputValidationClass.ValidateString(edtSOIDescription.Text), 
						InputValidationClass.ValidateString(edtDavidsDescription.Text));
	
				}
				else
					throw new Exception("One or more of the year fields contains an invalid year");
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
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if (!IsPostBack)
			{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FillForm();
			}
		}
		//---------------------------------------------------------------------------
		//When the user presses the save button the climate forecast is saved
		//---------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			SaveClimateForecast();
		}
		//---------------------------------------------------------------------------
		//When the user presses the cancel button, the page is reloaded so all changes
		//are lost and the page is how it was originally
		//---------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfEditClimateForecast.aspx");
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
