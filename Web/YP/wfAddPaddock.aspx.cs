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
	/// Summary description for wfAddPaddock.
	/// </summary>
	public class wfAddPaddock : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblPaddockName;
		protected System.Web.UI.WebControls.Label lblCropManagement;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.CheckBox chkSown;
		protected System.Web.UI.WebControls.Calendar cldSowDate;
		protected System.Web.UI.WebControls.Label lblCrop;
		protected System.Web.UI.WebControls.Label lblCultivar;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.DropDownList cboCultivars;

		//---------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the users
		//permissions are checked and the page is initialised
		//---------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForConsultantLevelPriviledges();
				InitialisePage();
				FunctionsClass.SetControlFocus("edtName", this);
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
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.chkSown.CheckedChanged += new System.EventHandler(this.chkSown_CheckedChanged);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		//-------------------------------------------------------------------------
		//Sets up the page for display to the user
		//-------------------------------------------------------------------------
		private void InitialisePage()
			{
			DisplayGrowersName();
			ChangeEnableCropDetails(false);
			FillCropsCombo();
			FillCultivarsCombo();
			cldSowDate.SelectedDate = DateTime.Today;
			cldSowDate.VisibleDate = DateTime.Today;
			}
		//-------------------------------------------------------------------------
		//Gets the name of the user from the database and sets it the name label
		//-------------------------------------------------------------------------
		private void DisplayGrowersName()
			{
			try
				{
				DataTable dtGrowersDetails = DataAccessClass.GetDetailsOfUser(Session["SelectedUserName"].ToString());
				lblName.Text = dtGrowersDetails.Rows[0]["Name"].ToString();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//When the Sown check box is changed, the page is updated.
		//-------------------------------------------------------------------------
		private void chkSown_CheckedChanged(object sender, System.EventArgs e)
			{
			ChangeEnableCropDetails(chkSown.Checked);
			}
		//-------------------------------------------------------------------------
		//If the sown check box is set to true, the components that take
		//sowing informaition are enabled, if the checkbox is set to false
		//then the components that take sowing information are disabled
		//-------------------------------------------------------------------------
		private void ChangeEnableCropDetails(bool bEnableCropDetails)
			{
			cboCrops.Enabled = bEnableCropDetails;
			cboCultivars.Enabled = bEnableCropDetails;
			cldSowDate.Enabled = bEnableCropDetails;
			}		
		//-------------------------------------------------------------------------
		//Gets all the crop types from the database and then fills
		//the crops combo box with them.
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
			{
			DataTable dtCropList = DataAccessClass.GetAllCrops();
			cboCrops.DataSource = dtCropList;
			cboCrops.DataTextField = "Type";
			cboCrops.DataBind();
			}
		//-------------------------------------------------------------------------
		//Gets all the culitvar types for the corresponding crop types
		//from the database and then fills the cultivars combo box with them.
		//-------------------------------------------------------------------------
		private void FillCultivarsCombo()
			{
			//Makes sure that there is a selected crop
			if(cboCrops.SelectedItem.Text != null && cboCrops.SelectedItem.Text != "")
				{
				string szSelectedCrop = cboCrops.SelectedItem.Text;
				DataTable dtCultivarList = DataAccessClass.GetAllCultivarsOfCrop(szSelectedCrop);
				cboCultivars.DataSource = dtCultivarList;
				cboCultivars.DataTextField = "Type";
				cboCultivars.DataBind();
				}
			}
		//-------------------------------------------------------------------------
		//When the user changes the crop type, the cultivars combo box is updated
		//to contain the cultivars of the selected crop
		//-------------------------------------------------------------------------
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillCultivarsCombo();
			}
		//-------------------------------------------------------------------------
		//Saves the new paddock's details to the database and the 
		//user is sent back to the ViewGrowers page.
		//-------------------------------------------------------------------------
		private void SavePaddock()
			{
			if(edtName.Text != "")
				{
				if(cboCultivars.SelectedItem.Text != "")
					{
					try
						{
						if(DataAccessClass.IsPaddockNameAvailable(edtName.Text, Session["SelectedUserName"].ToString()) == true)
							{
							//Saves all the details including sowing details
							if(chkSown.Checked == true)
								{
								DataAccessClass.InsertPaddock(InputValidationClass.ValidateString(edtName.Text), 
									cldSowDate.SelectedDate.ToString("yyyy-MM-dd"), cboCultivars.SelectedItem.Text, 
									Session["SelectedUserName"].ToString());
								}
							//Saves only the paddock name and consultant ID
							else
								{
								DataAccessClass.InsertPaddock(InputValidationClass.ValidateString(edtName.Text), "", 
									cboCultivars.SelectedItem.Text, Session["SelectedUserName"].ToString());
								}
							Server.Transfer("wfViewGrowers.aspx");
							}
						else
							{
							FunctionsClass.DisplayMessage(Page, "The selected user already has a paddock with this name");
							}
						}
					catch(Exception E)
						{
						FunctionsClass.DisplayMessage(Page, E.Message);
						}
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "Please select a cultivar");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please enter a paddock name");
				}
			}
		//-------------------------------------------------------------------------
		//When the save button is pressed the paddock details are saved
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SavePaddock();
			}
		//-------------------------------------------------------------------------
		//When the save button is pressed the paddock details are saved
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SavePaddock();
			}
		//-------------------------------------------------------------------------
		//When the cancel button is pressed the user is sent back to the 
		//ViewGrowers page.
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfViewGrowers.aspx");
			}
		//-------------------------------------------------------------------------
		//When the cancel image is pressed the user is sent back to the 
		//ViewGrowers page.
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfViewGrowers.aspx");
			}
		//-------------------------------------------------------------------------	
		}//END OF CLASS
	}//END OF NAMESPACE
