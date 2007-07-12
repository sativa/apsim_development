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
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblPaddockName;
		protected System.Web.UI.WebControls.Label lblCropManagement;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.CheckBox chkSown;
		protected System.Web.UI.WebControls.Label lblCrop;
		protected System.Web.UI.WebControls.Label lblCultivar;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected Janus.Web.GridEX.GridEX grdSowDate;
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDate;
		protected System.Data.DataColumn dcSowDate;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.CheckBox chkTriazine;
		protected System.Web.UI.WebControls.Label lblTriazine;
		protected System.Web.UI.WebControls.Label lblPopulation;
		protected System.Web.UI.WebControls.TextBox edtPopulation;
		protected System.Web.UI.WebControls.DropDownList cboRowConfiguration;
		protected System.Web.UI.WebControls.Label lblRowConfiguration;
		protected System.Web.UI.WebControls.Label lblPopulationUnit;
		protected System.Web.UI.WebControls.Label lblRowSpacing;
		protected System.Web.UI.WebControls.TextBox edtRowSpacing;
		protected System.Web.UI.WebControls.Label lblRowSpacingUnit;
		protected System.Web.UI.WebControls.DropDownList cboCultivars;



		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			System.Globalization.DateTimeFormatInfo.CurrentInfo.ShortDatePattern = "dd/MM/yyyy";
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.dsSowDate = new System.Data.DataSet();
			this.dtSowDate = new System.Data.DataTable();
			this.dcSowDate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.chkSown.CheckedChanged += new System.EventHandler(this.chkSown_CheckedChanged);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
			// 
			// dsSowDate
			// 
			this.dsSowDate.DataSetName = "NewDataSet";
			this.dsSowDate.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsSowDate.Tables.AddRange(new System.Data.DataTable[] {
																																	 this.dtSowDate});
			// 
			// dtSowDate
			// 
			this.dtSowDate.Columns.AddRange(new System.Data.DataColumn[] {
																																		 this.dcSowDate});
			this.dtSowDate.TableName = "SowDate";
			// 
			// dcSowDate
			// 
			this.dcSowDate.ColumnName = "SowDate";
			this.dcSowDate.DataType = typeof(System.DateTime);
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).EndInit();

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Sets up the page for display to the user
		//-------------------------------------------------------------------------
		private void InitialisePage()
			{
			DisplayGrowersName();
			ChangeEnableCropDetails(false);
			FillCropsCombo();
			FillCultivarsCombo();
			FillRowConfigurationCombo();
			DisplayCropTypeComponents();
			SetSowDate();
			}
		//-------------------------------------------------------------------------
		//Set the date shown in the sow date grid
		//-------------------------------------------------------------------------
		private void SetSowDate()
			{
			try
				{
				DataRow drSowDate;
				drSowDate = dsSowDate.Tables["SowDate"].NewRow();
				drSowDate["SowDate"] = DateTime.Today;
				dsSowDate.Tables["SowDate"].Rows.Add(drSowDate);
				this.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
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
		//Gets all the crop types from the database and then fills
		//the crops combo box with them.
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
			{
			DataTable dtCropList = DataAccessClass.GetUsersCrops(FunctionsClass.GetActiveUserName());
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
		//Fills the row configuration combo box with all the row configuration types form the database
		//-------------------------------------------------------------------------
		private void FillRowConfigurationCombo()
		{
			try
			{
				DataTable dtRowConfiguration = DataAccessClass.GetAllRowConfigurationTypes();
				cboRowConfiguration.DataSource = dtRowConfiguration;
				cboRowConfiguration.DataTextField = "Type";
				cboRowConfiguration.DataValueField = "Type";
				cboRowConfiguration.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
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
			grdSowDate.Enabled = bEnableCropDetails;
			}	
		//-------------------------------------------------------------------------
		//Displays visual components that are dependant on the type of crop selected
		//-------------------------------------------------------------------------
		private void DisplayCropTypeComponents()
		{
			SetVisabilityOfCanolaComponents();
			SetVisibilityOfSorgumComponents();
		}
		//-------------------------------------------------------------------------
		//Sets the visibility of the triazine option depending on the crop selected
		//-------------------------------------------------------------------------
		private void SetVisabilityOfCanolaComponents()
		{
			bool bTriazineVisibility = false;
			if(cboCrops.SelectedValue == "Canola")
				bTriazineVisibility = true;

			lblTriazine.Visible = bTriazineVisibility;
			chkTriazine.Visible = bTriazineVisibility;
		}	
		//-------------------------------------------------------------------------
		//Sets the visibility of the triazine option depending on the crop selected
		//-------------------------------------------------------------------------
		private void SetVisibilityOfSorgumComponents()
		{
			bool bSorgumComponentVisibility = false;
			if(cboCrops.SelectedValue == "Sorghum")
				bSorgumComponentVisibility = true;

			lblRowConfiguration.Visible = bSorgumComponentVisibility;
			cboRowConfiguration.Visible = bSorgumComponentVisibility;
			lblPopulation.Visible = bSorgumComponentVisibility;
			edtPopulation.Visible = bSorgumComponentVisibility;
			lblPopulationUnit.Visible = bSorgumComponentVisibility;
			lblRowSpacing.Visible = bSorgumComponentVisibility;
			edtRowSpacing.Visible = bSorgumComponentVisibility;
			lblRowSpacingUnit.Visible = bSorgumComponentVisibility;
		}	
		//-------------------------------------------------------------------------
		//Saves the new paddock's details to the database and the 
		//user is sent back to the ViewGrowers page.
		//-------------------------------------------------------------------------
		private void SavePaddock()
			{
			if(edtName.Text != "" && edtName.Text != null)
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
									(DateTime.ParseExact(grdSowDate.GetRow(0).Cells["SowDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
									cboCultivars.SelectedItem.Text, Convert.ToInt32(chkTriazine.Checked), cboRowConfiguration.SelectedValue, 
									ReturnPopulationValue(), InputValidationClass.ReturnTextBoxValueAsDouble(edtRowSpacing, 0),Session["SelectedUserName"].ToString());
								}
							//Saves only the paddock name and consultant ID
							else
								{
								DataAccessClass.InsertPaddock(InputValidationClass.ValidateString(edtName.Text), "", 
									cboCultivars.SelectedItem.Text, 0, "", 0, 0, Session["SelectedUserName"].ToString());
								}
							Server.Transfer("wfManageUsers.aspx");
							}
						else
							{
							throw new Exception("The selected user already has a paddock with this name");
							}
						}
					catch(Exception E)
						{
						FunctionsClass.DisplayMessage(Page, E.Message);
						}
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "Please ensure all fields are valid");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please enter a paddock name");
				}
			}
		//-------------------------------------------------------------------------
		//Returns the value of the Population text box after it is checked to insure 
		//that the value is a valid integer
		//-------------------------------------------------------------------------
		private int ReturnPopulationValue()
		{
			int iPopulation = 0;
			if(edtPopulation.Text != "")
			{
				if(InputValidationClass.IsInputAPositiveInteger(edtPopulation.Text))
				{
					iPopulation = Convert.ToInt32(edtPopulation.Text) / 10000;
				}
			}
			return iPopulation;
		}
		//---------------------------------------------------------------------------
		#endregion



		#region Form Events
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
		//-------------------------------------------------------------------------
		//When the user changes the crop type, the cultivars combo box is updated
		//to contain the cultivars of the selected crop
		//-------------------------------------------------------------------------
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillCultivarsCombo();
			DisplayCropTypeComponents();
			}
		//-------------------------------------------------------------------------
		//When the Sown check box is changed, the page is updated.
		//-------------------------------------------------------------------------
		private void chkSown_CheckedChanged(object sender, System.EventArgs e)
			{
			ChangeEnableCropDetails(chkSown.Checked);
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
			Server.Transfer("wfManageUsers.aspx");
			}
		//-------------------------------------------------------------------------
		//When the cancel image is pressed the user is sent back to the 
		//ViewGrowers page.
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfManageUsers.aspx");
			}
		//-------------------------------------------------------------------------	
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE