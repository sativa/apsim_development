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
	/// Summary description for wfViewDropDowns.
	/// </summary>
	public class wfViewDropDowns : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.DropDownList cboDropDownTypes;
		protected System.Web.UI.WebControls.Label lblDropDownTypes;
		protected System.Web.UI.WebControls.Label lblDropDownValue;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.ListBox lstDropDownValues;
		protected System.Web.UI.WebControls.ImageButton btnAddValueImg;
		protected System.Web.UI.WebControls.ImageButton btnDeleteValueImg;
		protected System.Web.UI.WebControls.LinkButton btnAddValue;
		protected System.Web.UI.WebControls.LinkButton btnDeleteValue;
		protected System.Web.UI.WebControls.ImageButton btnAddTypeImg;
		protected System.Web.UI.WebControls.LinkButton btnAddType;
		protected System.Web.UI.WebControls.ImageButton btnDeleteTypeImg;
		protected System.Web.UI.WebControls.LinkButton btnDeleteType;



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
			this.btnDeleteValue.Click += new System.EventHandler(this.btnDeleteValue_Click);
			this.btnDeleteType.Click += new System.EventHandler(this.btnDeleteType_Click);
			this.btnDeleteTypeImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteTypeImg_Click);
			this.btnAddType.Click += new System.EventHandler(this.btnAddType_Click);
			this.btnAddTypeImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnAddTypeImg_Click);
			this.btnAddValue.Click += new System.EventHandler(this.btnAddValue_Click);
			this.btnDeleteValueImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteValueImg_Click);
			this.btnAddValueImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnAddValueImg_Click);
			this.cboDropDownTypes.SelectedIndexChanged += new System.EventHandler(this.cboDropDownTypes_SelectedIndexChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Fills the form with data from the database
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			FillDropDownTypesCombo();
			FillDropDownValuesList();
			}
		//-------------------------------------------------------------------------
		//Fills the drop down values list with all the drop down values that correspond
		//to the selected drop down type
		//-------------------------------------------------------------------------
		private void FillDropDownValuesList()
			{
			//If a drop down type is selected then fill the list
			if(cboDropDownTypes.SelectedValue != null && cboDropDownTypes.SelectedValue != "")
				{
				string szTableName = cboDropDownTypes.SelectedValue.ToString(); 
				DataTable dtDropDownValues = DataAccessClass.GetAllTypesAndIDsFromTable(szTableName);
				lstDropDownValues.DataSource = dtDropDownValues;
				lstDropDownValues.DataTextField = "Type";
				lstDropDownValues.DataValueField = "ID";
				lstDropDownValues.DataBind();
				}
			//If no drop down type is selected then display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No drop down type selected");
				}
			}
		//-------------------------------------------------------------------------
		//Fills the drop down types combo box with all the drop down types from the
		//database
		//-------------------------------------------------------------------------
		private void FillDropDownTypesCombo()
			{
			DataTable dtDropDownTypes = DataAccessClass.GetAllDropDownTypes();
			cboDropDownTypes.DataSource = dtDropDownTypes;
			cboDropDownTypes.DataTextField = "Type";
			cboDropDownTypes.DataValueField = "TableName";
			cboDropDownTypes.DataBind();
			}
		//-------------------------------------------------------------------------
		//Deletes the selected type from the database
		//-------------------------------------------------------------------------
		private void DeleteType()
			{
			//If a drop down type is selected then remove it from the database
			if(cboDropDownTypes.SelectedValue != null && cboDropDownTypes.SelectedValue != "")
				{
				DataAccessClass.DeleteDropDownType(InputValidationClass.ValidateString(cboDropDownTypes.SelectedItem.Text));
				Server.Transfer("wfViewDropDowns.aspx");
				}
			//If no drop down type is selected then display and error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No type selected");
				}
			}
		//-------------------------------------------------------------------------
		//Deletes the selected value from the database
		//-------------------------------------------------------------------------
		private void DeleteValue()
			{
			//If a drop down value is selected then remove it from the database
			if(lstDropDownValues.SelectedValue != null && lstDropDownValues.SelectedValue != "")
				{
				try
					{
					DataAccessClass.DeleteRecordFromTable(lstDropDownValues.SelectedValue, cboDropDownTypes.SelectedValue);
					Server.Transfer("wfViewDropDowns.aspx");
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no drop down value is selected then display and error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No value selected");
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
			//Adds an attribute to the two delete buttons that causes a 
			//confirmation warning to appear when the user presses the buttons
			btnDeleteValue.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected value \");");
			btnDeleteType.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected type \");");
			}
		//-------------------------------------------------------------------------
		//When the user selects a different drop down type the drop down values
		//list is updated.
		//-------------------------------------------------------------------------
		private void cboDropDownTypes_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillDropDownValuesList();
			}
		//-------------------------------------------------------------------------
		//When the user presses the add type button, they are transfered to the 
		// Add Drop Down Type page
		//-------------------------------------------------------------------------
		private void btnAddType_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfAddDropDownType.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the add type button, they are transfered to the 
		// Add Drop Down Type page
		//-------------------------------------------------------------------------
		private void btnAddTypeImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfAddDropDownType.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete type button, the selected drop down type 
		//is removed.
		//-------------------------------------------------------------------------
		private void btnDeleteType_Click(object sender, System.EventArgs e)
			{
			DeleteType();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete type image, the selected drop down type 
		//is removed.
		//-------------------------------------------------------------------------
		private void btnDeleteTypeImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeleteType();
			}
		//-------------------------------------------------------------------------
		//When the user presses the add button, they are transfered to the 
		// Add Drop Down Value page
		//-------------------------------------------------------------------------
		private void btnAddValue_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfAddDropDownValue.aspx");
		}
		//-------------------------------------------------------------------------
		//When the user presses the add button, they are transfered to the 
		// Add Drop Down Value page
		//-------------------------------------------------------------------------
		private void btnAddValueImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfAddDropDownValue.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete value button, the selected drop down 
		//value is removed.
		//-------------------------------------------------------------------------
		private void btnDeleteValue_Click(object sender, System.EventArgs e)
			{
			DeleteValue();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete value image, the selected drop down 
		//value is removed.
		//-------------------------------------------------------------------------
		private void btnDeleteValueImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeleteValue();
			}
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
