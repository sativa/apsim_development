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
	/// Summary description for wfAddDropDownValue.
	/// </summary>
	public class wfAddDropDownValue : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblDropDownTypes;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.DropDownList cboDropDownTypes;
		protected System.Web.UI.WebControls.TextBox edtDropDownValue;
		protected System.Web.UI.WebControls.Label lblDropDownValue;
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FillDropDownTypesCombo();
				FunctionsClass.SetControlFocus("edtDropDownValue", this);
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
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion
		
		//-------------------------------------------------------------------------
		//Fills the cboDropDownTypes combo box with all the different types
		//from the database
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
		//Save a new dropdown value to the database and linked to the coresponding 
		//dropdown type.  The user is then transfered back to the ViewDropDowns page.
		//-------------------------------------------------------------------------
		private void SaveDropDownValue()
			{
			if(cboDropDownTypes.SelectedValue != null && cboDropDownTypes.SelectedValue != "" &&
				edtDropDownValue.Text != "")
				{
				DataAccessClass.InsertDropDownValue(InputValidationClass.ValidateString(edtDropDownValue.Text), 
					cboDropDownTypes.SelectedValue);
				Server.Transfer("wfViewDropDowns.aspx");
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please ensure all fields are entered");
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button a new dropdown value is saved
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveDropDownValue();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button a new dropdown value is saved
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveDropDownValue();
			}
		//-------------------------------------------------------------------------
		//When the user presses the Cancel button they are transfered back to
		//the ViewDropDowns page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfViewDropDowns.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the Cancel image they are transfered back to
		//the ViewDropDowns page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfViewDropDowns.aspx");
			}
		//-------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
