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
	/// Summary description for wfAddDropDownType.
	/// </summary>
	public class wfAddDropDownType : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.Label lblDropDownType;
		protected System.Web.UI.WebControls.TextBox edtDropDownType;
		protected System.Web.UI.WebControls.TextBox edtTableName;
		protected System.Web.UI.WebControls.Label lblTableName;

		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the users
		//permissions are checked
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FunctionsClass.SetControlFocus("edtDropDownType", this);
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
		//Saves a new dropDown type to the database and the user is then 
		//sent back to the ViewDropDowns page
		//-------------------------------------------------------------------------
		private void SaveDropDown()
			{
			if(edtDropDownType.Text != "" && edtTableName.Text != "")
				{
				DataAccessClass.InsertDropDownType(InputValidationClass.ValidateString(edtDropDownType.Text), 
					InputValidationClass.ValidateString(edtTableName.Text));
				Server.Transfer("wfViewDropDowns.aspx");
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please ensure that all fields are filled");
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the details are saved
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveDropDown();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save image the details are saved
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveDropDown();
			}
		//-------------------------------------------------------------------------
		//If the Cancel button is pressed then the user is transfered to the
		//DropDowns page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfViewDropDowns.aspx");
			}
		//-------------------------------------------------------------------------
		//If the Cancel image is pressed then the user is transfered to the
		//DropDowns page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfViewDropDowns.aspx");
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMEPACE
