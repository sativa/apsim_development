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
	/// Summary description for wfRegistrationSupportedConsultant.
	/// </summary>
	public class wfRegistrationSupportedConsultant : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label Label6;
		protected System.Web.UI.WebControls.TextBox edtBusinessName;
		protected System.Web.UI.WebControls.Label Label4;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtSecondName;
		protected System.Web.UI.WebControls.TextBox edtFirstName;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label Label14;
		protected System.Web.UI.WebControls.Label Label12;
		protected System.Web.UI.WebControls.Label Label7;
		protected System.Web.UI.WebControls.Label Label10;
		protected System.Web.UI.WebControls.Label Label3;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Label Label1;
		protected Janus.Web.GridEX.EditControls.IntegerUpDown edtNumberOfGrowers;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.TextBox edtFax;
		protected System.Web.UI.WebControls.TextBox edtMobile;
		protected System.Web.UI.WebControls.TextBox edtPhone;
		protected System.Web.UI.WebControls.TextBox edtPostCode;
		protected System.Web.UI.WebControls.Label Label16;
		protected System.Web.UI.WebControls.TextBox edtTown;
		protected System.Web.UI.WebControls.Label Label15;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressThree;
		protected System.Web.UI.WebControls.Label Label13;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressTwo;
		protected System.Web.UI.WebControls.Label Label11;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressOne;
		protected System.Web.UI.WebControls.Label Label5;
		protected System.Web.UI.WebControls.HyperLink hylRegistrationMeu;
		protected System.Web.UI.WebControls.HyperLink hylHome;
		protected System.Web.UI.WebControls.Button btnNext;
	


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
			this.btnNext.Click += new System.EventHandler(this.btnNext_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region Form Functions
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		public int ReturnNumberOfGrowers()
		{
			return Convert.ToInt32(edtNumberOfGrowers.Value);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		public string ReturnEmailBody()
		{
			string szBody = RegistrationClass.ReturnEmailBody(edtFirstName.Text, edtSecondName.Text, null, edtPostalAddressOne.Text,
				edtPostalAddressTwo.Text, edtPostalAddressThree.Text, edtTown.Text, edtPostCode.Text, edtPhone.Text,
				edtMobile.Text, edtFax.Text, edtEmail.Text, null, null, null, null, "Supported Consultant");
			return szBody;
		}
		#endregion



		#region Form Events
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				edtNumberOfGrowers.Value = 1;
			}
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void btnNext_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfRegistrationSupportedConsultantGrowers.aspx");
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfRegistrationMenu.aspx");
		}
		//---------------------------------------------------------------------

		#endregion

	}//END OF CLASS
}//END OF NAMESPACE
