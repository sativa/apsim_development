using System;
using System.Collections;
using System.Collections.Specialized;
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
	/// Summary description for wfRegForm2.
	/// </summary>
	public class wfRegForm2 : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.TextBox FirstName;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.TextBox SecondName;
		protected System.Web.UI.WebControls.Label Label3;
		protected System.Web.UI.WebControls.TextBox Address;
		protected System.Web.UI.WebControls.Label Label4;
		protected System.Web.UI.WebControls.TextBox Phone;
		protected System.Web.UI.WebControls.TextBox Mobile;
		protected System.Web.UI.WebControls.Label Label6;
		protected System.Web.UI.WebControls.TextBox Fax;
		protected System.Web.UI.WebControls.Label Label7;
		protected System.Web.UI.WebControls.TextBox Email;
		protected System.Web.UI.WebControls.Label Label8;
		protected System.Web.UI.WebControls.Label Label9;
		protected System.Web.UI.WebControls.Label Label10;
		protected System.Web.UI.WebControls.TextBox PaddockName1;
		protected System.Web.UI.WebControls.TextBox PaddockName2;
		protected System.Web.UI.WebControls.TextBox TextBox4;
		protected System.Web.UI.WebControls.Label Label11;
		protected System.Web.UI.WebControls.TextBox PaddockName3;
		protected System.Web.UI.WebControls.TextBox TextBox6;
		protected System.Web.UI.WebControls.TextBox TextBox7;
		protected System.Web.UI.WebControls.Label Label12;
		protected System.Web.UI.WebControls.TextBox TextBox8;
		protected System.Web.UI.WebControls.TextBox TextBox9;
		protected System.Web.UI.WebControls.TextBox SoilType3;
		protected System.Web.UI.WebControls.TextBox SoilType1;
		protected System.Web.UI.WebControls.TextBox SoilType2;
		protected System.Web.UI.WebControls.Label Label13;
		protected System.Web.UI.WebControls.TextBox Crop1;
		protected System.Web.UI.WebControls.TextBox Crop2;
		protected System.Web.UI.WebControls.TextBox Crop3;
		protected System.Web.UI.WebControls.Label Label14;
		protected System.Web.UI.WebControls.TextBox Variety1;
		protected System.Web.UI.WebControls.TextBox Variety2;
		protected System.Web.UI.WebControls.TextBox Variety3;
		protected System.Web.UI.WebControls.Label Label15;
		protected System.Web.UI.WebControls.TextBox MetStation;
		protected System.Web.UI.WebControls.HyperLink HyperLink1;
		protected System.Web.UI.WebControls.Label Label16;
		protected System.Web.UI.WebControls.CheckBox Yes;
		protected System.Web.UI.WebControls.CheckBox No;
		protected System.Web.UI.WebControls.Label ConsultantLabel1;
		protected System.Web.UI.WebControls.TextBox ConsultantName;
		protected System.Web.UI.WebControls.Label ConsultantLabel2;
		protected System.Web.UI.WebControls.TextBox ConsultantPhone;
		protected System.Web.UI.WebControls.Button SubmitButton;
		protected System.Web.UI.WebControls.HyperLink JamesEmail;
		protected System.Web.UI.WebControls.Label Label5;
	
		private void Page_Load(object sender, System.EventArgs e)
		{
			// Put user code to initialize the page here
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
			this.SubmitButton.Click += new System.EventHandler(this.SubmitButton_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		// ---------------------------------------------------------------
		// user has clicked submit - send email with registration details.
		// ---------------------------------------------------------------
		private void SubmitButton_Click(object sender, System.EventArgs e)
			{
			string withConsultant;
			if (Yes.Checked)
				withConsultant = "yes";
			else
				withConsultant = "no";
			string body = "First name       = " + FirstName.Text + "\r\n" 
				        + "Second name      = " + SecondName.Text + "\r\n"
						+ "Postal Address   = " + Address.Text + "\r\n"
						+ "Phone            = " + Phone.Text + "\r\n"
						+ "Mobile           = " + Mobile.Text + "\r\n"
						+ "Fax              = " + Fax.Text + "\r\n"
						+ "Email            = " + Email.Text + "\r\n"
						+ "Weather station  = " + MetStation.Text + "\r\n"
						+ "With consultant  = " + withConsultant + "\r\n"
						+ "Consultant name  = " + ConsultantName.Text + "\r\n"
						+ "Consultant phone = " + ConsultantPhone.Text + "\r\n"
						+ "\r\n"
						+ "PADDOCK1\r\n"
						+ "   Name          = " + PaddockName1.Text + "\r\n"
						+ "   Soil type     = " + SoilType1.Text + "\r\n"
						+ "   Crop          = " + Crop1.Text + "\r\n"
						+ "   Variety       = " + Variety1.Text + "\r\n"
						+ "\r\n"
						+ "PADDOCK2\r\n"
						+ "   Name          = " + PaddockName2.Text + "\r\n"
						+ "   Soil type     = " + SoilType2.Text + "\r\n"
						+ "   Crop          = " + Crop2.Text + "\r\n"
						+ "   Variety       = " + Variety2.Text + "\r\n"
						+ "\r\n"
						+ "PADDOCK3\r\n"
						+ "   Name          = " + PaddockName3.Text + "\r\n"
						+ "   Soil type     = " + SoilType3.Text + "\r\n"
						+ "   Crop          = " + Crop3.Text + "\r\n"
						+ "   Variety       = " + Variety3.Text + "\r\n";

			EmailClass.SendEmail("dean.holzworth@csiro.au", "yieldprophet.com.au", 
				                 "Yield Prophet online registration",
								 body, new StringCollection(), System.Web.Mail.MailPriority.Normal);
			Server.Transfer("wfRegForm3.aspx");
			
			}

	}
}
