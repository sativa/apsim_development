using System;
using System.Collections;
using System.ComponentModel;
using System.Web;
using System.Web.SessionState;

namespace YieldProphet 
{
	/// <summary>
	/// Summary description for Global.
	/// </summary>
	public class Global : System.Web.HttpApplication
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		//private System.ComponentModel.IContainer components = null;

		public Global()
		{
			InitializeComponent();
		}	
		
		protected void Application_Start(Object sender, EventArgs e)
		{
			

		}
		//-------------------------------------------------------------------------
		//Initialises the session variables and the session timeout property
		//-------------------------------------------------------------------------
		protected void Session_Start(Object sender, EventArgs e)
		{
			//Initialise the Session information variables we will need
			Session.Add("UserName", "");
			Session.Add("SelectedUserName", "");
			Session.Add("SelectedPaddockName", "");
			Session.Add("SelectedReportName", "");
			Session.Add("SelectedReportYear", "0");
			//Set the time out variable (in minutes)
			Session.Timeout = 30;
		}

		protected void Application_BeginRequest(Object sender, EventArgs e)
		{

		}

		protected void Application_EndRequest(Object sender, EventArgs e)
		{

		}

		protected void Application_AuthenticateRequest(Object sender, EventArgs e)
		{

		}

		protected void Application_Error(Object sender, EventArgs e)
		{

		}

		protected void Session_End(Object sender, EventArgs e)
		{

		}

		protected void Application_End(Object sender, EventArgs e)
		{

		}
			
		#region Web Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    

		}
		#endregion
	}
}

