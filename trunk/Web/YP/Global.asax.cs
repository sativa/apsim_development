using System;
using System.Collections;
using System.ComponentModel;
using System.Web;
using System.Web.SessionState;

namespace YP2006 
{
	/// <summary>
	/// Summary description for Global.
	/// </summary>
	public class Global : System.Web.HttpApplication
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		static public string szDatabaseDateFormat = "yyyy-MM-dd";
		static public string szDisplayDateFormat = "dd/MM/yyyy";

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void RegisterChartComponent()
		{
			Xceed.Chart.Server.Licenser.LicenseKey = "CHW30-N7A8U-RTTLJ-FABA";
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void CreateSessionVariables()
		{
			//Initialise the Session information variables we will need
			Session.Add("UserName", "");
			Session.Add("SelectedUserName", "");
			Session.Add("SelectedPaddockName", "");
			Session.Add("AccessType", "");
			Session.Add("BannerImage", "");
			//Set the time out variable (in minutes)
			Session.Timeout = 30;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public Global()
		{
			RegisterChartComponent();
			InitializeComponent();
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Application_Start(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Session_Start(Object sender, EventArgs e)
		{
			CreateSessionVariables();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Application_BeginRequest(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Application_EndRequest(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Application_AuthenticateRequest(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Application_Error(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Session_End(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		protected void Application_End(Object sender, EventArgs e)
		{

		}
		//-------------------------------------------------------------------------
			
		#region Web Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.components = new System.ComponentModel.Container();
		}
		#endregion
	}
}

