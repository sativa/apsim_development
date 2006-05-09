using System;
using System.Web.Mail;
using System.Web.UI;
using System.Web;
using System.Data;

using System.Collections.Specialized;

namespace YP2006
	{
	/// <summary>
	/// Summary description for EmailClass.
	/// </summary>
	public class EmailClass
		{
		public EmailClass()
			{}
			
		//-------------------------------------------------------------------------
		//A generic function that takes in all the details needed to send an email
		//-------------------------------------------------------------------------
		public static bool SendEmail(string szEmailTo, string szEmailFrom, 
			string szSubject, string szBody, StringCollection scAttachments, MailPriority mpSend)
			{
			bool bSent = false;

			MailMessage mmEmailToSend = new MailMessage();
			MailAttachment maFileToAttach;
			
			mmEmailToSend.To = szEmailTo;
			mmEmailToSend.From = szEmailFrom;
			mmEmailToSend.Subject = szSubject;
			mmEmailToSend.Body = szBody;
			mmEmailToSend.Priority =  mpSend;
			if(scAttachments != null)
				{
				for(int iIndex = 0; iIndex < scAttachments.Count; iIndex++)
					{
					maFileToAttach = new MailAttachment(scAttachments[iIndex]);
					mmEmailToSend.Attachments.Add(maFileToAttach);
					}
				}
			System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
			SmtpMail.SmtpServer = Convert.ToString(settings["EmailServer"]);
			try
				{
				SmtpMail.Send(mmEmailToSend);
				bSent = true;
				}
			catch (Exception E)
				{throw E;}
			

			return bSent;
			}
		//-------------------------------------------------------------------------
		//Prepares the email text for an email that will be sent to the apsim run
		//machine.
		//-------------------------------------------------------------------------
		private static string PrepareReportEmailBody(string szReportName, string szUserName, string szPaddockName)
			{
			string szBody = "";
			
			DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(HttpContext.Current.Session["UserName"].ToString());
			string szUserEmail = dtUsersDetails.Rows[0]["Email"].ToString();
			string szApplicationName = HttpContext.Current.Request.ApplicationPath;
			//Remove the starting / character
			//EG: /YieldProphet becomes YieldProphet
			szApplicationName = szApplicationName.Remove(0,1);
			//Remove what ever file name that is currently appended to the applicationurl and 
			//replace it with the file name of the login page.
			//EG: http://www.YieldProphet.com.au/YieldProphet.html
			//becomes http://www.YieldProphet.com.au/wfLogin.apsx
			string szApplicationURL = HttpContext.Current.Request.Url.ToString();
			int iLengthOfNewString = szApplicationURL.LastIndexOf("/");
			szApplicationURL = szApplicationURL.Remove(iLengthOfNewString, (szApplicationURL.Length - iLengthOfNewString));
			szApplicationURL = szApplicationURL+"/wfLogin.aspx";
			szApplicationURL = szApplicationURL.Replace("http://", "");
			//Remove the http:// and any file name of directory structure that follows the 
			//root address.
			//EG: http://www.YieldProphet.com.au/YP/YieldProphet.html
			//becomes www.YieldProphet.com.au
			string szApplicationFTP = szApplicationURL;
			iLengthOfNewString = szApplicationFTP.IndexOf("/");
			szApplicationFTP = szApplicationFTP.Remove(iLengthOfNewString, (szApplicationFTP.Length - iLengthOfNewString));
			//Sets the directory to be that of the user
			string szReportDirectory = "YP/Reports/"+szUserName+"/"+DateTime.Today.Year.ToString();
			
			szBody = "username="+szUserName+"~~\r\n"+
				"paddockname="+szPaddockName+"~~\r\n"+
				"useremail="+szUserEmail+"~~\r\n"+
				"applicationname="+szApplicationName+"~~\r\n"+
				"applicationurl="+szApplicationURL+"~~\r\n"+
				"reportdescription="+szReportName+"~~\r\n"+
				"applicationftp="+szApplicationFTP+"~~\r\n"+
				"reportdirectory="+szReportDirectory+"~~\r\n";
			
			return szBody;	
			}
		//-------------------------------------------------------------------------
		//Sends a report email that doesn't uses data from the database
		//(IE: Climate report)
		//-------------------------------------------------------------------------
		public static bool SendReportEmail(string szReportName,	string szCropType, 
			string szReportType, string szReportXML, string szUserName, string szPaddockName)
			{
			bool bReportSent = false;
			System.Collections.Specialized.NameValueCollection settings = 
				(System.Collections.Specialized.NameValueCollection)System.
				Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
			string szSendEmailTo = Convert.ToString(settings["ReportEmailAddressTo"]);
			string szRecieveEmailFrom = Convert.ToString(settings["ReportEmailAddressFrom"]);
			string szSubject = Convert.ToString(settings["ReportEmailSubject"]);
			string szBody = PrepareReportEmailBody(szReportName, szUserName, szPaddockName);
			StringCollection scAttachments = new StringCollection();

			//Ensure that the directory exists for the report to be sent back to
			if(ReportClass.DoesUsersReportDirectoryExisit(szUserName, DateTime.Today.Year) == false)
			{
				ReportClass.CreateUsersReportDirectory(szUserName.ToString(), DateTime.Today.Year); 
			}

			//Generates the report files
			scAttachments = ReportClass.PrepareReportFiles(szReportType, szCropType, 
				szReportName, szReportXML, szUserName, szPaddockName);

			//Makes sure that all the files have been generated for the report
			//int iNumberOfFilesNeededForReport = 6;
			//if(scAttachments.Count == iNumberOfFilesNeededForReport)
			//{
			if(SendEmail(szSendEmailTo, szRecieveEmailFrom, szSubject, szBody, 
				scAttachments, MailPriority.High))
				{
				bReportSent = true;
				}
				//Email consultants to let them know that a report request has been made
				DataTable dtConsultants = DataAccessClass.GetUsersConsultants(HttpContext.Current.Session["UserName"].ToString());
			foreach(DataRow drConsultant in dtConsultants.Rows)
				{
				if(Convert.ToBoolean(Convert.ToInt32(drConsultant["Email"])) == true)
					{
					DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(HttpContext.Current.Session["UserName"].ToString());
					DataTable dtConsultantDetails = DataAccessClass.GetDetailsOfUser(drConsultant["UserName"].ToString());
					if(dtConsultantDetails.Rows.Count > 0 && dtUsersDetails.Rows.Count > 0)
						{
						SendEmail(dtConsultantDetails.Rows[0]["Email"].ToString(), "Apsimrun@csiro.au", "Automated reply from YP", 
							dtUsersDetails.Rows[0]["Name"].ToString() +" has requested a report", null, MailPriority.Normal);
						}
					}
				}
				//}
			return bReportSent;
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
