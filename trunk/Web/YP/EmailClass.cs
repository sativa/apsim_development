using System;
using System.Web.Mail;
using System.Web.UI;
using System.Web;
using System.Data;

using System.Collections.Specialized;

namespace YieldProphet
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
			try
				{
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
				SmtpMail.Send(mmEmailToSend);
				bSent = true;
				}
			catch(Exception)
				{}
			return bSent;
			}
		//-------------------------------------------------------------------------
		//Prepares the email text for an email that will be sent to the apsim run
		//machine.
		//-------------------------------------------------------------------------
		private static string PrepareReportEmailBody(string szReportName)
			{
			string szBody = "";
			try
				{
				string szPaddockID = HttpContext.Current.Session["SelectedPaddockID"].ToString();
				string szUserID = DataAccessClass.GetUserIDOfPaddock(szPaddockID).ToString();
				
				string szUserName = DataAccessClass.GetUserNameOfUser(szUserID);
				string szPaddockName = DataAccessClass.GetNameOfPaddock(szPaddockID);
				string szUserEmail = DataAccessClass.GetEmailOfUser(szUserID);
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
				string szReportDirectory = "YP/Reports/"+szUserID+"/"+DateTime.Today.Year.ToString();
				
				szBody = "username="+szUserName+"~~\r\n"+
					"paddockname="+szPaddockName+"~~\r\n"+
					"useremail="+szUserEmail+"~~\r\n"+
					"applicationname="+szApplicationName+"~~\r\n"+
					"applicationurl="+szApplicationURL+"~~\r\n"+
					"reportdescription="+szReportName+"~~\r\n"+
					"applicationftp="+szApplicationFTP+"~~\r\n"+
					"reportdirectory="+szReportDirectory+"~~\r\n";
				}
			catch(Exception)
				{}
			return szBody;	
			}
		//-------------------------------------------------------------------------
		//Sends a report email that doesn't uses data from the database
		//(IE: Climate report)
		//-------------------------------------------------------------------------
		public static bool SendReportEmail(string szReportName,	 
			string szReportTypeID, string szReportType, bool bEmailConParFiles)
			{
			bool bReportSent = false;
			try
				{
				System.Collections.Specialized.NameValueCollection settings = 
					(System.Collections.Specialized.NameValueCollection)System.
					Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				string szSendEmailTo = Convert.ToString(settings["ReportEmailAddressTo"]);
				string szRecieveEmailFrom = Convert.ToString(settings["ReportEmailAddressFrom"]);
				string szSubject = Convert.ToString(settings["ReportEmailSubject"]);
				string szBody = PrepareReportEmailBody(szReportName);
				StringCollection scAttachments = new StringCollection();
				if(szReportType == ReportClass.szAgronomicReport)
					{
					scAttachments = ReportClass.PrepareAgronomicReportFiles(szReportTypeID, szReportName);
					}
				else if(szReportType == ReportClass.szClimateReport)
					{
					scAttachments = ReportClass.PrepareClimateReportFiles(szReportTypeID, szReportName);
					}
				//Makes sure that all the files have been generated for the report
				int iNumberOfFilesNeededForReport = 5;
				if(scAttachments.Count == iNumberOfFilesNeededForReport)
					{
					if(SendEmail(szSendEmailTo, szRecieveEmailFrom, szSubject, szBody, 
						scAttachments, MailPriority.High))
						{
						bReportSent = true;
						}
					}
				}
			catch(Exception)
				{}
			return bReportSent;
			}
		//-------------------------------------------------------------------------
		//Sends a nitrogen comparison report email.  This takes in data from the
		//generate nitrogen report page that is used to generate the 
		//report files that are sent to the apsimrun machine
		//-------------------------------------------------------------------------
		public static bool SendNitrogenComparisonReportEmail(string szReportName,	 
			string szReportTypeID, string szReportType, bool bEmailConParFiles, 
			DataTable dtScenarioOne, string szScenarioOneDescription,
			DataTable dtScenarioTwo, string szScenarioTwoDescription,
			DataTable dtScenarioThree, string szScenarioThreeDescription)
			{
			bool bReportSent = false;
			try
				{
				System.Collections.Specialized.NameValueCollection settings = 
					(System.Collections.Specialized.NameValueCollection)System.
					Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				string szSendEmailTo = Convert.ToString(settings["ReportEmailAddressTo"]);
				string szRecieveEmailFrom = Convert.ToString(settings["ReportEmailAddressFrom"]);
				string szSubject = Convert.ToString(settings["ReportEmailSubject"]);
				string szBody = PrepareReportEmailBody(szReportName);
				StringCollection scAttachments = ReportClass.PrepareNitrogenComparisonReportFiles(szReportTypeID, szReportName, 
				dtScenarioOne, szScenarioOneDescription, dtScenarioTwo, szScenarioTwoDescription, dtScenarioThree, szScenarioThreeDescription);
				//Makes sure that all the files have been generated for the report
				int iNumberOfFilesNeededForReport = 5;
				if(scAttachments.Count == iNumberOfFilesNeededForReport)
					{
					if(SendEmail(szSendEmailTo, szRecieveEmailFrom, szSubject, szBody, 
						scAttachments, MailPriority.High))
						{
						bReportSent = true;
						}
					}
				}
			catch(Exception)
				{}
			return bReportSent;
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
