This read me file is intended to help other developers who are doing work on 
the Yield Prophet web application, some of it may be obvious but hopefully it will
helpful to you.


Frames:  All of the Yield Prophet site (except the login page) is held in a
frames page (YieldProphet.htm).  On the left there is side panel which displays the 
options available to the user and on the right there is the main panel which displays 
the page selected by the user.   


Session: A web application is stateless, therefore global variables don't work
as would be expected in the a normal windows application.  Every time a button is
pressed or a component that has the 'AutoPostBack' property set to true is used
the page will redisplay which means that everything is started from scratch.  The
components on the page remember their settings if the 'EnableViewState' property is
set to true.  But as your global variables don't have an EnableViewState property
they are reset.  There are a few ways to save these variables, in Yield Prophet
we are using a Session which stores the data in the memory of the server.
Our session details are initialised in: "Global.asax" in the function "Session_Start"
They are:
Session["UserID"]
Session["SelectedUserID"]
Session["SelectedPaddockID"]
Session["SelectedReport"]
Session["SelectedReportYear"]

Session["UserID"]: The global variable UserID is used to store the id of the logged
in user.  It is initialised to 0 and is only modified during the login process, where
it is set with the user's UserID from the database

Session["SelectedUserID"]: The global variable SelectedUserID is used as a temporary
store for the UserID of the selected grower.  It is initialised to 0 and is only modified
on the page wfViewGrowers.aspx when a consultant wishes to edit one of their grower's
details or when they wish to add a paddock to one of their growers or when they view the
reports of one of their growers.  The only pages were this variable should be used are the
wfEditGrower.aspx, wfAddPaddock.aspx or wfViewReports.aspx pages. 

Session["SelectedPaddockID"]: The global varible SelectedPaddockID is used as a temporary
store for the PaddockID for the selected paddock.  It is initialised to 0.
This variable is set when a user selects a paddock on the side bar or 
when a consultant selects a grower's paddock on the wfViewGrowers.aspx page to edit it.

Session["SelectedReport"] The global variable SelectedReport is used to store the
report name of the selected report, it is set at the wfViewReports.aspx page when a user 
selects to either view or rename a report.

Session["SelectedReportYear"] The global variable SelectedReportYear is used to store the
year of the selected report, it is set at the wfViewReports.aspx page when a user 
selects to either view or rename a report.


Database Access:
All access to the database goes through the class "DataAccessClass".
All the functions are static so an object of the class need not be
created.  The database is a MS Access database, but is set up to 
be easily transferred across to an MS SQL database and as a result a connection
is made for every query then dropped staight after the query has executed to
keep the connection pool as full as possible.


Report Storage:
Report are stored in a report folder on the webserver as .gif files.  The reports are
organised by userID and by the year in which they where generated
For example reports generated in 2005 for Stephen van Rees who has a userID of 1 will
be stored in the following directory:
/Reports/1/2005/


Grids:
The grids are all hooked up to a dataset and for a grid to display five empty rows
then five empty rows must be added to the dataset.  There seems to be a large problem
displaying these grids through different browers.


Browser Support:
By default only IE is recognised as a high level browser by any .NET web application.  This means
that you have to have code that tells the application to also recognise other browsers as
high level browers or the pages aren't displayed correctly.  This code is located in the 
web.config file under the browsercaps section.  Currently we have code for Firefox and
Safari (apple) but it isn't a total solution.  Some small things don't look exactly the same, but
mostly it is acceptable.  The only major browser problem remains with the grids.  
As of today (15/02/2005) the grids that have a calendar drop down don't work in Firefox, 
and none of the grids work in Safari.
Help is available at the janus support forum http://www.janusys.com/controls/


Page_Load Event:
On the Page_Load event of each page please ensure that you check the
session details and if appropriated the priviledges.
This can be done by using the following code:
if (!IsPostBack)//Checks to see if this is the first time the page has been loaded
	{	
	FunctionsClass.CheckSession();	//Checks the session to ensure a login has taken place
	FunctionsClass.CheckFor.......LevelPriviledges();//Checks the permissions of the user
	}
This is very important.  If the CheckSession() function isn't called, then 
there is no way to determine if the user has logged in to the application
or has instead navigated directly to this page.  Depending on the page you need to
check that the user has appropriate priviledges for the page.


Changeable Settings:
There are few setting such as database connection, email server and email address that are
set in the web.config file that can be modified whilst the application is running.
To change this values go to the bottom of the web.config file.  The following is an example
of how to access these variables through code:
System.Collections.Specialized.NameValueCollection settings = 
(System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
string szEmailServer = Convert.ToString(settings["EmailServer"]);
