//---------------------------------------------------------------------------
#ifndef DataH
#define DataH
#include <string>
#include <vector>
#include <boost\date_time\gregorian\gregorian.hpp>
#include <db.hpp>
namespace Adodb {
   class TADOConnection;
   };

class TWebSession;
//---------------------------------------------------------------------------
// This class encapsulates the non-gui data and logic behind the FarmWeb
// web site.
//---------------------------------------------------------------------------
class Data
   {
   public:
      Data(void);
      ~Data(void);

      //---------------------------------------------------------------------------
      // Open the specified database file. Will close any existing open connections.
      // Will throw a runtime_error on error (e.g. file not found)
      //---------------------------------------------------------------------------
      void open(const std::string& fileName);

      //---------------------------------------------------------------------------
      // Execute the specified SQL statement.
      //---------------------------------------------------------------------------
      void executeSQL(const std::string& sql);


      //---------------------------------------------------------------------------
      //-- LOOKUP VALUES ----------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Get the allowable data lookup names.
      //---------------------------------------------------------------------------
      void getLookupNames(std::vector<std::string>& lookupNames) const;
      //---------------------------------------------------------------------------
      // Set the allowable data values for the specified dataname
      //---------------------------------------------------------------------------
      void setLookupValues(const std::string& dataName,
                           const std::vector<std::string>& lookupValues);
      //---------------------------------------------------------------------------
      // Get the allowable data values for the specified dataname
      //---------------------------------------------------------------------------
      void getLookupValues(const std::string& dataName,
                           std::vector<std::string>& lookupValues) const;

      //---------------------------------------------------------------------------
      // Delete the lookup values for the specified dataname
      //---------------------------------------------------------------------------
      void deleteLookupValues(const std::string& dataName);

      //---------------------------------------------------------------------------
      //-- USER METHODS -----------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Add an admin user. If user already exists then the method throws runtime_error.
      //---------------------------------------------------------------------------
      void addAdminUser(const std::string& name,
                        const std::string& userName,
                        const std::string& password,
                        const std::string& email);
      //---------------------------------------------------------------------------
      // Add an consultant user. If user already exists then the method throws runtime_error.
      //---------------------------------------------------------------------------
      void addConsultantUser(const std::string& name,
                             const std::string& userName,
                             const std::string& password,
                             const std::string& email);
      //---------------------------------------------------------------------------
      // Add an normal (grower) user. If user already exists then the method throws runtime_error.
      // If consultant name doesn't exist then throw runtime_error.
      //---------------------------------------------------------------------------
      void addUser(const std::string& name,
                   const std::string& userName,
                   const std::string& password,
                   const std::string& consulantName,
                   const std::string& email);
      //---------------------------------------------------------------------------
      // Change user password.
      //---------------------------------------------------------------------------
      void changeUserPassword(const std::string& userName,
                              const std::string& password);

      enum UserType {user, consultant, administrator};
      //---------------------------------------------------------------------------
      // Return true if specified user is of the specified type. Will throw if
      // userName is not valid.
      //---------------------------------------------------------------------------
      bool userIsOfType(const std::string& userName, UserType userType);
      //---------------------------------------------------------------------------
      // Return a list of users.
      //---------------------------------------------------------------------------
      void getUsers(std::vector<std::string>& userNames);
      //---------------------------------------------------------------------------
      // Return a list of users for the specified consultant. Throws if consultantName
      // is not a valid consultant.
      //---------------------------------------------------------------------------
      void getUsersForConsultant(const std::string& consultantName,
                                 std::vector<std::string>& userNames);
      //---------------------------------------------------------------------------
      // Delete a user. If userName doesn't exist then method throws runtime_error
      //---------------------------------------------------------------------------
      void deleteUser(const std::string& userName);
      //---------------------------------------------------------------------------
      // Return true if login is ok.
      //---------------------------------------------------------------------------
      bool loginOk(const std::string& userName, const std::string& password) const;
      //---------------------------------------------------------------------------
      // Return the user's email address.
      //---------------------------------------------------------------------------
      std::string getUserEmail(const std::string& userName) const;
      void setUserEmail(const std::string& userName, const std::string& email);
      //---------------------------------------------------------------------------
      // Return the user's consultant's email address(s)
      //---------------------------------------------------------------------------
      void getConsultantEmailAddresses(const std::string& userName, std::vector<std::string>& emailAddresses) const;

      //---------------------------------------------------------------------------
      // Return the user's details.
      //---------------------------------------------------------------------------
      void getUserDetails(const std::string& userName,
                          std::string& name,
                          std::string& email,
                          std::string& password,
                          std::string& userType) const;

      //---------------------------------------------------------------------------
      // Return the name of the user given a userID. Throws an runtime_error if user
      // not found.
      //---------------------------------------------------------------------------
      std::string getNameOfUser(const std::string& userName) const;
      void setNameOfUser(const std::string& userName, const std::string& name);


      //---------------------------------------------------------------------------
      //-- PADDOCK METHODS --------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Return a list of all paddocks for user.
      //---------------------------------------------------------------------------
      void getPaddocks(const std::string& userName,
                       std::vector<std::string>& paddockNames) const;
      //---------------------------------------------------------------------------
      // Add a paddock in the database for the specified user. If user doesn't
      // exist or paddock already exists then the method will throw runtime_error.
      //---------------------------------------------------------------------------
      void addPaddock(const std::string& userName,
                      const std::string& paddockName);
      //---------------------------------------------------------------------------
      // Delete the specified paddock in the database for the specified user. Will
      // throw if user or paddock name are invalid.
      //---------------------------------------------------------------------------
      void deletePaddock(const std::string& userName,
                         const std::string& paddockName);


      //---------------------------------------------------------------------------
      //-- DATA METHODS -----------------------------------------------------------
      //---------------------------------------------------------------------------
      struct Property
         {
         Property(const std::string& n, const std::string& v)
            : name(n), value(v) { }
         bool operator== (const std::string& rhs)
            {return (strcmpi(name.c_str(), rhs.c_str()) == 0);}

         std::string name;
         std::string value;
         };
      typedef std::vector<Property> Properties;
      //---------------------------------------------------------------------------
      // Return all paddock properties to caller. If user or paddock name is invalid
      // the the method throws runtime_error.
      //---------------------------------------------------------------------------
      void getProperties(const std::string& userName,
                         const std::string& paddockName,
                         Properties& properties) const;

      //---------------------------------------------------------------------------
      // Set the paddock properties for the specified user and paddock. If user or paddock
      // doesn't exist then the method will throw runtime_error. If the data already
      // exists, then the data value is updated.
      //---------------------------------------------------------------------------
      void setProperties(const std::string& userName,
                         const std::string& paddockName,
                         const Properties& properties);
      //---------------------------------------------------------------------------
      // Return a specific property value.
      //---------------------------------------------------------------------------
      std::string getProperty(const string& userName,
                              const string& paddockName,
                              const string& propertyName);
      //---------------------------------------------------------------------------
      // Set a specific property value.
      //---------------------------------------------------------------------------
      void setProperty(const std::string& userName,
                       const std::string& paddockName,
                       const std::string& propertyName,
                       const std::string& propertyValue);
      //---------------------------------------------------------------------------
      // Delete a specific property value.
      //---------------------------------------------------------------------------
      void deleteProperty(const string& userName,
                          const string& paddockName,
                          const string& propertyName);

      //---------------------------------------------------------------------------
      //-- TEMPORAL DATA METHODS --------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // This structure is used to get and add temporal data.
      //---------------------------------------------------------------------------
      struct TemporalData
         {
         TemporalData(boost::gregorian::date d, const std::string& v, const std::string& t)
            : date(d), value(v), type(t) { }
         boost::gregorian::date date;
         std::string value;
         std::string type;
         };
      typedef std::vector<TemporalData> TemporalValues;
      //---------------------------------------------------------------------------
      // Add some temporal data for the specified user and paddock. If user or paddock
      // doesn't exist then the method will throw runtime_error. If the data already
      // exists on the specified dates, then the data will be changed.
      //---------------------------------------------------------------------------
      void addTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           const std::string& dataName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           const TemporalValues& values);
      //---------------------------------------------------------------------------
      // Add some temporal data for the specified user and paddock. If user or paddock
      // doesn't exist then the method will throw runtime_error. If the data already
      // exists on the specified dates, then the data will be changed.
      //---------------------------------------------------------------------------
      void addTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           const TemporalValues& values);
      //---------------------------------------------------------------------------
      // Get some temporal data for the specified user and paddock. If user or paddock
      // doesn't exist then the method will throw runtime_error.
      //---------------------------------------------------------------------------
      void getTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           const std::string& dataName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           TemporalValues& values);

      //---------------------------------------------------------------------------
      // Get some temporal data for the specified user and paddock. If user or paddock
      // doesn't exist then the method will throw runtime_error.
      //---------------------------------------------------------------------------
      void getTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           TemporalValues& values);

      //---------------------------------------------------------------------------
      // Generate a temporal data file in APSIM format containing columns
      // for the specified data names. If user or paddock
      // doesn't exist then the method will throw runtime_error.
      // Section name is written to the data file on the first line - it should
      // not have [ ] characters around it.
      //---------------------------------------------------------------------------
      void generateDataFile(const std::string& userName,
                            const std::string& paddockName,
                            const std::vector<std::string>& dataNames,
                            const std::string& fileName,
                            const std::string& sectionName);
      //---------------------------------------------------------------------------
      // Generate a temporal data file in APSIM format containing columns
      // for the specified data names. If user or paddock
      // doesn't exist then the method will throw runtime_error.
      // Section name is written to the data file on the first line - it should
      // not have [ ] characters around it.
      // This method will produce a complete data file with every day between
      // start and end date and will pad with zeros for any dates missing in database.
      //---------------------------------------------------------------------------
      void generateCompleteDataFile(const std::string& userName,
                                    const std::string& paddockName,
                                    const std::vector<std::string>& dataNames,
                                    const std::string& fileName,
                                    const std::string& sectionName,
                                    boost::gregorian::date startDate,
                                    boost::gregorian::date endDate);

      //---------------------------------------------------------------------------
      //-- REPORT generation methods ---------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Return a list of report template names.
      //---------------------------------------------------------------------------
      void getReportTemplateNames(std::vector<std::string>& templates);

      //---------------------------------------------------------------------------
      // Return the number of report template pages for the specified report.
      //---------------------------------------------------------------------------
      unsigned getNumReportTemplatePages(const std::string& reportName);

      //---------------------------------------------------------------------------
      // Return the template contents for the specified report. Throws if
      // report doesn't exist.
      //---------------------------------------------------------------------------
      std::string getReportTemplate(const std::string& reportName,
                                    unsigned pageNumber);

      //---------------------------------------------------------------------------
      // Set the template contents for the specified report. Overwrites
      // existing template if it already exists.
      //---------------------------------------------------------------------------
      void setReportTemplate(const std::string& reportName,
                             const std::string& templateContents,
                             unsigned pageNumber);

      //---------------------------------------------------------------------------
      // Delete the specified report template
      //---------------------------------------------------------------------------
      void deleteReportTemplate(const std::string& reportName,
                                unsigned pageNumber);

      //---------------------------------------------------------------------------
      // Generate all report files for the specified user and paddock.
      // If user name or paddock name or report name are invalid then the
      // method will throw runtime_error.
      // Method returns a list of all files generated.
      //---------------------------------------------------------------------------
      void generateReportFiles(const std::string& userName,
                               const std::string& paddockName,
                               const std::string& reportName,
                               const Properties& reportProperties,
                               const std::string& outputDirectory,
                               std::vector<std::string>& filesGenerated,
                               const std::string& reportDescription);
      //---------------------------------------------------------------------------
      // Generate a soil file and return its filename. Returns true if the
      // soil water parameters were bounded.
      //---------------------------------------------------------------------------
      bool generateSoilFile(const std::string& userName,
                            const std::string& paddockName,
                            const std::string& outputDirectory,
                            bool withSW,
                            std::string& fileGenerated);

      //---------------------------------------------------------------------------
      //-- REPORT access methods ---------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Return a list of reports to caller.
      //---------------------------------------------------------------------------
      void getReports(const std::string& reportBaseDirectory,
                      const std::string& userName,
                      std::vector<std::string>& reportNames);


      // --------------------------------------------------
      // Return a file name for the report.
      // --------------------------------------------------
      std::string Data::getReportFileName(const std::string& reportBaseDirectory,
                                          const std::string& userName,
                                          const std::string& reportName);

      //---------------------------------------------------------------------------
      // Delete the specified report file for the specified user.
      //---------------------------------------------------------------------------
      void deleteReport(const std::string& reportBaseDirectory,
                        const std::string& userName,
                        const std::string& reportName);

      //---------------------------------------------------------------------------
      // Rename the specified report file for the specified user.
      //---------------------------------------------------------------------------
      void renameReport(const std::string& reportBaseDirectory,
                        const std::string& userName,
                        const std::string& oldReportName,
                        const std::string& newReportName);



      //---------------------------------------------------------------------------
      //-- REGION access methods ----------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Add a region. Will throw if it already exists.
      //---------------------------------------------------------------------------
      void addRegion(const std::string& name);

      //---------------------------------------------------------------------------
      // Delete a region. Will throw if it doesn't exist.
      //---------------------------------------------------------------------------
      void deleteRegion(const std::string& name);

      //---------------------------------------------------------------------------
      // Return a list of regions
      //---------------------------------------------------------------------------
      void getRegions(std::vector<std::string>& names);




      //---------------------------------------------------------------------------
      //-- SOIL access methods ----------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Return a list of soils to caller.  Will throw if region not found.
      //---------------------------------------------------------------------------
      void getSoils(const std::string& regionName, std::vector<std::string>& names) const;

      //---------------------------------------------------------------------------
      // Set the data of the specified soil. If the soil already exists, then
      // its data will be overwritten. Will throw if region doesn't exist
      //---------------------------------------------------------------------------
      void setSoil(const std::string& regionName, const std::string& name, const std::string& data);

      //---------------------------------------------------------------------------
      // Get the data of the specified soil. Will throw if the soil or region doesn't exist.
      //---------------------------------------------------------------------------
      std::string getSoil(const std::string& regionName, const std::string& name) const;

      //---------------------------------------------------------------------------
      // Delete the specified soil if it exists. Will NOT throw if the soil doesn't exist.
      //---------------------------------------------------------------------------
      void deleteSoil(const std::string& regionName, const std::string& name);

      //---------------------------------------------------------------------------
      //-- MET STATIONS access methods ----------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Add a met station. Will throw if it already exists.
      //---------------------------------------------------------------------------
      void addMetStation(const std::string& regionName, const std::string& name, unsigned stationNumber);

      //---------------------------------------------------------------------------
      // Delete a met station. 
      //---------------------------------------------------------------------------
      void deleteMetStation(const std::string& regionName, const std::string& name);

      //---------------------------------------------------------------------------
      // Return a list of met stations
      //---------------------------------------------------------------------------
      void getMetStations(const std::string& regionName, std::vector<std::string>& names);

      //---------------------------------------------------------------------------
      //-- CLIMATE Forecast -------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Return all climate forecast information.
      //---------------------------------------------------------------------------
      void getClimateForecast(unsigned& phase, unsigned& phaseMonth,
                              unsigned& davidSYear1, unsigned& davidSYear2,
                              unsigned& davidSYear3, unsigned& davidSYear4,
                              unsigned& davidSYear5,
                              string& soiDescription,
                              string& davidsDescription) const;

      //---------------------------------------------------------------------------
      // set climate forecast information.
      //---------------------------------------------------------------------------
      void setClimateForecast(unsigned soiPhase, unsigned soiPhaseMonth,
                              unsigned davidSYear1, unsigned davidSYear2,
                              unsigned davidSYear3, unsigned davidSYear4,
                              unsigned davidSYear5,
                              const std::string& soiDescription,
                              const std::string& davidsDescription);
      //---------------------------------------------------------------------------
      // Return all climate forecast properties to caller.
      //---------------------------------------------------------------------------
      void getClimateForecastProperties(Properties& properties) const;

      //---------------------------------------------------------------------------
      // Return the apsim run machine's email address.
      //---------------------------------------------------------------------------
      std::string getApsimRunEmailAddress() const;

      //---------------------------------------------------------------------------
      // Return the help url.
      //---------------------------------------------------------------------------
      std::string getHelpUrl() const;

      Adodb::TADOConnection* connectn() {return connection;}

      //---------------------------------------------------------------------------
      // Return the special url that lives under afloman directory.
      //---------------------------------------------------------------------------
      static std::string getSpecialURL(TWebSession* webSession,
                                       const std::string& userName,
                                       const std::string& paddockName,
                                       const std::string& baseFileName);


   private:
      Adodb::TADOConnection* connection;

      //---------------------------------------------------------------------------
      // Return user id if the specified user exists in the database.
      // Throws if username not found.
      //---------------------------------------------------------------------------
      unsigned getUserId(const std::string& userName) const;

      //---------------------------------------------------------------------------
      // Return paddock id for the specified paddock.
      // Throws if paddockName not found.
      //---------------------------------------------------------------------------
      unsigned getPaddockId(unsigned userId, const std::string& paddockName) const;

      //---------------------------------------------------------------------------
      // Return region id for the specified region
      // Throws if regionName not found.
      //---------------------------------------------------------------------------
      unsigned getRegionId(const std::string& regionName) const;

      //---------------------------------------------------------------------------
      // Add a user. If user already exists then the method throws runtime_error
      //---------------------------------------------------------------------------
      void addUserInternal(const std::string& name,
                           const std::string& userType,
                           const std::string& userName,
                           const std::string& password,
                           const std::string& email);
      //---------------------------------------------------------------------------
      // Create a temporal data query.
      //---------------------------------------------------------------------------
      TDataSet* createTemporalDataQuery(const std::string& userName,
                                        const std::string& paddockName,
                                        const std::vector<std::string>& dataNames);

      //---------------------------------------------------------------------------
      // Lookup a met station and return it's station number. Throws
      // if station doesn't exist.
      //---------------------------------------------------------------------------
      unsigned lookupMetStationNumber(const std::string& userName,
                                      const std::string& paddockName,
                                      const std::string& name);

   };


#endif
