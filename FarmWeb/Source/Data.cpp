//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Data.h"
#include <ADODB.hpp>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\db_functions.h>
#include <general\io_functions.h>
#include <general\xml.h>
#include <general\macro.h>
#include <general\path.h>
#include <general\date_functions.h>
#include <ApsimShared\ApsimDataFileWriter.h>
#include <soil\soil.h>
#include <soil\soilSample.h>
#include <dir.h>
#include "TWebSession.h"

#pragma package(smart_init)
using namespace boost;
using namespace boost::gregorian;
//---------------------------------------------------------------------------
//constructor
//---------------------------------------------------------------------------
Data::Data(void)
   : connection(NULL)
   {
   }
//---------------------------------------------------------------------------
//constructor
//---------------------------------------------------------------------------
Data::~Data(void)
   {
   delete connection;
   }
//---------------------------------------------------------------------------
// Open the specified database file. Will close any existing open connections.
// Will throw a runtime_error on error (e.g. file not found)
//---------------------------------------------------------------------------
void Data::open(const std::string& fileName)
   {
   if (FileExists(fileName.c_str()))
      {
      delete connection;
      connection = new TADOConnection(NULL);

      string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                                "Data Source=?;"
                                "Persist Security Info=False";
      replaceAll(connectionString, "?", fileName);
      connection->Connected = false;
      connection->LoginPrompt = false;
      connection->ConnectionString = connectionString.c_str();
      connection->Connected = true;
      }
   else
      throw runtime_error("Cannot find file: " + fileName);
   }
//---------------------------------------------------------------------------
// Execute the specified SQL statement.
//---------------------------------------------------------------------------
void Data::executeSQL(const std::string& sql)
   {
   executeQuery(connection, sql);
   }
//---------------------------------------------------------------------------
// Set the allowable data values for the specified dataname
//---------------------------------------------------------------------------
void Data::setLookupValues(const std::string& dataName,
                           const std::vector<std::string>& dataValues)
   {
   deleteLookupValues(dataName);
   for (unsigned i = 0; i != dataValues.size(); i++)
      {
      ostringstream sql2;
      sql2 << "INSERT INTO [DataLookupValues] ([name], [value]) VALUES ("
           << singleQuoted(dataName) << ", " << singleQuoted(dataValues[i]) << ")";
      executeQuery(connection, sql2.str());
      }
   }
//---------------------------------------------------------------------------
// Delete the lookup values for the specified dataname
//---------------------------------------------------------------------------
void Data::deleteLookupValues(const std::string& dataName)
   {
   ostringstream sql;
   sql << "DELETE * FROM DataLookupValues"
       << " WHERE name = " << singleQuoted(dataName);
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Get the allowable data lookup names.
//---------------------------------------------------------------------------
void Data::getLookupNames(std::vector<std::string>& lookupNames) const
   {
   ostringstream sql;
   sql << "SELECT DISTINCT name FROM DataLookupValues ORDER BY name";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "name", lookupNames);
   delete query;
   }
//---------------------------------------------------------------------------
// Get the allowable data values for the specified dataname
//---------------------------------------------------------------------------
void Data::getLookupValues(const std::string& dataName,
                           std::vector<std::string>& dataValues) const
   {
   ostringstream sql;
   sql << "SELECT DISTINCT value FROM DataLookupValues"
       << " WHERE name = " << singleQuoted(dataName)
       << " ORDER by value";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "value", dataValues);
   delete query;
   }
//---------------------------------------------------------------------------
// Return user id if the specified user exists in the database. Returns
// 0 if not found.
//---------------------------------------------------------------------------
unsigned Data::getUserId(const std::string& userName) const
   {
   string sql = "SELECT * FROM Users WHERE userName = "
              + singleQuoted(userName);

   TDataSet* query = runQuery(connection, sql);
   if (query->Eof)
      {
      delete query;
      throw runtime_error("Cannot find user " + userName);
      }
   int id = 0;
   id = query->FieldValues["id"];
   delete query;
   return id;
   }
//---------------------------------------------------------------------------
// Add a user. If user already exists then the method throws runtime_error
//---------------------------------------------------------------------------
void Data::addUserInternal(const std::string& name,
                           const std::string& userType,
                           const std::string& userName,
                           const std::string& password,
                           const std::string& email)
   {
   ostringstream sql;
   sql << "INSERT INTO [Users] ([name], [type], [username], [password], [email]) VALUES (";
   sql << singleQuoted(name) << ", " << singleQuoted(userType);
   sql << ", " << singleQuoted(userName) << ", " << singleQuoted(password);
   sql << ", " << singleQuoted(email) << ")";
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Add an admin user. If user already exists then the method throws runtime_error.
//---------------------------------------------------------------------------
void Data::addAdminUser(const std::string& name,
                        const std::string& userName,
                        const std::string& password,
                        const std::string& email)
   {
   addUserInternal(name, "administrator", userName, password, email);
   }
//---------------------------------------------------------------------------
// Add an consultant user. If user already exists then the method throws runtime_error.
//---------------------------------------------------------------------------
void Data::addConsultantUser(const std::string& name,
                             const std::string& userName,
                             const std::string& password,
                             const std::string& email)
   {
   addUserInternal(name, "consultant", userName, password, email);
   }
//---------------------------------------------------------------------------
// Add an normal (grower) user. If user already exists then the method throws runtime_error.
// If consultant name doesn't exist then throw runtime_error.
//---------------------------------------------------------------------------
void Data::addUser(const std::string& name,
                   const std::string& userName,
                   const std::string& password,
                   const std::string& consulantName,
                   const std::string& email)
   {
   unsigned consultantId = getUserId(consulantName);
   addUserInternal(name, "user", userName, password, email);
   unsigned userId = getUserId(userName);

   ostringstream sql;
   sql << "INSERT INTO [ConsultantUserMap] ([consultantId], [userId]) VALUES (";
   sql << consultantId << ", " << userId << ')';
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Change user password.
//---------------------------------------------------------------------------
void Data::changeUserPassword(const std::string& userName,
                              const std::string& password)
   {
   unsigned userId = getUserId(userName);

   ostringstream sql;
   sql << "UPDATE [Users] SET [password] = " << singleQuoted(password)
       << " WHERE id = " << userId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return true if specified user is of the specified type. Will throw if
// userName is not valid.
//---------------------------------------------------------------------------
bool Data::userIsOfType(const std::string& userName, UserType userType)
   {
   ostringstream sql;
   sql << "SELECT * FROM Users WHERE userName = " << singleQuoted(userName);
   TDataSet* query = runQuery(connection, sql.str());
   AnsiString userTypeString;
   if (!query->Eof)
      userTypeString = query->FieldValues["type"];
   delete query;
   if (userTypeString == "")
      throw runtime_error("Cannot find user " + userName);
   if (userType == administrator)
      return (userTypeString == "administrator");
   else if (userType == consultant)
      return (userTypeString == "consultant");
   else
      return (userTypeString == "user");
   }
//---------------------------------------------------------------------------
// Return a list of users.
//---------------------------------------------------------------------------
void Data::getUsers(std::vector<std::string>& userNames)
   {
   ostringstream sql;
   sql << "SELECT * FROM Users ORDER BY userName";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "userName", userNames);
   delete query;
   }
//---------------------------------------------------------------------------
// Return a list of users for the specified consultant. Throws if consultantName
// is not a valid consultant.
//---------------------------------------------------------------------------
void Data::getUsersForConsultant(const std::string& consultantName,
                                 std::vector<std::string>& userNames)
   {
   unsigned consultantId = getUserId(consultantName);
   ostringstream sql;
   sql << "SELECT * FROM Users "
       << "WHERE id IN"
           << " (SELECT userId FROM ConsultantUserMap"
           << "  WHERE consultantId = " << consultantId << ')';
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "userName", userNames);
   delete query;
   }
//---------------------------------------------------------------------------
// Delete a user. If userName doesn't exist then method throws runtime_error
//---------------------------------------------------------------------------
void Data::deleteUser(const std::string& userName)
   {
   unsigned userId = getUserId(userName);

   ostringstream sql;
   sql << "DELETE FROM [Users] WHERE id = " << userId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return true if login is ok.
//---------------------------------------------------------------------------
bool Data::loginOk(const std::string& userName,
                   const std::string& password) const
   {
   ostringstream sql;
   sql << "SELECT * FROM Users WHERE userName = "
       << singleQuoted(userName) << " AND password = "
       << singleQuoted(password);
   TDataSet* query = runQuery(connection, sql.str());
   bool found = (query->RecordCount == 1);
   delete query;
   return found;
   }
//---------------------------------------------------------------------------
// Return the user's email address.
//---------------------------------------------------------------------------
std::string Data::getUserEmail(const std::string& userName) const
   {
   ostringstream sql;
   sql << "SELECT email FROM Users WHERE userName = "
       << singleQuoted(userName);
   TDataSet* query = runQuery(connection, sql.str());
   string email = getDBValue(query, "email");
   delete query;
   return email;
   }
//---------------------------------------------------------------------------
// Return the user's details.
//---------------------------------------------------------------------------
void Data::getUserDetails(const std::string& userName,
                          std::string& name,
                          std::string& email,
                          std::string& password,
                          std::string& userType) const
   {
   ostringstream sql;
   sql << "SELECT * FROM Users WHERE userName = "
       << singleQuoted(userName);
   TDataSet* query = runQuery(connection, sql.str());
   name = getDBValue(query, "name");
   email = getDBValue(query, "email");
   password = getDBValue(query, "password");
   userType = getDBValue(query, "type");
   delete query;
   }
//---------------------------------------------------------------------------
// Set the user's email address.
//---------------------------------------------------------------------------
void Data::setUserEmail(const std::string& userName, const std::string& email)
   {
   unsigned userId = getUserId(userName);

   ostringstream sql;
   sql << "UPDATE [Users] SET [email] = " << singleQuoted(email)
       << " WHERE id = " << userId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return the user's consultant's email address(s)
//---------------------------------------------------------------------------
void Data::getConsultantEmailAddresses(const std::string& userName, vector<string>& emailAddresses) const
   {
   unsigned userId = getUserId(userName);

   ostringstream sql;
   sql << "SELECT email FROM Users WHERE id in "
       <<     "(SELECT consultantId FROM ConsultantUserMap "
       <<     " WHERE userId = " << userId << ")";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "email", emailAddresses);
   delete query;
   }
//---------------------------------------------------------------------------
// Return the name of the user given a userID. Throws an exception if user
// not found.
//---------------------------------------------------------------------------
std::string Data::getNameOfUser(const std::string& userName) const
   {
   ostringstream sql;
   sql << "SELECT * FROM Users WHERE userName = " << singleQuoted(userName);

   TDataSet* query = runQuery(connection, sql.str());
   AnsiString name;
   if (!query->Eof)
      name = query->FieldValues["name"];
   delete query;
   if (name == "")
      throw runtime_error("Cannot find user: " + userName);

   return name.c_str();
   }
//---------------------------------------------------------------------------
// Set the name of the user
//---------------------------------------------------------------------------
void Data::setNameOfUser(const std::string& userName, const std::string& name)
   {
   unsigned userId = getUserId(userName);

   ostringstream sql;
   sql << "UPDATE [Users] SET [name] = " << singleQuoted(name)
       << " WHERE id = " << userId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return paddock id for the specified paddock. Returns 0 if not found.
//---------------------------------------------------------------------------
unsigned Data::getPaddockId(unsigned userId, const std::string& paddockName) const
   {
   ostringstream sql;
   sql << "SELECT id FROM Paddocks"
       << " WHERE name = " << singleQuoted(paddockName)
       << " AND userId = " << userId;

   TDataSet* query = runQuery(connection, sql.str());
   unsigned id = 0;
   if (query->RecordCount == 1)
      id = query->FieldValues["id"];
   delete query;
   if (id == 0)
      throw runtime_error("Cannot find paddock " + paddockName);
   return id;
   }
//---------------------------------------------------------------------------
// Return a list of all paddocks for user.
//---------------------------------------------------------------------------
void Data::getPaddocks(const std::string& userName,
                       std::vector<std::string>& paddockNames) const
   {
   ostringstream sql;
   sql << "SELECT Paddocks.name FROM Users,Paddocks "
          " WHERE Users.id = Paddocks.userId "
          " AND userName = " + singleQuoted(userName);

   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "name", paddockNames);
   delete query;
   }
//---------------------------------------------------------------------------
// Add a paddock in the database for the specified user. If user doesn't
// exist or paddock already exists then the method will throw runtime_error.
//---------------------------------------------------------------------------
void Data::addPaddock(const std::string& userName,
                      const std::string& paddockName)
   {
   unsigned userId = getUserId(userName);
   bool found;
   try
      {
      getPaddockId(userId, paddockName);
      found = true;
      }
   catch (const runtime_error& err)
      {
      found = false;
      }

   if (!found)
      {
      ostringstream sql;
      sql << "INSERT INTO [Paddocks] ([userId], [name]) VALUES (";
      sql << userId << ", " << singleQuoted(paddockName) << ")";
      executeQuery(connection, sql.str());
      }
   else
      throw runtime_error("Paddock " + paddockName
                          + " already exists in database for user " + userName
                          + ". Cannot add again.");

   }
//---------------------------------------------------------------------------
// Delete the specified paddock in the database for the specified user. Will
// throw if user or paddock name are invalid.
//---------------------------------------------------------------------------
void Data::deletePaddock(const std::string& userName,
                         const std::string& paddockName)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);
   ostringstream sql;
   sql << "DELETE FROM [Paddocks]"
       << " WHERE Id = " << paddockId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return all paddock properties to caller. If user or paddock name is invalid
// the the method throws runtime_error.
//---------------------------------------------------------------------------
void Data::getProperties(const std::string& userName,
                                const std::string& paddockName,
                                Properties& properties) const
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   ostringstream sql;
   sql << "SELECT name, value FROM Data"
       << " WHERE paddockId = " << paddockId;

   TDataSet* query = runQuery(connection, sql.str());
   while (!query->Eof)
      {
      properties.push_back(Property(AnsiString(query->FieldValues["name"]).c_str(),
                                    AnsiString(query->FieldValues["value"]).c_str()));
      query->Next();
      }
   delete query;
   }
//---------------------------------------------------------------------------
// Set the paddock properties for the specified user and paddock. If user or paddock
// doesn't exist then the method will throw runtime_error. If the data already
// exists, then the data value is updated.
//---------------------------------------------------------------------------
void Data::setProperties(const std::string& userName,
                         const std::string& paddockName,
                         const Properties& properties)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   // See if data is already in database.
   ostringstream sql;
   sql << "DELETE * FROM Data"
       << " WHERE paddockId = " << paddockId;
   executeQuery(connection, sql.str());

   for (Data::Properties::const_iterator p = properties.begin();
                                         p != properties.end();
                                         p++)
      {
      ostringstream sql2;
      sql2 << "INSERT INTO [Data] ([paddockId], [name], [value]) VALUES ("
           << paddockId << ", " << singleQuoted(p->name)
                        << ", " << singleQuoted(p->value) << ")";
      executeQuery(connection, sql2.str());
      }
   }
//---------------------------------------------------------------------------
// Return a specific property value.
//---------------------------------------------------------------------------
string Data::getProperty(const string& userName,
                         const string& paddockName,
                         const string& propertyName)
   {
   Data::Properties properties;
   getProperties(userName, paddockName, properties);
   Data::Properties::iterator property = find(properties.begin(),
                                              properties.end(),
                                              propertyName);
   if (property == properties.end())
      return "";
   else
      return property->value;
   }
//---------------------------------------------------------------------------
// Delete a specific property value.
//---------------------------------------------------------------------------
void Data::deleteProperty(const string& userName,
                          const string& paddockName,
                          const string& propertyName)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   // See if data is already in database.
   ostringstream sql;
   sql << "DELETE * FROM Data"
       << " WHERE paddockId = " << paddockId
       << " AND name = " << singleQuoted(propertyName);
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Set a specific property value.
//---------------------------------------------------------------------------
void Data::setProperty(const string& userName,
                       const string& paddockName,
                       const string& propertyName,
                       const string& propertyValue)
   {
   Data::Properties properties;
   getProperties(userName, paddockName, properties);
   Data::Properties::iterator property = find(properties.begin(),
                                              properties.end(),
                                              propertyName);
   if (property == properties.end())
      properties.push_back(Data::Property(propertyName, propertyValue));
   else
      property->value = propertyValue;
   setProperties(userName, paddockName, properties);
   }
//---------------------------------------------------------------------------
// Add some temporal data for the specified user and paddock. If user or paddock
// doesn't exist then the method will throw runtime_error. If the data already
// exists on the specified dates, then the data will be changed.
//---------------------------------------------------------------------------
void Data::addTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           const std::string& dataName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           const TemporalValues& values)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   // See if data is already in database.
   ostringstream sql;
   sql << "DELETE * FROM TemporalData"
       << " WHERE paddockId = " << paddockId
       << " AND   name = " << singleQuoted(dataName)
       << " AND date >= " << singleQuoted(to_iso_extended_string(startDate))
       << " AND date <= " << singleQuoted(to_iso_extended_string(endDate));
   executeQuery(connection, sql.str());

   TADOQuery* query = new TADOQuery(connection);
   try
      {
      query->Connection = connection;
      query->SQL->Text = "INSERT INTO [TemporalData] ([paddockId], [date], [name], [value]) "
                         "VALUES (:paddockId, :date, :name, :value)";
      query->Parameters->Items[0]->DataType = ftInteger;
      query->Parameters->Items[1]->DataType = ftString;
      query->Parameters->Items[2]->DataType = ftString;
      query->Parameters->Items[3]->DataType = ftString;
      query->Prepared = true;
      for (unsigned i = 0; i != values.size(); i++)
         {
         query->Parameters->Items[0]->Value = paddockId;
         query->Parameters->Items[1]->Value = to_iso_extended_string(values[i].date).c_str();
         query->Parameters->Items[2]->Value = dataName.c_str();
         query->Parameters->Items[3]->Value = values[i].value.c_str();
         query->ExecSQL();
         }
      delete query;
      }
   catch (const Exception& err)
      {
      delete query;
      throw runtime_error(err.Message.c_str());
      }
   }

//---------------------------------------------------------------------------
// Add some temporal data for the specified user and paddock. If user or paddock
// doesn't exist then the method will throw runtime_error. If the data already
// exists on the specified dates, then the data will be changed.
//---------------------------------------------------------------------------
void Data::addTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           const TemporalValues& values)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   // See if data is already in database.
   ostringstream sql;
   sql << "DELETE * FROM TemporalData"
       << " WHERE paddockId = " << paddockId
       << " AND date >= " << singleQuoted(to_iso_extended_string(startDate))
       << " AND date <= " << singleQuoted(to_iso_extended_string(endDate));
   executeQuery(connection, sql.str());

   TADOQuery* query = new TADOQuery(connection);
   try
      {
      query->Connection = connection;
      query->SQL->Text = "INSERT INTO [TemporalData] ([paddockId], [date], [name], [value]) "
                         "VALUES (:paddockId, :date, :name, :value)";
      query->Parameters->Items[0]->DataType = ftInteger;
      query->Parameters->Items[1]->DataType = ftString;
      query->Parameters->Items[2]->DataType = ftString;
      query->Parameters->Items[3]->DataType = ftString;
      query->Prepared = true;
      for (unsigned i = 0; i != values.size(); i++)
         {
         query->Parameters->Items[0]->Value = paddockId;
         query->Parameters->Items[1]->Value = to_iso_extended_string(values[i].date).c_str();
         query->Parameters->Items[2]->Value = values[i].type.c_str();
         query->Parameters->Items[3]->Value = values[i].value.c_str();
         query->ExecSQL();
         }
      delete query;
      }
   catch (const Exception& err)
      {
      delete query;
      throw runtime_error(err.Message.c_str());
      }
   }
//---------------------------------------------------------------------------
// Get some temporal data for the specified user and paddock. If user or paddock
// doesn't exist then the method will throw runtime_error.
//---------------------------------------------------------------------------
void Data::getTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           const std::string& dataName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           TemporalValues& values)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   ostringstream sql;
   sql << "SELECT date,value FROM TemporalData"
       << " WHERE paddockId = " << paddockId
       << " AND date >= " << singleQuoted(to_iso_extended_string(startDate))
       << " AND date <= " << singleQuoted(to_iso_extended_string(endDate))
       << " AND name = " << singleQuoted(dataName);

   TDataSet* query = runQuery(connection, sql.str());

   while (!query->Eof)
      {
      AnsiString dateString = query->FieldValues["date"];
      AnsiString valueString = query->FieldValues["value"];
      values.push_back(TemporalData(from_string(dateString.c_str()),
                                    valueString.c_str(),
                                    dataName));
      query->Next();
      }
   delete query;
   }

//---------------------------------------------------------------------------
// Get some temporal data for the specified user and paddock. If user or paddock
// doesn't exist then the method will throw runtime_error.
//---------------------------------------------------------------------------
void Data::getTemporalData(const std::string& userName,
                           const std::string& paddockName,
                           boost::gregorian::date startDate,
                           boost::gregorian::date endDate,
                           TemporalValues& values)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   ostringstream sql;
   sql << "SELECT date,value,name FROM TemporalData"
       << " WHERE paddockId = " << paddockId
       << " AND date >= " << singleQuoted(to_iso_extended_string(startDate))
       << " AND date <= " << singleQuoted(to_iso_extended_string(endDate))
       << " ORDER BY date";

   TDataSet* query = runQuery(connection, sql.str());

   while (!query->Eof)
      {
      AnsiString dateString = query->FieldValues["date"];
      AnsiString valueString = query->FieldValues["value"];
      AnsiString dataName = query->FieldValues["name"];
      values.push_back(TemporalData(from_string(dateString.c_str()),
                                    valueString.c_str(),
                                    dataName.c_str()));
      query->Next();
      }
   delete query;
   }

//---------------------------------------------------------------------------
// Create a temporal data query.
//---------------------------------------------------------------------------
TDataSet* Data::createTemporalDataQuery(const std::string& userName,
                                        const std::string& paddockName,
                                        const std::vector<std::string>& dataNames)
   {
   unsigned userId = getUserId(userName);
   unsigned paddockId = getPaddockId(userId, paddockName);

   string dataNamesSet;
   for (unsigned i = 0; i != dataNames.size(); i++)
      {
      if (i != 0)
         dataNamesSet += ", ";
      dataNamesSet += "\"" + dataNames[i] + "\"";
      }

   ostringstream sql;
   sql << "SELECT date,name,value FROM TemporalData"
       << " WHERE paddockId = " << paddockId
       << " AND name IN (" << dataNamesSet << ")"
       << " ORDER BY date, name";

   return runQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Generate a temporal data file in APSIM format containing columns
// for the specified data names. If user or paddock
// doesn't exist then the method will throw runtime_error.
// Section name is written to the data file on the first line - it should
// not have [ ] characters around it.
//---------------------------------------------------------------------------
void Data::generateDataFile(const std::string& userName,
                            const std::string& paddockName,
                            const std::vector<std::string>& dataNames,
                            const std::string& fileName,
                            const std::string& sectionName)
   {
   TDataSet* query = createTemporalDataQuery(userName, paddockName, dataNames);

   ApsimDataFileWriter writer;
   writer.open(fileName, sectionName);
   writer.addConstant(Value("allow_sparse_data", "()", "true", ""));
   writer.addConstant(Value("patch_all_years", "()", "true", ""));

   while (!query->Eof)
      {
      ApsimDataFileWriter::Values fields;

      AnsiString dateString = query->FieldValues["date"];
      date recordDate(from_string(dateString.c_str()));

      for (unsigned i = 0; i != dataNames.size(); i++)
         {
         AnsiString nameString = query->FieldValues["name"];
         if (nameString == dataNames[i].c_str())
            {
            AnsiString valueString = query->FieldValues["value"];
            fields.push_back(Value(nameString.c_str(), "()", valueString.c_str(), ""));
            query->Next();
            }
         }

      writer.addTemporalRecord(recordDate, fields);
      }
   writer.close();
   delete query;
   }
//---------------------------------------------------------------------------
// Generate a temporal data file in APSIM format containing columns
// for the specified data names. If user or paddock
// doesn't exist then the method will throw runtime_error.
// Section name is written to the data file on the first line - it should
// not have [ ] characters around it.
// This method will produce a complete data file with every day between
// start and end date and will pad with zeros for any dates missing in database.
//---------------------------------------------------------------------------
void Data::generateCompleteDataFile(const std::string& userName,
                                    const std::string& paddockName,
                                    const std::vector<std::string>& dataNames,
                                    const std::string& fileName,
                                    const std::string& sectionName,
                                    boost::gregorian::date startDate,
                                    boost::gregorian::date endDate)
   {
   TDataSet* query = createTemporalDataQuery(userName, paddockName, dataNames);

   ApsimDataFileWriter writer;
   writer.open(fileName, sectionName);
   writer.addConstant(Value("allow_sparse_data", "()", "false", ""));
   writer.addConstant(Value("patch_all_years", "()", "true", ""));
   writer.addConstant(Value("patch_variables_long_term", "()", "maxt mint radn", ""));

   date currentDate = startDate;
   while (currentDate <= endDate)
      {
      ApsimDataFileWriter::Values fields;

      // try and find the current date.
      date recordDate(pos_infin);
      while (!query->Eof)
         {
         AnsiString dateString = query->FieldValues["date"];
         recordDate = date(from_string(dateString.c_str()));
         if (recordDate >= currentDate)
            break;
         else
            query->Next();
         }

      for (unsigned i = 0; i != dataNames.size(); i++)
         {
         AnsiString valueString;
         if (currentDate == recordDate)
            {
            valueString = query->FieldValues["value"];
            query->Next();
            }
         else
            valueString = "0";
         fields.push_back(Value(dataNames[i], "()", valueString.c_str(), ""));
         }

      writer.addTemporalRecord(currentDate, fields);
      currentDate = currentDate + date_duration(1);
      }
   writer.close();
   delete query;
   }
//---------------------------------------------------------------------------
// Return a list of report template names
//---------------------------------------------------------------------------
void Data::getReportTemplateNames(std::vector<std::string>& names)
   {
   ostringstream sql;
   sql << "SELECT DISTINCT name FROM ReportGeneration";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "name", names);
   delete query;
   }
//---------------------------------------------------------------------------
// Return the number of report template pages for the specified report.
//---------------------------------------------------------------------------
unsigned Data::getNumReportTemplatePages(const std::string& reportName)
   {
   ostringstream sql;
   sql << "SELECT MAX(templatetype) FROM ReportGeneration"
       << " WHERE name = " << singleQuoted(reportName);

   TDataSet* query = runQuery(connection, sql.str());
   if (query->Eof)
      {
      delete query;
      throw runtime_error("Cannot find report template for report " + reportName);
      }
   else
      {
      unsigned numPages = query->Fields->Fields[0]->AsInteger;
      delete query;
      return numPages;
      }
   }
//---------------------------------------------------------------------------
// Return the template contents for the specified report. Throws if
// report doesn't exist.
//---------------------------------------------------------------------------
std::string Data::getReportTemplate(const std::string& reportName,
                                    unsigned pageNumber)
   {
   ostringstream sql;
   sql << "SELECT template FROM ReportGeneration"
       << " WHERE name = " << singleQuoted(reportName)
       << " AND templatetype = " << pageNumber;

   TDataSet* query = runQuery(connection, sql.str());
   if (query->Eof)
      {
      delete query;
      throw runtime_error("Cannot find report template for report " + reportName);
      }

   string templateContents = AnsiString(query->FieldValues["template"]).c_str();
   replaceAll(templateContents, "\r", "");
   delete query;
   replaceAll(templateContents, "#Quote#", "'");
   replaceAll(templateContents, "#DQuote#", "\"");
   return templateContents.c_str();
   }
//---------------------------------------------------------------------------
// Set the template contents for the specified report. Overwrites
// existing template if it already exists.
//---------------------------------------------------------------------------
void Data::setReportTemplate(const std::string& reportName,
                             const std::string& templateContents,
                             unsigned pageNumber)
   {
   string contents = templateContents;
   replaceAll(contents, "'", "#Quote#");
   replaceAll(contents, "\"", "#DQuote#");

   deleteReportTemplate(reportName, pageNumber);
   ostringstream sql;
   sql << "INSERT INTO [ReportGeneration] ([name], [templatetype], [template]) VALUES ("
       << singleQuoted(reportName) << ", " << pageNumber << ", " << singleQuoted(contents) << ")";
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Delete the specified report template
//---------------------------------------------------------------------------
void Data::deleteReportTemplate(const std::string& reportName,
                                unsigned pageNumber)
   {
   ostringstream sql;
   sql << "DELETE * FROM ReportGeneration"
       << " WHERE name = " << singleQuoted(reportName)
       << " AND templatetype = " << pageNumber;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Generate all report files for the specified user and paddock.
// If user name or paddock name or report name are invalid then the
// method will throw runtime_error.
// Method returns a list of all files generated.
//---------------------------------------------------------------------------
void Data::generateReportFiles(const std::string& userName,
                               const std::string& paddockName,
                               const std::string& reportName,
                               const Properties& reportProperties,
                               const std::string& outputDirectory,
                               std::vector<std::string>& filesGenerated,
                               const string& reportDescription)
   {
   unsigned numPages = getNumReportTemplatePages(reportName);
   string templateContents;
   for (unsigned page = 1; page <= numPages; page++)
      templateContents += getReportTemplate(reportName, page);

   date yesterday(day_clock::local_day());
   yesterday = yesterday - date_duration(2);

   Data::Properties properties;

   properties.push_back(Data::Property("growername", userName));
   properties.push_back(Data::Property("reportdescription", reportDescription));
   properties.push_back(Data::Property("yesterday_date", to_iso_extended_string(yesterday)));

   getProperties(userName, paddockName, properties);
   copy(reportProperties.begin(), reportProperties.end(),
        back_inserter(properties));
   getClimateForecastProperties(properties);

   // Look for any date properties.  If found then format to dd/mm/yyyy
   // and create a daymonth property.
   for (unsigned p = 0; p != properties.size(); p++)
      {
      unsigned posDate = properties[p].name.find("date");
      if (posDate != string::npos)
         {
         string dateString = properties[p].value;
         date currentDate = from_string(properties[p].value);
         properties[p].value = to_dmy(currentDate);

         // add a special sowdate variable without the year component.
         ostringstream out;
         out << currentDate.day() << '-' << getShortMonthString(currentDate.month());
         properties.push_back(Data::Property(properties[p].name.substr(0, posDate) + "daymonth",
                                             out.str()));
         }
      }
   // Look for a met station property.  If found then convert name to a number.
   Data::Properties::iterator metStation = find(properties.begin(),
                                                properties.end(),
                                                "metstation");
   if (metStation != properties.end())
      {
      try
         {
         unsigned number = lookupMetStationNumber(userName, paddockName, metStation->value);
         properties.push_back(Data::Property("StationNumber", lexical_cast<string>(number)));
         properties.push_back(Data::Property("StationName", metStation->value));
         }
      catch (const exception& err)
         { }
      }

   XMLDocument xmlDoc("data", XMLDocument::rootName);
   XMLNode paddockNode = xmlDoc.documentElement().appendChild("paddock", true);
   paddockNode.setAttribute("name", paddockName);

   for (Data::Properties::iterator p = properties.begin();
                                   p != properties.end();
                                   p++)
      {
      paddockNode.appendChild(p->name).setValue(p->value);
      }
   xmlDoc.write("test.xml");
   Macro macro;
   macro.go(xmlDoc.documentElement(), templateContents, filesGenerated,
            outputDirectory);
   }

// Sort in descending order - NOTE the order of lhs and rhs arguments.
bool operator< (const ffblk& rhs, const ffblk& lhs)
   {
   if (lhs.ff_fdate < rhs.ff_fdate)
      return true;
   else if (lhs.ff_fdate == rhs.ff_fdate)
      return lhs.ff_ftime < rhs.ff_ftime;
   else
      return false;
   }


//---------------------------------------------------------------------------
// Return a list of reports to caller.
//---------------------------------------------------------------------------
void Data::getReports(const std::string& reportBaseDirectory, const std::string& userName,
                      std::vector<std::string>& reportNames)
   {
   string Folder = reportBaseDirectory + "\\" + userName;
   string FileSpec = Folder + "\\*.*";

   vector<ffblk> files;
   struct ffblk ffblk;
   int done = findfirst(FileSpec.c_str(), &ffblk, FA_ARCH);
   while (!done)
      {
      bool Keep = (stristr(ffblk.ff_name, ".jpg")!=NULL ||
                   stristr(ffblk.ff_name, ".gif")!=NULL);
      if (Keep)
         files.push_back(ffblk);
      done = findnext (&ffblk);
      }
   sort(files.begin(), files.end());

   for (unsigned i = 0; i != files.size(); i++)
      reportNames.push_back(Path(files[i].ff_name).Get_name_without_ext());
   }
// --------------------------------------------------
// Return a file name for the report.
// --------------------------------------------------
string Data::getReportFileName(const std::string& reportBaseDirectory,
                               const std::string& userName,
                               const std::string& reportName)
   {
   Path FullFileName(reportBaseDirectory + "\\" + userName + "\\" + reportName + ".gif");
   if (!FullFileName.Exists())
      FullFileName.Set_extension(".jpg");
   if (!FullFileName.Exists())
      throw runtime_error("Cannot find filename for report: " + reportName);
   return FullFileName.Get_path();
   }
//---------------------------------------------------------------------------
// Delete the specified report file for the specified user.
//---------------------------------------------------------------------------
void Data::deleteReport(const std::string& reportBaseDirectory,
                        const std::string& userName,
                        const std::string& reportName)
   {
   string FileName = getReportFileName(reportBaseDirectory, userName, reportName);
   DeleteFile(FileName.c_str());
   }
//---------------------------------------------------------------------------
// Rename the specified report file for the specified user.
//---------------------------------------------------------------------------
void Data::renameReport(const std::string& reportBaseDirectory,
                        const std::string& userName,
                        const std::string& oldReportName,
                        const std::string& newReportName)
   {
   Path FullFileName(getReportFileName(reportBaseDirectory, userName, oldReportName));
   Path NewFileName = FullFileName;
   NewFileName.Set_name(newReportName.c_str());
   NewFileName.Set_extension(FullFileName.Get_extension().c_str());
   RenameFile(FullFileName.Get_path().c_str(), NewFileName.Get_path().c_str());
   }
//---------------------------------------------------------------------------
// Return region id for the specified region
// Throws if regionName not found.
//---------------------------------------------------------------------------
unsigned Data::getRegionId(const std::string& regionName) const
   {
   string sql = "SELECT * FROM Regions WHERE name = "
              + singleQuoted(regionName);

   TDataSet* query = runQuery(connection, sql);
   if (query->Eof)
      {
      delete query;
      throw runtime_error("Cannot find region " + regionName);
      }
   int id = 0;
   id = query->FieldValues["id"];
   delete query;
   return id;
   }
//---------------------------------------------------------------------------
// Add a region. Will throw if it doesn't exist.
//---------------------------------------------------------------------------
void Data::addRegion(const std::string& name)
   {
   vector<string> names;
   getRegions(names);
   if (find(names.begin(), names.end(), name) == names.end())
      {
      ostringstream sql;
      sql << "INSERT INTO [Regions] ([name]) VALUES ("
           << singleQuoted(name) << ")";
      executeQuery(connection, sql.str());
      }
   else
      throw runtime_error("Region: " + name + " already exists. Cannot add again.");
   }
//---------------------------------------------------------------------------
// Delete a region. Will throw if it doesn't exist.
//---------------------------------------------------------------------------
void Data::deleteRegion(const std::string& name)
   {
   unsigned regionId = getRegionId(name);
   ostringstream sql;
   sql << "DELETE * FROM Regions"
       << " WHERE id = " << regionId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return a list of regions
//---------------------------------------------------------------------------
void Data::getRegions(std::vector<std::string>& names)
   {
   ostringstream sql;
   sql << "SELECT name FROM Regions ORDER BY name";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "name", names);
   delete query;
   }
//---------------------------------------------------------------------------
// Return a list of soils to caller. Will throw if region not found.
//---------------------------------------------------------------------------
void Data::getSoils(const std::string& regionName, std::vector<std::string>& names) const
   {
   unsigned regionId = getRegionId(regionName);
   ostringstream sql;
   sql << "SELECT name FROM Soils "
       << "WHERE regionID = " << regionId
       << "  ORDER BY name";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "name", names);
   delete query;
   }
//---------------------------------------------------------------------------
// Set the data of the specified soil. If the soil already exists, then
// its data will be overwritten. Will throw if region doesn't exist
//---------------------------------------------------------------------------
void Data::setSoil(const std::string& regionName, const std::string& name, const std::string& data)
   {
   deleteSoil(regionName, name);

   string contents = data;
   replaceAll(contents, "'", "#Quote#");
   replaceAll(contents, "\"", "#DQuote#");

   ostringstream sql;
   sql << "INSERT INTO [Soils] ([regionID], [name], [data]) VALUES ("
       << getRegionId(regionName) << ", "
       << singleQuoted(name) << ", "
       << singleQuoted(contents) << ")";
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Get the data of the specified soil. Will throw if the soil or region doesn't exist.
//---------------------------------------------------------------------------
std::string Data::getSoil(const std::string& regionName, const std::string& name) const
   {
   unsigned regionId = getRegionId(regionName);

   ostringstream sql;
   sql << "SELECT data FROM Soils"
       << " WHERE name = " << singleQuoted(name)
       << " AND regionID = " << regionId;
   TDataSet* query = runQuery(connection, sql.str());
   if (query->Eof)
      {
      delete query;
      throw runtime_error("Cannot find soil: " + name);
      }
   string data = getDBValue(query, "data");
   replaceAll(data, "#Quote#", "'");
   replaceAll(data, "#DQuote#", "\"");

   delete query;
   replaceAll(data, "\r", "");
   return data;
   }

//---------------------------------------------------------------------------
// Delete the specified soil if it exists. Throws if region doesn't exist.
// Will NOT throw if the soil doesn't exist.
//---------------------------------------------------------------------------
void Data::deleteSoil(const std::string& regionName, const std::string& name)
   {
   unsigned regionId = getRegionId(regionName);
   ostringstream sql;
   sql << "DELETE * FROM Soils"
       << " WHERE name = " << singleQuoted(name)
       << " AND regionID = " << regionId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Generate a soil file and return its filename. Returns true if the
// soil water parameters were bounded.
//---------------------------------------------------------------------------
bool Data::generateSoilFile(const std::string& userName,
                            const std::string& paddockName,
                            const std::string& outputDirectory,
                            bool withSW,
                            std::string& soilFileName)
   {
   string regionName = getProperty(userName, paddockName, "region");
   string soilName = getProperty(userName, paddockName, "soiltype");
   string soilContents = getSoil(regionName, soilName);
   Soil soil(soilContents);

   string soilSampleString = getProperty(userName, paddockName, "soilsample1");
   if (soilSampleString != "")
      {
      SoilSample sample(soilSampleString);
      sample.mapSampleToSoil(soil);
      if (sample.hasData("Water", "sw"))
         soil.setSw(sample.sw());
      if (sample.hasData("Nitrogen", "no3"))
         soil.setNo3(sample.no3());
      if (sample.hasData("Nitrogen", "nh4"))
         soil.setNh4(sample.nh4());
      }
   soilSampleString = getProperty(userName, paddockName, "soilsample2");
   if (soilSampleString != "")
      {
      SoilSample sample(soilSampleString);
      sample.mapSampleToSoil(soil);
      if (sample.hasData("Nitrogen", "oc"))
         soil.setOc(sample.oc());
      if (sample.hasData("Other", "ec"))
         soil.setEc(sample.ec());
      if (sample.hasData("Nitrogen", "ph"))
         soil.setPh(sample.ph());
      if (sample.hasData("Other", "esp"))
         soil.setEsp(sample.esp());
      }
   bool profileNeededChanging = soil.autoCorrect();

   static const char* MACRO_TEMPLATE =
      "[soil.soilwat2.parameters]\n"
      "   #for_each water\n"
      "   diffus_const = water.diffusconst   ! coeffs for unsaturated water flow\n"
      "   diffus_slope = water.diffusslope\n"
      "   cn2_bare     = water.cn2bare    ! bare soil runoff curve number\n"
      "   cn_red       = water.cnred    ! potetial reduction in curve number due to residue\n"
      "   cn_cov       = water.cncov   ! cover for maximum reduction in curve number\n"
      "   salb         = water.salb  ! bare soil albedo\n"
      "   cona         = water.cona     ! stage 2 evap coef.\n"
      "   u            = water.u     ! stage 1 soil evaporation coefficient (mm)\n"
      "   #endfor\n"
      "\n"
      "   dlayer  = .thickness   ! layer thickness mm soil\n"
      "   air_dry = .airdry   ! air dry mm water/ mm soil\n"
      "   ll15    = .ll15   ! lower limit mm water/mm soil\n"
      "   dul     = .dul   ! drained upper limit mm water/mm soil\n"
      "   sat     = .sat   ! saturation mm water/mm soil\n"
      "   swcon   = .swcon   ! drainage coefficient\n"
      "   bd      = .bd   ! bulk density gm dry soil/cc moist soil\n"
      "\n"
      "[soil.soiln2.parameters]\n"
      "   #for_each nitrogen\n"
      "   root_cn      = nitrogen.rootcn     ! C:N ratio of initial root residues\n"
      "   root_wt      = nitrogen.rootwt   ! root residues as biomass (kg/ha)\n"
      "   soil_cn      = nitrogen.soilcn   ! C:N ratio of soil\n"
      "   enr_a_coeff  = nitrogen.enracoeff\n"
      "   enr_b_coeff  = nitrogen.enrbcoeff\n"
      "   profile_reduction =  off\n"
      "   #endfor\n"
      "\n"
      "   oc      = .oc   ! Soil Organic Carbon\n"
      "   ph      = .ph   ! pH of soil\n"
      "   fbiom   = .fbiom   ! Organic C Biomass Fraction\n"
      "   finert  = .finert   ! Inert Organic C Fraction\n"
      "   no3ppm  = .no3ppm   ! Nitrate Concentration\n"
      "   nh4ppm  = .nh4ppm   ! Ammonium Concentration\n"
      "\n"
      "#for_each crop\n"
      "[soil.crop.name.parameters]\n"
      "   ll      = crop.ll\n"
      "   kl      = crop.kl\n"
      "   xf      = crop.xf\n"
      "#endfor";

   soilFileName = outputDirectory + "\\" + userName + ".soil";
   ofstream out(soilFileName.c_str());

   if (withSW)
      soil.writeApsimPar(out, true);
   else
      soil.exportSoil(out, MACRO_TEMPLATE, true);
   return profileNeededChanging;
   }
//---------------------------------------------------------------------------
// Add a met station. Will throw if it already exists.
//---------------------------------------------------------------------------
void Data::addMetStation(const std::string& regionName, const std::string& name, unsigned stationNumber)
   {
   vector<string> names;
   getMetStations(regionName, names);
   if (find(names.begin(), names.end(), name) == names.end())
      {
      ostringstream sql;
      sql << "INSERT INTO [MetStations] ([regionID], [name], [stationNumber]) VALUES ("
           << getRegionId(regionName) << ", "
           << singleQuoted(name) << ", "
           << stationNumber << ")";
      executeQuery(connection, sql.str());
      }
   else
      throw runtime_error("Met station: " + name + " already exists. Cannot add again.");
   }
//---------------------------------------------------------------------------
// Delete a met station.
//---------------------------------------------------------------------------
void Data::deleteMetStation(const std::string& regionName, const std::string& name)
   {
   unsigned regionId = getRegionId(regionName);
   ostringstream sql;
   sql << "DELETE * FROM MetStations"
       << " WHERE name = " << singleQuoted(name)
       << " AND regionID = " << regionId;
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return a list of met stations
//---------------------------------------------------------------------------
void Data::getMetStations(const std::string& regionName, std::vector<std::string>& names)
   {
   unsigned regionId = getRegionId(regionName);
   ostringstream sql;
   sql << "SELECT name FROM MetStations "
       << "WHERE regionID = " << regionId
       << "  ORDER BY name";
   TDataSet* query = runQuery(connection, sql.str());
   getDBFieldValues(query, "name", names);
   delete query;
   }
//---------------------------------------------------------------------------
// Lookup a met station and return it's station number. Throws
// if station doesn't exist.
//---------------------------------------------------------------------------
unsigned Data::lookupMetStationNumber(const std::string& userName,
                                      const std::string& paddockName,
                                      const std::string& name)
   {
   string regionName = getProperty(userName, paddockName, "region");
   unsigned regionId = getRegionId(regionName);
   ostringstream sql;
   sql << "SELECT stationNumber FROM MetStations "
       << "WHERE regionID = " << regionId
       << " AND name = " << singleQuoted(name);
   TDataSet* query = runQuery(connection, sql.str());
   unsigned station_number = getDBUnsigned(query, "stationNumber");
   delete query;
   return station_number;
   }
//---------------------------------------------------------------------------
// Return all climate forecast information.
//---------------------------------------------------------------------------
void Data::getClimateForecast(unsigned& soiPhase, unsigned& soiMonth,
                              unsigned& davidSYear1, unsigned& davidSYear2,
                              unsigned& davidSYear3, unsigned& davidSYear4,
                              unsigned& davidSYear5,
                              string& soiDescription,
                              string& davidsDescription) const
   {
   TDataSet* query = runQuery(connection, "SELECT * FROM ClimateForecast");
   if (query->Eof)
      {
      delete query;
      throw runtime_error("Cannot find climate forecast info in database");
      }

   soiPhase = getDBUnsigned(query, "soiphase");
   soiMonth = getDBUnsigned(query, "soimonth");
   davidSYear1 = getDBUnsigned(query, "davidsYear1");
   davidSYear2 = getDBUnsigned(query, "davidsYear2");
   davidSYear3 = getDBUnsigned(query, "davidsYear3");
   davidSYear4 = getDBUnsigned(query, "davidsYear4");
   davidSYear5 = getDBUnsigned(query, "davidsYear5");
   soiDescription = getDBValue(query, "soidescription");
   davidsDescription = getDBValue(query, "davidsDescription");
   delete query;
   }
//---------------------------------------------------------------------------
// set climate forecast information.
//---------------------------------------------------------------------------
void Data::setClimateForecast(unsigned soiPhase, unsigned soiPhaseMonth,
                              unsigned davidSYear1, unsigned davidSYear2,
                              unsigned davidSYear3, unsigned davidSYear4,
                              unsigned davidSYear5,
                              const std::string& soiDescription,
                              const std::string& davidsDescription)
   {
   executeQuery(connection, "DELETE * FROM ClimateForecast");
   ostringstream sql;
   sql << "INSERT INTO [ClimateForecast] ([soiphase], [soimonth], "
       << " [davidsYear1], [davidsYear2], [davidsYear3], [davidsYear4], [davidsYear5], "
       << " [soidescription], [davidsdescription]) VALUES ("
        << soiPhase << ", " << soiPhaseMonth << ", "
        << davidSYear1 << ", " << davidSYear2 << ", " << davidSYear3 << ", "
        << davidSYear4 << ", " << davidSYear5 << ", "
        << singleQuoted(soiDescription) << ", "
        << singleQuoted(davidsDescription) << ")";
   executeQuery(connection, sql.str());
   }
//---------------------------------------------------------------------------
// Return all climate forecast properties to caller.
//---------------------------------------------------------------------------
void Data::getClimateForecastProperties(Properties& properties) const
   {
   try
      {
      unsigned soiPhase, soiPhaseMonth;
      unsigned davidSYear1, davidSYear2, davidSYear3, davidSYear4, davidSYear5;
      string soiDescription, davidsDescription;
      getClimateForecast(soiPhase, soiPhaseMonth,
                         davidSYear1, davidSYear2, davidSYear3, davidSYear4, davidSYear5,
                         soiDescription, davidsDescription);
      static const char* phases[5] = {"Negative", "Positive", "Falling", "Rising", "Zero"};
      properties.push_back(Property("soiphase", phases[soiPhase]));
      properties.push_back(Property("soimonth", getLongMonthString(soiPhaseMonth+1)));
      properties.push_back(Property("davidsyear1", lexical_cast<string>(davidSYear1)));
      properties.push_back(Property("davidsyear2", lexical_cast<string>(davidSYear2)));
      properties.push_back(Property("davidsyear3", lexical_cast<string>(davidSYear3)));
      properties.push_back(Property("davidsyear4", lexical_cast<string>(davidSYear4)));
      properties.push_back(Property("davidsyear5", lexical_cast<string>(davidSYear5)));

      replaceAll(soiDescription, "\r\n", "'#13#10'");
      properties.push_back(Property("soidescription", soiDescription));

      replaceAll(davidsDescription, "\r\n", "'#13#10'");
      properties.push_back(Property("davidsDescription", davidsDescription));
      }
   catch (const runtime_error& err)
      {
      }
   }

//---------------------------------------------------------------------------
// Return the apsim run machine's email address.
//---------------------------------------------------------------------------
string Data::getApsimRunEmailAddress() const
   {
   vector<string> emailAddresses;
   getLookupValues("ApsimRunEmailAddress", emailAddresses);
   if (emailAddresses.size() > 0)
      return emailAddresses[0];
   else
      throw runtime_error("Cannot find an email address for APSIM run machine!.");
   }
//---------------------------------------------------------------------------
// Return the help url.
//---------------------------------------------------------------------------
string Data::getHelpUrl() const    
   {
   vector<string> helpUrls;
   getLookupValues("HelpUrl", helpUrls);
   if (helpUrls.size() > 0)
      return helpUrls[0];
   else
      throw runtime_error("Cannot find the help url");
   }
//---------------------------------------------------------------------------
// Return the special url that lives under afloman directory.
//---------------------------------------------------------------------------
string Data::getSpecialURL(TWebSession* webSession,
                           const std::string& userName,
                           const std::string& paddockName,
                           const std::string& baseFileName)
   {
   string dirForFolder = webSession->getFilesDir() + "\\..\\..\\..\\Afloman\\Files\\" + userName;
   dirForFolder = ExpandFileName(dirForFolder.c_str()).c_str();
   dirForFolder += "\\";
   string fileSpec = dirForFolder + paddockName + "-" + baseFileName + ".*";

   struct ffblk ffblk;
   int error = findfirst(fileSpec.c_str(), &ffblk, FA_ARCH);
   if (error)
      return "";
   else
      {
      string urlForFolder = "http://www.apsim.info/apsim/afloman/files/" + userName;
      return urlForFolder + "/" +  ffblk.ff_name;
      }
   }

