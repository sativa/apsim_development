#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "InputComponent.h"
#include <ApsimShared\ApsimComponentData.h>
#include <ComponentInterface\MessageDataExt.h>
#include <ComponentInterface\ApsimVariant.h>
#include <ComponentInterface\datatypes.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\date_class.h>
#include <ApsimShared\ApsimDataFile.h>
#include <list>
#include <math.h>

using namespace std;

static const char* dayLengthType =
   "<type name=\"daylength\" kind=\"single\" units=\"hours\"/>";

// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new InputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
InputComponent::InputComponent(void)
   {
   data = NULL;
   }

// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
InputComponent::~InputComponent(void)
   {
   delete data;
   for (InputVariables::iterator i = variables.begin();
                                 i != variables.end();
                                 i++)
      delete i->second;
   }
// ------------------------------------------------------------------
// INIT1 method handler.
// ------------------------------------------------------------------
void InputComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   // register a few things.
   tickID = addRegistration(protocol::respondToEventReg, "tick", timeTypeDDML);
   newmetID = addRegistration(protocol::eventReg, "newmet", newmetTypeDDML);
   iAmMet = (stricmp(name, "met") == 0);
   if (iAmMet)
      daylengthID = addRegistration(protocol::respondToGetReg, "day_length", dayLengthType);
   else
      daylengthID = 0;

   try
      {
      fileName = componentData->getProperty("parameters", "filename");
      if (fileName == "")
         throw runtime_error("Cannot find a filename parameter for module: "
                             + string(name));
      data = new ApsimDataFile(fileName);

      readConstants();
      readHeadings();
      dateFieldsOk();

      // read in a line from the file so that the fields know what data
      // type they're dealing with.
      readLineFromFile();
      checkForSparseData();
      }
   catch (const runtime_error& errormsg)
      {
      error(errormsg.what(), true);
      }
   }
// ------------------------------------------------------------------
// INIT 2 - temporary
// ------------------------------------------------------------------
void InputComponent::doInit2(void)
   {
   if (allowSparseData)
      writeString("Sparse data is allowed");
   else
      writeString("Sparse data is not allowed");
   string msg = "INPUT File name: " + fileName;
   writeString(msg.c_str());
   }
// ------------------------------------------------------------------
// read in all constants from file.
// ------------------------------------------------------------------
void InputComponent::readConstants(void)
   {
   vector<string> constantNames;
   data->getConstantNames(constantNames);

   // read in each constant
   for (vector<string>::iterator constantI = constantNames.begin();
                                 constantI != constantNames.end();
                                 constantI++)
      {
      string value = data->getConstant(*constantI);
      string units;
      unsigned posStartUnit = value.find("(");
      if (posStartUnit != string::npos)
         {
         unsigned posEndUnit = value.find(")");
         if (posEndUnit == value.length()-1)
            {
            units = value.substr(posStartUnit, posEndUnit-posStartUnit+1);
            value = value.erase(posStartUnit, posEndUnit-posStartUnit+1);
            }
         }
      addVariable(*constantI, units, value, 1, false, false);
      }
   }
// ------------------------------------------------------------------
// read in all headings from file.
// ------------------------------------------------------------------
void InputComponent::readHeadings(void)
   {
   vector<string> fieldNames, fieldUnits;
   data->getFieldNames(fieldNames);
   data->getFieldUnits(fieldUnits);

   // Loop through all field names and look for an array specifier.
   // If one is found then strip it off, then add the variable to
   // our variables container.
   for (unsigned int fieldI = 0; fieldI != fieldNames.size(); fieldI++)
      {
      if (Str_i_Eq(fieldNames[fieldI], "year") ||
          Str_i_Eq(fieldNames[fieldI], "day"))
         fieldNames[fieldI] = string(name) + "_" + fieldNames[fieldI];
      string fieldNameMinusSpec;
      unsigned int arraySpec;
      bool isArray = removeArraySpec(fieldNames[fieldI], fieldNameMinusSpec, arraySpec);

      addVariable(fieldNameMinusSpec,
                  fieldUnits[fieldI],
                  data->getFieldValue(fieldI),
                  arraySpec, true, isArray);
      }
   }

// ------------------------------------------------------------------
// Find a specific variable whose name matches the one passed in.
// ------------------------------------------------------------------
InputComponent::InputVariables::iterator InputComponent::findVariable(const string& name)
   {
   for (InputVariables::iterator inputVariable = variables.begin();
                                 inputVariable != variables.end();
                                 inputVariable++)
      {
      if (Str_i_Eq(inputVariable->second->getName(), name))
         return inputVariable;
      }
   return variables.end();
   }
// ------------------------------------------------------------------
// Strip off the array specifier if found.  Return the variable
// name with the array specifier removed, and the array index.
// Returns true if field is an array.
// ------------------------------------------------------------------
bool InputComponent::removeArraySpec(const string& fieldName,
                                     string& fieldNameMinusSpec,
                                     unsigned int& arraySpec)
   {
   unsigned posStartArraySpec = fieldName.find("(");
   if (posStartArraySpec != string::npos)
      {
      unsigned posEndArraySpec = fieldName.find(")");
      if (posEndArraySpec == fieldName.length()-1)
         {
         fieldNameMinusSpec = fieldName.substr(0, posStartArraySpec);
         arraySpec = atoi(fieldName.substr(posStartArraySpec+1,
                                           posEndArraySpec-posStartArraySpec-1).c_str());
         return true;
         }
      else
         {
         string msg = "Invalid array specification on INPUT field name: ";
         msg += fieldName;
         throw runtime_error(msg);
         }
      }
   else
      {
      fieldNameMinusSpec = fieldName;
      arraySpec = 1;
      }
   return false;
   }

// ------------------------------------------------------------------
// Register all our variables.
// ------------------------------------------------------------------
void InputComponent::addVariable(const std::string& name,
                                 const std::string& units,
                                 const std::string& value,
                                 unsigned arrayIndex,
                                 bool isTemporal,
                                 bool isArray)
   {
   StringVariant* stringVariant;
   InputVariables::iterator variableI = findVariable(name);
   if (variableI == variables.end())
      {
      stringVariant = new StringVariant(this, name, units, value, !isTemporal);
      }
   else
      {
      stringVariant = variableI->second;
      if (value.find(" ") != string::npos)
         stringVariant->addValues(value);
      else
         stringVariant->addValue(value, arrayIndex-1);
      }
   if (isTemporal && find(temporalVariables.begin(), temporalVariables.end(),
                          stringVariant) == temporalVariables.end())
      temporalVariables.push_back(stringVariant);

   if (isArray)
      stringVariant->setIsArray();
   if (units != "")
      stringVariant->setUnits(units);

   // Last step - do the registration now that the stringVariant has a value
   // use the registration ID as the index into the variables map.
   unsigned int regID = stringVariant->doRegistration();
   variables.insert(InputVariables::value_type(regID, stringVariant));
   }
// ------------------------------------------------------------------
// Check to see if we need to handle sparse data or not.
// ------------------------------------------------------------------
void InputComponent::checkForSparseData(void)
   {
   InputVariables::iterator i = findVariable("allow_sparse_data");
   if (i == variables.end())
      allowSparseData = false;
   else
      i->second->asLogical(allowSparseData);
   }
// ------------------------------------------------------------------
// Advance the file to todays date.
// ------------------------------------------------------------------
bool InputComponent::advanceToTodaysData(void)
   {
   double fileDate;

   do
      {
      readLineFromFile();
      fileDate = getFileDate();
      }
   while (fileDate < todaysDate && data->next());

   if (fileDate == todaysDate)
      {
      data->next();
      for (InputVariables::iterator i = variables.begin();
                                    i != variables.end();
                                    ++i)
         i->second->useConstantValues(false);

      return true;
      }
   else
      {
      for (InputVariables::iterator i = variables.begin();
                                    i != variables.end();
                                    ++i)
         i->second->useConstantValues(true);
      return false;
      }
   }
// ------------------------------------------------------------------
// Get a file date from the variables container.
// ------------------------------------------------------------------
unsigned long InputComponent::getFileDate(void)
   {
   int year;
   yearI->asInteger(year);

   if (dayOfYearI != NULL)
      {
      int day_of_year;
      dayOfYearI->asInteger(day_of_year);
      GDate date;
      date.Set(day_of_year, year);
      return date.Get_jday();
      }
   else
      {
      int day;
      dayOfMonthI->asInteger(day);
      int month;
      monthI->asInteger(month);
      GDate date;
      date.Set(day, month, year);
      return date.Get_jday();
      }
   }
// ------------------------------------------------------------------
// If dates are not ok then throw error.
// ------------------------------------------------------------------
void InputComponent::dateFieldsOk(void)
   {
   InputVariables::iterator i = findVariable(string(name) + "_year");
   if (i != variables.end())
      yearI = i->second;
   i = findVariable(string(name) + "_day");
   if (i != variables.end())
      dayOfYearI = i->second;
   i = findVariable("day_of_month");
   if (i != variables.end())
      dayOfMonthI = i->second;
   i = findVariable("month");
   if (i != variables.end())
      monthI = i->second;
   bool ok = (yearI != NULL &&
           (dayOfYearI != NULL ||
            (dayOfMonthI != NULL && monthI != NULL)));
   if (!ok)
      throw runtime_error("APSIM input files must have year and day OR day, "
                          "month and year columns.");
   }

// ------------------------------------------------------------------
// Reads in a line of data from file and stores values in
// 'variables'.  Return true if values read ok.
// ------------------------------------------------------------------
void InputComponent::readLineFromFile()
   {
   // loop through all temporal variables (ie. goto end of variables
   // container) and get a value from the line and store into the
   // variable.  Each variable knows how many values to expect from
   // the line.
   TemporalVariables::iterator variableI = temporalVariables.begin();
   string value;
   unsigned fieldNumber = 0;
   while (variableI != temporalVariables.end())
      {
      for (unsigned valueI = 0; valueI != (*variableI)->numValues(); valueI++)
         {
         value = data->getFieldValue(fieldNumber++);
         (*variableI)->addValue(value, valueI);
         }
      variableI++;
      }
   }
// ------------------------------------------------------------------
// return a variable to caller.
// ------------------------------------------------------------------
void InputComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   if (queryData.ID == daylengthID)
      sendVariable(queryData, calcDayLength());
   else
      variables[queryData.ID]->sendVariable(queryData);
   }

// ------------------------------------------------------------------
// set the value of one of our variables.
// ------------------------------------------------------------------
bool InputComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   variables[setValueData.ID]->setVariable(setValueData);
   return true;
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void InputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == tickID)
      {
      protocol::timeType tick;
      todaysDate = tick.startday;
      if (!advanceToTodaysData() && !allowSparseData)
         {
         GDate errorDate;
         errorDate.Set((unsigned long) todaysDate);

         string msg = "Cannot find data in INPUT file for day: ";
         msg += IntToStr(errorDate.Get_day_of_year()).c_str();
         msg += " and year: ";
         msg += IntToStr(errorDate.Get_year()).c_str();
         error(msg.c_str(), true);
         }
      else
         publishNewMetEvent();
      }
   }
// ------------------------------------------------------------------
// Find a value and return it's numerical value.  Returns true if
// variable is found and it has a value.
// ------------------------------------------------------------------
bool InputComponent::getVariableValue(const string& name, float& value)
   {
   InputVariables::iterator i = findVariable(name);
   if (i != variables.end())
      return i->second->asFloat(value);
   else
      return false;
   }
// ------------------------------------------------------------------
// Calculate and return day length.
// ------------------------------------------------------------------
float InputComponent::calcDayLength(void)
   {
   GDate currentDate;
   currentDate.Set(todaysDate);

   float latitude;
   if (getVariableValue("latitude", latitude))
      {
      // Twilight is defined as the interval between sunrise or sunset and the
      // time when the true centre of the sun is 6 degrees below the horizon.
      // Sunrise or sunset is defined as when the true centre of the sun is 50'
      // below the horizon.
      float twligt = -6.0;
      unsigned dayOfYear = currentDate.Get_day_of_year();
      return dayLength(dayOfYear, latitude, twligt);
      }
   else
      return 0.0;
   }

// ------------------------------------------------------------------
// Calculate vp
// ------------------------------------------------------------------
float calcVP(float temp_arg)
   {
   return 6.1078 * exp(17.269*temp_arg / (237.3 + temp_arg));
   }
// ------------------------------------------------------------------
// Publish a newmet event.
// ------------------------------------------------------------------
void InputComponent::publishNewMetEvent(void)
   {
   if (iAmMet)
      {
      protocol::newmetType newmet;
      getVariableValue("maxt", newmet.maxt);
      getVariableValue("mint", newmet.mint);
      getVariableValue("radn", newmet.radn);
      getVariableValue("rain", newmet.rain);
      if (!getVariableValue("vp", newmet.vp))
         newmet.vp = calcVP(newmet.mint);
      publish(newmetID, newmet);
      }
   }
// ------------------------------------------------------------------
// Transfer of sign - from FORTRAN.
// The result is of the same type and kind as a. Its value is the abs(a) of a,
// if b is greater than or equal positive zero; and -abs(a), if b is less than
// or equal to negative zero.
// Example a = sign (30,-2) ! a is assigned the value -30
// ------------------------------------------------------------------
float sign(float a, float b)
   {
   if (b >= 0)
      return fabs(a);
   else
      return -fabs(a);
   }
// ------------------------------------------------------------------
// constrains a variable within bounds of lower and upper
//    Returns "lower", if "var" is less than "lower".  Returns "upper"
//    if "var" is greater than "upper".  Otherwise returns "var".
// ------------------------------------------------------------------
float bound(float var, float lower, float upper)
   {
   if (var < lower)
      return lower;
   else if (var > upper)
      return upper;
   else
      return var;
   }
// ------------------------------------------------------------------
// return the time elasped in hours between the specified sun angle
// from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.
// NB There is a small err in cos (90), thus a special
// case is made for this.
// ------------------------------------------------------------------
float InputComponent::dayLength(int dyoyr, float lat, float sun_angle)
   {
   float aeqnox = 82.25;               // equinox
   float pi =  3.14159265359;
   float dg2rdn = (2.0*pi) / 360.0;
   float decsol = 23.45116 * dg2rdn;   // amplitude of declination of sun
                                       //   - declination of sun at solstices.
                                       // cm says here that the maximum
                                       // declination is 23.45116 or 23 degrees
                                       // 27 minutes.
                                       // I have seen else_where that it should
                                       // be 23 degrees 26 minutes 30 seconds -
                                       // 23.44167
   float dy2rdn = (2.0*pi) /365.25;    // convert days to radians
   float rdn2hr = 24.0/(2.0*pi);       // convert radians to hours

   float alt;                          // twilight altitude limited to max/min
                                       //   sun altitudes end of twilight
                                       //   - altitude of sun. (radians)
   float altmn;                        // altitude of sun at midnight
   float altmx;                        // altitude of sun at midday
   float clcd;                         // cos of latitude * cos of declination
   float coshra;                       // cos of hour angle - angle between the
                                       //   sun and the meridian.
   float dec;                          // declination of sun in radians - this
                                       //   is the angular distance at solar
                                       //   noon between the sun and the equator.
   float hrangl;                       // hour angle - angle between the sun
                                       //   and the meridian (radians).
   float hrlt;                         // day_length in hours
   float latrn;                        // latitude in radians
   float slsd;                         // sin of latitude * sin of declination
   float sun_alt;                      // angular distance between
                                       // sunset and end of twilight - altitude
                                       // of sun. (radians)
                                       // Twilight is defined as the interval
                                       // between sunrise or sunset and the
                                       // time when the true centre of the sun
                                       // is 6 degrees below the horizon.
                                       // Sunrise or sunset is defined as when
                                       // the true centre of the sun is 50'
                                       // below the horizon.

   sun_alt = sun_angle * dg2rdn;

   // calculate daylangth in hours by getting the
   // solar declination (radians) from the day of year, then using
   // the sin and cos of the latitude.

   // declination ranges from -.41 to .41 (summer and winter solstices)

   dec = decsol*sin (dy2rdn* (dyoyr - aeqnox));

   // get the max and min altitude of sun for today and limit
   // the twilight altitude between these.

   if (fabs(lat) == 90.0)
      coshra = sign (1.0, -dec) * sign (1.0, lat);
   else
      {
      latrn = lat*dg2rdn;
      slsd = sin(latrn)*sin(dec);
      clcd = cos(latrn)*cos(dec);

      altmn = asin (bound (slsd - clcd, -1.0, 1.0));
      altmx = asin (bound (slsd + clcd, -1.0, 1.0));
      alt = bound (sun_alt, altmn, altmx);

      // get cos of the hour angle
      coshra = (sin (alt) - slsd) /clcd;
      coshra = bound (coshra, -1.0, 1.0);
      }

   // now get the hour angle and the hours of light
   hrangl = acos (coshra);
   hrlt = hrangl*rdn2hr*2.0;
   return hrlt;
   }

