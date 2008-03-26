//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "HowwetRainfall.h"
#include "TRainfallForm.h"
#include <ApsimShared\ApsimDataFile.h>
#include <boost\date_time\gregorian\gregorian.hpp>
#include <boost\lexical_cast.hpp>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\inifile.h>
#include <general\io_functions.h>
#include <numeric>

#pragma package(smart_init)
using namespace boost;
using namespace boost::gregorian;
using namespace std;
const unsigned MAX_PREVIOUS_RAIN_TITLES = 10;
class RainfallFile
   {
   public:
      //---------------------------------------------------------------------------
      // Return the names of the previous 'n' rainfall files accessed.
      //---------------------------------------------------------------------------
      static void getPreviousRainTitles(vector<string>& rainTitles);

      //---------------------------------------------------------------------------
      // constructor
      //---------------------------------------------------------------------------
      RainfallFile(void);

      //---------------------------------------------------------------------------
      // destructor
      //---------------------------------------------------------------------------
      ~RainfallFile(void);

      //---------------------------------------------------------------------------
      // Open a file.
      //---------------------------------------------------------------------------
      void open(const string& fileName);

      //---------------------------------------------------------------------------
      // Open a previous rainfall title.
      //---------------------------------------------------------------------------
      void openPreviousRainTitle(unsigned rainTitleIndex);

      //---------------------------------------------------------------------------
      // Return the current title.
      //---------------------------------------------------------------------------
      string getTitle(void);

      //---------------------------------------------------------------------------
      // Return the amount of rainfall on the specified day
      //---------------------------------------------------------------------------
      float getRain(date& rainDate);

      //---------------------------------------------------------------------------
      // Return the total amount of rainfall for the specified month.
      //---------------------------------------------------------------------------
      float getRainForMonth(int month, int year);

      //---------------------------------------------------------------------------
      // Return the total amount of rainfall for the specified month.
      //---------------------------------------------------------------------------
      void showRainfallManipulation(void);

      //---------------------------------------------------------------------------
      // Return the max and min date range for the specified rainfall file.
      //---------------------------------------------------------------------------
      void getRainfallDateRange(unsigned& minDate, unsigned& maxDate);

   private:
      ApsimDataFile* data;
      string fileName;

      //---------------------------------------------------------------------------
      // Return the filenames of the previous 'n' rainfall files accessed.
      //---------------------------------------------------------------------------
      static void getPreviousRainFiles(vector<string>& rainFileNames);

      //---------------------------------------------------------------------------
      // Store the specified filename in the previous filenames list.
      //---------------------------------------------------------------------------
      static void storeFileInPreviousList(const string& fileName);

   };
//---------------------------------------------------------------------------
// Return the names of the previous 'n' rainfall files accessed.
//---------------------------------------------------------------------------
void RainfallFile::getPreviousRainFiles(vector<string>& rainFileNames)
   {
   vector<string> files;
   AnsiString iniFileName = ExtractFileDir(Application->ExeName) + "\\howwet.ini";
   IniFile ini(iniFileName.c_str());
   ini.read("Rainfall files", "filename", files);
   for (unsigned f = 0; f != files.size(); f++)
      if (FileExists(files[f].c_str()))
         rainFileNames.push_back(files[f]);
   }
//---------------------------------------------------------------------------
// Return the names of the previous 'n' rainfall files accessed.
//---------------------------------------------------------------------------
void RainfallFile::getPreviousRainTitles(vector<string>& rainTitles)
   {
   getPreviousRainFiles(rainTitles);
   for_each(rainTitles.begin(), rainTitles.end(), RemovePathAndExtension);
   }
//---------------------------------------------------------------------------
// Store the specified filename in the previous filenames list.
//---------------------------------------------------------------------------
void RainfallFile::storeFileInPreviousList(const string& file)
   {
   string fileName = ExpandFileName(file.c_str()).c_str();
   vector<string> rainFileNames;
   getPreviousRainFiles(rainFileNames);

   vector<string> newList;
   newList.push_back(fileName);
   for (unsigned f = 0; f != min(rainFileNames.size(), MAX_PREVIOUS_RAIN_TITLES); f++)
      {
      if (!ExtractFileName(rainFileNames[f].c_str()).AnsiCompareIC(ExtractFileName(fileName.c_str())) == 0)
         newList.push_back(rainFileNames[f]);
      }
   AnsiString iniFileName = ExtractFileDir(Application->ExeName) + "\\howwet.ini";
   IniFile ini(iniFileName.c_str());
   ini.write("Rainfall files", "filename", newList);
   }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
RainfallFile::RainfallFile(void)
   {
   data = new ApsimDataFile;
   vector<string> rainFileNames;
   getPreviousRainFiles(rainFileNames);
   if (rainFileNames.size() > 0)
      open(rainFileNames[0]);
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
RainfallFile::~RainfallFile(void)
   {
   delete data;
   }
//---------------------------------------------------------------------------
// Open a file.
//---------------------------------------------------------------------------
void RainfallFile::open(const string& file)
   {
   fileName = file;
   data->open(fileName);
   storeFileInPreviousList(fileName);
   }
//---------------------------------------------------------------------------
// Open a previous rainfall title.
//---------------------------------------------------------------------------
void RainfallFile::openPreviousRainTitle(unsigned rainTitleIndex)
   {
   vector<string> rainFileNames;
   getPreviousRainFiles(rainFileNames);
   if (rainTitleIndex >= rainFileNames.size())
      throw runtime_error("Cannot find previous rainfall title with index of: "
                          + lexical_cast<string>(rainTitleIndex));
   open(rainFileNames[rainTitleIndex]);
   }
//---------------------------------------------------------------------------
// Return the current title.
//---------------------------------------------------------------------------
string RainfallFile::getTitle(void)
   {
   string title = fileName;
   RemovePathAndExtension(title);
   return title;
   }

//---------------------------------------------------------------------------
// Return the amount of rainfall on the specified day
//---------------------------------------------------------------------------
float RainfallFile::getRain(date& rainDate)
   {
   if (data->eof() || rainDate < data->getDate())
      data->first();
   data->gotoDate(rainDate);

   ApsimDataFile::iterator i = find(data->fieldsBegin(), data->fieldsEnd(),
                                    "rain");
   if (i == data->fieldsEnd())
      throw runtime_error("Cannot find a rain column in file " + fileName);
   return lexical_cast<float>(i->getValue());
   }
//---------------------------------------------------------------------------
// Return the total amount of rainfall for the specified month.
//---------------------------------------------------------------------------
float RainfallFile::getRainForMonth(int month, int year)
   {
   date_period monthPeriod(date(year, month, 1),
                           date(year, month, gregorian_calendar::end_of_month_day(year, month)));
   float rain = 0;
   date d = monthPeriod.begin();
   while (monthPeriod.contains(d))
      {
      rain = rain + getRain(d);
      d = d + date_duration(1);
      }
   return rain;
   }
//---------------------------------------------------------------------------
// Return the total amount of rainfall for the specified month.
//---------------------------------------------------------------------------
void RainfallFile::showRainfallManipulation(void)
   {
   data->close();
   TRainfallForm* form = new TRainfallForm(NULL);
   form->setup(fileName);
   form->ShowModal();
   if (form->getFileName() != "")
      open(form->getFileName());
   delete form;
   }
//---------------------------------------------------------------------------
// Return the max and min date range for the specified rainfall file.
//---------------------------------------------------------------------------
void RainfallFile::getRainfallDateRange(unsigned& minDate, unsigned& maxDate)
   {
   data->first();
   date firstDate = data->getDate();
   data->last();
   date lastDate = data->getDate();
   minDate = firstDate.julian_day();
   maxDate = lastDate.julian_day();
   }



//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Return the names of the previous 'n' rainfall titles accessed.
//---------------------------------------------------------------------------
extern "C" void _export getPreviousRainTitles(char* titlesCSV)
   {
   vector<string> rainTitles;
   RainfallFile::getPreviousRainTitles(rainTitles);
   string st = buildString(rainTitles, ",");
   strcpy(titlesCSV, st.c_str());
   }
//---------------------------------------------------------------------------
// Open the specified rainfall file.
//---------------------------------------------------------------------------
extern "C" RainfallFile* _export newRainfallFile()
   {
   return new RainfallFile;
   }
//---------------------------------------------------------------------------
// Close the specified rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export deleteRainfallFile(RainfallFile* rainfallFile)
   {
   delete rainfallFile;
   }
//---------------------------------------------------------------------------
// Open a rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export openRainFile(RainfallFile* rainfallFile, char* fileName)
   {
   try
      {
      rainfallFile->open(fileName);
      }
   catch (const exception& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
//---------------------------------------------------------------------------
// Open a previous rainfall title.
//---------------------------------------------------------------------------
extern "C" void _export openPreviousRainTitle(RainfallFile* rainfallFile, unsigned rainTitleIndex)
   {
   try
      {
      rainfallFile->openPreviousRainTitle(rainTitleIndex);
      }
   catch (const exception& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
//---------------------------------------------------------------------------
// Return the title of the current rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export getTitle(RainfallFile* rainfallFile, char* title)
   {
   try
      {
      string st = rainfallFile->getTitle();
      strcpy(title, st.c_str());
      }
   catch (const exception& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
//---------------------------------------------------------------------------
// Return the amount of rainfall for the specified date.
//---------------------------------------------------------------------------
extern "C" float _export getRain(RainfallFile* rainfallFile, unsigned julianDay)
   {
   try
      {
      return rainfallFile->getRain(date(julianDay));
      }
   catch (const exception& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      return 0.0;
      }
   }
//---------------------------------------------------------------------------
// Return the total amount of rainfall for the specified month.
//---------------------------------------------------------------------------
extern "C" float _export getRainForMonth(RainfallFile* rainfallFile, int month, int year)
   {
   try
      {
      return rainfallFile->getRainForMonth(month, year);
      }
   catch (const exception& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      return 0.0;
      }
   }
//---------------------------------------------------------------------------
// show the rainfall manipulation
//---------------------------------------------------------------------------
extern "C" void _export showRainfallManipulation(RainfallFile* rainfallFile)
   {
   try
      {
      rainfallFile->showRainfallManipulation();
      }
   catch (const exception& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
//---------------------------------------------------------------------------
// Return the max and min date range for the specified rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export getRainfallDateRange(RainfallFile* rainfallFile,
                                             unsigned& minDate, unsigned& maxDate)
   {
   rainfallFile->getRainfallDateRange(minDate, maxDate);

   }



