//---------------------------------------------------------------------------
#ifndef ControlFileConverterH
#define ControlFileConverterH

#include <string>
#include <stdexcept>
#include <iosfwd.h>
class ApsimControlFile;
class IniFile;

typedef void __fastcall (__closure *TControlFileConverterEvent)(const std::string& section);
//---------------------------------------------------------------------------
// This class converts a control file from 1 format to another.
//---------------------------------------------------------------------------
class __declspec(dllexport) ControlFileConverter
   {
   public:
      ControlFileConverter() {};

      // ------------------------------------------------------------------
      // Returns true if the specified control file needs converting
      // ------------------------------------------------------------------
      static bool ControlFileConverter::needsConversion(const std::string& fileName);

      //---------------------------------------------------------------------------
      // convert the specified control file using the version number of
      // APSIM and the version number in the control file.
      // Throws an exception if a problem was encountered.
      // If callback is not null, then it will be called for every section
      // in con file being converter.
      //---------------------------------------------------------------------------
      void convert(const string& fileName,
                   TControlFileConverterEvent callback) throw(runtime_error);

      //---------------------------------------------------------------------------
      // convert the specified control file using the commands in the specified
      // script file name.  Throws an exception if a problem was encountered.
      // If callback is not null, then it will be called for every section
      // in con file being converter.
      //---------------------------------------------------------------------------
      void convert(const string& fileName,
                   const string& scriptFileName,
                   TControlFileConverterEvent callback) throw(runtime_error);

   private:
      ApsimControlFile* con;
      std::string conSection;
      IniFile* script;
      std::string parFileToUse;
      std::ofstream log;

      //---------------------------------------------------------------------------
      // convert the control file using the commands in the specified section
      // in the script file.  Throws an exception if a problem was encountered.
      // Returns true on success.
      //---------------------------------------------------------------------------
      bool convertSection(const string& sectionName) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Evalulate the specified expression and return a value.  Returns true on
      // success.
      //---------------------------------------------------------------------------
      bool evaluate(const string& expression, string& value) const throw(runtime_error);

      // ------------------------------------------------------------------
      // evaluate the date arguments passed in and return a date.
      // ------------------------------------------------------------------
      bool evaluateDate(const string& arguments, string& value) const throw(runtime_error);

      //---------------------------------------------------------------------------
      // Execute the SetParameterValue command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeSetParameterValue(const string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Execute the RenameParameter command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeRenameParameter(const string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Execute the DeleteParameter command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeDeleteParameter(const string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Execute the ChangeInstantiation command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeChangeInstantiation(const string& arguments) throw(runtime_error);

      // ------------------------------------------------------------------
      // Execute the newFormatReportVariables function call
      // ------------------------------------------------------------------
      bool executeRemoveReportOutputSwitch
         (const string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Execute the MoveParameter command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeMoveParameter(const string& arguments) throw(runtime_error);

      // ------------------------------------------------------------------
      // Execute the NewFormatReportVariables command. Returns true on success
      // ------------------------------------------------------------------
      bool executeNewFormatReportVariables(const std::string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // move all parameters out of the control file and into a parameter file.
      //---------------------------------------------------------------------------
      bool executeMoveParametersOutOfCon(const std::string arguments) throw(runtime_error);

      // ------------------------------------------------------------------
      // Execute the RemoveSumAvgToTracker command. Returns true on success
      // ------------------------------------------------------------------
      bool executeRemoveSumAvgToTracker(const std::string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Execute the executeRemoveTrackerDefault command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeRemoveTrackerDefault(const std::string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // To a search and replace on all report variables
      //---------------------------------------------------------------------------
      bool executeSearchReplaceReportVariables(const std::string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Add a parameter file reference to all instances of a module in con file.
      //---------------------------------------------------------------------------
      bool executeAddParamFileToModule(const string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Remove any period characters in tracker variables.
      //---------------------------------------------------------------------------
      bool removePeriodsInReportAndTracker(const string& arguments) throw(runtime_error);

      //---------------------------------------------------------------------------
      // Rework the tracker variables to new format.
      //---------------------------------------------------------------------------
      bool ReworkTrackerVariables(const string& arguments) throw(runtime_error);

   };

// ------------------------------------------------------------------
// Returns true if the specified control file needs converting
// ------------------------------------------------------------------
bool needToConvertControlFile(const std::string& fileName);

//---------------------------------------------------------------------------
// convert the specified control file.
// Throws an error if a problem was encountered.
//---------------------------------------------------------------------------
void convertControlFile(const std::string& fileName) throw (std::runtime_error);

#endif

