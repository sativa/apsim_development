//---------------------------------------------------------------------------

#ifndef ApsimCommandsH
#define ApsimCommandsH
//---------------------------------------------------------------------------
// Send all files to EXCEL.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall excelFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsvisFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to Apsim Outlook.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimoutlookFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall runFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Convert all files to SIM format.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall createSimFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Compile all files
//---------------------------------------------------------------------------
extern "C" _export void __stdcall makeFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Build all files.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall buildFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to an editor.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall viewFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Send all files to ApsimReport
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimReportFiles(const char* csvFiles);

//---------------------------------------------------------------------------
// Open an interface file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall interfaceFiles(const char* csvFiles);

#endif