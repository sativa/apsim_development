//---------------------------------------------------------------------------

#ifndef HowwetRainfallH
#define HowwetRainfallH

class RainfallFile; // forward

//---------------------------------------------------------------------------
// Return the names of the previous 'n' rainfall titles accessed.
//---------------------------------------------------------------------------
extern "C" void _export getPreviousRainTitles(char* titlesCSV);

//---------------------------------------------------------------------------
// Open the specified rainfall file.
//---------------------------------------------------------------------------
extern "C" RainfallFile* _export newRainfallFile();

//---------------------------------------------------------------------------
// Close the specified rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export deleteRainfallFile(RainfallFile* rainfallFile);

//---------------------------------------------------------------------------
// Open a rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export openRainFile(RainfallFile* rainfallFile, char* fileName);

//---------------------------------------------------------------------------
// Open a previous rainfall title.
//---------------------------------------------------------------------------
extern "C" void _export openPreviousRainTitle(RainfallFile* rainfallFile, unsigned rainTitleIndex);

//---------------------------------------------------------------------------
// Return the title of the current rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export getTitle(RainfallFile* rainfallFile, char* title);

//---------------------------------------------------------------------------
// Return the amount of rainfall for the specified date.
//---------------------------------------------------------------------------
extern "C" float _export getRain(RainfallFile* rainfallFile, unsigned julianDay);

//---------------------------------------------------------------------------
// Return the total amount of rainfall for the specified month.
//---------------------------------------------------------------------------
extern "C" float _export getRainForMonth(RainfallFile* rainfallFile, int month, int year);

//---------------------------------------------------------------------------
// Display the rainfall manipulation form
//---------------------------------------------------------------------------
extern "C" void _export showRainfallManipulation(RainfallFile* rainfallFile);

//---------------------------------------------------------------------------
// Return the max and min date range for the specified rainfall file.
//---------------------------------------------------------------------------
extern "C" void _export getRainfallDateRange(RainfallFile* rainfallFile,
                                             unsigned& minDate, unsigned& maxDate);

#endif
