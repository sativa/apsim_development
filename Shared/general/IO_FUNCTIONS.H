//---------------------------------------------------------------------------
#ifndef io_functionsH
#define io_functionsH

#include <string>
#include <vector>
#include <dir.h>

std::string ExpandFileName(const char *);
bool FileExists (const std::string &);
bool DirectoryExists (const std::string &d);

//---------------------------------------------------------------------------
// Remove the path and extension from the specified file.
//---------------------------------------------------------------------------
void RemovePathAndExtension(std::string& fileName);

//---------------------------------------------------------------------------
// Return the temporary directory.
//---------------------------------------------------------------------------
std::string GetTempDir(void);

// ------------------------------------------------------------------
// Return a list of files/directories to caller.
// ------------------------------------------------------------------
void getDirectoryListing(const std::string& directoryName,
                         const std::string& extension,
                         std::vector<std::string>& dirList,
                         unsigned int attribute = FA_NORMAL,
                         bool fullPath = false);

// ------------------------------------------------------------------
// Remove invalid file name characters from the specified string e.g. / \ | *
// ------------------------------------------------------------------
void removeInvalidFileNameChars(std::string& fileName);

#endif
