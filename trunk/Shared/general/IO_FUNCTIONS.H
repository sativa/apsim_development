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
// Locate a file and return it's full absolute path name
// ------------------------------------------------------------------
std::string locateFile(std::vector<std::string>& searchDirectories,
                       const std::string& searchFile);

// ------------------------------------------------------------------
// Get a directory listing but make it recursive.
//      The max_num_levels_to_descend should be set to the number of
//      nested directories to descend into.  If set to some high number
//      (eg 10000) then all directories will be recursed into.  If set
//      to zero then no directories will be recursed into.  If
//      "include_specified_directory" is true then files in the specified
//      "directory" will be returned along with files in child directories.
// ------------------------------------------------------------------
void getRecursiveDirectoryListing(const std::string& directory,
                                  const std::string& fileSpec,
                                  int maxNumLevelsToDescend,
                                  bool includeSpecifiedDirectory,
                                  std::vector<std::string>& files);

// ------------------------------------------------------------------
// Copy or move the specified source files to the destination directory
// ------------------------------------------------------------------
void copyFiles(std::vector<std::string>& sourceFiles,
               const std::string& destinationDirectory,
               bool doMoveFiles);

// ------------------------------------------------------------------
// Copy or move the specified source files to the destination
// directory preserving the directory structure.
//     The source_base_directory is used to determine the root of all the
//     source files.  It is assumed that all source files are located
//     under this base directory.  Any that aren't are NOT copied.
//
//     Each source file has the "source_base_directory" part of its
//     path replaced with the destination directory. eg:
//        If           source file = c:\apsuite\apsim\chickpea\chickpea.apf
//           source_base_directory = c:\apsuite\apsim
//           destination_directory = c:\apswork
//        Then    destination file = c:\apswork\chickpea\chickpea.apf
// ------------------------------------------------------------------
void copyFilesPreserveDirectories(std::vector<std::string>& sourceFiles,
                                  const std::string& sourceBaseDirectory,
                                  const std::string& destinationDirectory,
                                  bool doMoveFiles,
                                  bool makeFilesReadWrite = false);

// ------------------------------------------------------------------
// Copy or move the specified source directories and all directories below them
// to the destination directory.
// ------------------------------------------------------------------
void copyDirectories(std::vector<std::string>& sourceDirectories,
                     const std::string& destinationDirectory,
                     bool doMoveFiles = false);

// ------------------------------------------------------------------
// Send the specified directory or files to the recycle bin.
// ------------------------------------------------------------------
void deleteFilesOrDirectories(std::vector<std::string>& directories);

// ------------------------------------------------------------------
// Return the youngest file in a given directory that matches the
// specified filespec.
// ------------------------------------------------------------------
std::string getYoungestFile(const std::string& directory,
                            const std::string& filespec);

// ------------------------------------------------------------------
// Rename the specified file or folder if there is a name collision.
// ------------------------------------------------------------------
void renameOnCollision(std::string& name, bool isFile = true);

// ------------------------------------------------------------------
// Remove invalid file name characters from the specified string e.g. / \ | *
// ------------------------------------------------------------------
void removeInvalidFileNameChars(std::string& fileName);

#endif
