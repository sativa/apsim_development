//---------------------------------------------------------------------------
#ifndef io functionsH
#define io functionsH

#include <general\string_functions.h>
#include <list>
#include <dos.h>
// ------------------------------------------------------------------
//  Short description:
//      return a list of files/directories to caller.

//  Notes:

//  Changes:
//    DPH 11/6/1997

// ------------------------------------------------------------------
void Get_directory_listing (const char* Directory_name,
                            const char* Extension,
                            std::list<std::string>& Dir_list,
                            unsigned int Attribute = FA_NORMAL,
                            bool Full_path = false);

// ------------------------------------------------------------------
//  Short description:
//      locate a file and return it's full absolute path name

//  Notes:
//      search file passed in should not contain any directory info.

//  Changes:
//    DPH 11/6/1997

// ------------------------------------------------------------------
std::string Locate_file (std::list<std::string>& Search_directories,
                         const char* Search_file);

// ------------------------------------------------------------------
//  Short description:
//      get a directory listing but make it recursive.

//  Notes:
//      The max_num_levels_to_descend should be set to the number of
//      nested directories to descend into.  If set to some high number
//      (eg 10000) then all directories will be recursed into.  If set
//      to zero then no directories will be recursed into.  If
//      "include_specified_directory" is true then files in the specified
//      "directory" will be returned along with files in child directories.

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Get_recursive_directory_listing(const char* Directory,
                                     const char* File_spec,
                                     int Max_num_levels_to_descend,
                                     bool Include_specified_directory,
                                     std::list<std::string>& Files);

// ------------------------------------------------------------------
//  Short description:
//     copy or move the specified source files to the destination directory

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Copy_files (std::list<std::string>& Source_files,
                 const char* Destination_directory,
                 bool Do_move_files);

// ------------------------------------------------------------------
//  Short description:
//     copy or move the specified source files to the destination
//     directory preserving the directory structure.

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

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Copy_files_preserve_directories(std::list<std::string>& Source_files,
                                     const char* Source_base_directory,
                                     const char* Destination_directory,
                                     bool Do_move_files,
                                     bool Make_files_read_write = false);

// ------------------------------------------------------------------
//  Short description:
//     copy or move the specified source directories and all directories below them
//     to the destination directory.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void  Copy_directories (std::list<std::string>& Source_directories,
                        const char* Destination_directory,
                        bool Do_move_files = false);

// ------------------------------------------------------------------
//  Short description:
//     send the specified directory or files to the
//     recycle bin.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Delete_files_or_directories (std::list<std::string>& Directories);

// ------------------------------------------------------------------
//  Short description:
//     return the youngest file in a given directory that matches the
//     specified filespec.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
std::string Get_youngest_file (const char* Directory,
                               const char* Filespec);

// ------------------------------------------------------------------
// Rename the specified file or folder if there is a name collision.
// ------------------------------------------------------------------
void renameOnCollision(AnsiString& name, bool isFile = true);


#endif
