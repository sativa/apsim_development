#include <iostream.h>
#include <stdlib.h>
#include <windows.h>
#include <dir.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>
#include "path.h"

using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    Return the drive string

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
string Path::Get_drive(void)
   {
   return Drive;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return the directory part of path.

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
string Path::Get_directory(void)
   {
   return Drive + Directory;
   }

// ------------------------------------------------------------------
//  Short description:
//    return just the file name to the caller.

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
string Path::Get_name(void)
	{
   return Name;
   }

// ------------------------------------------------------------------
//  Short description:
//    return just the file name without the extension to the caller.

//  Notes:

//  Changes:
//    DPH 17/6/97
//    dph 27/3/98 changed NPOS to NPOS in line with standard.

// ------------------------------------------------------------------
string Path::Get_name_without_ext(void)
	{
   size_t Pos_ext = Name.find(".");
   if (Pos_ext != string::npos)
      return Name.substr(0, Pos_ext);
   else
      return Name;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return the extension part of path.  e.g. if file = 'apsim.out' then
//    then extension returned = '.out'

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.
//    dph 27/3/98 changed string::npos to string::npos in line with standard.

// ------------------------------------------------------------------
string Path::Get_extension(void)
	{
   size_t Start_pos = Name.find("."); //XXX??WRONG - search from end
   if (Start_pos != string::npos)
      return Name.substr(Start_pos);
   else
      return "";
   }

// ------------------------------------------------------------------
//  Short description:
//    Return the path

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
string Path::Get_path(void)
   {
   string return_string;
   return_string = Drive + Directory;
   if (return_string.length() > 0)
      {
      char Last_char = return_string[return_string.length()-1];
      if (Last_char != '\\' && Name.length() > 0)
         return_string += "\\";
      }
   return_string += Name;
   return return_string;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return the full path

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
string Path::Get_full_path(void) const
   {
   string Return_string;
   if (!Is_empty())
      {
      Path Full_path = getCurrentFolder();

      if (Drive.length() > 0)
         Full_path.Set_drive (Drive.c_str());

      if (Directory.length() > 0)
         Full_path.Set_directory (Directory.c_str());

      if (Name.length() > 0)
         Full_path.Set_name (Name.c_str());

      Return_string = Full_path.Get_path();
      }

   return Return_string;
   }


// ------------------------------------------------------------------
//  Short description:
//    set the drive

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
void Path::Set_drive (const char* New_drive)
	{
   Drive = New_drive;
   To_lower(Drive);
   }

// ------------------------------------------------------------------
//  Short description:
//    set the directory

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
void Path::Set_directory (const char* New_directory)
	{
   Directory = New_directory;
   To_lower(Directory);
   }

// ------------------------------------------------------------------
//  Short description:
//    set the file name

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
void Path::Set_name (const char* New_name)
	{
   Name = New_name;
   To_lower(Name);
   }

// ------------------------------------------------------------------
//  Short description:
//    set the file extension

//  Notes: must place a "." in front of the intended extension. eg:
//
//         mypath.Set_name("myfile");
//         mypath.Set_extension(".myext");
// 
//  wouuld result in a file named "myfile.myext"
//

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.
//    dph 27/3/98 changed string::npos to string::npos in line with standard.
//

// ------------------------------------------------------------------
void Path::Set_extension (const char* New_extension)
	{
   size_t Pos_extension = Name.find(".");
   if (Pos_extension != string::npos)
      Name.replace(Pos_extension, string::npos, "");
   Name += New_extension;
   To_lower(Name);
   }

// ------------------------------------------------------------------
//  Short description:
//    set the full path.

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.
//    dph 27/3/98 changed string::npos to string::npos in line with standard.
//    dph 5/12/00 enhanced code to look for file extension after
//                directory path D394.

// ------------------------------------------------------------------
void Path::Set_path (const string& New_path)
	{
   if (New_path.length() > 0)
      {
      string New_path_string(New_path);
      stripLeadingTrailing(New_path_string, " ");

      // remove drive part of path.
      size_t Pos_drive = New_path_string.find(":");
      if (Pos_drive != string::npos)
         {
         Drive = New_path_string.substr (0, 2);
         New_path_string.replace (0, 2, "");
         }

      // NO - If pos. of last '.' is AFTER pos. of last '\' then
      // assume last part is a filename.  Otherwise whole path
      // is assumed to be a directory.
      // e.g. d:\apswork1.61\filename.ext = directory + filename
      // e.g. d:\apswork1.61\filename     = directory + no filename
      // e.g. filename                    = filename
      size_t Pos_directory = New_path_string.find_last_of("\\");
      size_t Pos_name = New_path_string.find_last_of(".");
      if (Pos_directory == string::npos)
         {
         Name = New_path_string;
         New_path_string = "";
         }
      else if (Pos_name != string::npos &&
               Pos_name > Pos_directory)
         {
         Pos_name = Pos_directory + 1;
         Name = New_path_string.substr (Pos_name);
         New_path_string.replace (Pos_name-1, string::npos, "");
         }

      // remove last backslash if necessary.
      if (New_path_string != "")
         {
         char Last_char = New_path_string[New_path_string.length()-1];
         if (Last_char == '\\')
            New_path_string.replace(New_path_string.length()-1, string::npos, "");
         }

      // whats left must be the directory
      Directory = New_path_string;
      }
   To_lower(Drive);
   To_lower(Directory);
   To_lower(Name);
   }

// ------------------------------------------------------------------
//  Short description:
//    Return true if the Directory *string* is empty (JW)

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
bool Path::Is_empty(void) const
   {
   return (Directory.length() == 0);
   }

// ------------------------------------------------------------------
//  Short description:
//    Return true if file exists.

//  Notes:
//    Works on file not directory (JW)

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
bool Path::Exists(void)
   {
   return (GetFileAttributes(Get_path().c_str()) != 0xFFFFFFFF);
   }

// ------------------------------------------------------------------
//  Short description:
//    change the directory to that specified.

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
void Path::Change_directory(void)
   {
#ifdef __WIN32__
   if (Directory.length() > 0)
      SetCurrentDirectory (Get_directory().c_str());
#else
   if (Directory.length() > 0)
      chdir (Directory.c_str());

   // Change the drive as well.

   if (Drive.length() > 0)
      {
      int Drive_letter = Drive[0] - 'a' + 1;
      _chdrive(Drive_letter);
      }
#endif
   }

/*void Path::Change_directory(void)
   {
   if (Directory.length() > 0)
      chdir (Directory.c_str());

#ifdef __WIN32__
   // Change the drive on windows as well.
   if (Drive.length() > 0)
      {
      int Drive_letter = Drive[0] - 'a' + 1;
      _chdrive(Drive_letter);
      }
#endif
   }  */
// ------------------------------------------------------------------
//  Short description:
//    append a directory

//  Notes:
//    change to the current working dir (JW)

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
void Path::Append_path (const char* path)
   {
   // need to make sure path is an absolute one.
   // get the current directory because we're going to change it.
   char Saved_directory[500];
   GetCurrentDirectory(sizeof Saved_directory, Saved_directory);

   // Change current directory
   Change_directory();

   // Get the full path name of the directory passed in.  This API routine
   // properly converts any relative paths to full paths.
   char Full_path[500];
   char* Ptr_to_name;
   GetFullPathName(path, sizeof Full_path, Full_path, &Ptr_to_name);

   // restore current directory.
   SetCurrentDirectory (Saved_directory);

   // setup drive.
   Set_path(Full_path);
   }

// ------------------------------------------------------------------
//  Short description:
//    remove the Directory *string* (JW)

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.
//    dph 27/3/98 changed string::npos to string::npos in line with standard.

// ------------------------------------------------------------------
string Path::Back_up_directory (void)
   {
   string Return_string;
   if (Directory.length() > 1)
      {
      size_t Pos_directory = Directory.find_last_of("\\");
      if (Pos_directory == string::npos && Directory[0] == '\\')
         Pos_directory = 0;
      if (Pos_directory != string::npos)
         {
         Return_string = Directory.substr(Pos_directory+1);
         Directory.replace(Pos_directory, string::npos, "");
         }
      else
         Return_string = Directory;
      }
   return Return_string;
   }
// ------------------------------------------------------------------
// Return the current working folder as a path
// ------------------------------------------------------------------
Path Path::getCurrentFolder(void)
	{
   char Buffer[_MAX_PATH];          // Current directory name.

   GetCurrentDirectory (sizeof Buffer, Buffer);
   Path returnPath(Buffer);
   return returnPath;
   }

// ------------------------------------------------------------------
// Return the temporary folder as a path
//
// Note: 
//
// ------------------------------------------------------------------
Path Path::getTempFolder(void)
	{
   char Buffer[_MAX_PATH];          // Current directory name.

   GetTempPath(sizeof Buffer, Buffer);
   Path returnPath(Buffer);
   return returnPath;
   }

std::string fileExtension(const std::string &filename) 
   //---------------------------------------------------------------------------
   // Return the extension (anything after final ".") of a filename.
   {
   string tail = fileTail(filename);
   size_t pos = tail.rfind(".");
   if (pos != string::npos)
      return tail.substr(pos+1);
   return "";
   }
std::string fileTail(const std::string &filename) 
   //---------------------------------------------------------------------------
   // Return the tail (anything after final "\") of a filename.
   {
   size_t pos = filename.rfind("/");
   if (pos != string::npos)
      return filename.substr(pos+1);

   pos = filename.rfind("\\");
   if (pos != string::npos)
      return filename.substr(pos+1);

   return filename;
   }

bool hasDirectories(const string &filename)
   //---------------------------------------------------------------------------
   // Return whether a filename has directories
   {
   if (filename.find("/") != string::npos || filename.find("\\") != string::npos) return 1;
   return 0;
   }
std::string fileRoot(const std::string &filename) 
   //---------------------------------------------------------------------------
   // Return the root (all of the characters in "filename" up to but not including the last "." ).
   {
   string dir;
   if (hasDirectories(filename))
      dir = fileDirName(filename) + "/";
   else 
      dir = "";   

   string tail =  fileTail(filename);
   size_t pos = tail.rfind(".");
   if (pos != string::npos)
      return (dir + tail.substr(0,pos));
   return (dir + tail);
   }

std::string fileDirName(const std::string &filename) 
   //---------------------------------------------------------------------------
   // Return the directory that this file lives in
   {
   size_t pos = filename.rfind("\\");
   if (pos != string::npos)
      return filename.substr(0,pos);
   pos = filename.rfind("/");
   if (pos != string::npos)
      return filename.substr(0,pos);
   return filename;
   }
   
bool fileExists(const std::string &filename)
   //---------------------------------------------------------------------------
   // Return whether a file exists or not
   {
   return (access(filename.c_str(), 0) == 0);
   }