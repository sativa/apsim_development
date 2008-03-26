#include <consts.h>

#include <gobjs\gpath.h>

#include <dir.h>
#include <io.h>
#include <fcntl.h>
#include <direct.h>

// *******************************************************************
       GString GPath::Name(void)  {
// *******************************************************************

//  Short description:
//    Return the file name part of path

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Name_part;               // Name part of string.
      size_t Start_pos;                // Starting position of name

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   Start_pos = Path_way.find_last_of("\\");
   if (Start_pos == NPOS)
      Start_pos = Path_way.find_last_of("/");

   if (Start_pos != NPOS)
      {
      Start_pos++;
      Name_part.assign(Path_way.substr(Start_pos));
      }
   else
      Name_part.assign(Path_way);

   return Name_part;
   }

// *******************************************************************
       GString GPath::Directory(void) {
// *******************************************************************

//  Short description:
//    Return the directory part of path.  

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Dir_part;                 // Directory part of string.
      size_t End_pos;                  // Starting position of name

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   End_pos = Path_way.find_last_of("\\");
   if (End_pos == NPOS)
      End_pos = Path_way.find_last_of("/");

   if (End_pos != NPOS)
      {
      // Need to check for drive specification immediately before the '\'
      // e.g. c:\.  If found then return the 'c:\' NOT 'c:'

      if (Path_way[End_pos] == ':')
         End_pos++;

      Dir_part.assign(Path_way.substr(0, End_pos));
      }

   return Dir_part;
   }

// *******************************************************************
       GString GPath::Extension(void)  {
// *******************************************************************

//  Short description:
//    Return the extension part of path.  e.g. if file = 'apsim.out' then
//    then extension returned = 'out'

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Ext_part;                 // Extension part of string.
      size_t Start_pos;                // Starting position of name

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   Start_pos = Path_way.find(".");
   if (Start_pos != NPOS)
      {
      Start_pos++;
      Ext_part.assign(Path_way.substr(Start_pos));
      }
   return Ext_part;
   }

// *******************************************************************
       GString GPath::Full_path(void) {
// *******************************************************************

//  Short description:
//    Return the full path GPath

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   return Path_way;
   }

// *******************************************************************
       bool GPath::Is_empty(void)  {
// *******************************************************************

//  Short description:
//    Return TRUE if path is empty.  False otherwise.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   return (Path_way.length() == 0);
   }

// *******************************************************************
       void GPath::To_unix(const GString& DOS_file_system,
                           const GString& UNIX_file_system)  {
// *******************************************************************

//  Short description:
//    Convert the path to unix format.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   if (DOS_file_system.length() != 0 && UNIX_file_system.length() != 0)
      {
      Path_way.Replace(DOS_file_system, UNIX_file_system);
      Path_way.Replace("\\", "/");
      }
   }

// *******************************************************************
       void GPath::To_dos(const GString& DOS_file_system,
                          const GString& UNIX_file_system)  {
// *******************************************************************

//  Short description:
//    Convert the path to DOS format.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   if (DOS_file_system.length() != 0 && UNIX_file_system.length() != 0)
      {
      Path_way.Replace(UNIX_file_system, DOS_file_system);
      Path_way.Replace("//", "\\");
      }
   }

// *******************************************************************
       void GPath::Set_to_cwd(void)  {
// *******************************************************************

//  Short description:
//    Delete the path and set it to the current working directory.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      char Buffer[_MAX_PATH];          // Current directory name.

// -------------------- Executable code section ----------------------

   getcwd(Buffer, sizeof(Buffer));
   Path_way.assign(Buffer);
   Path_way.append("\\");
   Path_way.to_lower();
   }

// *******************************************************************
       void GPath::Set_name(const GString& File_name)  {
// *******************************************************************

//  Short description:
//    Set the file name part of path

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Dir;                     // Directory part of name
      GString Ext;                     // Extension part of name

// -------------------- Executable code section ----------------------

   Dir = Directory();
   Ext = Extension();

   if (Dir.length() == 0)
      Path_way.assign(File_name);

   else
      {
      if (Dir.Get_last_char() != '\\')
         Dir.append("\\");

      Path_way.assign(Dir + File_name);
      }

   if (Extension().length() == 0)
      Set_extension(Ext);
   }

// *******************************************************************
       void GPath::Set_directory(const GString& Directory)  {
// *******************************************************************

//  Short description:
//    Set the directory part of path

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString File_name;               // File_name part of name
      GString Ext;                     // Extension part of name

// -------------------- Executable code section ----------------------

   File_name = Name();
   Ext = Extension();

   Path_way.assign(Directory);
   if (Path_way.Get_last_char() != '\\')
      Path_way.append("\\");

   Path_way.append(File_name);

   if (Extension().length() == 0)
      Set_extension(Ext);
   }

// *******************************************************************
       void GPath::Set_extension(const GString& Extension)  {
// *******************************************************************

//  Short description:
//    Set the extension part of path

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Dir;                     // Directory part of name
      GString File_name;               // File_name part of name
      GString Ext;                     // Extension to add
      size_t Pos;                      // Position of extension

// -------------------- Executable code section ----------------------

   Dir = Directory();
   File_name = Name();
   Ext = Extension;

   if (Ext.length() > 0 && Ext.find(".") == NPOS)
      Ext = "." + Ext;
   if (Dir.length() > 0 && Dir.Get_last_char() != '\\')
      Dir = Dir + "\\";
   Pos = File_name.find(".");
   if (Pos != NPOS)
      File_name.remove(Pos);

   Path_way.assign(Dir + File_name + Ext);

   }

// *******************************************************************
       void GPath::Make_relative(void)  {
// *******************************************************************

//  Short description:
//    Make sure the path is relative to current working directory.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GPath Current_dir;               // Current directory.
      GString Dot_dir;                 // Dot directory

// -------------------- Executable code section ----------------------

   Dot_dir.assign(".\\");

   Current_dir.Set_to_cwd();
   Path_way.Replace(Current_dir.Full_path(), Dot_dir);

   // If pathway doesn't have a directory part to it then add one.

   if (Directory().length() == 0)
      Path_way.prepend(".\\");
   }

// *******************************************************************
       void GPath::Make_absolute(void)  {
// *******************************************************************

//  Short description:
//    Make sure the path is an absolute directory.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      char Buffer[_MAX_PATH];          // Buffer to hold the absolute path

// -------------------- Executable code section ----------------------

   _fullpath(Buffer, Path_way.c_str(), sizeof(Buffer));
   if (strlen(Buffer) != 0)
      Path_way.assign(Buffer);
   }

// *******************************************************************
      bool GPath::Exists(void)  {
// *******************************************************************

//  Short description:
//    Does the current file exist ?

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      const int Not_open = -1;         // Handle returned when can't open file
      int Handle;                      // File handle of open file
      int Exists;                      // Does file exist ?

// -------------------- Executable code section ----------------------

   // Try to open file.  If it exists return TRUE and close handle
   // If it doesn't exist, return FALSE

   Handle = open(Full_path().c_str(), O_TEXT);
   Exists = (Handle != Not_open);
   if (Exists)
      close(Handle);
   return Exists;
   }

// *******************************************************************
       void GPath::Change_directory(void)  {
// *******************************************************************

//  Short description:
//    Change the current working directory to what the path holds.

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      GString Drv_letter;              // Drive letter of specified drive.

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   chdir(Directory().c_str());

   // Change the drive as well.

   if (Path_way.find(":") == 1)
      {
      int Drive_letter = Path_way[0] - 'a' + 1;
      _chdrive(Drive_letter);
      }
   }

// *******************************************************************
       GString GPath::Drive(void)  {
// *******************************************************************

//  Short description:
//    Return the drive part of path.  e.g. c:

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Drv_part;                // Drive part of string.
      size_t End_pos;                  // Starting position of name
      GPath CWD;                       // Current working directory

// -------------------- Executable code section ----------------------

   Path_way.to_lower();
   End_pos = Path_way.find(":");
   if (End_pos != NPOS)
      {
      // Got a drive - good.

      End_pos++;
      Drv_part.assign(Path_way.substr(0, End_pos));
      }
   else
      {
      CWD.Set_to_cwd();
      Drv_part = CWD.Drive();          // recursion but ok.
      }

   return Drv_part;
   }

