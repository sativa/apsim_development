#include <consts.h>

#include <gobjs\ginifile.h>

#include <stdio.h>

// *******************************************************************
       void GIni_file::Set_file_name(const GString& File_name)  {
// *******************************************************************

//  Short description:
//    set the name of the ini file.

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      OFSTRUCT of;

// -------------------- Executable code section ----------------------

   if (File_name.find("\\") != NPOS)
      {
      INI_file_name = File_name;
      }
   else
      {
      if (OpenFile(File_name.c_str(),&of,OF_EXIST) != -1)
         INI_file_name = of.szPathName;
      }
   }

// *******************************************************************
       void GIni_file::Read(const GString& Section, const GString& Key_name,
                            GString& Str)  {
// *******************************************************************

//  Short description:
//    read a GString from ini file.

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      char INI_string[500];            // String read from .ini file

// -------------------- Executable code section ----------------------

   GetPrivateProfileString(Section.c_str(), Key_name.c_str(),
      "", INI_string, sizeof(INI_string), INI_file_name.Full_path().c_str());

   Str = INI_string;

   size_t Pos = Str.find(";");
   if (Pos != NPOS)
      Str.remove(Pos);
   }

// *******************************************************************
       void GIni_file::Read(const GString& Section, const GString& Key_name,
                            char *Str, size_t Size)  {
// *******************************************************************

//  Short description:
//    read a char* from ini file.

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      GString INI_string;              // String read from .ini file

// -------------------- Executable code section ----------------------

   Read(Section, Key_name, INI_string);
   strncpy(Str, INI_string.c_str(), Size);
   }

// *******************************************************************
       void GIni_file::Read_list(const GString& Section, const GString& Key_name,
                                 String_array& String_list)  {
// *******************************************************************

//  Short description:
//    read a list of strings

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      int Current_index = 1;           // current key name to look for.
      char INI_string[500];            // String read from .ini file
      char Key[100];                   // Key to search for
      GString Stored_string;           // String being stored in list

// -------------------- Executable code section ----------------------

   // Empty string list

   String_list.Flush();

   wsprintf(Key, "%s%i", Key_name.c_str(), Current_index);
   GetPrivateProfileString(Section.c_str(), Key,
      "", INI_string, sizeof(INI_string), INI_file_name.Full_path().c_str());
   while (lstrlen(INI_string) != 0)
      {
      // Store the ini string in the return list.

      Stored_string = INI_string;
      String_list.Add(Stored_string);

      // Create next key name to search for.

      Current_index++;
      wsprintf(Key, "%s%i", Key_name.c_str(), Current_index);

      // Go get next key word from .ini file

      GetPrivateProfileString(Section.c_str(), Key,
         "", INI_string, sizeof(INI_string), INI_file_name.Full_path().c_str());
      }
   }

// *******************************************************************
       void GIni_file::Write(const GString& Section, const GString& Key_name,
                             const GString& Str)  {
// *******************************************************************

//  Short description:
//    write a GString to ini file.

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (Str.length() > 0)
      WritePrivateProfileString(Section.c_str(), Key_name.c_str(),
         Str.c_str(), INI_file_name.Full_path().c_str());
   else
      WritePrivateProfileString(Section.c_str(), Key_name.c_str(),
         NULL, INI_file_name.Full_path().c_str());

   }

// *******************************************************************
       void GIni_file::Write_list(const GString& Section, const GString& Key_name,
                                  String_array& String_list)  {
// *******************************************************************

//  Short description:
//    write a list of strings

//  Notes:

//  Changes:
//    DPH 23/11/94
//    DPH 7/7/95 Modified to call delete_key

//  Calls:

//  Internal variables
      int Current_index;               // current key name to look for.
      char Key[100];                   // Key to search for
      GString Stored_string;           // String being stored in list

// -------------------- Executable code section ----------------------

   for (Current_index = 0;
        Current_index < String_list.GetItemsInContainer();
        Current_index++)
      {
      wsprintf(Key, "%s%i", Key_name.c_str(), Current_index + 1);
      WritePrivateProfileString(Section.c_str(), Key,
         String_list[Current_index].c_str(), INI_file_name.Full_path().c_str());
      }

   wsprintf(Key, "%s%i", Key_name.c_str(), Current_index + 1);
   Delete_key(Section, GString(Key));
   }

// *******************************************************************
       void GIni_file::Delete_key(const GString& Section, const GString& Key_name)  {
// *******************************************************************

//  Short description:
//    Delete the key name from the specified section

//  Notes:

//  Changes:
//    DPH 7/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   WritePrivateProfileString(Section.c_str(), Key_name.c_str(),
      NULL, INI_file_name.Full_path().c_str());
   }

// *******************************************************************
       void GIni_file::Delete_section(const GString& Section)  {
// *******************************************************************

//  Short description:
//    Delete the section from the .ini file

//  Notes:

//  Changes:
//    DPH 7/7/95

//  Calls:

//  Internal variables
      GPath Temp_file_name;            // New output file name
      GString Line;                    // Line as read from .ini file
      GString Our_section;             // The section to look for

// -------------------- Executable code section ----------------------

   // Since Windows doesn't seem to have a delete section routine we have
   // to create our own.

   // Create the section to look for.

   Our_section = '[' + Section + ']';

   // Go open input and output streams.

   ifstream In_stream(INI_file_name.Full_path().c_str());
   Temp_file_name = INI_file_name;
   Temp_file_name.Set_extension("tmp");
   ofstream Out_stream(Temp_file_name.Full_path().c_str());

   // Go find the section in the .ini file.  Echo all lines up to the section
   // to the output stream.

   bool Found = FALSE;
   while (In_stream && !Found)
      {
      Line.read_to_delim(In_stream, '\n');
      Found = (Line == Our_section);
      if (!Found)
         Out_stream << Line << endl;
      }

   // If we've found our section then ignore all lines up to the start of the
   // next section.

   while (In_stream && Found)
      {
      Line.read_to_delim(In_stream, '\n');
      if (Line != "")
         Found = (Line[0] != '[');
      }

   // If found then simply copy all remaining lines to output stream.

   while (In_stream)
      {
      Out_stream << Line << endl;
      Line.read_to_delim(In_stream, '\n');
      }

   // Close all files, delete current .ini file an rename our .ini file
   // to the new name.

   In_stream.close();
   Out_stream.close();
   unlink(INI_file_name.Full_path().c_str());
   rename(Temp_file_name.Full_path().c_str(), INI_file_name.Full_path().c_str());
   }

