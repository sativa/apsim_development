#include <consts.h>

#include <gobjs\gstring.h>

#include <iostream.h>
#include <strstrea.h>
#include <stdio.h>

IMPLEMENT_CASTABLE(GString);
IMPLEMENT_STREAMABLE(GString);

// *******************************************************************
      void GString::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GString to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   string& St = *GetObject();
   os << St;
   }

// *******************************************************************
      void *GString::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GString from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   string& St = *GetObject();
   is >> St;

   return GetObject();
   }

// *******************************************************************
		void GString::Replace(const GString& String1, const GString& String2)  {
// *******************************************************************

//  Short description:
//    Replace string 1 with string 2 and return result.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      size_t Pos;                      // Position in string

// -------------------- Executable code section ----------------------

   // Determine if there is something to replace.

   Pos = find(String1);
   while (Pos != NPOS)
      {
      replace(Pos, String1.length(), String2);
      Pos = find(String1, Pos + String2.length());
      }
   }

// *******************************************************************
       void GString::Replace(const char *String1, const char *String2)  {
// *******************************************************************

//  Short description:
//    Replace string 1 with string 2 and return result.

//  Notes:

//  Changes:
//    DPH 17/11/94

//  Calls:

//  Internal variables
      GString Str1;                    // String1
      GString Str2;                    // String2

// -------------------- Executable code section ----------------------

   Str1.assign(String1);
   Str2.assign(String2);
   Replace(Str1, Str2);
   };

// *******************************************************************
       void GString::Assign (const GString& Str, size_t Pos, size_t n)  {
// *******************************************************************

//  Short description:
//    Assign a portion of string Str

//  Notes:

//  Changes:
//    DPH 18/11/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   assign(Str, Pos);
   if (n < length())
      remove(n);
   }

// *******************************************************************
       char GString::Read_token(istream& In_stream,
                                const GString& Init_skip_chars,
                                const GString& Delimiter_chars)  {
// *******************************************************************

//  Short description:
// Read in a token.  Skip all leading skip characters and stop when a delimiter
// character is found.  Returns the character causing a parse stop.

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      char Character[2];               // Character read from stream.
      bool Still_skipping_init = true; // Are we still skipping initial chars?

// -------------------- Executable code section ----------------------

   // Clear the strings contents

   assign("");

   // Skip all initial characters.

   while (In_stream && Still_skipping_init)
      {
      In_stream.get((char*) &Character, 2, 0);
      if (strlen(Character) == 0)
         Still_skipping_init = false;

      else if (Init_skip_chars.contains(Character))
         Still_skipping_init = true;

      else
         Still_skipping_init = false;
      }

   // Append characters to string until a delimiter is found.

   while (strlen(Character) != 0 && !Delimiter_chars.contains(Character) && In_stream)
      {
      append(Character);
      In_stream.get((char*) &Character, 2, 0);
      }

   // If reached end of file then return 0;

   if (!In_stream)
      return 0;

   return Character[0];
   }

// *******************************************************************
       void *GString::Get_pointer(void)  {
// *******************************************************************

//  Short description:
//    Treat string as a pointer.

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      void *Pointer_to_return;         // pointer to return to caller.

// -------------------- Executable code section ----------------------

   sscanf(c_str(), "%p", &Pointer_to_return);
   return Pointer_to_return;
   }

// *******************************************************************
       void GString::Set_pointer(void *Pointer)  {
// *******************************************************************

//  Short description:
//    Set string up as a pointer

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      char Temp_string[50];            // Temporary string.

// -------------------- Executable code section ----------------------

   sprintf(Temp_string, "%p", Pointer);
   assign(Temp_string);
   }

// *******************************************************************
       float GString::Get_real(void)  {
// *******************************************************************

//  Short description:
//    Treat string as a real value

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      float Real_value = 0.0;          // Real value to return
      char *End_char;

// -------------------- Executable code section ----------------------

   if (length() > 0)
      {
      Real_value = strtod(c_str(), &End_char);
      if (*End_char != NULL && *End_char != ' ')
         Real_value = 0.0;
      }
   return Real_value;
   }

// *******************************************************************
       void GString::Set_real(float Real_value)  {
// *******************************************************************

//  Short description:
//    Set string up as a pointer

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      char Temp_string[50];            // Temporary string.

// -------------------- Executable code section ----------------------

   ostrstream Out_stream(Temp_string, sizeof(Temp_string));
   Out_stream << Real_value << ends;
   assign(Temp_string);
   }

// *******************************************************************
       int GString::Get_integer(void)  {
// *******************************************************************

//  Short description:
//    Treat string as an integer value

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      int Integer_value = 0;             // integer value to return

// -------------------- Executable code section ----------------------

   if (length() > 0)
      Integer_value = atoi(c_str());

   return Integer_value;
   }

// *******************************************************************
       void GString::Set_integer(int Integer_value)  {
// *******************************************************************

//  Short description:
//    Set string up as a pointer

//  Notes:

//  Changes:
//    DPH 21/11/94

//  Calls:

//  Internal variables
      char Temp_string[50];            // Temporary string.

// -------------------- Executable code section ----------------------

   ostrstream Out_stream(Temp_string, sizeof(Temp_string));
   Out_stream << Integer_value << ends;
   assign(Temp_string);
   }

// *******************************************************************
       char GString::Get_last_char(void)  {
// *******************************************************************

//  Short description:
//    Return the last non blank character to caller.

//  Notes:

//  Changes:
//    DPH 23/11/94

//  Calls:

//  Internal variables
      size_t Pos;                      // Position of last non blank character
      char Return_char;                // Character to return to caller.

// -------------------- Executable code section ----------------------

   Pos = find_last_not_of(" ");
   if (Pos > length())
      Return_char = 0;

   else
      Return_char = get_at(Pos);

   return Return_char;
   }

// *******************************************************************
       void GString::Strip(StripType s, char c)  {
// *******************************************************************

//  Short description:
//    Strip away unwanted chars from string.

//  Notes:

//  Changes:
//    DPH 14/12/94

//  Calls:

//  Internal variables
   size_t Pos;                         // Position in string

// -------------------- Executable code section ----------------------

   if (length() > 0)
      {
      if (s == string::Both || s == string::Leading)
         {
         Pos = find_first_not_of(c);
         if (Pos == NPOS)
            Pos = length();
         remove(0, Pos);
         }
      }
   if (length() > 0)
      {
      if (s == string::Both || s == string::Trailing)
         {
         Pos = find_last_not_of(c);
         if (Pos < length() - 1)
            remove(Pos + 1);
         }
      }
   }

// *******************************************************************
       ipstream& operator >> (ipstream& is, String_array& Obj)  {
// *******************************************************************

//  Short description:
//    Read a string array from an ipstream.

//  Notes:

//  Changes:
//    DPH 21/9/95

//  Calls:

//  Internal variables
      int Num_elements;                // Number of elements to add to list.
      GString Str;                     // String read from stream.

// -------------------- Executable code section ----------------------

   Obj.Flush();

   is >> Num_elements;
   for (int Element = 0;
        Element < Num_elements;
        Element++)
      {
      is >> Str;
      Obj.Add(Str);
      }

   return is;
   }

// *******************************************************************
       opstream& operator << (opstream& os, const String_array& Obj)  {
// *******************************************************************

//  Short description:
//    Write a string array to an opstream.

//  Notes:

//  Changes:
//    DPH 21/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   os << Obj.GetItemsInContainer();
   for (int Element = 0;
        Element < Obj.GetItemsInContainer();
        Element++)
      {
      os << Obj[Element];
      }

   return os;
   }

// *******************************************************************
      bool GString::Is_numerical(void)  {
// *******************************************************************

//  Short description:
//    returns TRUE if string is numerical

//  Notes:

//  Changes:
//    DPH 14/12/94

//  Calls:

//  Internal variables
      char *End_char;

// -------------------- Executable code section ----------------------

   strtod(c_str(), &End_char);
   return !(*End_char != NULL && *End_char != ' ');
   }
