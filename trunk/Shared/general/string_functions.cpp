#include <general\string_functions.h>
#include <tchar.h>
#include <strstream>
#include <iomanip>

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 14/10/97 work around bug in Borland string class in routine
//                 find_last_not_of.  Doesn't work with 2 character string !!
//    dph 27/3/98 changed call to remove with call to replace in line with standard.

// ------------------------------------------------------------------
void Strip (string& text, const char* separators)
   {
   size_t Pos;

   // remove leading spaces
   Pos = text.find_first_not_of(separators);
   if (Pos > 0)
      text.replace (0, Pos, "");

   // remove trailing spaces  strrchr
   text = "`" + text;
   Pos = text.find_last_not_of(separators);
   if (Pos < text.length())
      text.replace (Pos+1, string::npos, "");
   text.replace (0, 1, "");
   }

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Strip (char* text, const char* separators)
   {
   if (text[0] != 0)
      {
      char* Pos1 = _tcsspnp(text, separators);
      char* Pos2 = strchr(Pos1, *separators);
      if (Pos2 != NULL)
         *Pos2 = 0;
      if (Pos1 != NULL && Pos1 != text)
         strcpy (text, Pos1);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return the key name and value on the line.

//  Notes:

//  Changes:
//    DPH 24/9/97

// ------------------------------------------------------------------
void Get_keyname_and_value (const char* line, string& Key_name, string& Key_value)
   {
   string Str_line = line;

   int Pos_equals = Str_line.find("=");
   if (Pos_equals > 0)
      {
      string Left_of_equals;
      Key_name = Str_line.substr (0, Pos_equals);
      Strip (Key_name, " ");
      To_lower(Key_name);
      Key_value = Str_line.substr (Pos_equals + 1);
      Strip (Key_value, " ");
      }
   else
      {
      Key_name = "";
      Key_value = "";
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    get a key value from a line. ie look for keyname = keyvalue
//    on the line passed in.  Returns keyvalue if found.  Blank otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997
//    dph 17/9/1997 changed "string& line" to "const char* line"

// ------------------------------------------------------------------
string Get_key_value (const char* line, const char* Key_name)
   {

   string Str_line = line;
   string Key_value;

   int Pos_equals = Str_line.find("=");
   if (Pos_equals > 0)
      {
      string Left_of_equals;
      Left_of_equals = Str_line.substr (0, Pos_equals);
      Strip (Left_of_equals, " ");
      To_lower(Left_of_equals);
      if (Left_of_equals == Key_name)
         {
         Key_value = Str_line.substr (Pos_equals + 1);
         Strip (Key_value, " ");
         }
      }
   return Key_value;
   }

// ------------------------------------------------------------------
//  Short description:
//    get a section name from a line. ie look for [section]
//    on the line passed in.  Returns name if found.  Blank otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997
//    dph 17/9/1997 changed "string& line" to "const char* line"

// ------------------------------------------------------------------
string Get_section_name (const char* line)
   {
   string Str_line = line;
   string Section_name;

   int Pos_open = Str_line.find("[");
   int Pos_close = Str_line.find("]");
   if (Pos_open >= 0 && Pos_close > 0)
      Section_name = Str_line.substr(Pos_open + 1, Pos_close - Pos_open - 1);
   To_lower (Section_name);
   return Section_name;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return true if string passed in is numerical.  False otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
bool Is_numerical (const char* Text)
   {
   char *endptr;
   strtod(Text, &endptr);
   return (*endptr == '\0');
   }

// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void To_lower (string& St)
   {
   char* buffer = new char[St.length() + 1];
   strcpy(buffer, St.c_str());
   strlwr(buffer);
   St = buffer;
   delete buffer;
   }

// ------------------------------------------------------------------
//  Short description:
//     function that takes a string and replaces all occurrances of
//     the substring with the replacement string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed NPOS to string::npos in line with standard.

// ------------------------------------------------------------------
void Replace_all (string& St, const char* Sub_string, const char* Replacement_string)
   {
   size_t Pos = St.find(Sub_string);
   while (Pos != string::npos)
      {
      St.replace(Pos, strlen(Sub_string), Replacement_string);
      Pos = St.find(Sub_string);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     convert a double to a string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed NPOS to string::npos in line with standard.

// ------------------------------------------------------------------
string ftoa(double Float, int Num_decplaces)
   {
   ostrstream buf;
   buf.setf(ios::fixed, ios::floatfield);
   buf << setprecision(Num_decplaces) << Float << ends;
   return buf.str();
   }

