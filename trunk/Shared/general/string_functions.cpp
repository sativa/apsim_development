#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\string_functions.h>
#include <tchar.h>
#include <strstream>
#include <iomanip>
#include <sysutils.hpp>
#include "stristr.h"
using namespace std;
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
//    dph 6/7/2001 added code to remove TAB chars d423

// ------------------------------------------------------------------
void Get_keyname_and_value (const char* line, string& Key_name, string& Key_value)
   {
   string Str_line = line;

   int Pos_equals = Str_line.find("=");
   if (Pos_equals > 0)
      {
      string Left_of_equals;
      Key_name = Str_line.substr (0, Pos_equals);
      Replace_all(Key_name, "\t", "   ");
      Strip (Key_name, " ");
      Key_value = Str_line.substr (Pos_equals + 1);
      Replace_all(Key_value, "\t", "   ");
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
//    dph 5/6/98 modified to use str_i_eq routine instead of to_lower.
//    dph 6/7/2001 added code to remove TAB chars d423

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
      Replace_all(Left_of_equals, "\t", "   ");
      Strip (Left_of_equals, " ");
      if (Str_i_Eq(Left_of_equals, Key_name))
         {
         Key_value = Str_line.substr (Pos_equals + 1);
         Replace_all(Key_value, "\t", "   ");
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
//    dph 6/7/2001 added code to remove TAB chars d423

// ------------------------------------------------------------------
string Get_section_name (const char* line)
   {
   string Str_line = line;
   string Section_name;

   unsigned int Pos_first_non_blank = Str_line.find_first_not_of (" \t");
   if (Pos_first_non_blank != string::npos && Str_line[Pos_first_non_blank] == '[')
      {
      int Pos_open = Str_line.find("[");
      int Pos_close = Str_line.find("]");
      if (Pos_open >= 0 && Pos_close > 0)
         Section_name = Str_line.substr(Pos_open + 1, Pos_close - Pos_open - 1);
      To_lower (Section_name);
      }
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
   delete [] buffer;
   }

// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void To_upper (string& St)
   {
   char* buffer = new char[St.length() + 1];
   strcpy(buffer, St.c_str());
   strupr(buffer);
   St = buffer;
   delete buffer;
   }

// ------------------------------------------------------------------
//  Short description:
//     function that takes a string and replaces all occurrances of
//     the substring with the replacement string.  Return true if
//     a replacement was made.
//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed string::npos to string::string::npos in line with standard.

// ------------------------------------------------------------------
bool Replace_all (string& St, const char* Sub_string, const char* Replacement_string)
   {
   bool replacementMade = false;
   size_t Pos = St.find(Sub_string);
   while (Pos != string::npos)
      {
      St.replace(Pos, strlen(Sub_string), Replacement_string);
      replacementMade = true;
      Pos = St.find(Sub_string);
      }
   return replacementMade;
   }

// ------------------------------------------------------------------
//  Short description:
//     convert a double to a string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed npos to string::npos in line with standard.

// ------------------------------------------------------------------
string ftoa(double Float, int Num_decplaces)
   {
//   ostrstream buf;
//   buf.setf(std::ios::fixed, std::ios::floatfield);
//   buf << std::setprecision(Num_decplaces) << Float << std::ends;
//   return buf.str();
   return FloatToStrF(Float, ffFixed, 12, Num_decplaces).c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//     case insensitive string comparison routine.

//  Notes:

//  Changes:
//    SB ????

// ------------------------------------------------------------------
int Str_i_Cmp(const string &a, const string &b)
{	return stricmp(a.c_str(),b.c_str());
}


// ------------------------------------------------------------------
//  Short description:
//     replace all chars in a give string with a replacement.  Can
//     handle a NULL char as a replacement char.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Replace_all_chars (char* St, char Char_to_replace, char Replacement_char)
   {
   char* ptr = St;
   ptr = strchr(ptr, Char_to_replace);
   while (ptr != NULL)
      {
      *ptr = Replacement_char;
      ptr = strchr(ptr + 1, Char_to_replace);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Get all words from a double null terminated string where each
//     word is separated by a null.  Windows API routines sometimes
//     do things this way.

//  Notes:

//  Changes:
//    DPH 4/1/1999

// ------------------------------------------------------------------
void Get_words_from_double_null_term (char* St, list<string>& Words)
   {
   char* StartPtr = St;
   char* EndPtr = St;
   EndPtr = strchr(StartPtr, 0);
   while (StartPtr != EndPtr)
      {
      Words.push_back (StartPtr);
      StartPtr = ++EndPtr;
      EndPtr = strchr(StartPtr, 0);
      }
   }

//----------------------------------------------------------------------------
int NumOccurrences (string text, string substring)
//----------------------------------------------------------------------------
// Description:
//   Return the number of occurrences of a given substring with the given string
//
// Notes:
//
//  Changes:
//    NH 13/12/2000

   {
   int counter = 0;       // count of occurences
   unsigned int pos = 0;  // search position - start at the beginning


   while (text.find(substring,pos)!=text.npos)
      {
      counter++;
      pos =text.find(substring,pos)+1;
      };
   return counter;
   }

// ------------------------------------------------------------------
//  Short description:
//     Return an attribute of the type.

//  Notes:
//     A line may look like:
//        <property name="prop1" value="prop1value" type=""/>
//     Where the attributes are name, value and type.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
string getAttributeFromLine(const string& attributeName,
                            const string& line)
   {
   unsigned posEquals = 0;
   bool found = false;
   string value;

   while ((posEquals = line.find("=", posEquals)) != string::npos && !found)
      {
      string name;
      getAttributeNameAndValue(line, posEquals, name, value);

      // is the attribute name the one we're looking for?
      found = Str_i_Eq(name, attributeName);
      }
   if (!found)
      value = "";
   return value;
   }


// ------------------------------------------------------------------
//  Short description:
//     Given the position of an equals sign within a line, extract
//     the name and value from the left and right hand side of the
//     equals.

//  Notes:
//     A line may look like:
//        <property name="prop1" value="prop1value" type=""/>
//     Where the attributes are name, value and type.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void getAttributeNameAndValue(const string& line,
                              unsigned int posEquals,
                              string& name,
                              string& value)
   {
   // get the bit to the left of the equals sign.
   string leftOfEquals = line.substr(0, posEquals);
   Strip(leftOfEquals, " ");

   // get the last word on the left of the equals i.e. the attribute name
   unsigned lastSpace = leftOfEquals.find_last_of(" \t");
   if (lastSpace == string::npos)
      name = leftOfEquals;
   else
      name = leftOfEquals.substr(lastSpace+1);

   string rightOfEquals = line.substr(posEquals+1);

   // Need to get bit inside of quotes.
   unsigned int firstQuote = rightOfEquals.find("\"");
   unsigned int secondQuote;
   if (firstQuote != string::npos)
       secondQuote= rightOfEquals.find("\"", firstQuote+1);
   if (firstQuote != string::npos && secondQuote != string::npos)
      value = rightOfEquals.substr(firstQuote+1, secondQuote-firstQuote-1);
   else
      value = "";
   }


// ------------------------------------------------------------------
//  Short description:
//     Remove an attribute from the specified line.

//  Notes
//     A line may look like:
//        <property name="prop1" value="prop1value" type=""/>
//     Where the attributes are name, value and type.

//  Changes:
//    dph 16/8/2001
// ------------------------------------------------------------------
void removeAttributeFromLine(std::string& line, const std::string& attribute)
   {
   string stToFind = " " + attribute + "=\"";

   char* posAttribute = stristr((char*)line.c_str(), stToFind.c_str());
   if (posAttribute != NULL)
      {
      unsigned startPos = posAttribute - line.c_str();
      unsigned endPos = line.find("\"", startPos + stToFind.length());
      if (endPos != string::npos)
         line.erase(startPos, endPos-startPos+1);
      }
   }

