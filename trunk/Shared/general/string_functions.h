#ifndef STRING_FUNCTIONS_H
#define STRING_FUNCTIONS_H

#include <general\mystring.h>

// ------------------------------------------------------------------
//  Short description:
//    splits a string into words.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class container>
void Split_string (string& text, const char* separators, container& words)
   {
   words.erase(words.begin(), words.end());

	int n = text.length();
   int start, stop;

   start = text.find_first_not_of(separators);

   while ((start >= 0) && (start < n))
      {
		stop = text.find_first_of(separators, start);
		if ((stop < 0) || (stop > n)) stop = n;
		words.push_back(text.substr(start, stop - start));
		start = text.find_first_not_of(separators, stop+1);
		}
   }

// ------------------------------------------------------------------
//  Short description:
//    builds up a string from a list of words.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class container>
void Build_string (container& words, const char* separators, string& text)
   {
   text = "";
   for (container ::iterator Iter = words.begin();
                               Iter != words.end();
                               Iter++)
      {
      text += *Iter;
      text += separators;
      }

   // remove last separator.
   int last = text.find_last_of(separators);
   text.remove(last);
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
string Get_section_name (const char* line);

// ------------------------------------------------------------------
//  Short description:
//    get a key value from a line. ie look for keyname = keyvalue
//    on the line passed in.  Returns keyvalue if found.  Blank otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997
//    dph 17/9/1997 changed "string& line" to "const char* line"

// ------------------------------------------------------------------
string Get_key_value (const char* line, const char* Key_name);

// ------------------------------------------------------------------
//  Short description:
//    return the key name and value on the line.

//  Notes:

//  Changes:
//    DPH 24/9/97

// ------------------------------------------------------------------
void Get_keyname_and_value (const char* line, string& Key_name, string& Key_value);

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Strip (char* text, const char* separators);

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Strip (string& text, const char* separators);

// ------------------------------------------------------------------
//  Short description:
//    Return true if string passed in is numerical.  False otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
bool Is_numerical (const char* Text);

// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void To_lower (string& St);

// ------------------------------------------------------------------
//  Short description:
//    converts all strings in container to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
template <class StringContainer>
void Strings_to_lower (StringContainer& words)
   {
   for (StringContainer ::iterator Iter = words.begin();
                                   Iter != words.end();
                                   Iter++)
      {
      To_lower (*Iter);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    locate a string in a container.  All comparisons are case
//    insensitive.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class StringContainer>
int Locate_string (const char* Search_string, StringContainer& words)
   {
   bool Found = false;
   int indx = -1;
   for (StringContainer ::iterator Iter = words.begin();
                                   Iter != words.end() && !Found;
                                   Iter++)
      {
      Found = (strcmpi(Search_string, (*Iter).c_str()) == 0);
      indx++;
      }
   if (!Found)
      indx = -1;
   return indx;
   }

#endif

