#ifndef STRING_FUNCTIONS_H
#define STRING_FUNCTIONS_H

#include <general\mystring.h>
#include <general\mylist.h>
#include <algorith>
#include <functional>

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
//    dph 15/5/98 fixed bug to remove last occurrence of separator.

// ------------------------------------------------------------------
template <class container>
void Build_string (container& words, const char* separators, string& text)
   {
   text = "";
   for (container ::iterator Iter = words.begin();
                               Iter != words.end();
                               Iter++)
      {
      if (Iter != words.begin())
         text += separators;
      text += *Iter;
      }
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
//    converts a string to uppercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void To_upper (string& St);

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

#ifdef __WIN32__
// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for removing a substring from all
//     strings in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Arg >
class remove_substring : private std::unary_function < Arg, void >
   {
   private:
      string Substring;
   public:
      remove_substring( const char* sub ) : Substring ( sub ) { }
      void operator(  ) ( Arg & x ) { x.remove(x.find(Substring)); }
    };

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for appending a substring to all
//     strings in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Arg >
class append_substring : private std::unary_function < Arg, void >
   {
   private:
      string Substring;
   public:
      append_substring( const char* sub ) : Substring ( sub ) { }
      void operator(  ) ( Arg & x ) { x.append(Substring)); }
    };

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for appending a substring to all
//     strings in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Arg >
class prepend_substring : private std::unary_function < Arg, void >
   {
   private:
      string Substring;
   public:
      prepend_substring( const char* sub ) : Substring ( sub ) { }
      void operator(  ) ( Arg & x ) { x = Substring + x; }
    };
#endif

// ------------------------------------------------------------------
//  Short description:
//     function that takes a string of numbers and returns a
//     stl container of doubles.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type>
void String_2_double_container (const char* Numbers,
                                container_type& container)
   {
   container.erase (container.begin(), container.end());
   list<string> string_container;
   string Number_string(Numbers);
   Split_string (Number_string, " ", string_container);
   for (list<string>::iterator Iter = string_container.begin();
                               Iter != string_container.end();
                               Iter++)
      {
      container.push_back (atof ( (*Iter).c_str() ));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     function that takes a container of numbers and converts to a string.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type>
void Double_container_2_string (container_type& container,
                                string& Numbers,
                                int precision)
   {
   ostrstream number_stream;
   number_stream.setf(ios::fixed, ios::floatfield);
   number_stream.precision (precision);

   list<string> string_container;
   for (container_type::iterator Iter = container.begin();
                                 Iter != container.end();
                                 Iter++)
      number_stream << *Iter << ' ';

   number_stream << ends;
   Numbers = number_stream.str();
   delete number_stream.str();
   }

// ------------------------------------------------------------------
//  Short description:
//     function that takes a string and replaces all occurrances of
//     the substring with the replacement string.

//  Notes:

//  Changes:
//    DPH 17/3/98

// ------------------------------------------------------------------
void Replace_all (string& St, const char* Sub_string, const char* Replacement_string);

// ------------------------------------------------------------------
//  Short description:
//     convert a double to a string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed NPOS to string::npos in line with standard.

// ------------------------------------------------------------------
string ftoa(double Float, int Num_decplaces);

#endif

