#ifndef STRING_FUNCTIONS_H
#define STRING_FUNCTIONS_H

#include <general\general.h>
#include <string>
#include <list>
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
void Split_string (const std::string& text, const char* separators, container& words)
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
//     This method splits values on a comma but also honours double quotes
//     ensuring something in double quotes is never split.

//  Notes:
//     eg: if text = value1, "value 2, 2a", value3
//     then: words[0] = value1
//           words[1] = value2, 2a
//           words[2] = value3

//  Changes:
//    DPH 21/7/2000

// ------------------------------------------------------------------
template <class container>
void SplitStringHonouringQuotes(const std::string& text,
                                const char* separators,
                                container& words) throw(std::string)
   {
   words.erase(words.begin(), words.end());

   std::string separatorsAndQuote = separators + std::string("\"");
   std::string separatorsAndSpace = separators + std::string(" ");
	unsigned int n = text.length();
   unsigned int start, stop;

   start = text.find_first_not_of(separatorsAndSpace);

   while (start < n)
      {
      if (text[start] == '\"')
         {
         stop = text.find("\"", start+1);
         if (stop == std::string::npos)
            throw std::string("Mismatched quotes in string: " + text);
         stop++;
         }
      else
         stop = text.find_first_of(separatorsAndQuote, start);
		if (stop > n) stop = n;
		words.push_back(text.substr(start, stop - start));
		start = text.find_first_not_of(separatorsAndSpace, stop+1);
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
void Build_string (container& words, const char* separators, std::string& text)
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
std::string GENERAL_EXPORT Get_section_name (const char* line);

// ------------------------------------------------------------------
//  Short description:
//    get a key value from a line. ie look for keyname = keyvalue
//    on the line passed in.  Returns keyvalue if found.  Blank otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997
//    dph 17/9/1997 changed "string& line" to "const char* line"

// ------------------------------------------------------------------
std::string GENERAL_EXPORT Get_key_value (const char* line, const char* Key_name);

// ------------------------------------------------------------------
//  Short description:
//    return the key name and value on the line.

//  Notes:

//  Changes:
//    DPH 24/9/97

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_keyname_and_value (const char* line, std::string& Key_name, std::string& Key_value);

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Strip (char* text, const char* separators);

// ------------------------------------------------------------------
//  Short description:
//    removes leading and trailing characters.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Strip (std::string& text, const char* separators);

// ------------------------------------------------------------------
//  Short description:
//    Return true if string passed in is numerical.  False otherwise.

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
bool GENERAL_EXPORT Is_numerical (const char* Text);

// ------------------------------------------------------------------
//  Short description:
//    converts a string to uppercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT To_upper (std::string& St);

// ------------------------------------------------------------------
//  Short description:
//    converts a string to lowercase

//  Notes:

//  Changes:
//    DPH 29/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT To_lower (std::string& St);

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
template <class Container >
class remove_substring_and_copy : private std::unary_function < std::string, void >
   {
   private:
      Container& container;
      std::string Substring;
   public:
      remove_substring_and_copy( const char* sub, Container& c )
         : Substring ( sub ), container(c) { }
      void operator(  ) ( const std::string& x )
         {
         std::string new_st(x);
         size_t pos = new_st.find(Substring);
         if (pos != std::string::npos)
            new_st.erase(pos);
         container.push_back (new_st);
         }
    };

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for appending a substring to all
//     strings in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Container >
class append_substring_and_copy : private std::unary_function < std::string, void >
   {
   private:
      Container& container;
      std::string Substring;
   public:
      append_substring_and_copy( const char* sub, Container& c )
         : Substring (sub), container(c) { }
      void operator(  ) ( const std::string& x )
         {
         std::string new_st(x);
         new_st.append(Substring);
         container.push_back (new_st);
         }
    };

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for appending a substring to all
//     strings in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Container >
class prepend_substring_and_copy : private std::unary_function <std::string, void >
   {
   private:
      Container& container;
      std::string Substring;
   public:
      prepend_substring_and_copy( const char* sub, Container& c )
         : Substring (sub), container(c) { }
      void operator() (const std::string& x)
         {
         container.push_back (Substring + x);
         }
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
   std::list<std::string> string_container;
   std::string Number_string(Numbers);
   Split_string (Number_string, " ", string_container);
   for (std::list<std::string>::iterator Iter = string_container.begin();
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
                                std::string& Numbers,
                                int precision)
   {
   ostrstream number_stream;
   number_stream.setf(ios::fixed, ios::floatfield);
   number_stream.precision (precision);

   std::list<std::string> string_container;
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
bool GENERAL_EXPORT Replace_all (std::string& St, const char* Sub_string, const char* Replacement_string);

// ------------------------------------------------------------------
//  Short description:
//     convert a double to a string.

//  Notes:

//  Changes:
//    DPH 17/3/97
//    dph 27/3/98 changed NPOS to string::npos in line with standard.

// ------------------------------------------------------------------
std::string GENERAL_EXPORT ftoa(double Float, int Num_decplaces);

// ------------------------------------------------------------------
//  Short description:
//     case insensitive string comparison routines.

//  Notes:

//  Changes:
//    SB ???

// ------------------------------------------------------------------
int GENERAL_EXPORT Str_i_Cmp(const std::string &a, const std::string &b);
#define Str_i_Eq(a,b)  (!Str_i_Cmp((a),(b)))

// ------------------------------------------------------------------
//  Short description:
//     replace all chars in a give string with a replacement.  Can
//     handle a NULL char as a replacement char.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
void Replace_all_chars (char* St, char Char_to_replace, char Replacement_char);

// ------------------------------------------------------------------
//  Short description:
//     Get all words from a double null terminated string where each
//     word is separated by a null.  Windows API routines sometimes
//     do things this way.

//  Notes:

//  Changes:
//    DPH 4/1/1999

// ------------------------------------------------------------------
void Get_words_from_double_null_term (char* St, std::list<std::string>& Words);

// ------------------------------------------------------------------
//  Short description:
//     Count and return the number of occurrances of a substring
//     in a text string.

//  Notes:

//  Changes:
//    NH 13/12/2000

// ------------------------------------------------------------------
int GENERAL_EXPORT NumOccurrences (std::string text, std::string substring);

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
std::string GENERAL_EXPORT getAttributeFromLine(const std::string& attributeName,
                                                const std::string& line);

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
void getAttributeNameAndValue(const std::string& line,
                              unsigned int posEquals,
                              std::string& name,
                              std::string& value);

// ------------------------------------------------------------------
//  Short description:
//     Return a list of attribute names and values from the specified line.

//  Notes:
//     A line may look like:
//        <property name="prop1" value="prop1value" type=""/>
//     Where the attributes are name, value and type.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
template <class CT>
void getAttributesFromLine(const std::string& line, CT& names, CT& values)
   {
   while (unsigned posEquals = line.find("=", posEquals) != std::string::npos)
      {
      std::string name;
      std::string value;
      getAttributeNameAndValue(line, posEquals, name, value);
      names.push_back(name);
      values.push_back(value);
      }
   }

#endif

