#if !defined (__gstring_h)
#define __gstring_h

#include <gobjs\gobjs.h>

#include <cstring.h>
#include <classlib\arrays.h>
#include <classlib\objstrm.h>
// *******************************************************************
		 class GOBJS_CLASS_EXPORT GString : public string, public TStreamableBase  {
// *******************************************************************

//  Short description:
//		Class derived from string to support new functionality.

//  Notes:

//  Changes:
//		DPH 16/11/94

// ------------------------- Private section -------------------------

	private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

	protected :

//   Protected data
//      none

//   Protected member functions
//      none

// -------------------------- Public section -------------------------

	public :

//   Public data
//      none

//   Public member functions
		void Replace(const GString& String1, const GString& string2);
													// Replace string 1 with string 2 and
													// return result.
		void Replace(const char *String1, const char *String2);
													// Replace string 1 with string 2 and
													// return result.
      GString::GString(string& St)
         {
         assign(St);
         };
      GString::GString(const char *Str)
         {
         assign(Str);
         set_case_sensitive(false);
         };                            // conversion from char array

      GString::GString(void)
         {
         set_case_sensitive(false);
         };                           // constructor

/*      GString& operator = (string& Str)
         {
         assign(Str);
         return *this;
         }                             // Set the path to the specified GString.
*/
      void Assign (const GString& Str, size_t Pos, size_t n);
                                       // Assign a portion of string Str

      char Read_token(istream& In_stream,
                      const GString& Leading_skip_chars,
                      const GString& Delimiter_chars);
                                       // Read in a token.  Skip all leading skip
                                       // characters and stop when a delimiter
                                       // character is found.  Returns the
                                       // character causing a parse stop.
      char Get_last_char(void);        // Return the last non blank character.

      float Get_real(void);            // convert the string to real
      int Get_integer(void);           // convert the string to integer
      void *Get_pointer(void);         // convert the string to a void pointer

      void Set_real(float Real_value); // Set the string up with a real value
      void Set_integer(int Int_value); // Set the string up with an integer
      void Set_pointer(void *Pointer); // Set the string up with a pointer.
      void Strip(StripType s = Trailing, char c = ' ');
                                       // Strip away unwanted chars from string.
      bool Is_numerical(void);         // returns TRUE if string is numerical
      DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GString, 1);
	};
typedef TArrayAsVector <GString> String_array;

ipstream& GOBJS_FUNCTION_EXPORT operator >> (ipstream& is, String_array& Obj);
opstream& GOBJS_FUNCTION_EXPORT operator << (opstream& os, const String_array& Obj);

#endif


