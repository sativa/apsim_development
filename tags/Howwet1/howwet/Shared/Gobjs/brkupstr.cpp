#include <consts.h>

#include <gobjs\brkupstr.h>

// *******************************************************************
       void Break_up_string(const GString& Str,
                                   String_array& Return_array)  {
// *******************************************************************

//  Short description:
//    Break up a string into an array.

//  Notes:

//  Changes:
//    DPH 3/1/95

//  Calls:

//  Internal variables
      GString Our_line = Str;          // Copy of string from caller
      GString Word;                    // Word from line

// -------------------- Executable code section ----------------------

   Our_line += " ";

   Return_array.Flush();
   istrstream In_stream ((char*) Our_line.c_str());

   In_stream >> Word;
   while (Word.length() > 0)
      {
      Return_array.Add(Word);

      In_stream >> Word;
      }
   }

void Split_string (const string& text, const char* separators, String_array& words)
   {
   words.Flush();

	int n = text.length();
   int start, stop;

   start = text.find_first_not_of(separators);

   while ((start >= 0) && (start < n))
      {
		stop = text.find_first_of(separators, start);
		if ((stop < 0) || (stop > n)) stop = n;
		words.Add(text.substr(start, stop - start));
		start = text.find_first_not_of(separators, stop+1);
		}
   }
