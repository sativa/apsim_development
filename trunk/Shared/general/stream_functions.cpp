#include <general\stream_functions.h>

// ------------------------------------------------------------------
//  Short description:
//    Read in a token.  Skip all leading skip characters and stop when
//    a delimiter character is found.  Returns the character that caused
//    the parsing to stop.

//  Notes:

//  Changes:
//    DPH 21/11/94
//    DPH 17/4/1997 - moved to C++ builder.

// ------------------------------------------------------------------
char Read_token(istream& In_stream,
                const char* Init_skip_chars,
                const char* Delimiter_chars,
                string& Token)
   {
   // Clear the strings contents
   Token = "";

   // Skip all initial characters.
   bool Still_skipping_init = true;
   char Ch[2];
   while (!In_stream.eof() && Still_skipping_init)
      {
      // get next character from stream.
      In_stream.get((char*) &Ch, 2, 0);

      // only continue skipping characters if the char. just extracted
      // is in the Init_skip_chars string.
      if (strlen(Ch) == 0)
         Still_skipping_init = false;

      else if (strchr(Init_skip_chars, Ch[0]) != NULL)
         Still_skipping_init = true;

      else
         Still_skipping_init = false;
      }

   // Append characters to string until a delimiter is found.
   while (strlen(Ch) != 0 &&
          strchr(Delimiter_chars, Ch[0]) == NULL &&
          In_stream)
      {
      Token += Ch[0];
      In_stream.get((char*) &Ch, 2, 0);
      }

   // If reached end of stream then return 0;

   if (!In_stream)
      return 0;

   return Ch[0];
   }


