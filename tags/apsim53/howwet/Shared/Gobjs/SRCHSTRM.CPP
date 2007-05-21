#include <consts.h>

#include <gobjs\srchstrm.h>

#include <string.h>

// *******************************************************************
       bool Search_stream(istream& In_stream,
                                 const char *Search_chars)  {
// *******************************************************************

//  Short description:
//    Search the input stream for the specified search characters.  Return
//    TRUE if found.  FALSE otherwise.  The position of the input stream will
//    be set to the start of the match if found.  In_stream should be BINARY

//  Notes:

//  Changes:
//    DPH 24/2/95

//  Calls:

//  Internal variables
      #define BUFFER_SIZE 16384        // Size of buffer.
      char *Buffer;                    // Buffer to hold characters.
      bool Found = false;              // Found string ok?
      long Max_compares;               // Maximum number of compares per buffer.
      int Num_compares_so_far = 0;     // Number of comparisons so far
      long Num_chars;                  // Number of characters
      char *Index;                     // index into buffer

// -------------------- Executable code section ----------------------

   Buffer = new char[BUFFER_SIZE];

   if (Buffer != NULL)
      {
      while (!Found && In_stream)
         {
         In_stream.read(Buffer, BUFFER_SIZE);
         Num_chars = In_stream.gcount();

         Index = Buffer;
         Max_compares = Num_chars - strlen(Search_chars) + 1;
         Num_compares_so_far = 0;
         while (!Found && Num_compares_so_far < Max_compares)
            {
            // Go see if we have a match for this position.

            Found = (strncmpi(Search_chars, Index, strlen(Search_chars)) == 0);

            Num_compares_so_far++;
            Index++;
            }
         // If not found then reposition in_stream ready for next buffer

         if (!Found && Num_chars == BUFFER_SIZE)
            {
            unsigned long Stream_pos = In_stream.tellg();
            Stream_pos -= strlen(Search_chars) - 1;
            In_stream.seekg(Stream_pos);
            In_stream.clear();
            }
         }

      // If we have a match then go reposition in_stream at start of match.

      if (Found)
         {
         Num_compares_so_far--;
         In_stream.seekg(-(Num_chars - Num_compares_so_far), ios::cur);
         In_stream.clear();
         }

      delete [] Buffer;
      }
   else
      Found = false;

   return Found;
   }
