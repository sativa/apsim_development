//---------------------------------------------------------------------------
#ifndef FStringH
#define FStringH

#include <values.h>
#include <windows.h>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a case insensitive FORTRAN string.

//  Notes:

//  Changes:
//    DPH 10/10/2000

// ------------------------------------------------------------------
class FString
   {
   public:
      // default constructor that doesn't alias to anything.
      FString(void)
         {
         aliasTo((const char*) NULL, 0);
         }
      // constructor for an alias to a char*
      FString(char* t)
         {
         aliasTo(t, strlen(t));
         }
      // constructor for an alias to a char*
      FString(const char* t)
         {
         aliasTo(t, strlen(t));
         }
      // constructor for an alias to a FORTRAN string
      FString(char* t, const unsigned int tLength)
         {
         aliasTo(t, tLength);
         calcRealLength();
         }
      // constructor for an alias to a FORTRAN string
      FString(const char* t, const unsigned int tLength)
         {
         aliasTo((char*)t, tLength);
         calcRealLength();
         }
      // alias to the specified string
      void aliasTo(char* t, const unsigned int tLength)
         {
         text = t;
         len = tLength;
         realLen = len;
         canModify = true;
         }
      // alias to the specified string
      void aliasTo(const char* t, const unsigned int tLength)
         {
         text = (char*)t;
         len = tLength;
         realLen = len;
         canModify = false;
         }

      const char* f_str(void) const
         {
         return text;
         }
      bool operator== (const FString& rhs) const
         {return (length() == rhs.length() &&
                  strncmpi(f_str(), rhs.f_str(), length()) == 0);}
      bool operator!= (const FString& rhs) const
         {return !(*this == rhs);}
      char operator[] (unsigned index) {return text[index];}
      FString& operator= (const FString& rhs)
         {
         if (canModify)
            {
            unsigned int rhsLength = rhs.length();
            if (len < rhsLength)
               error("String truncation.  FORTRAN string not long enough\n"
                     "to hold the string:\n", rhs);
            else
               {
               memcpy(text, rhs.f_str(), rhsLength);
               memset(&text[rhsLength], ' ', len - rhsLength);
               realLen = rhsLength;
               }
            }
         else
            aliasTo(rhs.f_str(), rhs.length());
         return *this;
         }
      FString& operator+ (const FString& rhs)
         {
         if (canModify)
            {
            unsigned int rhsLength = rhs.length();
            if (len - length() < rhsLength)
               error("String truncation.  FORTRAN string not long enough\n"
                     "to hold the string:\n", rhs);
            else
               {
               memcpy(&text[length()], rhs.text, rhsLength);
               memset(&text[length()+rhsLength], ' ', len - rhsLength-length());
               }
            }
         else
            error("Cannot modify const string: ", rhs);

         return *this;
         }
      unsigned int length(void) const
         {
         return realLen;
         }
      unsigned int maxLength(void) const
         {
         return len;
         }
      FString substr(unsigned int pos, unsigned int nchar = MAXINT) const
         {
         if (nchar == MAXINT)
            nchar = length() - pos;
         else if (pos+nchar > len)
            error("Invalid index into string: ", *this);
         return FString(&text[pos], nchar);
         }
      unsigned int find(const char* st, unsigned startPos = 0) const
         {
         unsigned stLength = strlen(st);
         for (unsigned int i = startPos; i < realLen; i++)
            {
            unsigned j = i;
            while (j-i < stLength && text[j] == st[j-i])
               j++;
            if (j-i == stLength)
               return i;
            }
         return MAXINT;
         }

   private:
      char* text;
      bool canModify;
      unsigned int len;
      unsigned int realLen;

      void calcRealLength(void)
         {
         for (realLen = len; realLen > 0 && text[realLen-1] == ' '; realLen--);
         }
      void error(const FString& msg1, const FString& msg2) const
         {
         char* buffer = new char[msg1.length() + msg2.length() + 1];
         strncpy(buffer, msg1.text, msg1.length());
         buffer[msg1.length()] = 0;
         strncat(buffer, msg2.text, msg2.length());
         buffer[msg1.length() + msg2.length()] = 0;
         MessageBox(NULL, buffer, "Internal error in FString", MB_ICONERROR | MB_OK);
         delete [] buffer;
         }

   };
//inline std::ostream& operator<< (std::ostream& out, const FString& st)
//   {
//   out.write(st.text, st.realLen);
//   return out;
//   }

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a list of FORTRAN strings.

//  Notes:

//  Changes:
//    DPH 10/10/2000

// ------------------------------------------------------------------
class FStrings
   {
   public:
      FStrings(char* t,
               const unsigned int elementlength,
               const unsigned int maxNumElement,
               unsigned int numElement)
         : maxNumElements(maxNumElement),
           numElements(numElement),
           elementLength(elementlength)
           {
           st.aliasTo(t, elementlength*maxNumElements);
           }

      template <class CT>
      FStrings& operator= (const CT& strings)
         {
         unsigned pos = 0;
         numElements = 0;
         if (strings.size() > maxNumElements)
            MessageBox(NULL, "Too many strings for FORTRAN string array", "", MB_OK);
         for (CT::const_iterator i = strings.begin();
                                 i != strings.end();
                                 i++)
            {
            FString rhs((*i).c_str());
            st.substr(pos, elementLength) = rhs;
            pos += elementLength;
            numElements++;
            }
         return *this;
         }

/*      template <class CT>
      void toC(CT& strings)
         {
         unsigned pos = 0;
         for (unsigned int i = 0; i < numElements; i++)
            {
            strings.push_back(st.substr(pos, elementLength).asString());
            pos += elementLength;
            }
         }
*/      FString getString(unsigned index) const
         {
         return st.substr(index * elementLength, elementLength);
         }
      void addString(FString& st)
         {
         getString(numElements) = st;
         numElements++;
         }

      unsigned getNumElements(void) const {return numElements;}
   private:
      unsigned maxNumElements;
      unsigned numElements;
      unsigned elementLength;
      FString st;
   };

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
