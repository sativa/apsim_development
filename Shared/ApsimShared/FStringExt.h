//---------------------------------------------------------------------------
#ifndef FStringExtH
#define FStringExtH

#include "FString.h"
#include <string>

std::string asString(const FString& st)
   {
   return std::string(st.f_str(), st.length());
   }

#endif
