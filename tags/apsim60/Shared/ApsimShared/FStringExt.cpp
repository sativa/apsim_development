#include <stdlib.h>
#include <string>
#include <stdexcept>
#include "FString.h"
#include <general/platform.h>
#include "FStringExt.h"
std::string EXPORT STDCALL asString(const FString& st)
   {
   return std::string(st.f_str(), st.length());
   }
