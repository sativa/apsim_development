#include <stdlib.h>
#include <string>
#include <stdexcept>

#include "FString.h"

std::string _export asString(const FString& st)
   {
   return std::string(st.f_str(), st.length());
   }
