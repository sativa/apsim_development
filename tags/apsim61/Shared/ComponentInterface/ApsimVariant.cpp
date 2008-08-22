//---------------------------------------------------------------------------

#include <stdexcept>
#include "ApsimVariant.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

std::string EXPORT protocol::DDML(const ApsimVariant& value)
   {
   return ("<type kind=\"variant\"/>");
   }
