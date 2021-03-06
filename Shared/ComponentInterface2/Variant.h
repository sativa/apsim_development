#ifndef VariantH
#define VariantH

#include <general/platform.h>
#include <string>
#include <vector>

class Variant 
      {
      public:
        EXPORT STDCALL Variant();
        EXPORT STDCALL ~Variant();
        char * bufStart;
        unsigned int bufLen;
      };

// ------------------------------------------------------------------
//  Short description:
//     Variant class for handling unspecified data types. These
//     structures are created at run time.
// ------------------------------------------------------------------
bool EXPORT STDCALL get(Variant&, const std::string& name, std::string &value);
bool EXPORT STDCALL get(Variant&, const std::string& name, float &value);

void EXPORT STDCALL pack(Variant&, const std::string& name, const std::string &value);
void EXPORT STDCALL pack(Variant&, const std::string& name, std::vector<float> &value);
void EXPORT STDCALL pack(Variant&, const std::string& name, std::vector<std::string> &value);

#endif
