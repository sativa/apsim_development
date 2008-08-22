#ifndef FortranWrapperH
#define FortranWrapperH

#include <vector>
#include <string>
#include <general/platform.h>

// Declarations from the FORTRAN side.
struct Instance
   {
   const char* id;
   unsigned int idSize;
   const char* g;
   unsigned int gSize;
   const char* p;
   unsigned int pSize;
   const char* c;
   unsigned int cSize;
   unsigned int dummy1;
   unsigned int dummy2;
   unsigned int dummy3;
   unsigned int dummy4;
   unsigned int dummy5;
   unsigned int dummy6;
   unsigned int dummy7;
   unsigned int dummy8;
   unsigned int dummy9;
   unsigned int dummy10;
   };
   
class ScienceAPI;
class CMPComponentInterface;

class EXPORT FortranWrapper
   {
   //---------------------------------------------------------------------------
   // Wrapper class for fortran routines.
   // Keeps pointers to fortran entry points: Main(), do_init1() etc..
   // and calls them when reqd.
   //---------------------------------------------------------------------------
   public:
      FortranWrapper(ScienceAPI*, CMPComponentInterface* componentInterface, void *);
      ~FortranWrapper(void);

      int subscribe(const std::string &name, void *address);
      void subscribedEventHandler(void *address);

      static FortranWrapper* currentInstance;
      ScienceAPI*            scienceAPI;
      CMPComponentInterface* componentInterface;
   private:
      void *                 dllHandle;       // The fortran dll we are wrapping

      Instance *instance;    // Pointer into fortran dll (same for all instantiations via getProcAddress())
      Instance myInstance;   // Saved copy of one instance as retrieved from alloc_dealloc()

      void swapInstanceIn(void);
      void onInit1(void);
   };

void ToFortran(const std::string& cValue,
               char* forValue, unsigned forValueLength);

void ToFortran(const std::vector<std::string>& cValue,
               char* forValue, unsigned forValueLength, int arraySize, int& numValues);

#endif
