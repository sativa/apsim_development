#ifndef FORTRANComponentWrapperH
#define FORTRANComponentWrapperH

#include <vector>;
#include "ScienceAPI.h"

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

static const char* nullType = "<type/>";
static const char* integerType = "<type kind=\"integer4\"/>";
static const char* integerArrayType = "<type kind=\"integer4\" array=\"T\"/>";
static const char* realType = "<type kind=\"single\"/>";
static const char* realArrayType = "<type kind=\"single\" array=\"T\"/>";
static const char* doubleType = "<type kind=\"double\"/>";
static const char* doubleArrayType = "<type kind=\"double\" array=\"T\"/>";
static const char* stringType = "<type kind=\"string\"/>";
static const char* stringArrayType = "<type kind=\"string\" array=\"T\"/>";
static const char* logicalType = "<type kind=\"boolean\"/>";

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

class FortranWrapper  
   {
   public:
      FortranWrapper(CMPComponentInterface *, ScienceAPI*, void *);
      ~FortranWrapper(void);

      int subscribe(const std::string &name, void *address);
      void subscribedEventHandler(void *address);

      static FortranWrapper* currentInstance;
      CMPComponentInterface* componentInterface;
      ScienceAPI*            scienceAPI;
   private:
      void *                 dllHandle;       // The fortran dll we are wrapping

      Instance *instance;    // Pointer into fortran dll (same for all instantiations via getProcAddress())
      Instance myInstance;   // Saved copy of one instance as retrieved from alloc_dealloc()
      
      void swapInstanceIn(void);
      void onInit1(void);
   };

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
