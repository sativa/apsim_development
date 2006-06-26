#ifndef BaseTypesH
#define BaseTypesH
#include <aps\fstring.h>
typedef void* HANDLE;

class IComponent;
class IComputation;
class ISimulation;
class PROTOCOLTransportAddress
   {
   public:
      PROTOCOLTransportAddress(void)
         : address(NULL) { }
      PROTOCOLTransportAddress(IComponent* anAddress)
         : address(anAddress) { }
      IComponent* address;
   };

class PROTOCOLDLLInfo
   {
   public:
      PROTOCOLDLLInfo(void) { };
      ~PROTOCOLDLLInfo(void) { };

      std::string fileName;
      HANDLE handle;
      void _stdcall (*CREATEProc)  (const char* aName, const int* anInstanceNo, const IComputation** aComputation, const char* ssdl, long aNameLength, long ssdlLength);
      void _stdcall (*INITProc)  (const int* anInstanceNo);
      void _stdcall (*TERMProc)  (const int* anInstanceNo);
      void _stdcall (*ACTIONProc)(const int* anInstanceNo, const char* anAction, int* aDnbytes, void* apData, int* aTnbytes, void* apTypDsc, long anActionLength, long DataLength);
      void _stdcall (*INEVENTProc)(const int* anInstanceNo, const char* anEvent, int* aDnbytes, void* apData, int* aTnbytes, void* apTypDsc, long anEventLength);
      int instanceNo;

   };

#endif
