//---------------------------------------------------------------------------
#ifndef SiloInputComponentH
#define SiloInputComponentH
#include <InputComponent.h>
// ------------------------------------------------------------------
// Encapsulates the APSIM SILO INPUT module
// ------------------------------------------------------------------
class SiloInputComponent : public InputComponent
   {
   public:
      SiloInputComponent(void);
      ~SiloInputComponent(void);

   protected:
      std::string stationNumber;
      
      virtual void openInputFile(void);
      virtual void doInit2(void);

   };
#endif
