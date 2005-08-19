//---------------------------------------------------------------------------
#ifndef PlantComponentH
#define PlantComponentH

#include "PlantInterface.h"

class Plant;

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Plant model and an APSIM simulation.
// ------------------------------------------------------------------
class PlantComponent : public protocol::Component , public commsInterface
   {
   private:
      Plant     *plant;    // The plant module

   public:
      PlantComponent(void);
      ~PlantComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      virtual void onApsimGetQuery(struct protocol::ApsimGetQueryData& apsimGetQueryData);

      void writeString (const char *msg);
      void warningError (const char *msg);

   };
#endif
