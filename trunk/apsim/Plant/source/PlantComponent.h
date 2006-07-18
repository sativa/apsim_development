#ifndef PlantComponentH
#define PlantComponentH

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Plant model and an APSIM simulation.
// ------------------------------------------------------------------
class IPlant;
class PlantComponent : public protocol::Component
   {
   private:
      IPlant     *plant;                     // The plant model

   public:
      PlantComponent(void);
      ~PlantComponent(void);
      void doInit1(const FString& sdml);
      void doInit2(void);
      bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      void onApsimGetQuery(protocol::ApsimGetQueryData& apsimGetQueryData);

      void writeString (const char *msg);
      void warningError (const char *msg);
   };
#endif
