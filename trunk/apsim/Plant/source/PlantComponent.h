//---------------------------------------------------------------------------
#ifndef PlantComponentH
#define PlantComponentH

class Plant;

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Plant model and an APSIM simulation.
// ------------------------------------------------------------------
class PlantComponent : public protocol::Component
   {
   public:
      PlantComponent(void);
      ~PlantComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);

      std::string getProperty(const std::string &a, const std::string& b) const 
         {
         return componentData->getProperty(a,b);
         }
      //void error(const string &, bool);
   private:
      Plant     *plant;    // The plant module
   };
#endif
