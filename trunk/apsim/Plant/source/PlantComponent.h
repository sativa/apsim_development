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
      virtual void respondToMethod(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      unsigned int addGettableVar(const char *systemName, protocol::DataTypeCode myType, bool isArray, const char *units);

      std::string readParameter(const string& sectionName,
                                const string& variableName)
          {
          std::string valueString = componentData->getProperty(sectionName, variableName);
          if (valueString.length() <= 0)
             {
             string baseSection = componentData->getProperty(sectionName, "derived_from");
             if (baseSection.length() > 0)
                {
                return readParameter(baseSection, variableName);
                }
             }
          return valueString;
          }
      std::string getProperty(const std::string &a, const std::string& b) const
         {
         return componentData->getProperty(a,b);
         }
   private:
      Plant     *plant;    // The plant module
   };
#endif
