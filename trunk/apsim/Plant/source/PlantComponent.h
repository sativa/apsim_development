//---------------------------------------------------------------------------
#ifndef PlantComponentH
#define PlantComponentH

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl
#include <boost/function.hpp>
#include <ComponentInterface/Component.h>
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

      // Search a list of "sections" for a parameter.
      std::string searchParameter(vector<string> &sectionNames,
                                  const std::string& variableName)
         {
         string result;
         for (unsigned int i = 0; i < sectionNames.size(); i++)
           if ((result = readParameter(sectionNames[i], variableName)) != "")
              return result;
         return result;
         };
      template <class T>
      bool searchParameter(vector<string> &sections,
                           const string &variableName,
                           T &value,
                           double lower,
                           double upper,
                           bool optional=false)
         {
         for (unsigned int i = 0; i < sections.size(); i++) 
           if (readParameter(sections[i], variableName, value, lower, upper, true))
              return true;

         if (!optional) 
            {
            string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + variableName;
            error(msg.c_str(), true);
            }
         return false;
         };
      template <class T>
      bool searchParameter(vector<string> &sects, const string &name,
                           T *v, int &numvals, 
                           double lower, double upper, 
                           bool isOptional = false)
         {
         for (unsigned int i = 0; i < sects.size(); i++) 
           if (readParameter(sects[i], name,  v, numvals, lower, upper, true))
              return true;

         if (!isOptional) 
            {
            string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + name;
            error(msg.c_str(), true);
            }
         return false;
         };
      template <class T>
      bool searchParameter(vector<string> &sections,
                           const string &variableName,
                           vector <T> &values,
                           double lower,
                           double upper,
                           bool isOptional=false)
         {
         for (unsigned int i = 0; i < sections.size(); i++) 
           if (readParameter(sections[i], variableName, values, lower, upper, true))
              return true;

         if (!isOptional) 
            {
            string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + variableName;
            error(msg.c_str(), true);
            }
         return false;
         };

   };
#pragma warn .inl
#endif
