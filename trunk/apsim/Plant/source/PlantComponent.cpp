//---------------------------------------------------------------------------
#include <general/pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ComponentInterface/Type.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <math>
#include <map>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "Plant.h"

using namespace std;

// ------------------------------------------------------------------
// DLL entry point
// ------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE /*hinst*/, unsigned long /*reason*/, void*)
   {
   return 1;
   }

// ------------------------------------------------------------------
// Create an instance of the Plant module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new PlantComponent;
   }
// ------------------------------------------------------------------
// Initialises the Plant component.
// ------------------------------------------------------------------
PlantComponent::PlantComponent()
   {
   plant = new Plant(this);
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PlantComponent::~PlantComponent(void)
   {
   delete plant;
   }
// ------------------------------------------------------------------
// Stage 1 initialisation.
// ------------------------------------------------------------------
void PlantComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);
   plant->doRegistrations();
   }

// ------------------------------------------------------------------
// Stage 2 initialisation.
// ------------------------------------------------------------------
void PlantComponent::doInit2(void)
   {
   protocol::Component::doInit2();
   plant->doInit();
   }

// ------------------------------------------------------------------
// Look for a messages we have an interest in.
// ------------------------------------------------------------------
void PlantComponent::respondToEvent(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant& variant)
   {
   plant->doEvent(eventID, variant);
   }

void PlantComponent::respondToMethod(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant& variant)
   {
   plant->doEvent(eventID, variant);
   }

void PlantComponent::onApsimGetQuery(protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   plant->onApsimGetQuery(apsimGetQueryData);
   }

// ------------------------------------------------------------------
// Return a variable to caller.
// ------------------------------------------------------------------
void PlantComponent::respondToGet(unsigned int& /*fromID*/, protocol::QueryValueData& queryData)
   {
   baseInfo *v = vMap[queryData.ID];
   if (v) v->sendVariable(this, queryData);
   }

// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool PlantComponent::respondToSet(unsigned int& /*fromID*/, protocol::QuerySetValueData& setValueData)
   {
   return (plant->setVariable(setValueData.ID, setValueData));
   }

void PlantComponent::addGettableVar(const char *systemName,
                                    protocol::DataTypeCode type,
                                    int length,
                                    boost::function2<void, protocol::Component *, protocol::QueryValueData &> ptr,
                                    const char *units,
                                    const char *desc)
   {
      unsigned int id = getReg(systemName, type, length>1, units); 
      
      // Add to variable map
      fnInfo *v = new fnInfo(systemName, type, length, ptr, units, desc);
      vMap.insert(UInt2InfoMap::value_type(id,v));
   }

void PlantComponent::addGettableVar(const char *systemName,
                                    protocol::DataTypeCode type,
                                    int length,
                                    void *ptr,
                                    const char *units,
                                    const char *desc)
   {
      unsigned int id = getReg(systemName, type, length>1, units); 
      
      // Add to variable map
      varInfo *v = new varInfo(systemName, type, length, ptr, units, desc);
      vMap.insert(UInt2InfoMap::value_type(id,v));
   }

// Build the xml fragment that describes this variable and publish to system
unsigned int PlantComponent::getReg(const char *systemName,
                                    protocol::DataTypeCode type, 
                                    bool isArray, 
                                    const char *units)
   {
   char buffer[200];
   strcpy(buffer, "<type kind=\"");
   switch (type)
      {
   	case protocol::DTint4:   {strcat(buffer, "integer4"); break;}
   	case protocol::DTsingle: {strcat(buffer, "single"); break;}
   	case protocol::DTboolean:{strcat(buffer, "boolean"); break;}
   	case protocol::DTstring: {strcat(buffer, "string"); break;}
   	default: {throw "Undefined gettable var type";}
      }
   strcat(buffer, "\" array=\"");
   if  (isArray) {
       strcat(buffer, "T");
   } else { 
       strcat(buffer, "F");
   }
   strcat(buffer, "\" units=\"(");
   strcat(buffer, units);
   strcat(buffer, ")\"/>");
   return this->addRegistration(protocol::respondToGetReg, systemName, buffer);
   }

////////////////////
baseInfo::baseInfo() { 
	myLength = 0; 
	myType = protocol::DTunknown; 
}

void varInfo::sendVariable(protocol::Component *systemInterface, protocol::QueryValueData& qd) 
   {
   switch (myType)
      {
      case protocol::DTdouble:
         if (myLength == 1) 
           systemInterface->sendVariable(qd, *(double *)myPtr);
         else if (myLength > 1) 
           systemInterface->sendVariable(qd,         
             protocol::vector<double>((double *)myPtr, (double *)myPtr + myLength));
         else
           throw "Length = 0 in varInfo::sendVariable";  
         break;
      case protocol::DTsingle:
         if (myLength == 1) 
           systemInterface->sendVariable(qd, *(float *)myPtr);
         else if (myLength > 1) 
           systemInterface->sendVariable(qd,         
             protocol::vector<float>((float *)myPtr, (float *)myPtr + myLength));
         else
           throw "Length = 0 in varInfo::sendVariable";  
         break;
      case protocol::DTint4 :
         if (myLength == 1) 
           systemInterface->sendVariable(qd, *(int *)myPtr);
         else if (myLength > 1) 
           systemInterface->sendVariable(qd,         
             protocol::vector<int>((int *)myPtr, (int *)myPtr + myLength));
         else
           throw "Length = 0 in varInfo::sendVariable";  
         break;
      case protocol::DTstring :
         if (myLength == 1) 
           systemInterface->sendVariable(qd, FString(((string *)myPtr)->c_str()));
         else if (myLength > 1) 
           throw  "String Array not yet implemented";
         else
           throw "Length = 0 in varInfo::sendVariable";  
         break;

      default:
         throw "Aiee unknown type in varInfo::sendVariable";
      }
   }
