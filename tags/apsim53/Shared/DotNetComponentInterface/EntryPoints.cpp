#include "StdAfx.h"

#using <mscorlib.dll>
#using <VBGeneral.dll>
#include <string>
#include <vector>
#include "Interfaces.h"
#include "ComponentInterface.h"
#include "ApsimComponent.h"
using namespace ComponentInterface;
using namespace std;   
   
ref class Instances
	{
	public:
		static ApsimComponent^ createNewInstance(const char* dllFileName) 
			{
			ApsimComponent^ Component = createInstanceOfComponent(dllFileName);
			if (numComponents == 0)
				c0 = Component;
			else
				c1 = Component;
			numComponents++;
			return Component;
			}
		static ApsimComponent^ at(unsigned index)
			{
			if (index == 0)
				return c0;
			else
				return c1;
			}
		
		static unsigned count() {return numComponents;}
	
	//private:
		static ApsimComponent^ c0;
		static ApsimComponent^ c1;
		//static array<ApsimComponent^>^ components;
		static unsigned numComponents = 0;
			
	};
vector<ComponentComms*> componentInterfaces;
   
// ------------------------------------------------------------------
// Simple stub to be linked with fortran dlls. This will cause
// Computation::loadComponent(computation.cpp) to firstly load
// the fortran wrapper, followed by the fortran after.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport ) void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }


// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall getDescriptionInternal(char* initScript, char* description)
   {
   
   }
   
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C"  __declspec( dllexport )
void __stdcall createInstance
   (const char* dllFileName,
    const unsigned int* compID,
    const unsigned int* parentID,
    unsigned int* instanceNumber,
    const unsigned int* callbackArg,
    ComponentComms::CallbackType callback)
    {
	ApsimComponent^ c = Instances::createNewInstance(dllFileName);
    pin_ptr<ApsimComponent^> component = &c;
    *instanceNumber = Instances::count()-1;
	
	componentInterfaces.push_back(new ComponentComms);
    componentInterfaces[*instanceNumber]->createInstance(*component, dllFileName, *compID, *parentID, 
														 callbackArg, callback);
	}
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall deleteInstance (unsigned* instanceNumber)
   {
   ApsimComponent^ c = Instances::at(*instanceNumber);
   pin_ptr<ApsimComponent^> component = &c;
   componentInterfaces[*instanceNumber]->deleteInstance(*component);
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall messageToLogic (unsigned* instanceNumber, char* message, bool* processed)
   {
   ApsimComponent^ c = Instances::at(*instanceNumber);
   pin_ptr<ApsimComponent^> component = &c;
   componentInterfaces[*instanceNumber]->messageToLogic(*component, message);
   *processed = true;
   }


