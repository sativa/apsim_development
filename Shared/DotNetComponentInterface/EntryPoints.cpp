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
			else if (numComponents == 1)
				c1 = Component;
			else if (numComponents == 2)
				c2 = Component;
			else if (numComponents == 3)
				c3 = Component;
			else if (numComponents == 4)
				c4 = Component;
			else if (numComponents == 5)
				c5 = Component;
			else if (numComponents == 6)
				c6 = Component;
			else if (numComponents == 7)
				c7 = Component;
			else if (numComponents == 8)
				c8 = Component;
			else if (numComponents == 9)
				c9 = Component;
			else
				c10 = Component;
			numComponents++;
			return Component;
			}
		static ApsimComponent^ at(unsigned index)
			{
			if (index == 0)
				return c0;
			else if (index == 1)
				return c1;
			else if (index == 2)
				return c2;
			else if (index == 3)
				return c3;
			else if (index == 4)
				return c4;
			else if (index == 5)
				return c5;
			else if (index == 6)
				return c6;
			else if (index == 7)
				return c7;
			else if (index == 8)
				return c8;
			else if (index == 9)
				return c9;
			else
				return c10;
			}
		
		static unsigned count() {return numComponents;}
	
	//private:
		static ApsimComponent^ c0;
		static ApsimComponent^ c1;
		static ApsimComponent^ c2;
		static ApsimComponent^ c3;
		static ApsimComponent^ c4;
		static ApsimComponent^ c5;
		static ApsimComponent^ c6;
		static ApsimComponent^ c7;
		static ApsimComponent^ c8;
		static ApsimComponent^ c9;
		static ApsimComponent^ c10;
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


