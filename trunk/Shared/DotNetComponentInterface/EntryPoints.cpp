#include "StdAfx.h"

#using <mscorlib.dll>
#using <VBGeneral.dll>
#include <string>
#include <vector>
#include "Interfaces.h"
//#include "ComponentInterface.h"
#include "ApsimComponent.h"
#include "ScienceAPI.h"
using namespace ModelFramework;
using namespace std;   
using namespace System::Collections::Generic;
using namespace System;
using namespace System::Collections;
using namespace System::Reflection;

   
ref class Instances
	{
	public:
		static ApsimComponent^ createNewInstance(const char* dllFileName,
												const unsigned int* compID,
												const unsigned int* parentID,
												unsigned int* instanceNumber,
												const unsigned int* callbackArg,
												CallbackType callback) 
			{
			String^ dll = gcnew String(dllFileName);
			if (System::IO::File::Exists(dll))
				{
				Assembly^ assembly = Assembly::LoadFrom(gcnew String(dllFileName));
				array<Type^>^ Types = assembly->GetTypes();
				System::Collections::IEnumerator^ myEnum = Types->GetEnumerator();
				while (myEnum->MoveNext())
					{
					Type^ oType = safe_cast<Type^>(myEnum->Current);
					
					// Go look for the [Model] attribute on a class.
					array<Object^>^ myAttributes = oType->GetCustomAttributes(true);
					for (int i = 0; i != myAttributes->Length; i++)
						{
						if (myAttributes[i]->ToString() == "Model")
							{
							ScienceAPI^ scienceAPI = gcnew ScienceAPI();

							array<Object^>^ Params = {scienceAPI};
							Object^ o = Activator::CreateInstance(oType, Params);
							
							ApsimComponent^ Component = gcnew ApsimComponent(o);
							Component->createInstance(dllFileName, *compID, *parentID, callbackArg, callback);
							components->Add(Component);

							scienceAPI->SetComponentInstance(Component);
							return Component;
							}
						}
					}
				}
			else
				{
				String^ msg = gcnew String("Cannot find dll: " + dll);
				System::Windows::Forms::MessageBox::Show(msg, "Error", System::Windows::Forms::MessageBoxButtons::OK);
				}

			return nullptr;
			}
		static ApsimComponent^ at(unsigned index)
			{
			return components[index];
			}
		
		static unsigned count() {return components->Count;}
	
		static List<ApsimComponent^>^ components = gcnew List<ApsimComponent^>();
		
	};

// ------------------------------------------------------------------
// Simple stub to be linked with fortran dlls. This will cause
// Computation::loadComponent(computation.cpp) to firstly load
// the fortran wrapper, followed by the fortran after.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport ) void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy_s(wrapperDll, 2, "");
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
    CallbackType callback)
    {
	ApsimComponent^ c = Instances::createNewInstance(dllFileName, compID, parentID, instanceNumber, callbackArg, callback);
	*instanceNumber = Instances::count()-1;
	}
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall deleteInstance (unsigned* instanceNumber)
   {
   ApsimComponent^ c = Instances::at(*instanceNumber);
   c->deleteInstance();
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" __declspec( dllexport )
void __stdcall messageToLogic (unsigned* instanceNumber, char* message, bool* processed)
   {
   ApsimComponent^ c = Instances::at(*instanceNumber);
   c->messageToLogic(message);
   *processed = true;
   }


