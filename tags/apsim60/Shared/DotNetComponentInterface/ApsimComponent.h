#pragma once
#using <System.dll>
#using <System.Xml.dll>
#include <vector>
#include "datatypes.h"
#include "ScienceAPI.h"

using namespace System::Reflection;
using namespace System::Xml;
typedef  void  (__stdcall *CallbackType)(const unsigned *callbackArg, char *message);

namespace ModelFramework {
public ref class ApsimComponent
	{
	private:
		Object^ Model;
		int ComponentI;
		System::Text::StringBuilder^ Contents;	
		ScienceAPI^ api;
		bool Init1;
		
		void RegisterAllProperties();
		void RegisterAllEventHandlers();
				
	internal:
	    String^ Name();
	    String^ Read(String^ Name, bool Optional);
		void Get(String^ Name, String^ Units, bool Optional, UnmanagedData& Data);
		void Set(String^ Name, String^ Units, UnmanagedData& Data);
		void Publish(String^ Name, UnmanagedData& Data);
		void CallMethodOfModel(MethodInfo^ Member, ApsimType^ Data);
		void error(String^ Message, bool IsFatal);
		void Warning(String^ msg);
		void Write(String^ msg);

	public:
		ApsimComponent(Object^ Model);

		// --------------------------------
		// Called by entry point.
		// --------------------------------
		void createInstance(const char* dllFileName,
							unsigned compID,
							unsigned parentID,
							const unsigned* callbackArg,
							CallbackType callback);
		
		// --------------------------------
		// Called by entry point.
		// --------------------------------
		void deleteInstance ();

		// --------------------------------
		// Called by entry point.
		// --------------------------------
		void messageToLogic (char* message);

	};	
	
	
	

// -----------------------------------
// Create an instance of the component
// -----------------------------------

ApsimComponent^ createInstanceOfComponent(const std::string& dllFileName);
	
};