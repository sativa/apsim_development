#include "stdafx.h"
#include "ApsimComponent.h"
#include "MessageData.h"
#include "ReflectionTemplates.h"
#using <System.dll>
using namespace System::Runtime::InteropServices;
using namespace ModelFramework;

extern "C" {
	[DllImport("ComponentInterface2.dll", EntryPoint = "CICreate", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CICreate(const unsigned* callbackarg, CallbackType callback, unsigned componentid, unsigned parentid);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIDelete", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CIDelete(int ComponentInterface);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMessageToLogic", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CIMessageToLogic(int ComponentInterface, char* Message);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CICreateData", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CICreateData(int ComponentInterface, int DataIndex);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CISubscribe", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CISubscribe(int ComponentInterface, String^ Name, void* Handler, UnmanagedData* Data, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIError", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIError(int ComponentInterface, String^ Message, bool IsFatal);
	
	[DllImport("ComponentInterface2.dll", EntryPoint = "CIRead", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	int CIRead(int ComponentInterface, String^ Name, bool Optional, StringBuilder^ Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIGet", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIGet(int ComponentInterface, String^ Name, String^ Units, bool Optional, void* Handler, 
			   UnmanagedData* Data, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CISet", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CISet(int ComponentInterface, String^ Name, String^ Units, void* Handler, 
			   UnmanagedData* Data, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIExpose", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIExpose(int ComponentInterface, String^ Name, String^ Units, String^ Description, bool Writable, void* Handler, 
			      UnmanagedData* Data, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPublish", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIPublish(int ComponentInterface, String^ Name, void* Handler, 
			       UnmanagedData* Data, String^ ddml);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIWrite", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIWrite(int ComponentInterface, String^ Line);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIWarning", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIWarning(int ComponentInterface, String^ Line);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIName", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void CIName(int ComponentInterface, StringBuilder^ Name);
}

extern "C" __declspec( dllexport )
unsigned __stdcall EventHandler(UnmanagedData* data, char* messageData, int OpCode)
	{
	try
		{
		if (OpCode == 1)
			data->pack(messageData);
		else if (OpCode == 2)
			data->unpack(messageData);
		else if (OpCode == 3)
			{
			return data->memorySize();
			}
		}
	catch (System::Exception^ err)
		{
		ApsimComponent^ Comp = (ApsimComponent^) data->GetComponentInstance();
		Comp->error(err->GetBaseException()->Message, true);
		}
	return 0;
	}

ApsimComponent::ApsimComponent(Object^ model)
	: Model(model)
	{
	// --------------------------------
	// constructor
	// --------------------------------
	}
void ApsimComponent::createInstance(const char* dllFileName,
					unsigned compID,
					unsigned parentID,
					const unsigned* callbackArg,
					CallbackType callback)
	{
	// --------------------------------
	// Called by entry point.
	// --------------------------------
	Contents = gcnew StringBuilder(10000);
	
	ComponentI = CICreate(callbackArg, callback, compID, parentID);
	}

// --------------------------------
// Called by entry point.
// --------------------------------
void ApsimComponent::deleteInstance()
	{
	CIDelete(ComponentI);
	}

// --------------------------------
// Called by entry point.
// --------------------------------
void ApsimComponent::messageToLogic (char* message)
	{
	static bool Init1 = true;
	if (Init1)
		{
		Init1 = false;
		RegisterAllFields<FieldInfo>(this, Model, ComponentI);
		RegisterAllFields<PropertyInfo>(this, Model, ComponentI);
		RegisterAllEventHandlers();
		}
	CIMessageToLogic(ComponentI, message);
	}

void ApsimComponent::error(String^ Message, bool IsFatal)
	{
	CIError(ComponentI, Message, IsFatal);
	}

String^ ApsimComponent::Name()
	{
	CIName(ComponentI, Contents);
	return Contents->ToString();
	}	

String^ ApsimComponent::Read(String^ Name, bool Optional)
	{
	CIRead(ComponentI, Name, Optional, Contents);
	return Contents->ToString();
	}
void ApsimComponent::Get(String^ Name, String^ Units, bool Optional, UnmanagedData& Data)
	{
	CIGet(ComponentI, Name, Units, Optional, &::EventHandler, &Data, Data.DDML());
	}
void ApsimComponent::Set(String^ Name, String^ Units, UnmanagedData& Data)
	{
	CISet(ComponentI, Name, Units, &::EventHandler, &Data, Data.DDML());
	}	
void ApsimComponent::Publish(String^ Name, UnmanagedData& Data)
	{	
	CIPublish(ComponentI, Name, &::EventHandler, &Data, Data.DDML());	
	}
void ApsimComponent::Write(String^ Line)
	{	
	CIWrite(ComponentI, Line);	
	}
void ApsimComponent::Warning(String^ Line)
	{	
	CIWarning(ComponentI, Line);	
	}
	
void ApsimComponent::CallMethodOfModel(MethodInfo^ Method, ApsimType^ Data)
	{
	if (Data == nullptr)
		Method->Invoke(Model, nullptr);
	else
		{
		array<Object^>^ Params = {Data};
		Method->Invoke(Model, Params);
		}
	}	
	
void ApsimComponent::RegisterAllProperties()
	{
	// ----------------------------------------------
	// Look for all properties and register them.		
	// ----------------------------------------------
	array<PropertyInfo^>^ Fields = Model->GetType()->GetProperties(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
	UnmanagedData* data = new WrapReflected<Single, PropertyInfo>(this, Model, Fields[0]);
	}

void ApsimComponent::RegisterAllEventHandlers()
	{
	// ----------------------------------------------
	// Look for all event handlers and register them.		
	// ----------------------------------------------
	array<MethodInfo^>^ Methods = Model->GetType()->GetMethods(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
	for (int m = 0; m != Methods->Length; m++)
		{
		array<Object^>^ myAttributes = Methods[m]->GetCustomAttributes(true);
		for (int i = 0; i != myAttributes->Length; i++)
			{
			Attribute^ Attr = dynamic_cast<Attribute^> (myAttributes[i]);
			String^ EventName = Methods[m]->Name;
			if (EventName->Substring(0, 2)->ToLower() != "on")
				throw gcnew Exception("Invalid event handler name: " + EventName + ". Name must start with 'On'");
			EventName = EventName->Substring(2);
			array<ParameterInfo^>^ pars = Methods[m]->GetParameters();
			
			UnmanagedData* data;
			ApsimType^ Argument;
			if (pars->Length > 0)
				Argument = static_cast<ApsimType^> (Activator::CreateInstance(pars[0]->ParameterType));
			else
				Argument = gcnew NullType();
			data = new WrapReflected<ApsimType^, MethodInfo>(this, Model, Methods[m], Argument);
 			CISubscribe(ComponentI, EventName, &::EventHandler, data, data->DDML());
			}
		}
	}

