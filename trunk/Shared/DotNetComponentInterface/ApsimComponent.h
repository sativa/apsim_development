#pragma once
#using <System.dll>
#include <vector>
#include "datatypes.h"
#include "ComponentInterface.h"
#include "utility.h"
#include "ApsimEvents.h"
#include "ApsimProperties.h"

using namespace System::Reflection;

namespace ComponentInterface {
public __gc class ApsimComponent
	{
	private:
		ComponentComms* comms;
		Assembly* MyAssembly;
	
		// -------------------------------------------
		// Look for all properties and register them.		
		// -------------------------------------------
		void RegisterAllProperties()
			{
			Type* Types[] = MyAssembly->GetTypes();
			for (int t = 0; t != Types->Count; t++)
				{
				if (Types[t]->BaseType->Name->CompareTo("ApsimComponent") == 0)
					{
					PropertyInfo* Properties[] = Types[t]->GetProperties(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
					for (int p = 0; p != Properties->Count; p++)
						{
						ComponentComms::ReadWriteType ReadWrite;
						if (Properties[p]->CanRead && Properties[p]->CanWrite)
							ReadWrite = ComponentComms::readWrite;
						else if (Properties[p]->CanRead)
							ReadWrite = ComponentComms::read;
						else
							ReadWrite = ComponentComms::write;

						String* units = "";
						String* name = Properties[p]->Name;
						Object* myAttributes[] = Properties[p]->GetCustomAttributes(true);
						for (int i = 0; i != myAttributes->Length; i++)
							{
							Attribute* Attr = dynamic_cast<Attribute*> (myAttributes[i]);
							String* Str = Attr->ToString();
							int PosDelimiter = Str->IndexOf("|");
							if (PosDelimiter != -1)
								{
								name = Str->Substring(0, PosDelimiter);
								units = Str->Substring(PosDelimiter+1);
								if (name->Length == 0)
								   name = Properties[p]->Name;
								}
							}
						if (myAttributes->Length > 0)
							{
							IData* data = NULL;
							String* MemberType = Properties[p]->get_PropertyType()->Name;
							if (MemberType->CompareTo("Single") == 0)
								data = new WrapMemberInfo<ApsimSingle, PropertyInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Double") == 0)
								data = new WrapMemberInfo<ApsimDouble, PropertyInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Int32") == 0)
								data = new WrapMemberInfo<ApsimInteger4, PropertyInfo>(Properties[p], this);
							else if (MemberType->CompareTo("String[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimString, String*>, PropertyInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Int32[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimInteger4, Int32>, PropertyInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Single[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimSingle, Single>, PropertyInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Double[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimDouble, Double>, PropertyInfo>(Properties[p], this);
							
							if (data != NULL)
								comms->registerProperty(stringToStdString(name), stringToStdString(units), ReadWrite, data);
							}
						}
					}
				}
			}	


		// -------------------------------------------
		// Look for all fields and register them.		
		// -------------------------------------------
		void RegisterAllFields()
			{
			Type* Types[] = MyAssembly->GetTypes();
			for (int t = 0; t != Types->Count; t++)
				{
				if (Types[t]->BaseType->Name->CompareTo("ApsimComponent") == 0)
					{
					FieldInfo* Properties[] = Types[t]->GetFields(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
					for (int p = 0; p != Properties->Count; p++)
						{
						ComponentComms::ReadWriteType ReadWrite = ComponentComms::ReadWriteType::read;
						String* units = "";
						String* name = Properties[p]->Name;
						
						Object* myAttributes[] = Properties[p]->GetCustomAttributes(true);
						for (int i = 0; i != myAttributes->Length; i++)
							{
							Attribute* Attr = dynamic_cast<Attribute*> (myAttributes[i]);
							String* Str = Attr->ToString();
							int PosDelimiter = Str->IndexOf("|");
							if (PosDelimiter != -1)
								{
								name = Str->Substring(0, PosDelimiter);
								if (name->Length == 0)
								   name = Properties[p]->Name;
								units = Str->Substring(PosDelimiter+1);
								PosDelimiter = units->IndexOf("|");
								if (PosDelimiter != -1)
									{
									String* ReadWriteString = units->Substring(PosDelimiter+1);
									units = units->Substring(0, PosDelimiter);
									if (ReadWriteString->CompareTo("Read") == 0)
										ReadWrite = ComponentComms::ReadWriteType::read;
									else if (ReadWriteString->CompareTo("Write") == 0)
										ReadWrite = ComponentComms::ReadWriteType::write;
									else	
										ReadWrite = ComponentComms::ReadWriteType::readWrite;
									}
								}
							}
						if (myAttributes->Length > 0)
							{
							IData* data = NULL;
							String* MemberType = Properties[p]->get_FieldType()->Name;
							if (MemberType->CompareTo("Single") == 0)
								data = new WrapMemberInfo<ApsimSingle, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Double") == 0)
								data = new WrapMemberInfo<ApsimDouble, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Int32") == 0)
								data = new WrapMemberInfo<ApsimInteger4, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("String") == 0)
								data = new WrapMemberInfo<ApsimString, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("String[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimString, String*>, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Int32[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimInteger4, Int32>, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Single[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimSingle, Single>, FieldInfo>(Properties[p], this);
							else if (MemberType->CompareTo("Double[]") == 0)
								data = new WrapMemberInfo<ApsimArray<ApsimDouble, Double>, FieldInfo>(Properties[p], this);
							
							if (data != NULL)
								comms->registerProperty(stringToStdString(name), stringToStdString(units), ReadWrite, data);
							}
						}
					}
				}
			}	
			
		// -------------------------------------------
		// Look for all event handlers and register them.		
		// -------------------------------------------
		void RegisterAllEventHandlers()
			{
			Type* Types[] = MyAssembly->GetTypes();
			for (int t = 0; t != Types->Count; t++)
				{
				if (Types[t]->BaseType->Name->CompareTo("ApsimComponent") == 0)
					{
					MethodInfo* Methods[] = Types[t]->GetMethods(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
					for (int m = 0; m != Methods->Count; m++)
						{
						Object* myAttributes[] = Methods[m]->GetCustomAttributes(true);
						for (int i = 0; i != myAttributes->Length; i++)
							{
							Attribute* Attr = dynamic_cast<Attribute*> (myAttributes[i]);
						    String* EventName = Attr->ToString();
							ParameterInfo*  pars[] = Methods[m]->GetParameters();
							if (pars->Length == 1)
								{
								IEventData* data = new WrapMethodInfo<MethodInfo> (Methods[m], this);
								comms->registerEventHandler(stringToStdString(EventName), data);
								}			
							}
						}
						
					}
				}
			}
				
		
	protected:
		String* Name;
		VBGeneral::APSIMData* Data;
		ApsimEvents* events;
		ApsimProperties* properties;		
		
		// ---------------------------------------
		// Notify system of a warning.
		// Errors should be thrown.
		// ---------------------------------------
		void warning(String* msg) 
			{ 
			comms->warning(stringToStdString(msg));
			}

		// ---------------------
		// Write to summary file
		// ---------------------
		void writeToSummary(String* line) 
			{ 
			comms->writeToSummary(stringToStdString(line));
			}

		// --------------------
		// Terminate simulation
		// --------------------
		void terminateSimulation(void) 
			{ 
			comms->terminateSimulation();
			}
			
	public:

		
		// --------------------------------
		// Called by componentinterface
		// --------------------------------
		void Setup(ComponentComms* componentComms, String* N, String* SDML)
			{
			comms = componentComms;
			events = new ApsimEvents(componentComms);
			properties = new ApsimProperties(componentComms);
			Name = N;
			Data = new VBGeneral::APSIMData(SDML);
			RegisterAllProperties();
			RegisterAllFields();
			RegisterAllEventHandlers();
			}


		// --------------------------------
		// Called by componentinterface
		// --------------------------------
		void SetAssembly(Assembly* assembly)
			{
			MyAssembly = assembly;
			}

		// -----------------------------------
		// Methods that may be overridden.
		// -----------------------------------
		virtual void Init1(void) { }
		virtual void Init2(void) { }
	
					
	};	
	
	
	

// -----------------------------------
// Create an instance of the component
// -----------------------------------

ApsimComponent* createInstanceOfComponent(const std::string& dllFileName);
	
};