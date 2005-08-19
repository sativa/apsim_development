#include "StdAfx.h"
#using <mscorlib.dll>
#pragma managed
#include "Interfaces.h"

using namespace System;
using namespace System::Collections;
using namespace System::Reflection;

// -----------------------------------
// Create an instance of the component
// -----------------------------------
IComponent* createInstanceOfComponent(const std::string& dllFileName)
   {
	Assembly* assembly = Assembly::LoadFrom(dllFileName.c_str());
	Type* Types[] = assembly->GetTypes();
	IEnumerator* myEnum = Types->GetEnumerator();
	while (myEnum->MoveNext())
		{
		Type* oType = __try_cast<Type*>(myEnum->Current);
		if (oType->BaseType->Name->CompareTo("IComponent") == 0)
			{
			return dynamic_cast<IComponent*> (Activator::CreateInstance(oType));
//			return component;
			}
		}
	return NULL;
   }
