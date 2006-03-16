#include "StdAfx.h"
#using <mscorlib.dll>
#pragma managed
#using <VBGeneral.dll>
#include "Interfaces.h"
#include "ApsimComponent.h"

using namespace System;
using namespace System::Collections;
using namespace System::Reflection;

namespace ComponentInterface {
// -----------------------------------
// Create an instance of the component
// -----------------------------------
ApsimComponent* createInstanceOfComponent(const std::string& dllFileName)
   {
	Assembly* assembly = Assembly::LoadFrom(dllFileName.c_str());
	Type* Types[] = assembly->GetTypes();
	IEnumerator* myEnum = Types->GetEnumerator();
	while (myEnum->MoveNext())
		{
		Type* oType = __try_cast<Type*>(myEnum->Current);
		if (oType->BaseType->Name->CompareTo("ApsimComponent") == 0)
			{
			ApsimComponent* Comp = dynamic_cast<ApsimComponent*> (Activator::CreateInstance(oType));
			Comp->SetAssembly(assembly);
			return Comp;
			}
		}
	return NULL;
   }
};