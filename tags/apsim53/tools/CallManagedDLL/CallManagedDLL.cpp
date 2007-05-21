#include "stdafx.h"
#include "CallManagedDLL.h"

using namespace System;
using namespace System::Reflection;
using namespace System::Windows::Forms;

	void CallDLLInternal(String^ fileName, String^ className, String^ methodName, String^ methodArgument)
		{	
		try
			{
			// load assembly and create an instance of class.
			Assembly^ assembly = Assembly::LoadFrom(fileName);
			Type^ t = assembly->GetType(className, true);
			Object^ dllObject = Activator::CreateInstance(t);

			// call required method.
			MethodInfo^ Method = t->GetMethod(methodName);
			if (Method == nullptr)
				MessageBox::Show("Cannot find method '" + methodName + "' in class '" + className + "'");
			else
				{
				array<Object^>^ methodArgs = gcnew array<Object^>(1);
				methodArgs[0] = methodArgument;
				Method->Invoke(dllObject, methodArgs);
				}
			}
		catch (Exception^ err)
			{
				MessageBox::Show("Error: " + err->Message);
			}
		}
extern "C" __declspec(dllexport) void __stdcall  CallDLL(const char* dllFileName, const char* className,
														 const char* methodName, const char* methodArgument)
	{
	CallDLLInternal(gcnew String(dllFileName), gcnew String(className), gcnew String(methodName), gcnew String(methodArgument));
	}
