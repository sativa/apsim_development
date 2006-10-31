#pragma once
#include <string>
#include <vector>
#include "Message.h"
#include <vcclr.h>
using namespace System;

using namespace System;
using namespace System::ComponentModel;
//using namespace System::Collections;
//using namespace System::Windows::Forms;
//using namespace System::Data;
//using namespace System::Drawing;

namespace ComponentInterface {

class ApsimInteger4;
class ApsimSingle;
class ApsimDouble;
class ApsimString;
class ApsimBoolean;
template <class InternalT, class ExternalT>
class ApsimArray;

// ----------------------------------------
// Data interface.
// ----------------------------------------
class IPackable
	{
	public:
		virtual void pack(Message& message) = 0;
		virtual void unpack(Message& message) = 0;
	};	
class IData : public IPackable
	{
	protected:
		void ThrowConvertError(const std::string& TypeName)
			{
			std::string msg = "Cannot convert from an " + TypeName + " to:\n" + std::string(ddml());
			throw gcnew Exception(gcnew String(msg.c_str()));
			}
	    
	public:
		virtual const char* ddml() = 0;
		virtual void SetValue(Object^ Value) {ThrowConvertError("object");}
		virtual void SetValue(ApsimArray<ApsimInteger4, Int32>* FromData) {ThrowConvertError("integer array");}
		virtual void SetValue(ApsimArray<ApsimSingle, Single>* FromData) {ThrowConvertError("single array");}
		virtual void SetValue(ApsimArray<ApsimDouble, Double>* FromData) {ThrowConvertError("double array");}
		virtual void SetValue(ApsimArray<ApsimString, String^>* FromData) {ThrowConvertError("string array");}
		virtual void SetValue(ApsimArray<ApsimBoolean, Boolean>* FromData) {ThrowConvertError("boolean array");}		
	};
class IEventData : public IData
	{
	public:
		virtual void invokeEvent(Message& message) 
			{
			throw gcnew System::Exception(gcnew String("Cannot invoke event"));
			};
	};
	
public ref class IManagedData abstract
	{
	public:
		virtual void pack(Message& message) = 0;
		virtual void unpack(Message& message) = 0;
		virtual const char* ddml() = 0;
		virtual void invokeEvent(Message& message) = 0;
	};
	

};