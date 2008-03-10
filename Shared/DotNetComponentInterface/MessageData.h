#pragma once

#include "Interfaces.h"
#include "DataTypes.h"
#include <string>
#include <vcclr.h>
using namespace System::Runtime::InteropServices;

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackBool", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void pack(char* messageData, Boolean Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackInt", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void pack(char* messageData, Int32 Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackSingle", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void pack(char* messageData, Single Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackDouble", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void pack(char* messageData, Double Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIPackString", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void pack(char* messageData, String^ Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackBool", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpack(char* messageData, Boolean% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackInt", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpack(char* messageData, Int32% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackSingle", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpack(char* messageData, Single% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackDouble", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpack(char* messageData, Double% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackString", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpack(char* messageData, System::Text::StringBuilder^ Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackBoolWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpackWithConverter(char* messageData, Boolean% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackIntWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpackWithConverter(char* messageData, Int32% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackSingleWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpackWithConverter(char* messageData, Single% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackDoubleWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpackWithConverter(char* messageData, Double% Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIUnPackStringWithConverter", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	void unpackWithConverter(char* messageData, System::Text::StringBuilder^ Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeBool", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	unsigned memorySize(bool Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeInt", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	unsigned memorySize(int Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeSingle", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	unsigned memorySize(float Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeDouble", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	unsigned memorySize(double Data);

	[DllImport("ComponentInterface2.dll", EntryPoint = "CIMemorySizeString", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
	unsigned memorySize(String^ Data);

	inline String^ DDML(Boolean Data)
		{
		return "<type kind=\"bool\"/>";
		}
	inline String^ DDML(Int32 Data)
		{
		return "<type kind=\"integer4\"/>";
		}
	inline String^ DDML(Single Data)
		{
		return "<type kind=\"single\"/>";
		}
	inline String^ DDML(Double Data)
		{
		return "<type kind=\"double\"/>";
		}
	inline String^ DDML(String^ Data)
		{
		return "<type kind=\"string\"/>";
		}

	inline void unpack(char* messageData, String^% st)
		{
		System::Text::StringBuilder^ Contents = gcnew System::Text::StringBuilder(500);
		::unpack(messageData, Contents);
		st = Contents->ToString();
		}
	inline void unpackWithConverter(char* messageData, String^% st)
		{
		System::Text::StringBuilder^ Contents = gcnew System::Text::StringBuilder(500);
		::unpackWithConverter(messageData, Contents);
		st = Contents->ToString();
		}

	
	template <class T>
	void pack(char* messageData, array<T>^ values)
	   {
	   ::pack(messageData, values->Length);
	   for (int i = 0; i < values->Length; i++)
		  ::pack(messageData, values[i]);
	   }

	template <class T>
	void unpack(char* messageData, array<T>^% values)
	   {
	   int count = 0;
	   ::unpack(messageData, count);
	   values = gcnew array<T>(count);
	   for (int i = 0; i < count; i++)
		  ::unpack(messageData, values[i]);
	   }

	template <class T>
	void unpackArrayOfStructures(char* messageData, array<T^>^% values)
	   {
	   int count = 0;
	   ::unpack(messageData, count);
	   values = gcnew array<T^>(count);
	   for (int i = 0; i < count; i++)
		  {
		  values[i] = gcnew T;
		  ::unpack(messageData, values[i]);
		  }
	   }


	template <class T>
	unsigned memorySize(array<T>^ values)
	   {
	   return 4 + values->Length * ::memorySize(values[0]);
	   }
	template <class T>
	String^ DDML(array<T>^ values)
		{
		T Dummy;
		String^ TypeString = ::DDML(Dummy);
		return TypeString->Substring(0, TypeString->Length-2) + " array=\"T\"/>";
		}
	   

namespace ModelFramework {
         
template <class T>
class WrapManaged : public UnmanagedData
	{
	public:
		gcroot<ApsimComponent^> Component;
		gcroot<T> Data;
		WrapManaged(ApsimComponent^ Comp, T data) : Component(Comp) 
			{
			Data = data;
			}
		void pack(char* message)
			{
			::pack(message, (T) Data);
			}
		void unpack(char* message)
			{
			T Temp = Data;
			::unpack(message, Temp);
			Data = Temp;
			}
		unsigned memorySize()
			{
			return ::memorySize((T) Data);
			}
		String^ DDML()
			{
			return ::DDML((T) Data);
			}
		virtual ApsimComponent^ GetComponentInstance() 
			{
			return Component;
			}
	
	};

template <class T>
class WrapManagedWithConverter : public WrapManaged<T>
	{
	public:
		WrapManagedWithConverter(ApsimComponent^ Comp, T data) : WrapManaged<T>(Comp, data) 
			{ }
		void unpack(char* message)
			{
			T Temp = Data;
			::unpackWithConverter(message, Temp);
			Data = Temp;
			}
	};
template <class T>
class WrapManagedRefWithConverter : public WrapManagedWithConverter<T>
	{
	public:
		WrapManagedRefWithConverter(ApsimComponent^ Comp, T^ data) : WrapManagedWithConverter<T>(Comp, *data) 
			{ }
	};	



};	
		