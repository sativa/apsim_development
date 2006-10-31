#include "ApsimProperties.h"

using namespace System;
using namespace ComponentInterface;


[AttributeUsage(AttributeTargets::Property)]
public ref class ApsimProperty : public System::Attribute
	{
	private:
		String^ MyName;
		String^ MyUnits;
		
	public:
		ApsimProperty(String^ Name, String^ Units) : MyName(Name), MyUnits(Units) { }

		virtual String^ ToString() override {return MyName->Concat(MyName, "|", MyUnits);}

	};


[AttributeUsage(AttributeTargets::Method)]
public ref class ApsimEvent : public System::Attribute
	{
	private:
		String^ MyName;
		
	public:
		ApsimEvent(String^ Name) : MyName(Name) { }

		virtual String^ ToString() override {return MyName;}

	};


[AttributeUsage(AttributeTargets::Field, AllowMultiple = true)]
public ref class ApsimVariable : public System::Attribute
	{
	private:
		String^ St;
		
	public:
		ApsimVariable(String^ Name, String^ Units, ApsimProperties::ReadWriteType ReadWrite) 
			{ 
			St = String::Concat(Name, "|", Units, "|");
			if (ReadWrite == ApsimProperties::ReadWriteType::Read)
				St = String::Concat(St, "Read");
			else if (ReadWrite == ApsimProperties::ReadWriteType::Write)
				St = String::Concat(St, "Write");
			else
				St = String::Concat(St, "ReadWrite");
			}

		virtual String^ ToString() override 
			{
			return St;
			}

	};

