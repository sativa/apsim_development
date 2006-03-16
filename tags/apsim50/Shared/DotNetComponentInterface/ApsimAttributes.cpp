#include "ApsimProperties.h"

using namespace System;
using namespace ComponentInterface;


[AttributeUsage(AttributeTargets::Property)]
public __gc class ApsimProperty : public System::Attribute
	{
	private:
		String* MyName;
		String* MyUnits;
		
	public:
		ApsimProperty(String* Name, String* Units) : MyName(Name), MyUnits(Units) { }

		virtual String* ToString() {return MyName->Concat(MyName, "|", MyUnits);}

	};


[AttributeUsage(AttributeTargets::Method)]
public __gc class ApsimEvent : public System::Attribute
	{
	private:
		String* MyName;
		
	public:
		ApsimEvent(String* Name) : MyName(Name) { }

		virtual String* ToString() {return MyName;}

	};


[AttributeUsage(AttributeTargets::Field, AllowMultiple = true)]
public __gc class ApsimVariable : public System::Attribute
	{
	private:
		String* St;
		
	public:
		ApsimVariable(String* Name, String* Units, ApsimProperties::ReadWriteType ReadWrite) 
			{ 
			St = String::Concat(Name, "|", Units, "|");
			if (ReadWrite == ApsimProperties::ReadWriteType::Read)
				St = String::Concat(St, "Read");
			else if (ReadWrite == ApsimProperties::ReadWriteType::Write)
				St = String::Concat(St, "Write");
			else
				St = String::Concat(St, "ReadWrite");
			}

		virtual String* ToString() 
			{
			return St;
			}

	};

