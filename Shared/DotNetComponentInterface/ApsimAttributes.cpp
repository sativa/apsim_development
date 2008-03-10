using namespace System;

[AttributeUsage(AttributeTargets::Method)]
public ref class EventHandler : public System::Attribute
	{};

[AttributeUsage(AttributeTargets::Field | AttributeTargets::Property, AllowMultiple = true)]
public ref class Output : public System::Attribute
	{
	};
[AttributeUsage(AttributeTargets::Field, AllowMultiple = true)]
public ref class Writable : public System::Attribute
	{
	};	
[AttributeUsage(AttributeTargets::Field  | AttributeTargets::Property, AllowMultiple = true)]
public ref class Units : public System::Attribute
	{
	private:
		String^ St;
		
	public:
		Units(String^ Units) 
			{ 
			St = Units;
			}

		virtual String^ ToString() override 
			{
			return St;
			}

	};	
[AttributeUsage(AttributeTargets::Field  | AttributeTargets::Property, AllowMultiple = true)]
public ref class Description : public System::Attribute
	{
	private:
		String^ St;
		
	public:
		Description(String^ Description) 
			{ 
			St = Description;
			}

		virtual String^ ToString() override 
			{
			return St;
			}

	};	
	

[AttributeUsage(AttributeTargets::Class, AllowMultiple = true)]
public ref class Model : public System::Attribute
	{
	};	

