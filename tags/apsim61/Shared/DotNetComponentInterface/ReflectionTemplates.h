#pragma once
#include "MessageData.h"

namespace ModelFramework {

template <class T>
T GetValue(FieldInfo^ Field, Object^ Model)
	{
	return (T) Field->GetValue(Model);
	}
template <class T>
void SetValue(FieldInfo^ Field, Object^ Model, T Data)
	{
	Field->SetValue(Model, Data);
	}
void GetFields(Type^ T, array<FieldInfo^>^% Fields)
	{
	Fields = T->GetFields(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
	}
String^ GetFieldTypeName(FieldInfo^ Field)
	{
	return Field->FieldType->Name;
	}
bool IsWritable(FieldInfo^ Field, Attribute^ Attr)
	{
	return (Attr->GetType()->Name == "Writable");
	}
template <class T>
T GetValue(PropertyInfo^ Field, Object^ Model)
	{
	return (T) Field->GetValue(Model, nullptr);
	}
template <class T>
void SetValue(PropertyInfo^ Field, Object^ Model, T Data)
	{
	Field->SetValue(Model, Data, nullptr);
	}
void GetFields(Type^ T, array<PropertyInfo^>^% Fields)
	{
	Fields = T->GetProperties(static_cast<BindingFlags>(BindingFlags::Public | BindingFlags::Instance));
	}
String^ GetFieldTypeName(PropertyInfo^ Property)
	{
	return Property->PropertyType->Name;
	}
bool IsWritable(PropertyInfo^ Field, Attribute^ Attr)
	{
	return Field->CanWrite;
	}
template <class T>
T GetValue(MethodInfo^ Method, Object^ Model)
	{
	throw gcnew Exception("Cannot pack a method");
	}
template <class T>
void SetValue(MethodInfo^ Method, Object^ Model, T Data)
	{
	Object^ t = (T) Data;
	Type^ typ = t->GetType();
	if (typ->Name == "NullType")
		Method->Invoke(Model, nullptr);
	else
		{
		array<Object^>^ Params = { (T)Data};
		Method->Invoke(Model, Params);
		}
	}
	
template <class T, class MI>
class WrapReflected : public WrapManaged<T>
	{
   private:
      gcroot<MI^> Field;
      gcroot<Object^> Model;
      gcroot<T> Temp;
	public:
      WrapReflected(ApsimComponent^ Comp, Object^ model, MI^ field) 
         : WrapManaged<T>(Comp, GetValue<T>(field, model)), Model(model), Field(field)
         { 
         }
      WrapReflected(ApsimComponent^ Comp, Object^ model, MI^ field, ApsimType^ data) 
         : WrapManaged<T>(Comp, data), Model(model), Field(field)
         { 
         }         
	  virtual void pack(char* message)
         {
         Data = GetValue<T>(Field, Model);
         WrapManaged<T>::pack(message);
		 }
      virtual void unpack(char* message)
         {
         WrapManaged<T>::unpack(message);
         SetValue<T>(Field, Model, (T) Data);
		 }
	  virtual unsigned memorySize()
         {
         Data = GetValue<T>(Field, Model);
         return WrapManaged<T>::memorySize();
		 }
	};
template <class T, class MI>
class WrapReflectedWithConverter : public WrapManagedWithConverter<T>
	{
   private:
      gcroot<MI^> Field;
      gcroot<Object^> Model;
      gcroot<T> Temp;
	public:
      WrapReflectedWithConverter(ApsimComponent^ Comp, Object^ model, MI^ field) 
         : WrapManagedWithConverter<T>(Comp, GetValue<T>(field, model)), Model(model), Field(field)
         {
         }
      WrapReflectedWithConverter(ApsimComponent^ Comp, Object^ model, MI^ field, ApsimType^ data) 
         : WrapManagedWithConverter<T>(Comp, data), Model(model), Field(field)
         { 
         }         
	  virtual void pack(char* message)
         {
         Data = GetValue<T>(Field, Model);
         WrapManagedWithConverter<T>::pack(message);
		 }
      virtual void unpack(char* message)
         {
         WrapManagedWithConverter<T>::unpack(message);
         SetValue<T>(Field, Model, (T) Data);
		 }
	  virtual unsigned memorySize()
         {
         Data = GetValue<T>(Field, Model);
         return WrapManaged<T>::memorySize();
		 }
	};	
template <class T> 
void RegisterAllFields(ApsimComponent^ Component, Object^ Model, int ComponentI)
	{
	// ----------------------------------------------
	// Look for all fields and register them.		
	// ----------------------------------------------
	array<T^>^ Fields;
	GetFields(Model->GetType(), Fields);
	for (int f = 0; f != Fields->Length; f++)
		{
		UnmanagedData* data = NULL;
		String^ Units = "";
		String^ Description = "";
		bool Writable = false;
		array<Object^>^ myAttributes = Fields[f]->GetCustomAttributes(true);
		for (int i = 0; i != myAttributes->Length; i++)
			{
			Attribute^ Attr = dynamic_cast<Attribute^> (myAttributes[i]);
			if (Attr->GetType()->Name == "Output")
				{
				if (GetFieldTypeName(Fields[f])->CompareTo("Single") == 0)
					data = new WrapReflectedWithConverter<Single, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("Double") == 0)
					data = new WrapReflectedWithConverter<Double, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("Int32") == 0)
					data = new WrapReflectedWithConverter<Int32, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("String") == 0)
					data = new WrapReflectedWithConverter<String^, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("String[]") == 0)
					data = new WrapReflected<array<String^>^, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("Int32[]") == 0)
					data = new WrapReflected<array<Int32>^, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("Single[]") == 0)
					data = new WrapReflected<array<Single>^, T>(Component, Model, Fields[f]);
				else if (GetFieldTypeName(Fields[f])->CompareTo("Double[]") == 0)
					data = new WrapReflected<array<Double>^, T>(Component, Model, Fields[f]);
				else
					throw gcnew Exception("Unknown field type: " + GetFieldTypeName(Fields[f]));
				}
			else if (Attr->GetType()->Name == "Units")
				Units = Attr->ToString();
			else if (Attr->GetType()->Name == "Description")
				Description = Attr->ToString();
			if (IsWritable(Fields[f], Attr))
				Writable = true;
			}
		if (data != NULL)	
			CIExpose(ComponentI, Fields[f]->Name, Units, Description, Writable, &::EventHandler, data, data->DDML());
		}
	}   	   

};