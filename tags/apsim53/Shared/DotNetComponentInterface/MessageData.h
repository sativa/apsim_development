#pragma once

#include "Message.h"
#include "Utility.h"
#include "Interfaces.h"
#include <string>
#include <vcclr.h>

namespace ComponentInterface {


class ApsimInteger4 : public IData
	{
	private:
		int Data;
	public:
		ApsimInteger4() : Data(0) { }
		ApsimInteger4(int value) : Data(value) { }
		ApsimInteger4(Int32^ value) : Data(*value) { }
		ApsimInteger4(Object^ value) {Data = Convert::ToInt32(value);}
		ApsimInteger4(Message& message) {unpack(message);}
		int value() {return Data;}
		Object^ AsObject() {return Data;}
		void SetValue(gcroot<Object^> Value) {Data = Convert::ToInt32(Value);}
		void pack(Message& message) 
			{
			*(int*)message.dataStream() = Data; 
			message.addToDataSize(4);	
			message.advanceDataStream(4);
			}
		void unpack(Message& message) 
			{
			Data = *(int*)message.dataStream(); 
			message.advanceDataStream(4);
			}

		const char* ddml()
			{
			return "<type kind=\"integer4\"/>";
			}
	};


class ApsimString : public IData
	{
	private:
		std::string Data;
	public:
		ApsimString() { }
		ApsimString(const std::string& st) : Data(st) { }
		ApsimString(String^ v) {Data = stringToStdString(v);}
		ApsimString(Object^ v) {Data = stringToStdString(dynamic_cast<String^> (v));}
		ApsimString(Message& message) {unpack(message);}
		
		String^ value() {return gcnew String(Data.c_str());}
		const char* CValue() {return Data.c_str();}
		Object^ AsObject() {return gcnew String(Data.c_str());}
		void SetValue(gcroot<Object^> Value) {Data = stringToStdString(Convert::ToString(Value));}
		
		#pragma warning(disable : 4996)
		void pack(Message& message) 
			{
			ApsimInteger4(Data.length()).pack(message);
			strcpy(message.dataStream(), Data.c_str()); 
			message.addToDataSize(Data.length());
			message.advanceDataStream(Data.length());
			}
		void unpack(Message& message) 
			{
			ApsimInteger4 Length;
			Length.unpack(message);
			int length = Length.value();
			
			char* buffer = new char[length+1];
			strncpy(buffer, message.dataStream(), length);
			buffer[length] = 0; 
			Data = buffer;
			message.advanceDataStream(length);
			delete [] buffer;
			}
		const char* ddml()
			{
			return "<type kind=\"string\"/>";
			}	
	};

class ApsimSingle : public IData
	{
	private:
		float Data;
	public:
		ApsimSingle() : Data(0) { }
		ApsimSingle(float value) : Data(value) { }
		ApsimSingle(Single^ value) : Data(*value) { }
		ApsimSingle(Object^ value) {Data = Convert::ToSingle(value);}
		ApsimSingle(Message& message) {unpack(message);}
		
		float value() {return Data;}
		Object^ AsObject() {return Data;}

		void SetValue(gcroot<Object^> Value) {Data = Convert::ToSingle(Value);}
		
		
		void pack(Message& message) 
			{
			*(float*)message.dataStream() = Data; 
			message.addToDataSize(4);	
			message.advanceDataStream(4);
			}
		void unpack(Message& message) 
			{
			Data = *(float*)message.dataStream(); 
			message.advanceDataStream(4);
			}

		const char* ddml()
			{
			return "<type kind=\"single\"/>";
			}
	};
class ApsimDouble : public IData
	{
	private:
		double Data;
	public:
		ApsimDouble() : Data(0) { }
		ApsimDouble(double value) : Data(value) { }
		ApsimDouble(Double^ value) : Data(*value) { }
		ApsimDouble(Object^ value){Data = Convert::ToDouble(value);}
		ApsimDouble(Message& message) {unpack(message);}
		
		double value() {return Data;}
		Object^ AsObject() {return Data;}
		void SetValue(gcroot<Object^> Value) {Data = Convert::ToDouble(Value);}
		void pack(Message& message) 
			{
			*(double*)message.dataStream() = Data; 
			message.addToDataSize(8);	
			message.advanceDataStream(8);
			}
		void unpack(Message& message) 
			{
			Data = *(double*)message.dataStream(); 
			message.advanceDataStream(8);
			}

		const char* ddml()
			{
			return "<type kind=\"double\"/>";
			}
	};
class ApsimBoolean : public IData
	{
	private:
		bool Data;
	public:
		ApsimBoolean() : Data(0) { }
		ApsimBoolean(bool value) : Data(value) { }
		ApsimBoolean(Boolean^ value) : Data(*value) { }
		ApsimBoolean(Object^ value){Data = Convert::ToBoolean(value);}
		ApsimBoolean(Message& message) {unpack(message);}
		
		bool value() {return Data;}
		Object^ AsObject() {return Data;}
		void SetValue(gcroot<Object^> Value) {Data = Convert::ToBoolean(Value);}
		void pack(Message& message) 
			{
			*(bool*)message.dataStream() = Data; 
			message.addToDataSize(1);	
			message.advanceDataStream(1);
			}
		void unpack(Message& message) 
			{
			Data = *(bool*)message.dataStream(); 
			message.advanceDataStream(1);
			}

		const char* ddml()
			{
			return "<type kind=\"boolean\"/>";
			}
	};


template <class InternalT, class ExternalT>
class ApsimArray : public IData
	{
	private:
		InternalT* Data;
		int Count;
		std::string TypeString;
		
		template <class IT, class XT>
		void SetValue(ApsimArray<IT, XT>* FromData)
			{
			Count = FromData->Length();
			delete Data;
			Data = new InternalT[Count];
			for (int i = 0; i != Count; i++)
				Data[i].SetValue((*FromData)[i].AsObject());
			}		
	public:
		ApsimArray() : Data(NULL) { }
		~ApsimArray() {delete Data;}
		ApsimArray(Object^ Values)
			{
			array<ExternalT>^ Vals = safe_cast<array<ExternalT> > (Convert::ChangeType(Values, array<ExternalT>::typeid));
			Count = Vals->Length;
			Data = new InternalT[Count];
			for (int i = 0; i != Count; i++)
				Data[i] = InternalT(Vals[i]);
			
			}
		ApsimArray(array<ExternalT>^ Values) 
			{ 
			Count = Values->Length;
			Data = new InternalT[Count];
			for (int i = 0; i != Count; i++)
				Data[i] = InternalT(Values[i]);
			}
		ApsimArray(Message& message) 
			{ 
			Data = NULL;
			unpack(message);
			}
			
		array<ExternalT>^ value()
			{
			array<ExternalT>^ Vals = gcnew array<ExternalT>(Count);
			for (int i = 0; i != Count; i++)
				Vals[i] = Data[i].value();			
			return Vals;
			}

		InternalT operator[](int Index)
			{
			if (Index >= Count)
				throw gcnew Exception("Invalid index found in ApsimArray");
			return Data[Index].value();
			}
			
		Object^ AsObject() {return value();}
		virtual void SetValue(gcroot<Object^> Value) {throw gcnew Exception("Cannot convert an object to an array");}
		
		int Length() {return Count;}
		
		
		void SetValue(ApsimArray<ApsimInteger4, Int32>* FromData) {SetValue<ApsimInteger4, Int32>(FromData);}
		void SetValue(ApsimArray<ApsimSingle, Single>* FromData)  {SetValue<ApsimSingle, Single>(FromData);}
		void SetValue(ApsimArray<ApsimDouble, Double>* FromData)  {SetValue<ApsimDouble, Double>(FromData);}
		void SetValue(ApsimArray<ApsimString, String^>* FromData) {SetValue<ApsimString, String^>(FromData);}
		void SetValue(ApsimArray<ApsimBoolean, Boolean>* FromData){SetValue<ApsimBoolean, Boolean>(FromData);}
	
		void pack(Message& message)
			{
			ApsimInteger4(Count).pack(message);
			for (int i = 0; i != Count; i++)
				Data[i].pack(message);
			}
		void unpack(Message& message)
			{
			ApsimInteger4 count;
			count.unpack(message);
			Count = count.value();
			delete Data;
			Data = new InternalT[Count];
			for (int i = 0; i != Count; i++)
				Data[i].unpack(message);
			}
		const char* ddml()
			{
			if (TypeString == "")
				{
				InternalT dummy;
				TypeString = dummy.ddml();
				TypeString = TypeString.substr(0, TypeString.length()-2) + " array=\"T\"/>";
				}
			return TypeString.c_str();
			}
	};


inline Object^ GetValueFromMember(Reflection::PropertyInfo^ Property, Object^ ComponentInstance)
	{return Property->GetValue(ComponentInstance, nullptr);}
inline Object^ GetValueFromMember(Reflection::FieldInfo^ Field, Object^ ComponentInstance)
	{return Field->GetValue(ComponentInstance);}
inline Object^ GetValueFromMember(Reflection::MethodInfo^ Method, Object^ ComponentInstance)
	{throw gcnew Exception("Cannot get a value from an event handler");}
	
inline void SetValueInMember(Reflection::PropertyInfo^ Property, Object^ ComponentInstance, Object^ Value)
	{Property->SetValue(ComponentInstance, Value, nullptr);}
inline void SetValueInMember(Reflection::FieldInfo^ Field, Object^ ComponentInstance, Object^ Value)
	{Field->SetValue(ComponentInstance, Value);}
inline void SetValueInMember(Reflection::MethodInfo^ Method, Object^ ComponentInstance, Object^ Value)
	{
	array<Object^>^ Params = {Value};
	Method->Invoke(ComponentInstance, Params);
	}
template <class T, class IT>
class WrapMemberInfo : public IEventData
   {
   private:
      gcroot<IT^> Member;
      gcroot<Object^> ComponentInstance;
      T Dummy;
	  std::string TypeString;
	  std::string Units;
   public:
      WrapMemberInfo(IT^ member, Object^ ComponentInst, const std::string& units) 
         : Member(member), ComponentInstance(ComponentInst), Units(units)
         { }

	  void SetValue(Object^ Value)
		{
		T value;
		value.SetValue(Value);
		SetValueInMember(Member, ComponentInstance, value.AsObject());
        }

            
      void pack(Message& message)
         {
         T value(GetValueFromMember(Member, ComponentInstance));
         value.pack(message);
		 }
      void unpack(Message& message)
         {
         T value(message);
         SetValueInMember(Member, ComponentInstance, value.AsObject());
         }
      const char* ddml()
         {
		 if (TypeString == "")
			 {
			 TypeString = Dummy.ddml();
			 TypeString = TypeString.substr(0, TypeString.length()-2) + " unit=\"" + Units + "\"/>";
			 }
         return TypeString.c_str();
         }
   };
template <class IT>
class WrapMethodInfo : public IEventData
   {
   private:
      gcroot<IT^> Member;
      gcroot<Object^> ComponentInstance;
      gcroot<IManagedData^> Dummy;
   public:
      WrapMethodInfo(IT^ member, Object^ ComponentInst) 
         : Member(member), ComponentInstance(ComponentInst)
         { 
         Dummy = static_cast<IManagedData^> (Activator::CreateInstance(Member->GetParameters()[0]->ParameterType));
         }
            
      void pack(Message& message)
         {
         throw gcnew Exception("Cannot pack a method");
		 }
      void unpack(Message& message)
         {
		 Dummy->unpack(message);
		 }
	  void invokeEvent(Message& message)
		 {
		 SetValueInMember(Member, ComponentInstance, Dummy);			
         }
      const char* ddml()
         {
         return Dummy->ddml();
         }
   };      
template <class T>
class WrapManaged : public IEventData
	{
	private:
		gcroot<T> Data;
	public:
		WrapManaged(T d) : Data(d) { }
		void pack(Message& message)
			{
			Data->pack(message);
			}
		void unpack(Message& message)
			{
			Data->unpack(message);
			}
		const char* ddml()
			{
			return Data->ddml();
			}
		void invokeEvent(Message& message)
			{
			Data->invokeEvent(message);
			}
	
	};


// Using this template IS NOT SAFE as it doesn't pin pointers. 
// Two instances of SLURP confirmed this. See micromet sample.
template <class InternalT, class ExternalT>
class WrapVariableAlias : public IData
   {
   private:								 
      gcroot<ExternalT> Variable  ;
      InternalT* Dummy;
	  std::string TypeString;
	  std::string Units;
   public:
      WrapVariableAlias(ExternalT& v, const std::string& units) : Variable(v), Units(units) 
		{Dummy = new InternalT;}
      ~WrapVariableAlias() {delete Dummy;}
      void pack(Message& message)
         {
         InternalT(Variable).pack(message);
         }
      const char* ddml()
         {
		 if (TypeString == "")
			 {
			 TypeString = Dummy->ddml();
			 TypeString = TypeString.substr(0, TypeString.length()-2) + " unit=\"" + Units + "\"/>";
			 }
 		 return TypeString.c_str();
         }
      void unpack(Message& message) 
		{
		Variable = InternalT(message).value();
		}
      virtual void invokeEvent(Message& message) {throw new Exception("Cannot unpack a calcFunction");}
   };


};	
		