#pragma once

#include "Message.h"
#include "Utility.h"
#include "Interfaces.h"
#include <string>

// -----------------------------------------------
// Integer packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, System::Int32 value) 
	{
	*(int*)message.dataStream() = value; 
	message.addToDataSize(4);	
	message.advanceDataStream(4);
	}
inline void unpack(Message& message,  System::Int32& value) 
	{
	value = *(int*)message.dataStream(); 
	message.advanceDataStream(4);
	}

inline const char* ddml(System::Int32& value)
	{
	return "<type kind=\"integer4\"/>";
	}
	
// -----------------------------------------------
// Real packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, System::Single value) 
	{
	*(float*)message.dataStream() = value;
	message.addToDataSize(4);	
	message.advanceDataStream(4);
	}
inline void unpack(Message& message,  System::Single& value) 
	{
	value = (float) *message.dataStream(); 	
	message.advanceDataStream(4);
	}
inline const char* ddml(System::Single& value)
	{
	return "<type kind=\"single\"/>";
	}

// -----------------------------------------------
// Double packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, System::Double value) 
	{
	*(double*)message.dataStream() = value;
	message.addToDataSize(8);	
	message.advanceDataStream(8);
	}
inline void unpack(Message& message, System::Double& value) 
	{
	value = (double) *message.dataStream(); 	
	message.advanceDataStream(8);
	}
inline const char* ddml(System::Double& value)
	{
	return "<type kind=\"double\"/>";
	}

// -----------------------------------------------
// Boolean packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, System::Boolean value) 
	{
	*(bool*)message.dataStream() = value; 
	message.addToDataSize(1);	
	message.advanceDataStream(1);
	}
inline void unpack(Message& message, System::Boolean& value) 
	{
	value = *(bool*)message.dataStream(); 
	message.advanceDataStream(1);
	}
// -----------------------------------------------
// String packing and unpacking routines.	
// -----------------------------------------------
inline void pack(Message& message, System::String* value) 
	{
	pack(message, value->Length);
	std::string st = stringToStdString(value);
	strcpy(message.dataStream(), st.c_str()); 
	message.addToDataSize(st.length());
	message.advanceDataStream(st.length());
	}
inline void unpack(Message& message,  System::String*& value) 
	{
	int length;
	unpack(message, length);
	
	char* buffer = new char[length+1];
	strncpy(buffer, message.dataStream(), length);
	buffer[length] = 0; 
	value = new System::String(buffer);
	message.advanceDataStream(length);
	delete [] buffer;
	}
inline const char* ddml(System::String* value)
	{
	return "<type kind=\"string\"/>";
	}
	
// -----------------------------------------------
// String packing and unpacking routines.	
// -----------------------------------------------
inline void pack(Message& message, const std::string& value) 
	{
	pack(message, (int)value.length());
	strcpy(message.dataStream(), value.c_str()); 
	message.addToDataSize(value.length());
	message.advanceDataStream(value.length());
	}
inline void unpack(Message& message,  std::string& value) 
	{
	int length;
	unpack(message, length);
	value.assign(message.dataStream(), length);
	message.advanceDataStream(length);
	}
		
	
//-------------------- Integer type
template <class T>
class Wrap : public IUnmanagedData
   {
   public:
      T __gc & value;
      Wrap(T __gc& data) : value(data) { }
      void pack(Message& message)
         {
         ::pack(message, value);
         }
      void unpack(Message& message)
         {
         ::unpack(message, value);
         }
      const char* ddml()
         {
         return ::ddml(value);
         }
   };
class WrapString : public IUnmanagedData
   {
   public:
      gcroot<String*> value;
      WrapString(String* data) {value = data;}
      void pack(Message& message)
         {
         ::pack(message, value);
         }
      void unpack(Message& message)
         {
         String* newString;
         ::unpack(message, newString);
         value = newString;
         }
      const char* ddml()
         {
         return ::ddml(value);
         }
   };   
public __gc class WrapperForUnmanaged : public IData
	{
	public:
		IUnmanagedData* data;
		WrapperForUnmanaged(IUnmanagedData* d) : data(d) { }
		void pack(Message& message)
			{
			data->pack(message);
			}
		void unpack(Message& message)
			{
			data->unpack(message);
			}
		const char* ddml()
			{
			return data->ddml();
			}
	
	};

	
		