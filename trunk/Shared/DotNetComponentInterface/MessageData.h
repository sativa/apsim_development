#pragma once

#include "Message.h"
#include <string>

// -----------------------------------------------
// Integer packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, int value) 
	{
	*(int*)message.dataStream() = value; 
	message.addToDataSize(4);	
	message.advanceDataStream(4);
	}
inline void unpack(Message& message,  int& value) 
	{
	value = *(int*)message.dataStream(); 
	message.advanceDataStream(4);
	}

// -----------------------------------------------
// Real packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, float value) 
	{
	*(float*)message.dataStream() = value;
	message.addToDataSize(4);	
	message.advanceDataStream(4);
	}
inline void unpack(Message& message,  float& value) 
	{
	value = (float) *message.dataStream(); 	
	message.advanceDataStream(4);
	}

// -----------------------------------------------
// Double packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, double value) 
	{
	*(double*)message.dataStream() = value;
	message.addToDataSize(8);	
	message.advanceDataStream(8);
	}
inline void unpack(Message& message,  double& value) 
	{
	value = (double) *message.dataStream(); 	
	message.advanceDataStream(8);
	}

// -----------------------------------------------
// Boolean packing and unpacking routines.
// -----------------------------------------------
inline void pack(Message& message, bool value) 
	{
	*(bool*)message.dataStream() = value; 
	message.addToDataSize(1);	
	message.advanceDataStream(1);
	}
inline void unpack(Message& message,  bool& value) 
	{
	value = *(bool*)message.dataStream(); 
	message.advanceDataStream(1);
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
	message.advanceDataStream(value.length());
	}
inline void pack(Message& message, const char* value) 
	{
	::pack(message, std::string(value));
	}
	