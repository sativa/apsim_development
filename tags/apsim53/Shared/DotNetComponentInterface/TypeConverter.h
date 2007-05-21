#pragma once
#include "Message.h"
#include "Interfaces.h"

namespace ComponentInterface {
class TypeConverter
	{
	public:
		void unpack(Message& message, const std::string& DataType, IData* data);
	
	private:
	
		void GetKindAndArray(const std::string& DataType, 
							std::string& Kind, bool& IsArray);
							
		void ThrowConvertError(const std::string& SourceKind, bool SourceIsArray, 
								const std::string& DestKind, bool DestIsArray);
							
	};
	
};