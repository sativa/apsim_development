#include "stdafx.h"
#include "TypeConverter.h"
#include "MessageData.h"
#include <vector>
using namespace ComponentInterface;
using namespace std;


template <class InternalT1, class ExternalT1, class InternalT2, class ExternalT2>
void ConvertArray(ApsimArray<InternalT1, ExternalT1>& SourceData, ApsimArray<InternalT2, ExternalT2>& DestData)
	{
	
	}

void TypeConverter::unpack(Message& message, const string& SourceDDML, IData* DestData)
	{
	bool SourceIsArray, DestIsArray;
	string SourceKind, DestKind;
	GetKindAndArray(SourceDDML, SourceKind, SourceIsArray);
	GetKindAndArray(DestData->ddml(), DestKind, DestIsArray);
		
	if (SourceKind == "" || (SourceKind == DestKind && SourceIsArray == DestIsArray))
		DestData->unpack(message);

	else if (DestIsArray)
		{
		if (!SourceIsArray)
			ThrowConvertError(SourceKind, SourceIsArray, DestKind, DestIsArray);
		if (SourceKind == "integer4")
			DestData->SetValue(&ApsimArray<ApsimInteger4, Int32>(message));
		else if (SourceKind == "single")
			DestData->SetValue(&ApsimArray<ApsimSingle, Single>(message));
		else if (SourceKind == "double")
			DestData->SetValue(&ApsimArray<ApsimDouble, Double>(message));
		else if (SourceKind == "string")
			DestData->SetValue(&ApsimArray<ApsimString, String*>(message));
		else if (SourceKind == "boolean")
			DestData->SetValue(&ApsimArray<ApsimBoolean, Boolean>(message));
		else 
			ThrowConvertError(SourceKind, SourceIsArray, DestKind, DestIsArray);
		}
	else
		{
		if (SourceIsArray)
			ThrowConvertError(SourceKind, SourceIsArray, DestKind, DestIsArray);
		else if (SourceKind == "integer4")
			DestData->SetValue(__box(ApsimInteger4(message).value()));
		else if (SourceKind == "single")
			DestData->SetValue(__box(ApsimSingle(message).value()));
		else if (SourceKind == "double")
			DestData->SetValue(__box(ApsimDouble(message).value()));
		else if (SourceKind == "string")
			DestData->SetValue(new String(ApsimString(message).value()));
		else if (SourceKind == "boolean")
			DestData->SetValue(__box(ApsimBoolean(message).value()));
		}
	}
	

void TypeConverter::GetKindAndArray(const std::string& DataType, 
									std::string& Kind, bool& IsArray)
	{	
	IsArray = (DataType.find("array=\"T\"") != string::npos);
	int PosKind = DataType.find("kind=\"");
	if (PosKind != string::npos)
		{				
		Kind = DataType.substr(PosKind + strlen("kind=\""));
		unsigned PosQuote = Kind.find('\"');
		if (PosQuote == string::npos)
			{
			string msg = "Invalid data type string: " + DataType;
			throw new Exception(msg.c_str());
			}
		Kind.erase(PosQuote);
		}
	}
	
void TypeConverter::ThrowConvertError(const std::string& SourceKind, bool SourceIsArray, 
										const std::string& DestKind, bool DestIsArray)
	{
	string SourceType = SourceKind;
	if (SourceIsArray)
		SourceType += " array";
	string DestType = DestKind;
	if (DestIsArray)
		DestType += " array";
		
	string msg = "Cannot convert from: " + SourceType + " to: " + DestType;
	throw new Exception(msg.c_str());
	
	}
										