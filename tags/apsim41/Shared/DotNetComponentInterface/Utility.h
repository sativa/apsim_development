#pragma once
#include <string>

inline std::string stringToStdString(System::String* st)
	{
	char utf8 __gc[] = System::Text::Encoding::UTF8->GetBytes(st);
	char __pin * cString = &utf8[0];
	return cString;
	}
