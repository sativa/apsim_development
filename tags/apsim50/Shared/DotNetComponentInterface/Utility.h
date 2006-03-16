#pragma once
#include <string>

inline std::string stringToStdString(System::String* st)
	{
	if (st == NULL || st->Length == 0)
		return "";
	else
		{
		char utf8 __gc[] = System::Text::Encoding::UTF8->GetBytes(st);
		char __pin * cString = &utf8[0];
		return cString;
		}
	}
