#pragma once
#include <string>

inline std::string stringToStdString(System::String^ st)
	{
	if (st == nullptr || st->Length == 0)
		return "";
	else
		{
		cli::array<System::Byte>^ utf8 = System::Text::Encoding::UTF8->GetBytes(st);
		pin_ptr<System::Byte> cString = &utf8[0];
		return std::string((const char*) cString);
		}
	}
