#ifndef FApsimComponentData_H
#define FApsimComponentData_H

class FString;

extern "C" ApsimComponentData* __stdcall newApsimComponentData(const char* xml,
                                                    unsigned xmlLength);
extern "C" void __stdcall deleteApsimComponentData(ApsimComponentData* componentData);

extern "C" bool __stdcall ApsimComponentData_getProperty
   (ApsimComponentData* componentData,
    const FString& propertyType,
    const FString& name,
    FString& value);
    
#endif
