extern "C" ApsimComponentData* __stdcall newApsimComponentData(const char* xml,
                                                    unsigned xmlLength);
extern "C" void __stdcall deleteApsimComponentData(ApsimComponentData* componentData);

extern "C" bool __stdcall ApsimComponentData_getProperty
   (ApsimComponentData* componentData,
    const char* name,
    char* value,
    unsigned nameLength,
    unsigned valueLength);
