#if defined (BUILDING_PROTOCOL)
   #define PROTOCOL_EXPORT _export
#else
   #define PROTOCOL_EXPORT _import
#endif
