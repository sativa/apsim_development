#if !defined (__gobjs_h)
#define __gobjs_h

#if defined (BUILDING_GOBJS)
   #define GOBJS_CLASS_EXPORT _export
   #define GOBJS_FUNCTION_EXPORT _export
#elif defined (NO_GOBJS_EXPORT)
   #define GOBJS_CLASS_EXPORT
   #define GOBJS_FUNCTION_EXPORT
#else
   #define GOBJS_CLASS_EXPORT _import
   #define GOBJS_FUNCTION_EXPORT
#endif

#endif
