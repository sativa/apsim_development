#define IMPLEMENT_OUR_STREAMABLE( cls )                             \
IMPLEMENT_STREAMABLE_CLASS( cls);                                   \
IMPLEMENT_STREAMER( cls );                                          \
IMPLEMENT_STREAMABLE_POINTER( cls )

