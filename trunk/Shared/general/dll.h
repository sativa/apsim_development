//---------------------------------------------------------------------------
#ifndef dllH
#define dllH

#include <string>
#include <stdexcept>
// ------------------------------------------------------------------
// Loads a dll into memory and returns a handle to it.
// ------------------------------------------------------------------
void *loadDLL(const std::string& filename) throw (std::runtime_error);

// ------------------------------------------------------------------
// Close the specified dll handle.
// ------------------------------------------------------------------
void closeDLL(void* handle);

// ------------------------------------------------------------------
// Return the address of a function in the dll specified by dllHandle.
// ------------------------------------------------------------------
void* dllProcAddress(void* dllHandle, const char* name);



#endif
