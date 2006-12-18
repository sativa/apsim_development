//---------------------------------------------------------------------------
// Wrapper dll for fortran routines.
// Keeps pointers to fortran entry points (Main(), do_init1() etc..
// and calls them when reqd.
#pragma hdrstop
#ifndef __WIN32__
   #include <dlfcn.h>
#else
   #include <windows.h>  // for LoadLibrary
#endif
#include <stdlib.h>
#include <stdio.h>

#include "Component.h"
#include "FORTRANComponentWrapper.h"
#include "Variants.h"
#include "datatypes.h"

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

static const int ERR_internal = 1;
static const int ERR_user = 2;

FortranWrapper* FortranWrapper::currentInstance = NULL;

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
FortranWrapper::FortranWrapper(void)
   : outgoingApsimVariant(this), incomingApsimVariant(this), queryData((unsigned)-1)
   {
   // nothing
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
FortranWrapper::~FortranWrapper(void)
   {
   // get FORTRAN to release memory blocks.
   const unsigned int doAllocate = false;
   const char *dlError;
   swapInstanceIn();
   alloc_dealloc_instance(&doAllocate);

   if (libraryHandle)
   {
#ifdef __WIN32__
      FreeLibrary(libraryHandle);
#else
      int return_code;
      return_code = dlclose(libraryHandle);
      if ( return_code ) {
	dlError = dlerror();
	throw runtime_error(dlError);
      }
#endif
   }
   }

void FortranWrapper::setup(void)
   {
   setupFortranDll();

   // get FORTRAN to create new memory blocks.
   const unsigned int doAllocate = true;
   alloc_dealloc_instance(&doAllocate);

   // save pointers and contents of these memory blocks.
   setupInstancePointers();
   }
// ------------------------------------------------------------------
// Find entry points. Leave NULL if not found..
// ------------------------------------------------------------------
void FortranWrapper::setupFortranDll(void)
   {
   my_Main = NULL;
   my_alloc_dealloc_instance = NULL;
   my_respondToEvent = NULL;
   const char *dlError = NULL;

#ifdef __WIN32__
   libraryHandle = LoadLibrary(this->dllName);
#else
   libraryHandle = dlopen(this->dllName, RTLD_NOW | RTLD_LOCAL);
   dlError = dlerror();
#endif

   if (libraryHandle != NULL || dlError)
      {
#ifdef __WIN32__
      my_Main = (Main_t*) GetProcAddress(libraryHandle, "Main");
      my_alloc_dealloc_instance = (alloc_dealloc_instance_t*) GetProcAddress(libraryHandle, "alloc_dealloc_instance");
      my_do_init1 = (do_init1_t*) GetProcAddress(libraryHandle, "doInit1");
      //my_do_init2 = GetProcAddress(handle, "");
      //my_do_commence = GetProcAddress(handle, "");
      //my_notify_termination = GetProcAddress(handle, "");
      //my_respondToGet = GetProcAddress(handle, "");
      //my_respondToSet = GetProcAddress(handle, "");
      my_respondToEvent = (respondToEvent_t*) GetProcAddress(libraryHandle, "respondToEvent");
      //my_respondToMethod = GetProcAddress(handle, "");
#else
      my_Main = (Main_t*) dlsym(libraryHandle, "Main");
      my_alloc_dealloc_instance = (alloc_dealloc_instance_t*) dlsym(libraryHandle, "alloc_dealloc_instance");
      my_do_init1 = (do_init1_t*) dlsym(libraryHandle, "doInit1");
      my_respondToEvent = (respondToEvent_t*) dlsym(libraryHandle, "respondToEvent");
#endif
      }
   }
// ------------------------------------------------------------------
// Find pointers to fortran common block, and keep a copy.
// ------------------------------------------------------------------
void FortranWrapper::setupInstancePointers(void)
   {
   instance = NULL;
   memset(&myInstance, 0, sizeof(Instance));

   if (libraryHandle != NULL)
      {
#ifdef __WIN32__
      getInstance_t *proc = (getInstance_t *) GetProcAddress(libraryHandle, "getInstance");
#else
      getInstance_t *proc = (getInstance_t *) dlsym(libraryHandle, "getInstance");
#endif
      if (proc != NULL)
         {
         (*proc) (&instance);
         myInstance = *instance;
         }
      else
         {
         throw "Missing getInstance()";
         }
      }
   }

void FortranWrapper::Main(const char* action, const char *data)
   {
   if (my_Main) {(*my_Main) (action, data, strlen(action), strlen(data));}
   }
void FortranWrapper::Main(const char* action, FString &data)
   {
   if (my_Main) {(*my_Main) (action, data.f_str(), strlen(action), data.length());}
   }
void FortranWrapper::Main(FString &action, FString &data)
   {
   if (my_Main) {(*my_Main) (action.f_str(), data.f_str(), action.length(), data.length());}
   }
void FortranWrapper::Main(FString &action, const char *data)
   {
   if (my_Main) {(*my_Main) (action.f_str(), data, action.length(), strlen(data));}
   }

void FortranWrapper::alloc_dealloc_instance(const unsigned int* doAllocate)
   {
   if (my_alloc_dealloc_instance) {(*my_alloc_dealloc_instance) (doAllocate);}
   }

// ------------------------------------------------------------------
// do init1 stuff
// ------------------------------------------------------------------
void FortranWrapper::doInit1(const FString& sdml)
   {
   // Set up the dll pointers
   setup();

   protocol::Component::doInit1(sdml);
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   if (my_do_init1)
      (*my_do_init1)();
   else
      Main("create", "");

   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// do init2 stuff
// ------------------------------------------------------------------
void FortranWrapper::doInit2(void)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   Main("init2", "");

   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// do commence stuff
// ------------------------------------------------------------------
void FortranWrapper::doCommence(void)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   Main("start", "");

   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// respond to a get request.
// ------------------------------------------------------------------
void FortranWrapper::respondToGet(unsigned int& fromID, protocol::QueryValueData& qData)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   FString name = getRegistrationName(qData.ID);
   inApsimGetQuery = false;
   queryData = qData;
   Main("get", name);

   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// respond to a set request.
// ------------------------------------------------------------------
bool FortranWrapper::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& querySetData)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   FString name = getRegistrationName(querySetData.ID);

   incomingVariant.aliasTo(querySetData.variant);
   inRespondToSet = true;
   messageWasUsed = true;

   Main("set", name);

   *instance = saved;
   currentInstance = savedThis;

   return messageWasUsed;
   }
// ------------------------------------------------------------------
// respond to a notification of termination
// ------------------------------------------------------------------
void FortranWrapper::notifyTermination(void)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   Main("end_run", "");

   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// respond to an event.
// ------------------------------------------------------------------
void FortranWrapper::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& var)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   FString event = getRegistrationName(eventID);
   incomingApsimVariant.aliasTo(var.getMessageData());
   inRespondToSet = false;

   messageWasUsed = true;
   char cevent[80];

   Main(event, " ");
   if (!messageWasUsed)
      if (my_respondToEvent) {(*my_respondToEvent)(fromID, eventID, &var);}


   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// respond to an onApsimGetQuery message
// ------------------------------------------------------------------
void FortranWrapper::onApsimGetQuery(protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   inApsimGetQuery = true;
   Main("get", apsimGetQueryData.name);

   *instance = saved;
   currentInstance = savedThis;
   }
// ------------------------------------------------------------------
// respond to an onApsimSetQuery message
// ------------------------------------------------------------------
bool FortranWrapper::onApsimSetQuery(protocol::ApsimSetQueryData& apsimSetQueryData)
   {
   Instance saved = *instance;
   FortranWrapper* savedThis = currentInstance;
   swapInstanceIn();

   incomingVariant.aliasTo(apsimSetQueryData.variant);
   messageWasUsed = true;
   inRespondToSet = true;

   Main("set", apsimSetQueryData.name);

   *instance = saved;
   currentInstance = savedThis;

   return messageWasUsed;
   }
// ------------------------------------------------------------------
// swap an instance in.
// ------------------------------------------------------------------
void FortranWrapper::swapInstanceIn(void)
   {
   *instance = myInstance;
   currentInstance = this;
   }

// ------------------------------------------------------------------
//  Short description:
//     Create an instance of our FORTRAN component.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new FortranWrapper;
   }

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// DLL Exports
// ------------------------------------------------------------------

// ------------------------------------------------------------------
//  Short description:
//    add a registration to the system.  Return it's registration ID
//    to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL add_registration
   (RegistrationType* kind, const char* name, const char* type,
    const char* alias, const char* componentNameOrID,
    unsigned nameLength, unsigned typeLength, unsigned aliasLength,
    unsigned componentNameOrIDLength)
   {
   return FortranWrapper::currentInstance->addRegistration
      (*kind, FString(name, nameLength, FORString),
       protocol::Type(FString(type, typeLength, FORString)),
       FString(alias, aliasLength, FORString),
       FString(componentNameOrID, componentNameOrIDLength, FORString));
   }

string addUnitsToDDML(const string& ddml, const string& units)
   {
   string returnString = ddml;
   unsigned pos = returnString.find("/>");
   if (pos != string::npos)
      returnString = returnString.substr(0, pos) + " unit=\"" + units + "\"/>";
   return returnString;
   }


// ------------------------------------------------------------------
//  Short description:
//    add a registration to the system.  Return it's registration ID
//    to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL add_registration_with_units
   (RegistrationType* kind, const char* name, const char* type,
    const char* units,
    unsigned nameLength, unsigned typeLength, unsigned unitsLength)
   {
   string ddml = addUnitsToDDML(asString(FString(type, typeLength, FORString)),
                                asString(FString(units, unitsLength, FORString)));
   return FortranWrapper::currentInstance->addRegistration
      (*kind, FString(name, nameLength, FORString),
       protocol::Type(FString(ddml.c_str(), ddml.length(), CString)),
       FString(""),
       FString(""));
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to issue an error

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL fortran_error(const char* msg, unsigned int* isFatal,
                                      unsigned int msgLength)
   {
   FortranWrapper::currentInstance->error
      (FString(msg, msgLength, FORString), *isFatal);
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to terminate a simulation

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL terminate_simulation(void)
   {
   FortranWrapper::currentInstance->terminateSimulation();
   }

// ------------------------------------------------------------------
//  Short description:
//    get the value(s) of a variable from the system.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL get_variables
   (unsigned int* registrationID, protocol::Variants** values)
   {
   return FortranWrapper::currentInstance->getVariables
      (*registrationID, *values);
   }

// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to write a string to the summary file

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL write_string(const char* msg, unsigned int msgLength)
   {
   FortranWrapper::currentInstance->writeString(FString(msg, msgLength, FORString));
   }

// ------------------------------------------------------------------
//  Short description:
//   Send a message to system

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL send_message(protocol::Message* message)
   {
   FortranWrapper::currentInstance->send_message(message);
   }

// ------------------------------------------------------------------
//  Short description:
//    return the componentID to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL get_componentID(void)
   {
   return FortranWrapper::currentInstance->get_componentID();
   }

// ------------------------------------------------------------------
//  Short description:
//    return the parentID to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL get_parentID(void)
   {
   return FortranWrapper::currentInstance->get_parentID();
   }

// ------------------------------------------------------------------
//  Short description:
//    return the componentdata object to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  EXPORT STDCALL get_componentData(void)
   {
   return FortranWrapper::currentInstance->get_componentData();
   }

// ------------------------------------------------------------------
//  Short description:
//    return the component's name to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void  EXPORT STDCALL get_name(char* name, unsigned nameLength)
   {
   FortranWrapper::currentInstance->get_name(name, nameLength);
   }

// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to push a routine onto stack

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL push_routine(const char* routineName,
                                       unsigned int routineNameLength)
   {
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to pop a routine from the stack

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL pop_routine(const char* routineName,
                                       unsigned int routineNameLength)
   {

   }

// ------------------------------------------------------------------
//  Short description:
//    Perform a case insensitive string comparison and return true
//    if strings are equal.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" bool EXPORT STDCALL strings_equal(const char* st1, const char* st2,
                                        unsigned st1Length, unsigned st2Length)
   {
   return (FString(st1, st1Length, FORString) == FString(st2, st2Length, FORString));
   }

// ------------------------------------------------------------------
//  Short description:
//    return a type string for the specified registration ID.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_registration_type_string
   (unsigned int* variableID, char* typeString, unsigned typeStringLength)
   {
   FString F(typeString, typeStringLength, FORString);
   FortranWrapper::currentInstance->get_registration_type_string
      (*variableID, F);
   }

// ------------------------------------------------------------------
//  Short description:
//    return a type string for the specified registration ID.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_registration_name
   (unsigned int* variableID, char* nameString, unsigned nameStringLength)
   {
   FString F(nameString, nameStringLength, FORString);
   FortranWrapper::currentInstance->get_registration_name
      (*variableID, F);
   }

// ------------------------------------------------------------------
//  Short description:
//    return a type string for the specified registration ID.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL get_setVariableSuccess(void)
   {
   return FortranWrapper::currentInstance->get_set_variable_success();
   }
// ------------------------------------------------------------------
//  Short description:
//    display a setvariable error message.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_variable_error(unsigned int* regID)
   {
   FortranWrapper::currentInstance->set_variable_error(*regID);
   }
// ------------------------------------------------------------------
//  Short description:
//     Called from FORTRAN to indicate a message was not used.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL message_unused(void)
   {
   FortranWrapper::currentInstance->message_unused();
   }

// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_integer_var
   (const char* variableName, const char* units, const int* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), integerType, *value);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_integer_array
   (const char* variableName, const char* units, int* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<int> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), integerArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_real_var
   (const char* variableName, const char* units, float* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), realType, *value);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_real_array
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), realArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_double_var
   (const char* variableName, const char* units, double* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), doubleType, *value);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_double_array
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<double> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), doubleArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_logical_var
   (const char* variableName, const char* units, int* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   bool b = *value;
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), logicalType, b);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_char_var
   (const char* variableName, const char* units, const char* value,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), stringType, FString(value, valueLength, FORString));
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_char_array
   (const char* variableName, const char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FStrings values(value, valueLength, *numvals, *numvals);
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), stringArrayType, values);
   }
// ------------------------------------------------------------------
// Module is returning the value of a variable to the system.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL respond2get_time_var
   (const char* variableName, const char* units, const protocol::TimeType* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   string timeDDML = DDML(protocol::TimeType());
   FortranWrapper::currentInstance->respond2var
      (FString(variableName, variableNameLength, FORString),
       FString(units, unitsLength, FORString), timeDDML.c_str(), *value);
   }
// ------------------------------------------------------------------
// Bound check a real variable and produce an error if out of bound.
// ------------------------------------------------------------------
void boundCheckVar(double value, double lower, double upper,
                   const FString& variableName)
   {
   if (value < lower || value > upper)
      {
      char varName[100];
      strncpy(varName, variableName.f_str(), variableName.length());
      varName[variableName.length()] = 0;

      char msg[500];
      sprintf(msg, "Value of variable is out of bounds.\n"
                   "Variable: %s\n"
                   "Value: %16.7f\n"
                   "Bounds: %16.7f to %16.7f",
              varName, value, lower, upper);
      FortranWrapper::currentInstance->error(msg, false);
      }
   }
// ------------------------------------------------------------------
// Bound check a real variable and produce an error if out of bound.
// ------------------------------------------------------------------
void boundCheckRealArray(float* values, unsigned numvals, float lower, float upper,
                         const FString& variableName)
   {
   for (unsigned i = 0; i != numvals; i++)
      boundCheckVar(values[i], lower, upper, variableName);
   }
// ------------------------------------------------------------------
// Bound check a double variable and produce an error if out of bound.
// ------------------------------------------------------------------
void boundCheckDoubleArray(double* values, unsigned numvals, double lower, double upper,
                           const FString& variableName)
   {
   for (unsigned i = 0; i != numvals; i++)
      boundCheckVar(values[i], lower, upper, variableName);
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_integer_var
   (unsigned* componentID, const char* variableName, const char* units,
    int* value, unsigned* numvals, int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), integerType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_integer_vars
   (unsigned* requestNo, const char* variableName, const char* units,
    int* value, unsigned* numvals, int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), integerType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }

// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_var
   (unsigned* componentID, const char* variableName, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_array
   (unsigned* componentID, const char* variableName, unsigned* arraySize, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realArrayType, values, *numvals);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckRealArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_array_optional
   (unsigned* componentID, const char* variableName, unsigned* arraySize, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realArrayType, values, *numvals, true);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckRealArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_arrays
   (unsigned* requestNo, const char* variableName, unsigned* arraySize, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*requestNo, FString(variableName, variableNameLength, FORString), realArrayType, values, *numvals);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckRealArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_var_optional
   (unsigned* componentID, const char* variableName, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), realType, *value, *numvals, true);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_real_vars
   (unsigned* requestNo, const char* variableName, const char* units,
    float* value, unsigned* numvals, float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), realType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_var
   (unsigned* componentID, const char* variableName, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_var_optional
   (unsigned* componentID, const char* variableName, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleType, *value, *numvals, true);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_vars
   (unsigned* requestNo, const char* variableName, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), doubleType, *value, *numvals);
   if (*numvals == 0)
      *value = 0;
   else
      boundCheckVar(*value, *lower, *upper, FString(variableName, variableNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_double_array
   (unsigned* componentID, const char* variableName, unsigned* arraySize, const char* units,
    double* value, unsigned* numvals, double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<double> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleArrayType, values, *numvals);
   if (*numvals == 0)
      {
      for (unsigned i = 0; i != *arraySize; i++)
         value[i] = 0.0;
      }
   else
      {
      *numvals = values.size();
      boundCheckDoubleArray(value, *numvals, *lower, *upper, FString(variableName, variableNameLength, FORString));
      }
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_char_var
   (unsigned* componentID, const char* variableName, const char* units,
    char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength,
    unsigned valueLength)
   {
   FString valueString;
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), stringType,
       valueString, *numvals);
   FString(value, valueLength, FORString) = valueString;
   if (*numvals == 0)
      memset((char*)value, ' ', valueLength);
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_char_var_optional
   (unsigned* componentID, const char* variableName, const char* units,
    char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString valueString(value, valueLength, FORString);
   FortranWrapper::currentInstance->get_var
      (*componentID, FString(variableName, variableNameLength, FORString), stringType,
       valueString, *numvals, true);
   FString(value, valueLength, FORString) = valueString;
   if (*numvals == 0)
      memset((char*)value, ' ', valueLength);
   }
// ------------------------------------------------------------------
// Module is requesting the value of a variable from another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL get_char_vars
   (unsigned* requestNo, const char* variableName, const char* units,
    char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString valueString;
   FortranWrapper::currentInstance->get_vars
      (*requestNo, FString(variableName, variableNameLength, FORString), stringType,
       valueString, *numvals);
   FString(value, valueLength, FORString) = valueString;
   if (*numvals == 0)
      memset((char*)value, ' ', valueLength);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_real_var(unsigned* componentID, const char* variableName,
                                       const char* units, const float* value,
                                       unsigned variableNameLength,
                                       unsigned unitsLength )
   {
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), realType,
       *value);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_real_array(unsigned* componentID, const char* variableName,
                                         const char* units, float* value, unsigned* numvals,
                                         unsigned variableNameLength,
                                         unsigned unitsLength)
   {
   protocol::vector<float> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), realArrayType, values);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_double_var(unsigned* componentID, const char* variableName,
                                         const char* units, const double* value,
                                         unsigned variableNameLength,
                                         unsigned unitsLength )
   {
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleType,
       *value);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_double_array(unsigned* componentID, const char* variableName,
                                           const char* units, double* value, unsigned* numvals,
                                           unsigned variableNameLength,
                                           unsigned unitsLength)
   {
   protocol::vector<double> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), doubleArrayType, values);
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_char_var(unsigned* componentID, const char* variableName,
                                       const char* units, const char* value,
                                       unsigned variableNameLength,
                                       unsigned unitsLength, unsigned valueLength)
   {
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), stringType,
       FString(value, valueLength, FORString));
   }
// ------------------------------------------------------------------
// Module wants to set the value of a variable in another module.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL set_char_array(unsigned* componentID, const char* variableName,
                                         const char* units, char* value, unsigned* numvals,
                                         unsigned variableNameLength, unsigned unitsLength,
                                         unsigned valueLength)
   {
   FStrings values(value, valueLength, *numvals, *numvals);
   FortranWrapper::currentInstance->set_var
      (*componentID, FString(variableName, variableNameLength, FORString), stringArrayType, values);
   }
// ------------------------------------------------------------------
// Module is requesting a new postbox.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL new_postbox( )
   {
   FortranWrapper::currentInstance->new_postbox();
   }
// ------------------------------------------------------------------
// Module is requesting to delete the postbox.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL delete_postbox( )
   {
   }
// ------------------------------------------------------------------
// Module wants to send an event.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL event_send(const char* eventName, unsigned eventNameLength)
   {
   FortranWrapper::currentInstance->event_send(FString(eventName, eventNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module wants to send an event
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL event_send_directed(const char* moduleName, const char* eventName,
                                      unsigned moduleNameLength, unsigned eventNameLength)
   {
   FortranWrapper::currentInstance->event_send
      (FString(eventName, eventNameLength, FORString), FString(moduleName, moduleNameLength, FORString));
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_integer_var
   (const char* variableName, const char* units, int* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTint4, false, *value);
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_real_var
   (const char* variableName, const char* units, float* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTsingle, false, *value);
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_real_array
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<float> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTsingle, true, values);
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_double_var
   (const char* variableName, const char* units, double* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTdouble, false, *value);
   }
// ------------------------------------------------------------------
// Module is posting a value into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_double_array
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength)
   {
   protocol::vector<double> values(value, *numvals, *numvals);
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTdouble, true, values);
   }
// ------------------------------------------------------------------
// Module is posting a string into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_char_var
   (const char* variableName, const char* units, const char* value,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTstring, false,
       FString(value, valueLength, FORString));
   }
// ------------------------------------------------------------------
// Module is posting a string array into a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL post_char_array
   (const char* variableName, const char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FStrings values(value, valueLength, *numvals, *numvals);
   FortranWrapper::currentInstance->post_var
      (FString(variableName, variableNameLength, FORString), protocol::DTstring, true,
       values);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_var
   (const char* variableName, const char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString newValue;
   FortranWrapper::currentInstance->collect_var
      (FString(variableName, variableNameLength, FORString), protocol::DTstring, false,
       newValue, *numvals, false);
   FString(value, valueLength, FORString) = newValue;
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_var_optional
   (const char* variableName, const char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString newValue;
   FortranWrapper::currentInstance->collect_var
      (FString(variableName, variableNameLength, FORString), protocol::DTstring, false,
       newValue, *numvals, true);
   FString(value, valueLength, FORString) = newValue;
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_array
   (const char* variableName, unsigned* arraySize, char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FStrings values(value, valueLength, *arraySize, 0);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTstring, true, values, *numvals, false);
   *numvals = values.getNumElements();
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_char_array_optional
   (const char* variableName, unsigned* arraySize, char* units, char* value, unsigned* numvals,
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FStrings values(value, valueLength, *arraySize, 0);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTstring, true, values, *numvals, true);
   *numvals = values.getNumElements();
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_var
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, false, *value, *numvals, false);
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_var_optional
   (const char* variableName, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, false, *value, *numvals, true);
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_array
   (const char* variableName, unsigned* arraySize, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, true, values, *numvals, false);
   *numvals = values.size();
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_real_array_optional
   (const char* variableName, unsigned* arraySize, const char* units, float* value, unsigned* numvals,
    float* lower, float* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   protocol::vector<float> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTsingle, true, values, *numvals, true);
   *numvals = values.size();
   boundCheckRealArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_integer_var
   (const char* variableName, const char* units, int* value, unsigned* numvals,
    int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTint4, false, *value, *numvals, false);
   boundCheckVar(*value, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_integer_var_optional
   (const char* variableName, const char* units, int* value, unsigned* numvals,
    int* lower, int* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTint4, false, *value, *numvals, true);
   boundCheckVar(*value, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_double_var
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTdouble, false, *value, *numvals, false);
   boundCheckVar(*value, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_double_var_optional
   (const char* variableName, const char* units, double* value, unsigned* numvals,
    double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTdouble, false, *value, *numvals, true);
   boundCheckDoubleArray(value, *numvals, *lower, *upper, name);
   }
// ------------------------------------------------------------------
// Module is getting a value from a variant.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL collect_double_array
   (const char* variableName, unsigned* arraySize, const char* units, double* value, unsigned* numvals,
    double* lower, double* upper,
    unsigned variableNameLength, unsigned unitsLength)
   {
   FString name(variableName, variableNameLength, FORString);
   protocol::vector<double> values(value, 0, *arraySize);
   FortranWrapper::currentInstance->collect_var
      (name, protocol::DTdouble, true, values, *numvals, false);
   *numvals = values.size();
   boundCheckDoubleArray(value, *numvals, *lower, *upper, name);
   }

// ------------------------------------------------------------------
// remove leading spaces
// ------------------------------------------------------------------
void stripBlanks(FString& str)
   {
   unsigned firstNonBlank = 0;
   while (str[firstNonBlank] == ' ')
      firstNonBlank++;

   unsigned lastNonBlank = str.length()-1;
   while (str[lastNonBlank] == ' ')
      lastNonBlank--;
   str = str.substr(firstNonBlank, lastNonBlank-firstNonBlank+1);
   }

// ------------------------------------------------------------------
// Store the specified string into the 'postbox'
// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL store_in_postbox(const char* str, unsigned strLength)
   {
   FString line(str, strLength, FORString);

   unsigned posStart = 0;

   while (posStart < line.length())
      {
      unsigned posComma = line.find(",", posStart);
      if (posComma == FString::npos)
         posComma = line.length();

      unsigned posEquals = line.find("=", posStart);
      if (posEquals != FString::npos)
         {
         FString name = line.substr(posStart, posEquals-posStart);
         stripBlanks(name);

         FString value = line.substr(posEquals+1, posComma-posEquals-1);
         unsigned posOpenBracket = value.find("(");
         if (posOpenBracket != FString::npos)
            {
            unsigned posCloseBracket = value.find(")", posOpenBracket);
            if (posCloseBracket != FString::npos)
               value.erase(posOpenBracket, posCloseBracket-posOpenBracket+1);
            }
         stripBlanks(value);

         post_char_var(name.f_str(), " ", value.f_str(),
                       name.length(), 1, value.length());
         }
      else
         return false;
      posStart = posComma+1;
      }
   return true;
   }
// ------------------------------------------------------------------
// return the posting module to caller.
// ------------------------------------------------------------------
extern "C" int EXPORT STDCALL get_posting_module(void)
   {
   return FortranWrapper::currentInstance->getFromID();
   }
// ------------------------------------------------------------------
// return the posting module to caller.
// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL component_id_to_name(unsigned* id, char* name,
                                                   unsigned nameLength)
   {
   FString nameString;
   bool ok = FortranWrapper::currentInstance->componentIDToName(*id, nameString);
   if (ok)
      FString(name, nameLength, FORString) = nameString;
   return ok;
   }
// ------------------------------------------------------------------
// return the posting module to caller.
// ------------------------------------------------------------------
extern "C" unsigned EXPORT STDCALL component_name_to_id(char* name, unsigned* id,
                                                   unsigned nameLength)
   {
   return FortranWrapper::currentInstance->componentNameToID
      (FString(name, nameLength, FORString), *id);
   }
// ------------------------------------------------------------------
// Module is reading a string from a file.
// ------------------------------------------------------------------
extern "C" int EXPORT STDCALL read_parameter
   (const char* parameterName, const char* sectionName, char* value, int* optional,
    unsigned parameterNameLength, unsigned sectionNameLength, unsigned valueLength)
   {
   FString fsect = FString(sectionName, sectionNameLength, FORString);
   FString fpar = FString(parameterName, parameterNameLength, FORString);
   FString fvar = FString(value, valueLength, FORString);
   bool found = FortranWrapper::currentInstance->readParameter(fsect, fpar, fvar, *optional);

   if (found)
      return true;

   else
      {
      memset((char*)value, ' ', valueLength);
      return false;
      }
   }
// ------------------------------------------------------------------
// Change the component order.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL change_component_order
   (char* moduleList, unsigned* numModules,
    unsigned moduleListLength)
   {
   FStrings names(moduleList, moduleListLength, *numModules, *numModules);
   FortranWrapper::currentInstance->changeComponentOrder(names);
   }

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl
