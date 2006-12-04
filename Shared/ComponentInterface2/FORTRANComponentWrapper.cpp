//---------------------------------------------------------------------------
// Wrapper dll for fortran routines.
// Keeps pointers to fortran entry points (Main(), do_init1() etc..
// and calls them when reqd.
#include <stdexcept>
#include <string>
#pragma warn -inl

#include <boost/lexical_cast.hpp>

#include <ComponentInterface2/CMPScienceAPI.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <general/platform.h>
#include <general/dll.h>
#include <map>

#include "FORTRANComponentWrapper.h"

FortranWrapper* FortranWrapper::currentInstance = NULL;

using namespace std;

// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL createInstance
   (const char* dllFileName,
    unsigned int* componentID,
    unsigned int* parentID,
    unsigned int* instanceNumber,
    unsigned int* callbackArg,
    CallbackType* callback)
   {
printf("createInstance called, dll=%s\n", dllFileName);
   // create a component interface and a science api that the component
   // will talk to.
   CMPComponentInterface *componentInterface = new CMPComponentInterface(callbackArg, callback, *componentID, *parentID);
   CMPScienceAPI *scienceAPI = new CMPScienceAPI(*componentInterface);
   void *dllHandle = loadDLL(dllFileName);
   FortranWrapper *component = new FortranWrapper(componentInterface, scienceAPI, dllHandle);

   // The instance number we return to the PM is a pointer to the component
   // object we just created.
   *instanceNumber = (unsigned) component;
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL deleteInstance (unsigned* instanceNumber)
   {
   FortranWrapper *component = (FortranWrapper*) *instanceNumber;
   delete component;
   }

// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   FortranWrapper *component = (FortranWrapper*) *instanceNumber;
   component->componentInterface->messageToLogic(*message);
   *processed = true; // ???? not sure why we need this.
   }

// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript,
                                                         char* description)
   {
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
FortranWrapper::FortranWrapper(CMPComponentInterface *componentinterface,
                               ScienceAPI* scienceapi, 
                               void *handle) : 
                                  scienceAPI(scienceapi),   
                                  componentInterface(componentinterface)
   {
printf("FortranWrapper::FortranWrapper called, handle=%x\n", handle);
   dllHandle = handle;

   // Instance pointers allow us to fiddle the fortran code "behind its back"..
   // 1. tell the fortran runtime to allocate instance pointers
   void STDCALL (*alloc_dealloc_instance) (const unsigned int* );
   alloc_dealloc_instance = (void STDCALL (*)(const unsigned int* )) dllProcAddress(dllHandle, "alloc_dealloc_instance");
   if (alloc_dealloc_instance)
      {
      const unsigned int doAllocate = true;
      (*alloc_dealloc_instance) (&doAllocate);
      }

   // Get a copy of them so we can swap them in&out later.
   void STDCALL (*getInstance) (Instance **);
   getInstance = (void STDCALL (*)(Instance**)) dllProcAddress(dllHandle, "getInstance");
   if (getInstance)
      {
      (*getInstance) (&instance);
      myInstance = *instance;
      }
   
   // We will want an init1 event when it comes..
   scienceAPI->subscribe("init1", nullFunction(&FortranWrapper::onInit1));
   }

// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
FortranWrapper::~FortranWrapper(void)
   {
   // get FORTRAN to release memory blocks.
   void STDCALL (*alloc_dealloc_instance) (const unsigned int* );
   alloc_dealloc_instance = (void STDCALL (*)(const unsigned int* )) dllProcAddress(dllHandle, "alloc_dealloc_instance");
   if (alloc_dealloc_instance)
      {
      const unsigned int doAllocate = false;
      (*alloc_dealloc_instance) (&doAllocate);
      }

   if (componentInterface) delete componentInterface;
   if (scienceAPI) delete scienceAPI;
   if (dllHandle) closeDLL(dllHandle);
   }

void FortranWrapper::onInit1(void)
   {
printf("FortranWrapper::onInit1 called, handle=%x\n", dllHandle);
   unsigned STDCALL (*initRoutine)(void);
   initRoutine = (unsigned STDCALL(*)(void)) dllProcAddress(dllHandle, "create");
   if (initRoutine) 
      {
      swapInstanceIn();
printf("calling init routine\n");
      initRoutine();
      }
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
// Our fortran module is subscribing to an event. Keep track of this instance
// so we can swap in instance pointer when needed
// ------------------------------------------------------------------
int FortranWrapper::subscribe(const std::string &name, void *address)
   {

   typedef boost::function1<void, void*> pfcall;
   pfcall p = boost::function1<void, void*>(
                 boost::bind(&FortranWrapper::subscribedEventHandler, this, address));

   scienceAPI->subscribe(name, nullFunction(p));
   return 1;
   }
void FortranWrapper::subscribedEventHandler(void *address)
   {
   swapInstanceIn();

   void STDCALL (*subscription) (void);
   subscription = (void STDCALL (*)(void)) address;

   (*subscription)();
   }

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// DLL Exports

extern "C" int EXPORT STDCALL subscribe(const char *name, void *address, int nameLen)
// ------------------------------------------------------------------
//     Called from FORTRAN to subscribe to an event 
// ------------------------------------------------------------------
   {
   return (FortranWrapper::currentInstance->subscribe(string(name, nameLen), address));
   }

// String converters
void s2f (char *dest, int maxdest, const string &src) 
   {
   memcpy(dest, src.c_str(), min(src.size(), maxdest));
   }
void f2s (string &dest, const char *src, int srcLen) 
   {
   dest = string(src, srcLen);
   }

extern "C" void EXPORT STDCALL fortran_error(const char* msg, unsigned int* isFatal,
                                      unsigned int msgLength)
// ------------------------------------------------------------------
//     Called from FORTRAN to issue an error
// ------------------------------------------------------------------
   {
   FortranWrapper::currentInstance->componentInterface->error(string(msg, msgLength), *isFatal);
   }

extern "C" void EXPORT STDCALL terminate_simulation(void)
// ------------------------------------------------------------------
//     Called from FORTRAN to terminate a simulation
// ------------------------------------------------------------------
   {
   //sendMessage(newTerminateSimulationMessage(componentID, parentID));
   //FortranWrapper::currentInstance->terminateSimulation();
   }

extern "C" void EXPORT STDCALL write_string(const char* msg, unsigned int msgLength)
// ------------------------------------------------------------------
//     Called from FORTRAN to write a string to the summary file
// ------------------------------------------------------------------
   {
   FortranWrapper::currentInstance->componentInterface->write(string(msg, msgLength));
   }

#if 0
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
#endif
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
#if 0
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
   (const char* variableName, const char* units, const protocol::timeType* value,
    unsigned variableNameLength, unsigned unitsLength)
   {
   string timeDDML = DDML(protocol::timeType());
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
#endif
// ------------------------------------------------------------------
// Module is reading a string from our "parameters".
// ------------------------------------------------------------------
extern "C" int EXPORT STDCALL read_real_raw
   (const char* parameterName, float* value, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }
extern "C" int EXPORT STDCALL read_double_raw
   (const char* parameterName, double* value, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }
extern "C" int EXPORT STDCALL read_integer_raw
   (const char* parameterName, int* value, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }
extern "C" int EXPORT STDCALL read_logical_raw
   (const char* parameterName, int* value, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }

extern "C" int EXPORT STDCALL read_string_raw
   (const char* parameterName, char* value, int* optional,
    unsigned parameterNameLength, unsigned valueLength)
   {
   memset((char*)value, ' ', valueLength);
   std::string name(parameterName, parameterNameLength);
   std::string sValue;

   if (FortranWrapper::currentInstance->scienceAPI->read(name, sValue, *optional)) 
      {
      s2f(value, valueLength, sValue.c_str());
      return true;
      }
   return false;
   }
extern "C" int EXPORT STDCALL read_real_array_raw
   (const char* parameterName, float* value, int *maxElem, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   std::vector<int> iValue;
   if (FortranWrapper::currentInstance->scienceAPI->read(name, iValue, *optional)) 
      {
      for (int i = 0; i < (int)iValue.size() && i < *maxElem; i++) 
         {
         value[i] = iValue[i];
         }
      return (min(iValue.size(), *maxElem));
      }
   return (0);
   }
extern "C" int EXPORT STDCALL read_double_array_raw
   (const char* parameterName, double* value, int *maxElem, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }
extern "C" int EXPORT STDCALL read_integer_array_raw
   (const char* parameterName, int* value, int *maxElem, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }
extern "C" int EXPORT STDCALL read_logical_array_raw
   (const char* parameterName, int* value, int *maxElem, int* optional,
    unsigned parameterNameLength)
   {
   std::string name(parameterName, parameterNameLength);
   return (FortranWrapper::currentInstance->scienceAPI->read(name, *value, *optional)); 
   }

extern "C" void EXPORT STDCALL get_real_raw
   (const char* variableName, const char* variableUnits, 
    float* value, float *lower, float *upper,
    int *optional, unsigned variableNameLength, unsigned unitsLength)
   {
   std::string name(variableName, variableNameLength);
   std::string units(variableUnits, unitsLength);
   FortranWrapper::currentInstance->scienceAPI->get(name, units, *optional, *value); 
   if (*value < *lower || *value > *upper)
       {
       std::string msg = string("Bound check warning while getting variable.\n"
                           "Variable  : ") + variableName + string("\n"
                           "Condition : ") + ftoa(*lower, 2) + string(" <= ") +
                            boost::lexical_cast<std::string>(value) + string(" <= ") + ftoa(*upper, 2);
       FortranWrapper::currentInstance->componentInterface->error(msg.c_str(), false);
       }
   }

extern "C" void EXPORT STDCALL get_char_raw
   (const char* variableName, const char* variableUnits, 
    char* value, 
    int *optional, 
    unsigned variableNameLength, unsigned unitsLength, unsigned valueLength)
   {
   std::string name(variableName, variableNameLength);
   std::string units(variableUnits, unitsLength);
   std::string sValue;
   FortranWrapper::currentInstance->scienceAPI->get(name, units, *optional, sValue); 
   }

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

