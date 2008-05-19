#ifndef FORTRANComponentWrapperH
#define FORTRANComponentWrapperH

#include <ComponentInterface/ApsimVariant.h>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

static const char* nullType = "<type/>";
static const char* integerType = "<type kind=\"integer4\"/>";
static const char* integerArrayType = "<type kind=\"integer4\" array=\"T\"/>";
static const char* realType = "<type kind=\"single\"/>";
static const char* realArrayType = "<type kind=\"single\" array=\"T\"/>";
static const char* doubleType = "<type kind=\"double\"/>";
static const char* doubleArrayType = "<type kind=\"double\" array=\"T\"/>";
static const char* stringType = "<type kind=\"string\"/>";
static const char* stringArrayType = "<type kind=\"string\" array=\"T\"/>";
static const char* logicalType = "<type kind=\"boolean\"/>";
namespace protocol {
  std::string EXPORT DDML(const protocol::vector<int>& )
     { return(integerArrayType); }
  std::string EXPORT DDML(const protocol::vector<float>& )
     { return(realArrayType); }
  std::string EXPORT DDML(const protocol::vector<double>& )
     { return(doubleArrayType); }
  std::string EXPORT DDML(const FString& ) 
     { return(stringType); }
  std::string EXPORT DDML(const protocol::vector<FString>& ) 
     { return(stringArrayType); }
  std::string EXPORT DDML(const FStrings& ) 
     { return(stringArrayType); }
  std::string EXPORT DDML(const ApsimVariant& ) 
     { return("<type kind=\"Variant\"/>"); }
}
typedef EXPORT STDCALL void (Main_t)(const char* action, const char* data, unsigned actionLength, unsigned dataLength);
typedef EXPORT STDCALL void (alloc_dealloc_instance_t)(const unsigned int* doAllocate);
typedef EXPORT STDCALL void (do_init1_t)();
//typedef EXPORT STDCALL void (do_init2_t)(void);
//typedef EXPORT STDCALL void (do_commence_t)(void);
//typedef EXPORT STDCALL void (notify_termination_t)(void);
//typedef EXPORT STDCALL void (respondToGet_t)(unsigned int& fromID, protocol::QueryValueData* queryData);
//typedef EXPORT STDCALL unsigned (respondToSet_t)(unsigned int& fromID, unsigned int& variableID, protocol::Variant** variant);
typedef EXPORT STDCALL void (respondToEvent_t)(unsigned int& fromID, unsigned int& eventID, protocol::Variant* variant);
//typedef EXPORT STDCALL void (respondToMethod_t)(unsigned int& fromID, unsigned int& methodID, protocol::Variant** variant);

// Declarations from the FORTRAN side.
struct Instance
   {
   const char* id;
   unsigned int idSize;
   const char* g;
   unsigned int gSize;
   const char* p;
   unsigned int pSize;
   const char* c;
   unsigned int cSize;
   unsigned int dummy1;
   unsigned int dummy2;
   unsigned int dummy3;
   unsigned int dummy4;
   unsigned int dummy5;
   unsigned int dummy6;
   unsigned int dummy7;
   unsigned int dummy8;
   unsigned int dummy9;
   unsigned int dummy10;
   };

typedef EXPORT STDCALL void (getInstance_t) (Instance **);

class FortranWrapper : public protocol::Component
   {
   public:
      FortranWrapper(void);
      ~FortranWrapper(void);
      void send_message(protocol::Message* message)
         {
         sendMessage(message);
         }

      void get_name(char* n, unsigned nLength)
         {FString(n, nLength, FORString) = getName().c_str();}
      unsigned get_componentID(void) {return componentID;}
      unsigned get_parentID(void)    {return parentID;}
      unsigned get_componentData(void) {return (unsigned)componentData;}

      bool get_set_variable_success(void)
         {
         return getSetVariableSuccess();
         }
      void set_variable_error(unsigned int regID)
         {
         setVariableError(regID);
         }
      static FortranWrapper* currentInstance;

      void message_unused(void)
         {
         messageWasUsed = false;
         }
      template <class T>
      void get_var(int destID,
                   const FString& variableName, const FString& dataTypeString,
                   T& value, unsigned& numvals, bool isOptional = false)
         {
         int regId = FortranWrapper::currentInstance->
                             addRegistration(::get,
                                             destID,
                                             asString(variableName), 
                                             asString(dataTypeString));

         protocol::Variant *variant;
         if (getVariable(regId, &variant, isOptional))
            {
            bool ok = variant->unpack(value);
            if (!ok)
               {
               char buffer[100];
               strcpy(buffer, "Cannot use array notation on a scalar variable.\n"
                              "VariableName:");
               strncat(buffer, variableName.f_str(), variableName.length());
               error(FString(buffer), true);
               numvals = 0;
               }
            else
               {
               fromID = variant->getFromId();
               numvals = 1;
               }
            }
         else
            {
            numvals = 0;
            }
         }
      template <class T>
      void get_vars(unsigned requestNo, const FString& variableName,
                    const FString& dataTypeString, T& value, unsigned& numvals)
         {
         if (requestNo == 1)
            {
            int regId = FortranWrapper::currentInstance->addRegistration
                                 (::get, -1, variableName, dataTypeString);
            FortranWrapper::currentInstance->getVariables(regId, &vars);
            } 
         if (vars != NULL && vars->size() > 0)
            {
            protocol::Variant* var = vars->getVariant(requestNo-1);
            if (var != NULL)
               {
               var->unpack(value);
               numvals = 1;
               fromID = var->getFromId();
               return;
               }
            }
         numvals = 0;
         }

      template <class T>
      void respond2var(const FString& variableName, const FString& units,
                       const FString& dataTypeString, const T& value)
         {
         if (inApsimGetQuery)
            {
            static char buffer[1000];
            strcpy(buffer, "");
            strncat(buffer, dataTypeString.f_str(), dataTypeString.length());
            unsigned insertPos = dataTypeString.find(">");
            if (insertPos != FString::npos && units.length() > 0)
               {
               insertPos--;
               buffer[insertPos] = 0;
               strcat(buffer, " unit=\"");
               strncat(buffer, units.f_str(), units.length());
               strcat(buffer, "\"/>");
               }
            addRegistration(::respondToGet, -1, variableName, buffer);
            }
         else
            sendVariable(queryData, value);
         }
      template <class T>
      void set_var(int destID, const FString& variableName,
                   const FString& dataTypeString, const T& value)
         {
         string regName;
         if (destID < 0) 
           ApsimRegistry::getApsimRegistry().unCrackPath(componentID, asString(variableName), destID, regName);
         else 
           regName = asString(variableName);
           
         unsigned variableID = addRegistration(::set,
                                               destID,
                                               regName,
                                               asString(dataTypeString));
         setVariable(variableID, value);
         }
      void new_postbox(void)
         {
         outgoingApsimVariant.reset();
         post_var("sender", protocol::DTstring, false, getName());
         post_var("sender_id", protocol::DTint4, false, componentID);
         }
      template <class T>
      void collect_var(const FString& variableName, protocol::DataTypeCode dataType,
                       bool isArray, T& value, unsigned& numvals, bool isOptional)
         {
         bool ok;
         if (inRespondToSet)
            {
            protocol::TypeConverter* converter = NULL;
            if (getTypeConverter(variableName,
                                 incomingVariant.getType().getCode(),
                                 dataType,
                                 incomingVariant.getType().isArray(),
                                 isArray,
                                 converter))
               {
               incomingVariant.setTypeConverter(converter);
               }
            incomingVariant.unpack(value);

            delete converter;
            ok = true;
            }
         else
            ok = incomingApsimVariant.get(variableName, dataType, isArray, value);
         if (!ok)
            {
            if(!isOptional)
               {
               char buffer[500];
               strcpy(buffer, "Cannot collect variable from postbox.  Variable doesn't exist.\n"
                              "Variable: ");
               strncat(buffer, variableName.f_str(), variableName.length());
               error(FString(buffer), true);
               }
            numvals = 0;
            }
         else
            numvals = 1;
         }
      template <class T>
      void post_var(const FString& variableName, protocol::DataTypeCode dataType,
                    bool isArray, const T& value)
         {
         outgoingApsimVariant.store(variableName, dataType, isArray, value);
         }

      void event_send(int id, const FString& eventName);

      unsigned getFromID(void)
         {
         return fromID;
         }

   protected:
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void doCommence(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& qData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& querySetData);
      virtual void notifyTermination(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& var);
      virtual void onApsimGetQuery(unsigned int fromID, protocol::ApsimGetQueryData& apsimGetQueryData);
      virtual bool onApsimSetQuery(protocol::ApsimSetQueryData& apsimSetQueryData);

   private:
      Instance *instance;    // Pointer into fortran dll (same for all instantiations via getProcAddress())
      Instance myInstance;   // Saved copy of one instance as retrieved from alloc_dealloc()
      bool messageWasUsed;
      bool inApsimGetQuery;
      bool inRespondToSet;
      protocol::QueryValueData queryData;
      protocol::ApsimVariant outgoingApsimVariant;
      protocol::ApsimVariant incomingApsimVariant;
      protocol::Variant incomingVariant;
      protocol::Variants* vars;
      unsigned fromID;

      void swapInstanceIn(void);

      void setup(void);
      void setupFortranDll(void);
      void setupInstancePointers(void);

      void *libraryHandle;
      Main_t *my_Main;
      alloc_dealloc_instance_t *my_alloc_dealloc_instance;
      do_init1_t *my_do_init1;
      //do_init2_t *my_do_init2;
      //do_commence_t *my_do_commence;
      //notify_termination_t *my_notify_termination;
      //respondToGet_t *my_respondToGet;
      //respondToSet_t *my_respondToSet;
      respondToEvent_t *my_respondToEvent;
      //respondToMethod_t *my_respondToMethod;

      void Main(const char* action, const char *data);
      void Main(const char* action, FString &data);
      void Main(FString &action, FString &data);
      void Main(FString &action, const char *data);
      void alloc_dealloc_instance(const unsigned int* doAllocate);
   };

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
