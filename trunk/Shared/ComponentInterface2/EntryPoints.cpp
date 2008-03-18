#include <stdexcept>
#include <string>

#include <general/platform.h>
#include <general/dll.h>
#include <ComponentInterface2/CMPScienceAPI.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <map>

using namespace std;

unsigned CreateComponent(ScienceAPI* scienceAPI, CMPComponentInterface* componentInterface, const char* dllFileName, void* dllHandle);
void DeleteComponent(unsigned component, void* dllHandle);

struct Bit
   {
   CMPComponentInterface* componentInterface;
   CMPScienceAPI* scienceAPI;
   void* dllHandle;
   unsigned component;

   ~Bit()
      {
      delete componentInterface;
      delete scienceAPI;

      DeleteComponent(component, dllHandle);

      closeDLL(dllHandle);
      }
   };


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
   Bit* bit = new Bit;

   // create a component interface and a science api that the component
   // will talk to.
   bit->componentInterface = new CMPComponentInterface(callbackArg, callback, *componentID, *parentID);
   bit->scienceAPI = new CMPScienceAPI(*bit->componentInterface);

   // go create an instance of our component by loading the correct dll
   // and calling a createComponent entry point.
   bit->dllHandle = loadDLL(dllFileName);
   bit->component = CreateComponent(bit->scienceAPI, bit->componentInterface,
                                    dllFileName, bit->dllHandle);

   // The instance number we return to the PM is a pointer to the component
   // object we just created.
   *instanceNumber = (unsigned) bit;        
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL deleteInstance (unsigned* instanceNumber)
   {
   Bit* bit = (Bit*) *instanceNumber;
   delete bit;
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   Bit* bit = (Bit*) *instanceNumber;
   bit->componentInterface->messageToLogic(*message);
   *processed = true; // ???? not sure why we need this.
   }

// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript,
                                                         char* description)
   {
   XMLDocument* Doc = new XMLDocument(initScript, XMLDocument::xmlContents);
   std::string dllFileName = Doc->documentElement().getAttribute("executable");
   std::string instanceName = Doc->documentElement().getAttribute("name");
   //MessageBox(NULL, instanceName.c_str(), "", MB_OK);

   // create an instance of the module.
   unsigned dummy = 0;
   unsigned instanceNumber = 0;
   createInstance(dllFileName.c_str(), &dummy, &dummy, &instanceNumber, &dummy, NULL);

   Init1Type init1;
   init1.sdml = initScript;
   init1.fqn = instanceName;
   init1.inStartup = true;

   // call init1.
   Message& init1Message = newMessage(Message::Init1, 0, 0, false, init1);
   bool processed;
   messageToLogic(&instanceNumber, &init1Message, &processed);

   Bit* bit = (Bit*) instanceNumber;

   std::string compDescription =  bit->componentInterface->getDescription(dllFileName);
   strcpy(description, compDescription.c_str());

   // delete the instance.
   deleteInstance(&instanceNumber);
   }


extern "C" CMPComponentInterface* EXPORT STDCALL  CICreate
   (unsigned* callbackarg, CallbackType* callback, unsigned componentid, unsigned parentid)
   {
   return new CMPComponentInterface(callbackarg, callback, componentid, parentid);
   }

extern "C" void EXPORT STDCALL  CIDelete
   (CMPComponentInterface* componentInterface)
   {
   delete componentInterface;
   }

extern "C" void EXPORT STDCALL  CIMessageToLogic
   (CMPComponentInterface* componentInterface, Message* message)
   {
   componentInterface->messageToLogic(*message);
   }

extern "C" void EXPORT STDCALL  CIName
   (CMPComponentInterface* componentInterface, char* name)
   {
   strcpy(name, componentInterface->getName().c_str());
   }

extern "C" int EXPORT STDCALL  CICreateData
   (CMPComponentInterface* componentInterface, int DataIndex)
   {
   return 0;
   }

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
class DOTNETWrapper : public IPackableData
   {
   public:
      typedef EXPORT STDCALL unsigned (ForeignHandlerType)(int foreignDataHandle, DOTNETWrapper& messageData, int OpCode);

   private:
      int foreignDataHandle;
      ForeignHandlerType* handler;


   public:
      MessageData* messageData;
      std::string sourceDDML;
      ArraySpecifier* arraySpecifier;

      DOTNETWrapper(ForeignHandlerType* ForeignHandler, bool isGetter, int foreignH, const char* ddml)
         {
         handler = ForeignHandler;
         foreignDataHandle = foreignH;
         this->ddml = ddml;
         }
      virtual void setValue(const std::vector<std::string>& values)
         {
         throw runtime_error("Cannot call setValue on a function");
         }

      virtual void pack(MessageData& messageData)
         {
         this->messageData = &messageData;
         handler(foreignDataHandle, *this, 1);
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML,
                          ArraySpecifier* arraySpecifier)
         {
         this->messageData = &messageData;
         this->sourceDDML = sourceDDML;
         this->arraySpecifier = arraySpecifier;
         handler(foreignDataHandle, *this, 2);
         }

      virtual unsigned memorySize()
         {
         return handler(foreignDataHandle, *this, 3);
         }

   };
extern "C" void EXPORT STDCALL  CISubscribe(CMPComponentInterface* componentInterface,
                                            const char* Name,
                                            DOTNETWrapper::ForeignHandlerType* ForeignHandler,
                                            int ForeignDataHandle,
                                            const char* ddml)
   {
   try
      {
      IPackableData* Handler = new DOTNETWrapper(ForeignHandler, false, ForeignDataHandle, ddml);
      componentInterface->subscribe(Name, Handler);
      }
   catch (const exception& err)
      {
      componentInterface->error(err.what(), true);
      }
   }
extern "C" void EXPORT STDCALL  CIPublish(CMPComponentInterface* componentInterface,
                                          const char* Name,
                                          DOTNETWrapper::ForeignHandlerType* ForeignHandler,
                                          int ForeignDataHandle,
                                          const char* ddml)
   {
   try
      {
      IPackableData* Handler = new DOTNETWrapper(ForeignHandler, false, ForeignDataHandle, ddml);
      componentInterface->publish(Name, Handler);
      }
   catch (const exception& err)
      {
      componentInterface->error(err.what(), true);
      }
   }
extern "C" void EXPORT STDCALL  CIGet(CMPComponentInterface* componentInterface,
                                      const char* Name,
                                      const char* Units,
                                      bool Optional,
                                      DOTNETWrapper::ForeignHandlerType* ForeignHandler,
                                      int ForeignDataHandle,
                                      const char* ddml)
   {
   try
      {
      IPackableData* Handler = new DOTNETWrapper(ForeignHandler, false, ForeignDataHandle, ddml);
      componentInterface->get(Name, Units, Optional, Handler);
      }
   catch (const exception& err)
      {
      componentInterface->error(err.what(), true);
      }
   }
extern "C" void EXPORT STDCALL  CISet(CMPComponentInterface* componentInterface,
                                      const char* Name,
                                      const char* Units,
                                      DOTNETWrapper::ForeignHandlerType* ForeignHandler,
                                      int ForeignDataHandle,
                                      const char* ddml)
   {
   try
      {
      IPackableData* Handler = new DOTNETWrapper(ForeignHandler, false, ForeignDataHandle, ddml);
      componentInterface->set(Name, Units, Handler);
      }
   catch (const exception& err)
      {
      componentInterface->error(err.what(), true);
      }
   }
extern "C" void EXPORT STDCALL  CIExpose(CMPComponentInterface* componentInterface,
                                         const char* Name,
                                         const char* Units,
                                         const char* Description,
                                         bool Writable,
                                         DOTNETWrapper::ForeignHandlerType* ForeignHandler,
                                         int ForeignDataHandle,
                                         const char* ddml)
   {
   try
      {
      IPackableData* Handler = new DOTNETWrapper(ForeignHandler, false, ForeignDataHandle, ddml);
      componentInterface->expose(Name, Units, Description, Writable, Handler);
      }
   catch (const exception& err)
      {
      componentInterface->error(err.what(), true);
      }
   }

extern "C" void EXPORT STDCALL  CIRead
   (CMPComponentInterface* componentInterface, char* Name, bool Optional, char* Data)
   {
   string st;
   IPackableData* Temp = new CMPBuiltIn<string>(st);
   componentInterface->read(Name, Temp, Optional);
   strcpy(Data, st.c_str());
   }
extern "C" void EXPORT STDCALL  CIError
   (CMPComponentInterface* componentInterface, char* Message, bool IsFatal)
   {
   componentInterface->error(Message, IsFatal);
   }

extern "C" void EXPORT STDCALL  CIPackBool(DOTNETWrapper& wrapper, bool Data)
   {
   pack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIPackInt(DOTNETWrapper& wrapper,  int Data)
   {
   pack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIPackSingle(DOTNETWrapper& wrapper, float Data)
   {
   pack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIPackDouble(DOTNETWrapper& wrapper, double Data)
   {
   pack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIPackString(DOTNETWrapper& wrapper, char* Data)
   {
   pack(*wrapper.messageData, string(Data));
   }

extern "C" void EXPORT STDCALL  CIUnPackBoolWithConverter(DOTNETWrapper& wrapper, bool& Data)
   {
   CMPBuiltIn<bool> Converter(Data);
   Converter.unpack(*wrapper.messageData, wrapper.sourceDDML, wrapper.arraySpecifier);
   }
extern "C" void EXPORT STDCALL  CIUnPackIntWithConverter(DOTNETWrapper& wrapper,  int& Data)
   {
   CMPBuiltIn<int> Converter(Data);
   Converter.unpack(*wrapper.messageData, wrapper.sourceDDML, wrapper.arraySpecifier);
   }
extern "C" void EXPORT STDCALL  CIUnPackSingleWithConverter(DOTNETWrapper& wrapper, float& Data)
   {
   CMPBuiltIn<float> Converter(Data);
   Converter.unpack(*wrapper.messageData, wrapper.sourceDDML, wrapper.arraySpecifier);
   }
extern "C" void EXPORT STDCALL  CIUnPackDoubleWithConverter(DOTNETWrapper& wrapper, double& Data)
   {
   CMPBuiltIn<double> Converter(Data);
   Converter.unpack(*wrapper.messageData, wrapper.sourceDDML, wrapper.arraySpecifier);
   }
extern "C" void EXPORT STDCALL  CIUnPackStringWithConverter(DOTNETWrapper& wrapper, char* Data)
   {
   string st;
   CMPBuiltIn<string> Converter(st);
   Converter.unpack(*wrapper.messageData, wrapper.sourceDDML, wrapper.arraySpecifier);
   strcpy(Data, st.c_str());
   }
extern "C" void EXPORT STDCALL  CIUnPackBool(DOTNETWrapper& wrapper,  bool& Data)
   {
   unpack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIUnPackInt(DOTNETWrapper& wrapper,  int& Data)
   {
   unpack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIUnPackSingle(DOTNETWrapper& wrapper, float& Data)
   {
   unpack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIUnPackDouble(DOTNETWrapper& wrapper, double& Data)
   {
   unpack(*wrapper.messageData, Data);
   }
extern "C" void EXPORT STDCALL  CIUnPackString(DOTNETWrapper& wrapper, char* Data)
   {
   string st;
   unpack(*wrapper.messageData, st);
   strcpy(Data, st.c_str());
   }
extern "C" unsigned EXPORT STDCALL  CIMemorySizeBool(bool Data)
   {
   return memorySize(Data);
   }
extern "C" unsigned EXPORT STDCALL  CIMemorySizeInt(int Data)
   {
   return memorySize(Data);
   }
extern "C" unsigned EXPORT STDCALL  CIMemorySizeSingle(float Data)
   {
   return memorySize(Data);
   }
extern "C" unsigned EXPORT STDCALL  CIMemorySizeDouble(double Data)
   {
   return memorySize(Data);
   }
extern "C" unsigned EXPORT STDCALL  CIMemorySizeString(char* Data)
   {
   return memorySize(string(Data));
   }

