//---------------------------------------------------------------------------
#ifndef computationH
#define computationH
#include "interfaces.h"
#include "ProtocolExport.h"

namespace protocol {
typedef _stdcall void (CallbackType)(const unsigned int *compInst, Message *message);

// ------------------------------------------------------------------
//  Short description:
//    Encapsulates a component "computation".  A computation is a
//    DLL Handle + an instance number.  All calls to the DLL go through
//    this class.

//  Notes:

//  Changes:
//    dph 22/2/2000
//    dph 12/7/2001 modified to make protocol compliant 1.0

// ------------------------------------------------------------------
class PROTOCOL_EXPORT Computation : public IComputation
   {
   public:
      Computation(const std::string& name,
                  const std::string& fileName,
                  unsigned int componentId,
                  unsigned int parentId);
      ~Computation(void);

      bool isOk(void) const  {return (createInstanceProc != NULL &&
                                      deleteInstanceProc != NULL &&
                                      messageToLogicProc != NULL);}
      virtual void messageToLogic(const Message* message) const
         {
         bool messageProcessed;
         (*messageToLogicProc) (&instanceNo, message, &messageProcessed);
         }
      unsigned getInstanceNo(void) const {return instanceNo;}

   private:
      int instanceNo;
      HANDLE handle;

      void createInstance(const std::string& filename,
                          unsigned int componentId,
                          unsigned int parentId);
      virtual void deleteInstance(void) const;
      std::string getWrapperFilename(const std::string& filename);

      void _stdcall (*createInstanceProc)(const char* dllFileName,
                                          const unsigned int* componentID,
                                          const unsigned int* parentID,
                                          const int* anInstanceNo,
                                          const int* callbackArg,
                                          CallbackType* callback);

      void _stdcall (*deleteInstanceProc)(const int* anInstanceNo);
      void _stdcall (*messageToLogicProc)(const int* anInstanceNo,
                                          const Message* messageHeader,
                                          bool* bProcessed);
      bool loadComponent(const std::string& filename);
      void unloadComponent(void);

   };

} // namespace protocol
#endif
