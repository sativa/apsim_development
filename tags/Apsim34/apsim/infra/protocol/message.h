//---------------------------------------------------------------------------
#ifndef MessageH
#define MessageH
#include "BaseTypes.h"
#include <ostream>
#include "protocolexport.h"
class PROTOCOL_EXPORT PROTOCOLMessage
   {
   public:
      PROTOCOLMessage(const FString& a, const FString& d = "")
         : action(a), data(d) { }
      virtual ~PROTOCOLMessage(void) { }
      const FString& action;
      const FString& data;

      virtual void writeDebug(std::ostream& out) const
         {out << ", Action=" << action << ", Data = " << data;}

   };
#endif
