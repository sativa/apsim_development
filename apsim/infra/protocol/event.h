//---------------------------------------------------------------------------
#ifndef EventH
#define EventH
#include "Message.h"
#include <ostream>
//---------------------------------------------------------------------------
class PROTOCOL_EXPORT PROTOCOLEvent : public PROTOCOLMessage
   {
   public:
      PROTOCOLEvent(const FString& name, const FString& data = "")
         : PROTOCOLMessage(name, data) { };
      ~PROTOCOLEvent(void) { };

      virtual void writeDebug(std::ostream& out) const
         {out << ", EVENT name=" << action << ", Data = " << data;}

   };
#endif
 