//---------------------------------------------------------------------------
#include "InterfaceLayer.h"
#include "event.h"
#include "coordinator.h"
#include "computation.h"
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLInterfaceLayer::PROTOCOLInterfaceLayer(const IComponent* aComponent,
                                               ICoordinator* aCoordinator)
   : component(aComponent), coordinator(aCoordinator)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    send a message to the specified address.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLInterfaceLayer::sendMessage(const PROTOCOLTransportAddress& toAddress,
                                         PROTOCOLMessage& aMsg) const
   {
   PROTOCOLEvent* event = dynamic_cast<PROTOCOLEvent*> (&aMsg);
   const PROTOCOLComputation* computation = dynamic_cast<const PROTOCOLComputation*>
                                           (toAddress.address->getComputation());
   if (computation == NULL)
      {
      // message has come from outside the system.
      PROTOCOLCoordinator* coordinator = dynamic_cast<PROTOCOLCoordinator*>(toAddress.address);
      if (aMsg.action == "get")
         coordinator->getSystemVariable(aMsg.data);
      else if (aMsg.action == "set")
         coordinator->setSystemVariable(aMsg.data);
      else
         coordinator->doSystemMessage(aMsg);
      }
   else
      {
      if (event == NULL)
         toAddress.address->getComputation()->action(aMsg);
      else
         toAddress.address->getComputation()->inEvent(*event);
      }
   }

