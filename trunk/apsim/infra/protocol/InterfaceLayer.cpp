//---------------------------------------------------------------------------
#include "InterfaceLayer.h"
#include "event.h"
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
   if (event == NULL)
      toAddress.address->getComputation()->action(aMsg);
   else
      toAddress.address->getComputation()->inEvent(*event);
   }

