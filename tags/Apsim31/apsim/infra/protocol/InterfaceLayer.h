//---------------------------------------------------------------------------
#ifndef InterfaceLayerH
#define InterfaceLayerH
#include "interfaces.h"
#include "ProtocolExport.h"
// ------------------------------------------------------------------
//  Short description:
//    This class provides a interface to a component.  Each component
//    creates an instance of this class.  All messages from and to a
//    component are sent through this layer.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOL_EXPORT PROTOCOLInterfaceLayer
   {
   public:
      PROTOCOLInterfaceLayer(const IComponent* component,
                             ICoordinator* coordinator);

      void sendMessage(const PROTOCOLTransportAddress& toAddress,
                       PROTOCOLMessage& aMsg) const;
      void receiveMessage(PROTOCOLMessage& aMsg);

      ICoordinator* getCoordinator(void) {return coordinator;}
   private:
      const IComponent* component;
      ICoordinator* coordinator;

   };
#endif
