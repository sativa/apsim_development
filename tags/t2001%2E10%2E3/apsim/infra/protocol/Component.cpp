//---------------------------------------------------------------------------
#include "Component.h"
#include "InterfaceLayer.h"
#include "computation.h"
#include <fstream>
// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComponent::PROTOCOLComponent(const string& aName,
                                     ICoordinator* coordinator,
                                     const string& dllFileName,
                                     const string& ssdl)
   : name(aName), filename(dllFileName)
   {
   IL = new PROTOCOLInterfaceLayer(this, coordinator);
   computation = new PROTOCOLComputation(this, dllFileName, ssdl);
   }

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComponent::PROTOCOLComponent(const string& aName,
                                     ICoordinator* coordinator)
   : name(aName)
   {
   IL = new PROTOCOLInterfaceLayer(this, coordinator);
   computation = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComponent::~PROTOCOLComponent(void)
   {
   delete IL;
   delete computation;
   }
// ------------------------------------------------------------------
//  Short description:
//     create the component.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComponent::create(void)
   {
   computation->create();
   }
// ------------------------------------------------------------------
//  Short description:
//     terminate component

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComponent::term(void)
   {
   if (computation != NULL)
      computation->terminate();
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise component

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComponent::init(void)
   {
   if (computation != NULL)
      computation->initialise();
   }

// ------------------------------------------------------------------
//  Short description:
//     dispatch event to component logic

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComponent::inEvent(PROTOCOLEvent& Event)
   {
   getComputation()->inEvent(Event);
   }
