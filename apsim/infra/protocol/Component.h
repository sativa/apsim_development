//---------------------------------------------------------------------------
#ifndef ComponentH
#define ComponentH
#include "interfaces.h"
#include "ProtocolExport.h"
#include "interfacelayer.h"
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a component.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOL_EXPORT PROTOCOLComponent : virtual public IComponent
   {
   public:
      PROTOCOLComponent(const string& aName,
                        ICoordinator* coordinator);
      PROTOCOLComponent(const string& name,
                        ICoordinator* coordinator,
                        const string& dllFileName,
                        const string& ssdl);

      bool operator< (const PROTOCOLComponent& rhs) const
         {return name < rhs.name;}
      ~PROTOCOLComponent(void);
      virtual void create(void);
      virtual void init(void);
      virtual void term(void);
      virtual string getName(void) const {return name;}
      virtual string getFilename(void) const {return filename;}
      virtual const IComputation* getComputation(void) const {return computation;}
      virtual ICoordinator* getCoordinator(void) {return IL->getCoordinator();}
      virtual void inEvent(PROTOCOLEvent& Event);

   protected:
      IComputation* computation;
      PROTOCOLInterfaceLayer* IL;
      string name;
      string filename;

   };


#endif
