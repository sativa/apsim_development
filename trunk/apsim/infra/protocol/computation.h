//---------------------------------------------------------------------------
#ifndef computationH
#define computationH
#include "event.h"
#include "protocolexport.h"
#include "interfaces.h"
// ------------------------------------------------------------------
//  Short description:
//    Encapsulates a component "computation".  A computation is a
//    set of entry points into a component's calculation part.
//    eg: init, term, action, inevent, outevent.
                           
//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOL_EXPORT PROTOCOLComputation : public IComputation
   {
   public:
      PROTOCOLComputation(IComponent* component,
                          const std::string& dllFileName,
                          const std::string& ssdl);
      ~PROTOCOLComputation(void);

      virtual void init(void) {initialise();}
      virtual IComponent* getComponent(void) const {return component;}

      virtual void create(void) const;
      virtual void initialise(void) const;
      virtual void terminate(void) const;
      virtual void action(PROTOCOLMessage& Event) const;
      virtual void inEvent(PROTOCOLEvent& Event) const;
      virtual void outEvent(PROTOCOLEvent& Event) const { };
      virtual void complete(PROTOCOLMessage& Event) const { };
      virtual void idle(void) const{ };
   private:
      PROTOCOLDLLInfo dllInfo;
      IComponent* component;
      int anInstanceNo;
      std::string ssdl;

   };
#endif
