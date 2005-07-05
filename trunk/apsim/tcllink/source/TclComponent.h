//---------------------------------------------------------------------------
#ifndef TclComponentH
#define TclComponentH

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Tcl interpreter and an APSIM simulation.
// ------------------------------------------------------------------
class TclComponent : public protocol::Component
   {
   public:
      TclComponent(void);
      ~TclComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);

      int apsimGet( Tcl_Interp *interp, const string &varname);
      bool apsimSet(Tcl_Interp *interp, const string &varname, Tcl_Obj *obj);

      void addRegistration(const string &name) ;
      void sendMessage(const string &moduleName, const string &actionName,
                       protocol::ApsimVariant &outgoingApsimVariant);
   private:

      typedef std::map<unsigned, std::string> UInt2StringMap;
      UInt2StringMap rules;
      UInt2StringMap variables;
      Tcl_Interp *Interp;
   };
#endif
