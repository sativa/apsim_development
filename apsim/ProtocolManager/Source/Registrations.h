//---------------------------------------------------------------------------
#ifndef RegistrationsH
#define RegistrationsH
#include <ComponentInterface\messages.h>

class RegComponent;
class Registration;
//---------------------------------------------------------------------------
// This class encapsulates a collection of registrations.
// Registration Id's must be unique for a given component. So to
// retrieve a registration the caller must supply a component id
// and a registration id.
//---------------------------------------------------------------------------
class Registrations
   {
   public:
      typedef std::map<unsigned, Registration*> Regs;

      //---------------------------------------------------------------------------
      // destructor
      //---------------------------------------------------------------------------
      ~Registrations(void);

      //---------------------------------------------------------------------------
      // Add a registration that has the specified id, name and kind. The
      // ownerComponentId is the id of the component that is adding the
      // registrations.  Name can be of the form: component.name.
      // Will throw if registration id already exists.
      // Will throw if registration name and type already matches an existing registration.
      // Will throw if registration is a method call and it isn't directed to
      // a specific component.
      //---------------------------------------------------------------------------
      void add(unsigned ownerComponentId,
               unsigned regId,
               const std::string& name,
               protocol::RegistrationType type,
               const std::string& ownerComponentName,
               bool resolve = false);

      //---------------------------------------------------------------------------
      // Return the name of a registration to caller.
      //---------------------------------------------------------------------------
      string getRegistrationName(unsigned componentId, unsigned regId);

      //---------------------------------------------------------------------------
      // Return true if the specified registration is a method call.
      //---------------------------------------------------------------------------
      bool isMethodCall(unsigned componentId, unsigned regId);

      //---------------------------------------------------------------------------
      // Find registrations that match the specified mask
      // (regname, component.regname, *.regname or component.*) and
      // return info
      //---------------------------------------------------------------------------
      struct Info
         {
         unsigned componentId;
         unsigned regId;
         string fqn;
         protocol::RegistrationType type;
         };
      void findMatchingRegistrations(const string& mask, protocol::RegistrationType type,
                                     vector<Info>& matches);

      //---------------------------------------------------------------------------
      // Delete a registration for the specified component that has the
      // specified id. Will throw if componentId or regId is invalid
      //---------------------------------------------------------------------------
      void erase(unsigned componentId, unsigned regId);

      //---------------------------------------------------------------------------
      // Resolve all registrations.
      // Will throw if a registration has more subscriptions that expected
      //    e.g. if a methodCall has > 1 subscription.
      // Will throw if a method call does not have a component as part of the
      //    registration name.
      //---------------------------------------------------------------------------
      void resolveAll(void);

      //---------------------------------------------------------------------------
      // Return a list of subscribed registrations. Will throw if id is invalid.
      //---------------------------------------------------------------------------
      typedef std::vector<std::pair<unsigned, unsigned> > Subscriptions;
      void getSubscriptions(unsigned componentId, unsigned regId, Subscriptions& subscriptions);

      //---------------------------------------------------------------------------
      // Create a formatted report of all registrations.
      //---------------------------------------------------------------------------
      void printReport(string& contents);

   private:
      typedef std::map<unsigned, RegComponent*> Components;
      Components components;

      //---------------------------------------------------------------------------
      // Resolve the specified registrations.
      // Will throw if a registration has more subscriptions that expected
      //    e.g. if a methodCall has > 1 subscription.
      // Will throw if a method call does not have a component as part of the
      //    registration name.
      //---------------------------------------------------------------------------
      void resolve(Regs& regs, unsigned componentId);

      //---------------------------------------------------------------------------
      // Resolve the specified registration
      // Will throw if a registration has more subscriptions that expected
      //    e.g. if a methodCall has > 1 subscription.
      // Will throw if a method call does not have a component as part of the
      //    registration name.
      //---------------------------------------------------------------------------
      void resolve(Registration& reg, unsigned componentId, unsigned regId);

      //---------------------------------------------------------------------------
      // Return a reference to the component that matches the specified name.
      // Throws if name is invalid.
      //---------------------------------------------------------------------------
      RegComponent* getComponent(const std::string& name);

      //---------------------------------------------------------------------------
      // Check the specified id. Throw if id is not a valid index into
      // components container.
      //---------------------------------------------------------------------------
      void checkId(unsigned id);

      friend RegComponent;
   };
#endif
