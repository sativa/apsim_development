//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Registrations.h"
#include <general\stl_functions.h>
#include <general\string_functions.h>

#pragma package(smart_init)
using namespace std;
using namespace protocol;

//---------------------------------------------------------------------------
// Return a string representation of the specified registration type.
//---------------------------------------------------------------------------
string asString(RegistrationType type)
   {
    switch (type)
      {
      case getVariableReg : return "getVariableReg";
      case setVariableReg : return "setVariableReg";
      case eventReg       : return "eventReg";
      case methodCallReg  : return "methodCallReg";

      case respondToGetReg : return "respondToGetRegs";
      case respondToSetReg : return "respondToSetReg";
      case respondToEventReg       : return "respondToEventReg";
      case respondToMethodCallReg  : return "respondToMethodCallReg";
      };
   throw runtime_error("Invalid registration type: " + asString(type));
   }
//---------------------------------------------------------------------------
// Return the opposite of the specified type.
// e.g. if getVariableReg passed in, returns respondToGetReg
//---------------------------------------------------------------------------
RegistrationType opposite(RegistrationType type)
   {
    switch (type)
      {
      case getVariableReg : return respondToGetReg;
      case setVariableReg : return respondToSetReg;
      case eventReg       : return respondToEventReg;
      case methodCallReg  : return respondToMethodCallReg;

      case respondToGetReg : return getVariableReg;
      case respondToSetReg : return setVariableReg;
      case respondToEventReg       : return eventReg;
      case respondToMethodCallReg  : return methodCallReg;
      };
   throw runtime_error("Invalid registration type: " + asString(type));
   }

//---------------------------------------------------------------------------
// Structure for holding an individual registration.
//---------------------------------------------------------------------------
class Registration
   {
   public:
      //---------------------------------------------------------------------------
      // constructor. Will throw if a method call isn't directed to a specific
      // component.
      //---------------------------------------------------------------------------
      Registration(const string& n, RegistrationType t)
         : regName(n), type(t)
         {
         unsigned posPeriod = regName.find('.');
         if (posPeriod != string::npos)
            {
            regComponent = regName.substr(0, posPeriod);
            regName.erase(0, posPeriod+1);
            }
         else if (type == methodCallReg)
            throw runtime_error("All method calls must be directed to a specific component. "
                                "Method call: " + regName);
         }
      //-----------------------x----------------------------------------------------
      // Return name to caller.
      //---------------------------------------------------------------------------
      std::string getName(void) const {return regName;}

      //---------------------------------------------------------------------------
      // Return the regComponent to caller.
      //---------------------------------------------------------------------------
      std::string getRegComponent(void) const {return regComponent;}

      //---------------------------------------------------------------------------
      // Return type to caller.
      //---------------------------------------------------------------------------
      RegistrationType getType(void) const {return type;}

      //---------------------------------------------------------------------------
      // Return true if this registration is a method call.
      //---------------------------------------------------------------------------
      bool isMethodCall()
         {
         return (type == methodCallReg);
         }

      //---------------------------------------------------------------------------
      // add the specified subscription to this registration.
      //---------------------------------------------------------------------------
      void addSubscription(unsigned componentId, unsigned regId)
         {
         subscriptions.push_back(make_pair(componentId, regId));
         }

      //---------------------------------------------------------------------------
      // Returns true if this registration exactly matches the one specified.
      //---------------------------------------------------------------------------
      bool operator== (const Registration& rhs)
         {
         return (regComponent == rhs.regComponent
                 && regName == rhs.regName
                 && type == rhs.type);
         }
      //---------------------------------------------------------------------------
      // Returns true if this registration exactly matches the one specified.
      //---------------------------------------------------------------------------
      bool matchForSubscription (const Registration& rhs)
         {
         return (Str_i_Eq(regName, rhs.regName));
         }

      //---------------------------------------------------------------------------
      // clear all subscriptions.
      //---------------------------------------------------------------------------
      void clearSubscriptions(void)
         {
         subscriptions.erase(subscriptions.begin(), subscriptions.end());
         }
      //---------------------------------------------------------------------------
      // Return a list of subscribed registrations. Will throw if id is invalid.
      //---------------------------------------------------------------------------
      void getSubscriptions(Registrations::Subscriptions& subs)
         {
         subs = subscriptions;
         }

   private:
      string regComponent;
      string regName;
      RegistrationType type;
      Registrations::Subscriptions subscriptions;

   };
//---------------------------------------------------------------------------
// This class encapsulates a collection of registrations for a specific
// component.
//---------------------------------------------------------------------------
class RegComponent
   {
   public:

      //---------------------------------------------------------------------------
      // constructor
      //---------------------------------------------------------------------------
      RegComponent(const string& n, unsigned i)
         : name(n), id(i) {}

      //---------------------------------------------------------------------------
      // clear the specified registrations.
      //---------------------------------------------------------------------------
      RegComponent::~RegComponent()
         {
         for (Registrations::Regs::iterator reg = index.begin();
                                            reg != index.end();
                                            reg++)
            delete reg->second;
         }

      //---------------------------------------------------------------------------
      // Return name of component to caller.
      //---------------------------------------------------------------------------
      string getName(void) {return name;}

      //---------------------------------------------------------------------------
      // Return id of component to caller.
      //---------------------------------------------------------------------------
      unsigned getId(void) {return id;}

      //---------------------------------------------------------------------------
      // Return the name of a registration to caller.
      //---------------------------------------------------------------------------
      string getRegistrationName(unsigned regId)
         {
         checkId(regId);
         return index[regId]->getName();
         }

      //---------------------------------------------------------------------------
      // Return true if the specified registration is a method call.
      //---------------------------------------------------------------------------
      bool isMethodCall(unsigned regId)
         {
         checkId(regId);
         return index[regId]->isMethodCall();
         }

      //---------------------------------------------------------------------------
      // Return a reference to a collection of registrations for the specified
      // type.
      //---------------------------------------------------------------------------
      Registrations::Regs& getRegsOfType(protocol::RegistrationType type)
         {
          switch (type)
            {
            case getVariableReg : return gets;
            case setVariableReg : return sets;
            case eventReg       : return events;
            case methodCallReg  : return methods;

            case respondToGetReg : return respondToGets;
            case respondToSetReg : return respondToSets;
            case respondToEventReg       : return respondToEvents;
            case respondToMethodCallReg  : return respondToMethods;
            };
         throw runtime_error("Invalid registration type: " + asString(type));
         }

      //---------------------------------------------------------------------------
      // Add a registration that has the specified id, name and kind. The
      // ownerComponentId is the id of the component that is adding the
      // registrations.  Name can be of the form: component.name.
      // Will throw if registration id already exists.
      // Will throw if registration name and type already matches an existing registration.
      // Will throw if registration is a method call and it isn't directed to
      // a specific component.
      //---------------------------------------------------------------------------
      Registration& add(unsigned regId, const string& name, RegistrationType type)
         {
         if (index.find(regId) != index.end())
            throw runtime_error("Registration id already exists. Registration name: " + name);

         Registrations::Regs* regsToAddTo;
         if (type == respondToGetSetReg)
            regsToAddTo = &getRegsOfType(respondToGetReg);
         else
            regsToAddTo = &getRegsOfType(type);

         Registration* newReg = new Registration(name, type);
         regsToAddTo->insert(make_pair(regId, newReg));

         if (type == respondToGetSetReg)
            getRegsOfType(respondToSetReg).insert(make_pair(regId, newReg));

         index.insert(make_pair(regId, newReg));
         return *newReg;
         }
      //---------------------------------------------------------------------------
      // Delete a registration for the specified component that has the
      // specified id. Will throw if componentId or regId is invalid
      //---------------------------------------------------------------------------
      void erase(unsigned regId)
         {
         checkId(regId);

         RegistrationType type = index[regId]->getType();
         delete index[regId];
         index.erase(regId);
         getRegsOfType(type).erase(regId);
         }

      //---------------------------------------------------------------------------
      // Find any subscriptions that match the specified registration. Store
      // any subscriptions into the specified registration
      //---------------------------------------------------------------------------
      void findSubs(Registration& regToFind, unsigned componentId, unsigned regId)
         {
         RegistrationType oppositeType = opposite(regToFind.getType());
         Registrations::Regs& regsToSearch = getRegsOfType(oppositeType);
         for (Registrations::Regs::iterator reg = regsToSearch.begin();
                             reg != regsToSearch.end();
                             reg++)
            {
            if (regToFind.matchForSubscription(*reg->second))
               {
               if (oppositeType == respondToGetReg || oppositeType == respondToSetReg
                   || oppositeType == respondToEventReg || oppositeType == respondToMethodCallReg)
                  regToFind.addSubscription(id, reg->first);
               else
                  reg->second->addSubscription(componentId, regId);
               }
            }
         }
      //---------------------------------------------------------------------------
      // Return a list of subscribed registrations. Will throw if id is invalid.
      //---------------------------------------------------------------------------
      void getSubscriptions(unsigned regId, Registrations::Subscriptions& subscriptions)
         {
         checkId(regId);
         index[regId]->getSubscriptions(subscriptions);
         }

      //---------------------------------------------------------------------------
      // Find registrations that match the specified mask
      // (regname or *) and return info
      //---------------------------------------------------------------------------
      void findMatchingRegistrations(const string& mask, protocol::RegistrationType type,
                                     std::vector<Registrations::Info>& matches)
         {
         Registrations::Regs& regsToSearch = getRegsOfType(type);
         for (Registrations::Regs::iterator reg = regsToSearch.begin();
                             reg != regsToSearch.end();
                             reg++)
            {
            if (mask == "*" || Str_i_Eq(reg->second->getName(), mask))
               {
               Registrations::Info info;
               info.componentId = id;
               info.regId = reg->first;
               info.fqn = name + "." + reg->second->getName();
               info.type = type;
               matches.push_back(info);
               }
            }
         }
      //---------------------------------------------------------------------------
      // print all regisrations to the specified contents string.
      //---------------------------------------------------------------------------
      void printReport(string& contents, Registrations* registrations)
         {
         printRegs(gets, contents, registrations);
         printRegs(sets, contents, registrations);
         printRegs(events, contents, registrations);
         printRegs(methods, contents, registrations);
         }

   private:
      string name;
      unsigned id;
      Registrations::Regs gets;
      Registrations::Regs sets;
      Registrations::Regs events;
      Registrations::Regs methods;
      Registrations::Regs respondToGets;
      Registrations::Regs respondToSets;
      Registrations::Regs respondToEvents;
      Registrations::Regs respondToMethods;
      Registrations::Regs index;

      //---------------------------------------------------------------------------
      // Check the specified id. Throw if id is not a valid index into
      // index container.
      //---------------------------------------------------------------------------
      void checkId(unsigned id)
         {
         if (index.find(id) == index.end())
            {
            string idstring = IntToStr(id).c_str();
            throw runtime_error("Invalid registration id. ID = " + idstring
                                + ". Component = " + name);
            }
         }

      //---------------------------------------------------------------------------
      // print all regisrations to the specified contents string.
      //---------------------------------------------------------------------------
      void printRegs(Registrations::Regs& regs, string& contents,
                     Registrations* registrations)
         {
         for (Registrations::Regs::iterator reg = regs.begin();
                                            reg != regs.end();
                                            reg++)
            {
            contents += name + "." + reg->second->getName() + " (";
            if (reg->second->getType() == getVariableReg)
               contents += "get";
            else if (reg->second->getType() == setVariableReg)
               contents += "set";
            else if (reg->second->getType() == eventReg)
               contents += "event";
            else
               contents += "method";
            contents += ") = ";

            Registrations::Subscriptions subs;
            reg->second->getSubscriptions(subs);
            for (Registrations::Subscriptions::iterator sub = subs.begin();
                                                        sub != subs.end();
                                                        sub++)
               {
               if (sub != subs.begin())
                  contents += ", ";
               contents += registrations->components[sub->first]->getName();
               }
            contents += "\n";
            }
         }
   };

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Registrations::~Registrations(void)
   {
   for (Components::iterator c = components.begin();
                             c != components.end();
                             c++)
      delete c->second;
   }
//---------------------------------------------------------------------------
// Add a registration that has the specified id, name and kind. The
// ownerComponentId is the id of the component that is adding the
// registrations.  Name can be of the form: component.name.
// Will throw if registration id already exists.
// Will throw if registration name and type already matches an existing registration.
// Will throw if registration is a method call and it isn't directed to
// a specific component.
//---------------------------------------------------------------------------
void Registrations::add(unsigned componentId,
                        unsigned regId,
                        const string& name,
                        RegistrationType type,
                        const string& componentName,
                        bool resolveReg)
   {
   if (components.find(componentId) == components.end())
      components.insert(make_pair(componentId, new RegComponent(componentName, componentId)));

   Registration& newReg = components[componentId]->add(regId, name, type);
   if (resolveReg)
      resolve(newReg, componentId, regId);
   }
//---------------------------------------------------------------------------
// Return the name of a registration to caller.
//---------------------------------------------------------------------------
string Registrations::getRegistrationName(unsigned componentId, unsigned regId)
   {
   checkId(componentId);
   return components[componentId]->getRegistrationName(regId);
   }
//---------------------------------------------------------------------------
// Return true if the specified registration is a method call.
//---------------------------------------------------------------------------
bool Registrations::isMethodCall(unsigned componentId, unsigned regId)
   {
   checkId(componentId);
   return components[componentId]->isMethodCall(regId);
   }
//---------------------------------------------------------------------------
// Delete a registration for the specified component that has the
// specified id. Will throw if componentId or regId is invalid
//---------------------------------------------------------------------------
void Registrations::erase(unsigned componentId, unsigned regId)
   {
   checkId(componentId);
   components[componentId]->erase(regId);
   }
//---------------------------------------------------------------------------
// Resolve all registrations
// Will throw if a registration has more subscriptions that expected
//    e.g. if a methodCall has > 1 subscription.
// Will throw if a method call does not have a component as part of the
//    registration name.
//---------------------------------------------------------------------------
void Registrations::resolveAll()
   {
   for (Components::iterator c = components.begin();
                             c != components.end();
                             c++)
      {
      resolve(c->second->getRegsOfType(getVariableReg), c->second->getId());
      resolve(c->second->getRegsOfType(setVariableReg), c->second->getId());
      resolve(c->second->getRegsOfType(eventReg), c->second->getId());
      resolve(c->second->getRegsOfType(methodCallReg), c->second->getId());
      }
   }
//---------------------------------------------------------------------------
// Resolve the specified registrations.
// Will throw if a registration has more subscriptions that expected
//    e.g. if a methodCall has > 1 subscription.
// Will throw if a method call does not have a component as part of the
//    registration name.
//---------------------------------------------------------------------------
void Registrations::resolve(Regs& regs, unsigned componentId)
   {
   for (Regs::iterator reg = regs.begin();
                       reg != regs.end();
                       reg++)
      {
      resolve(*reg->second, componentId, reg->first);
      }
   }
//---------------------------------------------------------------------------
// Resolve the specified registration.
// Will throw if a registration has more subscriptions that expected
//    e.g. if a methodCall has > 1 subscription.
// Will throw if a method call does not have a component as part of the
//    registration name.
//---------------------------------------------------------------------------
void Registrations::resolve(Registration& reg, unsigned componentId, unsigned regId)
   {
   reg.clearSubscriptions();
   string regComponent = reg.getRegComponent();
   if (regComponent == "")
      {
      for (Components::iterator c = components.begin();
                                c != components.end();
                                c++)
         c->second->findSubs(reg, componentId, regId);
      }
   else
      {
      RegComponent* comp = getComponent(regComponent);
      if (comp != NULL)
         comp->findSubs(reg, componentId, regId);
      }
   }
//---------------------------------------------------------------------------
// Return a reference to the component that matches the specified name.
// Throws if name is invalid.
//---------------------------------------------------------------------------
RegComponent* Registrations::getComponent(const string& name)
   {
   for (Components::iterator c = components.begin();
                             c != components.end();
                             c++)
      {
      if (Str_i_Eq(c->second->getName(), name))
         return c->second;
      }
   return NULL; 
   }
//---------------------------------------------------------------------------
// Return a list of subscribed registrations. Will throw if id is invalid.
//---------------------------------------------------------------------------
void Registrations::getSubscriptions(unsigned componentId, unsigned regId, Subscriptions& subscriptions)
   {
   checkId(componentId);
   components[componentId]->getSubscriptions(regId, subscriptions);
   }
//---------------------------------------------------------------------------
// Check the specified id. Throw if id is not a valid index into
// components container.
//---------------------------------------------------------------------------
void Registrations::checkId(unsigned id)
   {
   if (components.find(id) == components.end())
      {
      string idstring = IntToStr(id).c_str();
      throw runtime_error("Invalid component id. ID = " + idstring);
      }
   }
//---------------------------------------------------------------------------
// Find registrations that match the specified mask
// (regname, component.regname, *.regname or component.*) and
// return info
//---------------------------------------------------------------------------
void Registrations::findMatchingRegistrations(const string& mask, protocol::RegistrationType type,
                                              std::vector<Info>& matches)
   {
   string componentName = "*";
   string regName;
   unsigned posPeriod = mask.find('.');
   if (posPeriod == string::npos)
      regName = mask;
   else
      {
      componentName = mask.substr(0, posPeriod);
      regName = mask.substr(posPeriod+1);
      }

   for (Components::iterator c = components.begin();
                            c != components.end();
                            c++)
      {
      if (componentName == "*" || Str_i_Eq(componentName, c->second->getName()))
        c->second->findMatchingRegistrations(regName, type, matches);
      }
   }
//---------------------------------------------------------------------------
// Create a formatted report of all registrations.
//---------------------------------------------------------------------------
void Registrations::printReport(string& contents)
   {
   contents = "";
   for (Components::iterator c = components.begin();
                            c != components.end();
                            c++)
      {
      c->second->printReport(contents, this);
      }
   }

