#include <stdlib.h>
#include <general/platform.h>

#include <stdexcept>
#include <iostream>

#include "ApsimRegistrationType.h"
#include "ApsimRegistration.h"
#include "ApsimRegistry.h"

using namespace std;

ApsimRegistry GlobalApsimRegistry;    // singleton registry.

ApsimRegistry& EXPORT ApsimRegistry::getApsimRegistry(void)
// ------------------------------------------------------------------
// Return the singleton global ApsimRegistry.
// ------------------------------------------------------------------
   {
   return GlobalApsimRegistry;
   }
 
 ApsimRegistry::~ApsimRegistry()
   {
   reset();
   }

// Clean up 
void ApsimRegistry::reset(void)
   {
   for (registrations_type::iterator i = registrations.begin();
        i != registrations.end();
        i++) 
      delete i->second;  
   components.empty();
   taintedComponents.empty();
   registrations.empty();
   }


// Add a registration if not already present
// fixme - we really need to rethink this!!
unsigned int ApsimRegistry::add(ApsimRegistration *reg)
   {
   if ( reg->getName().rfind(".") != string::npos)
     throw std::runtime_error("trying to add a qualified registration " + reg->getName());

   if ( reg->getTypeCode() != ::respondToEvent && !isForeign(reg->getComponentID()))
      {
      // return old registration if present. NB. foreigns are often "re-registered"
      //   with different IDs!!!
      registrations_type::iterator i, j, a, b;

      a = registrations.lower_bound(reg->getNameWithoutBrackets());
      b = registrations.upper_bound(reg->getNameWithoutBrackets());

      for (i = a; i != b; i++)
         {
         if (i->second->getTypeCode() == reg->getTypeCode() &&
             i->second->getComponentID() == reg->getComponentID() &&
             i->second->getDestinationID() == reg->getDestinationID() )
            {
//            cout << "removing (" << i->second->getType() << "." << i->second->getComponentID() << "." <<
//              i->second->getName() << "->" << i->second->getDestinationID() << ")= " << ((unsigned int)i->second) << " called again - returning " << i->second->getName() << endl;
//            delete reg;
//            return ((unsigned int)i->second);
              delete i->second;
              registrations.erase(i);
              break;
            }
         }
      }   
   registrations.insert(registrations_type::value_type(reg->getNameWithoutBrackets(), reg));

//   cout << "add (" << reg->getType() << ":" << reg->getComponentID() << "." <<
//        reg->getName() << "->" << reg->getDestinationID() << ")= " << ((unsigned int)reg) << " called\n";
//   cout << "add (" << reg->getType() << ":" << reg->getComponentID() << "." <<
//        reg->getName() << ")=" << reg->getDDML() << " \n";
   return ((unsigned int)reg);
   }


// Find subscribers to an event.
void ApsimRegistry::lookup(ApsimRegistration * reg, 
                           std::vector<ApsimRegistration*>&subscribers)
   {
//   string regName = reg->getName();
//   cout << "lookup:" << reg->getRegID() << ":subscribers to " << 
//        reg->getType() << "." << reg->getDestinationID() << "." << reg->getName() << "=";

   if (reg->getDestinationID() >= 0) 
      {
      vector<int> destination;
      destination.push_back(reg->getDestinationID());

      lookup(reg, destination, subscribers);

      if (reg->getTypeCode() != ::event) 
         pruneDuplicates(subscribers);
      }
   else if (reg->getTypeCode() == ::get || reg->getTypeCode() == ::set) 
      {
      vector<int> siblingsAndParents;
      getSiblingsAndParents(reg->getComponentID(),
                            siblingsAndParents);

      lookup(reg, siblingsAndParents, subscribers);

      pruneDuplicates(subscribers);
      }
   else 
      {
      // Push (events)
      vector<int> siblingsAndDescendants;
      getSiblingsAndDescendants(reg->getComponentID(), 
                                siblingsAndDescendants);

      lookup(reg, siblingsAndDescendants, subscribers);
      
      // "events" can have multiple subscribers in the same component
      // so no pruning needed.
      }
//   for (unsigned int i = 0; i != subscribers.size(); i++) 
//     cout << componentByID(subscribers[i]->getComponentID()) << ",";
//   cout <<  "\n";
   }

// Find the "subscribers" to a registration. 
//  -"candidates" is a list of component IDs that are "in scope" 
//    of this registration
//  -a list of subscribers is returned
//  -duplicates may exist in the returned list
void ApsimRegistry::lookup(ApsimRegistration * reg, 
                           std::vector<int> &candididates, 
                           std::vector<ApsimRegistration*>&subscribers)
   {
   registrations_type::iterator i, a, b;

   a = registrations.lower_bound(reg->getNameWithoutBrackets());
   b = registrations.upper_bound(reg->getNameWithoutBrackets());

   for (i = a; i != b; i++)
      {
      if (reg->matchSubscriberType(i->second) &&
          ::find(candididates.begin(),
                 candididates.end(),
                 i->second->getComponentID()) != candididates.end() )
         {
         subscribers.push_back(i->second);
         }
      }   
   }
void ApsimRegistry::pruneDuplicates( std::vector<ApsimRegistration*>&subscribers)
   {
   // some modules register both "respondToGet" and "respondToGetSet"
   //  - make sure the list we return is unique.
   for (vector<ApsimRegistration*>::iterator sub1 = subscribers.begin();
        sub1 != subscribers.end(); sub1++) 
      {
      for (vector<ApsimRegistration*>::iterator sub2 = sub1 + 1;
           sub2 != subscribers.end(); sub2++) 
         {
         if ((*sub1)->getComponentID() ==  (*sub2)->getComponentID())
            {
            subscribers.erase(sub2);
            break;
            } 
         }
      }
   }
void ApsimRegistry::erase(int owner, unsigned int regID)
   {
   ApsimRegistration *reg = find(owner, regID);
   if (reg == NULL) return;

   for (registrations_type::iterator i = registrations.begin();
        i != registrations.end();
        i++)
      if (reg->isMatch(i->second))
         {
         registrations.erase(i);
         delete reg;
         }
   }

void ApsimRegistry::addComponent(int parentID,
                                 int componentID, 
                                 const std::string &name)
   {
   Component c;
   c.ID = componentID;
   c.Name = name;
   components.push_back(c);
   
   PTree<Component> *p = new PTree<Component>();
   p->item = c;
   p->parent = findComponent(parentID);
   p->parent->children.push_back(p);
   }

// see if a registration is in scope (ie has the same parent)
// as the calling component.
bool ApsimRegistry::inScope(int caller, ApsimRegistration *reg)
   {
   int regComponentID = reg->getComponentID();
   PTree<Component>* regNode = findComponent(regComponentID);
   if (regNode == NULL) {throw std::runtime_error("NULL node in ApsimRegistry::inScope!");}
   int regParent = regNode->parent->item.ID;

   PTree<Component>* callerNode = findComponent(caller);
   if (callerNode == NULL) {throw std::runtime_error("NULL node in ApsimRegistry::inScope!");}
   int callerParent = callerNode->parent->item.ID;
   return (regParent == callerParent);
   }

bool ApsimRegistry::inScope(ApsimRegistry::PTree<ApsimRegistry::Component>*callerNode, ApsimRegistration *reg)
   {
   int regComponentID = reg->getComponentID();
   PTree<Component>* regNode = findComponent(regComponentID);
   if (regNode == NULL) {throw std::runtime_error("NULL node in ApsimRegistry::inScope!");}
   int regParent = regNode->parent->item.ID;

   int callerParent = callerNode->parent->item.ID;
   return (regParent == callerParent);
   }


ApsimRegistry::PTree<ApsimRegistry::Component>* ApsimRegistry::findComponent(int componentID)
  {
  return findComponent(&paddocks, componentID);
  }

ApsimRegistry::PTree<ApsimRegistry::Component>* ApsimRegistry::findComponent(ApsimRegistry::PTree<ApsimRegistry::Component> *node, int componentID)
  {
  if (node->item.ID == componentID) return node;
  PTree<Component>* child;
  for (unsigned i = 0; i < node->children.size(); i++) 
     {
     if ((child = findComponent(node->children[i], componentID)) != NULL)
        return child;
     } 
  return NULL;
  }

void ApsimRegistry::getSiblings(int componentID, vector<int> &siblings)
   {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getSiblings!");}
   PTree<Component>* container = node->parent;

   for (unsigned i = 0; i != container->children.size(); i++)
      {
      siblings.push_back(container->children[i]->item.ID); 
      }
   }

void ApsimRegistry::getSiblingsAndDescendants(int componentID, vector<int> &siblings)
   {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getSiblingsAndDescendants!");}
   PTree<Component>* container = node->parent;

   for (unsigned i = 0; i != container->children.size(); i++)
      {
      siblings.push_back(container->children[i]->item.ID); 
      getDescendants(container->children[i], siblings);
      }
   }

// Fixme -this should be recursive
void ApsimRegistry::getSiblingsAndParents(int componentID, vector<int> &siblings)
   {
   PTree<Component>* node = findComponent(componentID);
   if (node == NULL) {throw std::runtime_error("NULL node in getSiblingsAndParents!");}
   PTree<Component>* container = node->parent;

   for (unsigned i = 0; i != container->children.size(); i++)
      {
      if (container->children[i]->item.ID != componentID)   
         siblings.push_back(container->children[i]->item.ID); 
      }

   PTree<Component>* grandparent = container->parent;
   if (grandparent == NULL) {throw std::runtime_error("NULL node in getSiblingsAndParents2!");}
   if (grandparent != container)
      {
      for (unsigned i = 0; i != grandparent->children.size(); i++)
        {
        if (grandparent->children[i]->item.ID != container->parent->item.ID)   
           siblings.push_back(grandparent->children[i]->item.ID); 
        }
      }
   }

void ApsimRegistry::getDescendants(ApsimRegistry::PTree<ApsimRegistry::Component>*node, vector<int> &siblings)
   {
   for (unsigned i = 0; i != node->children.size(); i++)
      {
      siblings.push_back(node->children[i]->item.ID); 
      getDescendants(node->children[i], siblings);
      }
   }
int ApsimRegistry::componentByName(const std::string &name)
   {
   for (unsigned int i = 0; i < components.size(); i++ )
      if (Str_i_Eq(components[i].Name,name))
         return components[i].ID;
   return(-1); 
   }

std::string ApsimRegistry::componentByID(int id)
   {
   for (unsigned int i = 0; i < components.size(); i++ )
      if (components[i].ID == id)
         return components[id].Name;

   string x= "<?undefined?(";
   x += itoa(id);
   x += ")>";
   return x;  
}
void ApsimRegistry::setForeignTaint(int id)
{
   taintedComponents.push_back(id); 
}

void ApsimRegistry::clearForeignTaint(int id)
{
   vector<int>::iterator i = 
        std::find(taintedComponents.begin(),
                  taintedComponents.end(),
                  id);

   if (i != taintedComponents.end())
     taintedComponents.erase(i); 
}

bool ApsimRegistry::isForeign(int id)
{
   return(std::find(taintedComponents.begin(),
                    taintedComponents.end(),
                    id) != taintedComponents.end());
}

// Find a registration object from a 
// component & registration ID. "Native" registrations
// are easy, foreigns need special care.
ApsimRegistration *ApsimRegistry::find(int ownerID,
                                       unsigned int regnID)
   {
   if (isForeign(ownerID)) 
      {
      for (registrations_type::iterator i = registrations.begin();
           i != registrations.end();
           i++)
         {
         if (i->second->getComponentID() == ownerID &&
             i->second->getRegID() == regnID)
            return (i->second);
         }
      return NULL; 
      }
   return ((ApsimRegistration *) regnID);
   }


// ".paddock1.wheat.sow"
// "wheat.sow"
// "*.sow"
// etc
void ApsimRegistry::unCrackPath(int fromID,                 // IN: id of module asking
                                const std::string &fqName,  // IN: [module.]name string
                                int &id,                    // out
                                std::string &name)          // out
{
   size_t pos = fqName.rfind(".");
   if (pos != string::npos)
      {
      name = fqName.substr(pos+1);
      string componentName = fqName.substr(0,pos);
      if (componentName[0] == '.') 
        {
        id = componentByName(componentName);
        if (id < 0) {throw std::runtime_error("Unknown module name " + componentName);}
        }
      else if (componentName[0] == '*') 
        {
        id = -1;
        }
      else 
        {
        // prepend container name of sending component
        string container = componentByID(fromID) ;
        size_t cpos = container.rfind(".");
        if (cpos != string::npos) 
           container = container.substr(0,cpos);

        componentName = container + "." + componentName;

        id = componentByName(componentName);
        if (id < 0) {throw std::runtime_error("Unknown module name " + componentName);}
        }
      }
   else
      {
      id = -1;
      name = fqName;
      }
}

// Write a description of whatever a component has registered
std::string ApsimRegistry::getDescription(int componentID)
   {
   registrations_type::iterator i;
   string returnString;
 
   typedef std::map<std::string, std::string> StringMap;
   StringMap properties;
 
   for (i = registrations.begin(); i != registrations.end(); i++) 
      {
      ApsimRegistration *reg = i->second;
      if (reg->getComponentID() == componentID) 
         {
         if (reg->getTypeCode() == ::respondToGet)
               {
               string st = "   <property name=\"";
               st += reg->getName();
               st += "\" access=\"read\" init=\"F\">\n";
               st += reg->getType();
               st += "</property>\n";
               properties.insert(make_pair(reg->getName(), st));
               }
         else if (reg->getTypeCode() == ::respondToSet)
               {
               string st = "   <property name=\"";
               st += reg->getName();
               st += "\" access=\"write\" init=\"F\">\n";
               st += reg->getType();
               st += "</property>\n";
               properties.insert(make_pair(reg->getName(), st));
               }
         else if (reg->getTypeCode() == ::respondToGetSet)
               {
               string st = "   <property name=\"";
               st += reg->getName();
               st += "\" access=\"both\" init=\"F\">\n";
               st += reg->getType();
               st += "</property>\n";
               properties.insert(make_pair(reg->getName(), st));
               }
         else if (reg->getTypeCode() == ::respondToEvent)
            {
            string st = "   <event name=\"";
            st += reg->getName();
            st += "\" kind=\"subscribed\">";
            XMLDocument* doc = new XMLDocument(reg->getType(), XMLDocument::xmlContents);
            st += doc->documentElement().innerXML();
            delete doc;
            st += "</event>\n";
            properties.insert(make_pair(reg->getName(), st));
            }
         else if (reg->getTypeCode() == ::event)
            {
            string st = "   <event name=\"";
            st += reg->getName();
            st += "\" kind=\"published\">";
            XMLDocument* doc = new XMLDocument(reg->getType(), XMLDocument::xmlContents);
            st += doc->documentElement().innerXML();
            delete doc;
            st += "</event>\n";
            properties.insert(make_pair(reg->getName(), st));
            }
         else if (reg->getTypeCode() == ::get)
            {
            string st = "   <driver name=\"";
            st += reg->getName();
            st += "\">\n";
            st += reg->getType();
            st += "</driver>\n";
            properties.insert(make_pair(reg->getName(), st));
            }
         }
      }
   for (StringMap::iterator i = properties.begin();
                            i != properties.end();
                            i++)
         returnString += i->second + "\n";

   return returnString;
   }

void ApsimRegistry::dumpStats(void)
   {
   int nget=0;
   int nrespondToGet=0;
   int nset=0;
   int nrespondToSet=0;
   int nevent=0;
   int nrespondToEvent=0;
   int nrespondToGetSet=0;
   for (registrations_type::iterator i = registrations.begin();
        i != registrations.end();
        i++) 
      {
      if (i->second->getTypeCode() == ::get) nget++;  
      else if (i->second->getTypeCode() == ::respondToGet) nrespondToGet++;
      else if (i->second->getTypeCode() == ::set) nset++;
      else if (i->second->getTypeCode() == ::respondToSet) nrespondToSet++;
      else if (i->second->getTypeCode() == ::event) nevent++;
      else if (i->second->getTypeCode() == ::respondToEvent) nrespondToEvent++;
      else if (i->second->getTypeCode() == ::respondToGetSet) nrespondToGetSet++;
      }

   cout << "Registration types\n";
   cout << "get="<<nget<<endl;
   cout << "respondToGet="<<nrespondToGet<<endl;
   cout << "set="<<nset<<endl;
   cout << "respondToSet="<<nrespondToSet<<endl;
   cout << "event="<<nevent<<endl;
   cout << "respondToEvent="<<nrespondToEvent<<endl;
   cout << "respondToGetSet="<<nrespondToGetSet<<endl;
   }

void ApsimRegistry::dumpComponentTree(void)
   {
   PTree<Component>* root = findComponent(0);
   if (root == NULL) {throw std::runtime_error("NULL node in dumpComponentTree!");}
   int indent = 0;
   for (unsigned i = 0; i != root->children.size(); i++)
      {
      dumpComponentTree(indent, root->children[i]);
      }
   }
void ApsimRegistry::dumpComponentTree(int indent, PTree<Component>* node)
   {
   for (int i=0; i < indent; i++) cout << " ";
   cout << node->item.ID << ":" << node->item.Name << endl;
   for (unsigned i = 0; i != node->children.size(); i++)
      {
      dumpComponentTree(1+indent, node->children[i]);
      }
   }
