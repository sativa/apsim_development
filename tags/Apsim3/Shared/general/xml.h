//---------------------------------------------------------------------------
#ifndef xmlH
#define xmlH
#include <string>
#include <stdexcept>
#include <general\VCLAdaptors.h>
#include <general\stl_functions.h>
class XMLNode;

struct XMLDocumentImpl;

namespace Msxml2_tlb
  {
   class IXMLDOMNode;
   }

//---------------------------------------------------------------------------
// This class encapsulates an XML document and provides an STL like interface.
//---------------------------------------------------------------------------
class XMLDocument
   {
   public:
      XMLDocument(void);
      XMLDocument(const std::string& fileName);
      XMLDocument(const std::string& xml, bool dummy);

      ~XMLDocument(void);

      void setRootNode(const std::string& rootNode);
      void write(const std::string& fileName) const;
      void writeXML(std::string& xml) const;
      XMLNode documentElement(void);

      void setDirty(bool d) {dirty = d;}
      bool isDirty(void) const {return dirty;}
   private:
      XMLDocumentImpl* docImpl;
      mutable bool dirty;
   };
//---------------------------------------------------------------------------
// This class encapsulates a node within an XML document
//---------------------------------------------------------------------------
class XMLNode
   {
   public:
      typedef TreeNodeIterator<XMLNode> iterator;
      iterator begin() const;
      iterator end() const;

      XMLNode(::XMLDocument* doc, Msxml2_tlb::IXMLDOMNode* n);
       ~XMLNode(void);
      XMLNode(const XMLNode& rhs);
      XMLNode& operator= (const XMLNode& rhs);

      std::string getName(void) const;
      std::string getAttribute(const std::string& attributeName) const;
      std::string getValue(void) const;
      void setAttribute(const std::string& attributeName,
                        const std::string& attributeValue);
      void setValue(const std::string& value, bool asCData = false);
      XMLNode appendChild(const std::string& nodeName, bool alwaysAppend = false);
      XMLNode::iterator erase(XMLNode::iterator& nodeIterator);

      void writeXML(std::string& xml) const;

   private:
      XMLDocument* parent;
      Msxml2_tlb::IXMLDOMNode* node;

      XMLNode getNextSibling(void) const;

   friend iterator;  // needed so that TreeNodeIterator can get to getNextSibling.
   };

//---------------------------------------------------------------------------
// Handy predicate that can help find a node with a particular name
// and 'name' attribute.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class NodeEquals
   {
   private:
      std::string nodeName;
      std::string nameAttribute;
   public:
      NodeEquals(const std::string& nodename, const std::string& nameattribute)
         : nodeName(nodename), nameAttribute(nameattribute) {}

      bool operator () (T& arg)
         {return (strcmpi(arg.getName().c_str(), nodeName.c_str()) == 0 &&
                  strcmpi(arg.getAttribute("name").c_str(), nameAttribute.c_str()) == 0);};
   };
//---------------------------------------------------------------------------
// Handy predicate that can help find a node that has the specified name
// and attribute values.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class NodeAttributesEquals
   {
   private:
      std::string nodeName;
      std::string attribute1Name;
      std::string attribute1Value;
      std::string attribute2Name;
      std::string attribute2Value;
   public:
      NodeAttributesEquals(const std::string& nodename,
                           const std::string& attribute1name,
                           const std::string& attribute1value,
                           const std::string& attribute2name,
                           const std::string& attribute2value)
         : nodeName(nodename),
           attribute1Name(attribute1name),
           attribute1Value(attribute1value),
           attribute2Name(attribute2name),
           attribute2Value(attribute2value)
            {}

      bool operator () (T& arg)
         {return (strcmpi(arg.getName().c_str(), nodeName.c_str()) == 0 &&
                  strcmpi(arg.getAttribute(attribute1Name).c_str(), attribute1Value.c_str()) == 0 &&
                  strcmpi(arg.getAttribute(attribute2Name).c_str(), attribute2Value.c_str()) == 0);}
   };
//---------------------------------------------------------------------------
// Handy function that will append a new child node IF that node doesn't
// already exist.
//---------------------------------------------------------------------------
XMLNode appendChildIfNotExist(XMLNode& node,
                              const std::string& nodeName,
                              const std::string& nameAttribute)
   {
   XMLNode::iterator childI = std::find_if(node.begin(),
                                           node.end(),
                                           NodeEquals<XMLNode>(nodeName, nameAttribute));
   if (childI != node.end())
      return XMLNode(*childI);
   else
      {
      XMLNode child = node.appendChild(nodeName, true);
      child.setAttribute("name", nameAttribute);
      return child;
      }
   }
//---------------------------------------------------------------------------
// Handy predicate that can help find a node with a particular value.
// e.g. can be used with find_if
//---------------------------------------------------------------------------
template <class T>
class ValueEquals
   {
   private:
      std::string value;
   public:
      ValueEquals(const std::string& v) : value(v) {}

      bool operator () (T& arg)
         {return (strcmpi(arg.getValue().c_str(), value.c_str()) == 0);};
   };
//---------------------------------------------------------------------------
// Handy functor that calls T.getAttribute("name") and stores result in a container.
//---------------------------------------------------------------------------
template <class CT, class T>
class GetNameAttributeFunction
   {
   private:
      CT& Container;
   public:
      GetNameAttributeFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back(arg.getAttribute("name"));
         };
   };

//---------------------------------------------------------------------------
// Handy functor that calls T.getValue() and stores result in a container.
//---------------------------------------------------------------------------
template <class CT, class T>
class GetValueFunction
   {
   private:
      CT& Container;
   public:
      GetValueFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back(arg.getValue());
         };
   };
// ------------------------------------------------------------------
// Handy function that will delete all nodes with the specified name.
// ------------------------------------------------------------------
void eraseNodes(XMLNode& node, const std::string& name)
   {
/*   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 EqualToName<XMLNode>(name));
   while (i != node.end())
      {
      node.erase(i);
      i = find_if(node.begin(),
                  node.end(),
                  EqualToName<XMLNode>(name));
      }
*/   }


#endif
