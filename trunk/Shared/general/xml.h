//---------------------------------------------------------------------------
#ifndef xmlH
#define xmlH
#include <string>
class XMLNode;

namespace Msxml2_tlb
   {
   class IXMLDOMNode;
   }
class XMLDocumentImpl;
// ------------------------------------------------------------------
// This template provides a tree iterator.
//   It requires of type T the following methods:
//      T* getNextSibling(void) const;
// ------------------------------------------------------------------
template <class T>
struct TreeNodeIterator
   {
   typedef ptrdiff_t   difference_type;
   typedef T value_type;
   typedef T *pointer;
   typedef T &reference;
   typedef std::forward_iterator_tag  iterator_category;

   TreeNodeIterator(const T n)
      : node(n)
      {}
   reference operator* ()
      {return node;}
   pointer operator-> ()
      {return &node;}

   TreeNodeIterator& operator++()
      {
      node = node.getNextSibling();
      return *this;
      }
   TreeNodeIterator operator++(int)
      {
      TreeNodeIterator tmp(*this);
      node = node.getNextSibling();
      return tmp;
      }
   bool operator==(const TreeNodeIterator& rhs) const
      {
      return node.node == rhs.node.node;
      }
   bool operator!=(const TreeNodeIterator& rhs) const
      {
      return node.node != rhs.node.node;
      }

   private:
      T node;
   };
//---------------------------------------------------------------------------
// This class encapsulates an XML document and provides an STL like interface.
//---------------------------------------------------------------------------
class XMLDocument
   {
   public:
      XMLDocument(const std::string& rootNodeName = "DummyRootNode");
      ~XMLDocument(void);

      void readXML(const std::string& xml) throw(std::runtime_error);
      void read(const std::string& fileName) throw (std::runtime_error);
      void write(const std::string& fileName) const;
      void writeXML(std::string& xml) const;
      XMLNode documentElement(void);
   private:
      XMLDocumentImpl* docImpl;

      void throwParseError(void) const throw(std::runtime_error);
   };
//---------------------------------------------------------------------------
// This class encapsulates a node within an XML document
//---------------------------------------------------------------------------
class XMLNode
   {
   public:
      XMLNode(Msxml2_tlb::IXMLDOMNode* n)
         : node(n) { }

      std::string getName(void) const;
      std::string getAttribute(const std::string& attributeName) const;
      std::string getValue(void) const;
      void setAttribute(const std::string& attributeName,
                        const std::string& attributeValue);
      void setValue(const std::string& value, bool asCData = false);
      XMLNode appendChild(const std::string& nodeName, bool alwaysAppend = false);
      void deleteChild(const std::string& childName);

      typedef TreeNodeIterator<XMLNode> iterator;
      iterator begin() const;
      iterator end() const;

      void writeXML(std::string& xml) const;

   private:
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

#endif
