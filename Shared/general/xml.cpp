#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "xml.h"
#include "msxml2_ocx.h"
#include "msxml2_tlb.h"
#include <general\stl_functions.h>
#include <general\string_functions.h>
#pragma package(smart_init)

//---------------------------------------------------------------------------
// Conversion function - from string to variant.
//---------------------------------------------------------------------------
Variant asVariant(const string& st)
   {
   WideString wst(st.c_str());
   return Variant(wst);
   }
//---------------------------------------------------------------------------
// This small structure contains the actual xmlDocument.  This is done to avoid
// including utilcls.h and the Mircosoft XML headers into our header.
//---------------------------------------------------------------------------
struct XMLDocumentImpl
   {
   Msxml2_tlb::TCOMIXMLDOMDocument xmlDoc;
   XMLDocumentImpl()
      : xmlDoc(CoDOMDocument40::Create())
      {
      }
   XMLDocumentImpl(const string& fileName)
      : xmlDoc(CoDOMDocument40::Create())
      {
      short isSuccessful;
      xmlDoc->load(asVariant(fileName), &isSuccessful);
      if (!isSuccessful)
         throwParseError();
      }
   XMLDocumentImpl(const string& xml, bool dummy)
      : xmlDoc(CoDOMDocument40::Create())
      {
      xmlDoc->loadXML(WideString(xml.c_str()));
      }

   //---------------------------------------------------------------------------
   // Throw a formatted exception based on the last parseError
   //---------------------------------------------------------------------------
   void throwParseError(void) const throw(runtime_error)
      {
      // We must get info on error
      IXMLDOMParseErrorPtr error = xmlDoc->parseError;

      // Show error line & column, source line and reason
      AnsiString msg = "";
      msg.sprintf("Error on line %d column %d : ", error->line, error->linepos);
      msg += "\r\n";
      msg += error->srcText;
      msg += "\r\n";
      msg += AnsiString::StringOfChar(' ', error->linepos - 1);
      msg += "^\r\n";
      msg += error->reason;
      throw runtime_error(msg.c_str());
      }
   };
//---------------------------------------------------------------------------
// Conversion function - from variant to string
//---------------------------------------------------------------------------
string asString(Variant st)
   {
   return AnsiString(st.bstrVal).c_str();
   }
// ------------------------------------------------------------------
// Format the specified xml string.
// ------------------------------------------------------------------
void formatXML(std::string& xml)
   {
//   Replace_all(xml, ">\r\n", ">");
//   Replace_all(xml, "\t", "");
   int level = 0;
   unsigned pos = xml.find("><");
   while (pos != string::npos)
      {
      // if we're about to close a tag then reduce level.
      if (xml[pos+2] == '/')
         level--;

      // if previous tag was a closure then leave level alone.
      else if (xml[pos-1] == '/' || xml[xml.find_last_of("</", pos)] == '/')
         {
         // dont change level
         }
      else
         level++;

      string st = string("\r\n") + string(max(level*3, 0), ' ') +"<";
      xml.replace(pos+1, 1, st);
      pos = xml.find("><", pos);
      }
   }

//---------------------------------------------------------------------------
// destructor for node.
//---------------------------------------------------------------------------
XMLNode::XMLNode(::XMLDocument* doc, Msxml2_tlb::IXMLDOMNode* n)
   : parent(doc), node(n)
   {
   }
//---------------------------------------------------------------------------
// copy constructor for node
//---------------------------------------------------------------------------
XMLNode::XMLNode(const XMLNode& rhs)
   {
   parent = rhs.parent;
   node = rhs.node;
   if (node != NULL)
      node->AddRef();
   }
//---------------------------------------------------------------------------
// destructor for node.
//---------------------------------------------------------------------------
XMLNode::~XMLNode(void)
   {
   if (node != NULL)
      node->Release();
   }
//---------------------------------------------------------------------------
// assignment operator for a node
//---------------------------------------------------------------------------
XMLNode& XMLNode::operator= (const XMLNode& rhs)
   {
   if (node != NULL)
      node->Release();
   parent = rhs.parent;
   node = rhs.node;
   if (node != NULL)
      node->AddRef();
   return *this;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument()
   {
   docImpl = new XMLDocumentImpl();
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& fileName)
   {
   docImpl = new XMLDocumentImpl(fileName);
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& xml, bool dummy)
   {
   docImpl = new XMLDocumentImpl(xml, dummy);
   dirty = true;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
XMLDocument::~XMLDocument(void)
   {
   delete docImpl;
   }
//---------------------------------------------------------------------------
// set the root node of the document.
//---------------------------------------------------------------------------
void XMLDocument::setRootNode(const std::string& rootNodeName)
   {
   IXMLDOMElementPtr docNode = docImpl->xmlDoc->createElement(WideString(rootNodeName.c_str()));
   docImpl->xmlDoc->documentElement = docNode;
   }
//---------------------------------------------------------------------------
// return the root document element
//---------------------------------------------------------------------------
XMLNode XMLDocument::documentElement(void)
   {
   return XMLNode(this, docImpl->xmlDoc->documentElement);
   }
//---------------------------------------------------------------------------
// write the contents of this document to the specified file.
//---------------------------------------------------------------------------
void XMLDocument::write(const std::string& fileName) const
   {
   string xml;
   writeXML(xml);
   ofstream out(fileName.c_str(), ios::binary);
   out << xml;
   dirty = false;
   }
//---------------------------------------------------------------------------
// write the contents of this document to the specified string.
//---------------------------------------------------------------------------
void XMLDocument::writeXML(std::string& xml) const
   {
   xml = asString(docImpl->xmlDoc->xml);
   formatXML(xml);
   }
//---------------------------------------------------------------------------
// transform the document using the specified stylesheet.
//---------------------------------------------------------------------------
string XMLDocument::transformUsingStyleSheet(const std::string& stylesheetFileName) const
   {
   XMLDocumentImpl* styleDocImpl = new XMLDocumentImpl(stylesheetFileName);
   WideString st = docImpl->xmlDoc->documentElement->transformNode(styleDocImpl->xmlDoc);
   delete styleDocImpl;
   return AnsiString(st).c_str();
   }
//---------------------------------------------------------------------------
// return the name of the node.
//---------------------------------------------------------------------------
string XMLNode::getName(void) const
   {
   if (node != NULL)
      return asString(node->nodeName);
   else
      return "";
   }
// ------------------------------------------------------------------
// Return a list of attributes for node.
// ------------------------------------------------------------------
void XMLNode::getAttributes(std::vector<std::string>& names) const
   {
   if (node != NULL)
      {
      Msxml2_tlb::IXMLDOMNamedNodeMap* attributes = node->get_attributes();
      for (int i = 0; i != attributes->get_length(); i++)
         {
         Msxml2_tlb::IXMLDOMNode* attribute = attributes->get_item(i);
         names.push_back(asString(attribute->nodeName));
         }
      }
   }
// ------------------------------------------------------------------
// Return an attribute of the node.
// ------------------------------------------------------------------
string XMLNode::getAttribute(const std::string& attributeName) const
   {
   string returnString;
   if (node != NULL)
      {
      Variant value;
      Msxml2_tlb::IXMLDOMNamedNodeMap* attributes = node->get_attributes();
      Msxml2_tlb::IXMLDOMNode* attribute = NULL;
      if (attributes != NULL)
         {
         attribute = attributes->getNamedItem(WideString(attributeName.c_str()));
         if (attribute != NULL)
            {
            returnString = asString(attribute->text);
            attribute->Release();
            }
         attributes->Release();
         }
       }
   return returnString;
   }
// ------------------------------------------------------------------
// Return the value of the node.
// ------------------------------------------------------------------
std::string XMLNode::getValue(void) const
   {
   if (node != NULL)
      {
      Variant v = node->get_text();
      return AnsiString(v).c_str();
      }
   return "";
   }
// ------------------------------------------------------------------
// Set an attribute of this node.
// ------------------------------------------------------------------
void XMLNode::setAttribute(const string& attributeName,
                           const string& attributeValue)
   {
   if (node != NULL && attributeValue != "")
      {
      Msxml2_tlb::IXMLDOMDocumentPtr owner = node->ownerDocument;

      Msxml2_tlb::IXMLDOMNamedNodeMap* attributes = node->get_attributes();
      if (attributes != NULL)
         {
         Msxml2_tlb::IXMLDOMNode* attribute = attributes->getNamedItem(WideString(attributeName.c_str()));
         if (attribute != NULL)
            {
            attribute->set_text(WideString(attributeValue.c_str()));
            attribute->Release();
            attributes->Release();
            return;
            }
         }

      IXMLDOMAttributePtr attribute = owner->createAttribute(WideString(attributeName.c_str()));
      attribute->set_text(WideString(attributeValue.c_str()));
      IXMLDOMNodePtr namedItem = attributes->setNamedItem(attribute);

      attributes->Release();
      parent->setDirty(true);
      }
   }
// ------------------------------------------------------------------
// Set the value of this node.  If asCData = true, then the text
// will be added as a CData section.
// ------------------------------------------------------------------
void XMLNode::setValue(const std::string& value, bool asCData)
   {
   if (asCData)
      {
      Msxml2_tlb::IXMLDOMDocumentPtr owner = node->ownerDocument;

      Msxml2_tlb::IXMLDOMCDATASectionPtr section = owner->createCDATASection(WideString(value.c_str()));
      Msxml2_tlb::IXMLDOMNodePtr newNode = node->appendChild(section);
      }
   else
      node->set_text(WideString(value.c_str()));
   parent->setDirty(true);
   }
// ------------------------------------------------------------------
// Add a child node to this node.  If alwaysAppend = true then
// a new node will always be appended.  If alwaysAppend = false then
// a new node will only be appended if it doesn't already exist.
// ------------------------------------------------------------------
XMLNode XMLNode::appendChild(const std::string& nodeName, bool alwaysAppend)
   {
   if (!alwaysAppend)
      {
      iterator i = find_if(begin(), end(),
                           EqualToName<XMLNode>(nodeName));
      if (i != end())
         return *i;
      }
   Msxml2_tlb::IXMLDOMDocumentPtr owner = node->ownerDocument;
   Msxml2_tlb::IXMLDOMElementPtr childNode = owner->createElement(WideString(nodeName.c_str()));
   parent->setDirty(true);
   return XMLNode(parent, node->appendChild(childNode));
   }
// ------------------------------------------------------------------
// Add a child node to this node.  If alwaysAppend = true then
// a new node will always be appended.  If alwaysAppend = false then
// a new node will only be appended if it doesn't already exist.
// ------------------------------------------------------------------
XMLNode XMLNode::appendChild(XMLNode childNode, bool alwaysAppend)
   {
   if (!alwaysAppend)
      {
      iterator i = find_if(begin(), end(),
                           EqualToName<XMLNode>(childNode.getName()));
      if (i != end())
         return *i;
      }
   parent->setDirty(true);
   return XMLNode(parent, node->appendChild(childNode.node));
   }
// ------------------------------------------------------------------
// Delete a child node from this node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::erase(XMLNode::iterator& child)
   {
   if (child != end())
      {
      iterator next = child;
      next++;
      node->removeChild(child->node);   // presumably we don't have to delete the child.
      parent->setDirty(true);
      return next;
      }
   return child;
   }
// ------------------------------------------------------------------
// Return the next sibling.
// ------------------------------------------------------------------
XMLNode XMLNode::getNextSibling(void) const
   {
   if (node != NULL)
      return XMLNode(parent, node->nextSibling);
   else
      return XMLNode(parent, NULL);
   }
// ------------------------------------------------------------------
// Return an iterator to the first child node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::begin() const
   {
   if (node != NULL)
      return XMLNode::iterator(XMLNode(parent, node->firstChild));
   else
      return XMLNode(parent, NULL);
   }
// ------------------------------------------------------------------
// Return an iterator to the last child node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::end() const
   {
   return XMLNode::iterator(XMLNode(parent, NULL));
   }
//---------------------------------------------------------------------------
// write the contents of this node to the specified string.
//---------------------------------------------------------------------------
void XMLNode::writeXML(std::string& xml) const
   {
   xml = AnsiString(node->xml).c_str();
   }

