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
// This small structure contains the actual xmlDocument.  This is done to avoid
// including utilcls.h and the Mircosoft XML headers into our header.
//---------------------------------------------------------------------------
struct XMLDocumentImpl
   {
   Msxml2_tlb::TCOMIXMLDOMDocument xmlDoc;
   XMLDocumentImpl(const Msxml2_tlb::TCOMIXMLDOMDocument& d)
      : xmlDoc(d) { }
   };

//---------------------------------------------------------------------------
// Conversion function - from string to variant.
//---------------------------------------------------------------------------
Variant asVariant(const string& st)
   {
   WideString wst(st.c_str());
   return Variant(wst);
   }
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
   Replace_all(xml, "\r\n", "\n");
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

      string st = string("\n") + string(level*3, ' ') +"<";
      xml.replace(pos+1, 1, st);
      pos = xml.find("><", pos);
      }
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& rootNodeName)
   {
   docImpl = new XMLDocumentImpl(CoDOMDocument40::Create());
   docImpl->xmlDoc->appendChild
      (docImpl->xmlDoc->createProcessingInstruction(L"xml", L"version='1.0'"));
   docImpl->xmlDoc->documentElement
      = docImpl->xmlDoc->createElement(asVariant(rootNodeName));
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
// read in contents of document.
//---------------------------------------------------------------------------
void XMLDocument::read(const std::string& fileName) throw (runtime_error)
   {
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find file: " + fileName);

   short isSuccessful;
   docImpl->xmlDoc->load(asVariant(fileName), &isSuccessful);
   if (!isSuccessful)
      throwParseError();

   }
//---------------------------------------------------------------------------
// read in contents of document.
//---------------------------------------------------------------------------
void XMLDocument::readXML(const std::string& xml) throw (runtime_error)
   {
   // Start with an emty DOM
   docImpl->xmlDoc->loadXML(asVariant(xml));
   dirty = false;
   }
//---------------------------------------------------------------------------
// write the contents of this document to the specified file.
//---------------------------------------------------------------------------
void XMLDocument::write(const std::string& fileName) const
   {
   string xml;
   writeXML(xml);
   ofstream out(fileName.c_str());
   out << xml;
   dirty = false;
   }
//---------------------------------------------------------------------------
// write the contents of this document to the specified string.
//---------------------------------------------------------------------------
void XMLDocument::writeXML(std::string& xml) const
   {
   docImpl->xmlDoc->preserveWhiteSpace = false;
   xml = AnsiString(docImpl->xmlDoc->xml).c_str();
   formatXML(xml);
   }
//---------------------------------------------------------------------------
// Throw a formatted exception based on the last parseError
//---------------------------------------------------------------------------
void XMLDocument::throwParseError(void) const throw(runtime_error)
   {
   // We must get info on error
   Msxml2_tlb::IXMLDOMParseError* error = docImpl->xmlDoc->parseError;

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
//---------------------------------------------------------------------------
// return the root document element
//---------------------------------------------------------------------------
XMLNode XMLDocument::documentElement(void)
   {
   return XMLNode(this, docImpl->xmlDoc->documentElement);
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
// Return an attribute of the node.
// ------------------------------------------------------------------
string XMLNode::getAttribute(const std::string& attributeName) const
   {
   if (node != NULL)
      {
      Variant value;
      Msxml2_tlb::IXMLDOMNamedNodeMap* attributes = node->get_attributes();
      if (attributes != NULL)
         {
         Msxml2_tlb::IXMLDOMNode* attribute = attributes->getNamedItem(asVariant(attributeName));
         if (attribute != NULL)
            return asString(attribute->text);
         }
      }
   return "";
   }
// ------------------------------------------------------------------
// Return the value of the node.
// ------------------------------------------------------------------
std::string XMLNode::getValue(void) const
   {
   if (node != NULL)
      {
      Variant value = node->get_text();
      return asString(value);
      }
   return "";
   }
// ------------------------------------------------------------------
// Set an attribute of this node.
// ------------------------------------------------------------------
void XMLNode::setAttribute(const string& attributeName,
                           const string& attributeValue)
   {
   if (node != NULL)
      {
      Msxml2_tlb::IXMLDOMNode* attribute;
      Msxml2_tlb::IXMLDOMNamedNodeMap* attributes = node->get_attributes();
      if (attributes != NULL)
         attribute = attributes->getNamedItem(asVariant(attributeName));
      if (attribute == NULL)
         {
         attribute = node->ownerDocument->createAttribute(asVariant(attributeName));
         attributes->setNamedItem(attribute);
         }

      attribute->set_text(asVariant(attributeValue));
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
      Msxml2_tlb::IXMLDOMCDATASection* section = node->ownerDocument->createCDATASection(asVariant(value));
      node->appendChild(section);
      }
   else
      node->set_text(asVariant(value));
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
   Msxml2_tlb::IXMLDOMElement* childNode = node->get_ownerDocument()
                              ->createElement(asVariant(nodeName));
   node->appendChild(childNode);
   parent->setDirty(true);
   return XMLNode(parent, childNode);
   }
// ------------------------------------------------------------------
// Delete a child node from this node.
// ------------------------------------------------------------------
void XMLNode::deleteChild(const std::string& nodeName)
   {
   iterator child = find_if(begin(), end(), EqualToName<XMLNode>(nodeName));
   if (child != end())
      {
      node->removeChild(child->node);   // presumably we don't have to delete the child.
      parent->setDirty(true);
      }
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
   formatXML(xml);
   }

