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
   _di_IXMLDocument xmlDoc;
   XMLDocumentImpl()
      : xmlDoc(NewXMLDocument()) { }
   XMLDocumentImpl(const string& fileName)
      : xmlDoc(LoadXMLDocument(AnsiString(fileName.c_str()))) { }
   XMLDocumentImpl(const string& xml, bool dummy)
      : xmlDoc(LoadXMLData(AnsiString(xml.c_str()))) { }
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
   Replace_all(xml, "\r\n", "");
   Replace_all(xml, "\t", "");
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

      string st = string("\n") + string(max(level*3, 0), ' ') +"<";
      xml.replace(pos+1, 1, st);
      pos = xml.find("><", pos);
      }
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument()
   : docElementNode(NULL)
   {
   docImpl = new XMLDocumentImpl();
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& fileName)
   : docElementNode(NULL)
   {
   docImpl = new XMLDocumentImpl(fileName);
   createDocElementNode();
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& xml, bool dummy)
   : docElementNode(NULL)
   {
   docImpl = new XMLDocumentImpl(xml, dummy);
   createDocElementNode();
   dirty = true;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
XMLDocument::~XMLDocument(void)
   {
   delete docElementNode;
   delete docImpl;
   }
//---------------------------------------------------------------------------
// create the document element node.
//---------------------------------------------------------------------------
void XMLDocument::createDocElementNode(void)
   {
   delete docElementNode;
   docElementNode = new XMLNode(this, docImpl->xmlDoc->DocumentElement);
   }
//---------------------------------------------------------------------------
// set the root node of the document.
//---------------------------------------------------------------------------
void XMLDocument::setRootNode(const std::string& rootNodeName)
   {
   docImpl->xmlDoc->ChildNodes->Clear();
   docImpl->xmlDoc->DocumentElement = docImpl->xmlDoc->CreateNode(rootNodeName.c_str());
   createDocElementNode();
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
   xml = docImpl->xmlDoc->XML->Text.c_str();
   formatXML(xml);
   }
//---------------------------------------------------------------------------
// return the name of the node.
//---------------------------------------------------------------------------
string XMLNode::getName(void) const
   {
   if (node != NULL)
      return asString(node->NodeName);
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
      AnsiString value = node->Attributes[attributeName.c_str()];
      return value.c_str();
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
      AnsiString text;
      if (node->ChildNodes->Count == 1)
         text = node->ChildNodes->First()->Text;
      else
         text = node->Text;
      return text.c_str();
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
      node->Attributes[attributeName.c_str()] = AnsiString(attributeValue.c_str());
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
      _di_IXMLNode newNode = node->OwnerDocument->CreateNode(value.c_str(), ntCData);
      node->ChildNodes->Add(newNode);
      }
   else
      node->Text = value.c_str();
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
   _di_IXMLNode childNode = node->AddChild(nodeName.c_str());
   parent->setDirty(true);
   return XMLNode(parent, childNode);
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
      node->ChildNodes->Delete(child->getName().c_str());   // presumably we don't have to delete the child.
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
      return XMLNode(parent, node->ParentNode->ChildNodes->FindSibling(node, 1));
   else
      return XMLNode(parent, NULL);
   }
// ------------------------------------------------------------------
// Return an iterator to the first child node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::begin() const
   {
   if (node != NULL && node->HasChildNodes)
      return XMLNode::iterator(XMLNode(parent, node->ChildNodes->First()));
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
   xml = AnsiString(node->XML).c_str();
   }

