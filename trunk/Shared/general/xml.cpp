#include "xml.h"
#include <libxml\parser.h>
#include <libxml\tree.h>
#include <general\stl_functions.h>
#include <general\string_functions.h>
#include <general\io_functions.h>
#include <fstream>
#include <sstream>
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& rootNode, RootName rootName)
   {
   doc = xmlNewDoc(BAD_CAST "1.0");
   xmlDocSetRootElement(doc, xmlNewNode(NULL, BAD_CAST "root"));
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& fileName)
   {
   doc = xmlReadFile(fileName.c_str(), NULL, 0);
   dirty = true;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
XMLDocument::XMLDocument(const std::string& xml, XmlContents xmlContents)
   {
   doc = xmlReadMemory(xml.c_str(), xml.size(), "noname.xml", NULL, 0);
   dirty = true;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
XMLDocument::~XMLDocument(void)
   {
   xmlFreeDoc(doc);
   xmlCleanupParser();
   }
//---------------------------------------------------------------------------
// return the root document element
//---------------------------------------------------------------------------
XMLNode XMLDocument::documentElement(void)
   {
   return XMLNode(this, xmlDocGetRootElement(doc));
   }
//---------------------------------------------------------------------------
// write the contents of this document to the specified file.
//---------------------------------------------------------------------------
void XMLDocument::write(const std::string& fileName)
   {
   xmlSaveFormatFile(fileName.c_str(), doc, 1);
   dirty = false;
   }
//---------------------------------------------------------------------------
// constructor for node.
//---------------------------------------------------------------------------
XMLNode::XMLNode(XMLDocument* doc, xmlNode* n)
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
   }
//---------------------------------------------------------------------------
// destructor for node.
//---------------------------------------------------------------------------
XMLNode::~XMLNode(void)
   {
   }
//---------------------------------------------------------------------------
// assignment operator for a node
//---------------------------------------------------------------------------
XMLNode& XMLNode::operator= (const XMLNode& rhs)
   {
   parent = rhs.parent;
   node = rhs.node;
   return *this;
   }
//---------------------------------------------------------------------------
// return the name of the node.
//---------------------------------------------------------------------------
string XMLNode::getName(void) const
   {
   if (node != NULL)
      return (char*) node->name;
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
      xmlAttr* attributes = node->properties;
      xmlAttr* attr = attributes;
      while (attr != NULL)
         {
         names.push_back((char*)attr->name);
         attr = attr->next;
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
      xmlChar* st = xmlGetProp(node, attributeName.c_str());
      returnString = (char*) st;
      xmlFree(st);
      }
   return returnString;
   }
// ------------------------------------------------------------------
// Return the value of the node.
// ------------------------------------------------------------------
std::string XMLNode::getValue(void) const
   {
   if (node != NULL)
      return (char*)node->children->content;
   else
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
      xmlSetProp(node, attributeName.c_str(), attributeValue.c_str());
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
      xmlNode* cdata = xmlNewCDataBlock(node->doc, value.c_str(), value.length());
      xmlAddChild(node, cdata);
      }
   else
      xmlNodeSetContent(node, value.c_str());
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
   xmlNode* newChild = xmlNewChild(node, NULL, nodeName.c_str(), "");
   parent->setDirty(true);
   return XMLNode(parent, newChild);
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
   return XMLNode(parent, xmlAddChild(node, childNode.node));
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
      xmlUnlinkNode(child->node);
      xmlFreeNode(child->node);
      parent->setDirty(true);
      return next;
      }
   return child;
   }
// ------------------------------------------------------------------
// Look at the node passed in. If it is 'valid' then simply return it.
// Otherwise iterate through the siblings until a valid 1 is found.
// Returns NULL if non found.
// ------------------------------------------------------------------
xmlNode* findFirstValidNode(xmlNode* node)
   {              
   while (node != NULL && node->type == XML_TEXT_NODE)
       node = node->next;
   return node;
   }
// ------------------------------------------------------------------
// Return the next sibling.
// ------------------------------------------------------------------
XMLNode XMLNode::getNextSibling(void) const
   {
   if (node != NULL)
      return XMLNode(parent, findFirstValidNode(node->next));
   else
      return XMLNode(parent, NULL);
   }
// ------------------------------------------------------------------
// Return an iterator to the first child node.
// ------------------------------------------------------------------
XMLNode::iterator XMLNode::begin() const
   {
   if (node != NULL)
      return XMLNode::iterator(XMLNode(parent, findFirstValidNode(node->children)));
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
string XMLNode::write() const
   {
   string tempFileName = GetTempDir();
   tempFileName += "\\temp.xml";
   xmlOutputBufferPtr buf = xmlOutputBufferCreateFilename(tempFileName.c_str(), NULL, false);
   xmlNodeDumpOutput(buf, node->doc, node, 0, 1, NULL);
   string returnString = (char*) buf->buffer->content;
   xmlFree(buf);
   return returnString;
   }

