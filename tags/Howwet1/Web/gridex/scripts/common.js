///////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////
function hideBackFrame(id)
	{
		var e = document.getElementById(id);
		if(e != null)
			e.style.visibility = "hidden"; 
	}
	function showBackFrame(id, left, top, width, height)
	{
		var e = document.getElementById(id); 
		if(e == null)
		{
			try
			{
				var frameSrc = "javascript:void(0);";
				e = document.createElement("IFRAME");			
				document.body.appendChild(e); 
				e.frameBorder = 0; 
				e.id = id;
				e.border = 0; 
				e.style.border = "none"; 
				e.style.visibility = "hidden"; 
				e.style.position = "absolute";
				e.style.zIndex = 999;
			}
			catch(err)
			{ } 
		}   		
		e.style.pixelLeft = left;
		e.style.pixelTop = top;
		e.style.pixelWidth = width;
		e.style.pixelHeight = height; 
		e.style.visibility = "visible"; 
	}
var activeElement = null;
function cancelEvent()
{
	window.event.returnValue = false;
	window.event.cancelBubble = true;
	return false; 
}
function getPixelValue(value)
{
	if(value == null || value.length == 0)
		return 0; 		
	var p = 0; 	
	var i = value.indexOf("px"); 	
	if(i > 0)
		p = parseInt(value.substr(0, i)); 
	else
	{
		try {  p = parseInt(value, 10); }
		catch(err)
		{
			p = 0; 
		}
	}		
	return p; 
}
function getPixelTop(element)
{
	var top = 0;
	while(element != null)
	{					
		top += element.offsetTop;
		element = element.offsetParent; 				
	}
	return top; 
}
function getPixelLeft(element)
{
	var left = 0; 
	while(element != null)
	{				
		left += element.offsetLeft;				
		element = element.offsetParent; 
	}
	return left; 
}
function trim(text)
{        	
	if(text == null)
		return ""; 
	var charCode = -1; 	
	do
	{
		charCode = text.substring(0,1).charCodeAt();
		if(charCode == 32 || charCode == 160)
			text = text.substring(1, text.length);			
	} while(charCode == 32 || charCode == 160); 
    do
    {
		charCode = text.substring(text.length-1,text.length); 
		if(charCode == 32 || charCode == 160)
			text = text.substring(0, text.length-1); 			
    } while(charCode == 32 || charCode == 160);    
   return text;
}
function fireEventEx(events,name,params)
{
	if(events == null || events.length == 0)
		return null;
			
	var l = events.length; 
	var handler = "";		
	for(var i=0; i<l; i=i+2)
	{
		if(events[i] == name) 
		{
			handler = events[i+1];
			i = l; 
		}
	}
	if(handler != "")
	{	
		var _params = "";
		var l = (params == null) ? 0 : params.length; 
		for(var i=0;i<l; i++)
		{
			if(_params != "")
				 _params += ",";
			_params += "params[" + i + "]";
		}		
		var _cmd = "return eval(" + handler + "("  + _params + "))";
		var _function = new Function("params", _cmd);
		return _function(params);
	}
	return null; 
}
function getKeyCode(e, i)
{
	var c = window.event.keyCode; 			
	if(i.selectionStart != null)
		c = window.event.which; 
	return c; 
}
function getItem(collection, id)
{
	var l = collection.length; 
	for(var i = 0; i < l; i++)
	{
		if(collection.item(i).getAttribute("id") == id)
			return collection.item(i); 
	}
	return null;
}
var browser = new function()
{
	this.isNetscape = (navigator.appName == "Netscape");
	this.isIE = ((navigator.userAgent.indexOf("MSIE 5.5") != -1) && (navigator.userAgent.indexOf("Windows") != -1)) || ((navigator.userAgent.indexOf("MSIE 6.0") != -1) && (navigator.userAgent.indexOf("Windows") != -1)) || ((navigator.userAgent.indexOf("MSIE 7") != -1) && (navigator.userAgent.indexOf("Windows") != -1)); 
	this.getCurrentStyle = getCurrentStyle; 
	this.handleEvent = handleEvent; 
	function handleEvent(element, eventName, handler)
	{
		try
		{
			if(this.isIE)
				element.attachEvent("on"+eventName, handler); 			
			else
				element.addEventListener(eventName, handler, false); 
		}
		catch(err)
		{ } 		
	}
	function getCurrentStyle(element, attribute)
	{
		try
		{
			if(this.isIE)
				return element.currentStyle.getAttribute(attribute); 
			else
				return document.defaultView.getComputedStyle(element,null).getPropertyValue(attribute); 
		}
		catch(err)
		{ } 
	}		
	if(navigator.appName == "Netscape" || document.defaultView != null)
	{
		var loadXML = function(xml)
		{
			var objDOMParser = new DOMParser();        
			var objDoc = objDOMParser.parseFromString(xml, "text/xml");        
			while (this.hasChildNodes())
				this.removeChild(this.lastChild);            
			for (var i=0; i < objDoc.childNodes.length; i++) 
			{            				
				var objImportedNode = this.importNode(objDoc.childNodes[i], true);                        
				this.appendChild(objImportedNode);        
			} 
		}
		var allChilds =  function (ID) 
		{	
			return allChildsCore(ID, this.all); 
		};		
		function allChildsCore(ID, _all)
		{	
			var childs = new Array(); 	
			var _allLength = _all.length; 
			for(var i = 0; i < _allLength; i++)
			{		
				if(_all[i].id == ID)
					childs[childs.length] = _all[i]; 
			}
			return childs; 
		}		
		var allGetter = function () 
		{
			var a = this.getElementsByTagName("*");
			var node = this;
			a.tags = function (sTagName) 
			{
				return node.getElementsByTagName(sTagName);
			};
			return a;
		};	
		var childrenGetter = function() 
		{
			var a = this.childNodes;
			return a; 
		}	
		function convertTextToHTML(s) {
			s = s.replace(/\&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\n/g, "<BR>");
			while (/\s\s/.test(s))
				s = s.replace(/\s\s/, "&nbsp; ");
			return s.replace(/\s/g, " ");
		}		
		if(navigator.appName == "Netscape")
		{
			CSSStyleDeclaration.prototype.__defineGetter__("pixelLeft", function() {    return parseInt(this.left) || 0;});
			CSSStyleDeclaration.prototype.__defineSetter__("pixelLeft", function (value) {    this.left = value + "px";});
			CSSStyleDeclaration.prototype.__defineGetter__("pixelHeight", function() {    return parseInt(this.height) || 0;});
			CSSStyleDeclaration.prototype.__defineSetter__("pixelHeight", function (value) {    this.height = value + "px";});
			CSSStyleDeclaration.prototype.__defineGetter__("pixelTop", function() {    return parseInt(this.top) || 0;});
			CSSStyleDeclaration.prototype.__defineSetter__("pixelTop", function (value) {    this.top = value + "px";});
			CSSStyleDeclaration.prototype.__defineGetter__("pixelWidth", function() {    return parseInt(this.width) || 0;});
			CSSStyleDeclaration.prototype.__defineSetter__("pixelWidth", function (value) {    this.width = value + "px";});
		}
		Document.prototype.loadXML = loadXML; 
		if( document.implementation.hasFeature("XPath", "3.0") )
		{
			XMLDocument.prototype.selectNodes = function(cXPathString, xNode)
			{
				if( !xNode ) { xNode = this; } 
				var oNSResolver = this.createNSResolver(this.documentElement)
				var aItems = this.evaluate(cXPathString, xNode, oNSResolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null)
				var aResult = [];
				for( var i = 0; i < aItems.snapshotLength; i++)
				{
					aResult[i] =  aItems.snapshotItem(i);
				}
				return aResult;
			}
			Element.prototype.selectNodes = function(cXPathString)
			{
				if(this.ownerDocument.selectNodes)
				{
					return this.ownerDocument.selectNodes(cXPathString, this);
				}
				else{throw "For XML Elements Only";}
			}
			XMLDocument.prototype.selectSingleNode = function(cXPathString, xNode)
			{
				if( !xNode ) { xNode = this; } 
				var xItems = this.selectNodes(cXPathString, xNode);
				if( xItems.length > 0 )
				{
					return xItems[0];
				}
				else
				{
					return null;
				}
			}   
			Element.prototype.selectSingleNode = function(cXPathString)
			{	
				if(this.ownerDocument.selectSingleNode)
				{
					return this.ownerDocument.selectSingleNode(cXPathString, this);
				}
				else{throw "For XML Elements Only";}
			}
		}
		HTMLDocument.prototype.getChildsById = allChilds; 
		HTMLDocument.prototype.__defineGetter__("all", allGetter);
		HTMLDocument.prototype.__defineGetter__("activeElement", function() {
			return activeElement ;			
		});
		HTMLDocument.prototype.__defineSetter__("activeElement", function(a) {					
			activeElement = a;			
		});
		HTMLElement.prototype.getChildsById = allChilds; 				
		HTMLElement.prototype.__defineGetter__("all", allGetter);		
		HTMLElement.prototype.swapNode = function (node) {
			try
			{
				var nextSibling = this.nextSibling;
				var parentNode = this.parentNode;
				node.parentNode.replaceChild(this, node);			
				parentNode.insertBefore(node, nextSibling);  
			}
			catch(err) {  } 
		};
		HTMLElement.prototype.__defineSetter__("innerText", function (sText) {
			this.innerHTML = convertTextToHTML(sText);
			return sText;		
		});
		var tmpGet;
		HTMLElement.prototype.__defineGetter__("innerText", tmpGet = function () {
			var r = this.ownerDocument.createRange();
			r.selectNodeContents(this);
			return r.toString();
		});
		HTMLElement.prototype.contains = function (oEl) {
			if (oEl == this) return true;
			if (oEl == null) return false;
			return this.contains(oEl.parentNode);		
		};
		HTMLElement.prototype.__defineGetter__("canHaveChildren", function () {
			switch (this.tagName) {
				case "AREA":
				case "BASE":
				case "BASEFONT":
				case "COL":
				case "FRAME":
				case "HR":
				case "IMG":
				case "BR":
				case "INPUT":
				case "ISINDEX":
				case "LINK":
				case "META":
				case "PARAM":
				return false;
			}
			return true;
		});
		HTMLElement.prototype.__defineGetter__("outerHTML", function () {
			var attr, attrs = this.attributes;
			var str = "<" + this.tagName;
			for (var i = 0; i < attrs.length; i++) {
				attr = attrs[i];
				if (attr.specified)
					str += " " + attr.name + '="' + attr.value + '"';
			}
			if (!this.canHaveChildren)
				return str + ">";		
			return str + ">" + this.innerHTML + "</" + this.tagName + ">";
		});
		HTMLElement.prototype.__defineGetter__("parentElement", function () {
				if (this.parentNode == this.ownerDocument) 
					return null;
				return this.parentNode;
		});
		HTMLTableElement.prototype.__defineGetter__("cells", function() {
			return this.getElementsByTagName("TD"); 
		}); 
		HTMLElement.prototype.contains = function (oEl) 
		{
			if (oEl == this) return true;
			if (oEl == null) return false;
			return this.contains(oEl.parentNode);		
		};
		HTMLElement.prototype.__defineGetter__("currentStyle", function() {    return getComputedStyle(this, null);});
		HTMLElement.prototype.__defineGetter__("children", childrenGetter); 
		Event.prototype.__defineGetter__("srcElement", function () {
			var node = this.target;   
			while (node != null && node.nodeType != 1) node = node.parentNode;
			return node;
		});
		Event.prototype.__defineGetter__("toElement", function () {
			var node;
			if (this.type == "mouseout")
				node = this.relatedTarget;
			else if (this.type == "mouseover")
				node = this.target;
			else
				return null;
			var node = this.target;
			while (node != null && node.nodeType != 1) node = node.parentNode;
			return node;
		});
		Event.prototype.__defineGetter__("button", function() {    return (this.which == 1) ? 1 : (this.which == 2) ? 4 : 2;});
		Event.prototype.__defineSetter__("cancelBubble", function (b) {
			if (b) this.stopPropagation();
			return b;
		});	
		Event.prototype.__defineSetter__("returnValue", function (b) {
			if (!b) this.preventDefault();
			return b;
		});
		Event.prototype.__defineGetter__("offsetX", function () {
			return this.layerX;
		});
		Event.prototype.__defineGetter__("offsetY", function () {
			return this.layerY;
		});
		function emulateEventHandlers(eventNames)
		{
			for(var i=0; i<eventNames.length; i++)
			{		
				try
				{
					document.addEventListener(eventNames[i], function (e) {
						var err;
						try
						{
							window.event = e;							
							if(e.type == "focus" || e.type == "click" || e.type == "mousedown" || e.type == "select" || e.type == "scroll") 
							{												
								if(document.activeElement != null && document.activeElement != e.srcElement)
								{				
									var element = document.activeElement; 
									document.activeElement = e.srcElement; 											
									if(element != null && element.tagName == "SELECT")
										return;
									if(element.onblur != null)
										element.onblur();
									else if(element.blur != null)
										element.blur(); 																		
								}
								else								
									document.activeElement = e.srcElement;																	
							}							
						}
						catch(err) {    } 
					}, true); 		
				} catch(err) {   }
			}
		}
		emulateEventHandlers(["blur", "change",  "click", "focus", "keydown", "mousedown", "mousemove", "mouseover", "mouseup", "select", "scroll"]); 
	}
}