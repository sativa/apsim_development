//////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API 1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// extend IHTMLElement 
/////////////////////////////////////////////////////////////////////////////////////////////////////////
HTMLElement.prototype.swapNode = function (node) {
 try
 {
	var nextSibling = this.nextSibling;
	var parentNode = this.parentNode;
	node.parentNode.replaceChild(this, node);
	  parentNode.insertBefore(node, nextSibling);  
  }
  catch(err) {} 
};

HTMLTableElement.prototype.__defineGetter__("cells", function() 
{
	return this.getElementsByTagName("TD"); 
}); 

HTMLElement.prototype.__defineGetter__("parentElement", function () {
		if (this.parentNode == this.ownerDocument) return null;
		return this.parentNode;
});

var allGetter = function () {
		var a = this.getElementsByTagName("*");
		var node = this;
		a.tags = function (sTagName) {
			return node.getElementsByTagName(sTagName);
		};
		return a;
	};
HTMLDocument.prototype.__defineGetter__("all", allGetter);
HTMLElement.prototype.__defineGetter__("all", allGetter);

HTMLDocument.prototype.__defineGetter__("activeElement", function() {
	return this.documentElement.getAttribute("activeElement"); 	
});
HTMLDocument.prototype.__defineSetter__("activeElement", function(a) {
	this.documentElement.setAttribute("activeElement", a); 
});

var childrenGetter = function() {
	var a = this.childNodes;
	return a; 
}


HTMLElement.prototype.__defineGetter__("children", childrenGetter); 

function convertTextToHTML(s) {
		s = s.replace(/\&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\n/g, "<BR>");
		while (/\s\s/.test(s))
			s = s.replace(/\s\s/, "&nbsp; ");
		return s.replace(/\s/g, " ");
}

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

var allChilds =  function (ID) {
	
	var childs = new Array(); 
	var _all = this.all; 
	var _allLength = _all.length; 
	for(var ielement = 0; ielement < _allLength; ielement++)
	{		
		if(_all[ielement].id == ID)
		{
			childs[childs.length] = _all[ielement]; 
		}		
	}
	return childs; 
};


HTMLDocument.prototype.getChildsById = allChilds; 
HTMLElement.prototype.getChildsById = allChilds; 


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

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// emulate events 
/////////////////////////////////////////////////////////////////////////////////////////////////////////
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
					window.setAttribute("event", e);
					if(e.type == "focus" || e.type == "click" || e.type == "mousedown")
					{
						if(document.activeElement != null && document.activeElement != e.srcElement)
						{				
							if(document.activeElement.blur != null)
								document.activeElement.blur(); 
						}					
						document.activeElement = e.srcElement; 										
					}
				}
				catch(err) {} 
			}, true); 		
		} catch(err) {}
	}
}

emulateEventHandlers(["blur", "change",  "click", "focus", "keydown", "mousedown", "mousemove", "mouseover", "mouseup", "select"]); 