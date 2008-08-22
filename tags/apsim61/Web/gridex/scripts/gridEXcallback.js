/////////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
/////////////////////////////////////////////////////////////////////
var pagestate = null; 
var jgxml = null; 
function commonCallBack(id,response)
{
	try
	{  		
		var g = null;  
		var node = null;   	   	
		if(jgxml == null)
		{
			if(browser.isIE)	
				jgxml = new ActiveXObject("Microsoft.XMLDOM"); 
			else
			{				
				if(document.implementation && document.implementation.createDocument)
					jgxml = document.implementation.createDocument("","",null); 
			}
		}
		if(jgxml == null)
			return; 
		jgxml.loadXML(response);
		node = jgxml.selectSingleNode("GRIDEX//ERROR"); 
		if(node == null)
		{
			node = jgxml.selectSingleNode("GRIDEX//STATE"); 
			document.getElementsByName("__VIEWSTATE")[0].value = node.childNodes[0].nodeValue; 
			 node = jgxml.selectSingleNode("GRIDEX//CSS"); 
			var c = node.childNodes[0].nodeValue; 
			var t = document.getElementById(id+"style"); 
			if(t != null)
			{
				if(browser.isIE)
				{
					t.disabled = true;
					t.id = ""; 
					t.removeNode(); 					
					t = null; 
				}				
			}
			if(t == null)
			{			
				t = document.createElement("style");
				t.type = "text/css";
				t.id = id+"style";
				var head = document.getElementsByTagName("head")[0];
				head.insertBefore(t,null); 		
				var l = document.styleSheets.length; 
				for(var i=0;i<l;i++)
				{
					if(document.styleSheets[i].id == id+"style")
					{
						t = document.styleSheets[i];
						i = l;
					}
				}
			}			
			if(browser.isIE)			
			{
				t.disabled = true;
				t.cssText = c;
				t.disabled = false; 
			}
			else
				t.innerHTML = c;			
			node = jgxml.selectSingleNode("GRIDEX//HTML"); 
			g = getGridEXFromID(id); 
			g.getHtmlGridEX().innerHTML = node.childNodes[0].nodeValue; 					
			node = jgxml.selectSingleNode("GRIDEX//SCRIPT"); 
			eval(node.childNodes[0].nodeValue); 
			g = getGridEXFromID(id); 
			g.gridEX_onload(); 
			g.getHtmlGridEX().style.cursor = "";
			_idiv = null; 
			_irow = null; 						
		}
		else
		{
			alert("Error - " + node.childNodes[0].nodeValue); 
			try { var g = getGridEXFromID(id); g.callBackPending = false; } catch(f) {}
		}
	}
	catch(e)
	{ 				
		var m = ""; 
		if(e.number != null)
			m += e.number;
		if(m.length > 0)
			m += " ";
		if(e.description != null)
			m += e.description; 	
		if(m.length == 0)
			m = e; 		 
		alert("Error - Response " + m); 
		try { var g = getGridEXFromID(id); g.callBackPending = false;  } catch(f) {}
	} 
}
var _idiv = null; 
function updateCallBack(id,html)
{
	if(_idiv == null)
	{		
		var g = getGridEXFromID(id); 
		var h = HtmlDecode(html); 
		h = replaceInstances(h, "%WIDTH%", "100%"); 
		_idiv = g.getRootTable().getHtmlItemsTable().offsetParent; 
		_idiv.innerHTML = h; 		
		if(_idiv.getElementsByTagName("TABLE").length > 0)
		{
			_idiv.getElementsByTagName("TABLE")[0].style.fontFamily = g.getHtmlGridEX().currentStyle.fontFamily; 
			_idiv.getElementsByTagName("TABLE")[0].style.fontSize = g.getHtmlGridEX().currentStyle.fontSize; 
		}
	}	
}
var _irow = null;
function collapseCallBack(id,html,text)
{
	if(_irow == null)
	{		
		var g = getGridEXFromID(id);	
		g.getHtmlGridEX().style.cursor = "wait"; 		
		var e = null;
		if(window.event != null && window.event.srcElement != null)
		{
			e = window.event.srcElement;
			e.style.cursor = "wait"; 
		}
		var r = g.getGridEXRow().getInnerRow(); 
		for(var i=0;i<r.childNodes.length;i++)
		{
			if(r.childNodes[i].nodeType == 1)
				r.childNodes[i].style.cursor = "wait"; 
		}
		_irow = r; 
	}
}
function expandCallBack(id,html,text)
{
	if(_irow == null)
	{
		var g = getGridEXFromID(id);	
		g.getHtmlGridEX().style.cursor = "wait"; 		
		var e = null;
		if(window.event != null && window.event.srcElement != null)
		{
			e = window.event.srcElement;
			if(text != null && text.length > 0)
				e.setAttribute("title", text); 
			e.style.cursor = "wait"; 
		}
		var r = g.getGridEXRow().getInnerRow(); 
		for(var i=0;i<r.childNodes.length;i++)
		{
			if(r.childNodes[i].nodeType == 1)
			{
				if(text != null && text.length > 0)
					r.childNodes[i].setAttribute("title", text); 				
				r.childNodes[i].style.cursor = "wait"; 
			}
		}
		_irow = r; 
	}		
}
function GridEXCallBack(id,url,html,text)
{
	var id = id;
	var html = html; 
	var text = text; 
	var XmlRequest = null; 
	this.DoCallBack = DoCallBack; 	
	function URLEncode(t)
	{		
		if(encodeURIComponent != null)			
			return encodeURIComponent(t); 
		else
		{
			var SAFECHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_.!~*'()";	
			var HEX = "0123456789ABCDEF";
			var f = new Array(); 
			var l = t.length; 
			var g = 0; 
			try
			{
				for (var i = 0; i < l; i++ ) 
				{
					var ch = t.charAt(i);
					if (ch == " ") 
						f[g++] = "+"; 
					else if (SAFECHARS.indexOf(ch) != -1) 
						f[g++] = ch; 
					else 
					{
						var c = ch.charCodeAt(0);
						if (c > 255) 
							f[g++] = "+"; 
						else 
						{
							f[g++]="%";
							f[g++]=HEX.charAt((c >> 4) & 0xF);
							f[g++]=HEX.charAt(c & 0xF);		
						}
					}			  
				} 		 
			}
			catch(err)
			{ return ""; } 
			return f.join(''); 				
		}
	}
	var _eventArgument = ""; 
	var _eventCallBack = null; 
	var _eventUpdate = null; 
	var _eventArgs = null;
	function callBackState()
	{
		if(XmlRequest == null)
			return false;
		switch(XmlRequest.readyState)
		{			
			case 1,2,3:
			{
				if(_eventUpdate != null)
					_eventUpdate(id,html,text);
			} break;
			case 4:
			{
				if(_eventCallBack != null)
				{
					_eventCallBack(id, XmlRequest.responseText); 
					XmlRequest = null; 
				}
			} break; 			
			default:
			{				
				if(_eventUpdate != null)
					_eventUpdate(id,html,text);
			} break; 
		}
		return true; 		
	}	
	function DoCallBackEx()
	{
		try
		{
			XmlRequest = new XMLHttpRequest(); 
		}
		catch(e)
		{
			try
			{
				XmlRequest = new ActiveXObject("Microsoft.XMLHTTP");					
			}
			catch(e)
			{ return false; }
		}		
		var postDATA = "gxCB=true&gxCBI="+id+"&gxCBE="+_eventArgument;
		var g = getGridEXFromID(id); 
		g.callBackPending = true; 
		var form = document.getElementsByName(g.formID)[0]; 
		for(var i=0;i<form.length;++i) 
		{
			var element = form.elements[i];
			if (element.name) 
			{
				var elementValue = null;
				if (element.nodeName == "INPUT") 
				{
					var inputType = element.getAttribute("TYPE").toUpperCase();
					if (inputType == "TEXT" || inputType == "PASSWORD" || inputType == "HIDDEN") 
						elementValue = element.value;
					else if (inputType == "CHECKBOX" || inputType == "RADIO") 
					{
						if (element.checked)
							elementValue = element.value;						
					}
				} 
				else if (element.nodeName == "SELECT") 
					elementValue = element.value;		
				if(element.name != "__EVENTTARGET" && element.name != "__EVENTARGUMENT")		
				{
					if (elementValue) 
						postDATA += "&" + element.name + "=" + URLEncode(elementValue);				
					else
						postDATA += "&" + element.name + "="; 
				}
			}
		}		
		XmlRequest.onreadystatechange = callBackState;
		XmlRequest.open("POST",url,true);
		XmlRequest.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		XmlRequest.send(postDATA); 		
	}
	function DoCallBack(eventArgument,eventCallBack,eventUpdate,eventArgs)
	{
		_eventCallBack = eventCallBack; 		
		_eventUpdate = eventUpdate;
		_eventArgument = eventArgument; 
		_eventArgs = eventArgs;
		if(_eventUpdate != null)
		{
			_eventUpdate(id,html,text);
			window.setTimeout(DoCallBackEx,10); 
		}
		else
			DoCallBackEx();		
	}
}