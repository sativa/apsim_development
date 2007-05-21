//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
var pendingBlur = false;
var dragcolumn = null; 
var resizeline = null;  
var canceledByUser = false; 
var columnResizing = false; 
var columnDraging = false; 
var couldDragColumn = null;
var couldDragHeader = null;
var couldResizeColumn = null;  
var couldResizeColumnSet = null; 
var couldResizeHeader = null; 
var couldStartDrag = false;
var couldStartResize = false; 
var dragpoint = null; 
var resizepoint = null; 
var columnresizeMode = -1; 
var columndragMode = -1;
var currpressedcolumn = null; 
var currcolumn = null; 
var currgroupbybox = null; 
var groupnewpos = null; 
var currgrouppos = null;
var currgrouptable = null; 
var currcolumnset = null; 
var currheader = null;
var columnfordrop = null;
var droptarget = -1;
var divtt = null;
function body_onmousemove()
{
	if(couldStartDrag && dragpoint != null)
	{		
		if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
		{	
			startColumnDrag(couldDragColumn, couldDragHeader);
			couldStartDrag = false;
			couldDragColumn = couldDragHeader = null; 
			dragpoint = null; 
		}			
		window.event.cancelBubble = true; 
	}
	else if(columnDraging)
	{
		if(browser.isIE)
			showColumnDrag();
		else
			drag_onmousemove(event);
		window.event.cancelBubble = true; 
	}
}
function gridEX_onmousemove()
{
	if(columnResizing)
		showResizeLine();
	else if(columnDraging)
	{
		if(browser.isIE)
			showColumnDrag();
		else
			drag_onmousemove(event);
	}
}
function gridEX_onmouseup()
{
	if(window.event.button == 1 || !browser.isIE)
	{
		if(columnResizing)
		{
			if(columnresizeMode == 1)
				endColumnResize();
			else if(columnresizeMode == 2)
				endColumnSetResize(); 					
		}
		else if(columnDraging)
		{
			if(!browser.isIE)
				drag_onmouseup();
				
			cancelColumnDraging();
		}
	}	
}
function onSelectStartCore(id)
{
	var g = getGridEXFromID(id); 
	var c = g.FireEvent("SelectionStart", [g]); 
	if(c != null && c)
		return; 		
	if(window.event.srcElement != null)
	{
		var element = window.event.srcElement;
		if(element.tagName != null)
		{
			if(element.tagName == "INPUT" && element.type != null && (element.type == "text" || element.type == "password"))
					return;
				if(element.tagName == "TEXTAREA")
					return;		
		}
	}
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return false;
}
function gridEX_onselectstart(id)
{	
	return onSelectStartCore(id); 		
}
function header_onselectstart()
{		
	window.event.cancelBubble = true;
	window.event.returnValue = false; 
	return true;
}
function header_onmousedown() { var element = window.event.srcElement; }
function hcscolumn_oncontextmenu()
{
	window.event.cancelBubble = true; 
	window.event.returnValue = false; 
}
function hcscolumn_onselectstart()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return true; 
}
function retrieveColumnSet(value)
{	
	var values = value.split("|"); 
	var h = retrieveHeader(values[0]+"|"+values[1]+"|"+values[2]); 
	var c = h.getColumnSets().getColumnSetInIndex([Number(values[3])]); 
	return c;	
}
function hcscolumn_onmousemove()
{
	var c = getColumnFromElement(window.event.srcElement);
	retrieveColumnSet(c.getAttribute("columnset")).column_onmousemove();
}
function hcscolumn_onmouseout()
{
	var c = getColumnFromElement(window.event.srcElement); 
	retrieveColumnSet(c.getAttribute("columnset")).column_onmouseout(); 
}
function hcscolumn_onmouseover()
{
	var c = getColumnFromElement(window.event.srcElement);
	retrieveColumnSet(c.getAttribute("columnset")).column_onmouseover();
}
function hcscolumn_onmousedown()
{	
	var c = getColumnFromElement(window.event.srcElement);
	retrieveColumnSet(c.getAttribute("columnset")).column_onmousedown();
}
function hcscolumn_onmouseup()
{	
	var c = getColumnFromElement(window.event.srcElement);
	retrieveColumnSet(c.getAttribute("columnset")).column_onmouseup();
}
function hcscolumn_ondblclick()
{
	var c = getColumnFromElement(window.event.srcElement);
	retrieveColumnSet(c.getAttribute("columnset")).column_ondblclick();
}
function hcscolumn_onclick()
{
	var c = getColumnFromElement(window.event.srcElement); 
	retrieveColumnSet(c.getAttribute("columnset")).column_onclick(); 
}
function retrieveGroupByBox()
{
	var element = window.event.srcElement;
	while(element != null && element.getAttribute("gi") == null)
		element = element.parentElement; 
	if(element != null)
	{
		var g = getGridEXFromID(element.getAttribute("gi")); 
		return g.getGroupByBox();
	}
	return null; 
}
function ggroupbybox_onclick()
{	
	var g = retrieveGroupByBox();
	if(g != null)
		g.groupbybox_onclick(); 
}
function ggroupbyboxinfotext_onclick()
{
	var g = retrieveGroupByBox();
	if(g != null)
		g.groupbyboxinfotext_onclick(); 
}
function table_onmousemove()
{
	if(couldStartDrag && dragpoint != null)
	{		
		if(Math.abs(window.event.clientX - dragpoint.X()) > 4 || Math.abs(window.event.clientY - dragpoint.Y()) > 4)
		{	
			startColumnDrag(couldDragColumn, couldDragHeader);
			couldStartDrag = false;
			couldDragColumn = couldDragHeader = null; 
			dragpoint = null; 				
			window.event.cancelBubble = true; 
		}
	}
	else if(columnDraging)
	{
		if(browser.isIE)
			showColumnDrag(); 
		else
			drag_onmousemove(event);
		window.event.cancelBubble = true; 
	}					
}
function table_onselectstart()
{
	var element = window.event.srcElement; 
	while(element != null && element.getAttribute("gid") == null)
		element = element.parentElement;
	if(element != null)
		return onSelectStartCore(element.getAttribute("gid"));	
}
function gtable_onblur()
{
	var e = window.event.srcElement;
	if(e.getAttribute("table") != null)	
	{
		var values = e.getAttribute("table").split("|"); 
		var g = getGridEXFromID(values[0]);
		var t = g.getTables().getTableByID(values[1]);
		t.table_onblur();		
	}
}
function gtable_onscroll()
{	
	var e = window.event.srcElement;
	if(e.getAttribute("table") != null)		
	{
		var values = e.getAttribute("table").split("|"); 
		var g = getGridEXFromID(values[0]);
		var t = g.getTables().getTableByID(values[1]);
		t.table_onscroll();
	}
}
function gcolumnset_onselectstart()
{		
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return true; 
}	
function gbbcolumn_onmousedown()
{
	var c = getColumnFromElement(window.event.srcElement); 
	if(c == null)
		return; 
	retrieveGroupByBox(c.getAttribute("gi")).column_onmousedown();
}
function gbbcolumn_onmouseup()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveGroupByBox(c.getAttribute("gi")).column_onmouseup();
}
function gbbcolumn_onmousemove()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveGroupByBox(c.getAttribute("gi")).column_onmousemove();
}
function gbbcolumn_onmouseover()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveGroupByBox(c.getAttribute("gi")).column_onmouseover(); 
}
function gbbcolumn_onmouseout()
{
	var c = getColumnFromElement(window.event.srcElement); 
	if(c == null)
		return; 
	retrieveGroupByBox(c.getAttribute("gi")).column_onmouseout(); 
}
function retrieveHeader(value)
{
	var values = value.split("|");
	var g = getGridEXFromID(values[0]); 
	var t = g.getTables().getTableByID(values[1]); 
	var h = t.getHeaders(); 
	var i = Number(values[2]); 
	if(h.length != null)
		return h[i];
	else
		return h; 
}
function hcolumn_onmousedown()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_onmousedown();
}
function hcolumn_onmousemove()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_onmousemove();
}
function hcolumn_onmouseover()
{	
	var c = getColumnFromElement(window.event.srcElement);	
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_onmouseover();
}
function hcolumn_onmouseout()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_onmouseout(); 
}
function hcolumn_onmouseup()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_onmouseup();
}
function hcolumn_onclick()
{
	var c = getColumnFromElement(window.event.srcElement); 
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_onclick(); 
}
function hcolumn_ondblclick()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_ondblclick();
}
function hcolumn_oncontextmenu()
{
	var c = getColumnFromElement(window.event.srcElement);
	if(c == null)
		return; 
	retrieveHeader(c.getAttribute("header")).column_oncontextmenu();
}
function ggridEX_onload(id)
{
	var g = getGridEXFromID(id);	
	g.gridEX_onload(); 
}
function ggridEX_onsubmit(id)
{
	var g = getGridEXFromID(id);	
	g.gridEX_onsubmit();
}
function ggridEX_onunload(id)
{
	var g = getGridEXFromID(id);	
	g.gridEX_onunload();
}
var gpsizes = null; 
function ggridEX_onresize(id)
{
	if(gpsizes === null)
		gpsizes = new Array(); 
	var gpsize = gpsizes[id]; 
	if(gpsize == null || (gpsize[0] != window.document.body.clientWidth || gpsize[1] != window.document.body.clientHeight))
	{
		var g = getGridEXFromID(id);	
		g.gridEX_onresize();
		if(gpsize == null)
			gpsize = new Array(); 
		gpsize[0] = window.document.body.clientWidth;
		gpsize[1] = window.document.body.clientHeight; 
		gpsizes[id] = gpsize; 
	}
	else
		return;
}
function ggridEX_onblur(id)
{
	var g = getGridEXFromID(id);	
	g.gridEX_onblur();
}
function gbody_onselectstart(id)
{
	var g = getGridEXFromID(id);	
	g.body_onselectstart();
}
function ggridEX_onkeydown(id)
{
	var g = getGridEXFromID(id);	
	g.gridEX_onkeydown();
}
function ggridEX_onmousewheel(id)
{
	var g = getGridEXFromID(id);	
	g.gridEX_onmousewheel();
}
function Point(x, y)
{
	var x = x;
	var y = y; 	
	this.X = X;
	this.Y = Y;
	function X() { return x; }	
	function Y() { return y;  }
}
var _imgToUpdate = null; 
function updateImage(src)
{
	if(_imgToUpdate != null)
		_imgToUpdate.src = src; 
		
	_imgToUpdate = null; 
}
function PasswordText(text,passwordChar)
{
	var m = "";
	for(var i=0;i<text.length;i++)
		m += passwordChar;
	return m; 
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
function HtmlDecode(s) 
{ 
      var out = ""; 
      if (s==null) return;   
      var l = s.length; 
      for (var i=0; i<l; i++) 
      { 
            var ch = s.charAt(i);             
            if (ch == '&') 
            { 
				var semicolonIndex = s.indexOf(';', i+1);                   
				if (semicolonIndex > 0) 
				{ 
					var entity = s.substring(i + 1, semicolonIndex); 
                    if (entity.length > 1 && entity.charAt(0) == '#') 
                    { 
						if(entity.charAt(1) == 'x' || entity.charAt(1) == 'X') 
							ch = String.fromCharCode(eval('0'+entity.substring(1))); 
                        else 
                            ch = String.fromCharCode(eval(entity.substring(1))); 
                    } 
                    else 
                    { 
						switch (entity) 
                        { 
							case 'quot': ch = String.fromCharCode(0x0022); break; 
                            case 'amp': ch = String.fromCharCode(0x0026); break; 
                            case 'lt': ch = String.fromCharCode(0x003c); break; 
                            case 'gt': ch = String.fromCharCode(0x003e); break; 
                            case 'nbsp': ch = String.fromCharCode(0x00a0); break; 
                            case 'iexcl': ch = String.fromCharCode(0x00a1); break; 
                            case 'cent': ch = String.fromCharCode(0x00a2); break; 
                            case 'pound': ch = String.fromCharCode(0x00a3); break; 
                            case 'curren': ch = String.fromCharCode(0x00a4); break; 
                            case 'yen': ch = String.fromCharCode(0x00a5); break; 
                            case 'brvbar': ch = String.fromCharCode(0x00a6); break; 
                            case 'sect': ch = String.fromCharCode(0x00a7); break; 
                            case 'uml': ch = String.fromCharCode(0x00a8); break; 
                            case 'copy': ch = String.fromCharCode(0x00a9); break; 
                            case 'ordf': ch = String.fromCharCode(0x00aa); break; 
                            case 'laquo': ch = String.fromCharCode(0x00ab); break; 
                            case 'not': ch = String.fromCharCode(0x00ac); break; 
                            case 'shy': ch = String.fromCharCode(0x00ad); break; 
                            case 'reg': ch = String.fromCharCode(0x00ae); break; 
                            case 'macr': ch = String.fromCharCode(0x00af); break; 
                            case 'deg': ch = String.fromCharCode(0x00b0); break; 
                            case 'plusmn': ch = String.fromCharCode(0x00b1); break; 
                            case 'sup2': ch = String.fromCharCode(0x00b2); break; 
                            case 'sup3': ch = String.fromCharCode(0x00b3); break; 
                            case 'acute': ch = String.fromCharCode(0x00b4); break; 
                            case 'micro': ch = String.fromCharCode(0x00b5); break; 
                            case 'para': ch = String.fromCharCode(0x00b6); break; 
                            case 'middot': ch = String.fromCharCode(0x00b7); break; 
                            case 'cedil': ch = String.fromCharCode(0x00b8); break; 
                            case 'sup1': ch = String.fromCharCode(0x00b9); break; 
                            case 'ordm': ch = String.fromCharCode(0x00ba); break; 
                            case 'raquo': ch = String.fromCharCode(0x00bb); break; 
                            case 'frac14': ch = String.fromCharCode(0x00bc); break; 
                            case 'frac12': ch = String.fromCharCode(0x00bd); break; 
                            case 'frac34': ch = String.fromCharCode(0x00be); break; 
                            case 'iquest': ch = String.fromCharCode(0x00bf); break; 
                            case 'Agrave': ch = String.fromCharCode(0x00c0); break; 
                            case 'Aacute': ch = String.fromCharCode(0x00c1); break; 
                            case 'Acirc': ch = String.fromCharCode(0x00c2); break; 
                            case 'Atilde': ch = String.fromCharCode(0x00c3); break; 
                            case 'Auml': ch = String.fromCharCode(0x00c4); break; 
                            case 'Aring': ch = String.fromCharCode(0x00c5); break; 
                            case 'AElig': ch = String.fromCharCode(0x00c6); break; 
                            case 'Ccedil': ch = String.fromCharCode(0x00c7); break; 
                            case 'Egrave': ch = String.fromCharCode(0x00c8); break; 
                            case 'Eacute': ch = String.fromCharCode(0x00c9); break; 
                            case 'Ecirc': ch = String.fromCharCode(0x00ca); break; 
                            case 'Euml': ch = String.fromCharCode(0x00cb); break; 
                            case 'Igrave': ch = String.fromCharCode(0x00cc); break; 
                            case 'Iacute': ch = String.fromCharCode(0x00cd); break; 
                            case 'Icirc': ch = String.fromCharCode(0x00ce ); break; 
                            case 'Iuml': ch = String.fromCharCode(0x00cf); break; 
                            case 'ETH': ch = String.fromCharCode(0x00d0); break; 
                            case 'Ntilde': ch = String.fromCharCode(0x00d1); break; 
                            case 'Ograve': ch = String.fromCharCode(0x00d2); break; 
                            case 'Oacute': ch = String.fromCharCode(0x00d3); break; 
                            case 'Ocirc': ch = String.fromCharCode(0x00d4); break; 
                            case 'Otilde': ch = String.fromCharCode(0x00d5); break; 
                            case 'Ouml': ch = String.fromCharCode(0x00d6); break; 
                            case 'times': ch = String.fromCharCode(0x00d7); break; 
                            case 'Oslash': ch = String.fromCharCode(0x00d8); break; 
                            case 'Ugrave': ch = String.fromCharCode(0x00d9); break; 
                            case 'Uacute': ch = String.fromCharCode(0x00da); break; 
                            case 'Ucirc': ch = String.fromCharCode(0x00db); break; 
                            case 'Uuml': ch = String.fromCharCode(0x00dc); break; 
                            case 'Yacute': ch = String.fromCharCode(0x00dd); break; 
                            case 'THORN': ch = String.fromCharCode(0x00de); break; 
                            case 'szlig': ch = String.fromCharCode(0x00df); break; 
                            case 'agrave': ch = String.fromCharCode(0x00e0); break; 
                            case 'aacute': ch = String.fromCharCode(0x00e1); break; 
                            case 'acirc': ch = String.fromCharCode(0x00e2); break; 
                            case 'atilde': ch = String.fromCharCode(0x00e3); break; 
                            case 'auml': ch = String.fromCharCode(0x00e4); break; 
                            case 'aring': ch = String.fromCharCode(0x00e5); break; 
                            case 'aelig': ch = String.fromCharCode(0x00e6); break; 
                            case 'ccedil': ch = String.fromCharCode(0x00e7); break; 
                            case 'egrave': ch = String.fromCharCode(0x00e8); break; 
                            case 'eacute': ch = String.fromCharCode(0x00e9); break; 
                            case 'ecirc': ch = String.fromCharCode(0x00ea); break; 
                            case 'euml': ch = String.fromCharCode(0x00eb); break; 
                            case 'igrave': ch = String.fromCharCode(0x00ec); break; 
                            case 'iacute': ch = String.fromCharCode(0x00ed); break; 
                            case 'icirc': ch = String.fromCharCode(0x00ee); break; 
                            case 'iuml': ch = String.fromCharCode(0x00ef); break; 
                            case 'eth': ch = String.fromCharCode(0x00f0); break; 
                            case 'ntilde': ch = String.fromCharCode(0x00f1); break; 
                            case 'ograve': ch = String.fromCharCode(0x00f2); break; 
                            case 'oacute': ch = String.fromCharCode(0x00f3); break; 
                            case 'ocirc': ch = String.fromCharCode(0x00f4); break; 
                            case 'otilde': ch = String.fromCharCode(0x00f5); break; 
                            case 'ouml': ch = String.fromCharCode(0x00f6); break; 
                            case 'divide': ch = String.fromCharCode(0x00f7); break; 
                            case 'oslash': ch = String.fromCharCode(0x00f8); break; 
                            case 'ugrave': ch = String.fromCharCode(0x00f9); break; 
                            case 'uacute': ch = String.fromCharCode(0x00fa); break; 
                            case 'ucirc': ch = String.fromCharCode(0x00fb); break; 
                            case 'uuml': ch = String.fromCharCode(0x00fc); break; 
                            case 'yacute': ch = String.fromCharCode(0x00fd); break; 
                            case 'thorn': ch = String.fromCharCode(0x00fe); break; 
                            case 'yuml': ch = String.fromCharCode(0x00ff); break; 
                            case 'OElig': ch = String.fromCharCode(0x0152); break; 
                            case 'oelig': ch = String.fromCharCode(0x0153); break; 
                            case 'Scaron': ch = String.fromCharCode(0x0160); break; 
                            case 'scaron': ch = String.fromCharCode(0x0161); break; 
                            case 'Yuml': ch = String.fromCharCode(0x0178); break; 
                            case 'fnof': ch = String.fromCharCode(0x0192); break; 
                            case 'circ': ch = String.fromCharCode(0x02c6); break; 
                            case 'tilde': ch = String.fromCharCode(0x02dc); break; 
                            case 'Alpha': ch = String.fromCharCode(0x0391); break; 
                            case 'Beta': ch = String.fromCharCode(0x0392); break; 
                            case 'Gamma': ch = String.fromCharCode(0x0393); break; 
                            case 'Delta': ch = String.fromCharCode(0x0394); break; 
                            case 'Epsilon': ch = String.fromCharCode(0x0395); break; 
                            case 'Zeta': ch = String.fromCharCode(0x0396); break; 
                            case 'Eta': ch = String.fromCharCode(0x0397); break; 
                            case 'Theta': ch = String.fromCharCode(0x0398); break; 
                            case 'Iota': ch = String.fromCharCode(0x0399); break; 
                            case 'Kappa': ch = String.fromCharCode(0x039a); break; 
                            case 'Lambda': ch = String.fromCharCode(0x039b); break; 
                            case 'Mu': ch = String.fromCharCode(0x039c); break; 
                            case 'Nu': ch = String.fromCharCode(0x039d); break; 
                            case 'Xi': ch = String.fromCharCode(0x039e); break; 
                            case 'Omicron': ch = String.fromCharCode(0x039f); break; 
                            case 'Pi': ch = String.fromCharCode(0x03a0); break; 
                            case ' Rho ': ch = String.fromCharCode(0x03a1); break; 
                            case 'Sigma': ch = String.fromCharCode(0x03a3); break; 
                            case 'Tau': ch = String.fromCharCode(0x03a4); break; 
                            case 'Upsilon': ch = String.fromCharCode(0x03a5); break; 
                            case 'Phi': ch = String.fromCharCode(0x03a6); break; 
                            case 'Chi': ch = String.fromCharCode(0x03a7); break; 
                            case 'Psi': ch = String.fromCharCode(0x03a8); break; 
                            case 'Omega': ch = String.fromCharCode(0x03a9); break; 
                            case 'alpha': ch = String.fromCharCode(0x03b1); break; 
                            case 'beta': ch = String.fromCharCode(0x03b2); break; 
                            case 'gamma': ch = String.fromCharCode(0x03b3); break; 
                            case 'delta': ch = String.fromCharCode(0x03b4); break; 
                            case 'epsilon': ch = String.fromCharCode(0x03b5); break; 
                            case 'zeta': ch = String.fromCharCode(0x03b6); break; 
                            case 'eta': ch = String.fromCharCode(0x03b7); break; 
                            case 'theta': ch = String.fromCharCode(0x03b8); break; 
                            case 'iota': ch = String.fromCharCode(0x03b9); break; 
                            case 'kappa': ch = String.fromCharCode(0x03ba); break; 
                            case 'lambda': ch = String.fromCharCode(0x03bb); break; 
                            case 'mu': ch = String.fromCharCode(0x03bc); break; 
                            case 'nu': ch = String.fromCharCode(0x03bd); break; 
                            case 'xi': ch = String.fromCharCode(0x03be); break; 
                            case 'omicron': ch = String.fromCharCode(0x03bf); break; 
                            case 'pi': ch = String.fromCharCode(0x03c0); break; 
                            case 'rho': ch = String.fromCharCode(0x03c1); break; 
                            case 'sigmaf': ch = String.fromCharCode(0x03c2); break; 
                            case 'sigma': ch = String.fromCharCode(0x03c3); break; 
                            case 'tau': ch = String.fromCharCode(0x03c4); break; 
                            case 'upsilon': ch = String.fromCharCode(0x03c5); break; 
                            case 'phi': ch = String.fromCharCode(0x03c6); break; 
                            case 'chi': ch = String.fromCharCode(0x03c7); break; 
                            case 'psi': ch = String.fromCharCode(0x03c8); break; 
                            case 'omega': ch = String.fromCharCode(0x03c9); break; 
                            case 'thetasym': ch = String.fromCharCode(0x03d1); break; 
                            case 'upsih': ch = String.fromCharCode(0x03d2); break; 
                            case 'piv': ch = String.fromCharCode(0x03d6); break; 
                            case 'ensp': ch = String.fromCharCode(0x2002); break; 
                            case 'emsp': ch = String.fromCharCode(0x2003); break; 
                            case 'thinsp': ch = String.fromCharCode(0x2009); break; 
                            case 'zwnj': ch = String.fromCharCode(0x200c); break; 
                            case 'zwj': ch = String.fromCharCode(0x200d); break; 
                            case 'lrm': ch = String.fromCharCode(0x200e); break; 
                            case 'rlm': ch = String.fromCharCode(0x200f); break; 
                            case 'ndash': ch = String.fromCharCode(0x2013); break; 
                            case 'mdash': ch = String.fromCharCode(0x2014); break; 
                            case 'lsquo': ch = String.fromCharCode(0x2018); break; 
                            case 'rsquo': ch = String.fromCharCode(0x2019); break; 
                            case 'sbquo': ch = String.fromCharCode(0x201a); break; 
                            case 'ldquo': ch = String.fromCharCode(0x201c); break; 
                            case 'rdquo': ch = String.fromCharCode(0x201d); break; 
                            case 'bdquo': ch = String.fromCharCode(0x201e); break; 
                            case 'dagger': ch = String.fromCharCode(0x2020); break; 
                            case 'Dagger': ch = String.fromCharCode(0x2021); break; 
                            case 'bull': ch = String.fromCharCode(0x2022); break; 
                            case 'hellip': ch = String.fromCharCode(0x2026); break; 
                            case 'permil': ch = String.fromCharCode(0x2030); break; 
                            case 'prime': ch = String.fromCharCode(0x2032); break; 
                            case 'Prime': ch = String.fromCharCode(0x2033); break; 
                            case 'lsaquo': ch = String.fromCharCode(0x2039); break; 
                            case 'rsaquo': ch = String.fromCharCode(0x203a); break; 
                            case 'oline': ch = String.fromCharCode(0x203e); break; 
                            case 'frasl': ch = String.fromCharCode(0x2044); break; 
                            case 'euro': ch = String.fromCharCode(0x20ac); break; 
                            case 'image': ch = String.fromCharCode(0x2111); break; 
                            case 'weierp': ch = String.fromCharCode(0x2118); break; 
                            case 'real': ch = String.fromCharCode(0x211c); break; 
                            case 'trade': ch = String.fromCharCode(0x2122); break; 
                            case 'alefsym': ch = String.fromCharCode(0x2135); break; 
                            case 'larr': ch = String.fromCharCode(0x2190); break; 
                            case 'uarr': ch = String.fromCharCode(0x2191); break; 
                            case 'rarr': ch = String.fromCharCode(0x2192); break; 
                            case 'darr': ch = String.fromCharCode(0x2193); break; 
                            case 'harr': ch = String.fromCharCode(0x2194); break; 
                            case 'crarr': ch = String.fromCharCode(0x21b5); break; 
                            case 'lArr': ch = String.fromCharCode(0x21d0); break; 
                            case 'uArr': ch = String.fromCharCode(0x21d1); break; 
                            case 'rArr': ch = String.fromCharCode(0x21d2); break; 
                            case 'dArr': ch = String.fromCharCode(0x21d3); break; 
                            case 'hArr': ch = String.fromCharCode(0x21d4); break; 
                            case 'forall': ch = String.fromCharCode(0x2200); break; 
                            case 'part': ch = String.fromCharCode(0x2202); break; 
                            case 'exist': ch = String.fromCharCode(0x2203); break; 
                            case 'empty': ch = String.fromCharCode(0x2205); break; 
                            case 'nabla': ch = String.fromCharCode(0x2207); break; 
                            case 'isin': ch = String.fromCharCode(0x2208); break; 
                            case 'notin': ch = String.fromCharCode(0x2209); break; 
                            case 'ni': ch = String.fromCharCode(0x220b); break; 
                            case 'prod': ch = String.fromCharCode(0x220f); break; 
                            case 'sum': ch = String.fromCharCode(0x2211); break; 
                            case 'minus': ch = String.fromCharCode(0x2212); break; 
                            case 'lowast': ch = String.fromCharCode(0x2217); break; 
                            case 'radic': ch = String.fromCharCode(0x221a); break; 
                            case 'prop': ch = String.fromCharCode(0x221d); break; 
                            case 'infin': ch = String.fromCharCode(0x221e); break; 
                            case 'ang': ch = String.fromCharCode(0x2220); break; 
                            case 'and': ch = String.fromCharCode(0x2227); break; 
                            case 'or': ch = String.fromCharCode(0x2228); break; 
                            case 'cap': ch = String.fromCharCode(0x2229); break; 
                            case 'cup': ch = String.fromCharCode(0x222a); break; 
                            case 'int': ch = String.fromCharCode(0x222b); break; 
                            case 'there4': ch = String.fromCharCode(0x2234); break; 
                            case 'sim': ch = String.fromCharCode(0x223c); break; 
                            case 'cong': ch = String.fromCharCode(0x2245); break; 
                            case 'asymp': ch = String.fromCharCode(0x2248); break; 
                            case 'ne': ch = String.fromCharCode(0x2260); break; 
                            case 'equiv': ch = String.fromCharCode(0x2261); break; 
                            case 'le': ch = String.fromCharCode(0x2264); break; 
                            case 'ge': ch = String.fromCharCode(0x2265); break; 
                            case 'sub': ch = String.fromCharCode(0x2282); break; 
                            case 'sup': ch = String.fromCharCode(0x2283); break; 
                            case 'nsub': ch = String.fromCharCode(0x2284); break; 
                            case 'sube': ch = String.fromCharCode(0x2286); break; 
                            case 'supe': ch = String.fromCharCode(0x2287); break; 
                            case 'oplus': ch = String.fromCharCode(0x2295); break; 
                            case 'otimes': ch = String.fromCharCode(0x2297); break; 
                            case 'perp': ch = String.fromCharCode(0x22a5); break; 
                            case 'sdot': ch = String.fromCharCode(0x22c5); break; 
                            case 'lceil': ch = String.fromCharCode(0x2308); break; 
                            case 'rceil': ch = String.fromCharCode(0x2309); break; 
                            case 'lfloor': ch = String.fromCharCode(0x230a); break; 
                            case 'rfloor': ch = String.fromCharCode(0x230b); break; 
                            case 'lang': ch = String.fromCharCode(0x2329); break; 
                            case 'rang': ch = String.fromCharCode(0x232a); break; 
                            case 'loz': ch = String.fromCharCode(0x25ca); break; 
                            case 'spades': ch = String.fromCharCode(0x2660); break; 
                            case 'clubs': ch = String.fromCharCode(0x2663); break; 
                            case 'hearts': ch = String.fromCharCode(0x2665); break; 
                            case 'diams': ch = String.fromCharCode(0x2666); break; 
                            default: ch = ''; break; 
                        } 
                     } 
                     i = semicolonIndex; 
               } 
         }             
         out += ch; 
   }   
   return out;       
}
function previewrow_oncollapse(cell, gridexID, tableID)
{
	if(cell.nodeType == 1 && cell.tagName == "TD")
	{
		var g = getGridEXFromID(gridexID); 
		if(g.getIsInitialized != null && g.getIsInitialized())
		{
			var gt = g.getTables().getTableByID(tableID); 			
			var r = cell.parentElement; 						
			var gr = g.RetrieveRow(getRootRowFromInner(r), r, gt); 
			gr.CollapsePreviewRow(cell); 
		}
	}
}
function browseURLByRow(r)
{
	var u = r.getURL();
	var t = r.getURLTarget();
	if(t == null || t.length == 0)
		t = "_self";
	
	window.open(u, t); 
}
function setCurrentColumn(gridex, cell)
{
	if(cell == null)
		document.getElementsByName(gridex.getID()+"_currentcol")[0].value = ""; 
	else
		document.getElementsByName(gridex.getID()+"_currentcol")[0].value = cell.getColumn().getTable().getID() + "," + cell.getColumn().getColumnIndex(); 
}
function onSelectRow(innerRow, gridex, gridexTable)
{		
	var r = gridex.RetrieveRow(getRootRowFromInner(innerRow), innerRow, gridexTable);		
	var c = gridex.setCurrentRow(r); 
	if(c == null || c)
	{
		gridex.getSelectedItems().SelectRow(r); 	
		setRowForEditOrFilter(r);
	}
}
function setRowForEditOrFilter(r)
{
	if(r.containsURL())
	{			
		browseURLByRow(r); 
		return; 
	}
	if(r.getType() == 9 || (r.getTable().getAllowEdit() && (r.getType() == 3 || r.getType() == 4)))
		r.BeforeEdit(); 
	else if(r.getType() == 11 && r.getGridEX().getFilterMode() == 1)
		r.BeforeFilter(); 
	else if(!r.getTable().getAllowEdit() && (r.getType() == 3 || r.getType() == 4))
	{		
		var c = r.getCellSelected();		
		setCurrentColumn(r.getGridEX(), c); 
		if(c != null)
		{								
			if(c.getColumn().getSelectable() && c.getColumn().getActAsSelector())
				r.CheckRow(c.getValue(),c.getColumn().getClientID(),true,true);
			else if(c.getColumn().getColumnType() == 4)
			{				
				window.event.cancelBubble = true;
				window.event.returnValue = false;
				return false;
			}				
		}
	}
}
function getTableOffsetParent(e)
{
	var t = e.parentElement;
	var d = false;
	while(!d)
	{
		if(t == null)
			d = true;
		else
		{
			if(t.tagName == "TABLE" && t.getAttribute("id") != null)
				d = true;
			else
				t = t.parentElement;
		}
	}
	return t; 
}
function getInnerItemRowCore(row, gridex)
{
	var _innerItemRow = null; 
	var _tableID = null; 
	if(gridex.isHierarchicalGrid())
	{			
		var _tmpTable = row.cells[0].childNodes[0]; 
		var _tmpRow = _tmpTable.rows[0]; 
		var l = _tmpRow.cells.length; 
		for(var i = 0; i < l && _innerItemRow == null; i++)
		{
			if(_tmpRow.cells[i].childNodes[0].tagName == "TABLE")
				_innerItemRow = _tmpRow.cells[i].childNodes[0].rows[0]; 
		}			
		if(_innerItemRow == null)			
			throw Error("unable to retrieve inner row"); 
	}		
	if(row.getAttribute("t") != null)
		_tableID = row.getAttribute("t"); 
		
	if(_tableID != null && gridex.getTables().getTableByID(_tableID).getUseColumnSets())
	{
		if(_innerItemRow == null)
			_innerItemRow = row;
			
		if(getType() == 3 || getType() == 9)				
			return _innerItemRow.cells[0].childNodes[0].rows[0]; 
		else
			return _innerItemRow; 
	}
	else if(gridex.isHierarchicalGrid() && _innerItemRow != null)
		return _innerItemRow;
	else
		return row;
}
function selectionChanged(row, rowID, tableID, gridexID)
{
	var r = null; 
	var g = getGridEXFromID(gridexID);
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		r = g.RetrieveRow(getRootRowFromInner(row), row, g.getTables().getTableByID(tableID))
		if(g.getSelectedItems() != null && g.getSelectedItems().Count() == 1)
		{
			if(g.getSelectedItems().getSelectedItemInIndex(0).getRow() == r)
			{
				setRowForEditOrFilter(r);
				return false; 
			}
		}				
	}
	if(r != null)
	{
		g.selpb = false;
		var c = g.setCurrentRow(r, true); 
		g.selpb = true; 
		if(c == null || c)
		{				
			g.setHitTestArea(0);
			setRowForEditOrFilter(r);
			g.FireEvent("Click", [g, window.event.button, window.event.clientX, window.event.clientY]); 
			g.FireEvent("SelectionChanged", [r]); 
			g.DoPostBack(null, "SelectionChanged:"+r.getID());
		}
	}
}
function gxrc(row, tableID, gridexID)
{	
	var g = getGridEXFromID(gridexID);
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		var gt = g.getTables().getTableByID(tableID);			
		onSelectRow(row, g, gt); 
		g.setHitTestArea(0); 
		g.FireEvent("Click", [g,window.event.button, window.event.clientX, window.event.clientY]); 
		try
		{
			if(window.event.type == "contextmenu")
			{
				window.event.cancelBubble = true;
				window.event.returnValue = false;
			}
		}
		catch(err) { } 
	}	
}
function gxrdbc(row, tableID, gridexID)
{
	var g = getGridEXFromID(gridexID);
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		var gt = g.getTables().getTableByID(tableID);			
		onSelectRow(row, g, gt); 		
		g.setHitTestArea(0); 
		g.FireEvent("DoubleClick", [g, window.event.clientX, window.event.clientY]); 
		if(g.dcpb)
			g.DoPostBack(null, "DoubleClick"); 
	}
}
function clickRowPreviewCore(g, pr, tableID)
{
	var gt = g.getTables().getTableByID(tableID);			
	var r = null; 
	if(g.isHierarchicalGrid())
	{
		r = pr.offsetParent.rows[pr.rowIndex - 1]; 
		if(gt.getUseColumnSets())
			r = r.cells[0].childNodes[0].rows[0];
	}
	else
	{
		r = gt.getHtmlItemsTable().rows[pr.rowIndex - 1]; 		
		if(gt.getUseColumnSets())
			r = r.cells[0].childNodes[0].rows[0];
	}		
	onSelectRow(r, g, gt); 
	if(getTypeOfTD(window.event.srcElement) == "rh")
		g.setHitTestArea(4);
	else
		g.setHitTestArea(14); 
}
function rowpreview_onclick(previewRow, tableID, gridexID)
{	
	var g = getGridEXFromID(gridexID); 
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		clickRowPreviewCore(g, previewRow, tableID); 		
		g.FireEvent("Click", [g,window.event.button, window.event.clientX, window.event.clientY]); 		
		if(window.event.type == "contextmenu")
		{
			window.event.cancelBubble = true;
			window.event.returnValue = false; 			
		}
	}
}
function rowpreview_ondblclick(previewRow, tableID, gridexID)
{
	var g = getGridEXFromID(gridexID); 
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		clickRowPreviewCore(g, previewRow, tableID);
		g.FireEvent("DoubleClick", [g,window.event.clientX, window.event.clientY]);
	}
}
function gxrcl(rowID, tableID, gridexID, action)
{
	var rr = document.getElementById(rowID); 
	if(rr == null)
		throw Error("unable to find HTML TR for row id '" + rowID + "'"); 
	var g = getGridEXFromID(gridexID); 
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		var gt = g.getTables().getTableByID(tableID);
		var r = g.RetrieveRow(rr, null, gt, null);
		if(action == 1)
			r.Expanding();
		else if(action == 0)
			r.Collapsing(); 	
	}
}
function gxlrc(rowID, tableID, gridexID)
{
	var rr = document.getElementById(rowID); 
	if(rr == null)
		throw Error("unable to find HTML TR for row id '" + rowID + "'"); 		
	var g = getGridEXFromID(gridexID); 
	if(g.getIsInitialized != null && g.getIsInitialized())
	{
		var gt = g.getTables().getTableByID(tableID);
		var r = g.RetrieveRow(rr, null, gt, null); 					
		if(r.getExpanded())
			r.Collapsing();
		else
			r.Expanding();
		if(r == g.getGridEXRow() || !g.getSelectOnExpand())
			g.ReportRowsStatus();
		if(window.event.srcElement != null && gt.getAutoSizeExpandColumn() != null)
		{
			var _td = getColumnFromElement(window.event.srcElement);
			if(_td != null && _td.getAttribute("type") == "ec" && _td.getAttribute("id") == gt.getAutoSizeExpandColumn() + "_L")
				gt.AutoSizeExpandColumn(_td); 
		}		
		var c = g.setCurrentRow(r, g.getSelectOnExpand());
		if(c == null || c)
		{
			if(!g.getSelectOnExpand())
			{
				window.event.cancelBubble = true;
				window.event.returnValue = false;
				return false;
			}
		}
		return true; 
	}
}
function selector_checkItems(checkbox, columnID, tableID, gridexID, fireEvent)
{	
	var g = getGridEXFromID(gridexID); 
	if(g.getIsInitialized != null && g.getIsInitialized())
	{				
		var t = g.getTables().getTableByID(tableID);
		var hs = t.getHeaders(); 
		if(hs.length > 1)
		{
			for(var i = 0; i < hs.length; i++)						
				hs[i].CheckSelectors(columnID, checkbox.checked, true);
		}
		else if(hs.length == 1)
			hs[0].CheckSelectors(columnID, checkbox.checked, true); 
		else
			hs.CheckSelectors(columnID, checkbox.checked, true);
		var l = -1;
		var asc = new Array(); 
		l = t.getColumns().Count();
		for(var i=0;i<l;i++)
		{		
			var c = t.getColumns().getGridEXColumn(i); 
			if(c.getActAsSelector())
				asc[asc.length] = c.getClientID(); 
		}		
		var r = null; 
		var rt = -1; 
		var ir = null;
		var it = g.getRootTable().getHtmlItemsTable(); 
		l = it.rows.length; 
		for(var i=0;i<l;i++)
		{
			ir = it.rows[i];			
			if(ir.getAttribute("t") == tableID) 
			{			
				rt = ir.getAttribute("type"); 
				if(rt != "1" && rt != "2" && rt != "6" && rt != "7" && rt != "10")
				{				
					if(rt == "3" || rt == "4" || rt == null)
					{
						for(var j=0;j<asc.length;j++)
						{							
							var cb = null;
							if(browser.isIE)
								cb = ir.all(asc[j]+"_L").childNodes[0].getElementsByTagName("INPUT");
							else							
								cb = ir.getChildsById(asc[j]+"_L")[0].getElementsByTagName("SPAN")[0].getElementsByTagName("INPUT");								
							cb[0].checked = checkbox.checked; 
						}
						if(checkbox.checked)
							ir.setAttribute("checked", "true");
						else
							ir.setAttribute("checked", "false");
					}					
				}
			}
		}		
		if(checkbox.checked)		
			t.checkedCount = t.getRecordsCount(); 
		else
			t.checkedCount = 0;		
		g.ReportRowsStatus(); 
		g.FireEvent("RowCheckedChanged", [checkbox.checked, t, null]);
		if(fireEvent != null && fireEvent == 1)
		{
			var argument = "RowCheckedChanged";
			if(checkbox.checked)
				argument += ":3";
			else
				argument += ":4";
			g.DoPostBack(null, argument); 
		}
	}
}
function drawDownArrow(x, y)
{	
	var i = 0;
	while(i < 5)
	{
		var l = document.getElementById("da" + (i+1));
		if(l !=null)
		{
			l.style.pixelLeft = x -  i;
			l.style.pixelTop = y + i; 
			l.style.display = "";
			l.style.visibility = "visible"; 			
			i++;
		}
	}			
	var a = document.getElementById("da" + (i+1));
	if(a != null)
	{	
		a.style.pixelLeft = x - 1;
		a.style.pixelTop = y + i; 
		a.style.display = "";
		a.style.visibility = "visible"; 
	}
}	
function drawUpArrow(x, y)
{		
	var i = 0;	
	var s = null; 
	while(i < 5)
	{	
		var l = document.getElementById("ua" + (i+1));
		if(l != null)
		{
			s = l.style; 		
			s.pixelLeft = x - i;
			s.pixelTop = y - i;
			s.display = "";
			s.visibility = "visible";
			i++; 			
		}
	}	
	var a = document.getElementById("ua" + (i+1));
	if(a != null)
	{
		s = a.style;
		s.pixelLeft = x - 1;
		s.pixelTop = y - 4 - s.pixelHeight;
		s.display = "";
		s.visibility = "visible"; 
	}
}
function hideColumnForDrop()
{	
	for(var i = 1; i <= 6; i++)
	{
		var a = document.getElementById("da" + i);
		a.style.display = "none"; 
		a.style.visibility = "hidden";
		var b = document.getElementById("ua" + i);
		b.style.display = "none";
		b.style.visibility = "hidden"; 
	}
}
function drag_onselectstart()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false;
	return false; 
}
function drag_onmousemove()
{
	if(columnDraging && currcolumn != null)	
	{
		var _result = null; 	
		showColumnDrag();		
		if(columndragMode == 1)
		{	
			droptarget = -1; 
			_result = currheader.getGridEXTable().HitTestColumnHeaders(window.event.clientX, window.event.clientY);			
			if(_result != null && _result[0] != null && _result[0].id != currcolumn.id)
			{
				droptarget = 1;
				_result[1].ShowColumnForDrop(window.event.clientX, _result[0]); 
				currheader = _result[1]; 
			}
			else if(currheader.getGridEX().getGroupByBox() != null)
			{	
				if(currheader.getGridEXTable().getColumns().getGridEXColumnByClientID(currcolumn.id).getAllowGroup())
				{
					_result = currheader.getGridEX().getGroupByBox().HitTestColumns(window.event.clientX, window.event.clientY, 1);
					if(_result != null && _result[0] != null)
					{
						droptarget = 2;
						currgroupbybox = currheader.getGridEX().getGroupByBox();
						currgroupbybox.ShowColumnForDrop(window.event.clientX, _result[0], _result[1], _result[2]); 
					}
				}
			}			
			if(_result == null)								
				hideColumnForDrop();				
		}
		else if(columndragMode == 2)
		{
			droptarget = -1;
			if(currheader.getGridEX().getGroupByBox() != null)
			{
				_result = currheader.getGridEX().getGroupByBox().HitTestColumns(window.event.clientX, window.event.clientY, 1); 
				if(_result != null && _result[0] != null)
				{
					droptarget = 2; 
					currgroupbybox = currheader.getGridEX().getGroupByBox();
					currgroupbybox.ShowColumnForDrop(window.event.clientX, _result[0], _result[1], _result[2]); 					
				}								
				if(_result == null)
					hideColumnForDrop(); 
			}			
		}
		else if(columndragMode == 3) 
		{			
			droptarget = -1;			
			_result = currgroupbybox.HitTestColumns(window.event.clientX, window.event.clientY, 2); 
			if(_result != null && _result[0] != null && _result[0].id != currcolumn.id)
			{				
				droptarget = 2;
				currgroupbybox.ShowColumnForDrop(window.event.clientX, _result[0], _result[1], 0); 
			}
			else
			{	
				var _tableindex = currgroupbybox.getGridEX().getTables().getIndexOf(currgrouptable); 
				if(_tableindex != -1)
				{
					var _table = currgroupbybox.getGridEX().getTables().getTableInIndex(_tableindex); 
					_result = _table.HitTestColumnHeaders(window.event.clientX, window.event.clientY); 
					if(_result != null && _result[0] != null)
					{
						droptarget = 1;
						currheader = _result[1];
						currheader.ShowColumnForDrop(window.event.clientX, _result[0]); 
					}
				}
			}					
			if(_result == null)
				hideColumnForDrop(); 
		}
	}
}
function drag_onmouseup()
{
	if(window.event.button == 1 || !browser.isIE)
	{ 
		if(columnDraging && currcolumn != null) 
		{			
			if(droptarget == 1)
			{					
				currheader.DropColumn(currcolumn); 
				endColumnDrag();
				if(columndragMode != 3)
					ShowColumnUnPressed();	
				currpressedcolumn = null;
				if(!browser.isIE)
				{
					window.event.returnValue = false;
					window.event.cancelBubble = true; 
					return false; 
				} 
			}
			else if(droptarget == 2)
				currgroupbybox.DropColumn();
			else
			{
				if(columndragMode == 3 && currgroupbybox != null)
					currgroupbybox.DropColumn(); 
				else
				{				
					cancelColumnDraging();
					ShowColumnUnPressed();
					currpressedcolumn = null;
				}
			}
		}		
	}
}
function startGroupDrag(groupcolumn, gridEXGroupByBox, grouptable)
{
	canceledByUser = false; 
	columnDraging = true;
	currcolumn = groupcolumn; 
	currgroupbybox = gridEXGroupByBox;
	currgrouptable = grouptable; 
	columndragMode = 3; 
	showColumnDrag(); 
}
function startColumnDrag(column, gridEXHeader)
{
	columnDraging = true; 
	currcolumn = column;
	currheader = gridEXHeader;
	columndragMode = 1; 
	showColumnDrag();
}
function startColumnSetDrag(column, gridEXHeader, gridEXColumnSet)
{
	canceledByUser = false; 
	columnDraging = true;
	currcolumn = column;
	currheader = gridEXHeader;	
	currcolumnset = gridEXColumnSet; 
	columndragMode = 2;
	showColumnDrag();
}
function endColumnDrag()
{
	columnDraging = false;
	currcolumn = null;
	currgroupbybox = null;	
	groupnewpos = null; 
	currgrouppos = null;
	currgrouptable = null;	
	currheader = null; 
	columnfordrop = null;	
	droptarget = -1; 
	hideColumnDrag(); 
}
function hideColumnDrag()
{
	dragcolumn.style.display = "none";
	dragcolumn.style.visibility = "hidden"; 
	hideColumnForDrop(); 
}
function showColumnHeaderDrag()
{			
	if(dragcolumn.style.visibility == "hidden")
	{		
		if(currcolumn == null)
			return; 			
			
		dragcolumn.style.backgroundColor = currcolumn.currentStyle.backgroundColor;
		dragcolumn.style.color = currcolumn.currentStyle.color;
		dragcolumn.style.backgroundImage = currcolumn.currentStyle.backgroundImage;
		var _colpressed = document.getElementById("colpressed"); 
		if(_colpressed != null)
		{						
			dragcolumn.style.borderBottom = _colpressed.style.borderBottom;
			dragcolumn.style.borderRight = _colpressed.style.borderRight;		
			dragcolumn.style.borderTop = _colpressed.style.borderTop;
			dragcolumn.style.borderLeft = _colpressed.style.borderLeft; 
		}
		else
		{	
			dragcolumn.style.borderBottom = currcolumn.currentStyle.borderBottom;
			dragcolumn.style.borderRight = currcolumn.currentStyle.borderRight;
			dragcolumn.style.borderTop = currcolumn.currentStyle.borderTop; 
			dragcolumn.style.borderLeft = currcolumn.currentStyle.borderLeft;
		}		
		dragcolumn.style.padding = currcolumn.currentStyle.padding;
		if(currcolumn.font != null && currcolumn.font != "") dragcolumn.style.font = currcolumn.font;
		dragcolumn.style.fontFamily = currcolumn.currentStyle.fontFamily;
		dragcolumn.style.fontSize = currcolumn.currentStyle.fontSize; 
		dragcolumn.style.fontStyle = currcolumn.currentStyle.fontStyle; 
		dragcolumn.style.fontVariant = currcolumn.currentStyle.fontVariant; 
		dragcolumn.style.fontWeight = currcolumn.currentStyle.fontWeight; 
		dragcolumn.style.textAlign = currcolumn.currentStyle.textAlign; 
	}	
	if(dragcolumn.style.visibility == "hidden")
	{				
		dragcolumn.style.pixelWidth =  currcolumn.offsetWidth; 
		dragcolumn.style.pixelHeight = currcolumn.offsetHeight; 		
		/* BY X
		dragcolumn.innerHTML =  currcolumn.childNodes[0].innerHTML; 
		if(browser.isIE)
			dragcolumn.childNodes[0].style.padding = browser.getCurrentStyle(currcolumn.childNodes[0], "padding"); 
		else
			dragcolumn.childNodes[1].style.padding = browser.getCurrentStyle(currcolumn.childNodes[1], "padding"); 
		*/
		dragcolumn.innerHTML = currcolumn.innerHTML; 
		dragcolumn.style.padding = currcolumn.currentStyle.padding;
		try
		{
			dragcolumn.style.whiteSpace = currcolumn.currentStyle.whiteSpace;
		} catch(err) { }
		var _spanpressed = _colpressed.childNodes[0];
		var _child = dragcolumn.childNodes[0];
		/* BY X		
		if(browser.isIE)
			_child = dragcolumn.childNodes[0];
		else
			_child = dragcolumn.childNodes[1]; 
		*/
		_child.style.borderLeft = _spanpressed.style.borderLeft;
		_child.style.borderTop = _spanpressed.style.borderTop;
		_child.style.borderRight = _spanpressed.style.borderRight;
		_child.style.borderBottom = _spanpressed.style.borderBottom;
	}	
	if(dragcolumn.style.visibility == "hidden")
	{	
		if(columndragMode == 1) 
		{
			var _htmlheader = currheader.getHtmlHeader(); 			
			var divtable = currheader.getGridEXTable().getHtmlDiv(); 
			var g = currheader.getGridEXTable().getGridEX();			
			var _isrtl = (g.getHtmlGridEX().getAttribute("rtl") == "1");
			var _left = getPixelLeft(currcolumn);	
			if(!currheader.getIsRoot() && !_isrtl)
				_left -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
			else if(!currheader.getIsRoot())
			{
				_left += (currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollLeft;
				_left += (currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.clientWidth) - (currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollWidth - currheader.getGridEXTable().getHtmlItemsTable().offsetParent.offsetWidth);
			}
			dragcolumn.style.pixelLeft = _left;			
			var _top = getPixelTop(currcolumn);
			if(!currheader.getIsRoot())
				_top -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollTop;
				
			var _offsety = (_top + currcolumn.offsetHeight) - dragpoint.Y();
			dragcolumn.style.pixelTop = (window.event.clientY + _offsety) - currcolumn.offsetHeight;  
			dragcolumn.varX = window.event.clientX - dragcolumn.style.pixelLeft; 
			dragcolumn.varY = window.event.clientY - dragcolumn.style.pixelTop;
		}
		else if(columndragMode == 2)	
		{
			var htmlTable = currcolumnset.getHtmlColumnSet(); 
			var divtable = currcolumnset.getGridEXTable().getHtmlDiv(); 
			var divgridex = currcolumnset.getGridEXTable().getGridEX().getHtmlGridEX();			
			var _left = getPixelLeft(currcolumn);
			var _isrtl = (divgridex.getAttribute("rtl") == "1");			
			dragcolumn.style.pixelLeft = _left; 			
			var _top = getPixelTop(currcolumn); 			
			var _offsety = (_top + currcolumn.offsetHeight) - dragpoint.Y(); 			
			dragcolumn.style.pixelTop = (window.event.clientY + _offsety) - currcolumn.offsetHeight; 						
			dragcolumn.varX = window.event.clientX - dragcolumn.style.pixelLeft;
			dragcolumn.varY = window.event.clientY - dragcolumn.style.pixelTop; 
		}
	}
	else
	{
		dragcolumn.style.pixelLeft = window.event.clientX - dragcolumn.varX;		
		dragcolumn.style.pixelTop = window.event.clientY - dragcolumn.varY; 
	}	
	dragcolumn.style.display = "";
	dragcolumn.style.visibility = "visible";
}
function showColumnGroupDrag()
{	
	if(dragcolumn != null)
	{
		var _colstyle = currcolumn.currentStyle;		
		dragcolumn.style.backgroundColor = _colstyle.backgroundColor;
		dragcolumn.style.backgroundImage = _colstyle.backgroundImage; 
		dragcolumn.style.color = _colstyle.color; 
		dragcolumn.style.borderBottom = (_colstyle.borderBottomWidth + " " + _colstyle.borderBottomStyle + " " + _colstyle.borderBottomColor); 
		dragcolumn.style.borderRight = (_colstyle.borderRightWidth + " " + _colstyle.borderRightStyle + " " + _colstyle.borderRightColor); 
		dragcolumn.style.borderTop = (_colstyle.borderTopWidth + " " + _colstyle.borderTopStyle + " " + _colstyle.borderTopColor); 
		dragcolumn.style.borderLeft = (_colstyle.borderLeftWidth + " " + _colstyle.borderLeftStyle + " " + _colstyle.borderLeftColor); 
		dragcolumn.style.padding = _colstyle.padding;		
		if(currcolumn.style.font != null && currcolumn.style.font != "") dragcolumn.style.font = currcolumn.style.font;
		dragcolumn.style.fontFamily = _colstyle.fontFamily;
		dragcolumn.style.fontSize = _colstyle.fontSize;
		dragcolumn.style.fontStyle = _colstyle.fontStyle;
		dragcolumn.style.fontVariant = _colstyle.fontVariant;
		dragcolumn.style.fontWeight = _colstyle.fontWeight;
		dragcolumn.style.textAlign = _colstyle.textAlign;		
	}	
	if(dragcolumn.style.visibility == "hidden")
	{		
		dragcolumn.style.pixelWidth =  currcolumn.offsetWidth;  
		dragcolumn.style.pixelHeight = currcolumn.offsetHeight; 
		dragcolumn.innerHTML =  currcolumn.childNodes[0].outerHTML; 		
		var _child = dragcolumn.childNodes[0];
		_child.style.borderLeft = currcolumn.childNodes[0].style.borderLeft;
		_child.style.borderTop = currcolumn.childNodes[0].style.borderTop;
		_child.style.borderRight = currcolumn.childNodes[0].style.borderRight;
		_child.style.borderBottom = currcolumn.childNodes[0].style.borderBottom;						
	}	
	if(dragcolumn.style.visibility == "hidden")
	{
		dragcolumn.style.pixelLeft = getPixelLeft(currcolumn); 
		dragcolumn.style.pixelTop = getPixelTop(currcolumn);
		dragcolumn.varX = window.event.clientX - dragcolumn.style.pixelLeft;
		dragcolumn.varY = window.event.clientY - dragcolumn.style.pixelTop; 
	}
	else
	{
		dragcolumn.style.pixelLeft = window.event.clientX - dragcolumn.varX;		
		dragcolumn.style.pixelTop = window.event.clientY - dragcolumn.varY; 
	}	
	dragcolumn.style.display = "";
	dragcolumn.style.visibility = "visible";
}
function showColumnDrag()
{	
	if(columndragMode == 3)
		showColumnGroupDrag(); 
	else
		showColumnHeaderDrag(); 	
}
function cancelColumnDraging()
{
	hideColumnDrag();	
	columnDraging = false; 
	currcolumn = null; 
	currheader = null; 
	currgrouptable = null;
	currgroupbybox = null; 	
}
function getMaximumColumnHeaderSize(gridEXColumn, headerColumn)
{	
	var gid = gridEXColumn.getTable().getGridEX().getID(); 
	var sc = document.getElementById(gid+"_spancell"); 
	if(sc == null)
	{		
		sc = document.createElement("SPAN"); 
		sc.id = gid + "_spancell"; 
		sc.style.visibility = "hidden"; 
		document.body.appendChild(sc); 
	}
	else
		sc.style.display = "";		
	var w = getRealCellWidth(sc, headerColumn, gid);
	if(sc != null)
	{
		sc.style.display = "none";
		sc.style.visibility = "hidden";
	}
	return w;
}
function getMaximumColumnSize(gridEXColumn)
{
	var _cell = null; 
	var _cells = null;
	if(browser.isIE)
		_cells = document.getElementsByName(gridEXColumn.getClientID() + "_L"); 
	else
		_cells = document.getChildsById(gridEXColumn.getClientID() + "_L"); 
	var _cellsLength = _cells.length; 
	var _cellsScanned = 0;
	var _cellsToCount = 10; 
	var _cellWidth = -1; 
	var _realWidth = -1; 
	var r = null; 	
	if(_cellsLength == 0)
		return -1;
		
	var gid = gridEXColumn.getTable().getGridEX().getID(); 
	var sc = document.getElementById(gid + "_spancell"); 	
	if(sc == null)
	{
		sc = document.createElement("SPAN");
		sc.id = gid + "_spancell"; 
		sc.style.visibility = "hidden"; 
		document.body.appendChild(sc); 				
	}
	else
		sc.style.display = "";
	for(var i = 0; i < _cellsLength && _cellsScanned < _cellsToCount; i++)
	{		
		_cell = _cells[i]; 
		r = getRootRowFromInner(_cell.parentElement); 
		if(r.style.display != "none")
		{						
			_realWidth = getRealCellWidth(sc, _cell, gid); 											
			if(_realWidth > _cellWidth)
				_cellWidth = _realWidth;				
			_cellsScanned++; 
		}				
	}	
	if(sc != null)
	{
		sc.style.display = "none";
		sc.style.visibility = "hidden";		
	}	
	return _cellWidth; 
}
function drawResizeLine(left, top, height)
{	
	resizeline.style.pixelLeft = left;
	resizeline.style.pixelTop = top; 	
	resizeline.style.pixelHeight = height; 
	resizeline.style.display = "block";
	resizeline.style.visibility = "visible";
}
function resizeline_onmouseup()
{
	if(window.event.button == 1)
	{
		if(columnResizing)
		{
			if(columnresizeMode == 1)
				endColumnResize();
			else if(columnresizeMode == 2)
				endColumnSetResize(); 
		}
	}
}
function cancelColumnSetResize()
{
	hideColumnResizeLine();
	columnResizing = false;
	currcolumn = null;
	currcolumnset = null; 	
	couldStartResize = false;
	resizepoint = null; 
}
function startColumnSetResize(column, gridEXColumnSet)
{
	canceledByUser = false; 
	currcolumnset = gridEXColumnSet;
	currcolumn = column; 
	columnResizing = true;
	columnresizeMode = 2;
	showColumnSetResizeLine(); 	
}
function endColumnSetResize()
{	
	if(currcolumn != null && currcolumnset != null)
	{
		currcolumnset.ResizeColumnSetHeader(currcolumn, window.event.clientX); 	
		hideColumnResizeLine(); 
		columnResizing = false;
		currcolumn = null;
		currcolumnset = null;
	}
}
function showColumnSetResizeLine()
{	
	if(resizeline != null)	
	{			
		var gt = currcolumnset.getGridEXTable(); 
		var g = gt.getGridEX();
		var _rtl = g.getHtmlGridEX().getAttribute("rtl") == "1";
		var leftmin = -1;
		if(_rtl)
			leftmin = (getPixelLeft(currcolumn) + currcolumn.offsetWidth) - 9;
		else
			leftmin = getPixelLeft(currcolumn) - getHorizontalScrollOffset(g) + 9; 
		if(currcolumn.type != null && currcolumn.type == "ch")
			leftmin += gt.getHeaderWidth(); 			
		if(currcolumn.getAttribute("pec") != null && currcolumn.type != "ch")
			leftmin += 18; 	
		if(_rtl)
		{
			if(window.event.clientX + getScrollLeft(g) >= leftmin)
				return; 
		}
		else
		{
			if(window.event.clientX + getScrollLeft(g) <= leftmin)
				return;
		}			
		var divgridex = g.getHtmlGridEX(); 
		var divtable = currcolumnset.getGridEXTable().getHtmlDiv();				
		var left = window.event.clientX - 2;
		left += getScrollLeft(g);
		var top = getPixelTop(currcolumn); 
		var height = gt.getHtmlItemsTable().offsetParent.offsetHeight;
		height -= (top - getPixelTop(gt.getHtmlItemsTable().offsetParent));		
		drawResizeLine(left, top, height); 
	}	
}
function startColumnResize(column, gridEXHeader)
{	
	canceledByUser = false; 
	currheader = gridEXHeader; 
	currcolumn = column; 
	columnResizing = true;
	columnresizeMode = 1;
	showColumnResizeLine();
}
function endColumnResize()
{	
	if(currcolumn != null && currheader != null)
	{			
		currheader.ResizeColumnWidth(currcolumn, event.clientX);
		hideColumnResizeLine();
		columnResizing = false;
		currcolumn = null;
		currheader = null; 		
	}
}
function cancelColumnResize()
{	
	hideColumnResizeLine(); 
	columnResizing = false;
	couldStartResize = false;
	resizepoint = null; 
}
function getMinLeft(td, table)
{
	var leftmin = 9;
	if(td.getAttribute("type") == "ch")
		leftmin += table.getHeaderWidth(); 
	if(td.getAttribute("pec") != null && td.getAttribute("type") != "ch")
		leftmin += 18; 
	return leftmin; 
}
function getMinimalWidth(td)
{
	var s = getSortWidth(td); 
	if(s == 0)
		return 8;
	else
		return s; 		
}
function fixRightToLeftScroll()
{
	var x = 0; 
	if(document.body.currentStyle.overflowY != "hidden" && document.body.currentStyle.overflowY != "visible")
	{
		if((document.body.currentStyle.overflowY == "auto" && document.body.scrollHeight >= document.body.offsetHeight) || document.body.currentStyle.overflowY != "auto")
			x = 17;
	}
	return x;
}
function showColumnResizeLine()
{	
	if(resizeline != null)
	{		
		var gt = currheader.getGridEXTable(); 
		var g = gt.getGridEX(); 
		var _htmltable = gt.getHtmlItemsTable().parentElement;		
		var divgridex = g.getHtmlGridEX();
		var divtable = gt.getHtmlDiv();
		var htmlheader = currheader.getHtmlHeader();
		var _rtl = divgridex.getAttribute("rtl") == "1"; 
		var leftmin = -1;
		if(_rtl)		
			leftmin = (getPixelLeft(currcolumn) + currcolumn.offsetWidth) - 9; 
		else
			leftmin = (currcolumn.offsetLeft + 9)  + (divtable.offsetLeft + divgridex.offsetLeft + htmlheader.offsetParent.offsetLeft); 		
		if(!currheader.getIsRoot() && !_rtl)
			leftmin -= gt.getHtmlItemsTable().offsetParent.scrollLeft;
		else if(!currheader.getIsRoot())
		{
			leftmin += (gt.getHtmlItemsTable().offsetParent.scrollWidth - gt.getHtmlItemsTable().offsetParent.offsetWidth) - gt.getHtmlItemsTable().offsetParent.scrollLeft;
			if(g.getRootTable().getHtmlItemsTable().offsetLeft >= 0)
				leftmin += (g.getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - g.getRootTable().getHtmlItemsTable().offsetParent.clientWidth) - (g.getRootTable().getHtmlItemsTable().offsetParent.scrollWidth - g.getRootTable().getHtmlItemsTable().offsetParent.offsetWidth);
		}			
		if(currcolumn.type != null && currcolumn.type == "ch")
			leftmin += gt.getHeaderWidth();			
		if(currcolumn.getAttribute("pec") != null && currcolumn.type != "ch")
			leftmin += 18;				
		if(_rtl)
		{
			var _l = window.event.clientX;
			_l -= fixRightToLeftScroll();
			_l -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft); 
			if(_l >= leftmin)
				return; 
		}
		else
		{
			if(window.event.clientX + getScrollLeft(g) <= leftmin)
				return;
		}			
		var top = getPixelTop(currcolumn);
		if(!currheader.getIsRoot())
			top -= currheader.getGridEXTable().getHtmlItemsTable().offsetParent.scrollTop;			
		var left = event.clientX - 2;
		left += getScrollLeft(g); 		
		if(_rtl)
		{
			left -= fixRightToLeftScroll();
			left -= (document.body.scrollWidth - document.body.clientWidth - document.body.scrollLeft);
		}
		var height = gt.getHtmlItemsTable().offsetParent.offsetHeight; 		
		height -=	(top - getPixelTop(gt.getHtmlItemsTable().offsetParent));
		drawResizeLine(left, top, height); 
	}					
}
function hideColumnResizeLine()
{
	if(resizeline != null)
	{
		resizeline.style.display = "none";
		resizeline.style.visibility = "hidden";
	}
}
function cancelCurrentUIEvents()
{
	var cancel = false;
	if(columnResizing)
	{
		cancelColumnResize(); 
		cancel = true;
	}
	if(columnDraging)
	{
		cancelColumnDraging(); 	
		cancel = true; 
	}
	if(currpressedcolumn != null)	
	{
		ShowColumnUnPressed();
		cancel = true; 
	}
	if(!cancel)
		return; 
	currpressedcolumn = null;			
	couldStartDrag = false; 
	couldDragColumn = null; 
	couldDragHeader = null;
	dragpoint = null;	
	canceledByUser = true; 
}
function loadAdditionalElements(gridex)
{	
	if(resizeline == null)
	{
		resizeline = document.createElement("DIV");
		resizeline.id = "resizeline";		
		resizeline.style.backgroundColor = "black"; 
		resizeline.style.pixelWidth = 1;
		resizeline.style.position = "absolute";
		resizeline.style.overflow = "hidden"; 
		resizeline.style.display = "none";
		resizeline.style.visibility = "hidden"; 		
		resizeline.style.zIndex = 3001; 
		resizeline.style.cursor = cursorResize;
		browser.handleEvent(resizeline, "mouseup", function() { resizeline_onmouseup(); } );
		document.body.appendChild(resizeline);		
	}	
	if(dragcolumn == null)
	{			
		dragcolumn = document.createElement("DIV");
		dragcolumn.id = "dragcolumn"; 
		dragcolumn.varX = -1; 
		dragcolumn.style.cursor = "default";	
		dragcolumn.style.position = "absolute";
		dragcolumn.style.pixelHeight = 1; 
		dragcolumn.style.display = "none";
		dragcolumn.style.visibility = "hidden";
		dragcolumn.style.overflow = "hidden";		
		dragcolumn.style.zIndex = 3001;	
		browser.handleEvent(dragcolumn, "selectstart", function() { drag_onselectstart(); } ); 
		browser.handleEvent(dragcolumn, "mousemove", function() { drag_onmousemove(); } ); 
		browser.handleEvent(dragcolumn, "mouseup", function() { drag_onmouseup(); } );
		document.body.appendChild(dragcolumn); 					
	}			
	var index = 0;
	while(index < 5)
	{			
		var line = document.getElementById("ua" + (index+1)); 
		if(line == null)
		{
			line = document.createElement("DIV");
			line.id="ua" + (index+1);
			line.style.zIndex = 3000;
			line.style.position = "absolute"; 
			line.style.backgroundColor = "red";
			line.style.fontSize = "1px";
			line.style.pixelWidth = (index + 1) * 2;
			line.style.pixelHeight = 1;
			line.style.padding = "0px 0px"; 
			line.style.overflow = "hidden"; 
			line.style.display = "none"; 
			line.style.visibility  = "hidden";
			document.body.appendChild(line);			
		}				
		line = document.getElementById("da" + (index+1)); 
		if(line == null)
		{
			line = document.createElement("DIV");
			line.id = "da" + (index+1); 
			line.style.zIndex = 3000; 
			line.style.position = "absolute"; 
			line.style.backgroundColor = "red";
			line.style.fontSize = "1px"; 				
			line.style.pixelWidth = (index + 1) * 2;
			line.style.pixelHeight = 1;
			line.style.padding = "0px 0px"; 
			line.style.overflow = "hidden"; 
			line.style.display = "none"; 
			line.style.visibility = "hidden";						
			document.body.appendChild(line); 
		}			
		index++;
	}		
	var arrow = document.getElementById("ua" + (index+1)); 
	if(arrow == null)
	{
		arrow = document.createElement("DIV");
		arrow.id  = "ua" + (index+1);
		arrow.style.zIndex = 3000;
		arrow.style.backgroundColor = "red";
		arrow.style.fontSize = "1px"; 
		arrow.style.padding = "0px 0px";
		arrow.style.overflow = "hidden"; 
		arrow.style.position = "absolute";
		arrow.style.pixelWidth = 4;
		arrow.style.pixelHeight = 4;
		arrow.style.display = "none"; 
		arrow.style.visibility = "hidden"; 
		document.body.appendChild(arrow); 
	}	
	arrow = document.getElementById("da" + (index+1)); 
	if(arrow == null)
	{
		arrow = document.createElement("DIV"); 
		arrow.id = "da" + (index+1);
		arrow.style.zIndex = 3000;
		arrow.style.backgroundColor = "red"; 
		arrow.style.fontSize = "1px"; 
		arrow.style.overflow = "hidden";
		arrow.style.padding = "0px 0px"; 
		arrow.style.position = "absolute";
		arrow.style.pixelHeight = 4;
		arrow.style.pixelWidth = 4;
		arrow.style.display = "none"; 
		arrow.style.visibility = "hidden"; 
		arrow.style.zIndex = 3002; 
		document.body.appendChild(arrow); 
	}			
	if(document.getElementById("colpressed") == null)
	{
		var _tdpressed = document.createElement("DIV");
		_tdpressed.id = "colpressed";
		_tdpressed.style.display = "none"; 
		_tdpressed.style.visibility = "hidden";
		_tdpressed.innerHTML = "<span>&nbsp;</span>"; 
		document.body.appendChild(_tdpressed);
	}	
}
function showResizeLine()
{
	if(columnresizeMode == 1)	
		showColumnResizeLine();
	else if(columnresizeMode == 2)	
		showColumnSetResizeLine(); 
}
function cancelResizeLine()
{
	if(columnresizeMode == 1)
		cancelColumnResize();
	else if(columnresizeMode == 2)
		cancelColumnSetResize(); 
}
function gridex_columnbuttonclick(gridexid, tableid, colindex, serverside) 
{	 
	var e = window.event.srcElement;
	while(e.tagName != "TD")
		e = e.parentElement;
	if(e != null && e.disabled)
		return; 	
	window.setTimeout(before_columnbuttonclick(gridexid, tableid, colindex, serverside), 200); 
}
function after_columnbuttonclick(gi, ti, ci, ss)
{
	var g =getGridEXFromID(gi);
	if(g.getGridEXRow() == null)
		return; 		
	var t = g.getTables().getTableByID(ti); 			
	var c = t.getColumns().getGridEXColumn(ci);
	g.FireEvent("ColumnButtonClick", [c]);
	if(ss == 1)
	{
		var r = g.getGridEXRow();
		var a = r.getID()+":"+ci;
		g.DoPostBack(null, "ColumnButtonClick:"+a); 
	}
}
function before_columnbuttonclick(gridexid, tableid, colindex, serverside) { return "after_columnbuttonclick('" + gridexid + "','" + tableid + "'," + colindex + "," + serverside + ")"; }
function getMinimalColumnSetsWidth(columnset)
{	
	var _columnsets = columnset.getGridEXHeader().getColumnSets(); 
	var _columnsetslength = _columnsets.getCount();
	var _columnset = null; 	
	var _cellslength = -1;
	var _htmlcolumnset = null; 
	var _minimalArray = new Array(); 
	var _minimalcols = null; 
	var _minimalwidth = 0; 
	for(var _index = 0; _index < _columnsetslength; _index++)
	{		
		if(_index != columnset.getIndex())
		{		
			_columnset = _columnsets.getColumnSetInIndex(_index); 
			_htmlcolumnset = _columnset.getHtmlColumnSet(); 			
			_cellslength = _htmlcolumnset.cells.length; 
			_minimalcols = new Array(); 
			for(var _icell = 0; _icell < _cellslength; _icell++)
			{				
				_cell = _htmlcolumnset.cells[_icell]; 
				if(_cell.type != "space")
				{
					if(_cell.type != "header") 
					{
						_minimalsize = 8;						
						if(_cell.type == "ch")
							_minimalsize += _columnset.getGridEXTable().getHeaderWidth();
						_minimalsize += getSortWidth(_cell); 					
						if(_minimalcols[_cell.usecol] != null)
						{
							if(_minimalsize >= _minimalcols[_cell.usecol])
								_minimalcols[_cell.usecol] = _minimalsize; 
						}
						else
							_minimalcols[_cell.usecol] = _minimalsize; 
					}
				}
			}
			_minimalwidth = 0; 
			for(var _icol = 0; _icol < _minimalcols.length; _icol++)
				_minimalwidth += _minimalcols[_icol]; 
			
			_minimalArray[_index] = _minimalwidth; 
		}
		else 
			_minimalArray[_index] = -1; 
	}
	return _minimalArray; 
}
function getDivRoot(htmlDiv, divType)
{
	var e = null; 
	var l = htmlDiv.childNodes.length; 
	for(var i=0;i<l;i++)
	{
		e = htmlDiv.childNodes[i]; 
		if(e.tagName =="DIV" && e.type != null && parseInt(e.type, 10) == divType)
			return e;
	}				
	return null;
}
function getScrollLeft(g)
{
	return g.getHtmlGridEX().offsetParent.scrollLeft; 	
}
function getScrollLeftEx(g)
{
	return g.getRootTable().getHtmlItemsTable().offsetParent.scrollLeft; 
}
function getRequiredScrollLeft(g)
{
	var sl = 0;
	if(g != null)
	{
		if(g.getHtmlGridEX().offsetParent != document.body)
		{
			sl += getScrollLeft(g);
			sl -= document.body.scrollLeft;
		}
		else
			sl += document.body.scrollLeft;
	}
	else		
		sl += document.body.scrollLeft;
	return sl;
}
function updateTableSize(_table, _cols)
{
	if(_cols == null)
		_cols = _table.getElementsByTagName("COL"); 
			
	var _sumwidth = 0; 
	for(var i = 0; i < _cols.length; i++)
		_sumwidth += getPixelColWidth(_cols[i].width); 
	_table.style.width = _sumwidth + "px"; 
}
function getRequiredScrollTop(gridex)
{
	var s = 0;
	if(gridex != null)
	{
		if(gridex.getHtmlGridEX().offsetParent != document.body)
		{
			s += gridex.getHtmlGridEX().offsetParent.scrollTop;
			s -= document.body.scrollTop;
		}
		else
			s += document.body.scrollTop;
	}
	else	
		s += document.body.scrollTop;		
	return s;
}
function resetRootTableScroll(rootTable)
{	
	if(rootTable == null)
		return; 
		
	var _rtl = rootTable.getGridEX().getHtmlGridEX().getAttribute("rtl") == "1";
	var _t = rootTable.getHtmlItemsTable(); 
	var _s = _t.offsetParent.scrollLeft;
	if(_s >= 0)
	{
		var _h = getDivRoot(rootTable.getHtmlDiv(), 1); 
		if(_h != null)
		{	
			if(_rtl)
			{
				_s = (_t.offsetParent.scrollWidth - _t.offsetParent.clientWidth) - _s;
				_s *= -1;
				_h.childNodes[0].style.pixelLeft = (_s * -1); 
			}
			else
				_h.style.pixelLeft = (_s * -1);	
		}
		var _th = getDivRoot(rootTable.getHtmlDiv(), 2);
		if(_th != null)
			_th.style.pixelLeft = (_s * -1); 		
		var _nr = getDivRoot(rootTable.getHtmlDiv(), 7);
		if(_nr != null)			
			_nr.style.pixelLeft = (_s * -1); 		
		var _fr = getDivRoot(rootTable.getHtmlDiv(), 9)
		if(_fr != null)
			_fr.style.pixelLeft = (_s * -1);
	}
}
function fixTableSize(rootTable)
{
	var t = rootTable.getHtmlItemsTable(); 
	var h = getDivRoot(rootTable.getHtmlDiv(), 1); 
	if(h != null)		
		t.style.pixelWidth = h.childNodes[0].offsetWidth;	
}
function getMinimalColumnSetsCoreWidth(minimalArray, excludeIndex)
{	
	var m = 0; 
	for(var i = 0; i < minimalArray.length; i++)
	{
		if(i != excludeIndex)
			m += minimalArray[i]; 
	}
	return m; 
}
function ShowColumnPressed(column, pressed)	
{	
	var _colpressed = document.getElementById("colpressed");	
	if(column.type != null && column.type == "header") 
	{
		_colpressed.style.borderLeft = column.style.borderLeft;
		_colpressed.style.borderRight = column.style.borderRight;
		_colpressed.style.borderTop = column.style.borderTop; 
		_colpressed.style.borderBottom = column.style.borderBottom; 
	}
	else
	{
		_colpressed.style.borderLeft = column.currentStyle.borderLeftWidth + " " + column.currentStyle.borderLeftStyle + " " + column.currentStyle.borderLeftColor; 
		_colpressed.style.borderRight = column.currentStyle.borderRightWidth + " " + column.currentStyle.borderRightStyle + " " + column.currentStyle.borderRightColor;  
		_colpressed.style.borderTop = column.currentStyle.borderTopWidth + " " + column.currentStyle.borderTopStyle + " " + column.currentStyle.borderTopColor; 
		_colpressed.style.borderBottom = column.currentStyle.borderBottomWidth + " " + column.currentStyle.borderBottomStyle + " " + column.currentStyle.borderBottomColor; 
	}	
	var _colspan = column.childNodes[0];	
	var _colspanpressed = _colpressed.childNodes[0];			
	_colspanpressed.style.borderLeft =  _colspan.style.borderLeft;	
	_colspanpressed.style.borderRight = _colspan.style.borderRight;	
	_colspanpressed.style.borderTop = _colspan.style.borderTop;		
	_colspanpressed.style.borderBottom = _colspan.style.borderBottom;	
	column.style.borderLeftColor = "graytext";
	column.style.borderRightColor = "graytext";
	column.style.borderTopColor = "graytext";
	column.style.borderBottomColor = "graytext";	
	_colspan.style.borderLeftColor = column.offsetParent.currentStyle.backgroundColor;	// column.offsetParent.style.backgroundColor;
	_colspan.style.borderRightColor = column.offsetParent.currentStyle.backgroundColor;
	_colspan.style.borderTopColor = column.offsetParent.currentStyle.backgroundColor;
	_colspan.style.borderBottomColor = column.offsetParent.currentStyle.backgroundColor;		
	currpressedcolumn = column; 
}	
function ShowColumnUnPressed()
{	
	if(currpressedcolumn != null)
	{
		if(currpressedcolumn.className.indexOf("columnPressedHeader") == 0)
			currpressedcolumn.className = currpressedcolumn.className.substr(19); 
		else
		{
			var _colpressed = document.getElementById("colpressed"); 
			currpressedcolumn.style.borderLeftColor = _colpressed.style.borderLeftColor;
			currpressedcolumn.style.borderRightColor = _colpressed.style.borderRightColor;
			currpressedcolumn.style.borderTopColor = _colpressed.style.borderTopColor;
			currpressedcolumn.style.borderBottomColor = _colpressed.style.borderBottomColor;		
			var _colspanpressed = _colpressed.childNodes[0]; 		
			var _colspan = _colpressed.childNodes[0];
			_colspan.style.borderLeftColor = _colspanpressed.style.borderLeftColor;
			_colspan.style.borderRightColor = _colspanpressed.style.borderRightColor;
			_colspan.style.borderTopColor = _colspanpressed.style.borderTopColor;
			_colspan.style.borderBottomColor = _colspanpressed.style.borderBottomColor;
		}
	}
}
function getGridEXOffsetParent(gridex)
{
	if(gridex.parentElement.tagName == "DIV")
		return gridex.parentElement;	
	else if(gridex.offsetParent.tagName == "BODY")
		return gridex.offsetParent;
	else if(gridex.parentElement.tagName != "FORM")
		return gridex.parentElement; 
	else 
		return gridex.offsetParent; 
}
function getBorderStyleWidth(border)
{
	var width = 0;			
	if(border != null && border != "")
	{
		var indexOf = border.indexOf("px");
		if(indexOf > 0)
			width += parseInt(border.substr(0, indexOf));		
	}
	return width; 
}
function getRealCellWidth(sc, cell, gridexID)
{		    
	sc.style.paddingLeft = cell.childNodes[0].currentStyle.paddingLeft; 
	sc.style.paddingRight = cell.childNodes[0].currentStyle.paddingRight;
	var _childlength = cell.childNodes.length; 
	var _array = new Array(_childlength); 
	for(var _ichild = 0; _ichild < _childlength; _ichild++)
	{	
		_child = cell.childNodes[_ichild]; 
		_array[_ichild] = _child.innerHTML; 		
	}
	sc.innerHTML = _array.join(); 				
	return  sc.offsetWidth; 
}
function getColumnFromElement(e)
{
	while(e != null && e.tagName != "TD")
		e = e.parentElement;
	return e;
}
function getPadding(padding)
{
	var pixelpadding = 0;
	var indexof = padding.indexOf("px");
	if(indexof > 0)
		pixelpadding = parseInt(padding.substr(0, indexof));		
	return pixelpadding;
}
function getPaddingBottom(item)
{
	var s = null;
	s = item.style; 
	if(s.paddingBottom != "")
		return getPadding(s.paddingBottom);			
	s = item.currentStyle;
	if(s.paddingBottom != "")
		return getPadding(s.paddingBottom);		
	return 0;
}
function getPaddingTop(item)
{
	var s = null;
	s = item.style; 
	if(s.paddingTop != "")
		return getPadding(s.paddingTop);			
	s = item.currentStyle;
	if(s.paddingTop != "")
		return getPadding(s.paddingTop);		
	return 0;	
}
function getPaddingLeft(item)
{	
	var _style = null;
	_style = item.style; 
	if(_style.paddingLeft != "")
		return getPadding(_style.paddingLeft);	
		
	_style = item.currentStyle;
	if(_style.paddingLeft != "")
		return getPadding(_style.paddingLeft);
		
	return 0;
}
function getStylePaddingLeft(style)
{
	if(style.paddingLeft != "")
		return getPadding(style.paddingLeft); 
		
	return 0; 
}
function getPaddingRight(item)
{
	var _style = null;
	_style = item.style;
	if(_style.paddingRight != "")
		return  getPadding(_style.paddingRight);	
		
	_style = item.currentStyle; 
	if(_style.paddingRight != "")
		return getPadding(_style.paddingRight); 
	
	return 0;
}
function getStylePaddingRight(style)
{
	if(style.paddingRight != "")
		return getPadding(style.paddingRight); 
		
	return 0; 
}
function getBorderTopWidth(td)
{
	var width = 0;
	if(td.style.borderTopWidth != "")
		width = getBorderStyleWidth(td.style.borderTopWidth); 
	else if(td.currentStyle.borderTopWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderTopWidth); 
	return width; 	
}
function getBorderBottomWidth(td)
{
	var width = 0; 
	if(td.style.borderBottomWidth != "")
		width = getBorderStyleWidth(td.style.borderBottomWidth);
	else if(td.currentStyle.borderBottomWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderBottomWidth); 
	return width; 
}
function getBorderLeftWidth(td)
{
	var width = 0;
	if(td.style.borderLeftWidth  != "")
		width = getBorderStyleWidth(td.style.borderLeftWidth); 
	else if(td.currentStyle.borderLeftWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderLeftWidth);
	return width;
}
function getBorderRightWidth(td)
{
	var width = 0; 
	if(td.style.borderRightWidth != "")
		width = getBorderStyleWidth(td.style.borderRightWidth);
	else if(td.currentStyle.borderRightWidth != "")
		width = getBorderStyleWidth(td.currentStyle.borderRightWidth); 
	return width; 
}
function getElementWidth(element)
{
	var width = 0; 
	width = element.style.pixelWidth; 
	if(width == 0)		
	{
		if(element.currentStyle.width != "")
			width = getPixelWidth(element.currentStyle.width); 
	}
	return width; 
}
function getStyleBorderWidth(style)
{	
	var width = 0; 
	if(style.borderLeftWidth != "")
	{
		var indexOf = style.borderLeftWidth.indexOf("px"); 
		if(indexOf > 0)
			width += parseInt(style.borderLeftWidth.substr(0, indexOf)); 
	}	
	if(style.borderRightWidth != "")
	{
		var indexOf = style.borderRightWidth.indexOf("px");
		if(indexOf > 0)
			width += parseInt(style.borderRightWidth.substr(0, indexOf)); 		
	}
	return width; 
}
function getBorderWidth(td)
{
	var width = 0;	
	if(td.style.borderLeftWidth != "")
	{
		var indexOf = td.style.borderLeftWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.style.borderLeftWidth.substr(0, indexOf)); 
	}
	else if(td.currentStyle.borderLeftWidth != "")
	{
		var indexOf = td.currentStyle.borderLeftWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.currentStyle.borderLeftWidth.substr(0, indexOf));
	}	
	if(td.style.borderRightWidth != "")
	{
		var indexOf = td.style.borderRightWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.style.borderRightWidth.substr(0, indexOf)); 
	}
	else if(td.currentStyle.borderRightWidth != "")
	{
		var indexOf = td.currentStyle.borderRightWidth.indexOf("px");
		if(indexOf > 0) width += parseInt(td.currentStyle.borderRightWidth.substr(0, indexOf)); 
	}
	return width; 
}
function isInResizeArea(td, htmlTable, table)
{
	var x = window.event.offsetX;
	var y = window.event.offsetY;		
	if(browser.isIE)
	{
		if(td.getElementsByTagName("SPAN").length == 2)
		{
			var s = td.getElementsByTagName("SPAN")[1] ;
			if(s == window.event.srcElement || s.contains(window.event.srcElement))
				x += td.getElementsByTagName("SPAN")[0].offsetWidth; 
		}
	}	
	var ylow = 0;
	var yhigh = ylow + td.offsetHeight; 		
	var xlow;
	var xhigh; 				
	var tdleft = td.offsetLeft; 				
	if(tdleft + td.offsetWidth + 2 <= htmlTable.offsetWidth)
	{
		xlow = td.offsetWidth - 5;	
		xhigh = td.offsetWidth + 3; 		
	}
	else
	{
		xlow = td.offsetWidth - 5;
		xhigh = td.offsetWidth; 
	}				
	if((x >= xlow && x <= xhigh) && (y >= ylow && y <= yhigh))
		return true;
	else
		return false;
}
function getTypeOfTD(e)
{
	if(e == null)	
		return ""; 
		
	while(e != null && e.tagName != "TD")
		e = e.parentElement; 
		
	if(e != null && e.tagName == "TD")
		return e.getAttribute("type"); 
	else
		return ""; 
}
function getPreviewClassName(row)
{
	if(row.getIsAlternating())
		return row.getTable().getRowCss(4); 
	else
		return row.getTable().getRowCss(5); 
}	
function getPreviewSelectedClassName(row)
{
	return row.getTable().getRowCss(6); 
}	
function getClassName(row)
{
	var className = "";
	className = row.getGridEX().getClassName(row.getID()); 
	if(className != null)
		return className; 
		
	var rowType = row.getType(); 
	switch(rowType)
	{
		case 3:
		{
			if(!row.getIsAlternating())
				className = row.getTable().getRowCss(0); 
			else
				className = row.getTable().getRowCss(2); 
		} break;			
		case 5:
			className = row.getTable().getRowCss(7); 
		break; 			
		case 8:			
			className = row.getTable().getRowCss(14); 
			break; 			 
		case 9:
			className = row.getTable().getRowCss(20); 
		break; 			
		case 11:
			className = row.getTable().getRowCss(26); 
		break;
		case 12:
			className = row.getTable().getRowCss(28); 
		break; 			
		default:
			className = row.getTable().getRowCss(0); 
		break; 
	}		
	return className; 
}			
function getSelectedClassName(row)
{
	var className = row.getGridEX().getSelectedClassName(row.getID());
	if(className != null)
		return className; 		
	var rowType = row.getType(); 
	switch(rowType)
	{
		case 3:
			className = row.getTable().getRowCss(1);
		break; 			
		case 8:			
			className = row.getTable().getRowCss(30); 
		break; 									
		default:
			className = row.getTable().getRowCss(1); 
		break; 
	}		
	return className; 
}
function getColumnIDFromCellID(cellID)
{
	var i = cellID.lastIndexOf("_L"); 
	if(i > 0)
		return cellID.substring(0, i);
	return ""; 
}
function getObjectFromID(id)
{
	var obj = null;
	eval("obj = " + id + ";"); 
	if(obj == null)
		throw Error("invalid object with id '" + id + "'"); 		
	return obj; 
}
function getGridEXFromID(id)
{
	return getObjectFromID(id);
}
function getHorizontalScrollOffset(gridex)
{	
	var t = gridex.getRootTable();
	var o = t.getHtmlItemsTable().offsetParent.scrollLeft;
	return o; 
}
function getVerticalScrollOffsetCore(e)
{	
	return e.scrollTop;
}
function getVerticalScrollOffset(gridex)
{	
	return getVerticalScrollOffsetCore(gridex.getRootTable().getHtmlItemsTable().offsetParent); 	
}
function getPixelTop(e)
{
	var t = 0;
	while(e != null)
	{					
		t += e.offsetTop;
		if(e.scrollTop != null && e.scrollTop != 0 && e.tagName != "BODY")
		{
			if(e.tagName == "DIV" && e.getAttribute("type") == "4")
			{ } 
			else
			{
				if(e.tagName != "HTML")
					t -= e.scrollTop;
			}
		}
		e = e.offsetParent; 				
	}
	return t; 
}
function getAdjustPixelTop(gridex, excludeBody, excludeTable)
{
	var adjust = 0;
	if(excludeBody == null || !excludeBody)
		adjust += document.body.scrollTop;		
	if(excludeTable == null || !excludeTable)
		adjust += gridex.getRootTable().getHtmlItemsTable().offsetParent.scrollTop;
	return adjust;
}
function getAvailableClientHeight(element)
{
	return document.body.clientHeight;		
}
function getBottomOffset(element)
{
	var o = 0;
	o += getPixelWidth(element.currentStyle.marginBottom); 
	return o; 
}
function getTopOffset(element)
{
	var o = 0; 
	o += getPixelWidth(element.currentStyle.marginTop);
	return o;
}
function getOffsetTopForEdit(gridEXHtml)
{
	var o = 0; 
	if(gridEXHtml.currentStyle.position == "relative")
	{
		var op = getGridEXOffsetParent(gridEXHtml); 
		if(op.tagName != "BODY")					
			o += getPixelTop(op); 
	}
	return o; 
}
function getOffsetLeftForEdit(gridEXHtml)
{
	var o = 0; 
	if(gridEXHtml.currentStyle.position == "relative")
	{
		var op = getGridEXOffsetParent(gridEXHtml); 
		if(op.tagName != "BODY")
			o += getPixelLeft(op); 
	}
	return o; 
}
function getRTLScrollWidth(e)
{
	if(e.offsetParent.scrollHeight >= e.offsetParent.clientHeight)
	{
		if(e.offsetLeft <= 0)
			return 17;
	}
	return 0; 
}
function getPixelLeft(e, rtl)
{
	var l = 0; 
	while(e != null)
	{		
		if(rtl == null || !rtl)
			l += e.offsetLeft;		
		else if(rtl != null && rtl)
		{
			if(e.offsetLeft > 0)
				l += e.offsetLeft; 
		}
		e = e.offsetParent; 
	}
	return l;
}
function getPercentWidth(w)
{
	if(w.indexOf("%") > 0)
		return parseInt(w.substring(0, w.indexOf("%")), 10);		
	return 0; 
}
function getPixelWidth(w)
{
	if(w.indexOf("px") > 0)
		return parseInt(w.substring(0, w.indexOf("px")), 10); 
	return 0; 
}
function getPixelColWidth(w)
{
	if(w == null || w.length == 0)
		return 0; 		
	if(w.indexOf("px") > 0)
		return getPixelWidth(w); 		
	return parseInt(w, 10); 
}
function getRootRowFromInner(e)
{
	while(e != null)
	{
		if(e.nodeType == 1 && e.tagName == "TR" && e.getAttribute("id") != null && e.getAttribute("t") != null)
			return e;
		e = e.parentElement; 
	}
	if(e == null)
		throw Error("unable to find root row"); 
}
function getHierarchicalRow(element)
{	
	var isrow = false; 
	do
	{	
		if(element == null)
			throw Error("hierarchical or grouped row is not found"); 
	
		if(element.tagName == "TR" && element.id != null && element.getAttribute("t") != null) 
			isrow = true;
		else
			element = element.parentElement;
		
	} while(!isrow);
	return element; 	
}
function getHierarchicalRowTop(e)
{	
	e = getHierarchicalRow(e); 		
	return e.offsetTop;	
}
function getTextNode(node)
{	
	if(node.nodeType == 3)
		return node;
	else
	{
		for(var i=0;i<node.childNodes.length;i++)
		{
			var anode = getTextNode(node.childNodes[i]);
			if(anode != null)
				return anode;				
		}
		return null;
	}
}
function getSortWidth(column)
{		
	if(column.childNodes.length == 2)
		return column.childNodes[1].offsetWidth; 
	else
		return 0; 
}
function replaceInstances(v, f, r)
{
	while(v.indexOf(f) >= 0)
	{ v = v.replace(f, r); }
	return v; 
}
function normalizeValue(v)
{
	if(v != null && typeof(v) == "string")
	{
		v = replaceInstances(v, "&ent;", "\r\n");
		v = replaceInstances(v, "&quot;", "\"");		
		v = replaceInstances(v, "&apos;", "'"); 
	}
	return v;	
}
function updateColumnDefinitionInField(field, cellID, cellPos, cellWidth, innerWidth)
{		
	var _exists = false; 	
	var _colsdef = null; 
	if(field.value == null || field.value.length == 0)
		_colsdef = new Array(); 
	else
		_colsdef = field.value.split(","); 
	
	var _newpos = null;
	var _newwidth = null; 
	var l = _colsdef.length; 	
	if(l > 0)
	{		
		var _coldef = null; 	
		var _change = false; 
		for(var i = 0; i < l; i++)
		{
			_coldef = _colsdef[i].split(":"); 				
			if(_coldef[0] == cellID)
			{				
				_exists = true; 
				_change = false;
				if(parseInt(_coldef[1],10) != cellPos)
					_change = true; 
					
				if(parseInt(_coldef[2], 10) != cellWidth)
					_change = true; 
					
				if(innerWidth != null && parseInt(_coldef[3], 10) != innerWidth)
					_change = true; 
					
				if(_change)
				{
					_newpos = (parseInt(_coldef[1],10) != cellPos) ? cellPos : _coldef[1]; 					
					if(cellWidth != -1 && cellWidth != parseInt(_coldef[2], 10))
						_newwidth = cellWidth;
					else
						_newwidth = parseInt(_coldef[2], 10);
					var _item = 
					_colsdef[i] = cellID + ":" + _newpos + ":" + _newwidth; 
					if(innerWidth != null)
						_colsdef[i] += ":" + innerWidth; 
				}
			}
		}		
		if(!_exists)
		{
			var _item = cellID + ":" + cellPos + ":" + cellWidth;
			if(innerWidth != null)
				_item += ":" + innerWidth; 
			_colsdef[_colsdef.length] = _item; 			
		}
	}
	else
	{	
		var _item = cellID + ":" + cellPos + ":" + cellWidth;
		if(innerWidth != null)
			_item += ":" + innerWidth; 
		_colsdef[_colsdef.length] = _item; 
	}	
	field.value = _colsdef.join(","); 
}
function setGroupEventData(gridexID, gaction, gtable, gpos, gnewpos, gcolumn, hcolumnpos)
{		
	var input = document.getElementsByName(gridexID + "_eventdata")[0]; 
	if(input == null)
		throw Error("event data field is null"); 
			
	input.value = gaction + ":" + gtable + ":" + gpos + ":" + gnewpos + ":" + gcolumn + ":" + hcolumnpos; 
}
function rebuildArray(array)
{
	var j = 0;
	for(var i=0;i<array.length;i++)
	{
		if(array[i] != null)
		{
			array[j] = array[i];
			j++;
		}
	}
	array.length = j;
}
function unloadArray(arr)
{
	if(arr != null)
	{
		for(var i = 0; i < arr.length; i++)
		{
			var o = arr[i];					
			delete o; 
			o = null;
		}
	}	
}
function unloadObjectArray(arr)
{
	if(arr != null)
	{
		for(var i=0;i<arr.length;i++)
		{
			var o=arr[i];
			if(o.Unload != null)
				o.Unload(); 
			delete o;
			o = null; 
		}
	}
}
function unloadRows(rows)
{
	if(rows != null)
	{
		for(var i=0;i<rows.length;i++)
		{
			var r=rows[i];
			r.Unload();
			delete r; 
			r = null;
		}		
	}	
}
function cancelEvent()
{
	window.event.returnValue = false;
	window.event.cancelBubble = true;
	return false;
}
function getRealWidth(e,f)
{
	var w = getPixelWidth(e.currentStyle.width); 
	if(w != 0)
	{
		if(w == e.offsetWidth)
			return e.clientWidth; 
		else
		{
			if(f == null || f)
				w -= (getPaddingLeft(e.getElementsByTagName("SPAN")[0]) + getPaddingRight(e.getElementsByTagName("SPAN")[0])); 		
			else
				return w; 
		}
	}
	else
		w = e.scrollWidth;		
	return w; 
}
function getRealHeight(e,f)
{
	var h = getPixelWidth(e.currentStyle.height); 
	if(h != 0)
	{
		if(h == e.offsetHeight)
			return e.clientHeight;
		else
		{
			if(f == null || f)
				h -= (getPaddingTop(e.getElementsByTagName("SPAN")[0]) + getPaddingBottom(e.getElementsByTagName("SPAN")[0])); 
			else
				return h; 
		}
	}
	else
	{
		if(e.clientHeight > e.scrollHeight)
			h = e.clientHeight;
		else
			h = e.scrollHeight;		
	}
	return h; 
}