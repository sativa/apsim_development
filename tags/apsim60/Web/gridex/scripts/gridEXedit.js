//////////////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////////////
var cellForEdit = null; 
var isEditing = false; 
var objectForEdit = null;
var TEXTBOX_MAXLENGTH= 2147483647;
var currentDropDown = null;
function showDropDownBackFrame(left, top, width, height)
{
	var element = document.getElementById("dropDownBackFrame");
	if(element == null)
	{
		var frameSrc = "javascript:void(0);";
		element = document.createElement("IFRAME");
		try
		{
			frameSrc  =	 editorsFrameUrl;
		} catch(err) { } 
		element.src = frameSrc; 
		document.body.appendChild(element); 
		element.id = "dropDownBackFrame";
		element.style.position = "absolute";
		element.style.visibility = "hidden";
		element.style.zIndex = 999;
	}
	element.style.pixelLeft = left;
	element.style.pixelTop = top;
	element.style.pixelWidth = width;
	element.style.pixelHeight = height;
	element.style.visibility = "visible"; 	
}
function hideDropDownBackFrame()
{
	var element = document.getElementById("dropDownBackFrame");
	if(element != null)
		element.style.visibility = "hidden";
}
function attachCheckBoxEvent(checkbox, gridexid)
{
	checkbox.setAttribute("we", 1); 	
	browser.handleEvent(checkbox, "keydown", function e() { 
		if(window.event.keyCode == 9)
		{
			getGridEXFromID(gridexid).TabElementChanging(true); 
			window.event.returnValue = false;
			window.event.cancelBubble = true;		
			return false; 
		}
		else if(window.event.keyCode == 32)
		{
			var row = getGridEXFromID(gridexid).getGridEXRow();
			if(!row.getCellSelected().getColumn().getSelectable())
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true;
				return false;
			}
		}
		else if(window.event.keyCode == 13)
		{			
			var row = getGridEXFromID(gridexid).getGridEXRow(); 
			if(row.getRowType() == "FilterRow")
				getGridEXFromID(gridexid).ResumeFilterOperation(); 
			else if(row.getRowType() == "Record" || row.getRowType() == "NewRecord")
				getGridEXFromID(gridexid).ResumeEditOperation(); 
		}
	}); 
}
function dropdownbutton_mouseover(img,url)
{		
	if(img.getAttribute("status") == null || parseInt(img.getAttribute("status"), 10) != 1)
		img.src = url; 
}
function DropDownRow_OnMouseOver(row, itemCSS, itemSelCSS)
{	
	if(currentDropDown != null && currentDropDown.ItemOnMouseOver != null)
		currentDropDown.ItemOnMouseOver(row, itemCSS, itemSelCSS); 
}
function DropDownRow_OnMouseOut(row, itemCSS, itemSelCSS)
{		
	if(currentDropDown != null && currentDropDown.ItemOnMouseOut != null)
		currentDropDown.ItemOnMouseOut(row, itemCSS, itemSelCSS); 
}
function DropDownRow_OnClick(row, argument)
{	
	if(currentDropDown != null && currentDropDown.ItemOnClick != null)
		currentDropDown.ItemOnClick(row, argument); 
}
function DropDownRow_OnExpand(rowID, action)
{	
	if(currentDropDown != null && currentDropDown.ItemOnExpand != null)
		currentDropDown.ItemOnExpand(rowID, action);
}
function gcddbutton_onblur()
{
	var e = window.event.srcElement;
	while(e != null && e.tagName != "DIV"  && e.getAttribute("combo") == null)
		e = e.parentElement;
	if(e != null)
		retrieveEditor("e"+e.getAttribute("combo")).button_onblur();
}
function gcddbutton_onmousedown()
{
	var e = window.event.srcElement;
	while(e != null && e.getAttribute("combo") == null)
		e = e.parentElement;
	if(e != null)
		retrieveEditor("e"+e.getAttribute("combo")).button_onmousedown();
}
function gcddtextbox_onblur()
{
	var e = window.event.srcElement;
	retrieveEditor("e"+e.getAttribute("combo")).textbox_onblur();
}
function gcddtextbox_onchange()
{
	var e = window.event.srcElement;
	retrieveEditor("e"+e.getAttribute("combo")).textbox_onchange();
}
function gcddtextbox_onkeypress()
{
	var e = window.event.srcElement;
	retrieveEditor("e"+e.getAttribute("combo")).textbox_onkeypress();
}
function gcddtextbox_onkeydown()
{
	var e = window.event.srcElement;
	retrieveEditor("e"+e.getAttribute("combo")).textbox_onkeydown();
}
function gcddtextbox_onkeyup()
{
	var e = window.event.srcElement;
	retrieveEditor("e"+e.getAttribute("combo")).textbox_onkeyup(); 
}
function getListItem(e)
{
	if(e == null)
		throw Error("unable to find element"); 
			
	if((e.nodeType == 1 || !browser.isIE) && e.tagName == "TR")
		return e;
	else
		return getListItem(e.parentElement);
}	
function GridEXEditingArgs(cell, value)
{
	var editingCell = cell; 
	var editingValue = value; 	
	this.getCell = getCell; 
	this.getValue = getValue;
	this.setValue = setValue;	
	function getCell() { return editingCell; }	
	function getValue() { return editingValue; }	
	function setValue(value) { editingValue = value; }	
	return this; 
}
function GridEXUpdatingCellArgs(cell, initialValue, value)
{
	var updatingCell = cell;
	var updatingInitialValue = initialValue;
	var updatingValue = value; 	
	this.getCell = getCell;
	this.getValue = getValue; 
	this.getInitialValue = getInitialValue; 
	this.setValue = setValue; 	
	function getCell() { return updatingCell; }	
	function getInitialValue() { return updatingInitialValue; }	
	function getValue() { return updatingValue; }			
	function setValue(value) { updatingValue = value; }	
	return this;
}
function ParseEditValueArgs(cell, value, action)
{
	var action = action;
	var cell = cell;	
	var proposedValue = value;
	this.getAction = getAction; 
	this.getCell = getCell;
	this.getValue = getValue;
	this.setValue = setValue; 
	function getAction() { return action; } 
	function getCell() { return cell; } 
	function getValue() { return proposedValue; }
	function setValue(value) { proposedValue = value; } 
	return this; 
}
function gmcc(id)
{
	var control = null;
	eval("control = " + id);
	if(control == null)
		throw new Error("MultiColumnCombo '" + id + "' object is null");		
	control.Initialize(); 
}
function mccdb_onmouseover(element, css)
{
	element.className = css;	
}
function GridEXMultiColumnCombo(id,sid,ddid,type,image,readonly,autopb,events)
{	
	var obj = null; 
	var events = events; 
	this.autoPostBack = (autopb == 1) ? true : false; 
	this.serverID = sid; 
	this.onValueChanged = onValueChanged; 
	this.Initialize = Initialize;
	this.DropDown = obj; 
	function onValueChanged()
	{
		FireEvent("ValueChanged", [control]); 
	}
	this.Text = function()
	{
		var text = ""; 
		try
		{
			var element = document.getElementById(ddid+"_textbox"); 
			text = element.value; 
		}
		catch(e) {}
		return text; 
	}
	this.Value = function ()
	{
		var value = null; 
		try
		{
			var element = document.getElementsByName(id+"_value")[0]; 
			value = element.value; 
		}
		catch(e) {} 
		return value; 
	}	
	var obj = null; 
	function Initialize()
	{
		obj = null; 
		if(type == 1)
			obj =  new GridEXComboDropDown(ddid,true,id); 
		else
			obj = new GridEXDropDown(ddid,true,id);
		obj.image = image; 		
		obj.readonly = (readonly == 1);
		control.DropDown = obj; 
		FireEvent("Load", [control]); 
	}	
	function FireEvent(eventname, eventparams)
	{	
		if(events == null || events.length == 0)
			return null;			
		var l = events.length; 
		var e = "";		
		for(var i=0; i<l; i=i+2)
		{
			if(events[i] == eventname) 
			{
				e = events[i+1];
				i = l; 
			}
		}
		if(e != "")
		{	
			var p = "";
			var l = eventparams.length; 
			for(var i = 0; i < l; i++)
			{
				if(p != "") p += ",";
				p += "eventparams[" + i + "]"; 
			}		
			var c = "return eval(" + e + "("  + p + "))";
			var f = new Function("eventparams", c);
			return f(eventparams);
		}
		return null; 
	}
	browser.handleEvent(window, "load", function() { gmcc(id); });	
	var control = this; 
	return this; 
}
function GridEXComboDropDown(id,mcc,mccid)
{
	var id = id;	
	var _clientevents = null;
	try {  _clientevents = eval(id + "_client_events"); } catch(E) { _clientevents = null; }	
	var gridEX = new GridEX(id, eval(id + "_client"), eval(id + "_client_tables"), eval(id + "_client_selected_items"), eval(id + "_client_rows_css"), eval(id + "_client_hidden_values"), _clientevents );
	eval(id + " = gridEX;");
	if(mcc == null || !mcc)
		gridEX.Initialize()
	else
		gridEX.gridEX_onload(); 
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXComboDropDown '" + id + "'"); 		
	if(mcc == null || !mcc)
	{
		if(button.parentElement != null && button.parentElement.tagName != "BODY")
			document.body.appendChild(button); 
	}		
	var textbox = document.getElementById(id + "_textbox"); 	
	if(textbox == null)
		throw Error("unable to find textbox for GridEXComboDropDown '" + id + "'"); 	
	if(mcc == null || !mcc)
	{	
		if(textbox.parentElement != null && textbox.parentElement.tagName != "BODY")
			document.body.appendChild(textbox); 
	}		
	var innerGridEX = gridEX.getHtmlGridEX();
	if(innerGridEX.parentElement != null && innerGridEX.parentElement.tagName != " BODY")
		document.body.appendChild(innerGridEX); 		
	var itemValue = null; 
	var itemDisplay = null; 
	var characterCasing = -1; 	
	var currentItemID = null;
	var currentItem = null; 	
	var initialHeight = -1;
	var itemOwner = null;	
	var owner = null;
	var left = -1;
	var top = -1; 
	var height = -1; 
	var width = -1; 	
	var image = "";
	var readonly = false; 
	this.image = image;	
	this.readonly = readonly; 	
	this.getCharacterCasing = getCharacterCasing; 	
	this.getDisplay = getDisplay; 
	this.getDisplayByValue = getDisplayByValue;
	this.getID = getID; 
	this.getInnerTextBox = getInnerTextBox; 
	this.getValue = getValue; 	
	this.setCharacterCasing = setCharacterCasing; 
	this.setOwner = setOwner;
	this.setLeft = setLeft; 
	this.setTop = setTop; 
	this.setWidth = setWidth; 
	this.setHeight = setHeight; 
	this.setStyle = setStyle; 
	this.setValue = setValue; 	
	this.Hide = Hide; 
	this.Show = Show; 	
	this.GridEX_OnBlur = GridEX_OnBlur; 
	this.GridEX_OnKeyDown = GridEX_OnKeyDown; 	
	this.ItemOnMouseOver = ItemOnMouseOver; 
	this.ItemOnMouseOut = ItemOnMouseOut; 
	this.ItemOnClick = ItemOnClick;
	this.ItemOnExpand = ItemOnExpand; 
	this.Unload = Unload;
	this.button_onblur = button_onblur;
	this.button_onmousedown = button_onmousedown;
	this.textbox_onblur = textbox_onblur;
	this.textbox_onchange = textbox_onchange;
	this.textbox_onkeypress = textbox_onkeypress;
	this.textbox_onkeydown = textbox_onkeydown;
	this.textbox_onkeyup = textbox_onkeyup; 
	function getDisplayFromItem(item) { return item.getAttribute("displayMember"); }	
	function getValueFromItem(item) { return item.getAttribute("value"); }	
	function getID() { return id; }	
	function getInnerTextBox() { return textbox; }	
	function getValue() { return itemValue; }	
	function getDisplay() { return itemDisplay; }	
	function getCharacterCasing() { return characterCasing; }	
	function setCharacterCasing(value) { characterCasing = value; }	
	function setOwner(value) { owner = value; }	
	function setLeft(value) { left = value; }	
	function setTop(value) { top = value; }	
	function setHeight(value) { height = value; }	
	function setWidth(value) { width = value; }	
	function setStyle(style)
	{
		if(style != null)
		{			
			textbox.style.fontFamily = style.fontFamily;
			textbox.style.fontSize = style.fontSize; 
			textbox.style.textAlign = style.textAlign;
		}
	}
	function getDisplayByValue(value)
	{
		var item = gridEX.getRootTable().FindRowByValue(value);
		if(item != null && item.length == 2)
			return getDisplayFromItem(item[1]); 
		return "";
	}	
	function setValue(value, text)
	{	
		itemValue = value;
		var item = gridEX.getRootTable().FindRowByValue(value); 
		if(item != null && item.length == 2)
		{						
			itemDisplay = getDisplayFromItem(item[1]); 
			updateTextBox(); 
		}
		else if(value == null && text != null)
		{
			itemDisplay = text;
			updateTextBox(); 
		}
		else if(item == null)
		{
			if(value != null)
				itemDisplay = value; 
			else
				itemDisplay = "";
			updateTextBox(); 
		} 
	}	
	function getItemFromInnerRow(element)
	{
		while(element != null)
		{
			if(element.nodeType == 1 && element.tagName == "TR" && element.id != null && element.getAttribute("t") != null && element.getAttribute("t") != "")
				return element; 		
			element = element.parentElement; 
		}			
		if(element == null)
			throw Error("unable to find item"); 
	}	
	function unselectDropDownItem(itemID, item)
	{		
		if(itemID == null && item == null)
			return; 	
		var css = gridEX.getClassName(itemID); 
		if(css != null)
			item.className = css; 
		else
		{
			css = gridEX.getRootTable().getRowCss(0); 
			item.className = css; 
		}
	}	
	function selectDropDownItem(itemID, item)
	{
		if(itemID == null && item == null)
			return; 	
		var css = gridEX.getSelectedClassName(itemID); 
		if(css != null)
			item.className = css;
		else
		{
			css = gridEX.getRootTable().getRowCss(1); 
			item.className = css; 
		}
	}	
	function resetTextBoxImage()
	{
		if(gridEXDropDown.image != "")
		{
			textbox.style.backgroundImage = "none"; 
			textbox.style.paddingLeft = "0px"; 
		}
	}
	function commitNewText()
	{	
		if(textbox.value == itemDisplay)
			return;			
		var value = null; 
		var item = gridEX.getRootTable().FindRowWithDisplay(textbox.value); 
		if(item != null && item.length == 2)
		{
			value = getValueFromItem(item[1]);
			if(value != getValue())
				commitNewValueCore(item[1], value); 				
		}
		else if(item == null)
		{
			if(owner != null && owner.NotInList != null)
			{
				if(textbox.value != null)
					owner.NotInList(textbox.value); 
			}
			resetTextBoxImage(); 
		}
		if(mcc != null && mcc)
		{
			document.getElementsByName(mccid+"_text")[0].value = textbox.value;
			document.getElementsByName(mccid+"_value")[0].value = (value == null) ? "" : value; 
		}		
	}	
	function commitNewValueCore(item, value)
	{
		itemDisplay = getDisplayFromItem(item);
		itemValue = value; 									
		if(owner != null && owner.ValueChanged != null)
			owner.ValueChanged();	
	}
	function commitNewValue(item)
	{	
		var value = getValueFromItem(item);
		if(value != getValue())
		{	
			commitNewValueCore(item, value); 
			updateTextBox(); 
		}
		if(mcc != null && mcc)
		{
			document.getElementsByName(mccid+"_value")[0].value = (value == null) ? "" : value; 			
			var obj = getObjectFromID(mccid);
			obj.onValueChanged(); 
			if(obj.autoPostBack)
				__doPostBack(obj.serverID,"ValueChanged"); 
		}
	}	
	function updateTextBox()
	{
		textbox.value = itemDisplay;
		if(gridEXDropDown.image != "")
		{
			try
			{				
				var row = gridEX.getGridEXRow(); 							
				var img = row.getCellByColumnKey(gridEXDropDown.image).getInnerCell().getElementsByTagName("IMG")[0];
				textbox.style.backgroundImage = "url('" + img.src + "')"; 
				textbox.style.backgroundPosition = "center left"; 
				textbox.style.backgroundRepeat = "no-repeat"; 
				textbox.style.paddingLeft = ((img.offsetWidth == 0) ? 16 : img.offsetWidth) + 2 +  "px"; 
			}
			catch(err)
			{ } 
		}
		if(textbox.style.visibility == "visible")
			textbox.select(); 
	}	
	function updateDropDown()
	{							
		if((mcc == null || !mcc) && gridEX.getSelectedItems() != null && gridEX.getSelectedItems().Count() > 0)
			gridEX.getSelectedItems().Clear(); 	
		var item = gridEX.getRootTable().FindRowWithDisplay(textbox.value); 
		if(item != null && item.length == 2)
		{
			if(currentItemID != item[0] && currentItem != item[1])
				unselectDropDownItem(currentItemID, currentItem); 							
			selectDropDownItem(item[0], item[1]); 			
			currentItemID = item[0]; 
			currentItem = item[1];
		}
		else
		{
			unselectDropDownItem(currentItemID, currentItem);
			currentItemID = currentItem = itemOver = null; 
		}
		if(currentItem != null)
			itemOver = currentItem; 
		if(itemValue != null && currentItem != null)
			gridEX.setCurrentRow(gridEX.RetrieveRow(getRootRowFromInner(currentItem), currentItem, null), true); 
		else
		{
			if(gridEX.getGridEXRow() != null)
			{
				currentItemID = getRootRowFromInner(gridEX.getGridEXRow().getInnerRow());
				currentItem = gridEX.getGridEXRow().getInnerRow(); 
			}
		}
	}	
	function FindItemWithText(text)
	{
		var item = gridEX.getRootTable().FindRowWithDisplay(text); 
		if(item != null && item.length == 2)
			return item[1]; 		
		else
			return null; 						
	}	
	function HideGridEX()
	{
		if(gridEX.getGridEXRow() != null)
			gridEX.getGridEXRow().HideHeaderIndicator();
		hideDropDownBackFrame();
		innerGridEX.style.visibility = "hidden"; 		
		if(owner != null && owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.src = owner.getCell().getGridEX().ddbimg; 
				img.status = 0; 
			}
		}
	}	
	function HideButton() 
	{
		if(mcc != null && mcc)
			return; 		
		button.style.visibility = "hidden"; 
	}	
	function HideTextBox() 
	{ 
		if(mcc != null && mcc)
			return; 			
		textbox.style.visibility = "hidden"; 
	}	
	function ShowGridEX()	
	{	
		if(initialHeight == -1)
		{
			innerGridEX.style.display = "block"; 
			initialHeight = gridEX.getRootTable().getHtmlItemsTable().offsetParent.offsetHeight;			
		}
		if(mcc != null && mcc)
		{
			left = getPixelLeft(textbox); 
			top = getPixelTop(textbox); 
			currentDropDown = gridEXDropDown; 
		}			
		var scrollHeight = -1;
		var _fixedTop = -1; 
		var innerDropDownTable = null;
		scrollHeight = (document.body.clientHeight == 0 ? document.documentElement.clientHeight : document.body.clientHeight) + document.body.scrollTop; 			
		if(((top + button.offsetHeight + 1) + innerGridEX.offsetHeight > scrollHeight) || ((top + button.offsetHeight + 1) + innerGridEX.offsetHeight < scrollHeight))
		{		
			innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable(); 					
			if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
			{
				innerDropDownTable.offsetParent.style.pixelHeight = initialHeight; 
				innerGridEX.style.pixelHeight =  initialHeight;
				if(gridEX.getRootTable().getHeaders() != null)
				{
					var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
					innerGridEX.style.pixelHeight += gheader.getHtmlHeader().offsetHeight; 
				}
			}
			else
			{								
				var _headerheight = 0; 
				if(gridEX.getRootTable().getHeaders() != null)
				{
					var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
					_headerheight = gheader.getHtmlHeader().offsetHeight;
				}
				var _height = (scrollHeight - (top + button.offsetHeight + 1)) - _headerheight;
				if(_height <= _headerheight || currentItem != null && _height <= currentItem.offsetHeight || (gridEX.getRootTable().getHtmlItemsTable().rows.length > 0 && _height <= gridEX.getRootTable().getHtmlItemsTable().rows[0].offsetHeight))
				{
					if(initialHeight < top)
						_height = initialHeight;
					else
						_height = top - _headerheight;						
					_fixedTop = top - _height;
					innerDropDownTable.offsetParent.style.pixelHeight = _height; 
				}						
				else
					innerDropDownTable.offsetParent.style.pixelHeight =  _height; 
				innerGridEX.style.pixelHeight = _height + _headerheight; 
			}
		}
		else if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
		{				
			innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable();
			if(innerDropDownTable.offsetParent.offsetHeight != initialHeight)
			{
				innerDropDownTable.offsetParent.style.pixelHeight = initialHeight; 
				innerGridEX.style.pixelHeight = initialHeight;
				if(gridEX.getRootTable().getHeaders() != null)
				{
					var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
					innerGridEX.style.pixelHeight += gheader.getHtmlHeader().offsetHeight; 
				}
			}
		}			
		gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop = 0
		if(currentItem != null)
		{
			if((currentItem.offsetTop + currentItem.offsetHeight) >= (gridEX.getRootTable().getHtmlItemsTable().offsetParent.offsetHeight + gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop))
				gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop += currentItem.offsetTop
			else if((currentItem.offsetTop - gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop) < 0)
				gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop += currentItem.offsetTop;
		}		
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			innerGridEX.style.pixelWidth = width;
		if(innerGridEX.offsetWidth + left > document.body.clientWidth)
		{
			left -= (innerGridEX.offsetWidth + left) - document.body.clientWidth;
			left += document.body.scrollLeft; 
			if(left < 0)
			{
				left = 0;
				innerGridEX.style.pixelWidth = document.body.clientWidth; 
			}
		}
		innerGridEX.style.pixelLeft = left; 
		if(_fixedTop != -1)
			innerGridEX.style.pixelTop = _fixedTop; 
		else
			innerGridEX.style.pixelTop = top + (mcc ? button.clientHeight : button.offsetHeight + 1);		
		showDropDownBackFrame(innerGridEX.style.pixelLeft, innerGridEX.style.pixelTop, innerGridEX.offsetWidth, innerGridEX.offsetHeight);
		innerGridEX.style.visibility = "visible";
		if(innerGridEX.setActive != null)
			innerGridEX.setActive();
		else if(innerGridEX.focus != null)
			innerGridEX.focus(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}
	function Hide()
	{
		HideGridEX();
		HideButton(); 
		HideTextBox(); 
	}	
	function Show()
	{	
		currentDropDown = gridEXDropDown; 	
		if(!browser.isIE)
			button.style.display = ""; 
		button.style.zIndex = 1005; 
		button.style.visibility = "visible"; 
		button.style.pixelTop = top; 	
		button.style.pixelHeight = height; 		
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			button.style.pixelLeft = left; 
		else
			button.style.pixelLeft = (left + width) - button.offsetWidth; 				
		textbox.style.zIndex = 1004;
		textbox.style.visibility = "visible";
		textbox.style.pixelTop = top; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			textbox.style.pixelLeft = left + button.offsetWidth;
		else
			textbox.style.pixelLeft = left; 
		textbox.style.pixelHeight = height; 
		textbox.style.pixelWidth = width - button.offsetWidth;
		try
		{
		textbox.focus(); 	
		if(browser.isIE)	
			textbox.select(); 
		} catch(err) {   } 
	}
	function Unload()
	{		
		if(!browser.isIE)
			return;				
		delete button;
		button = null;		
		delete textbox;
		textbox = null;		
		gridEX.gridEX_onunload();
		delete gridEX;
		gridEX = null;
		delete innerGridEX;
		innerGridEX = null;
		delete gridEXDropDown;
		gridEXDropDown = null;
	}
	function ItemOnMouseOver(item, itemCss, itemSelCss)
	{
		if(currentItemID != null && currentItem != null && currentItem != item)
		{		
			var css = gridEX.getClassName(currentItemID); 
			if(css != null)
				currentItem.className = css; 
			else
			{
				css = gridEX.getRootTable().getRowCss(0); 
				currentItem.className = css; 
			}			
			currentItem = null; 
			currentItemID = null; 
		}		
		itemOver = item; 
		item.className = (item.className == itemSelCss) ? itemCss : itemSelCss;
		gridEX.setCurrentRow(gridEX.RetrieveRow(getRootRowFromInner(item), item, null), false); 
	}	
	function ItemOnMouseOut(item, itemCss, itemSelCss)
	{
		if(currentItemID != null && currentItem != null && currentItem != item)
		{
			var css = gridEX.getClassName(currentItemID); 
			if(css != null)
				currentItem.className = css;
			else
			{
				css = gridEX.getRootTable().getRowCss(0); 
				currentItem.className = css; 
			}			
			currentItem = null; 
			currentItemID = null; 
		}
		if(gridEX.getGridEXRow() != null && getRootRowFromInner(gridEX.getGridEXRow().getInnerRow()) == item)
			item.className = (item.className == itemCss) ? itemSelCss : itemCss; 				
		itemOver = null;
	}	
	function ItemOnClick(item, argument)
	{			
		gridEX.setCurrentRow(gridEX.RetrieveRow(getRootRowFromInner(item), item, null), true);
		commitNewValue(item);
		HideGridEX(); 		
		if(textbox.value != itemDisplay)
			textbox.value = itemDisplay;
		textbox.focus(); 
		textbox.select();
		if(argument != null)
		{
			if(owner.getCell != null)				
				owner.getCell().getGridEX().DoPostBack(argument[0], argument[1]);
		}
	}
	function ItemOnExpand(rowID, action)
	{
		var tr = document.getElementById(rowID); 
		if(tr == null)
			throw Error("unable to find HTML TR for row id '" + rowID + "'"); 
			
		var row = gridEX.RetrieveRow(tr, null, null); 
		gridEX.setCurrentRow(row, true); 
		if(action == 1)
			row.Expanding(); 
		else if(action == 0)
			row.Collapsing(); 			
	}
	function button_onblur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 			
		if(document.activeElement == innerGridEX || innerGridEX.contains(document.activeElement))
			return true;								
		Hide(); 
		return true; 
	}	
	function ShowGridEXCore()
	{
		var show = null;
		if(owner != null && owner.DropDown != null)
			show = owner.DropDown();
				
		if(owner != null && owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbpimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.status = 1; 
				img.src = owner.getCell().getGridEX().ddbpimg; 					
			}
		}
		if(show != null && !show)
			return;
				
		updateDropDown(); 				
		ShowGridEX();
	}
	function button_onmousedown()
	{
		if(this.readonly)
			return;
		if(innerGridEX.style.visibility != "visible")
			ShowGridEXCore();				
		else
		{
			if(owner != null && owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.status = 0;
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}		
			HideGridEX();	
			textbox.focus(); 
			textbox.select(); 
		}		
	}	
	function textbox_onblur()
	{
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 			
		if(document.activeElement == innerGridEX || innerGridEX.contains(document.activeElement))
			return true; 			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 			
		Hide(); 
		return true; 
	}	
	function textbox_onchange() { commitNewText(); }	
	function textbox_onkeydown()
	{
		if(window.event.keyCode == 13)
		{			
			commitNewText(); 		
			window.event.returnValue = false;  
			window.event.cancelBubble = true; 						
		}		
		else if(window.event.keyCode == 27)
		{
			itemValue = itemDisplay = null;
			textbox.value = "";
		}
		else if(window.event.keyCode == 9 || window.event.keyCode == 38 || (window.event.keyCode == 40 && !window.event.altKey))
			commitNewText();
		else if(window.event.keyCode == 40 && window.event.altKey)
		{			
			ShowGridEXCore();
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false; 
		}					
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
		
		return true; 
	}	
	function textbox_onkeypress()
	{
		if(!browser.isIE)
			return; 			
		var c = null;
		var C = null;		
		var display = null; 
		var newChar = null;		
		var indexOf = -1; 
		var range = null;
		var selectedRange = null; 		
		var textDisplay = null; 
		var textToFind = null; 
		var textRemain = null; 		
		var value = null; 		
		if(getCharacterCasing() == 3)
		{
			c = window.event.keyCode;
			newChar = String.fromCharCode(c).toUpperCase(); 
			C = newChar.charCodeAt(); 
			window.event.keyCode = C; 						
		}
		else if(getCharacterCasing() == 1)
		{
			C = window.event.keyCode;
			newChar = String.fromCharCode(C).toLowerCase(); 
			c = newChar.charCodeAt();
			window.event.keyCode = c;			
		}
		else
			newChar = String.fromCharCode(window.event.keyCode); 
		
		textToFind = textbox.value; 
		textDisplay = textbox.value;		
		if(document.activeElement == textbox && document.selection != null && document.selection.type == "Text")
		{
			range = textbox.createTextRange();
			selectedRange = document.selection.createRange();
			if(selectedRange.compareEndPoints("EndToEnd", range) == 0 && selectedRange.compareEndPoints("StartToStart", range) != 0)
			{
				indexOf = textDisplay.lastIndexOf(selectedRange.text);
				if(indexOf > 0)
				{
					textToFind = textDisplay.substr(0, indexOf);
					textToFind += newChar; 
				}
			}
			else if(selectedRange.compareEndPoints("EndToEnd", range) == 0 && selectedRange.compareEndPoints("StartToStart", range) == 0)
				textToFind = newChar; 
			else
				textToFind += newChar; 
		}
		else
			textToFind += newChar; 				
		var item = FindItemWithText(textToFind);
		if(item != null)
		{
			display = getDisplayFromItem(item);											
			itemDisplay = display;			
			updateTextBox();			
			textRemain = display.substr(textToFind.length); 				
			selectedRange = textbox.createTextRange();
			selectedRange.moveStart("character", textToFind.length); 
			selectedRange.moveEnd("character", textRemain.lenght); 
			selectedRange.select();
			value = getValueFromItem(item); 			
			if(value != getValue())
			{
				itemValue = value; 
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
				else if(mcc != null && mcc == true)
					getObjectFromID(mccid).onValueChanged(); 
					
			}			
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false;  
		}
		else
		{
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(-1);
			
			resetTextBoxImage(); 
		}			
		return true; 		
	}
	function textbox_onkeyup()
	{
		if(window.event.altKey && window.event.keyCode == 9)
			commitNewText();
			
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}
	function GridEX_OnBlur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
		{
			HideGridEX();
			return true; 
		}		
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
		{
			HideGridEX();
			return true; 
		}		
		if(innerGridEX.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
			
		Hide();
		return true; 
	}	
	function GridEX_OnKeyDown()
	{
		var c = true;
		if(itemOver != null)
		{
			unselectDropDownItem(itemOver.getAttribute("id"), itemOver); 
			itemOver = null; 
		}
		if(window.event.keyCode == 13)
		{
			if(gridEX.getGridEXRow() != null)
				commitNewValue(getRootRowFromInner(gridEX.getGridEXRow().getInnerRow()));
				
			HideGridEX(); 
			textbox.focus();
			textbox.select(); 
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			c = false; 
		}
		else if(window.event.keyCode == 27)
		{
			HideGridEX(); 
			textbox.focus();
			textbox.select(); 
			c = false; 
		}			
		return c; 
	}		
	var cbo = this;
	var eid = "e"+id;
	eval(eid+"=null;");
	eval(eid+"=cbo;");	
	browser.handleEvent(button, "blur", (browser.isIE ? gcddbutton_onblur : button_onblur)); 
	browser.handleEvent(button, "mousedown", (browser.isIE ? gcddbutton_onmousedown : button_onmousedown)); 		
	button.setAttribute("combo", id);
	browser.handleEvent(textbox, "blur", (browser.isIE ? gcddtextbox_onblur : textbox_onblur)); 
	browser.handleEvent(textbox, "change", (browser.isIE ? gcddtextbox_onchange : textbox_onchange)); 
	browser.handleEvent(textbox, "keypress", (browser.isIE ? gcddtextbox_onkeypress : textbox_onkeypress)); 
	browser.handleEvent(textbox, "keydown", (browser.isIE ? gcddtextbox_onkeydown : textbox_onkeydown)); 
	browser.handleEvent(textbox, "keyup", (browser.isIE ? gcddtextbox_onkeyup : textbox_onkeyup)); 	
	textbox.setAttribute("combo", id);
	gridEX.setOwner(this); 	
	var gridEXDropDown = this; 	
	return this; 
}
function gddbutton_onblur()
{
	var element = window.event.srcElement;
	while(element != null && element.getAttribute("combo") == null)
		element = element.parentElement;
	if(element != null)	
		try { retrieveEditor("e"+element.getAttribute("combo")).button_onblur(); } catch(e) { }
}
function gddbutton_onkeydown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement; 
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).button_onkeydown();
}
function gddbutton_onmousedown()
{
	var element = window.event.srcElement;
	while(element != null && element.getAttribute("combo") == null)
		element = element.parentElement; 
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).button_onmousedown();
}
function GridEXDropDown(id, mcc, mccid)
{
	var id = id;
	var _clientevents = null;
	try {  _clientevents = eval(id + "_client_events"); } catch(E) { _clientevents = null; }	
	var gridEX = new GridEX(id, eval(id + "_client"), eval(id + "_client_tables"), eval(id + "_client_selected_items"), eval(id + "_client_rows_css"), eval(id + "_client_hidden_values"), _clientevents);
	eval(id + " = gridEX;");
	if(mcc == null || !mcc)
		gridEX.Initialize(); 	
	else
		gridEX.gridEX_onload(); 
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXDropDown"); 
	if(mcc == null || !mcc)
	{		
		if(button.parentElement != null && button.parentElement.tagName != "BODY")
			document.body.appendChild(button); 
	}
	var textbox = null;
	if(mcc != null && mcc)
	{
		textbox = document.getElementById(id + "_textbox"); 	
		if(textbox == null)
			throw Error("unable to find textbox for GridEXComboDropDown '" + id + "'"); 
	}		
	var innerGridEX = gridEX.getHtmlGridEX();
	if(innerGridEX.parentElement != null && innerGridEX.parentElement.tagName != "BODY")
		document.body.appendChild(innerGridEX); 		
	var owner = null; 	
	var currentItem = null; 
	var currentItemID = null; 
	var itemOver = null; 	
	var left = -1;
	var top = -1;
	var width = -1;
	var height = -1; 	
	var initialHeight = -1; 
	var itemValue = null; 
	var itemDisplay = null; 		
	var image = ""; 
	var readonly = false; 
	this.image = image;
	this.readonly = readonly;
	this.getID = getID; 	
	this.getDisplay = getDisplay; 
	this.getDisplayByValue = getDisplayByValue;
	this.getValue = getValue; 	
	this.setLeft = setLeft; 
	this.setTop = setTop; 
	this.setWidth = setWidth; 
	this.setHeight = setHeight; 
	this.setOwner = setOwner; 
	this.setValue = setValue; 	
	this.GridEX_OnBlur = GridEX_OnBlur; 
	this.GridEX_OnKeyDown = GridEX_OnKeyDown; 	
	this.ItemOnMouseOver = ItemOnMouseOver; 
	this.ItemOnMouseOut = ItemOnMouseOut; 
	this.ItemOnClick = ItemOnClick;
	this.ItemOnExpand = ItemOnExpand; 	
	this.Hide = Hide;
	this.Show = Show; 	
	this.Unload = Unload; 
	this.button_onblur = button_onblur;
	this.button_onkeydown = button_onkeydown;
	this.button_onmousedown = button_onmousedown;
	function getID() { return id; }			
	function getDisplay() { return itemDisplay; }	
	function getValue() { return itemValue; }	
	function setLeft(value) { left = value; }	
	function setHeight(value) { height = value; }	
	function setValue(value)
	{
		itemValue = value; 				
		var item = gridEX.getRootTable().FindRowByValue(value);
		if(item != null && item.length == 2)
			itemDisplay = getDisplayFromItem(item[1]);						
	}
	function getDisplayByValue(value)
	{
		var item = gridEX.getRootTable().FindRowByValue(value); 
		if(item != null && item.length == 2)
			return getDisplayFromItem(item[1]);
		return ""; 
	}
	function setWidth(value) { width = value; }	
	function setTop(value) { top = value; }	
	function setOwner(value) { owner = value; }				
	function getItemFromInnerRow(element)
	{
		while(element != null)
		{
			if(element.nodeType == 1 && element.tagName == "TR" && element.id != null && element.getAttribute("t") != null)
				return element; 		
			element = element.parentElement; 
		}			
		if(element == null)
			throw Error("unable to find item"); 
	}	
	function updateDropDown()		
	{			
		if(!browser.isIE)
		{
			if(innerGridEX.style.display == "none")
				innerGridEX.style.display = "block"; 
		}
		if((mcc == null || !mcc) && gridEX.getSelectedItems() != null && gridEX.getSelectedItems().Count() > 0)
			gridEX.getSelectedItems().Clear(); 			
		var item = gridEX.getRootTable().FindRowByValue(itemValue);
		if(item != null && item.length == 2)
		{
			if(currentItemID != null && currentItem != null)
				unselectDropDownItem(currentItemID, currentItem); 				
			if((item[1].offsetTop + item[1].offsetHeight) >= (gridEX.getRootTable().getHtmlItemsTable().offsetParent.offsetHeight + gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop))
				gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop += item[1].offsetTop
			else if((item[1].offsetTop - gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop) < 0)
				gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop += item[1].offsetTop;
			selectDropDownItem(item[0], item[1]);			
			currentItemID = item[0]; 
			currentItem = item[1]; 
		}
		else
		{
			unselectDropDownItem(currentItemID, currentItem); 
			currentItemID = null; 
			currentItem = null; 
		}
		if(itemValue != null && currentItem != null)			
			gridEX.setCurrentRow(gridEX.RetrieveRow(getRootRowFromInner(currentItem), currentItem, null), false); // true
		else
		{
			if(gridEX.getGridEXRow() != null)
			{
				currentItemID = getRootRowFromInner(gridEX.getGridEXRow().getInnerRow()); 
				currentItem = gridEX.getGridEXRow().getInnerRow(); 
			}
		}		
	}	
	function unselectDropDownItem(itemID, item)
	{		
		if(itemID == null && item == null)
			return; 	
		var css = gridEX.getClassName(itemID); 
		if(css != null)
			item.className = css; 
		else
		{
			css = gridEX.getRootTable().getRowCss(0); 
			item.className = css; 
		}
	}	
	function selectDropDownItem(itemID, item)
	{
		if(itemID == null && item == null)
			return; 	
		var css = gridEX.getSelectedClassName(itemID); 
		if(css != null)
			item.className = css;
		else
		{
			css = gridEX.getRootTable().getRowCss(1); 
			item.className = css; 
		}
	}	
	function getValueFromItem(item) { return item.getAttribute("value"); }	
	function getDisplayFromItem(item) { return item.getAttribute("displayMember"); }	
	function commitNewValue(item)
	{			
		var value = getValueFromItem(item); 
		if(value != getValue())
		{	
			itemDisplay = getDisplayFromItem(item); 	
			itemValue = value;						
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
			
			try
			{
				textbox.value = itemDisplay;
				textbox.select(); 	
				if(gridEXDropDown.image != "")
				{			
					var row = gridEX.getGridEXRow(); 
					var img = row.getCellByColumnKey(gridEXDropDown.image).getInnerCell().getElementsByTagName("IMG")[0]; 
					textbox.style.backgroundImage = "url('" + img.src + "')"; 
					textbox.style.backgroundPosition = "center left"; 
					textbox.style.backgroundRepeat = "no-repeat"; 
					textbox.style.paddingLeft = (img.offsetWidth + 2) + "px";										
				}
			}
			catch(err)
			{ } 				
		}
		if(mcc != null && mcc)
		{
			document.getElementsByName(mccid+"_value")[0].value = itemValue; 
			var obj = getObjectFromID(mccid);
			obj.onValueChanged();
			if(obj.autoPostBack)
				__doPostBack(obj.serverID, "ValueChanged"); 
			
		}
	}	
	function HideButton() 	
	{ 
		if(mcc != null && mcc)
			return; 
			
		button.style.visibility = "hidden"; 
	}	
	function HideGridEX()
	{
		if(gridEX.getGridEXRow() != null)
			gridEX.getGridEXRow().HideHeaderIndicator(); 
		hideDropDownBackFrame();
		innerGridEX.style.visibility = "hidden";		
		if(owner != null && owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.src = owner.getCell().getGridEX().ddbimg; 
				img.status = 0; 
			}
		}		
	}		
	function ShowGridEX()
	{		
		if(initialHeight == -1)
		{
			innerGridEX.style.display = "block"; 
			initialHeight = gridEX.getRootTable().getHtmlItemsTable().offsetParent.offsetHeight; 
		}
		if(mcc)
		{
			left = getPixelLeft(textbox); 
			top = getPixelTop(textbox); 
			currentDropDown = gridEXDropDown; 
		}		
		var scrollHeight = -1;				
		var _fixedTop = -1; 
		var innerDropDownTable = null; 
		scrollHeight = (document.body.clientHeight == 0 ? document.documentElement.clientHeight : document.body.clientHeight) + document.body.scrollTop;
		if(((top + button.offsetHeight + 1) + innerGridEX.offsetHeight > scrollHeight) || ((top + button.offsetHeight + 1) + innerGridEX.offsetHeight < scrollHeight))
		{		
			innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable(); 					
			if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
			{
				innerDropDownTable.offsetParent.style.pixelHeight = initialHeight; 
				innerGridEX.style.pixelHeight = initialHeight;
				if(gridEX.getRootTable().getHeaders() != null)
				{
					var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders(); 
					innerGridEX.style.pixelHeight += gheader.getHtmlHeader().offsetHeight;
				}
			}
			else
			{						
				var _headerheight = 0; 
				if(gridEX.getRootTable().getHeaders() != null)
				{	
					var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders(); 
					_headerheight = gheader.getHtmlHeader().offsetHeight;
				}
				var _height = (scrollHeight - (top + button.offsetHeight + 1)) - _headerheight;	
				if(_height <= _headerheight || currentItem != null && _height <= currentItem.offsetHeight || (gridEX.getRootTable().getHtmlItemsTable().rows.length > 0 && _height <= gridEX.getRootTable().getHtmlItemsTable().rows[0].offsetHeight))
				{						
					if(initialHeight < top)
						_height = initialHeight;
					else
						_height = top - _headerheight;
					_fixedTop = (top - _height);						
					innerDropDownTable.offsetParent.style.pixelHeight = _height;
				}
				else
					innerDropDownTable.offsetParent.style.pixelHeight = _height; 
				innerGridEX.style.pixelHeight = _height + _headerheight; 
			}
		}
		else if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
		{				
			innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable();
			if(innerDropDownTable.offsetParent.offsetHeight != initialHeight)
			{
				innerDropDownTable.offsetParent.style.pixelHeight = initialHeight; 
				innerGridEX.style.pixelHeight = initialHeight;
				if(gridEX.getRootTable().getHeaders() != null)
				{						
					var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders(); 
					innerGridEX.style.pixelHeight += gheader.getHtmlHeader().offsetHeight; 
				}
			}
		}
		gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop = 0
		if(currentItem != null)
		{
			if((currentItem.offsetTop + currentItem.offsetHeight) >= (gridEX.getRootTable().getHtmlItemsTable().offsetParent.offsetHeight + gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop))
				gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop += currentItem.offsetTop;
			else if((currentItem.offsetTop - gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop) < 0)
				gridEX.getRootTable().getHtmlItemsTable().offsetParent.scrollTop += currentItem.offsetTop;						
		}		
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			innerGridEX.style.pixelWidth = width; 
		if(innerGridEX.offsetWidth + left > document.body.clientWidth)
		{
			left -= (innerGridEX.offsetWidth + left) - document.body.clientWidth;
			left += document.body.scrollLeft; 
			if(left < 0)
			{
				left = 0; 
				innerGridEX.style.pixelWidth = document.body.clientWidth;
			}
		}
		innerGridEX.style.pixelLeft = left;
		if(_fixedTop != -1)
			innerGridEX.style.pixelTop = _fixedTop;
		else
			innerGridEX.style.pixelTop = top + button.offsetHeight + 1;
		innerGridEX.style.visibility = "visible"; 
		showDropDownBackFrame(innerGridEX.style.pixelLeft, innerGridEX.style.pixelTop, innerGridEX.offsetWidth, innerGridEX.offsetHeight);
		if(innerGridEX.setActive != null)
			innerGridEX.setActive();
		else if(innerGridEX.focus != null)
			innerGridEX.focus();
		window.event.returnValue = false;
		window.event.cancelBubble = true;
	}
	function Unload()
	{	
		if(!browser.isIE)
			return; 		
		delete button;
		button = null;
		gridEX.gridEX_onunload();
		delete gridEX;
		gridEX = null;
		delete innerGridEX;
		innerGridEX = null; 
		delete gridEXDropDown;
		gridEXDropDown = null;
	}
	function ItemOnMouseOver(item, itemCss, itemSelCss)
	{		
		if(currentItemID != null && currentItem != null && currentItem != item)
		{		
			var css = gridEX.getClassName(currentItemID); 
			if(css != null)
				currentItem.className = css; 
			else
			{
				css = gridEX.getRootTable().getRowCss(0); 
				currentItem.className = css; 
			}			
			currentItem = null; 
			currentItemID = null; 
		}
		itemOver = item; 
		item.className = (item.className == itemSelCss) ? itemCss : itemSelCss;
		gridEX.setCurrentRow(gridEX.RetrieveRow(getRootRowFromInner(item), item, null), false);
	}	
	function ItemOnMouseOut(item, itemCss, itemSelCss)
	{		
		if(currentItemID != null && currentItem != null && currentItem != item)
		{
			var css = gridEX.getClassName(currentItemID); 
			if(css != null)
				currentItem.className = css;
			else
			{
				css = gridEX.getRootTable().getRowCss(0); 
				currentItem.className = css; 
			}			
			currentItem = null; 
			currentItemID = null; 
		}		
		if(gridEX.getGridEXRow() != null && getRootRowFromInner(gridEX.getGridEXRow().getInnerRow()) == item)			
			item.className = (item.className == itemCss) ? itemSelCss : itemCss; 		
		itemOver = null; 
	}	
	function ItemOnClick(item, argument)
	{		
		gridEX.setCurrentRow(gridEX.RetrieveRow(getRootRowFromInner(item), item, null), true); 				
		commitNewValue(item); 		
		HideGridEX(); 
		button.focus(); 
		if(argument != null)
		{
			if(owner != null && owner.getCell != null)
				owner.getCell().getGridEX().DoPostBack(argument[0], argument[1]); 
		}
	}
	function ItemOnExpand(rowID, action)
	{
		var tr = document.getElementById(rowID); 
		if(tr == null)
			throw Error("unable to find HTML TR for row id '" + rowID + "'"); 
			
		var row = gridEX.RetrieveRow(tr, null, null, null); 
		gridEX.setCurrentRow(row, true); 
		if(action == 1)
			row.Expanding(); 
		else if(action == 0)
			row.Collapsing(); 			
	}		
	function Hide()
	{
		HideGridEX(); 
		if(mcc == null || !mcc)
			HideButton(); 		
	}	
	function Show()
	{		
		if(!browser.isIE)
			button.style.display = "";	
		button.style.pixelTop = top; 
		button.style.pixelHeight = height; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			button.style.pixelLeft = left; 
		else
			button.style.pixelLeft = (left + width) - button.offsetWidth;		
		button.style.visibility = "visible"; 	
		button.focus(); 		
		currentDropDown = gridEXDropDown; 
	}
	function button_onblur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
		if(document.activeElement == innerGridEX || innerGridEX.contains(document.activeElement))
			return true;			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
				
		Hide();
		return true; 
	}	
	function button_onkeydown()
	{	
		if(owner != null && owner.KeyDown != null)
		{
			if(window.event.keyCode == 40 && window.event.altKey)
			{
				ShowGridEXCore();
				window.event.returnValue = false;
				window.event.cancelBubble = true;
				return false; 
			}
			owner.KeyDown(); 
			if(window.event.keyCode == 13 || window.event.keyCode == 33 || window.event.keyCode == 34)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}
		}			
		return true; 
	}	
	function ShowGridEXCore()
	{
		var show = null;
		if(owner != null && owner.DropDown != null)
			show = owner.DropDown();				
		if(owner != null && owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbpimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.status = 1; 
				img.src = owner.getCell().getGridEX().ddbpimg; 
			}
		}
		if(show != null && !show)
			return; 				
		updateDropDown();				
		ShowGridEX(); 
	}
	function button_onmousedown()
	{		
		if(this.readonly)
			return;
		if(innerGridEX.style.visibility != "visible")
			ShowGridEXCore(); 
		else
		{
			if(owner != null && owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.status = 0;
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}	
			HideGridEX();
		}
	}	
	function GridEX_OnBlur()
	{
		if(document.activeElement == button || button.contains(document.activeElement))
			return true;							
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 		
		Hide();
		return true; 
	}	
	function GridEX_OnKeyDown()
	{		
		var c = true;
		if(itemOver != null)
		{
			unselectDropDownItem(itemOver.getAttribute("id"), itemOver); 
			itemOver = null; 
		}			
		if(window.event.keyCode == 13)
		{
			if(gridEX.getGridEXRow() != null)
				commitNewValue(getRootRowFromInner(gridEX.getGridEXRow().getInnerRow()));

			if(textbox != null && textbox.value != itemDisplay)
				textbox.value = itemDisplay; 
				
			HideGridEX(); 
			button.focus(); 
			window.event.returnValue = false;
			window.event.cancelBubble = true; 			
			c = false; 
		}
		else if(window.event.keyCode == 27)
		{
			HideGridEX(); 
			try { button.focus(); } catch(err) { } 
			c = false; 
		}				
		return c; 
	}
	var btn = this;
	var eid = "e"+id;
	eval(eid+"=null;");
	eval(eid+"=btn;");	
	browser.handleEvent(button, "blur", (browser.isIE ? gddbutton_onblur : button_onblur)); 
	browser.handleEvent(button, "keydown", (browser.isIE ? gddbutton_onkeydown : button_onkeydown)); 
	browser.handleEvent(button, "mousedown", (browser.isIE ? gddbutton_onmousedown : button_onmousedown));
	button.setAttribute("combo", id);
	gridEX.setOwner(this); 	
	var gridEXDropDown = this; 	
	return this; 
}
function vlitem_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TR")
		element = element.parentElement;		
	if(element != null)
		retrieveEditor("e"+element.getAttribute("valuelist")).item_onclick();
}
function vlitem_onmouseover()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TR")
		element = element.parentElement;		
	if(element != null)
		retrieveEditor("e"+element.getAttribute("valuelist")).item_onmouseover();
}
function vlitem_onmouseout()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TR")
		element = element.parentElement;
	if(element != null)
		retrieveEditor("e"+element.getAttribute("valuelist")).item_onmouseout();
}
function vlbutton_onblur()
{	
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		retrieveEditor("e"+element.getAttribute("valuelist")).button_onblur();
}
function vlbutton_onkeydown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		retrieveEditor("e"+element.getAttribute("valuelist")).button_onkeydown();
}
function vlbutton_onmousedown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		retrieveEditor("e"+element.getAttribute("valuelist")).button_onmousedown();
}
function vllist_onblur(id)
{
	var element = document.getElementById(id);
	retrieveEditor("e"+element.getAttribute("valuelist")).list_onblur();
}
function vllist_onkeydown(id)
{
	var element = document.getElementById(id);
	retrieveEditor("e"+element.getAttribute("valuelist")).list_onkeydown();
}
function GridEXValueList(id)
{
	var id = id;
	var list = document.getElementById(id); 
	if(list == null)
		throw Error("unable to find list for ValueList"); 		
	if(list.parentElement != null && list.parentElement.tagName != "BODY")
		document.body.appendChild(list); 		
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find button for ValueList");		
	if(button.parentElement != null && button.parentElement.tagName != "BODY")
		document.body.appendChild(button); 		
	var itemCSS = null; 
	var selectedItemCSS = null; 	
	var currentItem = null; 		
	var itemDisplay = null;
	var itemImage = null; 
	var itemValue = null;	
	var itemOver = null; 	
	var owner = null; 	
	var left = -1;
	var top = -1; 
	var height = -1; 
	var width = -1; 	
	var listTable = null; 
	var initialHeight = -1;
	var itemsLength = -1; 
	var compareTarget = 1; 	
	this.ddpb = (list.getAttribute("ddpb") == "1"); 
	this.getDisplay = getDisplay; 
	this.getDisplayByValue = getDisplayByValue; 
	this.getID = getID; 
	this.getImage = getImage; 
	this.getInnerList = getInnerList;
	this.getItemCSS = getItemCSS; 
	this.getSelectedItemCSS = getSelectedItemCSS; 
	this.getValue = getValue; 	
	this.setCompareTarget = setCompareTarget; 
	this.setLeft = setLeft;
	this.setOwner = setOwner; 
	this.setTop = setTop; 
	this.setWidth = setWidth;
	this.setHeight = setHeight; 
	this.setItemCSS = setItemCSS; 
	this.setSelectedItemCSS = setSelectedItemCSS; 
	this.setValue = setValue; 	
	this.Focus = Focus; 
	this.Hide = Hide; 
	this.Show = Show; 	
	this.ShowList = ShowList; 
	this.Unload = Unload; 
	this.item_onclick = item_onclick;
	this.item_onmouseover = item_onmouseover;
	this.item_onmouseout = item_onmouseout;
	this.button_onblur = button_onblur;
	this.button_onkeydown = button_onkeydown;
	this.button_onmousedown = button_onmousedown;
	this.list_onblur = list_onblur;
	this.list_onkeydown = list_onkeydown;
	function getDisplay() { return itemDisplay; }	
	function getID() { return id; }	
	function getImage() { return itemImage; }
	function getInnerList() { return list; }
	function getItemCSS() { return itemCSS; }	
	function getSelectedItemCSS() { return selectedItemCSS; }	
	function getValue() { return itemValue; }
	function setCompareTarget(target) { compareTarget = target;  }	
	function setLeft(value) { left = value; }	
	function setOwner(value) { owner = value; }	
	function setTop(value) { top = value; }	
	function setHeight(value) { height = value; }	
	function setWidth(value) { width = value; }	
	function setItemCSS(value){ itemCSS = value; }	
	function setSelectedItemCSS(value) { selectedItemCSS = value;  }	
	function setValue(value)
	{
		itemValue = value;		
		var _item = getItemByValue(itemValue); 				
		if(currentItem != null && currentItem != _item)
			currentItem.className = getItemCSS();			
		if(_item != null)
		{
			_item.className = getSelectedItemCSS(); 			
			currentItem = _item; 
		}			
		itemDisplay = getDisplayFromItem(_item); 
		itemImage = getImageFromItem(_item); 
	}	
	function Focus() { button.focus(); } 	
	function Hide()
	{
		HideList(); 
		HideButton(); 
	}	
	function Show()
	{				
		if(!browser.isIE)
			button.style.display = ""; 		
		button.style.pixelTop = top; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			button.style.pixelLeft = left; 
		else
			button.style.pixelLeft = (left + width) - (button.offsetWidth == 0 ? 16 : button.offsetWidth); 
		button.style.visibility = "visible"; 
		button.style.pixelHeight = height;		
	}	
	function ShowValueList()
	{	
		if(initialHeight == -1)
		{			
			if(!browser.isIE)
				list.getElementsByTagName("TABLE")[0].style.height = "";
			list.getElementsByTagName("TABLE")[0].style.display = "block"; 			
			initialHeight = list.offsetHeight;
			if(!browser.isIE)
			{
				list.getElementsByTagName("TABLE")[0].style.height = "100%"; 
				list.getElementsByTagName("TABLE")[0].style.width = "100%"; 
			}
		}			
		var _fixedTop = -1; 
		var listHeight = -1;
		var scrollHeight = -1;		
		if(owner != null)
		{
			var _heightProposed = -1;
			var ownerTable = owner.getCell().getGridEX().getRootTable().getHtmlItemsTable();
			scrollHeight = (document.body.clientHeight == 0 ? document.documentElement.clientHeight : document.body.clientHeight) + document.body.scrollTop;
			list.getElementsByTagName("TABLE")[0].style.display = "block"; 
			if(list.offsetHeight != 0)
				listHeight = list.offsetHeight;
			else
				listHeight = list.style.pixelHeight; 				
			if(((top + height + 2) + listHeight > scrollHeight) || ((top + height + 2) + listHeight < scrollHeight))
			{						
				if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
					list.style.pixelHeight = initialHeight;
				else
				{
					if(scrollHeight - (top + height + 2) <= 0 || (currentItem != null && scrollHeight - (top + height + 2) <= currentItem.offsetHeight))
					{	
						if(initialHeight < top)						
							_heightProposed = initialHeight;													
						else
							_heightProposed = top - height; 
						_fixedTop = top - _heightProposed;
					}
					else
						_heightProposed = scrollHeight - (top + height +  2);						
					
					list.style.pixelHeight = _heightProposed; 
				}
			}
			else if(scrollHeight - (top + height + 2) >= initialHeight)
			{								
				if(list.offsetHeight != initialHeight)
					list.style.pixelHeight = initialHeight; 
			}
		}		
		list.scrollTop = 0; 
		if(currentItem != null)
		{			
			if((currentItem.offsetTop + currentItem.offsetHeight) >= list.offsetHeight)
				list.scrollTop += currentItem.offsetTop;
			else if((currentItem.offsetTop - list.scrollTop) < 0)
				list.scrollTop += currentItem.offsetTop; 				
		}		
		if(itemOver != null && itemOver != currentItem)
			itemOver.className = getItemCSS(); 
		if(currentItem != null)
			itemOver = currentItem; 
		if(_fixedTop != -1)			
			list.style.pixelTop = _fixedTop;
		else
			list.style.pixelTop = top + height + 2;			
		list.style.pixelWidth = width;		
		if(list.offsetWidth + left > document.body.clientWidth)
		{
			left -= (list.offsetWidth + left) - document.body.clientWidth; 
			left += document.body.scrollLeft;
			if(left < 0)
			{
				left = 0; 
				list.style.pixelWidth = document.body.clientWidth; 
			}
		}
		list.style.pixelLeft = left; 
		list.style.visibility = "visible";		
		showDropDownBackFrame(list.style.pixelLeft, list.style.pixelTop, width, list.offsetHeight); 
		if(list.setActive != null)
			list.setActive();
		else if(list.focus != null)
			list.focus(); 
	}				
	function HideList()
	{	
		hideDropDownBackFrame();
		list.style.visibility = "hidden"; 		
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.src = owner.getCell().getGridEX().ddbimg; 
				img.status = 0; 
			}
		}
	}	
	function HideButton() { button.style.visibility = "hidden"; }
	function Unload()
	{		
		if(!browser.isIE)
			return; 
		var row = null; 	
		for(var i = 0; i < itemsLength; i++)
		{
			row = listTable.rows[i];
			/*
			row.detachEvent("onclick", vlitem_onclick); 
			row.detachEvent("onmouseover", vlitem_onmouseover); 
			row.detachEvent("onmouseout", vlitem_onmouseout);
			row.removeAttribute("valuelist");
			*/
		}	
		/*
		button.detachEvent("onblur", vlbutton_onblur); 
		button.detachEvent("onkeydown", vlbutton_onkeydown); 
		button.detachEvent("onmousedown", vlbutton_onmousedown); 	
		button.removeAttribute("valuelist");	
		*/
		delete button;
		button = null; 
		//list.removeAttribute("valuelist");
		delete list;
		list = null;
		delete listTable;
		listTable = null;
	}	
	function getDisplayByValue(value)
	{
		var item = getItemByValue(value);
		if(item == null)
			return "";
		return getDisplayFromItem(item); 
	}
	function getItemByValue(value)
	{
		var row = null; 
		for(var index = 0; index < itemsLength; index++)
		{
			row = listTable.rows[index];
			if(compareTarget == 1)
			{
				if(row.getAttribute("value") == value)
					return row; 
			}
			else
			{
				if(getDisplayFromItem(row) == value)
					return row; 
			}
		}		
		return null; 
	}	
	function getValueFromItem(item) { return item.getAttribute("value"); }	
	function getDisplayFromItem(item)
	{
		if(item == null)
			return null; 
	
		var cell = item.cells[0]; 
		if(cell.childNodes.length == 2)
		{
			if(cell.childNodes[0].nodeType == 3)
				return trim(cell.childNodes[0].data); 
			if(cell.childNodes[1].nodeType == 3)
				return trim(cell.childNodes[1].data); 
		}		
		else if(cell.childNodes.length == 1)
		{
			if(cell.childNodes[0].nodeType == 3)
				return trim(cell.childNodes[0].data); 
		}		
		return null; 
	}	
	function getImageFromItem(item)
	{
		if(item == null)
			return null; 
	
		var cell = item.cells[0]; 
		if(cell.childNodes.length == 2)
		{
			if(cell.childNodes[0].nodeType == 1 && cell.childNodes[0].tagName == "IMG")
				return cell.childNodes[0].src; 
			if(cell.childNodes[1].nodeType == 1 && cell.childNodes[1].tagName == "IMG")
				return cell.childNodes[1].src;				
		}		
		return null; 
	}		
	function moveUpItem()
	{
		if(itemOver != null && itemOver.rowIndex > 0)
		{
			itemOver.className = getItemCSS();
			itemOver = listTable.rows[itemOver.rowIndex-1]; 
			itemOver.className = getSelectedItemCSS();
		}
	}	
	function moveDownItem()
	{
		if(itemOver != null && itemOver.rowIndex + 1 < itemsLength)
		{
			itemOver.className = getItemCSS(); 
			itemOver = listTable.rows[itemOver.rowIndex+1];
			itemOver.className = getSelectedItemCSS(); 
		}
	}	
	function selectTopItem()
	{
		if(itemOver != null)
			itemOver.className = getItemCSS(); 
	
		var item = listTable.rows[0];
		item.className = getSelectedItemCSS();
		return item;
	}	
	function selectBottomItem()
	{
		if(itemOver != null)
			itemOver.className = getItemCSS(); 
	
		var item = listTable.rows[itemsLength-1]; 
		item.className = getSelectedItemCSS(); 
		return item;
	}
	function item_onmouseover()
	{		
		var item = getListItem(window.event.srcElement);
		if(currentItem != null && item != currentItem)
		{
			currentItem.className = getItemCSS(); 
			currentItem = null; 
		}		
		item.className = getSelectedItemCSS();  
		itemOver = item; 
	}	
	function item_onmouseout()
	{
		var item = getListItem(window.event.srcElement); 
		if(currentItem != null && item != currentItem)
		{
			currentItem.className = getItemCSS(); 
			currentItem = null 
		}		
		item.className = getItemCSS(); 
		if(item == itemOver)
			itemOver = null; 
	}	
	function item_onclick()
	{
		var item = getListItem(window.event.srcElement);				
		var _value = getValueFromItem(item); 
		var _display = getDisplayFromItem(item);
		var _image = getImageFromItem(item); 		
		if(compareTarget != 1)
			_value = _display;
		
		if(_value != getValue())
		{
			itemValue = _value;
			itemDisplay = _display;
			itemImage = _image; 			
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
				
			if(currentItem != item)
			{
				currentItem = item; 
				currentItem.className = getSelectedItemCSS(); 
			}
		}		
		button.focus(); 
		HideList();
	}	
	function button_onblur()
	{
		if(document.activeElement == list || list.contains(document.activeElement))
			return true; 
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 

		Hide();
		return true; 		
	}	
	function button_onkeydown()
	{		
		if(owner != null && owner.KeyDown != null)
		{
			if(window.event.keyCode == 40 && window.event.altKey)
			{
				ShowList(); 
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
				return false;
			}
			owner.KeyDown(); 			
			if(window.event.keyCode == 13 || window.event.keyCode == 33 || window.event.keyCode == 34)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}			
		}
		return true; 
	}	
	function ShowList()
	{
		if(list.style.visibility != "visible")		
		{	
			var show = null;
			if(owner != null && owner.DropDown != null)
				show = owner.DropDown();
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.src = owner.getCell().getGridEX().ddbpimg; 
					img.status = 1; 
				}				
			}
			if(show != null && !show)
				return; 
				
			ShowValueList();
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.src = owner.getCell().getGridEX().ddbimg; 
					img.status = 0; 
				}
			}
			hideDropDownBackFrame();
			list.style.visibility = "hidden"; 
		}
	}
	function button_onmousedown() { ShowList(); }	
	function list_onblur()
	{	
		if(document.activeElement == button || button.contains(document.activeElement))
		{
			HideList(); 
			return true; 
		}
		if(document.activeElement == list || list.contains(document.activeElement))
			return true;

		if(owner != null && owner.Leaving != null)
			owner.Leaving();

		Hide(); 
		return true; 
	}	
	function list_onkeydown()
	{
		if(window.event.keyCode == 13)
		{
			if(itemOver != null)
			{
				var _value = getValueFromItem(itemOver); 
				if(_value != getValue())
				{
					itemValue = _value; 
					itemDisplay = getDisplayFromItem(itemOver); 
					itemImage = getImageFromItem(itemOver); 					
					if(owner != null && owner.ValueChanged != null)
						owner.ValueChanged(); 
				}
			}			
			HideList(); 
			button.focus();			
			window.event.returnValue = false; 
			window.event.cancelBubble = true;
			return false;			
		}
		else if(window.event.keyCode == 27)
		{
			HideList(); 
			button.focus(); 			
		}
		else if(window.event.keyCode == 38) 
		{
			if(itemOver == null)
				itemOver = selectTopItem();
			else
				moveUpItem(); 
		}
		else if(window.event.keyCode == 40) 
		{
			if(itemOver == null)
				itemOver = selectTopItem();
			else
				moveDownItem(); 
		}
		else if(window.event.keyCode == 33) 
			itemOver = selectTopItem(); 
		else if(window.event.keyCode == 34)
		{						
			if(itemOver == null)
				itemOver = selectTopItem(); 
			else
				itemOver = selectBottomItem(); 
		}
		return true; 		
	}		
	listTable = list.getElementsByTagName("TABLE")[0];
	itemsLength = listTable.rows.length; 	
	var lst = this;
	var eid = "e"+id;
	eval(eid+"=null;");
	eval(eid+"=lst;");
	var row = null; 	
	for(var i = 0; i < itemsLength; i++)
	{
		row = listTable.rows[i];
		browser.handleEvent(row,"click",(browser.isIE ? vlitem_onclick : item_onclick)); 
		browser.handleEvent(row, "mouseover", (browser.isIE ? vlitem_onmouseover : item_onmouseover));
		browser.handleEvent(row, "mouseout", (browser.isIE ? vlitem_onmouseout : item_onmouseout)); 		
		row.setAttribute("valuelist", id);		
	}
	browser.handleEvent(button, "blur", (browser.isIE ? vlbutton_onblur : button_onblur));
	browser.handleEvent(button, "keydown", (browser.isIE ? vlbutton_onkeydown : button_onkeydown)); 
	browser.handleEvent(button, "mousedown", (browser.isIE ? vlbutton_onmousedown : button_onmousedown));	
	button.setAttribute("valuelist", id);
	browser.handleEvent(list, "blur", (browser.isIE ? function() { return vllist_onblur(id); }  : list_onblur)); 
	browser.handleEvent(list, "keydown", (browser.isIE ? function() { return vllist_onkeydown(id); } : list_onkeydown));	
	list.setAttribute("valuelist", id);
	return this; 
}
function gcitem_onclick()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TR")
		element = element.parentElement;		
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).item_onclick();
}
function gcitem_onmouseover()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TR")
		element = element.parentElement;		
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).item_onmouseover();		
}
function gcitem_onmouseout()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "TR")
		element = element.parentElement;		
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).item_onmouseout();
}
function gcbutton_onblur()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).button_onblur();
}
function gcbutton_onmousedown()
{
	var element = window.event.srcElement;
	while(element != null && element.tagName != "DIV")
		element = element.parentElement;
	if(element != null)
		retrieveEditor("e"+element.getAttribute("combo")).button_onmousedown();
}
function gctextbox_onblur()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("combo")).textbox_onblur();
}
function gctextbox_onchange()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("combo")).textbox_onchange();
}
function gctextbox_onkeypress()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("combo")).textbox_onkeypress();
}
function gctextbox_onkeyup()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("combo")).textbox_onkeyup(); 
}
function gctextbox_onkeydown()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("combo")).textbox_onkeydown();
}
function gccombo_onblur(id)
{
	var element = document.getElementById(id);
	retrieveEditor("e"+element.getAttribute("combo")).combo_onblur();
}
function gccombo_onkeydown(id)
{
	var element = document.getElementById(id);
	retrieveEditor("e"+element.getAttribute("combo")).combo_onkeydown();
}
function GridEXCombo(id)
{
	var id = id; 
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find HTML element to act as dropdown button"); 		
	if(button.parentElement != null && button.parentElement.tagName != "BODY")
		document.body.appendChild(button); 		
	var textbox = document.getElementById(id + "_textbox"); 
	if(textbox == null)
		throw Error("unable to find HTML element to act as textbox"); 			
	if(textbox.parentElement != null && textbox.parentElement.tagName != "BODY")
		document.body.appendChild(textbox); 
	var combo = document.getElementById(id);
	if(combo == null) 
		throw Error("unable to find HTML element to act as GridEXCombo '" + id + "'"); 			
	if(combo.parentElement != null && combo.parentElement.tagName != "BODY")
		document.body.appendChild(combo); 	
	var autoComplete = true;	
	var paddingLeft = -1;
	var characterCasing = -1;
	var comboTable = null;
	var compareTarget = 1; 
	var initialHeight = -1; 
	var itemsLength = -1;		
	var left = -1;
	var top = -1; 
	var height = -1;
	var width = -1; 	
	var currentItem = null;	
	var itemOver = null; 
	var itemValue = null; 
	var itemDisplay = null; 
	var itemImage = null; 	
	var itemCSS = null;
	var selectedItemCSS = null; 	
	var owner = null;
	this.ddpb = (combo.getAttribute("ddpb") == "1");
	this.getCharacterCasing = getCharacterCasing; 
	this.getDisplay = getDisplay; 
	this.getDisplayByValue = getDisplayByValue; 
	this.getID = getID; 
	this.getImage = getImage; 
	this.getInnerCombo = getInnerCombo; 
	this.getInnerTextBox = getInnerTextBox; 
	this.getItemCSS = getItemCSS; 
	this.getSelectedItemCSS = getSelectedItemCSS; 
	this.getValue = getValue; 	
	this.setAutoComplete = setAutoComplete; 
	this.setCharacterCasing = setCharacterCasing; 
	this.setCompareTarget = setCompareTarget; 
	this.setLeft = setLeft; 
	this.setOwner = setOwner; 
	this.setWidth = setWidth; 
	this.setHeight = setHeight; 
	this.setItemCSS = setItemCSS; 
	this.setSelectedItemCSS = setSelectedItemCSS;
	this.setStyle = setStyle; 
	this.setTop = setTop; 
	this.setValue = setValue; 		
	this.Focus = Focus; 
	this.Hide = Hide;
	this.Show = Show;
	this.Unload = Unload; 
	this.item_onclick = item_onclick;
	this.item_onmouseover = item_onmouseover;
	this.item_onmouseout = item_onmouseout;
	this.button_onblur = button_onblur;
	this.button_onmousedown = button_onmousedown;
	this.textbox_onblur = textbox_onblur;
	this.textbox_onchange = textbox_onchange;
	this.textbox_onkeydown = textbox_onkeydown;
	this.textbox_onkeypress = textbox_onkeypress;
	this.textbox_onkeyup = textbox_onkeyup; 
	this.combo_onblur = combo_onblur;
	this.combo_onkeydown = combo_onkeydown;
	function getCharacterCasing() { return characterCasing; }	
	function getID() { return id; }	
	function getDisplay() { return itemDisplay; }
	function getDisplayByValue(value)
	{
		var item = getItemByValue(value);
		if(item == null)
			return "";			
		return getDisplayFromItem(item);
	}
	function getImage() { return itemImage; }
	function getInnerCombo() { return combo; }
	function getInnerTextBox() { return textbox; }	
	function getItemCSS() { return itemCSS; }	
	function getSelectedItemCSS() { return selectedItemCSS; }	
	function getValue() { return itemValue; }	
	function setAutoComplete(value) { autoComplete = value; }
	function setCharacterCasing(value) { characterCasing = value; }	
	function setCompareTarget(value) { compareTarget = value; }
	function setItemCSS(value) { itemCSS = value; }			
	function setLeft(value) { left = value; }	
	function setSelectedItemCSS(value) { selectedItemCSS = value; }	
	function setTop(value) { top = value; }	
	function setValue(value, text)
	{
		itemValue = value; 
		var item = getItemByValue(value); 
		if(item != null)
		{
			itemDisplay = getDisplayFromItem(item); 
			itemImage = getImageFromItem(item);			
			updateTextBox(); 			
		}
		else
		{	
			if(text != null)
				itemDisplay = text; 
			else
				itemDisplay = itemValue; 
			itemImage = null; 
			updateTextBox(); 
		}
	}	
	function setHeight(value) { height = value; }	
	function setWidth(value) { width = value; }	
	function setOwner(value) { owner = value; }	
	function setStyle(style)
	{
		if(style != null)
		{			
			textbox.style.fontFamily = style.fontFamily;
			textbox.style.fontSize = style.fontSize; 
			textbox.style.textAlign = style.textAlign;
		}		
	}			
	function commitNewText()
	{
		var text = textbox.value; 
		var item = getItemByDisplay(text); 
		if(item != null)
		{
			var value = getValueFromItem(item);			
			if(value != getValue())
			{				
				itemValue = value; 
				itemDisplay = getDisplayFromItem(item); 
				itemImage = getImageFromItem(item);
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
			}			
		}
		else
		{
			if(compareTarget == 2)
			{				
				itemValue = text;
				itemDisplay = text; 
				itemImage = null;
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
			}
			else if(item == null)
			{
				if(owner != null && owner.NotInList != null)
				{
					if(textbox.value != null)
						owner.NotInList(textbox.value); 
				}
			}
		}
	}
	function getItemByValue(value)
	{
		var row = null; 
		for(var i = 0; i < itemsLength; i++)
		{
			row = comboTable.rows[i];
			if(row.getAttribute("value") == value)
				return row; 
		}		
		return null; 
	}	
	function getItemByDisplay(text)
	{
		var row = null; 
		text = trim(text); 		
		for(var i = 0; i < itemsLength; i++)
		{
			row = comboTable.rows[i]; 
			if(text.toUpperCase() == trim(getDisplayFromItem(row)).toUpperCase())
				return row; 
		}
		return null; 
	}	
	function getPixelPaddingLeft(padding)
	{
		var p = 0;
		var i= padding.indexOf("px");
		if(i> 0)
			p = parseInt(padding.substr(0, i));		
		return p;
	}	
	function getValueFromItem(item) { return item.getAttribute("value"); }	
	function getDisplayFromItem(item)
	{
		if(item == null)
			return null; 	
		var cell = item.cells[0]; 
		if(cell.childNodes.length == 2)
		{
			if(cell.childNodes[0].nodeType == 3)
				return trim(cell.childNodes[0].data); 				
			if(cell.childNodes[1].nodeType == 3)
				return trim(cell.childNodes[1].data); 
		}		
		else if(cell.childNodes.length == 1)
		{
			if(cell.childNodes[0].nodeType == 3)
				return trim(cell.childNodes[0].data); 
		}		
		return null; 
	}	
	function getImageFromItem(item)
	{
		if(item == null)
			return null; 	
		var cell = item.cells[0]; 
		if(cell.childNodes.length == 2)
		{
			if(cell.childNodes[0].nodeType == 1 && cell.childNodes[0].tagName == "IMG")
				return cell.childNodes[0].src; 
				
			if(cell.childNodes[1].nodeType == 1 && cell.childNodes[1].tagName == "IMG")
				return cell.childNodes[1].src;				
		}		
		return null; 
	}	
	function moveUpItem()
	{
		if(itemOver != null && itemOver.rowIndex > 0)
		{
			itemOver.className = getItemCSS();
			itemOver = comboTable.rows[itemOver.rowIndex-1]; 
			itemOver.className = getSelectedItemCSS();
		}
	}	
	function moveDownItem()
	{
		if(itemOver != null && itemOver.rowIndex + 1 < itemsLength)
		{
			itemOver.className = getItemCSS(); 
			itemOver = comboTable.rows[itemOver.rowIndex+1];
			itemOver.className = getSelectedItemCSS(); 
		}
	}	
	function selectTopItem()
	{
		if(itemOver != null)
			itemOver.className = getItemCSS(); 	
		var item = comboTable.rows[0];
		item.className = getSelectedItemCSS();
		return item;
	}	
	function selectBottomItem()
	{
		if(itemOver != null)
			itemOver.className = getItemCSS(); 			
		var item = comboTable.rows[itemsLength-1]; 
		item.className = getSelectedItemCSS(); 
		return item;
	}	
	function updateTextBoxImage()
	{	
		if(itemImage != null)
		{
			textbox.style.backgroundImage = "url(" + itemImage + ")";
			if(textbox.getAttribute("dir") == "rtl")
				textbox.style.backgroundPosition = "center right";
			else
				textbox.style.bacgkroundPosition = "center left"; 			
			textbox.style.backgroundRepeat = "no-repeat"; 
			if(textbox.getAttribute("dir") == "rtl")
				textbox.style.paddingRight = (paddingLeft + 18) + "px";
			else
				textbox.style.paddingLeft = (paddingLeft + 18) + "px"; 
		}
		else
		{
			if(textbox.style.backgroundImage != "none" || textbox.style.backgroundImage != "" )			
			{
				if(textbox.getAttribute("dir") == "rtl")
				{
					if(paddingLeft - 18 >= 0)
						texbox.style.paddingRight = (paddingLeft - 18) + "px";
					else
						textbox.style.paddingRight = paddingLeft; 
				}
				else
				{
					if(paddingLeft - 18 >= 0)
						textbox.style.paddingLeft = (paddingLeft - 18) + "px";
					else
						textbox.style.paddingLeft = paddingLeft; 
				}
			}				
			textbox.style.backgroundImage = "none"; 			
		}	
	}	
	function updateTextBox()
	{	
		if(paddingLeft == -1)
			paddingLeft = getPixelPaddingLeft(textbox.style.paddingLeft); 
			
		updateTextBoxImage();
		if(itemDisplay == null)
			textbox.value = ""; 
		else		
			textbox.value = itemDisplay; 
	}	
	function FindItemWithText(textToFind)
	{		
		var item = null;
		var text = null; 
		textToFind = trim(textToFind); 
		for(var i = 0; i < itemsLength; i++)
		{			
			item = comboTable.rows[i]; 
			text = trim(getDisplayFromItem(item)); 
			if(text.toUpperCase().indexOf(textToFind.toUpperCase()) == 0)
				return item;
		}
		return null;
	}	
	function HideCombo()
	{
		hideDropDownBackFrame();
		combo.style.visibility = "hidden";
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.src = owner.getCell().getGridEX().ddbimg; 
				img.status = 0; 
			}
		}
	}	
	function HideButton() { button.style.visibility = "hidden"; }	
	function HideTextBox()
	{
		textbox.style.visibility = "hidden"; 
		textbox.value = ""; 
	}		
	function Focus()
	{
		textbox.focus(); 
		textbox.select(); 
	}	
	function Hide()
	{
		HideCombo() 
		HideButton(); 
		HideTextBox(); 		
	}	
	function Show()
	{
		if(!browser.isIE)
			button.style.display = ""; 
		button.style.zIndex = 1005;
		button.style.pixelTop = top; 
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			button.style.pixelLeft = left; 
		else
			button.style.pixelLeft = (left + width) - (button.offsetWidth == 0 ? 16 : button.offsetWidth); 
		button.style.pixelHeight = height; 				
		button.style.visibility = "visible"; 		
		textbox.style.zIndex = 1004;
		textbox.style.visibility = "visible";
		if(owner != null && owner.getCell != null && owner.getCell().getGridEX().getHtmlGridEX().getAttribute("rtl") == "1")
			textbox.style.pixelLeft = left + button.offsetWidth; 
		else
			textbox.style.pixelLeft = left + 1;
		textbox.style.pixelTop = top; 
		textbox.style.pixelHeight = height;
		textbox.style.pixelWidth = width - button.offsetWidth - 1; 
		try
		{
			button.focus(); 
		}
		catch(err)
		{ } 
	}		
	function ShowCombo()
	{	
		if(initialHeight == -1)
		{
			if(!browser.isIE)
				combo.getElementsByTagName("TABLE")[0].style.height = "";
			combo.getElementsByTagName("TABLE")[0].style.display = "block";			
			initialHeight = combo.offsetHeight;
			if(!browser.isIE)
			{
				combo.getElementsByTagName("TABLE")[0].style.height = "100%"; 
				combo.getElementsByTagName("TABLE")[0].style.width = "100%"; 
			}
		}		
		var item = getItemByDisplay(textbox.value);
		if(item == null)
		{
			currentItem = null; 
			for(var i=0;i<itemsLength;i++)
			{
				var l=comboTable.rows[i]; 
				if(l.className == getSelectedItemCSS())
					l.className = getItemCSS(); 
			}
		}
		else if(currentItem != item)
			currentItem = item; 		
		if(itemOver != null && itemOver != currentItem)
			itemOver.className = getItemCSS();			
		if(currentItem != null)
			itemOver = currentItem; 			
		if(currentItem != null)
			currentItem.className = getSelectedItemCSS(); 			
		var _fixedTop = -1;
		var scrollHeight = -1;		
		if(owner != null)
		{
			var _heightProposed = -1;
			var ownerTable = owner.getCell().getGridEX().getRootTable().getHtmlItemsTable();
			scrollHeight = (document.body.clientHeight == 0 ? document.documentElement.clientHeight : document.body.clientHeight) + document.body.scrollTop;
			combo.getElementsByTagName("TABLE")[0].style.display = "block"; 
			if(((top + height + 2) + combo.offsetHeight > scrollHeight) || ((top + height + 2) + combo.offsetHeight < scrollHeight))
			{						
				if(scrollHeight - (top + height + 1) >= initialHeight)
					combo.style.pixelHeight = initialHeight; 					
				else
				{
					if(scrollHeight - (top + height + 2) <= 0 || (currentItem != null && scrollHeight - (top + height + 2) <= currentItem.offsetHeight))
					{
						if(initialHeight < top)
							_heightProposed = initialHeight;
						else
							_heightProposed = top; 
						_fixedTop = top - _heightProposed;
					}
					else 
						_heightProposed = scrollHeight - (top + height + 2); 
					combo.style.pixelHeight = _heightProposed;
				}
			}
			else if(scrollHeight - (top + height + 2) >= initialHeight)
			{								
				if(combo.offsetHeight != initialHeight)
					combo.style.pixelHeight = initialHeight; 
			}
		}
		combo.scrollTop = 0;
		if(currentItem != null)
		{
			if((currentItem.offsetTop + currentItem.offsetHeight) >= combo.offsetHeight)
				combo.scrollTop += currentItem.offsetTop;
			else if((currentItem.offsetTop - combo.scrollTop) < 0)
				combo.scrollTop += currentItem.offsetTop; 
		}
		if(_fixedTop != -1)
			combo.style.pixelTop = _fixedTop;
		else
			combo.style.pixelTop = top + height + 2;		
		combo.style.pixelWidth = width;
		if(combo.offsetWidth + left > document.body.clientWidth)
		{			
			left -= (combo.offsetWidth + left) - document.body.clientWidth; 
			left += document.body.scrollLeft; 
			if(left < 0)
			{
				left = 0;
				combo.style.pixelWidth = document.body.clientWidth; 
			}
		}
		combo.style.pixelLeft = left; 
		combo.style.visibility = "visible";
		showDropDownBackFrame(combo.style.pixelLeft, combo.style.pixelTop, combo.offsetWidth, combo.offsetHeight);
		if(combo.setActive != null)
			combo.setActive();
		else if(combo.focus != null)
			combo.focus();
	}
	function Unload()
	{
		if(!browser.isIE)
			return; 					
		delete button;
		button = null; 		
		delete textbox;
		textbox = null;				
		delete combo;
		combo = null;
	}
	function button_onblur()
	{		
		if(button.style.visibility != "visible")
			return; 			
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 			
		if(document.activeElement == combo || combo.contains(document.activeElement))
			return true;
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 		
		if(owner != null && owner.Leaving != null)
			owner.Leaving();
		return true; 
	}	
	function ShowComboEx()
	{
		var show = null;
		if(owner != null && owner.DropDown != null)
			show = owner.DropDown();			
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbpimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.src = owner.getCell().getGridEX().ddbpimg; 
				img.status = 1; 
			}				
		}
		if(show != null && !show)
			return; 				
		ShowCombo();
	}
	function button_onmousedown()
	{		
		if(combo.style.visibility != "visible")
			ShowComboEx();
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.src = owner.getCell().getGridEX().ddbimg; 
					img.status = 0; 
				}
			}
			hideDropDownBackFrame();
			combo.style.visibility = "hidden"; 
		}
	}	
	function combo_onblur()
	{
		if(combo.style.visibility != "visible")
			return; 			
		if(document.activeElement == button || button.contains(document.activeElement))
		{
			HideCombo(); 
			return true; 
		}		
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
		{
			HideCombo();
			return true; 
		}		
		if(document.activeElement == combo || combo.contains(document.activeElement))
			return true; 		
		if(owner != null && owner.Leaving != null)
			owner.Leaving();		
		return true; 		
	}	
	function combo_onkeydown()
	{	
		if(window.event.keyCode == 13)
		{
			if(itemOver != null)
			{
				var _value = getValueFromItem(itemOver); 
				if(_value != getValue())
				{
					itemValue = _value; 
					itemDisplay = getDisplayFromItem(itemOver); 
					itemImage = getImageFromItem(itemOver); 					
					updateTextBox();
					if(owner != null && owner.ValueChanged != null)
						owner.ValueChanged();						
					HideCombo();
					textbox.focus(); 
					textbox.select(); 
				}
				else
					HideCombo(); 
			}
			else
				HideCombo();
			window.event.returnValue = false; 
			window.event.cancelBubble = true;
			return false;			
		}
		else if(window.event.keyCode == 27)
		{
			HideCombo(); 
			textbox.focus(); 			
		}
		else if(window.event.keyCode == 38) 
		{
			if(itemOver == null)
				itemOver = selectTopItem();
			else
				moveUpItem(); 
		}
		else if(window.event.keyCode == 40) 
		{
			if(itemOver == null)
				itemOver = selectTopItem();
			else
				moveDownItem(); 
		}
		else if(window.event.keyCode == 33) 
			itemOver = selectTopItem(); 
		else if(window.event.keyCode == 34)
		{						
			if(itemOver == null)
				itemOver = selectTopItem(); 
			else
				itemOver = selectBottomItem(); 
		}
		return true; 			
	}	
	function item_onmouseover()
	{		
		var item = getListItem(window.event.srcElement);
		if(currentItem != null && item != currentItem)
		{
			currentItem.className = getItemCSS(); 
			currentItem = null; 
		}		
		item.className = getSelectedItemCSS();  
		itemOver = item; 
	}	
	function item_onmouseout()
	{
		var item = getListItem(window.event.srcElement); 
		if(currentItem != null && item != currentItem)
		{
			currentItem.className = getItemCSS(); 
			currentItem = null 
		}		
		item.className = getItemCSS(); 
		if(item == itemOver)
			itemOver = null; 
	}	
	function item_onclick()
	{
		var item = getListItem(window.event.srcElement);		
		var _value = getValueFromItem(item); 
		var _display = getDisplayFromItem(item); 
		var _image = getImageFromItem(item); 		
		if(_value != getValue())
		{
			itemValue = _value;
			itemDisplay = _display; 
			itemImage = _image; 			
			updateTextBox(); 			
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
				
			if(currentItem != item)
			{
				currentItem = item; 
				currentItem.className = getSelectedItemCSS(); 
			}			
		}		
		textbox.focus(); 
		textbox.select(); 		
		HideCombo();
	}	
	function textbox_onblur()
	{				
		if(document.activeElement == button || button.contains(document.activeElement))			
			return true;			
		if(document.activeElement == combo || combo.contains(document.activeElement))		
			return true;		
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
	}	
	function textbox_onchange() 
	{
		if(textbox.style.visibility != "visible")
			return; 			
		commitNewText(); 
	}	
	function textbox_onkeypress()
	{
		if(!autoComplete)
			return; 	
		var c = null;
		var C = null;		
		var display = null; 
		var newChar = null;		
		var indexOf = -1; 
		var range = null;
		var selectedRange = null; 		
		var textDisplay = null; 
		var textToFind = null; 
		var textRemain = null; 		
		var value = null; 		
		if(getCharacterCasing() == 3)
		{
			c = window.event.keyCode;
			newChar = String.fromCharCode(c).toUpperCase(); 
			C = newChar.charCodeAt(); 
			window.event.keyCode = C; 						
		}
		else if(getCharacterCasing() == 1)
		{
			C = window.event.keyCode;
			newChar = String.fromCharCode(C).toLowerCase(); 
			c = newChar.charCodeAt();
			window.event.keyCode = c;			
		}
		else
			newChar = String.fromCharCode(window.event.keyCode); 
		
		textToFind = textbox.value; 
		textDisplay = textbox.value; 						
		if(document.activeElement == textbox && document.selection != null && document.selection.type == "Text")
		{
			range = textbox.createTextRange();
			selectedRange = document.selection.createRange();
			if(selectedRange.compareEndPoints("EndToEnd", range) == 0 && selectedRange.compareEndPoints("StartToStart", range) != 0)
			{
				indexOf = textDisplay.lastIndexOf(selectedRange.text);
				if(indexOf > 0)
				{
					textToFind = textDisplay.substr(0, indexOf);
					textToFind += newChar; 
				}
			}
			else if(selectedRange.compareEndPoints("EndToEnd", range) == 0 && selectedRange.compareEndPoints("StartToStart", range) == 0)
				textToFind = newChar; 
			else
				textToFind += newChar; 
		}
		else
			textToFind += newChar;
			
		var item = FindItemWithText(textToFind);
		if(item != null)
		{
			display = getDisplayFromItem(item);											
			itemDisplay = display;
			itemImage = getImageFromItem(item); 			
			updateTextBox(); 									
			textRemain = display.substr(textToFind.length); 				
			selectedRange = textbox.createTextRange();
			selectedRange.moveStart("character", textToFind.length); 
			selectedRange.moveEnd("character", textRemain.lenght); 
			selectedRange.select(); 			
			value = getValueFromItem(item); 			
			if(value != getValue())
			{
				itemValue = value; 
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged(); 
			}			
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false;  
		}
		else
		{
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(-1); 
		}		
		return true; 
	}	
	function textbox_onkeydown()
	{
		if(window.event.keyCode == 13)
		{			
			commitNewText(); 								
			window.event.returnValue = false;  
			window.event.cancelBubble = true; 
		}		
		else if(window.event.keyCode == 27)							
			itemValue = itemDisplay = itemImage =  textbox.value = null;
		else if(window.event.keyCode == 9 || window.event.keyCode == 38 || (window.event.keyCode == 40 && !window.event.altKey))
			commitNewText();		
		else if(window.event.keyCode == 40 && window.event.altKey)
		{			
			ShowComboEx();
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			return false; 
		}
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 		
		return true; 
	}	
	function textbox_onkeyup()
	{
		if(!browser.isIE)
		{	
			var t = textbox.value; 					
			var item = FindItemWithText(t);
			if(item != null)
			{
				var v = getValueFromItem(item);
				if(v != getValue())
				{
					itemValue = v;					
					itemDisplay = getDisplayFromItem(item);											
					itemImage = getImageFromItem(item); 			
					updateTextBox();					
					if(owner != null && owner.ValueChanged != null)
						owner.ValueChanged(); 
				}
			}
		}
		if(window.event.altKey && window.event.keyCode == 9)
			commitNewText();			
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}		
	comboTable = combo.getElementsByTagName("TABLE")[0]; 
	itemsLength = comboTable.rows.length; 	
	var row = null; 	
	var cbo = this;
	var eid = "e"+id;
	eval(eid+"=null;");
	eval(eid+"=cbo;");
	for(var i = 0; i < itemsLength; i++)
	{
		row = comboTable.rows[i]; 
		browser.handleEvent(row, "click", (browser.isIE ? gcitem_onclick : item_onclick));
		browser.handleEvent(row, "mouseover", (browser.isIE ? gcitem_onmouseover : item_onmouseover));
		browser.handleEvent(row, "mouseout", (browser.isIE ? gcitem_onmouseout : item_onmouseout));		
		row.setAttribute("combo", id);
	}	
	browser.handleEvent(button, "blur", (browser.isIE ? gcbutton_onblur : button_onblur));
	browser.handleEvent(button, "mousedown", (browser.isIE ? gcbutton_onmousedown : button_onmousedown));	
	button.setAttribute("combo", id);
	browser.handleEvent(textbox, "blur", (browser.isIE ? gctextbox_onblur : textbox_onblur));
	browser.handleEvent(textbox, "change", (browser.isIE ? gctextbox_onchange : textbox_onchange));	
	browser.handleEvent(textbox, "keypress", (browser.isIE ? gctextbox_onkeypress : textbox_onkeypress));
	browser.handleEvent(textbox, "keydown", (browser.isIE ? gctextbox_onkeydown : textbox_onkeydown));
	browser.handleEvent(textbox, "keyup", (browser.isIE ? gctextbox_onkeyup : textbox_onkeyup));	
	textbox.setAttribute("combo", id);
	browser.handleEvent(combo, "blur", (browser.isIE ? function() { return gccombo_onblur(id); }  : combo_onblur));
	browser.handleEvent(combo, "keydown", (browser.isIE ? function() { return gccombo_onkeydown(id); } : combo_onkeydown));
	combo.setAttribute("combo", id);
	return this; 
}
function tatextarea_onblur()
{
	var element = window.event.srcElement;
	element.getAttribute("textarea").textarea_onblur();
}
function tatextarea_onchange()
{
	var element = window.event.srcElement;
	element.getAttribute("textarea").textarea_onchange();
}
function tatextarea_onkeypress()
{
	var element = window.event.srcElement;
	element.getAttribute("textarea").textarea_onkeypress();
}
function tatextarea_onkeydown()
{
	var element = window.event.srcElement;
	element.getAttribute("textarea").textarea_onkeydown();
}
function tatextarea_onkeyup()
{
	var element = window.event.srcElement;
	element.getAttribute("textarea").textarea_onkeyup(); 
}
function GridEXEditTextArea(id)
{
	var id = id; 
	var textarea = document.getElementById(id); 
	if(textarea == null)
		throw Error("unable to find HTML textarea to act as GridEXEditTextArea '" + id + "'");		
	if(textarea.parentElement != null && textarea.parentElement.tagName != "BODY")
		document.body.appendChild(textarea); 		
	var height = -1; 
	var left = -1; 
	var top = -1; 
	var width = -1; 	
	var maxlength = -1; 
	var owner = null; 	
	var characterCasing = 2; 
	var text = null; 		
	this.getInnerTextArea = getInnerTextArea; 
	this.getID = getID; 
	this.getLeft = getLeft;
	this.getMaxLength = getMaxLength; 
	this.getTop = getTop;
	this.getHeight = getHeight; 
	this.getWidth = getWidth; 
	this.getText = getText; 	
	this.setCharacterCasing = setCharacterCasing; 
	this.setLeft = setLeft; 
	this.setTop = setTop; 
	this.setWidth = setWidth; 
	this.setHeight = setHeight; 
	this.setMaxLength = setMaxLength; 
	this.setOwner = setOwner; 
	this.setStyle = setStyle; 
	this.setText = setText; 		
	this.Focus = Focus; 
	this.Hide = Hide; 
	this.Show = Show;
	this.Unload = Unload; 
	this.textarea_onblur = textarea_onblur;
	this.textarea_onchange = textarea_onchange;
	this.textarea_onkeypress = textarea_onkeypress;
	this.textarea_onkeydown = textarea_onkeydown;
	this.textarea_onkeyup = textarea_onkeyup;
	function getID() { return id;  }	
	function getInnerTextArea() { return textarea;  }	
	function getLeft() { return left;  }	
	function getMaxLength() { return maxlength; }	
	function getText() { return text; }	
	function getTop() { return top; }	
	function getHeight() { return height; }	 
	function getWidth() { return width; }	
	function setCharacterCasing(value) { characterCasing = value; }	
	function setOwner(value) { owner = value; }	
	function setHeight(value) { height = value;  }	
	function setLeft(value) { left = value; }	
	function setMaxLength(value) { maxlength = value; }	
	function setTop(value) { top = value; }	
	function setWidth(value) { width = value; }	
	function setStyle(style)
	{
		if(style != null)
		{			
			textarea.style.fontFamily = style.fontFamily;
			textarea.style.fontSize = style.fontSize; 
			textarea.style.textAlign = style.textAlign;
		}		
	}	
	function setText(value)
	{
		text = value; 				
		if(text == null)	
			textarea.value = "";
		else
			textarea.value = text;
	}	
	function Hide()
	{
		textarea.style.visibility = "hidden"; 
		textarea.value = "";
		maxlength = -1; 
	}	
	function Show()
	{
		textarea.style.pixelLeft = getLeft(); 
		textarea.style.pixelTop = getTop();
		textarea.style.pixelWidth = getWidth(); 
		textarea.style.pixelHeight = getHeight(); 
		textarea.style.visibility = "visible"; 		
	}
	function Unload()
	{
		if(!browser.isIE)
			return; 					
		delete textarea;
		textarea = null; 
	}
	function Focus()
	{		
		textarea.focus();
		textarea.select(); 
	}		
	function commitNextText()
	{
		var oldText = getText(); 
		var newText = textarea.value; 		
		if(oldText != newText)
		{
			text = newText;
			if(owner != null && owner.TextChanged != null)
				owner.TextChanged(); 
		}
	}		
	function textarea_onblur()
	{
		if(textarea.style.visibility != "visible")
			return; 							
		if(owner != null && owner.Leaving != null)
			owner.Leaving();			
		window.event.returnValue = true;
		window.event.cancelBubble = false;
		return true;
	}	
	function textarea_onchange() { commitNextText(); }	
	function textarea_onkeydown()
	{
		if(window.event.keyCode == 9)
			commitNextText(); 
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
	}
	function textarea_onkeyup()
	{
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}
	function textarea_onkeypress()	
	{		
		var c = null;
		var C = null; 		
		if(getMaxLength() > 0)
		{
			if(textarea.value.length > getMaxLength())
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true;
				return false; 
			}
		}		
		if(characterCasing == 3)
		{
			c = window.event.keyCode;
			C = String.fromCharCode(c).toUpperCase().charCodeAt(); 
			window.event.keyCode = C; 			
		}
		else if(characterCasing == 1)
		{
			C = window.event.keyCode;
			c = String.fromCharCode(C).toLowerCase().charCodeAt();
			window.event.keyCode = c;
		}
	}			
	browser.handleEvent(textarea, "blur", (browser.isIE ? function() { return tatextarea_onblur(); } : textarea_onblur));
	browser.handleEvent(textarea, "change", (browser.isIE ? function() { return tatextarea_onchange(); } : textarea_onchange));
	browser.handleEvent(textarea, "keypress", (browser.isIE ? function() { return tatextarea_onkeypress(); } : textarea_onkeypress));
	browser.handleEvent(textarea, "keydown", (browser.isIE ? function() { return tatextarea_onkeydown(); } : textarea_onkeydown));
	browser.handleEvent(textarea, "keyup", (browser.isIE ? function() { return tatextarea_onkeyup(); } : textarea_onkeyup));	
	textarea.setAttribute("textarea", this);			
	return this;
}
function retrieveEditor(id)
{	
	return getObjectFromID(id); 
}
function tbtextbox_onchange()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("textbox")).textbox_onchange();
}
function tbtextbox_onkeydown()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("textbox")).textbox_onkeydown();
}
function tbtextbox_onkeypress()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("textbox")).textbox_onkeypress();
}
function tbtextbox_onkeyup()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("textbox")).textbox_onkeyup();
}
function tbtextbox_onmousewheel()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("textbox")).textbox_onmousewheel();
}
function tbtextbox_onblur()
{
	var element = window.event.srcElement;
	retrieveEditor("e"+element.getAttribute("textbox")).textbox_onblur();
}
function GridEXEditTextBox(id)
{
	var id = id;	
	var input = document.getElementById(id); 
	if(input == null)
		throw Error("unable to find HTML input to act as GridEXEditTextBox '" + id + "'");		
	if(input.parentElement != null && input.parentElement.tagName != "BODY")
		document.body.appendChild(input); 	
	var inputmask = null; 
	var invalidValueAction = -1;
	var invalidValueMessage = "";		
	var isvalid = true; 	
	var height = -1;
	var left = -1;
	var top = -1;
	var width = -1; 	
	var characterCasing = 2;		
	var text = null;	
	var owner = null;	
	this.getCharacterCasing = getCharacterCasing; 
	this.getHeight = getHeight; 
	this.getID = getID;
	this.getInnerHTML = getInnerHTML; 
	this.getInputMask = getInputMask;
	this.getLeft = getLeft; 
	this.getOwner = getOwner; 
	this.getText = getText;
	this.getTop = getTop; 
	this.getWidth = getWidth; 
	this.isValid = isValid; 
	this.setOwner = setOwner;
	this.setCharacterCasing = setCharacterCasing; 	
	this.setHeight = setHeight; 
	this.setLeft = setLeft;
	this.setMaxLength = setMaxLength;
	this.setText = setText; 
	this.setTop = setTop;
	this.setWidth = setWidth; 	
	this.setInputMask = setInputMask;	
	this.setInvalidValueAction = setInvalidValueAction;
	this.setInvalidValueMessage = setInvalidValueMessage; 
	this.setOwner = setOwner;
	this.setStyle = setStyle;	
	this.Focus = Focus; 
	this.Hide = Hide; 
	this.Show = Show; 
	this.Unload = Unload;
	this.textbox_onchange = textbox_onchange;
	this.textbox_onkeydown = textbox_onkeydown;	
	this.textbox_onkeypress = textbox_onkeypress;
	this.textbox_onkeyup = textbox_onkeyup;
	this.textbox_onmousewheel = textbox_onmousewheel;
	this.textbox_onblur = textbox_onblur;
	function getOwner() { return owner; }	
	function getCharacterCasing() { return characterCasing; }			
	function getID() { return id;  }	
	function getInnerHTML() {	return input; }	
	function getInputMask() { return inputmask; }	
	function getHeight() { return height; }	
	function getLeft() { return left; }	
	function getTop() {	return top; }	
	function getWidth() { return width; }	
	function getText() {	 return text; }			
	function isValid() { return isValid; }	
	function setLeft(value) { left = value; }	
	function setHeight(value) { height = value; }	
	function setTop(value) { top = value; }	
	function setWidth(value) { width = value; }	
	function setOwner(value) { owner = value; }	
	function setCharacterCasing(value) { characterCasing = value; }	
	function setInputMask(value) { inputmask = value; }	
	function setText(value)
	{
		text = value; 		
		if(text == null)	
			input.value = "";
		else
			input.value = text; 			
	}	
	function setInvalidValueAction(value) { invalidValueAction = value; }	
	function setInvalidValueMessage(value) { invalidValueMessage = value; }	
	function setMaxLength(value)
	{
		if(value == 0)
			input.maxLength = TEXTBOX_MAXLENGTH;
		else
			input.maxLength = value; 	
	}	
	function setStyle(style)
	{
		if(style != null)
		{			
			input.style.fontFamily = style.fontFamily;
			input.style.fontSize = style.fontSize; 
			input.style.textAlign = style.textAlign;
		}
	}	
	function getTrimmedText(value)
	{	
		var m = value.match(/^\s*(\S+(\s+\S+)*)\s*$/);
		return (m == null) ? "" : m[1];
	}	
	function requiresValidation() { return (getInputMask() != null && getInputMask().length > 0); }		
	function isValidText(value)
	{	
		if(getInputMask() == null || getInputMask().length == 0)
			return true; 			
		value = getTrimmedText(value); 
		if(value.length == 0)
			return true; 			
		var cancel = null;
		if(owner != null && owner.ApplyingInputMask != null)
			cancel = owner.ApplyingInputMask();			
		if(cancel == null || !cancel)
		{
			var rx = new RegExp(getInputMask());
			var matches = rx.exec(value);
			return (matches != null && value == matches[0]);
		}
		else
			return true; 
	}	
	function validateText(value)
	{	
		if(isValidText(input.value))						
			return true; 
		else
		{
			if(invalidValueAction == 2 || invalidValueAction == 3)		
			{								
				if(invalidValueAction == 3 && invalidValueMessage.length > 0)
					alert(invalidValueMessage);
			}			
			else if(invalidValueAction == 4 || invalidValueAction == 5)
			{								
				if(invalidValueAction == 5 && invalidValueMessage.length > 0)
					alert(invalidValueMessage);									
			}
			else if(invalidValueAction == 6 || invalidValueAction == 7)
			{
				if(invalidValueAction == 7 && invalidValueMessage.length > 0)
					alert(invalidValueMessage); 
			}			
			try { owner.Show(); }
			catch(e) { }
			return false; 
		}		
	}	
	function textbox_onblur()
	{		
		if(pendingBlur)
		{
			pendingBlur = false; 
			return;
		}		
		if(input.style.visibility != "visible")		
			return;			
		if(!isvalid)
		{			
			if(invalidValueAction == 6 || invalidValueAction == 7)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true;
				return false; 
			}
		}			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
		window.event.returnValue = true;
		window.event.cancelBubble = false;
		return true; 
	}	
	function textbox_onchange()	
	{	
		if(input.style.visibility == "hidden")
			return;			
		if(requiresValidation())
		{				
			isvalid = validateText(input.value); 															
			if(isvalid)
				commitNewText(); 
		}
		else
			commitNewText(); 
	}	
	function commitNewText()
	{
		var oldText = getText(); 
		var newText = input.value				
		if(oldText != newText)
		{
			text = newText; 
			if(owner != null && owner.TextChanged != null)
				owner.TextChanged();
		}				
	}	
	function textbox_onkeydown()
	{		
		if(window.event.keyCode == 13)
		{	
			if(requiresValidation())
			{
				isvalid = validateText(input.value); 
				if(isvalid)
					commitNewText();							
			}
			else
				commitNewText();
			window.event.returnValue = false;
			window.event.cancelBubble = true;
			if(!isvalid)
				return; 
		}
		else if(window.event.keyCode == 27)
			input.value = getText();
		else if(window.event.keyCode == 9 || window.event.keyCode == 40 || window.event.keyCode == 38)
		{
			if(requiresValidation())
			{
				isvalid = validateText(input.value);
				if(isvalid)
					commitNewText(); 
				else
					return; 
			}
			else
				commitNewText(); 
		}						
		if(owner != null && owner.KeyDown != null)			
			owner.KeyDown(); 
		return true; 
	}			
	function textbox_onkeypress()
	{			
		var c = null;
		var C = null; 	
		if(characterCasing == 3)
		{
			c = window.event.keyCode;
			C = String.fromCharCode(c).toUpperCase().charCodeAt(); 
			window.event.keyCode = C; 			
		}
		else if(characterCasing == 1)
		{
			C = window.event.keyCode;
			c = String.fromCharCode(C).toLowerCase().charCodeAt();
			window.event.keyCode = c;
		}					
	}	
	function textbox_onkeyup()
	{
		if(window.event.altKey && window.event.keyCode == 9)
		{
			if(requiresValidation())
			{
				isvalid = validateText(input.value);
				if(isvalid)
					commitNewText(); 
				else
					return; 
			}
			else
				commitNewText(); 
		}	
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}	
	function textbox_onmousewheel()
	{
		if(owner != null && owner.MouseWheel != null)
			owner.MouseWheel(); 
	}
	function Hide()
	{		
		input.value = ""; 
		input.style.visibility = "hidden"; 				
	}	
	function Focus()
	{
		input.select(); 
		input.focus(); 
	}	
	function Unload()
	{
		if(!browser.isIE)
			return; 				
		delete input;
		input = null; 
	}
	function Show()
	{
		input.style.pixelLeft = getLeft(); 
		input.style.pixelTop = getTop(); 
		input.style.pixelHeight = getHeight(); 
		input.style.pixelWidth = getWidth();
		input.style.visibility = "visible"; 
		input.style.display = "block";
	}	
	browser.handleEvent(input, "change", (browser.isIE ? tbtextbox_onchange : textbox_onchange));
	browser.handleEvent(input, "keydown", (browser.isIE ? tbtextbox_onkeydown : textbox_onkeydown));
	browser.handleEvent(input, "keypress", (browser.isIE ? tbtextbox_onkeypress : textbox_onkeypress));
	browser.handleEvent(input, "keyup", (browser.isIE ? tbtextbox_onkeyup : textbox_onkeyup));
	browser.handleEvent(input, "mousewheel", (browser.isIE ? tbtextbox_onmousewheel : textbox_onmousewheel));
	browser.handleEvent(input, "blur", (browser.isIE ? tbtextbox_onblur : textbox_onblur));	
	var txtbox = this; 
	var eid = "e"+id;
	eval(eid+"=null;");	
	eval(eid+"=txtbox;");	
	input.setAttribute("textbox",id);	
	return this;
}