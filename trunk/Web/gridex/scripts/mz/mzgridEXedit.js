//////////////////////////////////////////////////////////////////
// GridEX JavaScript MZ API  1.1.1009
// Copyright by Janus Systems S.A.
// 2002 - 2004
//////////////////////////////////////////////////////////////////
var cellForEdit = null; 
var isEditing = false; 
var objectForEdit = null;
var TEXTBOX_MAXLENGTH= 2147483647;
var currentDropDown = null;
function GridEXEditingArgs(cell, value)
{
	var editingCell = cell; 
	var editingValue = value; 	
	this.getCell = getCell; 
	this.getValue = getValue;
	this.setValue = setValue;	
	function getCell()
	{
		return editingCell; 
	}	
	function getValue()
	{
		return editingValue;
	}	
	function setValue(value)
	{
		editingValue = value; 
	}	
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
	function getCell()
	{
		return updatingCell; 
	}	
	function getInitialValue()
	{
		return updatingInitialValue; 
	}	
	function getValue()
	{
		return updatingValue; 
	}			
	function setValue(value)
	{
		updatingValue = value; 
	}	
	return this;
}
function dropdownbutton_mouseover(img, url)
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
function GridEXComboDropDown(id)
{
	var id = id; 
	var _clientevents = null;
	try {  _clientevents = eval(id + "_client_events"); } catch(E) { _clientevents = null; }	
	var gridEX = new GridEX(id, eval(id + "_client"), eval(id + "_client_tables"), eval(id + "_client_selected_items"), eval(id + "_client_rows_css"), eval(id + "_client_hidden_values"), _clientevents); 	
	gridEX.Initialize(); 	
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXComboDropDown '" + id + "'"); 
		
	var textbox = document.getElementById(id + "_textbox"); 
	if(textbox == null)
		throw Error("unable to find textbox for GridEXComboDropDown '" + id + "'"); 
		
	var innerGridEX = gridEX.getHtmlGridEX();	
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
	this.getCharacterCasing = getCharacterCasing; 	
	this.getDisplay = getDisplay;
	this.getDisplayByValue = getDisplayByValue;
	this.getGridEX = getGridEX; 
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
	function getDisplayFromItem(item)
	{
		return item.getAttribute("displayMember"); 
	}	
	function getValueFromItem(item)
	{
		return item.getAttribute("value"); 
	}
	function getGridEX()
	{
		return gridEX; 
	}
	function getID()
	{
		return id; 
	}	
	function getInnerTextBox()
	{
		return textbox; 
	}	
	function getValue()
	{
		return itemValue; 
	}	
	function getDisplay()
	{
		return itemDisplay; 
	}
	function getDisplayByValue(value)
	{
		var item = gridEX.getRootTable().FindRowByValue(value);
		if(item != null && item.length == 2)
			return getDisplayFromItem(item[1]); 
		return "";
	}
	function getCharacterCasing()
	{
		return characterCasing; 
	}	
	function setCharacterCasing(value)
	{
		characterCasing = value; 
	}	
	function setOwner(value)
	{
		owner = value; 
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setHeight(value)
	{
		height = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setStyle(style)
	{
		if(style != null)
		{			
			textbox.style.fontFamily = style.getPropertyValue("font-family"); 
			textbox.style.fontSize = style.getPropertyValue("font-size"); 
			textbox.style.textAlign = style.getPropertyValue("text-align"); 
		}
	}	
	function setValue(value,text)
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
			if(element.nodeType == 1 && element.tagName == "TR" && element.id != null && element.t != null)
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
	function commitNewText()
	{	
		if(textbox.value == itemDisplay)
			return;
			
		var item = gridEX.getRootTable().FindRowWithDisplay(textbox.value); 
		if(item != null && item.length == 2)
		{
			var value = getValueFromItem(item[1]);
			if(value != getValue())
			{
				itemDisplay = getDisplayFromItem(item[1]); 
				itemValue = value; 				
				if(owner != null && owner.ValueChanged != null)
					owner.ValueChanged();
			}
		}
		else if(item == null)
		{
			if(owner != null && owner.NotInList != null)
				owner.NotInList(textbox.value);
		}
	}	
	function commitNewValue(item)
	{	
		var value = getValueFromItem(item);		
		if(value != getValue())
		{				
			itemDisplay = getDisplayFromItem(item);
			itemValue = value; 
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged();
				
			updateTextBox(); 
		}
	}	
	function updateTextBox()
	{
		textbox.value = itemDisplay;
		if(textbox.style.visibility == "visible")
			textbox.select(); 
	}	
	function updateDropDown()
	{	
		if(innerGridEX.style.display == "none")
			innerGridEX.style.display = "block"; 
	
		if(gridEX.getSelectedItems() != null && gridEX.getSelectedItems().Count() > 0)
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
		if(owner == null)
			return; 
			
		if(gridEX.getGridEXRow() != null)
			gridEX.getGridEXRow().HideHeaderIndicator();
		innerGridEX.style.visibility = "hidden"; 
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button; 
				img.src = owner.getCell().getGridEX().ddbimg; 
				img.setAttribute("status", 0); 
			}
		}
	}	
	function HideButton()
	{
		button.style.visibility = "hidden"; 		
	}	
	function HideTextBox()
	{
		textbox.style.visibility = "hidden"; 
	}	
	function ShowGridEX()	
	{	
		if(initialHeight == -1)
		{
			innerGridEX.style.display = "block";
			initialHeight = gridEX.getRootTable().getHtmlItemsTable().offsetParent.offsetHeight;			
		}			
		var scrollHeight = -1;		
		if(owner != null)
		{
			var _fixedTop = -1; 
			var innerDropDownTable = null; 
			var ownerTable = owner.getCell().getGridEX().getRootTable().getHtmlItemsTable();
			scrollHeight = window.innerHeight; 
			if(((top + button.offsetHeight + 1) + innerGridEX.offsetHeight > scrollHeight) || ((top + button.offsetHeight + 1) + innerGridEX.offsetHeight < scrollHeight))
			{		
				innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable(); 					
				if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
				{
					innerDropDownTable.offsetParent.style.height = initialHeight + "px"; 
					if(gridEX.getRootTable().getHeaders() != null)
					{
						var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
						innerGridEX.style.height = (initialHeight + gheader.getHtmlHeader().offsetHeight) + "px"; 
					}
					else
						innerGridEX.style.height = initialHeight + "px"; 
				}
				else
				{								
					var _headerheight = 0; 
					if(gridEX.getRootTable().getHeaders() != null)
					{
						var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
						_headerheight = gheader.getHtmlHeader().offsetHeight
					}
					var _height = (scrollHeight - (top + button.offsetHeight + 1)) - _headerheight; 
					if(_height <= _headerheight || currentItem != null && _height <= currentItem.offsetHeight)
					{
						if(initialHeight < top)
							_height = initialHeight;
						else
							_height = top - _headerheight; 
						_fixedTop = (top - _height); 						
						innerDropDownTable.offsetParent.style.height = _height + "px"; 
					}						
					else
						innerDropDownTable.offsetParent.style.height =  _height + "px"; 
					innerGridEX.style.height = (_height + _headerheight) + "px";
				}
			}
			else if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
			{				
				innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable();
				if(innerDropDownTable.offsetParent.offsetHeight != initialHeight)
				{
					innerDropDownTable.offsetParent.style.height = (initialHeight) + "px"; 
					if(gridEX.getRootTable().getHeaders() != null)
					{
						var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
						innerGridEX.style.height = (initialHeight + gheader.getHtmlHeader().offsetHeight) + "px"; 
					}
					else
						innerGridEX.style.height = initialHeight + "px"; 
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
		}
		innerGridEX.style.left = (left) + "px"; 
		if(_fixedTop != -1)
			innerGridEX.style.top = _fixedTop + "px"; 
		else
			innerGridEX.style.top = ((top + button.offsetHeight) + 1) + "px";		
		innerGridEX.style.visibility = "visible"; 		
		if(innerGridEX.focus != null)
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
		button.style.display = ""; 
		button.style.visibility = "visible"; 
		button.style.top = top + "px"; 	
		button.style.left = ((left + width) - button.offsetWidth) + "px"; 
		button.style.height = height + "px";		
		textbox.style.visibility = "visible";
		textbox.style.top = top + "px"; 
		textbox.style.left = left + "px"; 
		textbox.style.height = height + "px"; 
		textbox.style.width = (width - button.offsetWidth) + "px";
		try
		{
			textbox.focus(); 
			textbox.select(); 
		}
		catch(err)
		{}
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
		item.className = (item.className == itemCss) ? itemSelCss : itemCss; 		
		if(itemOver != null && itemOver == item)
			itemOver = null;
	}	
	function ItemOnClick(item, argument)
	{		
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
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == innerGridEX || innerGridEX.contains(document.activeElement))
			return true;
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
			
		Hide(); 
		return true; 
	}	
	function button_onmousedown()
	{
		if(innerGridEX.style.visibility != "visible")
		{		
			var show = null; 
			if(owner != null && owner.DropDown != null)
				show = owner.DropDown();
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button; 					
					img.setAttribute("status", 1);
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}
			if(show != null && !show)
				return;
				
			updateDropDown(); 				
			ShowGridEX();
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button;
					img.setAttribute("status", 0);
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
		if(document.activeElement == button || button.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == innerGridEX || innerGridEX.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
			
		Hide(); 
		return true; 
	}	
	function textbox_onchange()
	{
		commitNewText(); 
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
			itemValue = itemDisplay = null;
		else if(window.event.keyCode == 38 || window.event.keyCode == 40)
			commitNewText();
		
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown(); 
		
		return true; 
	}
	function textbox_onkeyup()
	{
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}
	function textbox_onkeypress()
	{
		return true; 
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
		if(window.event.keyCode == 13)
		{
			if(itemOver != null)
				commitNewValue(itemOver); 
				
			HideGridEX(); 
			textbox.focus();
			textbox.select(); 
			window.event.returnValue = false;
			window.event.cancelBubble = true; 			
		}
		else if(window.event.keyCode == 27)
		{
			HideGridEX(); 
			textbox.focus();
			textbox.select(); 
		}			
		return true;
	}
	button.addEventListener("blur", button_onblur, false); 
	button.addEventListener("mousedown", button_onmousedown, false);	
	textbox.addEventListener("blur", textbox_onblur, false); 
	textbox.addEventListener("change", textbox_onchange, false);
	textbox.addEventListener("keypress", textbox_onkeypress, false);
	textbox.addEventListener("keydown", textbox_onkeydown, false); 		
	textbox.addEventListener("keyup", textbox_onkeyup, false); 
	gridEX.setOwner(this); 	
	var gridEXDropDown = this; 	
	return this; 
}
function GridEXDropDown(id)
{
	var id = id;
	var _clientevents = null;
	try {  _clientevents = eval(id + "_client_events"); } catch(E) { _clientevents = null; }
	var gridEX = new GridEX(id, eval(id + "_client"), eval(id + "_client_tables"), eval(id + "_client_selected_items"), eval(id + "_client_rows_css"), eval(id + "_client_hidden_values"), _clientevents); 	
	gridEX.Initialize(); 	
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find button for GridEXDropDown"); 
		
	var innerGridEX = gridEX.getHtmlGridEX(); 	
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
	this.getID = getID;
	this.getDisplay = getDisplay;
	this.getDisplayByValue = getDisplayByValue;
	this.getGridEX = getGridEX; 
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
	this.Hide = Hide;
	this.Show = Show; 	
	function getID()
	{
		return id; 
	}			
	function getDisplay()
	{
		return itemDisplay; 
	}
	function getDisplayByValue(value)
	{
		var item = gridEX.getRootTable().FindRowByValue(value); 
		if(item != null && item.length == 2)
			return getDisplayFromItem(item[1]);
		return ""; 
	}
	function getGridEX()
	{
		return gridEX; 
	}
	function getValue()
	{
		return itemValue; 
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setHeight(value)
	{
		height = value; 
	}	
	function setValue(value)
	{
		itemValue = value; 				
		var item = gridEX.getRootTable().FindRowByValue(value);
		if(item != null && item.length == 2)
			itemDisplay = getDisplayFromItem(item[1]);						
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setOwner(value)
	{
		owner = value; 
	}			
	function getItemFromInnerRow(element)
	{
		while(element != null)
		{
			if(element.nodeType == 1 && element.tagName == "TR" && element.id != null && element.t != null)
				return element; 
		
			element = element.parentElement; 
		}			
		if(element == null)
			throw Error("unable to find item"); 
	}	
	function updateDropDown()
	{	
		if(innerGridEX.style.display == "none")
			innerGridEX.style.display = "block"; 
	
		if(gridEX.getSelectedItems() != null && gridEX.getSelectedItems().Count() > 0)
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
	function getValueFromItem(item)
	{
		return item.getAttribute("value"); 
	}	
	function getDisplayFromItem(item)
	{
		return item.getAttribute("displayMember"); 
	}	
	function commitNewValue(item)
	{			
		var value = getValueFromItem(item); 		
		if(value != getValue())
		{	
			itemDisplay = getDisplayFromItem(item); 	
			itemValue = value;
			if(owner != null && owner.ValueChanged != null)
				owner.ValueChanged(); 
		}
	}	
	function HideButton()
	{
		button.style.display = "none"; 
		button.style.visibility = "hidden"; 
	}	
	function HideGridEX()
	{
		if(gridEX.getGridEXRow() != null)
			gridEX.getGridEXRow().HideHeaderIndicator();
		innerGridEX.style.visibility = "hidden";
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.setAttribute("status", 0);
				img.src = owner.getCell().getGridEX().ddbimg;
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
		var scrollHeight = -1;		
		if(owner != null)
		{
			var _fixedTop = -1;
			var innerDropDownTable = null; 
			var ownerTable = owner.getCell().getGridEX().getRootTable().getHtmlItemsTable();
			scrollHeight = window.innerHeight;
			if(((top + button.offsetHeight + 1) + innerGridEX.offsetHeight > scrollHeight) || ((top + button.offsetHeight + 1) + innerGridEX.offsetHeight < scrollHeight))
			{		
				innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable(); 					
				if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
				{
					innerDropDownTable.offsetParent.style.height = initialHeight + "px"; 
					if(gridEX.getRootTable().getHeaders() != null)
					{
						var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
						innerGridEX.style.height = (initialHeight + gheader.getHtmlHeader().offsetHeight) + "px";
					}
					else
						innerGridEX.style.height = initialHeight + "px"; 						
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
					if(_height <= _headerheight || currentItem != null && _height <= currentItem.offsetHeight)
					{			
						if(initialHeight < top)
							_height = initialHeight; 
						else
							_height  = top - _headerheight;			
						_fixedTop = (top - _height);						
						innerDropDownTable.offsetParent.style.height = _height + "px";
					}
					else
						innerDropDownTable.offsetParent.style.height = _height + "px"; 
					innerGridEX.style.height = (_height + _headerheight) + "px"; 
				}
			}
			else if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
			{				
				innerDropDownTable = gridEX.getRootTable().getHtmlItemsTable();
				if(innerDropDownTable.offsetParent.offsetHeight != initialHeight)
				{
					innerDropDownTable.offsetParent.style.height = initialHeight + "px"; 
					if(gridEX.getRootTable().getHeaders() != null)
					{	
						var gheader = (gridEX.getRootTable().getHeaders().length > 0) ? gridEX.getRootTable().getHeaders()[0] : gridEX.getRootTable().getHeaders();
						innerGridEX.style.height = (initialHeight + gheader.getHtmlHeader().offsetHeight) + "px"; 
					}
					else
						innerGridEX.style.height = initialHeight + "px"; 
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
		}			
		innerGridEX.style.left = (left) + "px";
		if(_fixedTop != -1)
			innerGridEX.style.top = _fixedTop;
		else
			innerGridEX.style.top = (top + button.offsetHeight + 1) + "px";		
		innerGridEX.style.visibility = "visible"; 
		if(innerGridEX.focus != null)
			innerGridEX.focus(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true; 
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
		item.className = (item.className == itemCss) ? itemSelCss : itemCss; 		
		if(itemOver != null && itemOver == item)
			itemOver = null; 		
	}	
	function ItemOnClick(item, argument)
	{		
		commitNewValue(item); 
		HideGridEX(); 
		button.focus();
		if(argument != null)
		{
			if(owner.getCell != null)
				owner.getCell().getGridEX().DoPostBack(argument[0], argument[1]);
		}
	}	
	function Hide()
	{
		HideGridEX(); 
		HideButton(); 		
	}	
	function Show()
	{					
		button.style.display = ""; 
		button.style.visibility = "visible"; 					
		button.style.top = (top) + "px"; 
		button.style.left = ((left + width) - button.offsetWidth) + "px";		
		button.style.height = (height) + "px";		
		button.focus();
		currentDropDown = gridEXDropDown; 
	}		
	function button_onblur()
	{	
		try
		{
			if(document.activeElement == innerGridEX || innerGridEX.contains(document.activeElement))
				return true;
		}
		catch(err) { }
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
			
		Hide();
		return true; 
	}	
	function button_onkeydown()
	{	
		if(owner != null && owner.KeyDown != null)
		{
			owner.KeyDown(); 
			if(window.event.keyCode == 13)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}
		}			
		return true; 
	}	
	function button_onmousedown()
	{		
		if(innerGridEX.style.visibility != "visible")
		{
			var show = null;
			if(owner != null && owner.DropDown != null)
				show = owner.DropDown();
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 1);
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}
			if(show != null && !show)
				return;
				
			updateDropDown();				
			ShowGridEX(); 
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 0);
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
		if(innerGridEX.style.visibility == "hidden")
			return; 
	
		if(window.event.keyCode == 13)
		{
			if(itemOver != null)
				commitNewValue(itemOver); 
				
			HideGridEX(); 
			button.focus(); 
			window.event.returnValue = false;
			window.event.cancelBubble = true; 			
		}
		else if(window.event.keyCode == 27)
		{
			HideGridEX(); 
			button.focus(); 
		}			
		return true; 
	}				
	button.addEventListener("blur", button_onblur, false); 
	button.addEventListener("keydown", button_onkeydown, false); 
	button.addEventListener("mousedown", button_onmousedown, false); 	
	gridEX.setOwner(this); 	
	var gridEXDropDown = this; 	
	return this; 
}
function GridEXValueList(id)
{
	var id = id;
	var list = document.getElementById(id); 
	if(list == null)
		throw Error("unable to find list for ValueList"); 
		
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find button for ValueList");
		
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
	function getDisplay()
	{
		return itemDisplay; 
	}	
	function getID()
	{
		return id; 
	}	
	function getImage()
	{
		return itemImage; 
	}
	function getInnerList()
	{
		return list;
	}
	function getItemCSS()
	{
		return itemCSS; 
	}	
	function getSelectedItemCSS()
	{
		return selectedItemCSS; 
	}	
	function getValue()
	{
		return itemValue; 
	}
	function setCompareTarget(target)
	{
		compareTarget = target; 
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setOwner(value)
	{
		owner = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setHeight(value)
	{
		height = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setItemCSS(value)
	{
		itemCSS = value; 
	}	
	function setSelectedItemCSS(value)
	{
		selectedItemCSS = value; 
	}	
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
	function Focus()
	{
		button.focus();
	}	
	function Hide()
	{
		HideList(); 
		HideButton(); 
	}	
	function Show()
	{				
		button.style.display = "";
		button.style.visibility = "visible"; 
		button.style.top = top + "px"; 
		button.style.left = ((left + width) - button.offsetWidth) + "px"; 
		button.style.height = height + "px";
		button.focus();
	}	
	function ShowValueList()
	{	
		if(initialHeight == -1)
		{
			list.style.visibility = "visible";
			list.getElementsByTagName("TABLE")[0].style.height = ""; 
			list.getElementsByTagName("TABLE")[0].style.display = "block"; 
			initialHeight = list.offsetHeight;
			list.getElementsByTagName("TABLE")[0].style.height = "100%"; 
		}
		var _fixedTop = -1;
		var listHeight = -1;
		var scrollHeight = -1;		
		if(owner != null)
		{
			var _heightProposed = -1;
			var ownerTable = owner.getCell().getGridEX().getRootTable().getHtmlItemsTable();
			scrollHeight = window.innerHeight; 
			list.getElementsByTagName("TABLE")[0].style.display = "block"; 
			if(list.offsetHeight != 0)
				listHeight = list.offsetHeight;
			else
				listHeight = getPixelValue(list.style.height); 
			if(((top + height + 2) + listHeight > scrollHeight) || ((top + height + 2) + listHeight < scrollHeight))
			{						
				if(scrollHeight - (top + button.offsetHeight + 1) >= initialHeight)
					list.style.height = initialHeight + "px"; 					
				else
				{
					if(scrollHeight - (top + height + 2) <= 0 || (currentItem != null && scrollHeight - (top + height + 2) <= currentItem.offsetHeight))
					{
						_heightProposed = initialHeight;
						_fixedTop = top - _heightProposed;
					}
					else					
						_heightProposed = scrollHeight - (top + height + 2);
					list.style.height = _heightProposed + "px";
				}
			}
			else if(scrollHeight - (top + height + 2) >= initialHeight)
			{								
				if(list.offsetHeight != initialHeight)
					list.style.height = initialHeight + "px"; 
			}
		}
		
		list.style.left = left + "px"; 
		if(_fixedTop != -1)
			list.style.top = _fixedTop + "px"; 
		else
			list.style.top = (top + height + 2) + "px";
		list.style.width = width + "px"; 
		list.style.visibility = "visible";			
	}			
	function HideList()
	{
		list.style.visibility = "hidden"; 
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.setAttribute("status", 0);
				img.src = owner.getCell().getGridEX().ddbimg;
			}
		}
	}	
	function HideButton()
	{
		button.style.display = "none"; 
		button.style.visibility = "hidden"; 
	}	
	function getItem(element)
	{		
		if(element == null)
			throw Error("unable to find element"); 
			
		if(element.nodeType == 1 && element.tagName == "TR")
			return element;
		else
			return getItem(element.parentElement);
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
	function getValueFromItem(item)
	{
		return item.getAttribute("value"); 
	}	
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
		var item = getItem(window.event.srcElement);
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
		var item = getItem(window.event.srcElement); 
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
		var item = getItem(window.event.srcElement);		
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
		window.event.returnValue = false; 
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
			owner.KeyDown(); 			
			if(window.event.keyCode == 13)
			{
				window.event.returnValue = false;
				window.event.cancelBubble = true; 
			}
		}
		return true; 
	}	
	function button_onmousedown()
	{
		if(list.style.visibility != "visible")		
		{	
			if(owner != null && owner.DropDown != null)
				owner.DropDown();
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 1);
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}
			ShowValueList();
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 0);
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}
			list.style.visibility = "hidden"; 
		}
	}	
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
					{
						owner.ValueChanged(); 
					}
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
	var row = null; 	
	for(var i = 0; i < itemsLength; i++)
	{
		row = listTable.rows[i];
		row.addEventListener("mousedown", item_onclick, false); 
		row.addEventListener("mouseover", item_onmouseover, false); 
		row.addEventListener("mouseout", item_onmouseout, false);		
	}	
	button.addEventListener("blur", button_onblur, false); 
	button.addEventListener("keydown", button_onkeydown, false); 
	button.addEventListener("mousedown", button_onmousedown, false); 	
	list.addEventListener("blur", list_onblur, false); 
	list.addEventListener("keydown", list_onkeydown, false);	
	return this; 
}
function GridEXCombo(id)
{
	var id = id; 
	var button = document.getElementById(id + "_button"); 
	if(button == null)
		throw Error("unable to find HTML element to act as dropdown button"); 
		
	var textbox = document.getElementById(id + "_textbox"); 
	if(textbox == null)
		throw Error("unable to find HTML element to act as textbox"); 	

	var combo = document.getElementById(id);
	if(combo == null) 
		throw Error("unable to find HTML element to act as GridEXCombo '" + id + "'"); 	
	
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
	function getCharacterCasing()
	{
		return characterCasing; 
	}	
	function getID()
	{
		return id; 
	}	
	function getDisplay()
	{
		return itemDisplay; 
	}
	function getDisplayByValue(value)
	{
		var item = getItemByValue(value);
		if(item == null)
			return "";
			
		return getDisplayFromItem(item);
	}
	function getImage()
	{
		return itemImage;
	}	
	function getInnerTextBox()
	{
		return textbox; 
	}	
	function getItemCSS()
	{
		return itemCSS; 
	}	
	function getSelectedItemCSS()
	{
		return selectedItemCSS; 
	}	
	function getValue()
	{
		return itemValue; 
	}	
	function setAutoComplete(value)
	{
		autoComplete = value; 
	}
	function setCharacterCasing(value)
	{
		characterCasing = value; 
	}	
	function setCompareTarget(value)
	{
		compareTarget = value; 
	}
	function setItemCSS(value)
	{		
		itemCSS = value; 
	}			
	function setLeft(value)
	{
		left = value; 
	}	
	function setSelectedItemCSS(value)
	{
		selectedItemCSS = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
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
		if(currentItem != null && currentItem != item)
			currentItem.className = getItemCSS();
		itemOver = currentItem = null; 			
		if(item != null)
		{
			item.className = getSelectedItemCSS(); 
			itemOver = currentItem = item;
		}
	}	
	function setHeight(value)
	{
		height = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setOwner(value)
	{
		owner = value; 
	}	
	function setStyle(style)
	{
		if(style != null)
		{			
			textbox.style.fontFamily = style.getPropertyValue("font-family");
			textbox.style.fontSize = style.getPropertyValue("font-size"); 
			textbox.style.textAlign = style.getPropertyValue("text-align");
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
					owner.NotInList(textbox.value); 
			}
		}
	}	
	function getItem(element)
	{		
		if(element == null)
			throw Error("unable to find element"); 
			
		if(element.nodeType == 1 && element.tagName == "TR")
			return element;
		else
			return getItem(element.parentElement);
	}	
	function getItemByValue(value)
	{
		var row = null; 
		for(var index = 0; index < itemsLength; index++)
		{
			row = comboTable.rows[index];
			if(row.getAttribute("value") == value)
				return row; 
		}		
		return null; 
	}	
	function getItemByDisplay(text)
	{
		var row = null; 
		text = trim(text); 		
		for(var index = 0; index < itemsLength; index++)
		{
			row = comboTable.rows[index]; 
			if(text.toUpperCase() == trim(getDisplayFromItem(row)).toUpperCase())
				return row; 
		}
		return null; 
	}	
	function getPixelPaddingLeft(padding)
	{
		var pixelpadding = 0;
		var indexof = padding.indexOf("px");
		if(indexof > 0)
			pixelpadding = parseInt(padding.substr(0, indexof));		
		return pixelpadding;
	}	
	function getValueFromItem(item)
	{
		return item.getAttribute("value"); 
	}	
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
			textbox.style.backgroundLeft = "center"; 
			textbox.style.backgroundTop = "center"; 
			textbox.style.backgroundRepeat = "no-repeat"; 
			textbox.style.paddingLeft = (paddingLeft + 18) + "px"; 
		}
		else
		{
			if(textbox.style.backgroundImage != "none" || textbox.style.backgroundImage != "" )			
			{
				if(paddingLeft - 18 >= 0)
					textbox.style.paddingLeft = (paddingLeft - 18) + "px";
				else
					textbox.style.paddingLeft = paddingLeft; 
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
		for(var index = 0; index < itemsLength; index++)
		{			
			item = comboTable.rows[index]; 
			text = trim(getDisplayFromItem(item)); 
			if(text.toUpperCase().indexOf(textToFind.toUpperCase()) == 0)
				return item;
		}
		return null;
	}	
	function HideCombo()
	{
		combo.style.visibility = "hidden"; 
		if(owner.getCell != null)
		{
			if(owner.getCell().getGridEX().ddbimg != "")
			{
				var img = button.getElementsByTagName("IMG")[0]; 
				img.setAttribute("status", 0);
				img.src = owner.getCell().getGridEX().ddbimg;
			}
		}
	}	
	function HideButton()
	{
		button.style.display = "none"; 
		button.style.visibility = "hidden"; 
	}	
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
		button.style.display = ""; 
		button.style.visibility = "visible"; 
		button.style.top = top + "px"; 
		button.style.left = ((left + width) - button.offsetWidth) + "px"; 
		button.style.height = height + "px"; 		
		textbox.style.visibility = "visible"; 		
		textbox.style.left = (left + 1) + "px";
		textbox.style.top = top + "px"; 
		textbox.style.height = height + "px";
		textbox.style.width = (width - button.offsetWidth - 1) + "px"; 
	}		
	function ShowCombo()
	{	
		if(initialHeight == -1)
		{
			combo.style.visibility = "visible"; 
			combo.getElementsByTagName("TABLE")[0].style.height = ""; 
			combo.getElementsByTagName("TABLE")[0].style.display = "block";				
			initialHeight = combo.offsetHeight; 
			combo.getElementsByTagName("TABLE")[0].style.height = "100%";
		}			
		var _fixedTop = -1;
		var _heightProposed = -1; 
		var scrollHeight = -1;		
		if(owner != null)
		{
			var ownerTable = owner.getCell().getGridEX().getRootTable().getHtmlItemsTable();
			scrollHeight = window.innerHeight;
			combo.getElementsByTagName("TABLE")[0].style.display = "block"; 
			if(((top + height + 2) + combo.offsetHeight > scrollHeight) || ((top + height + 2) + combo.offsetHeight < scrollHeight))
			{						
				if(scrollHeight - (top + height + 1) >= initialHeight)
					combo.style.height = initialHeight + "px"; 					
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
					combo.style.height = _heightProposed + "px";
				}
			}
			else if(scrollHeight - (top + height + 2) >= initialHeight)
			{								
				if(combo.offsetHeight != initialHeight)
					combo.style.height = initialHeight + "px"; 
			}
		}		
		combo.style.left = left + "px";
		if(_fixedTop != -1)
			combo.style.top = _fixedTop + "px"; 
		else
			combo.style.top = (top + height + 2) + "px"; 
		combo.style.width = width + "px";
		combo.style.visibility = "visible"; 
		if(combo.focus != null)
			combo.focus(); 
		window.event.returnValue = false;
		window.event.cancelBubble = true; 
	}	
	function button_onblur()
	{	
		if(document.activeElement == textbox || textbox.contains(document.activeElement))
			return true; 
			
		if(document.activeElement == combo || combo.contains(document.activeElement))
			return true; 
			
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 			
		
		return true; 
	}	
	function button_onkeydown()
	{
		if(window.event.keyCode == 27)
			Hide(); 
	}
	function button_onmousedown()
	{					
		if(combo.style.visibility != "visible")
		{
			if(owner != null && owner.DropDown != null)
				owner.DropDown();
				
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbpimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 1);
					img.src = owner.getCell().getGridEX().ddbpimg; 					
				}
			}
			ShowCombo();
		}
		else
		{
			if(owner.getCell != null)
			{
				if(owner.getCell().getGridEX().ddbimg != "")
				{
					var img = button.getElementsByTagName("IMG")[0]; 
					img.setAttribute("status", 0);
					img.src = owner.getCell().getGridEX().ddbimg;
				}
			}
			combo.style.visibility = "hidden"; 
		}
	}	
	function combo_onblur()
	{
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
		var item = getItem(window.event.srcElement);
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
		var item = getItem(window.event.srcElement); 
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
		var element = window.event.srcElement;
		var item = getItem(element);				
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
		
		if(owner != null && owner.Leaving != null)
			owner.Leaving(); 
	}	
	function textbox_onchange()
	{
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
			itemValue = itemDisplay = itemImage = null;
		else if(window.event.keyCode == 38 || window.event.keyCode == 40)
			commitNewText();		
		if(owner != null && owner.KeyDown != null)
			owner.KeyDown();
		return true; 
	}
	function textbox_onkeyup()
	{
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
	}
	comboTable = combo.getElementsByTagName("TABLE")[0];
	itemsLength = comboTable.rows.length; 	
	var row = null; 	
	for(var i = 0; i < itemsLength; i++)
	{
		row = comboTable.rows[i]; 		
		row.addEventListener("mousedown", item_onclick, false); 
		row.addEventListener("mouseover", item_onmouseover, false); 
		row.addEventListener("mouseout", item_onmouseout, false); 		
	}
	button.addEventListener("blur", button_onblur, false); 
	button.addEventListener("mousedown", button_onmousedown, false); 	
	button.addEventListener("keydown", button_onkeydown, false);
	textbox.addEventListener("blur", textbox_onblur, false); 
	textbox.addEventListener("change", textbox_onchange, false);	
	textbox.addEventListener("keypress", textbox_onkeypress, false);
	textbox.addEventListener("keydown", textbox_onkeydown, false); 	
	textbox.addEventListener("keyup", textbox_onkeyup, false); 
	combo.addEventListener("blur", combo_onblur, false); 
	combo.addEventListener("keydown", combo_onkeydown, false);
	return this; 
}
function GridEXEditTextArea(id)
{
	var id = id; 
	var textarea = document.getElementById(id); 
	if(textarea == null)
		throw Error("unable to find HTML textarea to act as GridEXEditTextArea '" + id + "'");
		
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
	function getID()
	{
		return id; 
	}	
	function getInnerTextArea()
	{
		return textarea; 
	}	
	function getLeft()
	{
		return left; 
	}	
	function getMaxLength()
	{
		return maxlength; 
	}	
	function getText()
	{
		return text; 
	}	
	function getTop()
	{
		return top; 
	}	
	function getHeight()
	{
		return height; 
	}	
	function getWidth()
	{
		return width; 
	}	
	function setCharacterCasing(value)
	{
		characterCasing = value; 
	}	
	function setOwner(value)
	{
		owner = value; 
	}	
	function setHeight(value)
	{
		height = value; 
	}	
	function setLeft(value)
	{
		left = value; 
	}	
	function setMaxLength(value)
	{
		maxlength = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
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
	function textarea_onchange()
	{
		commitNextText(); 
	}	
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
	textarea.addEventListener("blur", textarea_onblur, false); 
	textarea.addEventListener("change", textarea_onchange, false); 
	textarea.addEventListener("keypress", textarea_onkeypress, false); 
	textarea.addEventListener("keydown", textarea_onkeydown, false); 		
	textarea.addEventListener("keyup", textarea_onkeyup, false); 		
	return this;
}
function GridEXEditTextBox(id)
{
	var id = id;	
	var input = document.getElementById(id); 
	if(input == null)
		throw Error("unable to find HTML input to act as GridEXEditTextBox '" + id + "'");
	
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
	function getOwner()
	{
		return owner; 
	}	
	function getCharacterCasing()
	{
		return characterCasing; 
	}			
	function getID()
	{
		return id; 
	}	
	function getInnerHTML()
	{	
		return input; 
	}	
	function getInputMask()
	{
		return inputmask; 
	}	
	function getHeight()
	{
		return height; 
	}	
	function getLeft()
	{
		return left; 
	}	
	function getTop()
	{	
		return top; 
	}	
	function getWidth()
	{
		return width; 
	}	
	function getText()
	{	
		return text;
	}			
	function isValid()
	{		
		return isValid; 
	}	
	function setLeft(value)
	{
		left = value;
	}	
	function setHeight(value)
	{
		height = value; 
	}	
	function setTop(value)
	{
		top = value; 
	}	
	function setWidth(value)
	{
		width = value; 
	}	
	function setOwner(value)
	{
		owner = value; 
	}	
	function setCharacterCasing(value)
	{
		characterCasing = value; 
	}	
	function setInputMask(value)
	{
		inputmask = value; 
	}	
	function setText(value)
	{
		text = value; 		
		if(text == null)	
			input.value = "";
		else
			input.value = text; 			
	}	
	function setInvalidValueAction(value)
	{
		invalidValueAction = value; 
	}	
	function setInvalidValueMessage(value)
	{
		invalidValueMessage = value; 
	}	
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
			input.style.fontFamily = style.getPropertyValue("font-family"); 
			input.style.fontSize = style.getPropertyValue("font-size"); 
			input.style.textAlign = style.getPropertyValue("text-align");			
		}
	}	
	function getTrimmedText(value)
	{	
		var m = value.match(/^\s*(\S+(\s+\S+)*)\s*$/);
		return (m == null) ? "" : m[1];
	}	
	function requiresValidation()
	{
		return (getInputMask() != null && getInputMask().length > 0); 
	}		
	function isValidText(value)
	{	
		if(getInputMask() == null || getInputMask().length == 0)
			return true; 
			
		value = getTrimmedText(value); 
		if(value.length == 0)
			return true; 
			
		var cancel = null;
		if(owner != null && owner.ApplyingInputMask)	
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
			return false; 
		}		
	}	
	function textbox_onblur()	
	{		
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
		}
		else if(window.event.keyCode == 27)
			input.value = getText();
		else if(window.event.keyCode == 9)
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
		if(owner != null && owner.KeyUp != null)
			owner.KeyUp(); 
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
	function Show()
	{		
		input.style.left = getLeft() + "px"; 
		input.style.top = getTop() + "px"; 
		input.style.height = getHeight() + "px"; 
		input.style.width = getWidth() + "px";		
		input.style.visibility = "visible";		
	}	
	input.addEventListener("change", textbox_onchange, false);
	input.addEventListener("keydown", textbox_onkeydown, false);
	input.addEventListener("keypress", textbox_onkeypress, false); 	
	input.addEventListener("keyup", textbox_onkeyup, false); 
	input.addEventListener("blur", textbox_onblur, false); 
	return this;
}